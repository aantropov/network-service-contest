#include <iostream>
#include <cstdio>
#include <functional>
#include <mutex>
#include <atomic>
#include <thread>
#include <queue>
#include <ppl.h>
#include <concurrent_queue.h>

#define WIN32_LEAN_AND_MEAN

#include <windows.h>
#include <winsock2.h>
#include <ws2tcpip.h>

struct Client
{
	SOCKET ClientSocket = INVALID_SOCKET;
	bool m_bIsConnected = false;

	__forceinline void Read(std::vector<float>& res)
	{
		const uint32_t BufferLength = 65536 / 2;

		res.clear();

		char recvbuf[BufferLength];
		int recvbuflen = BufferLength;
		int iResult;
		uint32_t offset = 0;

		do
		{
			iResult = recv(ClientSocket, recvbuf, recvbuflen, 0);
			if (iResult > 0)
			{
				if (res.size() == 0)
				{
					uint32_t size = (uint32_t) * reinterpret_cast<float*>(recvbuf);
					res.resize(1u + size * size * 2u);
				}

				memcpy(&res[offset], recvbuf, iResult);

				offset += iResult / 4;
			}
			else if (iResult == 0)
			{
				m_bIsConnected = true;
			}
			else
			{
				printf("recv failed with error: %d\n", WSAGetLastError());
				return;
			}

		} while (iResult > 0);
	}

	__forceinline void Send(const uint32_t num, const char* bytes)
	{
		int iSendResult = send(ClientSocket, bytes, num, 0);
		if (iSendResult == SOCKET_ERROR)
		{
			printf("send failed with error: %d\n", WSAGetLastError());
		}
	}

	__forceinline void Close()
	{
		if (ClientSocket == INVALID_SOCKET)
			return;

		// shutdown the connection since we're done
		int iResult = shutdown(ClientSocket, SD_BOTH);
		if (iResult == SOCKET_ERROR)
		{
			printf("shutdown failed with error: %d\n", WSAGetLastError());
		}

		// cleanup
		closesocket(ClientSocket);
		ClientSocket = INVALID_SOCKET;
	}
};

struct Task
{
	Client client;

	uint32_t size;
	float* m1;
	float* m2;

	__forceinline void Deserialize(std::vector<float>& res)
	{
		if (res.size() < 3)
		{
			return;
		}

		size = (uint32_t)res[0];

		m1 = &res[1];
		m2 = &res[size * size + 1];
	}

	__forceinline std::vector<float> Multiply()
	{
		std::vector<float> res(size * size);

		for (uint32_t i = 0; i < size; i++)
		{
			for (uint32_t j = i + 1; j < size; j++)
			{
				std::swap(m2[i * size + j], m2[j * size + i]);
			}
		}

		for (uint32_t i = 0; i < size; i++)
		{
			for (uint32_t k = 0; k < size; k++)
			{
				float val = 0;
				for (uint32_t j = 0; j < size; j++)
				{
					val += m1[i * size + j] * m2[k * size + j];
				}

				res[k + i * size] = val;
			}
		}

		return res;
	}

	__forceinline void Process()
	{
		std::vector<float> binary;

		client.Read(binary);
		if (client.m_bIsConnected)
		{
			Deserialize(binary);
			std::vector<float> sendBytes = Multiply();
			client.Send((uint32_t)sendBytes.size() * 4, (const char*)(&sendBytes[0]));
		}
		client.Close();
	}
};

using TaskPtr = std::shared_ptr<Task>;

class Scheduler;

class Worker
{
public:

	Worker(Scheduler* scheduler, std::condition_variable& conditional) : m_refresh(conditional), m_scheduler(scheduler) {}

	void Start()
	{
		m_thread = std::make_unique<std::thread>(&Worker::Process, this);
	}

	void Process();
	void Join() { m_thread->join(); }

protected:
	std::condition_variable& m_refresh;
	std::unique_ptr<std::thread> m_thread;
	Scheduler* m_scheduler;
};

class Scheduler
{
public:

	void Start()
	{
		const unsigned coresCount = std::thread::hardware_concurrency();
		const unsigned numThreads = std::max(1u, coresCount - 2u);

		for (uint32_t i = 0; i < numThreads; i++)
		{
			m_workers.emplace_back(Worker(this, m_conditional));
		}

		for (uint32_t i = 0; i < numThreads; i++)
		{
			m_workers[i].Start();
		}
	}

	void PushTask(TaskPtr&& task) { m_tasks.push(task); m_conditional.notify_one(); }
	bool PopTask(TaskPtr& outTask)
	{
		outTask = nullptr;
		if (m_tasks.try_pop(outTask))
		{
			return true;
		}
		return false;
	}

	__forceinline size_t NumTasks() const { return m_tasks.unsafe_size(); }

	void Terminate()
	{
		s_bIsTerminating = true;

		s_bIsTerminating = true;
		m_conditional.notify_all();

		for (auto& el : m_workers)
		{
			el.Join();
		}
	}

	static std::atomic<bool> s_bIsTerminating;

protected:

	std::condition_variable m_conditional{};
	std::vector<Worker> m_workers{};
	concurrency::concurrent_queue<TaskPtr> m_tasks{};
};

std::atomic<bool> Scheduler::s_bIsTerminating = false;

void Worker::Process()
{
	std::mutex threadExecutionMutex;

	while (!Scheduler::s_bIsTerminating)
	{
		std::unique_lock<std::mutex> lk(threadExecutionMutex);

		TaskPtr pCurrent;
		m_refresh.wait(lk, [this, &pCurrent] { return Scheduler::s_bIsTerminating || this->m_scheduler->PopTask(pCurrent); });

		if (pCurrent)
		{
			pCurrent->Process();
		}

		lk.unlock();
	}
}

class WebService
{
public:

	void Listen(Scheduler* scheduler, const std::string port = "27015")
	{
		WSADATA wsaData;
		int iResult;

		SOCKET ListenSocket = INVALID_SOCKET;

		struct addrinfo* result = NULL;
		struct addrinfo hints;

		// Initialize Winsock
		iResult = WSAStartup(MAKEWORD(2, 2), &wsaData);
		if (iResult != 0)
		{
			printf("WSAStartup failed with error: %d\n", iResult);
			return;
		}

		ZeroMemory(&hints, sizeof(hints));
		hints.ai_family = AF_INET;
		hints.ai_socktype = SOCK_STREAM;
		hints.ai_protocol = IPPROTO_TCP;
		hints.ai_flags = AI_PASSIVE;

		// Resolve the server address and port
		iResult = getaddrinfo(NULL, port.c_str(), &hints, &result);
		if (iResult != 0)
		{
			printf("getaddrinfo failed with error: %d\n", iResult);
			WSACleanup();
			return;
		}

		// Create a SOCKET for connecting to server
		ListenSocket = socket(result->ai_family, result->ai_socktype, result->ai_protocol);
		if (ListenSocket == INVALID_SOCKET)
		{
			printf("socket failed with error: %ld\n", WSAGetLastError());
			freeaddrinfo(result);
			WSACleanup();
			return;
		}

		// Setup the TCP listening socket
		iResult = bind(ListenSocket, result->ai_addr, (int)result->ai_addrlen);
		if (iResult == SOCKET_ERROR)
		{
			printf("bind failed with error: %d\n", WSAGetLastError());
			freeaddrinfo(result);
			closesocket(ListenSocket);
			WSACleanup();
			return;
		}

		freeaddrinfo(result);

		iResult = listen(ListenSocket, SOMAXCONN);
		if (iResult == SOCKET_ERROR)
		{
			printf("listen failed with error: %d\n", WSAGetLastError());
			closesocket(ListenSocket);
			WSACleanup();
			return;
		}

		while (!Scheduler::s_bIsTerminating)
		{
			//There is DDOS! We need to skip some of connections
			if (scheduler->NumTasks() > 100000)
			{
				Sleep(1);
			}

			Client client;
			client.ClientSocket = accept(ListenSocket, NULL, NULL);

			if (client.ClientSocket == INVALID_SOCKET)
			{
				printf("accept failed with error: %d\n", WSAGetLastError());
				return;
			}

			auto pTask = std::make_shared<Task>();
			pTask->client = std::move(client);
			scheduler->PushTask(std::move(pTask));
		}

		// No longer need server socket
		closesocket(ListenSocket);
		WSACleanup();
	}
};

int main()
{
	try
	{
		Scheduler scheduler;
		scheduler.Start();

		std::thread listenConnections([&scheduler]()
		{
			while (!scheduler.s_bIsTerminating)
			{
				WebService webService;
				webService.Listen(&scheduler);
			}
			printf("Rerun webservice\n");
		});

		getchar();

		scheduler.Terminate();
	}
	catch (const std::exception& e) { printf("%s", e.what()); }

	return 0;
}
