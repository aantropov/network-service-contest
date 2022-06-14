#include <list>
#include <vector>
#include <iostream>
//#include <format>
#include <shared_mutex>
#include <thread>

#define WIN32_LEAN_AND_MEAN
#define NOMINMAX
#include <Windows.h>
#include <WinSock2.h>

struct ThreadSyncObject {
	std::vector<SOCKET> sockets;
	std::shared_mutex mutex;
	std::thread workerThread;
	std::atomic_bool run{true};
	std::size_t id = 0;
	HANDLE hEvent = 0;
};

struct Task {
	SOCKET socket = 0;
	WSAOVERLAPPED overlapped{ 0 };
	WSABUF buffer{ 0 };
	DWORD flags;
	float matrixSize = 0.0f;
	std::size_t matrixRowSize = 0;
	std::size_t matrixTotalSize = 0;
	std::size_t matrixTotalSizeBytes = 0;
	std::vector<float> matrixData;
	bool calculated = false;

	float& MatrixAt(std::size_t matrixIndex, std::size_t row, std::size_t column) {
		std::size_t elemIndex = (matrixIndex * matrixTotalSize) + (row * matrixRowSize) + column;
		return matrixData[elemIndex];
	}

	void CalculateMatrix() {
		//auto t1 = clock();
		for (std::size_t i = 0; i < matrixRowSize; i++) {
			for (std::size_t k = 0; k < matrixRowSize; k++) {
				for (std::size_t j = 0; j < matrixRowSize; j++) {
					MatrixAt(2, i, j) += MatrixAt(0, i, k) * MatrixAt(1, k, j);
				}
			}
		}
		calculated = true;
		//auto t2 = clock();
		//std::cout << std::format("{}ms\r\n", t2 - t1);
	}

	void ReceiveMatrixData() {
		matrixRowSize = static_cast<std::size_t>(matrixSize);
		matrixTotalSize = matrixRowSize * matrixRowSize;
		matrixTotalSizeBytes = matrixTotalSize * sizeof(float);
		matrixData.resize(matrixTotalSizeBytes * 3);
		for (auto& f : matrixData)
			f = 0.0f;
		buffer = WSABUF{
			.len = static_cast<DWORD>(matrixTotalSizeBytes * 2),
			.buf = reinterpret_cast<char*>(matrixData.data())
		};
		flags = MSG_WAITALL;
		int status = WSARecv(socket, &buffer, 1, NULL, &flags, &overlapped, NULL);
		if (status == 0) {
			SetEvent(overlapped.hEvent);
		}
	}

	void ReceiveMatrixSize() {
		buffer = WSABUF{
			.len = sizeof(float),
			.buf = reinterpret_cast<char*>(&matrixSize)
		};
		flags = MSG_WAITALL;
		int status = WSARecv(socket, &buffer, 1, NULL, &flags, &overlapped, NULL);
		if (status == 0) {
			ReceiveMatrixData();
		}
	}

	void SendMatrixMul() {
		CalculateMatrix();
		buffer = WSABUF{
			.len = static_cast<DWORD>(matrixTotalSizeBytes),
			.buf = reinterpret_cast<char*>(matrixData.data() + matrixTotalSize * 2)
		};
		flags = 0;
		int status = WSASend(socket, &buffer, 1, NULL, flags, &overlapped, NULL);
		if (status == 0) {
			SetEvent(overlapped.hEvent);
		}
	}
};

void WorkerMain(ThreadSyncObject* tso) {
	SetThreadPriority(GetCurrentThread(), THREAD_PRIORITY_HIGHEST);
	std::list<Task> tasks;
	std::vector<WSAEVENT> eventsToWait;

	while (tso->run) {
		//std::cout << std::format("{}| Worker update\r\n", tso->id);
		bool newSocketsAvailable = false;
		{
			std::shared_lock rwlock(tso->mutex);
			newSocketsAvailable = !tso->sockets.empty();
		}
		if (newSocketsAvailable) {
			//std::cout << std::format("{}| Find non-empty sockets\r\n", tso->id);
			std::lock_guard lock(tso->mutex);
			for (const auto& socket : tso->sockets) {
				//std::cout << std::format("{}|{}| AddSocket\r\n", tso->id, tasks.size());
				tasks.emplace_back(Task{ .socket = socket });
				tasks.back().overlapped.hEvent = CreateEvent(NULL, FALSE, FALSE, NULL);
				tasks.back().ReceiveMatrixSize();
				//std::cout << std::format("{}|{}| ReceiveMatrixSize\r\n", tso->id, tasks.size());
			}
			tso->sockets.clear();
			//std::cout << std::format("{}| Clear sockets\r\n", tso->id);
		}

		eventsToWait.clear();
		eventsToWait.push_back(tso->hEvent);
		for (const auto& task : tasks) {
			eventsToWait.push_back(task.overlapped.hEvent);
		}

		DWORD activeEvent = WaitForMultipleObjects(eventsToWait.size(), eventsToWait.data(), false, INFINITE);
		DWORD taskIndex = activeEvent - WAIT_OBJECT_0;
		if (taskIndex == 0) {
			continue;
		}
		taskIndex--;
		if (taskIndex > tasks.size()) {
			//std::cout << std::format("{}| Unknown event {}\r\n", tso->id, activeEvent);
			break;
		}

		auto taskIt = std::next(tasks.begin(), taskIndex);
		auto &task = *taskIt;

		DWORD bytesReceived = 0;
		DWORD flags = 0;
		if (!WSAGetOverlappedResult(task.socket, &task.overlapped, &bytesReceived, TRUE, &flags)) {
			//std::cout << std::format("{}|{}| WSAGetOverlappedResult failed\r\n", tso->id, taskIndex);
			closesocket(task.socket);
			CloseHandle(task.overlapped.hEvent);
			tasks.erase(taskIt);
			continue;
		}

		if (task.matrixData.empty()) {
			//std::cout << std::format("{}|{}| ReceiveMatrixData\r\n", tso->id, taskIndex);
			task.ReceiveMatrixData();
			continue;
		}

		if (!task.calculated) {
			//std::cout << std::format("{}|{}| SendMatrixMul\r\n", tso->id, taskIndex);
			task.SendMatrixMul();
			continue;
		}
		shutdown(task.socket, SD_SEND);
		closesocket(task.socket);
		CloseHandle(task.overlapped.hEvent);
		tasks.erase(taskIt);
		//std::cout << std::format("{}|{}| Destroyed task\r\n", tso->id, taskIndex);
	}
}

int main() {
	SetPriorityClass(GetCurrentProcess(), HIGH_PRIORITY_CLASS);
	SetThreadPriority(GetCurrentThread(), THREAD_PRIORITY_NORMAL);
	std::size_t maxWorkers = std::max(std::thread::hardware_concurrency() - 1, 1u);
	std::vector<ThreadSyncObject> threadSyncs(maxWorkers);

	for (size_t i = 0; i < threadSyncs.size(); i++)
		threadSyncs[i].id = i;
	for (auto& tso : threadSyncs) {
		tso.workerThread = std::thread(WorkerMain, &tso);
		tso.hEvent = CreateEvent(NULL, FALSE, FALSE, NULL);
	}

	//init socket
	WSADATA wsaData;
	if (WSAStartup((2, 2), &wsaData) != 0) {
		//std::cout << std::format("WSAStartup failed with errror {}\r\n", WSAGetLastError());
		return -1;
	}

	SOCKET listenSocket = WSASocket(AF_INET, SOCK_STREAM, 0, NULL, 0, WSA_FLAG_OVERLAPPED);
	if (listenSocket == INVALID_SOCKET) {
		//std::cout << std::format("listenSocket failed with errror {}\r\n", WSAGetLastError());
		return -1;
	}

	SOCKADDR_IN listenAddr;
	listenAddr.sin_family = AF_INET;
	listenAddr.sin_addr.s_addr = htonl(INADDR_ANY);
	listenAddr.sin_port = htons(27015);

	if (bind(listenSocket, (PSOCKADDR)&listenAddr, sizeof(listenAddr)) == SOCKET_ERROR) {
		//std::cout << std::format("bind failed with errror {}\r\n", WSAGetLastError());
		return -1;
	}

	if (listen(listenSocket, SOMAXCONN)) {
		//std::cout << std::format("listen failed with errror {}\r\n", WSAGetLastError());
		return -1;
	}

	std::size_t activeWorker = 0;
	while (true) {
		SOCKET acceptSocket = accept(listenSocket, NULL, NULL);

		//std::cout << std::format("Pushing socket for worker {}\r\n", threadSyncs[activeWorker].id);
		{
			std::lock_guard lock(threadSyncs[activeWorker].mutex);
			threadSyncs[activeWorker].sockets.push_back(acceptSocket);
		}
		SetEvent(threadSyncs[activeWorker].hEvent);

		activeWorker = activeWorker + 1 < maxWorkers ? activeWorker + 1 : 0;
	}

	return 0;
}
