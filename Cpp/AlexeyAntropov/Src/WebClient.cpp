#define WIN32_LEAN_AND_MEAN

#include <windows.h>
#include <winsock2.h>
#include <ws2tcpip.h>
#include <stdlib.h>
#include <stdio.h>
#include <vector>
#include <time.h>
#include <string>
#include <thread>
#include <fstream>
#include <inttypes.h>
#include <random>

#define DEFAULT_BUFLEN 65536
#define DEFAULT_PORT "27015"

struct Timer
{
	int64_t m_counterStart = 0;
	int64_t m_counterEnd = 0;
	int64_t m_counterAcc = 0;
	double m_pcFrequence = 0.0;

	void Start()
	{
		LARGE_INTEGER li;
		if (!QueryPerformanceFrequency(&li))
		{
		}

		m_pcFrequence = double(li.QuadPart) / 1000.0;

		QueryPerformanceCounter(&li);
		m_counterStart = li.QuadPart;
	}

	void Stop()
	{
		LARGE_INTEGER li;
		QueryPerformanceCounter(&li);
		m_counterEnd = li.QuadPart;

		m_counterAcc += m_counterEnd - m_counterStart;
	}

	int64_t ResultMs() const
	{
		return int64_t(double(m_counterEnd - m_counterStart) / m_pcFrequence);
	}

	int64_t ResultAccumulatedMs() const
	{
		if (m_pcFrequence == 0.0)
		{
			return 0;
		}
		return int64_t((double)m_counterAcc / m_pcFrequence);
	}

	void Clear()
	{
		m_counterStart = 0;
		m_counterEnd = 0;
		m_counterAcc = 0;
		m_pcFrequence = 0.0;
	}
};

int ClientTask(std::string serverUrl, uint32_t matrixSize, uint32_t matrixDispersion, int64_t& accumulated)
{
	std::random_device rd;
	std::mt19937 g(rd());

	//////////////////////////////////
	// Generate Matrix
	uint32_t size = matrixSize + (g() % matrixDispersion - matrixDispersion / 2);

	std::vector<float> packet(1 + 2 * size * size);
	packet[0] = (float)size;

	//printf("size = %d\n", size);
	for (uint32_t i = 0; i < 2 * size * size; i++)
	{
		packet[1 + i] = (float)(g() % 100);
		/*printf("%f ", packet[1 + i]);

		if ((i + 1) % size == 0)
		{
			printf("\n");
		}

		if ((i + 1) % (size * size) == 0)
		{
			printf("\n");
		}*/
	}
	///////////////////////////////

	WSADATA wsaData;
	SOCKET ConnectSocket = INVALID_SOCKET;
	struct addrinfo* result = NULL,
		* ptr = NULL,
		hints;
	char recvbuf[DEFAULT_BUFLEN];
	int iResult;
	int recvbuflen = DEFAULT_BUFLEN;

	// Initialize Winsock
	iResult = WSAStartup(MAKEWORD(2, 2), &wsaData);
	if (iResult != 0) {
		printf("WSAStartup failed with error: %d\n", iResult);
		return 1;
	}

	Timer timer;
	timer.Start();

	ZeroMemory(&hints, sizeof(hints));
	hints.ai_family = AF_UNSPEC;
	hints.ai_socktype = SOCK_STREAM;
	hints.ai_protocol = IPPROTO_TCP;

	// Resolve the server address and port
	iResult = getaddrinfo(serverUrl.c_str(), DEFAULT_PORT, &hints, &result);
	if (iResult != 0) {
		printf("getaddrinfo failed with error: %d\n", iResult);
		WSACleanup();
		return 1;
	}

	// Attempt to connect to an address until one succeeds
	for (ptr = result; ptr != NULL; ptr = ptr->ai_next) {

		// Create a SOCKET for connecting to server
		ConnectSocket = socket(ptr->ai_family, ptr->ai_socktype,
			ptr->ai_protocol);
		if (ConnectSocket == INVALID_SOCKET) {
			printf("socket failed with error: %ld\n", WSAGetLastError());
			WSACleanup();
			return 1;
		}

		// Connect to server.
		iResult = connect(ConnectSocket, ptr->ai_addr, (int)ptr->ai_addrlen);
		if (iResult == SOCKET_ERROR) {
			closesocket(ConnectSocket);
			ConnectSocket = INVALID_SOCKET;
			continue;
		}
		break;
	}

	freeaddrinfo(result);

	if (ConnectSocket == INVALID_SOCKET) {
		printf("Unable to connect to server! %ld\n", WSAGetLastError());
		WSACleanup();
		return 1;
	}

	// Send an initial buffer
	iResult = send(ConnectSocket, (const char*)(&packet[0]), (1 + 2 * size * size) * 4, 0);
	if (iResult == SOCKET_ERROR) {
		printf("send failed with error: %d\n", WSAGetLastError());
		closesocket(ConnectSocket);
		WSACleanup();
		return 1;
	}

	printf("Bytes Sent: %ld\n", iResult);

	// shutdown the connection since no more data will be sent
	iResult = shutdown(ConnectSocket, SD_SEND);
	if (iResult == SOCKET_ERROR) {
		printf("shutdown failed with error: %d\n", WSAGetLastError());
		closesocket(ConnectSocket);
		WSACleanup();
		return 1;
	}

	uint32_t receivedBytes = size * size * 4;

	// Receive until the peer closes the connection
	do {
		iResult = recv(ConnectSocket, recvbuf, recvbuflen, 0);
		if (iResult > 0)
		{
			receivedBytes -= iResult;
			//printf("Bytes received: %d\n", iResult);
		}
		else if (iResult == 0)
		{
			std::ofstream out;
			out.open("err.txt", std::ios_base::app | std::ios_base::in);
			out << "Error \n";
			out.close();
		}
		else
			printf("recv failed with error: %d\n", WSAGetLastError());

	} while (receivedBytes);

	iResult = shutdown(ConnectSocket, SD_RECEIVE);
	closesocket(ConnectSocket);
	WSACleanup();

	timer.Stop();
	accumulated += timer.ResultAccumulatedMs();

	return 0;
}

int __cdecl main(int argc, char** argv)
{	
	uint32_t matrixSize = 1000;
	uint32_t matrixDispersion = 100;
	uint32_t numTries = 1600;

	std::string serverUrl = "127.0.0.1";

	if (argc > 1)
	{
		matrixSize = std::atoi(argv[1]);
	}

	if (argc > 2)
	{
		matrixDispersion = std::atoi(argv[2]);
	}

	if (argc > 3)
	{
		numTries = std::atoi(argv[3]);
	}

	if (argc > 4)
	{
		serverUrl = std::string(argv[4]);
	}

	int64_t acc = 0;
	int64_t maxTime = 0;
	int64_t numSuccesess = 0;
	for (int32_t i = numTries; i > 0; i--)
	{
		int64_t time = 0;
		if (ClientTask(serverUrl.c_str(), matrixSize, matrixDispersion, time) == 0)
		{
			numSuccesess++;
		}

		acc += time;
		maxTime = std::max(time, maxTime);

		printf("Max=%" PRId64 "ms, Time=%" PRId64 "ms, Acc=" "%" PRId64 "ms\n", maxTime, time, acc);
	}

	std::ofstream out;
	out.open("acc.txt", std::ios_base::app | std::ios_base::in);
	out << ((double)acc / (double)(std::max(numSuccesess, 1ll))) << std::endl;
	out.close();

	out.open("max.txt", std::ios_base::app | std::ios_base::in);
	out << (maxTime) << std::endl;
	out.close();

	return 0;
}