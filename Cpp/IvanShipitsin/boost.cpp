#include <boost/asio/awaitable.hpp>
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/ip/tcp.hpp>
#include <boost/asio/io_context.hpp>
#include <boost/asio/detached.hpp>
#include <boost/asio/write.hpp>
#include <boost/asio/read.hpp>
#include <thread>

using boost::asio::ip::tcp;
using boost::asio::co_spawn;
using boost::asio::detached;
using boost::asio::use_awaitable;
using boost::asio::buffer;

struct MatrixCalculator {
	std::vector<float> matrixData;
	std::size_t matrixRowSize = 0;
	std::size_t matrixTotalSize = 0;

	float& MatrixAt(std::size_t matrixIndex, std::size_t row, std::size_t column) {
		std::size_t elemIndex = (matrixIndex * matrixTotalSize) + (row * matrixRowSize) + column;
		return matrixData[elemIndex];
	}

	void CalculateMatrix() {
		for (std::size_t i = 0; i < matrixRowSize; i++) {
			for (std::size_t k = 0; k < matrixRowSize; k++) {
				for (std::size_t j = 0; j < matrixRowSize; j++) {
					MatrixAt(2, i, j) += MatrixAt(0, i, k) * MatrixAt(1, k, j);
				}
			}
		}
	}
};

boost::asio::awaitable<void> HandlerAccept(tcp::socket socket) {
	float matrixSize;
	co_await async_read(socket, buffer(&matrixSize, sizeof(float)), use_awaitable);
	
	MatrixCalculator matrix;
	matrix.matrixRowSize = matrixSize;
	matrix.matrixTotalSize = matrix.matrixRowSize * matrix.matrixRowSize;
	matrix.matrixData.resize(matrix.matrixTotalSize * 3);

	co_await async_read(socket, buffer(matrix.matrixData.data(), matrix.matrixTotalSize * 2 * sizeof(float)), use_awaitable);

	matrix.CalculateMatrix();

	co_await async_write(socket, buffer(matrix.matrixData.data() + matrix.matrixTotalSize * 2, matrix.matrixTotalSize * sizeof(float)), use_awaitable);
}

boost::asio::awaitable<void> SocketListener() {
	auto executor = co_await boost::asio::this_coro::executor;
	tcp::acceptor acceptor(executor, { tcp::v4(), 27015 });
	for (;;)
	{
		tcp::socket socket = acceptor.accept();
		//tcp::socket socket = co_await acceptor.async_accept(use_awaitable);
		co_spawn(executor, HandlerAccept(std::move(socket)), detached);
	}
}

int main() {
	SetPriorityClass(GetCurrentProcess(), HIGH_PRIORITY_CLASS);
	std::size_t maxWorkers = std::max(std::thread::hardware_concurrency(), 2u);
	boost::asio::io_context io_context(maxWorkers);

	co_spawn(io_context, SocketListener(), detached);

	std::vector<std::thread> threads;
	for (size_t i = 0; i < maxWorkers; i++) {
		threads.emplace_back(std::thread{
				[&io_context]() {
				SetThreadPriority(GetCurrentThread(), THREAD_PRIORITY_HIGHEST);
					io_context.run();
				}
			});
	}
	for (auto& t : threads) {
		t.join();
	}

	return 0;
}
