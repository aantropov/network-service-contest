#include <boost/asio/awaitable.hpp>
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/ip/tcp.hpp>
#include <boost/asio/io_context.hpp>
#include <boost/asio/detached.hpp>
#include <boost/asio/write.hpp>
#include <boost/asio/read.hpp>
#include <thread>
#include <Eigen/Geometry>

using boost::asio::ip::tcp;
using boost::asio::co_spawn;
using boost::asio::detached;
using boost::asio::use_awaitable;
using boost::asio::buffer;

boost::asio::awaitable<void> HandlerAccept(tcp::socket socket) {
	float matrixSize;
	co_await async_read(socket, buffer(&matrixSize, sizeof(float)), use_awaitable);

	const std::size_t matrixRowSize = static_cast<std::size_t>(matrixSize);
	const std::size_t matrixTotalSize = matrixRowSize * matrixRowSize;

	std::vector<float> matrixData(matrixTotalSize * 3);

	Eigen::Map<Eigen::MatrixXf> a(matrixData.data() + matrixTotalSize * 0, matrixRowSize, matrixRowSize);
	Eigen::Map<Eigen::MatrixXf> b(matrixData.data() + matrixTotalSize * 1, matrixRowSize, matrixRowSize);
	Eigen::Map<Eigen::MatrixXf> c(matrixData.data() + matrixTotalSize * 2, matrixRowSize, matrixRowSize);

	co_await async_read(socket, buffer(matrixData.data(), matrixTotalSize * 2 * sizeof(float)), use_awaitable);

	c = a * b;
	c.eval();
	
	co_await async_write(socket, buffer(c.data(), matrixTotalSize * sizeof(float)), use_awaitable);
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
