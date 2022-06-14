using System;
using System.Buffers;
using System.Diagnostics;
using System.IO;
using System.Net;
using System.Net.Sockets;
using static System.Runtime.InteropServices.MemoryMarshal;
using System.Threading;
using System.Threading.Tasks;

namespace Matrix
{
    class Program
    {
        public const int MaxMatrixSize = 2048;
        public const int Port = 27015;
        public const int MinChunkSize = 4096;
        public const int MaxChunks = 64;
        public const int MinChunkMatrixSize = 100;
        public const int MaxChunkConcurrentRequests = 42;

        public static volatile int CurrentWorkers = 0;

        [MTAThread]
        static async Task Main(string[] args)
        {
            var tcpServer = new TcpListener(IPAddress.Any, Port);
            tcpServer.Start();
            var pool = ArrayPool<byte>.Create(MaxMatrixSize * MaxMatrixSize * sizeof(float), 100);
            // var pool = ArrayPool<byte>.Shared;
            
            while (true)
            {
                var client = await tcpServer.AcceptTcpClientAsync();
                // _ = Task.Run(() => ProcessClientAsync(client, pool));
                _ = ProcessClientAsync(client, pool);
            }
        }
        
        static async Task ProcessClientAsync(TcpClient client, ArrayPool<byte> pool, CancellationToken ct = default)
        {
            try
            {
                var stream = client.GetStream();
                int size = await ReadSize(stream, ct);
                Console.WriteLine(size);
                int memSize = size * size * sizeof(float);
                var m1 = pool.Rent(memSize);
                var m2 = pool.Rent(memSize);
                var result = pool.Rent(memSize);
                try
                {
                    await ReadMemory(stream, m1, memSize, ct);
                    await ReadMemory(stream, m2, memSize, ct);
                    await Task.Run(() =>
                    {
                        try
                        {
                            bool allowChunks =  Interlocked.Increment(ref CurrentWorkers) < MaxChunkConcurrentRequests;
                            MatrixMath.Transpose(Cast<byte, float>(m2), size);
                            if (allowChunks && size < MinChunkMatrixSize)
                                MatrixMath.Multiply(Cast<byte, float>(m1), Cast<byte, float>(m2), Cast<byte, float>(result), size);
                            else
                                MatrixMath.MultiplyChunks(m1, m2, result, size, MaxChunks, MinChunkSize);
                        }
                        finally
                        {
                            Interlocked.Decrement(ref CurrentWorkers);
                        }
                    }, ct);
                    await stream.WriteAsync(result, 0, memSize, ct);
                    stream.Close(30000);
                }
                finally
                {
                    pool.Return(m1);
                    pool.Return(m2);
                    pool.Return(result);
                }
            }
            catch (Exception exception)
            {
                Console.WriteLine(exception);
            }
            finally
            {
                client.Dispose();
            }
        }

        static async Task<int> ReadSize(Stream stream, CancellationToken ct)
        {
            var bytes = new byte[sizeof(float)];
            await ReadMemory(stream, bytes, 4, ct);
            return (int)Cast<byte, float>(bytes)[0];
        }
        
        static async Task ReadMemory(Stream stream, byte[] memory, int bytesToRead, CancellationToken ct)
        {
            int offset = 0;
            do
            {
                int bytesRead = await stream.ReadAsync(memory, offset, bytesToRead, ct);
                offset += bytesRead;
                bytesToRead -= bytesRead;
            }
            while (bytesToRead > 0);
        }
    }
}