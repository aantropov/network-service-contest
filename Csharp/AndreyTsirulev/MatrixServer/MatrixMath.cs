using System;
using System.Numerics;
using System.Threading.Tasks;
using static System.Runtime.InteropServices.MemoryMarshal;

namespace Matrix
{
    public static class MatrixMath
    {
        public static void Transpose(Span<float> matrix, int size)
        {
            for (int i = 0; i < size; i++)
                for (int j = i + 1; j < size; j++)
                {
                    ref float l = ref matrix[i * size + j];
                    ref float r = ref matrix[j * size + i];
                    (l, r) = (r, l);
                }
        }

        public static void Multiply(ReadOnlySpan<float> matrix1, ReadOnlySpan<float> matrix2T, Span<float> result, int size)
        {
            for (int i = 0; i < size; i++)
            {
                int iSize = i * size;
                for (int j = 0; j < size; j++)
                {
                    var row1 = matrix1.Slice(iSize, size);
                    var row2 = matrix2T.Slice(j * size, size);
                    result[iSize + j] = DotVectorized(row1, row2, size);
                }
            }
        }

        public static void MultiplyChunks(ReadOnlyMemory<byte> matrix1, ReadOnlyMemory<byte> matrix2T, Memory<byte> result, int size, int maxChunks, int minChunkSize)
        {
            int size2 = size * size;
            int desiredChunkSize = size2 / maxChunks;
            int chunkSize = minChunkSize;
            while (chunkSize < desiredChunkSize)
                chunkSize *= 2;
            int numChunks = size2 / chunkSize;
            if (size2 % chunkSize > 0)
                numChunks++;
            Parallel.For(0, numChunks, c => MultiplyChunk(matrix1, matrix2T, result, size, c * chunkSize, chunkSize));
        }

        public static void MultiplyChunk(ReadOnlyMemory<byte> matrix1, ReadOnlyMemory<byte> matrix2T, Memory<byte> result, int size, int offset, int chunkSize)
        {
            MultiplyChunk(Cast<byte, float>(matrix1.Span), Cast<byte, float>(matrix2T.Span), Cast<byte, float>(result.Span), size, offset, chunkSize);
        }

        public static void MultiplyChunk(ReadOnlySpan<float> matrix1, ReadOnlySpan<float> matrix2T, Span<float> result, int size, int offset, int chunkSize)
        {
            int last = Math.Min(size * size, offset + chunkSize);
            for (int c = offset; c < last; c++)
            {
                int i = c / size;
                int j = c % size;
                var row1 = matrix1.Slice(i * size, size);
                var row2 = matrix2T.Slice(j * size, size);
                result[c] = DotVectorized(row1, row2, size);
            }
        }

        public static float DotNaive(ReadOnlySpan<float> v1, ReadOnlySpan<float> v2, int size)
        {
            float r = 0f;
            for (int k = 0; k < size; k++)
            {
                r += v1[k] * v2[k];
            }
            return r;
        }

        public static float DotVectorized(ReadOnlySpan<float> v1, ReadOnlySpan<float> v2, int size)
        {
            float r = 0f;
            int vectorSize = Vector<float>.Count;
            int offset = 0;
            while (size - offset >= vectorSize)
            {
                r += Vector.Dot(new Vector<float>(v1.Slice(offset, vectorSize)), new Vector<float>(v2.Slice(offset, vectorSize)));
                offset += vectorSize;
            }
            while (offset < size)
            {
                r += v1[offset] * v2[offset];
                offset++;
            }
            return r;
        }
    }
}
