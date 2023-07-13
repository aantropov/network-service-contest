# Programming Languages Comparison Contest - Results

This repository contains the results of a contest that compared the implementation of a complex multithreaded task across various programming languages. Participants were tasked to develop a Network Service that multiplies two matrices and returns the result. 

## The Challenge

Participants selected a programming language and freely developed a Network Service with a focus on asynchrony/multithreading, with the results compared based on the complexity and performance of the solutions.

## The Solutions

Here are the languages that participants chose along with their experiences and the time it took to implement the solution:

### C++17
Time spent: 2.5h + 0.5h (profiling)

### Python3 / PyTorch
Time spent: 2.5h  
Additional details: The idea was to run the solution on GPU.

### Erlang

### C++20
Time spent: 3h + 0.5h (due to a bug from inattention)  
Additional details: Two hours were spent learning winapi overlapped io - an API that seems to have all forms of asynchrony (callbacks for operation completion + select for launched operations + synchronous work). Focus was on asynchrony and multithreading. The matrix multiplication function was very basic and very slow. However, results with a large number of connections and small input data can be interesting.

### Golang
Time spent: 1.5h  
Additional details: With knowledge of the language, the solution could be accomplished in 30 mins.

### Julia
Time spent: 1.5h  
Additional details: Had to adjust the client to remove a call to shutdown(). Overall, the language seemed nicer than Golang, but the support for network input/output was clearly limping.

### Erlang
Time spent: 4h  
Additional details: 2.5 hours on writing the sequential version, 1 hour testing and fixing (different float formats, + large matrices come in separate pieces that had to be combined). Half an hour attempting to parallelize multiplication. In general, multiplying matrices in Erlang did not feel good, it was easier in C++. But making a parallel server is easy, and working with binary data is nice.

### C++20 Boost.Asio
Time spent: 15min  
Additional details: The author had already written the basic variant and was familiar with the Asio library. The multiplication loop was stolen from the basic variant.

### C++20 Boost.Asio Eigen
Time spent: 20min + 20min (useless optimization attempts)  
Additional details: The author ported their boost variant to an external math library with vectorization support. The total time spent was 20 minutes to read the documentation, connect the library, and port the code.

## Final Assessment
The table below shows the performances of different solutions for varying sizes of matrix multiplication tasks. The measurement used is time, where a lower value signifies higher performance.

| Name                      | Small (max)      | Medium (max)    | Heavy (max)     | Full (max)      | Huge (max)      |
| ------------------------- | ---------------- | --------------- | --------------- | --------------- | --------------- |
| cpp_alexey_antropov       | 63 ±6 (314 ±66)  | 234 ±32 (307 ±6)| 565 ±5 (655 ±132)| 350 ±11 (785 ±290)| 7305 ±140 (7679 ±216) |
| cpp_boost_ivan_shipitsin  | 68 ±9 (376 ±89)  | 243 ±8 (319 ±9) | 879 ±9 (1104 ±42)| 467 ±27 (950 ±72) | 19806 ±219 (20520 ±233)|
| cpp_winapi_ivan_shipitsin | 66 ±7 (367 ±77)  | 238 ±13 (307 ±7)| 560 ±4 (624 ±11) | 361 ±10 (567 ±29) | 9012 ±188 (9340 ±313) |
| csharp_andrey_tsirulev    | 67 ±8 (366 ±84)  | 236 ±24 (311 ±8)| 560 ±11 (796 ±294)| 369 ±9 (537 ±56) | 1501 ±109 (1751 ±186) |
| eigen_ivan_shipitsin      | 57 ±10 (399 ±129)| 236 ±19 (308 ±7)| 573 ±93 (735 ±156)| 353 ±7 (515 ±17) | 1336 ±126 (2042 ±715) |
| golang_sergei_lvov        | 60 ±6 (332 ±61)  | 234 ±13 (307 ±6)| 571 ±5 (703 ±167) | 358 ±8 (555 ±32) | 13597 ±369 (14449 ±255) |
| python_cpu_denis_perevalov| 68 ±8 (340 ±84)  | 237 ±17 (308 ±7)| 557 ±9 (703 ±297) | 351 ±8 (485 ±17) | 1379 ±114 (1767 ±179) |
| python_gpu_denis_perevalov| 81 ±5 (767 ±514) | 238 ±15 (307 ±7)| 555 ±13 (631 ±11) | 363 ±11 (491 ±17) | 1424 ±89 (1877 ±324) |
| rust_igor_burdunin        | 64 ±7 (311 ±67)  | 232 ±33 (314 ±9)| 554 ±8 (658 ±149) | 351 ±9 (509 ±24) | 3688 ±226 (4750 ±350) |
| erlang_andrey_tsirulev    | 68 ±8 (408 ±116) | 230 ±17 (308 ±6)| 549 ±9 (620 ±17)  | 358 ±8 (522 ±35) | 2033 ±112 (2880 ±540) |
| erlang_pure_daniil_pavlenko| 1553 ±18 (2887 ±133)| 9706 ±125 (12398 ±226)| 36939 ±603 (40153 ±1539)| 17195 ±2771 (29137 ±6086)| N/A |
| erlang_bin_daniil_pavlenko| 4789 ±231 (7785 ±708)| 14612 ±12130 (19628 ±14748)| N/A | N/A | N/A |

In brief, we can see that the solutions provided by 'Alexey Antropov (C++)' and 'Andrey Tsirulev (C#)' offered the most efficient performance in the majority of categories.
