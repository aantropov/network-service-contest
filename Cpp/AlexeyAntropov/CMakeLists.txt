cmake_minimum_required(VERSION 3.10)

project(WebService VERSION 0.1)

set_property(GLOBAL PROPERTY USE_FOLDERS ON)

include_directories ("${PROJECT_SOURCE_DIR}/Src/")

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED True)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /W3")

add_compile_options(/permissive-)
add_compile_options($<$<NOT:$<CONFIG:DEBUG>>:/Oi>)
add_compile_options($<$<NOT:$<CONFIG:DEBUG>>:/Ot>)
add_compile_options($<$<NOT:$<CONFIG:DEBUG>>:/GT>)
add_compile_options(/GF)
add_compile_options(/WX)
add_compile_options(/arch:AVX2)

message ("cxx Flags:" ${CMAKE_CXX_FLAGS})

add_compile_definitions(NOMINMAX)
add_definitions(-DFD_SETSIZE=8182)
add_executable (WebService Src/WebService.cpp)
target_link_libraries(WebService Ws2_32.lib Mswsock.lib AdvApi32.lib)

add_executable (WebClient Src/WebClient.cpp)
target_link_libraries(WebClient Ws2_32.lib Mswsock.lib AdvApi32.lib)