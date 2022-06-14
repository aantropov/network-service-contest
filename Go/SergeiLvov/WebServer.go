package main

import "fmt"
import "os"
import "net"
import "unsafe"
import "io/ioutil"

const HOST = ""
const PORT = "27015"

func HandleClient(conn net.Conn) {
    buf, err := ioutil.ReadAll(conn)
    if err != nil {
        fmt.Println("Failed to read data from the connection: ", err.Error())
        os.Exit(1)
    }

    // golang does not allow slices of unknown size
    // so I used "big enough" number 1<<30
    data := (*[1<<30]float32)(unsafe.Pointer(&buf[0]))

    N := int(data[0]) // matrix size

    float_size := int(unsafe.Sizeof(data[0]))
    if len(buf)/float_size != (1+2*N*N) {
        fmt.Println("Invalid size of the network packet!")
        os.Exit(1)
    }

    A := data[1:1+N*N]
    B := data[1+N*N:1+2*N*N]
    C := make([]float32, N*N)
    for y := 0; y < N; y++ {
        for x := 0; x < N; x++ {
            var C_elem float32 = 0
            for i := 0; i < N; i++ {
                A_index := y*N + i
                B_index := i*N + x
                C_elem += A[A_index] * B[B_index]
            }
            C_index := y*N + x
            C[C_index] = C_elem
        }
    }

    out_buf := (*[1<<30]byte)(unsafe.Pointer(&C[0]))
    bytes_sent, err := conn.Write(out_buf[0:N*N*float_size])
    if err != nil {
        fmt.Println("Failed to send data back")
        os.Exit(1)
    }
    if bytes_sent != N*N*float_size {
        fmt.Println("Not all data was sent")
        os.Exit(1)
    }
    conn.Close()
}

func main() {
    acceptor, err := net.Listen("tcp", HOST+":"+PORT)
    if err != nil {
        fmt.Println("Error listening: ", err.Error())
        os.Exit(1)
    }
    defer acceptor.Close()
    fmt.Println("Listening on " + HOST+":"+PORT)
    for {
        conn, err := acceptor.Accept()
        if err != nil {
            fmt.Println("Error accepting: ", err.Error());
            os.Exit(1)
        }
        go HandleClient(conn)
    }
    fmt.Println("Hello, World!")
}
