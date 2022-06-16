import asyncio, socket, struct
import numpy as np
  
port = 27015

async def handle_client(client):
    loop = asyncio.get_event_loop()
    
    data = await loop.sock_recv(client,4)
    N = int(struct.unpack('f', data)[0])
    
    n = 2*N*N*4
    data = await loop.sock_recv(client,n)
    while len(data) < n:
        data += await loop.sock_recv(client,n - len(data))

    if len(data) != n:
        print("Bad request")
        return
    
    data = np.frombuffer(data, dtype=np.float32) 
    A = data[0:N*N].reshape(N,N)
    B = data[N*N:].reshape(N,N)
    response = (np.matmul(A,B)).tobytes()
    await loop.sock_sendall(client, response)
    client.close()

async def run_server():
    print("Starting MatrixPython CPU at port:", port)
    server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    
    # use for connection from local machine
    print("   local connections")
    server.bind(('localhost', port))  
    
    # use for connection from other machine
    #print("   external connections")
    #server.bind((socket.gethostname(), port)) 
    
    server.listen()
    server.setblocking(False)

    loop = asyncio.get_event_loop()

    while True:
        client, _ = await loop.sock_accept(server)
        loop.create_task(handle_client(client))

asyncio.run(run_server())

