# -*- coding: utf-8 -*-
#---------------------------------------------------
# Server for matrix multiplication on Python/pytorch/CUDA
# v.1.05
# @author: denis.perevalov
#---------------------------------------------------
#Requirements: GPU
#
#Installation: 
#1. Install CUDA 11.3,
#2. Install Anaconda, 
#3. Install pytorch:
#      Anaconda > CMD.exe prompt > pip3 install torch torchvision torchaudio --extra-index-url https://download.pytorch.org/whl/cu113
#
#Run: 
# Start Anaconda, 
# CMD.exe prompt, 
# python MatrixPython.py
#---------------------------------------------------

import asyncio, socket
import struct
import numpy as np
import torch
  
port = 27015
listen_max_clients = 8 # Limit of incoming clients
float_size = 4

async def handle_client(client):
    loop = asyncio.get_event_loop()
    
    # Read matrix size
    data = await loop.sock_recv(client,float_size)
    n = int(struct.unpack('f', data)[0])
    
    # Read matrix data
    need_bytes = 2*n*n*float_size
    print("processing ", n, "x", n)
    data = await loop.sock_recv(client,need_bytes)
    while len(data) < need_bytes:
        part = await loop.sock_recv(client,need_bytes - len(data))
        data += part
    # Check data validity
    if len(data) != need_bytes:
        print("Bad request, received: ", len(data), "expected: ", need_bytes)
        await loop.sock_sendall(client, data)
        return
    
    # Convert to float numpy
    floats = np.frombuffer(data, dtype=np.float32) 
    # print("floats", len(floats))
    A = floats[0:n*n].reshape(n,n)
    B = floats[n*n:].reshape(n,n)
    # print("   A",A.shape,"B",B.shape)
    
    
    # numpy multiplication:
    #C = A*B
    
    # pytorch/CUDA multiplication:
    A = torch.from_numpy(A).to("cuda")
    B = torch.from_numpy(B).to("cuda")
    C = (A*B).to("cpu").numpy()
    
    # Send response
    # print("   C",C.shape)
    response = C.tobytes()
    # print("response",len(response),n*n*4)
    await loop.sock_sendall(client, response)
    client.close()

# Running server
async def run_server():
    print("MatrixPython - Starting server for matrix multiplication on CUDA")
    print("    port:", port)
    server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    server.bind(('localhost', port))
    server.listen(listen_max_clients) # Limit of incoming clients
    server.setblocking(False)

    loop = asyncio.get_event_loop()

    while True:
        client, _ = await loop.sock_accept(server)
        loop.create_task(handle_client(client))

# Main
if (not torch.cuda.is_available()):
    print("No CUDA, exiting")
    exit
asyncio.run(run_server())

