use std::thread::{self, JoinHandle};
use std::net::{TcpListener, TcpStream, Shutdown};
use std::io::{Read, Write};
use byteorder::{ByteOrder, LittleEndian, BigEndian}; 
use std::sync::{mpsc, Arc};
use std::cmp;

extern crate num_cpus;
struct Worker {
    id: usize,
    thread: thread::JoinHandle<()>,
}

fn handle_client(mut stream: TcpStream) {
    let mut  num_cores = num_cpus::get();

    let mut data_size =  [0 as u8; 4];
    match stream.read(&mut data_size) {
        Ok(size) => {
            let raw_size = LittleEndian::read_f32(&mut data_size) as u32;
            let buffer_size = raw_size as usize;

            num_cores = cmp::min(num_cores, buffer_size);

            let mut data = vec![0 as u8; buffer_size * buffer_size * 2 * 4];
            let mut acc = 0;

            while match stream.read(&mut data[acc..buffer_size * buffer_size * 2 * 4]) {
                Ok(size) => {
                    acc += size;
                    if acc == buffer_size * buffer_size * 2 * 4
                    {
                        let first: &[f32] = unsafe {
                            std::slice::from_raw_parts(data.as_ptr() as *const f32, buffer_size * buffer_size)
                        };

                        let second: &[f32] = unsafe {
                            std::slice::from_raw_parts(data.as_ptr().offset(buffer_size as isize * buffer_size as isize) as *const f32, buffer_size * buffer_size)
                        };

                        let chunk_size = buffer_size as f32 / num_cores as f32;
                        let mut threads = Vec::with_capacity(num_cores);


                        let (tx, rx) =  mpsc::channel();
                        let amat1 = Arc::new(first.clone());
                        let amat2 = Arc::new(second.clone());

                        for thr in 0..num_cores
                        {
                            let index_begin = (thr as f32 * chunk_size) as usize;
                            let index_end = ((thr + 1) as f32 * chunk_size) as usize;

                            let tx = tx.clone();
                            let amat1 = Arc::clone(&amat1);
                            let amat2 = Arc::clone(&amat2);
                            threads.push(
                                thread::spawn(move || {
                                     let mut ans = vec![0 as f32; buffer_size * (index_end - index_begin)];
                                    for i in 0..index_end - index_begin
                                    {
                                        for j in 0..buffer_size
                                        {
                                            let mut acc: f32 = 0 as f32;
                                            for k in 0..buffer_size
                                            {
                                                let first_matrix_index = (i + index_begin) * buffer_size + k;
                                                let second_matrix_index = k * buffer_size + j;
                                                acc += amat1[first_matrix_index] * amat2[second_matrix_index];
                                            }


                                            ans[i * buffer_size + j] = acc;    
                                        }
                                    }

                                    tx.send((ans)).unwrap();
                                })
                            );
                        }
                        
                        
                        let mut result = Vec::new();
                        for v in rx.iter().take(num_cores){
                            let (mat) = v;
                            result.extend(mat);
                        }
                        for thr in threads
                        {
                            thr.join().unwrap();
                        }

                        let output: &[u8] = unsafe {
                            std::slice::from_raw_parts(result.as_ptr() as *const u8, result.len() * 4)
                        };

                        stream.write(output).unwrap();
                        false
                    }
                    else {
                        true
                    }
                },
                Err(_) => {
                    println!("[Read matrices] An error occurred, terminating connection with {}", stream.peer_addr().unwrap());
                    stream.shutdown(Shutdown::Both).unwrap();
                    false
                }
                }{}
            },
        Err(_) => {
            println!("[Read size] An error occurred, terminating connection with {}", stream.peer_addr().unwrap());
            stream.shutdown(Shutdown::Both).unwrap();
        }
    }
}

fn main() {
    let listener = TcpListener::bind("127.0.0.1:27015").unwrap();
    println!("Server listening on port 27015");
    for stream in listener.incoming() {
        match stream {
            Ok(stream) => {
                thread::spawn(move|| {
                    handle_client(stream)
                });
            }
            Err(e) => {
                println!("Error: {}", e);
            }
        }
    }

    drop(listener);
}