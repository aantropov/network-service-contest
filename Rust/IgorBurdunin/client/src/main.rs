use std::net::{TcpStream};
use std::io::{Read, Write};
use std::time::{Instant};
use std::cmp;
use std::ptr;

use rand::Rng;

fn main() {
    let tries: u32 = 128;
    let mut acc: u128 = 0;
    let mut acc_size: u32 = 0;
    let mut max_time: u128 = 0;
	for i in 0..tries
	{
        let (time, size) = ClientTask();
        acc_size += size;
        max_time = cmp::max(max_time, time);

        acc += max_time;
    }
    println!("tries - {}; acc - {}; max_time = {}; avg_time - {}; avg_size - {}", tries, acc, max_time, acc as f32 / tries as f32, acc_size as f32 / tries as f32);
    println!("Terminated.");
}

fn ClientTask() -> (u128, u32)
{
    match TcpStream::connect("127.0.0.1:27015") {
        Ok(mut stream) => {
            let mut rng = rand::thread_rng();
            let mut size: u32  = 200 + rng.gen::<u32>() % 200;

            let mut msg = vec![0 as f32; 1 + size as usize * size as usize * 2];
            let ptr_msg = msg.as_mut_ptr();
            msg[0] = size as f32;

            let buffer_size = size as usize * size as usize;
            for i in 1..buffer_size
            {
                msg[i] = rng.gen();
            } 
        
            let unstructured_vec: &[u8] = unsafe {
                std::slice::from_raw_parts(msg.as_ptr() as *const u8, msg.len() * 4)
            };

            let now = Instant::now();
            stream.write(unstructured_vec).unwrap();

            let mut output = vec![0 as u8; size as usize * size as usize * 4];
            let mut acc = 0;

            while match stream.read(&mut output[acc..size as usize * size as usize * 4]) {
                Ok(sz) => {
                    acc += sz;
                    if acc == size as usize * size as usize * 4
                    {
                        println!("Reply is ok!");
                        false
                    }
                    else
                    {
                        true
                    }
                },
                Err(e) => {
                    println!("Failed to receive data: {}", e);
                    false
                }
            }{}

            let time = Instant::now() - now;
            return (time.as_millis(), size);
        },
        Err(e) => {
            println!("Failed to connect: {}", e);
        }
    }
    return (0, 0);
}