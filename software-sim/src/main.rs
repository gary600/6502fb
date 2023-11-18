mod cpu;

use std::{thread, sync::{mpsc::{channel, Receiver}, Mutex, Arc}, time::Duration, process::exit};

use cpu::{CpuContext, create_arch, CPU};

use minifb::{Window, WindowOptions, Scale};

pub struct Ctx {
    pub ram: [u8; 0x8000], // 32k
    pub eeprom: [u8; 0x2000], // 8k

    // Framebuffer
    pub vram: [u8; 0x10000], // 64k
    pub fb_config: u8,
}

impl Ctx {
    fn new(eeprom: [u8; 0x2000]) -> Ctx {
        Ctx {
            ram: [0u8; 0x8000],
            eeprom,
            vram: [0u8; 0x10000],
            fb_config: 0u8
        }
    }

    fn get_bank(&self) -> usize {
        ((self.fb_config & 0b00000010) >> 1) as usize
    }

    fn get_buf(&self) -> usize {
        (self.fb_config & 0b00000001) as usize
    }

    fn get_vram_address(&self, addr: u16) -> usize {
        (self.get_buf() << 15) | (self.get_bank() << 14) | (addr as usize)
    }
}

impl CpuContext for Ctx {
    fn read_immut(&self, addr: u16) -> u8 {
        match addr {
            0x0000 ..= 0x7FFF => self.ram[addr as usize],
            // Vram is write-only
            0xE000 ..= 0xFFFF => self.eeprom[(addr-0xE000) as usize],
            _ => 0x00
        }
    }

    fn read(&mut self, addr: u16) -> u8 { self.read_immut(addr) }

    fn write(&mut self, addr: u16, val: u8) {
        // println!("write: ${addr:04x} <- ${val:02x}");
        if addr == 0x08a0 {
            self.fb_config = val;
        }
        match addr {
            0x0000..=0x7FFF => self.ram[addr as usize] = val,
            0x8000..=0xBFFF => self.vram[self.get_vram_address(addr)] = val,
            // EEPROM is read-only
            _ => ()
        }
    }
}

impl Default for Ctx {
    fn default() -> Self {
        Ctx::new([0xEAu8; 0x2000])
    }
}

fn create_fb_thread(ctx: Arc<Mutex<Ctx>>) -> Receiver<()> {
    let (tx_nmi, rx_nmi) = channel::<()>();
    thread::spawn(move ||{
        let mut window = Window::new(
            "6502 Framebuffer Emulator",
            256, 256,
            WindowOptions {
                scale: Scale::X4,
                ..Default::default()
            }
        ).expect("Could not create window");
        
        println!("Created window");
        while window.is_open() {
            let ctx = ctx.lock().unwrap();
            // println!("frame");
            window.update_with_buffer(
                &ctx.vram.map(|x| {
                    let x = x as u32;
                    ((x & 0b11100000) << 16) |
                    ((x & 0b00011100) << (8 + 3)) |
                    ((x & 0b00000011) << 6)
                }),
                256,
                256
            ).unwrap();
            drop(ctx);
            thread::sleep(Duration::from_micros(15253));
            tx_nmi.send(()).unwrap();
            thread::sleep(Duration::from_micros(1400));
        }

        println!("FB thread shutting down");
        exit(0);
    });
    rx_nmi
}

fn main() {
    let prog = std::fs::read(std::env::args().nth(1).unwrap()).unwrap();
    let mut eeprom = [0u8; 0x2000];
    eeprom.copy_from_slice(&prog[2..]);
    eeprom[0x1FF0] = 0x4C;
    let arch = create_arch::<Ctx>();
    let ctx = Arc::new(Mutex::new(Ctx::new(eeprom)));
    let mut cpu = CPU::new();

    let rx_nmi = create_fb_thread(ctx.clone());

    // let mut cycle = 0u32;
    loop {
        let mut ctx = ctx.lock().unwrap();
        cpu.next(&mut *ctx, &arch);
        if rx_nmi.try_recv().is_ok() {
            cpu.trigger_nmi(&mut *ctx);
            // println!("NMI!");
        }
        drop(ctx);
        thread::sleep(Duration::from_micros(1));
        // println!("cycle {cycle}");
        // cycle += 1;
    }
}
