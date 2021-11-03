// Copyright (c) 2012-2013 Ludvig Strigeus
// This program is GPL Licensed. See COPYING for the full license.

`timescale 1ns / 1ps

// Asynchronous PSRAM controller for byte access
// After outputting a byte to read, the result is available 70ns later.
module MemoryController(
                 input clk,
                 input read_a,             // Set to 1 to read from RAM
                 input read_b,             // Set to 1 to read from RAM
                 input write,              // Set to 1 to write to RAM
                 input [23:0] addr,        // Address to read / write
                 input [7:0] din,          // Data to write
                 output reg [7:0] dout_a,  // Last read data a
                 output reg [7:0] dout_b,  // Last read data b
                 output reg busy,          // 1 while an operation is in progress

                 output reg MemOE,         // Output Enable. Enable when Low.
                 output reg MemWR,         // Write Enable. WRITE when Low.
                 output reg RamCS,         // Chip Enable. Active = LOW
                 output reg [23:0] MemAdr,
                 input [7:0] MemDB_in,
					  output [7:0] MemDB_out );
                 
    reg [7:0] data_to_write;
  assign MemDB_out = data_to_write;
  reg [1:0] cycles;
  reg r_read_a;
  
  always @(posedge clk) begin
    // Initiate read or write
    if (!busy) begin
      if (read_a || read_b || write) begin
        MemAdr <= addr[23:0];
        RamCS <= 0;    // 0 means active
        MemWR <= !(write != 0); // Active Low
        MemOE <= !(write == 0);
        busy <= 1;
        data_to_write <= din;
        cycles <= 0;
        r_read_a <= read_a;
      end else begin
        MemOE <= 1;
        MemWR <= 1;
        RamCS <= 1;
        busy <= 0;
        cycles <= 0;
      end
    end else begin
      if (cycles == 2) begin
        // Now we have waited 3x45 = 135ns, latch incoming data on read.
        if (!MemOE) begin
          if (r_read_a) dout_a <= MemDB_in;
          else dout_b <= MemDB_in;
        end
        MemOE <= 1; // Deassert Output Enable.
        MemWR <= 1; // Deassert Write
        RamCS <= 1; // Deassert Chip Select
        busy <= 0;
        cycles <= 0;
      end else begin
        cycles <= cycles + 1;
      end
    end
  end
endmodule  // MemoryController

// This parses iNES headers
module GameLoader(input clk, 
						input [127:0] ines_in,
                  output [31:0] mapper_flags,
                  output reg done);
  wire [7:0] ines[0:15]; // 16 bytes of iNES header
  
  assign ines[0][7:0] = ines_in[127:120];
  assign ines[1][7:0] = ines_in[119:112];
  assign ines[2][7:0] = ines_in[111:104];
  assign ines[3][7:0] = ines_in[103:96];
  assign ines[4][7:0] = ines_in[95:88];
  assign ines[5][7:0] = ines_in[87:80];
  assign ines[6][7:0] = ines_in[79:72];
  assign ines[7][7:0] = ines_in[71:64];
  assign ines[8][7:0] = ines_in[63:56];
  assign ines[9][7:0] = ines_in[55:48];
  assign ines[10][7:0] = ines_in[47:40];
  assign ines[11][7:0] = ines_in[39:32];
  assign ines[12][7:0] = ines_in[31:24];
  assign ines[13][7:0] = ines_in[23:16];
  assign ines[14][7:0] = ines_in[15:8];
  assign ines[15][7:0] = ines_in[7:0];
  
  wire [7:0] prgrom = ines[4];
  wire [7:0] chrrom = ines[5];
  
  wire [2:0] prg_size = prgrom <= 1  ? 0 :
                        prgrom <= 2  ? 1 : 
                        prgrom <= 4  ? 2 : 
                        prgrom <= 8  ? 3 : 
                        prgrom <= 16 ? 4 : 
                        prgrom <= 32 ? 5 : 
                        prgrom <= 64 ? 6 : 7;
                        
  wire [2:0] chr_size = chrrom <= 1  ? 0 : 
                        chrrom <= 2  ? 1 : 
                        chrrom <= 4  ? 2 : 
                        chrrom <= 8  ? 3 : 
                        chrrom <= 16 ? 4 : 
                        chrrom <= 32 ? 5 : 
                        chrrom <= 64 ? 6 : 7;
  
  wire [7:0] mapper = {ines[7][7:4], ines[6][7:4]};
  wire has_chr_ram = (chrrom == 0);
  assign mapper_flags = {16'b0, has_chr_ram, ines[6][0], chr_size, prg_size, mapper};
  always @(posedge clk) begin
	  done = 1;
  end
endmodule


module NES_SSDS3(input CLKNES,
                 input CPU_RESET,
					  input  [127:0] ines_in,
					  input [7:0] joypad1,
					  input [7:0] joypad2,
                 // VGA
                 output vga_v, output vga_h, output [3:0] vga_r, output [3:0] vga_g, output [3:0] vga_b,
					  output vga_clk,
                 // Memory
                 output MemOE,          // Output Enable. Enable when Low.
                 output MemWR,          // Write Enable. WRITE when Low.
                 output RamCS,          // Chip Enable. Active = LOW
                 output [23:0] MemAdr,
                 input [7:0] MemDB_in,
					  output [7:0] MemDB_out,
					  // Video signal
					  output pixel_clock,
					  output [8:0] cycle,
					  output [8:0] scanline,
					  output [14:0] pixel_out,
					  output [5:0] color_out,
  
                 // Sound board
					  output [15:0] audio_out,
					  output audio_clock,
                 output AUD_MCLK,
                 output AUD_LRCK,
                 output AUD_SCK,
                 output AUD_SDIN
                 );

  wire clock_locked;
  wire clk;
  assign clk = CLKNES;

  // NES Palette -> RGB332 conversion
  reg [14:0] pallut[0:63];
  initial $readmemh("nes_palette.txt", pallut);

  wire [8:0] cycle;
  wire [8:0] scanline;
  wire [15:0] sample;
  wire [5:0] color;
  wire joypad_strobe;
  wire [1:0] joypad_clock;
  wire [21:0] memory_addr;
  wire memory_read_cpu, memory_read_ppu;
  wire memory_write;
  wire [7:0] memory_din_cpu, memory_din_ppu;
  wire [7:0] memory_dout;
  reg [7:0] joypad_bits, joypad_bits2;
  reg [1:0] last_joypad_clock;
  wire [31:0] dbgadr;
  wire [1:0] dbgctr;

  reg [1:0] nes_ce;

  always @(posedge clk) begin
    if (joypad_strobe) begin
      joypad_bits <= joypad1;
      joypad_bits2 <= joypad2;
    end
    if (!joypad_clock[0] && last_joypad_clock[0])
      joypad_bits <= {1'b0, joypad_bits[7:1]};
    if (!joypad_clock[1] && last_joypad_clock[1])
      joypad_bits2 <= {1'b0, joypad_bits2[7:1]};
    last_joypad_clock <= joypad_clock;
  end
  
  wire [21:0] loader_addr;
  wire [7:0] loader_write_data;
  wire [31:0] mapper_flags;
  wire loader_done, loader_fail;
  GameLoader loader(clk, ines_in, mapper_flags, loader_done);

  wire reset_nes = CPU_RESET;
  wire run_mem = (nes_ce == 0) && !reset_nes;
  wire run_nes = (nes_ce == 3) && !reset_nes;
  
  assign pixel_clock = (nes_ce == 3);

  // NES is clocked at every 4th cycle.
  always @(posedge clk)
    nes_ce <= nes_ce + 1;
    
  NES nes(clk, reset_nes, run_nes,
          mapper_flags,
          sample, color,
          joypad_strobe, joypad_clock, {joypad_bits2[0], joypad_bits[0]},
          5'b11111,
          memory_addr,
          memory_read_cpu, memory_din_cpu,
          memory_read_ppu, memory_din_ppu,
          memory_write, memory_dout,
          cycle, scanline,
          dbgadr,
          dbgctr);

  // This is the memory controller to access the board's PSRAM
  wire ram_busy;
  MemoryController memory(clk,
                          memory_read_cpu && run_mem, 
                          memory_read_ppu && run_mem,
                          memory_write && run_mem || loader_write,
                          loader_write ? {2'b00, loader_addr} : {2'b00, memory_addr},
                          loader_write ? loader_write_data : memory_dout,
                          memory_din_cpu,
                          memory_din_ppu,
                          ram_busy,
                          MemOE, MemWR, RamCS, MemAdr, MemDB_in, MemDB_out);

  wire [14:0] doubler_pixel;
  wire doubler_sync;
  wire [9:0] vga_hcounter, doubler_x;
  wire [9:0] vga_vcounter;
  assign vga_clk = clk;
  
  VgaDriver vga(clk, vga_h, vga_v, vga_r, vga_g, vga_b, vga_hcounter, vga_vcounter, doubler_x, doubler_pixel, doubler_sync, 1);
  
  wire [14:0] pixel_in = pallut[color];
  assign color_out = color;
  assign pixel_out = pixel_in;
  assign audio_out = sample;
  
  Hq2x hq2x(clk, pixel_in, 1, 
            scanline[8],        // reset_frame
            (cycle[8:3] == 42), // reset_line
            doubler_x,          // 0-511 for line 1, or 512-1023 for line 2.
            doubler_sync,       // new frame has just started
            doubler_pixel);     // pixel is outputted

  wire [15:0] sound_signal = {sample[15] ^ 1'b1, sample[14:0]};
  wire [15:0] sound_signal_fir;
  wire sample_now_fir;
//  FirFilter fir(clk, sound_signal, sound_signal_fir, sample_now_fir);
  // Display mapper info on screen
//  always @(posedge clk) begin
//    led_enable <= 255;
//    led_value <= sound_signal;
//  end

  reg [7:0] sound_ctr;
  always @(posedge clk)
    sound_ctr <= sound_ctr + 1;
  wire sound_load = /*SW[6] ? sample_now_fir : */(sound_ctr == 0);
  assign audio_clock = sound_load;
  SoundDriver sound_driver(clk, 
      /*SW[6] ? sound_signal_fir : */sound_signal, 
      sound_load,
      sound_load,
      AUD_MCLK, AUD_LRCK, AUD_SCK, AUD_SDIN);

endmodule
