`default_nettype none

`timescale 1ns/10ps

module FbTest;

	logic [15:0] AddrPhys;
	logic [7:0] DataIn;

	logic Phi2;
	logic RW_n;
  logic NMI_n;

  logic [2:0] Red, Green;
  logic [1:0] Blue;

	logic HSync_n;
	logic VSync_n;

	logic AddrSel;

  Fb fb(.*);

  assign AddrSel = AddrPhys[15]; // VRAM: 0x8000->0xBFFF

  logic [11:0] dump;
  assign dump = {
    fb.counters.PxClock, ~HSync_n, ~VSync_n,
    Red, Green, Blue, 1'b0
  };

  initial begin
    string file;

    if ($test$plusargs("DUMP")) begin
      if ($value$plusargs("DUMP=%s", file))
        $dumpfile(file);
      else
        $dumpfile("vga.vcd");    
    end
    
    if ($test$plusargs("SMALLDUMP")) begin
      $dumpvars(0, dump);
      $display("Small dump specified");
    end
    else
      $dumpvars(0, fb);

    @(negedge fb.VBlank);
    @(negedge fb.VBlank);
    #1ms $finish;
  end

  // Emulate some writes
  initial begin
    Phi2 = 0;
    #10us;
    forever #0.5us Phi2 = ~Phi2; // 1 MHz clock
  end
  initial begin
    AddrPhys = '0;
    DataIn = '0;
    RW_n = 1;

    // Write a zero to row 0, col 1
    @(posedge Phi2);
    AddrPhys <= 16'h8001;
    DataIn <= '0;
    RW_n <= 0;
    @(posedge Phi2);
    RW_n <= 1;
    @(posedge Phi2);

    // Enable NMI
    AddrPhys <= 16'h80a0;
    DataIn <= 8'h04;
    RW_n <= 0;
    @(posedge Phi2);
    RW_n <= 1;
    @(posedge Phi2);

    // Wait for NMI, then swap buffers
    @(negedge NMI_n);
    @(posedge Phi2);
    AddrPhys <= 16'h80a0; // config reg
    DataIn <= 8'h05;
    RW_n <= 0;
    @(posedge Phi2)
    RW_n <= 1;
  end

endmodule