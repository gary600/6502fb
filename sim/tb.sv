`default_nettype none

`timescale 1ns/10ps

module FbTest;

	logic [15:0] AddrPhys;
	logic [7:0] DataIn;

	logic Phi2;
	logic RW_n;

  logic [2:0] Red, Green;
  logic [1:0] Blue;
  
	logic HSync_n;
	logic VSync_n;

	logic AddrSel;

  Fb fb(.*);

  assign AddrSel = AddrPhys[15];

  initial begin
    string file;
    if ($test$plusargs("DUMP")) begin
      if ($value$plusargs("DUMP=%s", file))
        $dumpfile(file);
      else
        $dumpfile("vga.vcd");
      
      $dumpvars(0, fb);
    end

    @(negedge fb.VBlank);
    @(negedge fb.VBlank);
    #1ms $finish;
  end

endmodule