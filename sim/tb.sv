`default_nettype none

`timescale 1ns/10ps

module FbTest;

	logic [15:0] AddrPhys;
	logic [7:0] DataIn;

	logic Phi2;
	logic RW_n;

	logic HSync;
	logic VSync;

	logic AddrSel;

  Fb fb(.*);

  assign AddrSel = AddrPhys[15];

  initial begin
    $dumpfile("vga.vcd");
    $dumpvars(0, fb);

    @(negedge fb.VBlank);
    #20000000 $finish;
  end

endmodule