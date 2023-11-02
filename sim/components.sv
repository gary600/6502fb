`default_nettype none

`timescale 1ns/10ps

`define TIMING

// All timings as specified at Vdd = 4.5V or 5V, worst listed Cl

module xor_hc86 (
  output logic y,
  input logic a, b
);
  xor
`ifdef TIMING
  #(0:12:20)
`endif
  (y, a, b);
endmodule

module nand_hc00 (
  output logic y,
  input logic a, b
);
  nand
`ifdef TIMING
  #(0:9:18)
`endif
  (y, a, b);
endmodule

module nor3_lv27 (
  output logic y,
  input logic a, b
);
  nor
`ifdef TIMING
  #(0:5.2:7.9)
`endif
  (y, a, b);
endmodule

module nand8_ahc30 (
  output logic y,
  input logic [7:0] a
);
  nand
`ifdef TIMING
  #(0:4.9:8)
`endif
  (y, a[7], a[6], a[5], a[4], a[3], a[2], a[1], a[0]);
endmodule

// TODO: replicate functionality
module counter4_lv163 #(parameter logic [3:0] init = '0) ();

`ifdef TIMING
  specify
    (posedge C *> Q) = (0:6.1:10.1);
    (posedge C *> RCO) = (0:6.6:10.1);
  endspecify
`endif

endmodule

module not_lv04 (
  output logic y,
  input logic a
);
  not
`ifdef TIMING
  #(0:5.1:7.5)
`endif
  (y, a);
endmodule

module jknff_hc109 #(parameter init = 0) (
	input logic C, J, K_n,
	output logic Q, Q_n
);

	initial Q = init;
	always_ff @(posedge C)
		case ({ J, K_n })
			2'b00: Q <= 1'b0;
			2'b10: Q <= ~Q;
			2'b01: Q <= Q;
			2'b11: Q <= 1'b1;
		endcase

	assign Q_n = ~Q;

`ifdef TIMING
  specify
    (posedge C *> Q) = (0:15:35);
  endspecify
`endif

endmodule

module and_lv08 (
  output logic y,
  input logic a, b
);
  and
`ifdef TIMING
  #(0:9:11) // NOTE: No "typ" specified, took from lower voltage range
`endif
  (y, a, b);
endmodule

module dff8_hc377 (
  output logic [7:0] q,
  input logic [7:0] d,
  input logic clk, en_n
);

  always_ff @(posedge clk)
    if (~en_n) q <= d;

`ifdef TIMING
  specify
    (posedge clk *> d) = (0:15:32);
  endspecify
`endif
endmodule

module or_lv08 (
  output logic y,
  input logic a, b
);
  or
`ifdef TIMING
  #(0:8:10) // NOTE: No "typ" specified, took from lower voltage range
`endif
  (y, a, b);
endmodule



module demux_cbt16390 #(parameter WIDTH = 16) (
	input logic [WIDTH-1:0] A,
	output tri [WIDTH-1:0] B1,
	output tri [WIDTH-1:0] B2,
	input logic OE1_n, OE2_n
);
	assign B1 = (~OE1_n) ? A : 'bz;
	assign B2 = (~OE2_n) ? A : 'bz;

`ifdef TIMING
  specify
    // T_en
    (negedge OE1_n *> B1) = (1.3:5.9:5.9); // No Typ
    (negedge OE2_n *> B2) = (1.3:5.9:5.9); // No Typ
    // T_dis
    (posedge OE1_n *> B1) = (1.3:5.9:5.9); // No Typ
    (posedge OE2_n *> B2) = (1.3:5.9:5.9); // No Typ
  endspecify
`endif

endmodule

module mux_cbt16390 #(parameter WIDTH = 16) (
	input logic [WIDTH-1:0] B1,
	input logic [WIDTH-1:0] B2,
	output logic [WIDTH-1:0] A,
	input logic OE1_n, OE2_n
);

	always_comb begin
		A = 'x;
		if ({ OE1_n, OE2_n } == 2'b01)
			A = B1;
		else if ({ OE1_n, OE2_n } == 2'b10)
			A = B2;
	end

endmodule