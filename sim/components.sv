`default_nettype none

`timescale 1ns/10ps

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
  input logic a, b, c
);
  nor
`ifdef TIMING
  #(0:5.2:7.9)
`endif
  (y, a, b, c);
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

// D Flipflop used within the 74163
module _counter_dff #(parameter init = 0) (
  input logic D_n, CP_neg,
  output logic Q, Q_n = ~init
);
  always_ff @(negedge CP_neg)
    Q_n <= D_n;

  assign Q = ~Q_n;

endmodule

module counter4_lv163 #(parameter init = 0) (
  input logic [3:0] D,
  input logic CET, CEP, PE_n, MR_n, CP,
  output logic [3:0] Q,
  output logic TC
);

  logic [3:0] D_n;
  assign D_n = ~D;

  logic mr_nor_npe, mr_nor_npe_nor_mr;
  nor g0(mr_nor_npe, PE_n, ~MR_n);
  nor g1(mr_nor_npe_nor_mr, mr_nor_npe, ~MR_n);

  logic cet_nand_cep;
  nand g2(cet_nand_cep, CET, CEP);
  
  logic [3:0] q, q_n, d_n;
  assign Q = ~q_n;

  assign d_n[0] = ~(
    (D_n[0] & mr_nor_npe)
    |
    (
      ~(~cet_nand_cep ^ q_n[0])
      &
      mr_nor_npe_nor_mr
    )
  );
  assign d_n[1] = ~(
    (D_n[1] & mr_nor_npe)
    |
    (
      ~(~(cet_nand_cep | q_n[0]) ^ q_n[1])
      &
      mr_nor_npe_nor_mr
    )
  );
  assign d_n[2] = ~(
    (D_n[2] & mr_nor_npe)
    |
    (
      ~(~(cet_nand_cep | q_n[0] | q_n[1]) ^ q_n[2])
      &
      mr_nor_npe_nor_mr
    )
  );
  logic x0;
  assign x0 = ~(cet_nand_cep | q_n[0] | q_n[1] | q_n[2]);
  assign d_n[3] = ~(
    (D_n[3] & mr_nor_npe)
    |
    (
      ((x0 & q_n[3]) | ~(x0 | q_n[3]))
      &
      mr_nor_npe_nor_mr
    )
  );

  _counter_dff #(.init(init)) ff0(.D_n(d_n[0]), .CP_neg(~CP), .Q(q[0]), .Q_n(q_n[0]));
  _counter_dff #(.init(init)) ff1(.D_n(d_n[1]), .CP_neg(~CP), .Q(q[1]), .Q_n(q_n[1]));
  _counter_dff #(.init(init)) ff2(.D_n(d_n[2]), .CP_neg(~CP), .Q(q[2]), .Q_n(q_n[2]));
  _counter_dff #(.init(init)) ff3(.D_n(d_n[3]), .CP_neg(~CP), .Q(q[3]), .Q_n(q_n[3]));

  and g3(TC, CET, q[0], q[1], q[2], q[3]);

// `ifdef TIMING
//   specify
//     (posedge C *> Q) = (0:6.1:10.1);
//     (posedge C *> RCO) = (0:6.6:10.1);
//   endspecify
// `endif

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
	output logic Q = init, Q_n
);

`ifdef TIMING
  localparam clk2q = (0:15:35);
`endif

  logic [1:0] op;
  assign op = {J, K_n};

	always_ff @(posedge C)
		case (op)
			2'b00: Q <=
`ifdef TIMING
        #clk2q
`endif
        1'b0;
			2'b10: Q <=
`ifdef TIMING
        #clk2q
`endif
        ~Q;
			2'b01: Q <= Q;
			2'b11: Q <=
`ifdef TIMING
        #clk2q
`endif
        1'b1;
		endcase

	assign Q_n = ~Q;

// `ifdef TIMING
//   // TODO: setup and hold
//   specify
//     pulsestyle_ondetect C;
//     if (op == 2'b01)(posedge C => (Q +: J, K_n)) = (0:15:35);
//   endspecify
// `endif

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

module dff8_hc377 #(parameter logic [7:0] init = '0) (
  output logic [7:0] q = init,
  input logic [7:0] d,
  input logic clk, en_n
);

  always_ff @(posedge clk)
    if (~en_n) q <=
`ifdef TIMING
      #(0:15:32)
`endif
      d;

endmodule

module or_lv32 (
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
`ifdef TIMING
  parameter oe_delay = (1.3:5.9:5.9);
`endif

  logic oe1_n_delayed, oe2_n_delayed;

  assign
`ifdef TIMING
    #oe_delay
`endif
    oe1_n_delayed = OE1_n;

  assign
`ifdef TIMING
    #oe_delay
`endif
    oe2_n_delayed = OE2_n;

	assign B1 = (~OE1_n) ? A : 'bz;
	assign B2 = (~OE2_n) ? A : 'bz;

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

module sram_as7c256 (
  input logic [14:0] A,
  inout tri [7:0] IO,
  input logic CS_n,
  input logic OE_n,
  input logic WE_n
);

  localparam BYTES = 32768;

  logic [7:0] contents [BYTES];

  initial for (int i = 0; i < BYTES; i++) contents[i] = i[7:0];

  always_latch begin
    if (~CS_n && ~WE_n)
      contents[A] = IO;
  end

  assign
`ifdef TIMING
    #(0:10:10)
`endif
    IO = (~CS_n && WE_n && ~OE_n) ? contents[A] : 'bz;

endmodule