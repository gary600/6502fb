`default_nettype none

`timescale 1ns/10ps

module Fb (
	input logic [15:0] AddrPhys,	// CPU address bus
	input logic [7:0] DataIn,		// CPU data bus

	input logic Phi2, // PHI2
	input logic RW_n,	// ???

	output logic HSync,
	output logic VSync,

	input logic AddrSel // assign to AddrPhys[14] or [15]
);

	logic BankSel, BufSel;

	logic [14:0] AddrCPU;
	assign AddrCPU = { BankSel, AddrPhys[13:0] };

	logic RegSel_n;
	nand u32d(RegSel_n, AddrCPU[5], AddrCPU[7]);

	logic U32_Pad4;
	logic EnableCPU_n;
	xor u6a(U32_Pad4, AddrPhys[14], AddrPhys[15]);
	nand u32b(EnableCPU_n, U32_Pad4, AddrSel);

	logic WriteMem;
	nor u35b(WriteMem, EnableCPU_n, Phi2, RW_n);

	logic WriteReg_n;
	logic U35_Pad6;
	nor u35c(U35_Pad6, EnableCPU_n, RegSel_n, RW_n);
	xor u6c(WriteReg_n, U35_Pad6, 1'b1);

	logic ConfigReg_Clock;
	xor u6b(ConfigReg_Clock, Phi2, 1'b1);

	logic VBlank, HBlank;

	logic NmiEnable;
	logic NMI_n;
	nand u32a(NMI_n, NmiEnable, VBlank);

	logic BlankBlack_n;
	nor u35a(BlankBlack_n, HBlank, VBlank, ConfigReg_BlankBlack);

	logic ConfigReg_BlankBlack, ConfigReg_BlankWhite;

	ConfigReg config_reg(
		.DataIn,
		.WriteReg_n,
		.Clock(ConfigReg_Clock),
		.BankSel, .BufSel,
		.BlankBlack(ConfigReg_BlankBlack), .BlankWhite(ConfigReg_BlankWhite),
		.NmiEnable
	);

	logic [9:0] Col;
	logic [9:0] Row;

	Counters counters(
		.Col, .Row,
		.HBlank, .VBlank, .HSync, .VSync
	);

	logic [7:0] PixelOut;

	Memory memory(
		.AddrCPU, .DataIn,
		.Col, .Row,
		.WriteMem,
		.BufSel,
		.PixelOut
	);
	
endmodule

module Memory (
	input logic [14:0] AddrCPU,
	input logic [7:0] DataIn,

	input logic WriteMem,
	input logic BufSel,

	input logic [9:0] Col,
	input logic [9:0] Row,

	output logic [7:0] PixelOut
);

	logic BufSel_n, WeA_n, WeB_n;
	nand u7a(BufSel_n, BufSel, BufSel);
	nand u7b(WeA_n, BufSel_n, WriteMem);
	nand u7c(WeB_n, BufSel,   WriteMem);

	tri [15:0] AddrA;
	tri [15:0] AddrB;
	BusDemux u3(.A({ 1'b0, AddrCPU }),            .B1(AddrA), .B2(AddrB), .OE1_n(BufSel), .OE2_n(BufSel_n));
	BusDemux u4(.A({ 1'b0, Row[8:2], Col[9:2] }), .B1(AddrB), .B2(AddrA), .OE1_n(BufSel), .OE2_n(BufSel_n));

	tri [7:0] DataA;
	tri [7:0] DataB;
	Sram buffer_a(.Addr(AddrA[14:0]), .Data(DataA), .CS_n(1'b0), .OE_n(BufSel_n), .WE_n(WeA_n));
	Sram buffer_b(.Addr(AddrB[14:0]), .Data(DataB), .CS_n(1'b0), .OE_n(BufSel),   .WE_n(WeB_n));

	BusDemux #(8) u33_data(.A(DataIn), .B1(DataA), .B2(DataB), .OE1_n(BufSel), .OE2_n(BufSel_n));
	BusMux #(8) u33_addr(.A(PixelOut), .B1(DataB), .B2(DataA), .OE1_n(BufSel), .OE2_n(BufSel_n));

endmodule

module Sram(
	input logic [14:0] Addr,
	inout tri [7:0] Data,
	input logic CS_n,
	input logic OE_n,
	input logic WE_n
);

	logic [7:0] contents[32768];

	always @(*) begin
		if (~CS_n && ~WE_n)
			contents[Addr] = Data;
	end

	assign Data = (~CS_n && WE_n && ~OE_n) ? contents[Addr] : 'bz;

endmodule

module BusDemux #(parameter WIDTH=16) (
	input logic [WIDTH-1:0] A,
	output tri [WIDTH-1:0] B1,
	output tri [WIDTH-1:0] B2,
	input logic OE1_n, OE2_n
);
	assign B1 = (~OE1_n) ? A : 'bz;
	assign B2 = (~OE2_n) ? A : 'bz;

endmodule

module BusMux #(parameter WIDTH=16) (
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

module JKN_FlipFlop #(parameter init = 0)(
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

endmodule

module Counters (
	output logic [9:0] Col,
	output logic [9:0] Row,

	output logic HSync, VSync,
	output logic HBlank, VBlank
);

	logic PxClock;
	initial begin
		PxClock = 1'b0;
		forever #19.86 PxClock = ~PxClock; // 19.86 * 2 == 39.721ns, which is 25.175MHz
	end

	/* HSync and HBlank Logic */

	logic HBlankSet_n;
	nand u14a(HBlankSet_n, Col[0], Col[1], Col[2], Col[3], Col[4], Col[5], Col[6], Col[9]);

	logic HBlankClear_n, HBlankClear;
	nand u18a(HBlankClear_n, Col[0], Col[1], Col[2], Col[3], Col[4], Col[8], Col[9], 1'b1);
	not u10c(HBlankClear, HBlankClear_n);

	JKN_FlipFlop #(.init(1)) u11a(.Q_n(HBlank), .J(HBlankClear), .K_n(HBlankSet_n), .C(PxClock));

	logic Col5_n, Col6_n, U10_Pad9, HSyncSet;
	nand u16a(U10_Pad9, Col[0], Col[1], Col[2], Col[3], Col5_n, Col6_n, Col[7], Col[9]);
	not u10d(HSyncSet, U10_Pad9);

	logic HSyncClear_n;
	nand u19a(HSyncClear_n, Col[0], Col[1], Col[2], Col[3], Col[5], Col[6], Col[7], Col[9]);

	JKN_FlipFlop u11b(.Q(HSync), .J(HSyncSet), .K_n(HSyncClear_n), .C(PxClock));

	not u10a(Col5_n, Col[5]);
	not u10b(Col6_n, Col[6]);

	/* VSync and VBlank Logic */

	logic U23_Pad1, U10_Pad13, VBlankSet;
	and u29a(U23_Pad1, HBlankClear, Row[0]);
	nand u23a(U10_Pad13, U23_Pad1, Row[1], Row[2], Row[3], Row[4], Row[6], Row[7], Row[8]);
	not u10f(VBlankSet, U10_Pad13);

	logic VBlankClear_n;
	nand u25a(VBlankClear_n, HBlankClear, Row[2], Row[3], Row[9], 1'b1, 1'b1, 1'b1, 1'b1);

	JKN_FlipFlop u24a(.Q(VBlank), .J(VBlankSet), .K_n(VBlankClear_n), .C(PxClock));

	logic Row1_n, Row2_n, Row4_n;
	not u28a(Row1_n, Row[1]);
	not u28b(Row2_n, Row[2]);
	not u28c(Row4_n, Row[4]);


	logic U29_Pad1, U29_Pad6, U26_Pad8, VSyncSet;
	and u29b(U29_Pad1, HBlankClear, Row[0]);
	and u29c(U29_Pad6, Row[5], Row[6]);
	nand u26a(U26_Pad8, U29_Pad1, Row1_n, Row2_n, Row[3], Row4_n, U29_Pad6, Row[7], Row[8]);
	not u28d(VSyncSet, U26_Pad8);

	logic VSyncClear_n;
	nand u27a(VSyncClear_n, HBlankClear, Row[0], Row[1], Row[3], Row[5], Row[6], Row[7], Row[8]);

	JKN_FlipFlop u24b(.Q(VSync), .J(VSyncSet), .K_n(VSyncClear_n), .C(PxClock));

	/* Column Counter */

	initial Col = '0;
	always_ff @(posedge PxClock)
		if (~HBlankClear_n)
			Col <= 10'd0;
		else
			Col <= Col + 10'd1;
	
	/* Row Counter */

	initial Row = '0;
	always_ff @(posedge PxClock)
		if (~VBlankClear_n)
			Row <= 10'd0;
		else if (HBlankClear)
			Row <= Row + 10'd1;

endmodule

module ConfigReg (
	input logic [7:0] DataIn,

	input logic WriteReg_n,
	input logic Clock,

	output logic BankSel, BufSel,
	output logic BlankBlack, BlankWhite,
	output logic NmiEnable
);

	logic [7:0] state;

	assign BufSel     = state[0];
	assign BankSel    = state[1];
	assign NmiEnable  = state[2];
	assign BlankWhite = state[3];
	assign BlankBlack = state[4];

	always_ff @(posedge Clock)
		if (~WriteReg_n)
			state <= DataIn;

endmodule
