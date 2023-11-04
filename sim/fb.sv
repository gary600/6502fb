`default_nettype none

`timescale 1ns/10ps

module Fb (
  input logic [15:0] AddrPhys,  // CPU address bus
  input logic [7:0] DataIn,    // CPU data bus

  input logic Phi2, // PHI2
  input logic RW_n,  // ???

	output logic [2:0] Red, Green,
	output logic [1:0] Blue,

  output logic HSync_n,
  output logic VSync_n,

  input logic AddrSel // assign to AddrPhys[14] or [15] to select address
);

  logic BankSel, BufSel;

  logic [14:0] AddrCPU;
  assign AddrCPU = { BankSel, AddrPhys[13:0] };

  logic RegSel_n;
  nand_hc00 u32d(RegSel_n, AddrCPU[5], AddrCPU[7]);

  logic U32_Pad4;
  logic EnableCPU_n;
  xor_hc86 u6a(U32_Pad4, AddrPhys[14], AddrPhys[15]);
  nand_hc00 u32b(EnableCPU_n, U32_Pad4, AddrSel);

  logic WriteMem;
  nor3_lv27 u35b(WriteMem, EnableCPU_n, Phi2, RW_n);

  logic WriteReg_n;
  logic U35_Pad6;
  nor3_lv27 u35c(U35_Pad6, EnableCPU_n, RegSel_n, RW_n);
  xor_hc86 u6c(WriteReg_n, U35_Pad6, 1'b1);

  logic ConfigReg_Clock;
  xor_hc86 u6b(ConfigReg_Clock, Phi2, 1'b1);

  logic VBlank, HBlank;

  logic NmiEnable;
  logic NMI_n;
  nand_hc00 u32a(NMI_n, NmiEnable, VBlank);

  logic BlankBlack_n, BlankWhite;
  nor3_lv27 u35a(BlankBlack_n, HBlank, VBlank, ConfigReg_BlankBlack);

  logic ConfigReg_BlankBlack;

  ConfigReg config_reg(
    .DataIn,
    .WriteReg_n,
    .Clock(ConfigReg_Clock),
    .BankSel, .BufSel,
    .BlankBlack(ConfigReg_BlankBlack), .BlankWhite,
    .NmiEnable
  );

  logic [9:0] Col, Row;

  Counters counters(
    .Col, .Row,
    .HBlank, .VBlank, .HSync_n, .VSync_n
  );

  logic [7:0] PixelOut;

  Memory memory(
    .AddrCPU, .DataIn,
    .Col, .Row,
    .WriteMem,
    .BufSel,
    .PixelOut
  );

	Dac dac(
		.PixelOut,
		.BlankBlack_n, .BlankWhite,
		.Red, .Green, .Blue
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
  nand_hc00 u7a(BufSel_n, BufSel, BufSel);
  nand_hc00 u7b(WeA_n, BufSel_n, WriteMem);
  nand_hc00 u7c(WeB_n, BufSel,   WriteMem);

  tri [15:0] AddrA;
  tri [15:0] AddrB;
  demux_cbt16390 u3(.A({1'b0, AddrCPU}),            .B1(AddrA), .B2(AddrB), .OE1_n(BufSel), .OE2_n(BufSel_n));
  demux_cbt16390 u4(.A({1'b0, Row[8:2], Col[9:2]}), .B1(AddrB), .B2(AddrA), .OE1_n(BufSel), .OE2_n(BufSel_n));

  tri [7:0] DataA;
  tri [7:0] DataB;
  sram_as7c256 buffer_a(.A(AddrA[14:0]), .IO(DataA), .CS_n(1'b0), .OE_n(BufSel_n), .WE_n(WeA_n));
  sram_as7c256 buffer_b(.A(AddrB[14:0]), .IO(DataB), .CS_n(1'b0), .OE_n(BufSel),   .WE_n(WeB_n));

  demux_cbt16390 #(8) u33_data(.A(DataIn), .B1(DataA), .B2(DataB), .OE1_n(BufSel), .OE2_n(BufSel_n));
  mux_cbt16390 #(8) u33_addr(.A(PixelOut), .B1(DataB), .B2(DataA), .OE1_n(BufSel), .OE2_n(BufSel_n));

endmodule

module Counters (
  output logic [9:0] Col,
  output logic [9:0] Row,

  output logic HSync_n, VSync_n,
  output logic HBlank, VBlank
);

  logic PxClock;
  initial begin
    PxClock = 1'b0;
    forever #19.86ns PxClock = ~PxClock; // 19.86 * 2 == 39.721ns, which is 25.175MHz
  end

  /* HSync and HBlank Logic */

  logic HBlankSet_n;
  nand8_ahc30 u14a(HBlankSet_n, {Col[0], Col[1], Col[2], Col[3], Col[4], Col[5], Col[6], Col[9]});

  logic HBlankClear_n, HBlankClear;
  nand8_ahc30 u18a(HBlankClear_n, {Col[0], Col[1], Col[2], Col[3], Col[4], Col[8], Col[9], 1'b1});
  not_lv04 u10c(HBlankClear, HBlankClear_n);

  jknff_hc109 #(.init(1)) u11a(.Q_n(HBlank), .J(HBlankClear), .K_n(HBlankSet_n), .C(PxClock), .Q());

  logic Col5_n, Col6_n, U10_Pad9, HSyncSet;
  nand8_ahc30 u16a(U10_Pad9, {Col[0], Col[1], Col[2], Col[3], Col5_n, Col6_n, Col[7], Col[9]});
  not_lv04 u10d(HSyncSet, U10_Pad9);

  logic HSyncClear_n;
  nand8_ahc30 u19a(HSyncClear_n, {Col[0], Col[1], Col[2], Col[3], Col[5], Col[6], Col[7], Col[9]});

  jknff_hc109 u11b(.Q_n(HSync_n), .J(HSyncSet), .K_n(HSyncClear_n), .C(PxClock), .Q());

  not_lv04 u10a(Col5_n, Col[5]);
  not_lv04 u10b(Col6_n, Col[6]);

  /* VSync and VBlank Logic */

  logic U23_Pad1, U10_Pad13, VBlankSet;
  and_lv08 u29a(U23_Pad1, HBlankClear, Row[0]);
  nand8_ahc30 u23a(U10_Pad13, {U23_Pad1, Row[1], Row[2], Row[3], Row[4], Row[6], Row[7], Row[8]});
  not_lv04 u10f(VBlankSet, U10_Pad13);

  logic VBlankClear_n;
  nand8_ahc30 u25a(VBlankClear_n, {HBlankClear, Row[2], Row[3], Row[9], 1'b1, 1'b1, 1'b1, 1'b1});

  jknff_hc109 u24a(.Q(VBlank), .J(VBlankSet), .K_n(VBlankClear_n), .C(PxClock), .Q_n());

  logic Row1_n, Row2_n, Row4_n;
  not_lv04 u28a(Row1_n, Row[1]);
  not_lv04 u28b(Row2_n, Row[2]);
  not_lv04 u28c(Row4_n, Row[4]);


  logic U29_Pad1, U29_Pad6, U26_Pad8, VSyncSet;
  and_lv08 u29b(U29_Pad1, HBlankClear, Row[0]);
  and_lv08 u29c(U29_Pad6, Row[5], Row[6]);
  nand8_ahc30 u26a(U26_Pad8, {U29_Pad1, Row1_n, Row2_n, Row[3], Row4_n, U29_Pad6, Row[7], Row[8]});
  not_lv04 u28d(VSyncSet, U26_Pad8);

  logic VSyncClear_n;
  nand8_ahc30 u27a(VSyncClear_n, {HBlankClear, Row[0], Row[1], Row[3], Row[5], Row[6], Row[7], Row[8]});

  jknff_hc109 u24b(.Q_n(VSync_n), .J(VSyncSet), .K_n(VSyncClear_n), .C(PxClock), .Q());

  /* Column Counter */
  logic ctc0, ctc1;
  logic [3:0] ccol2_out;
  counter4_lv163 ccol0(.D(4'h0), .Q(Col[3:0]), .TC(ctc0), .PE_n(1'b1), .CEP(1'b1), .CET(1'b1), .CP(PxClock), .MR_n(HBlankClear_n));
  counter4_lv163 ccol1(.D(4'h0), .Q(Col[7:4]), .TC(ctc1), .PE_n(1'b1), .CEP(ctc0), .CET(1'b1), .CP(PxClock), .MR_n(HBlankClear_n));
  counter4_lv163 ccol2(.D(4'h0), .Q(ccol2_out), .TC(), .PE_n(1'b1), .CEP(ctc0), .CET(ctc1), .CP(PxClock), .MR_n(HBlankClear_n));
  assign Col[9:8] = ccol2_out[1:0];
  
  /* Row Counter */
  logic rtc0, rtc1;
  logic [3:0] rcol2_out;
  counter4_lv163 rcol0(.D(4'h0), .Q(Row[3:0]), .TC(rtc0), .PE_n(1'b1), .CEP(1'b1), .CET(HBlankClear), .CP(PxClock), .MR_n(VBlankClear_n));
  counter4_lv163 rcol1(.D(4'h0), .Q(Row[7:4]), .TC(rtc1), .PE_n(1'b1), .CEP(rtc0), .CET(1'b1), .CP(PxClock), .MR_n(VBlankClear_n));
  counter4_lv163 rcol2(.D(4'h0), .Q(rcol2_out), .TC(), .PE_n(1'b1), .CEP(rtc0), .CET(rtc1), .CP(PxClock), .MR_n(VBlankClear_n));
  assign Row[9:8] = rcol2_out[1:0];

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

  dff8_hc377 r (
    .d(DataIn),
    .q(state),
    .clk(Clock),
    .en_n(WriteReg_n)
  );

endmodule

module Dac ( // not really but it does the stuff the DAC sheet does
	input logic [7:0] PixelOut,
	input logic BlankBlack_n, BlankWhite,
	output logic [2:0] Red, Green,
	output logic [1:0] Blue
);

	logic [7:0] pixel_white, pixel_black;

	or_lv32 g0(pixel_white[0], PixelOut[0], BlankWhite);
	or_lv32 g1(pixel_white[1], PixelOut[1], BlankWhite);
	or_lv32 g2(pixel_white[2], PixelOut[2], BlankWhite);
	or_lv32 g3(pixel_white[3], PixelOut[3], BlankWhite);
	or_lv32 g4(pixel_white[4], PixelOut[4], BlankWhite);
	or_lv32 g5(pixel_white[5], PixelOut[5], BlankWhite);
	or_lv32 g6(pixel_white[6], PixelOut[6], BlankWhite);
	or_lv32 g7(pixel_white[7], PixelOut[7], BlankWhite);

	and_lv08 g8 (pixel_black[0], pixel_white[0], BlankBlack_n);
	and_lv08 g9 (pixel_black[1], pixel_white[1], BlankBlack_n);
	and_lv08 g10(pixel_black[2], pixel_white[2], BlankBlack_n);
	and_lv08 g11(pixel_black[3], pixel_white[3], BlankBlack_n);
	and_lv08 g12(pixel_black[4], pixel_white[4], BlankBlack_n);
	and_lv08 g13(pixel_black[5], pixel_white[5], BlankBlack_n);
	and_lv08 g14(pixel_black[6], pixel_white[6], BlankBlack_n);
	and_lv08 g15(pixel_black[7], pixel_white[7], BlankBlack_n);

	assign {Red, Green, Blue} = pixel_black;

endmodule