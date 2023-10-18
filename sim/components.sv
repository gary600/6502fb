`default_nettype none

module BusMux_74CBT16390(
  inout logic [15:0] a, b1, b2;
  input logic oe1_n, oe2_n
);

  always_comb if (~oe1_n) begin
    a = b1;
    b1 = a;
  end
  else begin
    a = 'z;
    b1 = 'z;
  end

  always_comb if (~oe2_n) begin
    a = b2;
    b2 = a;
  end
  else begin
    a = 'z;
    b2 = 'z;
  end

endmodule

module Sram_7C256(
  input logic [14:0] addr,
  inout logic [7:0] data,
  
)