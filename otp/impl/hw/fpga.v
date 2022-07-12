module fpga (
    input CLK,
    input BTN_N, // user button, pressed = 0
    input RX,
    input CTS,
    output TX,
    output RTS
);

reg [1:0] power_on_resetn_ctr;

initial begin
    power_on_resetn_ctr = 2'b11;
end

always @(posedge CLK) begin
    if (power_on_resetn_ctr != 2'b00) begin
        power_on_resetn_ctr <= power_on_resetn_ctr - 1;
    end
end

wire btn_n_internal;
sync sync_btn(
    .clk (CLK),
    .ext (BTN_N),
    .int (btn_n_internal)
);

wire resetn = btn_n_internal & (power_on_resetn_ctr == 2'b00);

wire rx_internal;
sync sync_rx(
    .clk (CLK),
    .ext (RX),
    .int (rx_internal)
);

wire cts_internal;
sync sync_cts(
    .clk (CLK),
    .ext (CTS),
    .int (cts_internal)
);

otp otp(
    .clk (CLK),
    .resetn (resetn),
    .rx (rx_internal),
    .cts (cts_internal),
    .tx (TX),
    .rts (RTS)
);

endmodule
