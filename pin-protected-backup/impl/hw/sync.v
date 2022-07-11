module sync (
    input clk,
    input ext,
    output int,
);

reg [1:0] buffer;

always @(posedge clk) begin
    buffer[0] <= ext;
    buffer[1] <= buffer[0];
end

assign int = buffer[1];

endmodule
