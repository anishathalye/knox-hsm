module adder #(
    parameter WIDTH = 32
) (
    input clk,
    input resetn,
    input en,
    input [WIDTH-1:0] x,
    output reg [WIDTH-1:0] out,
);

reg [1:0] state;
reg [WIDTH-1:0] acc;
reg [63:0] count_cycle;

always @(posedge clk) begin
    count_cycle <= count_cycle + 1;

    if (!resetn) begin
        state <= 0;
        acc <= 0;
        count_cycle <= 0;
        out <= 0;
    end else begin
        if (en) begin
            if (state == 0) begin
                acc <= x;
                state <= 1;
            end else if (state == 1) begin
                out <= acc + x;
                state <= 2;
            end
        end
        if (state == 2) begin
            out <= count_cycle ^ count_cycle; // 0, but prevent it from getting optimized out
            state <= 0;
        end
    end
end

endmodule
