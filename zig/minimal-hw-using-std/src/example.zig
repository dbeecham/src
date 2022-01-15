const std = @import("std");

const msg = "Hello, World\n";

export fn _start() callconv(.Naked) noreturn {
    _ = std.os.linux.write(std.os.linux.STDOUT_FILENO, msg, msg.len);
    std.os.linux.exit(0);
}
