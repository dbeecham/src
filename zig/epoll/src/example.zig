const std = @import("std");
const linux = std.os.linux;
const errno = std.os.linux.getErrno;
const EPOLL = std.os.linux.EPOLL;


const EpollCreateError = error{
    EINVAL,
    NO
};


fn epoll_create1(flags: u32) EpollCreateError!i32 {
    const rc : usize = linux.epoll_create1(flags);
    switch (errno(rc)) {
        .SUCCESS => return @intCast(i32, rc),
        .INVAL => return error.EINVAL,
        else => |_| return error.NO
    }
}

// i dont know the type of the error yet...
inline fn func(source: std.builtin.SourceLocation, _: anytype) i32 {
    std.debug.print("{s}, {s}, {}\n", .{source.file, source.fn_name, source.line});
    return -1;
}

pub fn main() u8 {
    const epollfd: i32 = epoll_create1(1) catch |err| func(@src(), err);

    // Dont do this, std.os.epoll_create1 has a "catch unreachable on INVAL
//    const epollfd2: i32 = std.os.epoll_create1(1) catch |e| {
//        const source = @src();
//        std.debug.print("err={}, {s}, {s}, {}\n", .{e, source.file, source.fn_name, source.line});
//        return 0;
//    };

    const epollfd2: i32 = epoll_create1(1) catch |e| blk: {
        const source = @src();
        std.debug.print("err={}, {s}, {s}, {}\n", .{e, source.file, source.fn_name, source.line});
        break :blk -1;
    };

    std.debug.print("epollfd = {} epollfd2 = {}\n", .{epollfd, epollfd2});
    _ = std.os.linux.write(std.os.linux.STDOUT_FILENO, "Hello, World\n", 13);
    return 0;
}
