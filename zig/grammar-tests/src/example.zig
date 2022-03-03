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

fn func() i32 {
    return -1;
}

pub fn main() u8 {

    const epollfd2 : i32 = switch (epoll_create1(1)) {
        error.INVAL => -1,
        error.NO => -1
    };
    const epollfd : i32 = epoll_create1(1) catch blk: {
        std.debug.print("no\n", .{});
        break :blk -1;
    };

    std.debug.print("epollfd = {} epollfd2 = {}\n", .{epollfd, epollfd2});
    _ = std.os.linux.write(std.os.linux.STDOUT_FILENO, "Hello, World\n", 13);
    return 0;
}
