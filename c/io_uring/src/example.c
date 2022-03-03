#define _DEFAULT_SOURCE

#include <stdio.h>
#include <syslog.h>
#include <errno.h>
#include <string.h>
#include <unistd.h>
#include <sys/mman.h>
#include <sys/syscall.h>
#include <sys/types.h>
#include <linux/io_uring.h>
#include <stdint.h>
#include <fcntl.h>
#include <unistd.h>


struct example_sq_ring {
    unsigned * head;
    unsigned * tail;
    unsigned * ring_mask;
    unsigned * ring_entries;
    unsigned * flags;
    unsigned * dropped;
    unsigned * array;
};

struct example_cq_ring {
    unsigned * head;
    unsigned * tail;
    unsigned * ring_mask;
    unsigned * ring_entries;
    struct io_uring_cqe *cqes;
};

struct example_s {
    int ring_fd;
    struct io_uring_params io_uring_params;
    struct example_sq_ring sq_ring;
    struct example_cq_ring cq_ring;
    int file_fd;
};


#ifndef io_uring_setup
int io_uring_setup(unsigned int entries, struct io_uring_params *p) {
    return syscall(__NR_io_uring_setup, entries, p);
}
#endif


int example_init_io_uring (
    struct example_s * example
)
{

    int ret = 0;
    struct io_uring_params io_uring_params = {0};
    void * sq_ptr = NULL;
    void * cq_ptr = NULL;

    example->ring_fd = io_uring_setup(64, &io_uring_params);
    if (-1 == example->ring_fd) {
        syslog(LOG_ERR, "%s:%d:%s: io_uring_params: %s", __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }

    int sring_size = io_uring_params.sq_off.array + io_uring_params.sq_entries * sizeof(uint32_t);
    int cring_size = io_uring_params.cq_off.cqes + io_uring_params.cq_entries * sizeof(struct io_uring_cqe);

    if (0 != (io_uring_params.features & IORING_FEAT_SINGLE_MMAP)) {
        if (sring_size < cring_size) {
            sring_size = cring_size;
        } else {
            cring_size = sring_size;
        }
    }

    sq_ptr = mmap(
        /* addr = */ NULL, 
        /* len = */ sring_size,
        /* prot = */ PROT_READ | PROT_WRITE,
        /* flags = */ MAP_SHARED | MAP_POPULATE,
        /* fd = */ example->ring_fd,
        /* offset = */ IORING_OFF_SQ_RING
    );
    if (MAP_FAILED == sq_ptr) {
        syslog(LOG_ERR, "%s:%d:%s: mmap: %s", __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }

    // map completion queue separately in older kernels
    if (0 != (io_uring_params.features & IORING_FEAT_SINGLE_MMAP)) {
        cq_ptr = sq_ptr;
    }
    else {
        cq_ptr = mmap(
            /* addr = */ NULL, 
            /* len = */ cring_size,
            /* prot = */ PROT_READ | PROT_WRITE,
            /* flags = */ MAP_SHARED | MAP_POPULATE,
            /* fd = */ example->ring_fd,
            /* offset = */ IORING_OFF_CQ_RING
        );
        if (MAP_FAILED == cq_ptr) {
            syslog(LOG_ERR, "%s:%d:%s: mmap: %s", __FILE__, __LINE__, __func__, strerror(errno));
            return -1;
        }
    }

    example->sq_ring = (struct example_sq_ring) {
        .head = sq_ptr + io_uring_params.sq_off.head,
        .tail = sq_ptr + io_uring_params.sq_off.tail,
        .ring_mask = sq_ptr + io_uring_params.sq_off.ring_mask,
        .ring_entries = sq_ptr + io_uring_params.sq_off.ring_entries,
        .flags = sq_ptr + io_uring_params.sq_off.flags,
        .dropped = sq_ptr + io_uring_params.sq_off.dropped,
        .array = sq_ptr + io_uring_params.sq_off.array,
    };

    example->cq_ring = (struct example_cq_ring) {
        .head = cq_ptr + io_uring_params.cq_off.head,
        .tail = cq_ptr + io_uring_params.cq_off.tail,
        .ring_mask = cq_ptr + io_uring_params.cq_off.ring_mask,
        .ring_entries = cq_ptr + io_uring_params.cq_off.ring_entries,
        .cqes = cq_ptr + io_uring_params.cq_off.cqes,
    };

    return 0;
}


int example_readv (
    struct example_s * example
)
{

    int ret = 0;

    example->file_fd = open("file", O_RDONLY);
    if (-1 == example->file_fd) {
        syslog(LOG_ERR, "%s:%d:%s: open: %s", __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }
    

    return 0;
}


int example_init (
    struct example_s * example
)
{

    int ret = 0;

    ret = example_init_io_uring(example);
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d:%s: example_init_io_uring returned -1", __FILE__, __LINE__, __func__);
        return -1;
    }

    return 0;
}


int main (
    int argc,
    char const* argv[]
)
{
    int ret = 0;
    struct example_s example = {0};

    openlog("example", LOG_CONS | LOG_PID, LOG_USER);

    syslog(LOG_DEBUG, "%s:%d:%s: hi!", __FILE__, __LINE__, __func__);

    ret = example_init(&example);
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d:%s: example_init returned -1", __FILE__, __LINE__, __func__);
        return -1;
    }
    
    return 0;
}
