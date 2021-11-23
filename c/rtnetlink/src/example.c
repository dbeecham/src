#define _DEFAULT_SOURCE

#include <asm/types.h>
#include <sys/socket.h>
#include <sys/epoll.h>
#include <sys/ioctl.h>
#include <linux/netlink.h>
#include <linux/rtnetlink.h>
#include <arpa/inet.h>
#include <net/if.h>
#include <netinet/in.h>
#include <string.h>
#include <errno.h>
#include <syslog.h>
#include <unistd.h>
#include <fcntl.h>


#define EXAMPLE_SENTINEL 8090
#define EXAMPLE_EPOLLFD_SENTINEL 8091


struct example_epollfd_s {
    int sentinel;
    enum {
        EXAMPLE_EPOLLFD_TYPE_NETLINK
    } type;
    int fd;
};

struct example_s {
    int sentinel;
    int epollfd;
    struct example_epollfd_s epollfds[8];
};


int example_init (
    struct example_s * example
)
{

    int ret = 0;
    int netlinkfd = 0;

    example->sentinel = EXAMPLE_SENTINEL;

    // Create the epoll instance
    example->epollfd = epoll_create1(EPOLL_CLOEXEC);
    if (-1 == example->epollfd) {
        syslog(LOG_ERR, "%s:%d:%s: epoll_create1: %s", __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }


    // Open the rtnetlink interface
    netlinkfd = socket(AF_NETLINK, SOCK_RAW, NETLINK_ROUTE);
    if (-1 == netlinkfd) {
        syslog(LOG_ERR, "%s:%d:%s: socket: %s", __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }

    // Bind the netlink socket
    struct sockaddr_nl sa = {
        .nl_family = AF_NETLINK,
        .nl_groups = RTMGRP_LINK | RTMGRP_IPV4_IFADDR | RTMGRP_IPV6_IFADDR
    };

    ret = bind(netlinkfd, (struct sockaddr *)&sa, sizeof(sa));
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d:%s: bind: %s", __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }


    // Add the fd to the list of fds in the app struct (this is given to epoll
    // so that we can dispatch on it later)
    example->epollfds[0] = (struct example_epollfd_s) {
        .sentinel = EXAMPLE_EPOLLFD_SENTINEL,
        .type = EXAMPLE_EPOLLFD_TYPE_NETLINK,
        .fd = netlinkfd
    };


    // Add the netlink fd to epoll
    ret = epoll_ctl(
        example->epollfd,
        EPOLL_CTL_ADD,
        netlinkfd,
        &(struct epoll_event){
            .events = EPOLLIN | EPOLLERR | EPOLLHUP | EPOLLONESHOT,
            .data = {
                .ptr = &example->epollfds[0]
            }
        }
    );
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d:%s: epoll_ctl: %s", __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }
        

    return 0;
}


int example_epoll_event_netlink_newlink (
    struct example_s * example,
    struct epoll_event * event,
    struct example_epollfd_s * example_epollfd,
    struct ifinfomsg * ifi
)
{

    // this is called when an interface gets link info, but its also called
    // when link is removed from an interface. You need to check the flags to
    // see what state it's in.

    int ret = 0;

    syslog(LOG_INFO, "%s:%d:%s: interface %d just got link, type=%d, flags=%d, family=%d",
            __FILE__, __LINE__, __func__, ifi->ifi_index, ifi->ifi_type, ifi->ifi_flags, ifi->ifi_family);

    if (IFF_UP == (IFF_UP & ifi->ifi_flags)) {
        syslog(LOG_INFO, "%s:%d:%s: its up", __FILE__, __LINE__, __func__);
    }

    if (IFF_BROADCAST == (IFF_BROADCAST & ifi->ifi_flags)) {
        syslog(LOG_INFO, "%s:%d:%s: broadcast", __FILE__, __LINE__, __func__);
    }

    if (IFF_DEBUG == (IFF_DEBUG & ifi->ifi_flags)) {
        syslog(LOG_INFO, "%s:%d:%s: debug", __FILE__, __LINE__, __func__);
    }

    if (IFF_LOOPBACK == (IFF_LOOPBACK & ifi->ifi_flags)) {
        syslog(LOG_INFO, "%s:%d:%s: loopback", __FILE__, __LINE__, __func__);
    }

    if (IFF_POINTOPOINT == (IFF_POINTOPOINT & ifi->ifi_flags)) {
        syslog(LOG_INFO, "%s:%d:%s: point-to-point", __FILE__, __LINE__, __func__);
    }

    if (IFF_RUNNING == (IFF_RUNNING & ifi->ifi_flags)) {
        syslog(LOG_INFO, "%s:%d:%s: running", __FILE__, __LINE__, __func__);
    }

    if (IFF_NOARP == (IFF_NOARP & ifi->ifi_flags)) {
        syslog(LOG_INFO, "%s:%d:%s: no arp", __FILE__, __LINE__, __func__);
    }

    if (IFF_PROMISC == (IFF_PROMISC & ifi->ifi_flags)) {
        syslog(LOG_INFO, "%s:%d:%s: no arp", __FILE__, __LINE__, __func__);
    }

    if (IFF_ALLMULTI == (IFF_ALLMULTI & ifi->ifi_flags)) {
        syslog(LOG_INFO, "%s:%d:%s: allmulti", __FILE__, __LINE__, __func__);
    }

    if (IFF_MULTICAST == (IFF_MULTICAST & ifi->ifi_flags)) {
        syslog(LOG_INFO, "%s:%d:%s: multicast", __FILE__, __LINE__, __func__);
    }


    return 0;
}


int example_epoll_event_netlink_dellink (
    struct example_s * example,
    struct epoll_event * event,
    struct example_epollfd_s * example_epollfd,
    struct ifinfomsg * ifi
)
{
    // This is called when link is removed from a device; this probably means
    // the device was removed entirely from the device (e.g. unplugged a USB
    // ethernet device, or called 'ip link del veth').

    int ret = 0;

    syslog(LOG_INFO, "%s:%d:%s: interface %d just got removed, type=%d, flags=%d, family=%d",
            __FILE__, __LINE__, __func__, ifi->ifi_index, ifi->ifi_type, ifi->ifi_flags, ifi->ifi_family);

    if (IFF_UP == (IFF_UP & ifi->ifi_flags)) {
        syslog(LOG_INFO, "%s:%d:%s: its up", __FILE__, __LINE__, __func__);
    }

    if (IFF_BROADCAST == (IFF_BROADCAST & ifi->ifi_flags)) {
        syslog(LOG_INFO, "%s:%d:%s: broadcast", __FILE__, __LINE__, __func__);
    }

    if (IFF_DEBUG == (IFF_DEBUG & ifi->ifi_flags)) {
        syslog(LOG_INFO, "%s:%d:%s: debug", __FILE__, __LINE__, __func__);
    }

    if (IFF_LOOPBACK == (IFF_LOOPBACK & ifi->ifi_flags)) {
        syslog(LOG_INFO, "%s:%d:%s: loopback", __FILE__, __LINE__, __func__);
    }

    if (IFF_POINTOPOINT == (IFF_POINTOPOINT & ifi->ifi_flags)) {
        syslog(LOG_INFO, "%s:%d:%s: point-to-point", __FILE__, __LINE__, __func__);
    }

    if (IFF_RUNNING == (IFF_RUNNING & ifi->ifi_flags)) {
        syslog(LOG_INFO, "%s:%d:%s: running", __FILE__, __LINE__, __func__);
    }

    if (IFF_NOARP == (IFF_NOARP & ifi->ifi_flags)) {
        syslog(LOG_INFO, "%s:%d:%s: no arp", __FILE__, __LINE__, __func__);
    }

    if (IFF_PROMISC == (IFF_PROMISC & ifi->ifi_flags)) {
        syslog(LOG_INFO, "%s:%d:%s: no arp", __FILE__, __LINE__, __func__);
    }

    if (IFF_ALLMULTI == (IFF_ALLMULTI & ifi->ifi_flags)) {
        syslog(LOG_INFO, "%s:%d:%s: allmulti", __FILE__, __LINE__, __func__);
    }

    if (IFF_MULTICAST == (IFF_MULTICAST & ifi->ifi_flags)) {
        syslog(LOG_INFO, "%s:%d:%s: multicast", __FILE__, __LINE__, __func__);
    }

    return 0;
}


int example_epoll_event_netlink_newaddr (
    struct example_s * example,
    struct epoll_event * event,
    struct example_epollfd_s * example_epollfd,
    struct nlmsghdr * nlmsghdr
)
{

    int ret = 0;

    struct ifaddrmsg * ifa = NLMSG_DATA(nlmsghdr);
    int ifa_len = NLMSG_PAYLOAD(nlmsghdr, sizeof(*ifa));

    struct rtattr * rtattr = IFA_RTA(ifa);
    int rtattr_len = IFA_PAYLOAD(nlmsghdr);

    char name[IFNAMSIZ];
    char ipv4[INET_ADDRSTRLEN];
    char ipv6[INET6_ADDRSTRLEN];


    if (!RTA_OK(rtattr, rtattr_len)) {
        syslog(LOG_ERR, "%s:%d:%s: RTA_OK(rtattr, rtattr_len) returned false", __FILE__, __LINE__, __func__);
        return -1;
    }


    int i = 0;
    while (1) {

        if (IFA_LOCAL == rtattr->rta_type) {
            syslog(LOG_INFO, "%s:%d:%s: local", __FILE__, __LINE__, __func__);
        }

        else if (IFA_ADDRESS == rtattr->rta_type && AF_INET6 == ifa->ifa_family) {

            // get the name of the interface
            if (NULL == if_indextoname(ifa->ifa_index, name)) {
                syslog(LOG_ERR, "%s:%d:%s: if_indextoname: %s", __FILE__, __LINE__, __func__, strerror(errno));
                return -1;
            }

            // get ip as a printable string
            if (NULL == inet_ntop(AF_INET6, RTA_DATA(rtattr), ipv6, sizeof(ipv6))) {
                syslog(LOG_ERR, "%s:%d:%s: inet_ntop: %s", __FILE__, __LINE__, __func__, strerror(errno));
                return -1;
            }

            syslog(LOG_INFO, "%s:%d:%s: interface %d (%s) just got addr %s",
                    __FILE__, __LINE__, __func__, ifa->ifa_index, name, ipv6);


            if (IFA_F_TEMPORARY == (IFA_F_TEMPORARY & ifa->ifa_flags)) {
                syslog(LOG_INFO, "%s:%d:%s: temporary", __FILE__, __LINE__, __func__);
            }
            if (IFA_F_PERMANENT == (IFA_F_PERMANENT & ifa->ifa_flags)) {
                syslog(LOG_INFO, "%s:%d:%s: permanent", __FILE__, __LINE__, __func__);
            }
            if (IFA_F_TENTATIVE == (IFA_F_TENTATIVE & ifa->ifa_flags)) {
                syslog(LOG_INFO, "%s:%d:%s: tentative", __FILE__, __LINE__, __func__);
            }


            if (RT_SCOPE_UNIVERSE == (RT_SCOPE_UNIVERSE & ifa->ifa_scope)) {
                syslog(LOG_INFO, "%s:%d:%s: global scope", __FILE__, __LINE__, __func__);
            }
            else if (RT_SCOPE_LINK == (RT_SCOPE_LINK & ifa->ifa_scope)) {
                syslog(LOG_INFO, "%s:%d:%s: link scope", __FILE__, __LINE__, __func__);
            }

        }

        else if (IFA_ADDRESS == rtattr->rta_type && AF_INET == ifa->ifa_family) {

            if (NULL == if_indextoname(ifa->ifa_index, name)) {
                syslog(LOG_ERR, "%s:%d:%s: if_indextoname: %s", __FILE__, __LINE__, __func__, strerror(errno));
                return -1;
            }
            if (NULL == inet_ntop(AF_INET, RTA_DATA(rtattr), ipv4, sizeof(ipv4))) {
                syslog(LOG_ERR, "%s:%d:%s: inet_ntop: %s", __FILE__, __LINE__, __func__, strerror(errno));
                return -1;
            }

            syslog(LOG_INFO, "%s:%d:%s: interface %d (%s) just got addr %s",
                    __FILE__, __LINE__, __func__, ifa->ifa_index, name, ipv4);
        }

        else if (IFA_LABEL == rtattr->rta_type) {
            syslog(LOG_INFO, "%s:%d:%s: label", __FILE__, __LINE__, __func__);
        }

        else if (IFA_BROADCAST == rtattr->rta_type) {
            syslog(LOG_INFO, "%s:%d:%s: broadcast addr", __FILE__, __LINE__, __func__);
        }

        else if (IFA_ANYCAST == rtattr->rta_type) {
            syslog(LOG_INFO, "%s:%d:%s: anycast addr", __FILE__, __LINE__, __func__);
        }


        // ok fetch the next rt attribute
        rtattr = RTA_NEXT(rtattr, rtattr_len);
        if (!RTA_OK(rtattr, rtattr_len)) {
            break;
        }

        if (1024 < ++i) {
            syslog(LOG_ERR, "%s:%d:%s: infinite loop", __FILE__, __LINE__, __func__);
            return -1;
        }
    }

    return 0;
}


int example_epoll_event_netlink_deladdr (
    struct example_s * example,
    struct epoll_event * event,
    struct example_epollfd_s * example_epollfd,
    struct nlmsghdr * nlmsghdr
)
{

    int ret = 0;

    struct ifaddrmsg * ifa = NLMSG_DATA(nlmsghdr);
    int ifa_len = NLMSG_PAYLOAD(nlmsghdr, sizeof(*ifa));

    struct rtattr * rtattr = IFA_RTA(ifa);
    int rtattr_len = IFA_PAYLOAD(nlmsghdr);

    char name[IFNAMSIZ];
    char ipv4[INET_ADDRSTRLEN];
    char ipv6[INET6_ADDRSTRLEN];


    if (!RTA_OK(rtattr, rtattr_len)) {
        syslog(LOG_ERR, "%s:%d:%s: RTA_OK(rtattr, rtattr_len) returned false", __FILE__, __LINE__, __func__);
        return -1;
    }


    int i = 0;
    while (1) {

        if (IFA_LOCAL == rtattr->rta_type) {
            syslog(LOG_INFO, "%s:%d:%s: local", __FILE__, __LINE__, __func__);
        }

        else if (IFA_ADDRESS == rtattr->rta_type && AF_INET6 == ifa->ifa_family) {

            if (NULL == if_indextoname(ifa->ifa_index, name)) {
                syslog(LOG_ERR, "%s:%d:%s: if_indextoname: %s", __FILE__, __LINE__, __func__, strerror(errno));
                return -1;
            }
            if (NULL == inet_ntop(AF_INET6, RTA_DATA(rtattr), ipv6, sizeof(ipv6))) {
                syslog(LOG_ERR, "%s:%d:%s: inet_ntop: %s", __FILE__, __LINE__, __func__, strerror(errno));
                return -1;
            }

            syslog(LOG_INFO, "%s:%d:%s: interface %d (%s) lost addr %s",
                    __FILE__, __LINE__, __func__, ifa->ifa_index, name, ipv6);

        }

        else if (IFA_ADDRESS == rtattr->rta_type && AF_INET == ifa->ifa_family) {

            if (NULL == if_indextoname(ifa->ifa_index, name)) {
                syslog(LOG_ERR, "%s:%d:%s: if_indextoname: %s", __FILE__, __LINE__, __func__, strerror(errno));
                return -1;
            }
            if (NULL == inet_ntop(AF_INET, RTA_DATA(rtattr), ipv4, sizeof(ipv4))) {
                syslog(LOG_ERR, "%s:%d:%s: inet_ntop: %s", __FILE__, __LINE__, __func__, strerror(errno));
                return -1;
            }

            syslog(LOG_INFO, "%s:%d:%s: interface %d (%s) lost addr %s",
                    __FILE__, __LINE__, __func__, ifa->ifa_index, name, ipv4);
        }

        else if (IFA_LABEL == rtattr->rta_type) {
            syslog(LOG_INFO, "%s:%d:%s: label", __FILE__, __LINE__, __func__);
        }

        else if (IFA_BROADCAST == rtattr->rta_type) {
            syslog(LOG_INFO, "%s:%d:%s: broadcast addr", __FILE__, __LINE__, __func__);
        }

        else if (IFA_ANYCAST == rtattr->rta_type) {
            syslog(LOG_INFO, "%s:%d:%s: anycast addr", __FILE__, __LINE__, __func__);
        }


        // ok fetch the next rt attribute
        rtattr = RTA_NEXT(rtattr, rtattr_len);
        if (!RTA_OK(rtattr, rtattr_len)) {
            break;
        }

        if (1024 < ++i) {
            syslog(LOG_ERR, "%s:%d:%s: infinite loop", __FILE__, __LINE__, __func__);
            return -1;
        }
    }

    return 0;
}


int example_epoll_event_netlink (
    struct example_s * example,
    struct epoll_event * event,
    struct example_epollfd_s * example_epollfd
)
{

    int ret = 0;
    uint8_t buf[4096];
    int bytes_read = 0;
    struct nlmsghdr * nlmsghdr;

    bytes_read = read(example_epollfd->fd, buf, sizeof(buf));
    if (-1 == bytes_read) {
        syslog(LOG_ERR, "%s:%d:%s: read: %s", __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }
    if (0 == bytes_read) {
        syslog(LOG_ERR, "%s:%d:%s: read 0 bytes", __FILE__, __LINE__, __func__);
        return -1;
    }


    nlmsghdr = (struct nlmsghdr *)buf;
    if (!NLMSG_OK(nlmsghdr, bytes_read)) {
        syslog(LOG_ERR, "%s:%d:%s: NLMSG_OK(nlmsghdr) returned false", __FILE__, __LINE__, __func__);
        return -1;
    }

    int i = 0;
    while (1) {

        // dispatch on netlink message type
        switch (nlmsghdr->nlmsg_type) {
            case RTM_NEWLINK:
                ret = example_epoll_event_netlink_newlink(example, event, example_epollfd, (struct ifinfomsg*)NLMSG_DATA(nlmsghdr));
                if (-1 == ret) {
                    syslog(LOG_ERR, "%s:%d:%s: example_epoll_event_netlink_newlink returned -1", __FILE__, __LINE__, __func__);
                    return -1;
                }
                break;


            case RTM_DELLINK:
                ret = example_epoll_event_netlink_dellink(example, event, example_epollfd, (struct ifinfomsg*)NLMSG_DATA(nlmsghdr));
                if (-1 == ret) {
                    syslog(LOG_ERR, "%s:%d:%s: example_epoll_event_netlink_dellink returned -1", __FILE__, __LINE__, __func__);
                    return -1;
                }
                break;

            case RTM_NEWADDR:
                ret = example_epoll_event_netlink_newaddr(example, event, example_epollfd, nlmsghdr);
                if (-1 == ret) {
                    syslog(LOG_ERR, "%s:%d:%s: example_epoll_event_netlink_newaddr returned -1", __FILE__, __LINE__, __func__);
                    return -1;
                }
                break;

            case RTM_DELADDR:
                ret = example_epoll_event_netlink_deladdr(example, event, example_epollfd, nlmsghdr);
                if (-1 == ret) {
                    syslog(LOG_ERR, "%s:%d:%s: example_epoll_event_netlink_deladdr returned -1", __FILE__, __LINE__, __func__);
                    return -1;
                }
                break;


            default:
                syslog(LOG_ERR, "%s:%d:%s: no match on netlink message type %d", __FILE__, __LINE__, __func__, nlmsghdr->nlmsg_type);
                return -1;
        }


        // get the next nlmsghdr packet
        nlmsghdr = NLMSG_NEXT(nlmsghdr, bytes_read);
        if (!NLMSG_OK(nlmsghdr, bytes_read)) {
            break;
        }
        if (NLMSG_DONE == nlmsghdr->nlmsg_type) {
            break;
        }
        if (NLMSG_ERROR == nlmsghdr->nlmsg_type) {
            syslog(LOG_ERR, "%s:%d:%s: NLMSG_ERROR == nlmsghdr->nlmsg_type", __FILE__, __LINE__, __func__);
            return -1;
        }

        // loop around, but check for infinite loops.
        if (1024 < ++i) {
            syslog(LOG_ERR, "%s:%d:%s: infinite loop", __FILE__, __LINE__, __func__);
            return -1;
        }
    }


    // Ok, we've read all messages from the netlink now; let's rearm the fd on epoll.
    ret = epoll_ctl(
        example->epollfd,
        EPOLL_CTL_MOD,
        example_epollfd->fd,
        &(struct epoll_event){
            .events = EPOLLIN | EPOLLONESHOT,
            .data = event->data
        }
    );
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d:%s: epoll_ctl: %s", __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }

    return 0;
}


static int example_epoll_event_dispatch (
    struct example_s * example,
    struct epoll_event * event
)
{
    int ret = 0;
    struct example_epollfd_s * example_epollfd = event->data.ptr;

    if (EXAMPLE_EPOLLFD_SENTINEL != example_epollfd->sentinel) {
        syslog(LOG_ERR, "%s:%d:%s: sentinel is wrong", __FILE__, __LINE__, __func__);
        return -1;
    }

    switch (example_epollfd->type) {

        case EXAMPLE_EPOLLFD_TYPE_NETLINK:
            ret = example_epoll_event_netlink(example, event, example_epollfd);
            if (-1 == ret) {
                syslog(LOG_ERR, "%s:%d:%s: example_epoll_event_netlink returned -1", __FILE__, __LINE__, __func__);
                return -1;
            }
            return 0;


        default: 
            syslog(LOG_ERR, "%s:%d:%s: No match on epoll event.", __FILE__, __LINE__, __func__);
            return -1;

    }

    syslog(LOG_ERR, "%s:%d:%s: reached dead code", __FILE__, __LINE__, __func__);
    return -1;
}


static int example_epoll_handle_events (
    struct example_s * example,
    struct epoll_event epoll_events[8],
    int epoll_events_len
)
{
    int ret = 0;
    for (int i = 0; i < epoll_events_len; i++) {
        // (snippet: epdisp)
        ret = example_epoll_event_dispatch(example, &epoll_events[i]);
        if (0 != ret) {
            return ret;
        }
    }
    return 0;
}


int example_loop (
    struct example_s * example
)
{

    int ret = 0;

    int epoll_events_len = 0;
    struct epoll_event epoll_events[8];
    while (1) {
        epoll_events_len = epoll_wait(
                /* epollfd = */ example->epollfd,
                /* &events = */ epoll_events,
                /* events_len = */ 8,
                /* timeout = */ -1
                );

        // got interrupted, just try again.
        if (-1 == epoll_events_len && EINTR == errno) {
            continue;
        }

        if (-1 == epoll_events_len) {
            syslog(LOG_ERR, "%s:%d:%s: epoll_wait: %s", __FILE__, __LINE__, __func__, strerror(errno));
            return -1;
        }

        if (0 == epoll_events_len) {
            syslog(LOG_ERR, "%s:%d:%s: epoll_wait returned 0 events", __FILE__, __LINE__, __func__);
            return -1;
        }

        // dispatch on event
        // (snippet: epev)
        ret = example_epoll_handle_events(example, epoll_events, epoll_events_len);
        if (-1 == ret) {
            syslog(LOG_ERR, "%s:%d:%s: example_epoll_handle_events returned -1", __FILE__, __LINE__, __func__);
            return -1;
        }
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

    ret = example_init(&example);
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d:%s: example_init returned -1", __FILE__, __LINE__, __func__);
        return -1;
    }
    
    ret = example_loop(&example);
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d:%s: example_loop returned -1", __FILE__, __LINE__, __func__);
        return -1;
    }
    
    return 0;
}
