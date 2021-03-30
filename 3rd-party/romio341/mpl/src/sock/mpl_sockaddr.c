/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

/** Rationale:
 *    MPL wrap for handling IPv4 and IPv6.
 *
 *    Applications: pm, pmi, ch3.
 *    ch4 supports tcp sockets indirectly through ucx and ofi.
 */

/** Design considerations:
 *    Either IPv4 or IPv6, globally set as defalt or with command line option, to
 *    simplify logic.
 *    TCP only, no UDP or unix domain sockets.
 *
 *    Application use struct sockaddr_storage (typedefed to MPL_sockaddr_t) exclusively.
 *            MPL_get_sockaddr for hostname
 *            MPL_get_sockaddr_iface for network interface
 *            MPL_get_sockaddr_direct for listening socket on ANY or LOOPBACK
 *
 *    Simplified MPL_connect and MPL_listen interface.
 *            Both have a port parameter.
 *            MPL_listen combines bind with listen.
 */

/** Portability:
 *    struct sockaddr_storage:
 *            In case this struct is not available (in sys/socket.h), it can be
 *        circumvented by declare following (in mpl_sockaddr.h):
 *                    struct sockaddr_storage {
 *                            unsigend short ss_family;
 *                            char padding[126];
 *                    };
 *            Since we use typedef MPL_sockaddr_t, there is no need for code change.
 *            Only the ss_family field is directly accessed. All the other fields are
 *        always accessed by casting to either struct sockaddr_in or struct
 *        sockaddr_in6.
 *
 *    The implementation uses getaddrinfo and getifaddrs. The former, as with
 *    sockaddr_storage and sockaddr_in6, are documented in RFC 2553, 1999, and are
 *    expected to be supported on most supported platforms. getifaddrs is not in
 *    POSIX.1, but it is present on Linux since glibc 2.3.3, and available on BSD
 *    systems even earlier.
 */

#include "mplconfig.h"
#include <assert.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <netinet/in.h>
#include <string.h>
#include <ifaddrs.h>
#include <errno.h>
#include <stdio.h>

#include "mpl_sockaddr.h"

static int is_localhost(struct sockaddr *p_addr);

static int af_type = AF_INET;
static int _use_loopback = 0;
static int _max_conn = SOMAXCONN;

void MPL_sockaddr_set_aftype(int type)
{
    af_type = type;
}

int MPL_get_sockaddr(const char *s_hostname, MPL_sockaddr_t * p_addr)
{
    struct addrinfo ai_hint;
    struct addrinfo *ai_list;
    int ret;

#ifdef __APPLE__
    /* Macos adds .local to hostname when network is unavailable or limited.
     * This will result in long timeout in getaddrinfo below.
     * Bypass it by resetting the hostname to "localhost"
     */
    int n = strlen(s_hostname);
    if (n > 6 && strcmp(s_hostname + n - 6, ".local") == 0) {
        s_hostname = "localhost";
    }
#endif

    /* NOTE: there is report that getaddrinfo implementations will call kernel
     * even when s_hostname is entirely numerical string and it may cause
     * problems when host is configured with thousands of ip addresses.
     */
    /* TODO: detect the cases when s_hostname is entirely numerical string and
     * call inet_pton directly (-- do this on first bug report).
     */
    memset(p_addr, 0, sizeof(*p_addr));
    memset(&ai_hint, 0, sizeof(ai_hint));
    ai_hint.ai_family = af_type;
    ai_hint.ai_socktype = SOCK_STREAM;
    ai_hint.ai_protocol = IPPROTO_TCP;
    ai_hint.ai_flags = AI_V4MAPPED;
    ret = getaddrinfo(s_hostname, NULL, &ai_hint, &ai_list);
    if (ret) {
        return ret;
    }
    if (af_type == AF_INET) {
        memcpy(p_addr, ai_list->ai_addr, sizeof(struct sockaddr_in));
    } else if (af_type == AF_INET6) {
        memcpy(p_addr, ai_list->ai_addr, sizeof(struct sockaddr_in6));
    } else {
        assert(0);
    }
    freeaddrinfo(ai_list);
    return 0;
}

int MPL_get_sockaddr_direct(int type, MPL_sockaddr_t * p_addr)
{
    memset(p_addr, 0, sizeof(*p_addr));
    assert(type == MPL_SOCKADDR_ANY || type == MPL_SOCKADDR_LOOPBACK);
    if (af_type == AF_INET) {
        struct sockaddr_in *p_addr4 = (struct sockaddr_in *) p_addr;

        p_addr4->sin_family = AF_INET;
        if (type == MPL_SOCKADDR_LOOPBACK) {
            p_addr4->sin_addr.s_addr = htonl(0x7f000001);
        } else {
            p_addr4->sin_addr.s_addr = htonl(INADDR_ANY);
        }
        return 0;
    } else if (af_type == AF_INET6) {
        struct sockaddr_in6 *p_addr6 = (struct sockaddr_in6 *) p_addr;

        p_addr6->sin6_family = AF_INET6;
        if (type == MPL_SOCKADDR_LOOPBACK) {
            p_addr6->sin6_addr = in6addr_loopback;
        } else {
            p_addr6->sin6_addr = in6addr_any;
        }
        return 0;
    } else {
        assert(0);
    }
}

int MPL_get_sockaddr_iface(const char *s_iface, MPL_sockaddr_t * p_addr)
{
    struct ifaddrs *ifaddr;
    int ret;
    struct ifaddrs *ifa;
    int found = 0;

    memset(p_addr, 0, sizeof(*p_addr));
    ret = getifaddrs(&ifaddr);
    if (ret) {
        return ret;
    }
    ifa = ifaddr;
    while (ifa) {
        if (s_iface && ifa->ifa_name && strcmp(s_iface, ifa->ifa_name) != 0) {
            ifa = ifa->ifa_next;
            continue;
        }
        if (ifa->ifa_addr && ifa->ifa_addr->sa_family == af_type) {
            found++;
            if (af_type == AF_INET) {
                memcpy(p_addr, ifa->ifa_addr, sizeof(struct sockaddr_in));
            } else if (af_type == AF_INET6) {
                memcpy(p_addr, ifa->ifa_addr, sizeof(struct sockaddr_in6));
            }
            if (!is_localhost((struct sockaddr *) ifa->ifa_addr)) {
                break;
            }
        }
        ifa = ifa->ifa_next;
    }
    freeifaddrs(ifaddr);
    if (!found) {
        return -1;
    } else {
        return 0;
    }
}

int MPL_socket()
{
    return socket(af_type, SOCK_STREAM, IPPROTO_TCP);
}

int MPL_connect(int sock_fd, MPL_sockaddr_t * p_addr, unsigned short port)
{
    if (af_type == AF_INET) {
        ((struct sockaddr_in *) p_addr)->sin_port = htons(port);
        return connect(sock_fd, (const struct sockaddr *) p_addr, sizeof(struct sockaddr_in));
    } else if (af_type == AF_INET6) {
        ((struct sockaddr_in6 *) p_addr)->sin6_port = htons(port);
        return connect(sock_fd, (const struct sockaddr *) p_addr, sizeof(struct sockaddr_in6));
    } else {
        return -1;
    }
}

void MPL_set_listen_attr(int use_loopback, int max_conn)
{
    _use_loopback = use_loopback;
    _max_conn = max_conn;
}

int MPL_listen(int sock_fd, unsigned short port)
{
    MPL_sockaddr_t addr;
    int ret;

    if (_use_loopback) {
        MPL_get_sockaddr_direct(MPL_SOCKADDR_LOOPBACK, &addr);
    } else {
        MPL_get_sockaddr_direct(MPL_SOCKADDR_ANY, &addr);
    }
    if (af_type == AF_INET) {
        ((struct sockaddr_in *) &addr)->sin_port = htons(port);
        ret = bind(sock_fd, (const struct sockaddr *) &addr, sizeof(struct sockaddr_in));
    } else if (af_type == AF_INET6) {
        ((struct sockaddr_in6 *) &addr)->sin6_port = htons(port);
        ret = bind(sock_fd, (const struct sockaddr *) &addr, sizeof(struct sockaddr_in6));
    } else {
        assert(0);
    }
    if (ret) {
        return ret;
    }
    return listen(sock_fd, _max_conn);
}

int MPL_listen_anyport(int sock_fd, unsigned short *p_port)
{
    MPL_sockaddr_t addr;
    int ret;

    if (_use_loopback) {
        MPL_get_sockaddr_direct(MPL_SOCKADDR_LOOPBACK, &addr);
    } else {
        MPL_get_sockaddr_direct(MPL_SOCKADDR_ANY, &addr);
    }
    if (af_type == AF_INET) {
        ((struct sockaddr_in *) &addr)->sin_port = 0;
        ret = bind(sock_fd, (const struct sockaddr *) &addr, sizeof(struct sockaddr_in));
    } else if (af_type == AF_INET6) {
        ((struct sockaddr_in6 *) &addr)->sin6_port = 0;
        ret = bind(sock_fd, (const struct sockaddr *) &addr, sizeof(struct sockaddr_in6));
    } else {
        assert(0);
    }
    if (ret) {
        return ret;
    }
    unsigned int n = sizeof(addr);
    ret = getsockname(sock_fd, (struct sockaddr *) &addr, &n);
    if (ret) {
        return ret;
    }
    if (af_type == AF_INET) {
        *p_port = ntohs(((struct sockaddr_in *) &addr)->sin_port);
    } else if (af_type == AF_INET6) {
        *p_port = ntohs(((struct sockaddr_in6 *) &addr)->sin6_port);
    }
    return listen(sock_fd, _max_conn);
}

int MPL_listen_portrange(int sock_fd, unsigned short *p_port, int low_port, int high_port)
{
    MPL_sockaddr_t addr;
    int i;
    int ret;

    if (_use_loopback) {
        MPL_get_sockaddr_direct(MPL_SOCKADDR_LOOPBACK, &addr);
    } else {
        MPL_get_sockaddr_direct(MPL_SOCKADDR_ANY, &addr);
    }
    for (i = low_port; i <= high_port; i++) {
        ret = MPL_listen(sock_fd, i);
        if (ret == 0) {
            *p_port = i;
            break;
        } else if (errno == EADDRINUSE) {
            continue;
        } else {
            return -1;
        }
    }
    if (i > high_port) {
        return -2;
    }
    return listen(sock_fd, _max_conn);
}

int MPL_sockaddr_to_str(MPL_sockaddr_t * p_addr, char *str, int maxlen)
{
    unsigned char *p;

    /* TODO: consider inet_ntop */
    if (p_addr->ss_family == AF_INET) {
        p = (void *) &((struct sockaddr_in *) p_addr)->sin_addr;
        snprintf(str, maxlen, "%d.%d.%d.%d", p[0], p[1], p[2], p[3]);
    } else if (p_addr->ss_family == AF_INET6) {
        p = (void *) &((struct sockaddr_in6 *) p_addr)->sin6_addr;
        snprintf(str, maxlen,
                 "%02x%02x:%02x%02x:%02x%02x:%02x%02x:%02x%02x:%02x%02x:%02x%02x:%02x%02x",
                 p[0], p[1], p[2], p[3], p[4], p[5], p[6], p[7],
                 p[8], p[9], p[10], p[11], p[12], p[13], p[14], p[15]);
    }
    return 0;
}

int MPL_sockaddr_port(MPL_sockaddr_t * p_addr)
{
    if (p_addr->ss_family == AF_INET) {
        return ntohs(((struct sockaddr_in *) p_addr)->sin_port);
    } else if (p_addr->ss_family == AF_INET6) {
        return ntohs(((struct sockaddr_in6 *) p_addr)->sin6_port);
    }
    return 0;
}

int is_localhost(struct sockaddr *p_addr)
{
    char *p;

    if (p_addr->sa_family == AF_INET) {
        p = (void *) &((struct sockaddr_in *) p_addr)->sin_addr;
        return strncmp(p, "\x7f\x00\x00\x01", 4) == 0;
    } else if (p_addr->sa_family == AF_INET6) {
        p = (void *) &((struct sockaddr_in6 *) p_addr)->sin6_addr;
        return strncmp(p, "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x01", 16) == 0 ||
            strncmp(p, "\xfe\x80\0\0\0\0\0\0\0\0\0\0\0\0\0\x01", 16) == 0;
    } else {
        return 0;
    }
}
