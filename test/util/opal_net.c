/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2014 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/*
 * Unit tests for opal/util/net.h — IPv4/IPv6 address helpers.
 *
 * All inputs are constructed from literal constants so the tests are
 * fully deterministic and independent of runtime DNS or network state.
 *
 * Init order (mandatory):
 *   opal_init_util()  -- registers MCA parameters including
 *                        opal_net_private_ipv4 with its default value
 *                        "10.0.0.0/8;172.16.0.0/12;192.168.0.0/16;169.254.0.0/16"
 *   opal_net_init()   -- parses that string and fills the private_ipv4
 *                        table (without this, addr_isipv4public returns
 *                        true for everything -- see net.c line ~314)
 *
 * NOTE: -DNDEBUG is set; assert() is a no-op.  All verification MUST
 * use test_verify() / test_failure().  Never use assert().
 *
 * NOTE: opal_net_get_hostname() returns a per-thread static buffer
 * that must NOT be freed.
 *
 * NOTE: We skip /0 and /32 in prefix2netmask tests because the
 * underlying computation uses (1u << 32) which is undefined behaviour
 * in C; any value produced is non-deterministic on untested hardware.
 */

#include "opal_config.h"

#include "support.h"

#include "opal/util/net.h"
#include "opal/constants.h"
#include "opal/runtime/opal.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

/* ------------------------------------------------------------------ */
/* Helpers to build sockaddr structs from literal strings              */
/* ------------------------------------------------------------------ */

static struct sockaddr_in
make_sin(const char *dotquad, uint16_t port_host_order)
{
    struct sockaddr_in sin;
    memset(&sin, 0, sizeof(sin));
    sin.sin_family = AF_INET;
    sin.sin_port   = htons(port_host_order);
    inet_pton(AF_INET, dotquad, &sin.sin_addr);
    return sin;
}

#if OPAL_ENABLE_IPV6
static struct sockaddr_in6
make_sin6(const char *addr_str, uint16_t port_host_order)
{
    struct sockaddr_in6 sin6;
    memset(&sin6, 0, sizeof(sin6));
    sin6.sin6_family = AF_INET6;
    sin6.sin6_port   = htons(port_host_order);
    inet_pton(AF_INET6, addr_str, &sin6.sin6_addr);
    return sin6;
}
#endif /* OPAL_ENABLE_IPV6 */

/* ------------------------------------------------------------------ */
/* Test: opal_net_isaddr                                               */
/* ------------------------------------------------------------------ */

static void
test_isaddr(void)
{
    /* Valid IPv4 dotted-quad */
    test_verify("opal_net_isaddr(\"127.0.0.1\") is true",
                opal_net_isaddr("127.0.0.1"));
    test_verify("opal_net_isaddr(\"192.168.1.100\") is true",
                opal_net_isaddr("192.168.1.100"));
    test_verify("opal_net_isaddr(\"8.8.8.8\") is true",
                opal_net_isaddr("8.8.8.8"));

#if OPAL_ENABLE_IPV6
    /* Valid IPv6 literals */
    test_verify("opal_net_isaddr(\"::1\") is true",
                opal_net_isaddr("::1"));
    test_verify("opal_net_isaddr(\"fe80::1\") is true",
                opal_net_isaddr("fe80::1"));
    test_verify("opal_net_isaddr(\"2001:db8::1\") is true",
                opal_net_isaddr("2001:db8::1"));
#endif /* OPAL_ENABLE_IPV6 */

    /* Hostnames -- NOT numeric addresses */
    test_verify("opal_net_isaddr(\"localhost\") is false",
                !opal_net_isaddr("localhost"));
    test_verify("opal_net_isaddr(\"example.com\") is false",
                !opal_net_isaddr("example.com"));
    test_verify("opal_net_isaddr(\"\") is false",
                !opal_net_isaddr(""));
    test_verify("opal_net_isaddr(\"not_an_address\") is false",
                !opal_net_isaddr("not_an_address"));
}

/* ------------------------------------------------------------------ */
/* Test: opal_net_prefix2netmask                                       */
/*                                                                     */
/* The function returns htonl(((1u<<prefixlen)-1u) << (32-prefixlen)) */
/* i.e. the result is in NETWORK byte order.  We compare by applying  */
/* the same htonl() to our host-order expected values.                */
/*                                                                     */
/* We deliberately SKIP /0 and /32: both trigger shift-by-32 UB.      */
/* ------------------------------------------------------------------ */

static void
test_prefix2netmask(void)
{
    /* /8  -> 0xFF000000 host-order */
    test_verify("prefix2netmask(8) == htonl(0xFF000000)",
                htonl(0xFF000000u) == opal_net_prefix2netmask(8));

    /* /16 -> 0xFFFF0000 host-order */
    test_verify("prefix2netmask(16) == htonl(0xFFFF0000)",
                htonl(0xFFFF0000u) == opal_net_prefix2netmask(16));

    /* /24 -> 0xFFFFFF00 host-order */
    test_verify("prefix2netmask(24) == htonl(0xFFFFFF00)",
                htonl(0xFFFFFF00u) == opal_net_prefix2netmask(24));

    /* /1  -> 0x80000000 host-order */
    test_verify("prefix2netmask(1) == htonl(0x80000000)",
                htonl(0x80000000u) == opal_net_prefix2netmask(1));
}

/* ------------------------------------------------------------------ */
/* Test: opal_net_islocalhost                                          */
/* ------------------------------------------------------------------ */

static void
test_islocalhost(void)
{
    /* 127.0.0.1 */
    {
        struct sockaddr_in sin = make_sin("127.0.0.1", 0);
        test_verify("opal_net_islocalhost(127.0.0.1) is true",
                    opal_net_islocalhost((struct sockaddr *) &sin));
    }

    /* 127.0.0.2 -- still in 127.0.0.0/8 */
    {
        struct sockaddr_in sin = make_sin("127.0.0.2", 0);
        test_verify("opal_net_islocalhost(127.0.0.2) is true",
                    opal_net_islocalhost((struct sockaddr *) &sin));
    }

    /* 127.255.255.255 -- still in 127.0.0.0/8 */
    {
        struct sockaddr_in sin = make_sin("127.255.255.255", 0);
        test_verify("opal_net_islocalhost(127.255.255.255) is true",
                    opal_net_islocalhost((struct sockaddr *) &sin));
    }

    /* 128.0.0.1 -- just outside 127.0.0.0/8 */
    {
        struct sockaddr_in sin = make_sin("128.0.0.1", 0);
        test_verify("opal_net_islocalhost(128.0.0.1) is false",
                    !opal_net_islocalhost((struct sockaddr *) &sin));
    }

    /* 192.168.1.1 -- private but not loopback */
    {
        struct sockaddr_in sin = make_sin("192.168.1.1", 0);
        test_verify("opal_net_islocalhost(192.168.1.1) is false",
                    !opal_net_islocalhost((struct sockaddr *) &sin));
    }

    /* 8.8.8.8 -- public, definitely not localhost */
    {
        struct sockaddr_in sin = make_sin("8.8.8.8", 0);
        test_verify("opal_net_islocalhost(8.8.8.8) is false",
                    !opal_net_islocalhost((struct sockaddr *) &sin));
    }

    /*
     * Regression guard for the 255.0.0.0 bug (now fixed in opal/util/net.c):
     * the check previously masked only bit 7 of the top byte
     * (0x7F000000 == (0x7F000000 & ntohl(s_addr))), so 255.x.x.x
     * (0xFF000000) also matched as localhost.  It now masks the full top
     * byte (0xFF000000 == (0xFF000000 & ntohl(s_addr))), so 255.0.0.0 is
     * correctly not localhost, per the "127.0.0.0/8" contract.
     */
    {
        struct sockaddr_in sin = make_sin("255.0.0.0", 0);
        test_verify("opal_net_islocalhost(255.0.0.0) is false",
                    !opal_net_islocalhost((struct sockaddr *) &sin));
    }

#if OPAL_ENABLE_IPV6
    /* ::1 is the IPv6 loopback */
    {
        struct sockaddr_in6 sin6 = make_sin6("::1", 0);
        test_verify("opal_net_islocalhost(::1) is true",
                    opal_net_islocalhost((struct sockaddr *) &sin6));
    }

    /* fe80::1 is link-local, not loopback */
    {
        struct sockaddr_in6 sin6 = make_sin6("fe80::1", 0);
        test_verify("opal_net_islocalhost(fe80::1) is false",
                    !opal_net_islocalhost((struct sockaddr *) &sin6));
    }

    /* 2001:db8::1 is a documentation prefix, not loopback */
    {
        struct sockaddr_in6 sin6 = make_sin6("2001:db8::1", 0);
        test_verify("opal_net_islocalhost(2001:db8::1) is false",
                    !opal_net_islocalhost((struct sockaddr *) &sin6));
    }
#endif /* OPAL_ENABLE_IPV6 */
}

/* ------------------------------------------------------------------ */
/* Test: opal_net_samenetwork                                          */
/* ------------------------------------------------------------------ */

static void
test_samenetwork(void)
{
    /* Two addresses on the same /24 */
    {
        struct sockaddr_in a = make_sin("192.168.1.10", 0);
        struct sockaddr_in b = make_sin("192.168.1.20", 0);
        test_verify("samenetwork: 192.168.1.x/24 match",
                    opal_net_samenetwork((struct sockaddr *) &a,
                                        (struct sockaddr *) &b, 24));
    }

    /* Two addresses on different /24 subnets */
    {
        struct sockaddr_in a = make_sin("192.168.1.10", 0);
        struct sockaddr_in b = make_sin("192.168.2.10", 0);
        test_verify("samenetwork: 192.168.1.x vs 192.168.2.x /24 differ",
                    !opal_net_samenetwork((struct sockaddr *) &a,
                                         (struct sockaddr *) &b, 24));
    }

    /* Same /16, different /24 -> still same network at /16 */
    {
        struct sockaddr_in a = make_sin("10.1.1.5", 0);
        struct sockaddr_in b = make_sin("10.1.200.5", 0);
        test_verify("samenetwork: 10.1.x.x/16 match",
                    opal_net_samenetwork((struct sockaddr *) &a,
                                        (struct sockaddr *) &b, 16));
    }

    /* Same /16, different /24 -> different at /24 */
    {
        struct sockaddr_in a = make_sin("10.1.1.5", 0);
        struct sockaddr_in b = make_sin("10.1.200.5", 0);
        test_verify("samenetwork: 10.1.1.x vs 10.1.200.x /24 differ",
                    !opal_net_samenetwork((struct sockaddr *) &a,
                                         (struct sockaddr *) &b, 24));
    }

    /* Different families must not be the same network */
#if OPAL_ENABLE_IPV6
    {
        struct sockaddr_in  a4  = make_sin("127.0.0.1", 0);
        struct sockaddr_in6 a6  = make_sin6("::1", 0);
        test_verify("samenetwork: AF_INET vs AF_INET6 is always false",
                    !opal_net_samenetwork((struct sockaddr *) &a4,
                                         (struct sockaddr *) &a6, 8));
    }

    /* Two IPv6 addresses in the same /64 prefix */
    {
        struct sockaddr_in6 a = make_sin6("2001:db8::1", 0);
        struct sockaddr_in6 b = make_sin6("2001:db8::2", 0);
        test_verify("samenetwork: 2001:db8::/64 match",
                    opal_net_samenetwork((struct sockaddr *) &a,
                                        (struct sockaddr *) &b, 64));
    }

    /* Two IPv6 addresses in different /64 prefixes */
    {
        struct sockaddr_in6 a = make_sin6("2001:db8:1::1", 0);
        struct sockaddr_in6 b = make_sin6("2001:db8:2::1", 0);
        test_verify("samenetwork: 2001:db8:1:: vs 2001:db8:2:: /64 differ",
                    !opal_net_samenetwork((struct sockaddr *) &a,
                                         (struct sockaddr *) &b, 64));
    }
#endif /* OPAL_ENABLE_IPV6 */
}

/* ------------------------------------------------------------------ */
/* Test: opal_net_addr_isipv4public                                    */
/*                                                                     */
/* The default opal_net_private_ipv4 (set in opal_params_core.c) is:  */
/*   "10.0.0.0/8;172.16.0.0/12;192.168.0.0/16;169.254.0.0/16"        */
/* After opal_net_init() these are loaded, so RFC1918 addrs return    */
/* false and public addresses return true.                             */
/* ------------------------------------------------------------------ */

static void
test_addr_isipv4public(void)
{
    /* Google DNS -- unambiguously public */
    {
        struct sockaddr_in sin = make_sin("8.8.8.8", 0);
        test_verify("opal_net_addr_isipv4public(8.8.8.8) is true",
                    opal_net_addr_isipv4public((struct sockaddr *) &sin));
    }

    /* 1.1.1.1 -- Cloudflare DNS, public */
    {
        struct sockaddr_in sin = make_sin("1.1.1.1", 0);
        test_verify("opal_net_addr_isipv4public(1.1.1.1) is true",
                    opal_net_addr_isipv4public((struct sockaddr *) &sin));
    }

    /* 10.0.0.1 -- RFC1918 private (10.0.0.0/8) */
    {
        struct sockaddr_in sin = make_sin("10.0.0.1", 0);
        test_verify("opal_net_addr_isipv4public(10.0.0.1) is false (RFC1918)",
                    !opal_net_addr_isipv4public((struct sockaddr *) &sin));
    }

    /* 10.255.255.255 -- still in 10.0.0.0/8 */
    {
        struct sockaddr_in sin = make_sin("10.255.255.255", 0);
        test_verify("opal_net_addr_isipv4public(10.255.255.255) is false (RFC1918)",
                    !opal_net_addr_isipv4public((struct sockaddr *) &sin));
    }

    /* 172.16.0.1 -- RFC1918 private (172.16.0.0/12) */
    {
        struct sockaddr_in sin = make_sin("172.16.0.1", 0);
        test_verify("opal_net_addr_isipv4public(172.16.0.1) is false (RFC1918)",
                    !opal_net_addr_isipv4public((struct sockaddr *) &sin));
    }

    /* 172.31.255.255 -- still in 172.16.0.0/12 */
    {
        struct sockaddr_in sin = make_sin("172.31.255.255", 0);
        test_verify("opal_net_addr_isipv4public(172.31.255.255) is false (RFC1918)",
                    !opal_net_addr_isipv4public((struct sockaddr *) &sin));
    }

    /* 172.32.0.1 -- just outside 172.16.0.0/12 (172.16-31.x.x only) */
    {
        struct sockaddr_in sin = make_sin("172.32.0.1", 0);
        test_verify("opal_net_addr_isipv4public(172.32.0.1) is true (outside RFC1918)",
                    opal_net_addr_isipv4public((struct sockaddr *) &sin));
    }

    /* 192.168.0.1 -- RFC1918 private (192.168.0.0/16) */
    {
        struct sockaddr_in sin = make_sin("192.168.0.1", 0);
        test_verify("opal_net_addr_isipv4public(192.168.0.1) is false (RFC1918)",
                    !opal_net_addr_isipv4public((struct sockaddr *) &sin));
    }

    /* 169.254.1.1 -- link-local (169.254.0.0/16) */
    {
        struct sockaddr_in sin = make_sin("169.254.1.1", 0);
        test_verify("opal_net_addr_isipv4public(169.254.1.1) is false (link-local)",
                    !opal_net_addr_isipv4public((struct sockaddr *) &sin));
    }

#if OPAL_ENABLE_IPV6
    /* IPv6 addresses always return false from this function */
    {
        struct sockaddr_in6 sin6 = make_sin6("2001:db8::1", 0);
        test_verify("opal_net_addr_isipv4public(IPv6 addr) is false",
                    !opal_net_addr_isipv4public((struct sockaddr *) &sin6));
    }
#endif /* OPAL_ENABLE_IPV6 */
}

/* ------------------------------------------------------------------ */
/* Test: opal_net_addr_isipv6linklocal                                 */
/* ------------------------------------------------------------------ */

#if OPAL_ENABLE_IPV6
static void
test_addr_isipv6linklocal(void)
{
    /* fe80::1 is a link-local address */
    {
        struct sockaddr_in6 sin6 = make_sin6("fe80::1", 0);
        test_verify("opal_net_addr_isipv6linklocal(fe80::1) is true",
                    opal_net_addr_isipv6linklocal((struct sockaddr *) &sin6));
    }

    /* fe80::ffff is still link-local */
    {
        struct sockaddr_in6 sin6 = make_sin6("fe80::ffff", 0);
        test_verify("opal_net_addr_isipv6linklocal(fe80::ffff) is true",
                    opal_net_addr_isipv6linklocal((struct sockaddr *) &sin6));
    }

    /* ::1 is loopback, not link-local */
    {
        struct sockaddr_in6 sin6 = make_sin6("::1", 0);
        test_verify("opal_net_addr_isipv6linklocal(::1) is false",
                    !opal_net_addr_isipv6linklocal((struct sockaddr *) &sin6));
    }

    /* 2001:db8::1 is a global unicast address, not link-local */
    {
        struct sockaddr_in6 sin6 = make_sin6("2001:db8::1", 0);
        test_verify("opal_net_addr_isipv6linklocal(2001:db8::1) is false",
                    !opal_net_addr_isipv6linklocal((struct sockaddr *) &sin6));
    }

    /* IPv4 address -> always false */
    {
        struct sockaddr_in sin = make_sin("127.0.0.1", 0);
        test_verify("opal_net_addr_isipv6linklocal(IPv4 addr) is false",
                    !opal_net_addr_isipv6linklocal((struct sockaddr *) &sin));
    }
}
#endif /* OPAL_ENABLE_IPV6 */

/* ------------------------------------------------------------------ */
/* Test: opal_net_get_hostname                                         */
/*                                                                     */
/* Returns a per-thread static buffer -- do NOT free the result.      */
/* ------------------------------------------------------------------ */

static void
test_get_hostname(void)
{
    /* 127.0.0.1 -> should produce the string "127.0.0.1" */
    {
        struct sockaddr_in sin = make_sin("127.0.0.1", 0);
        const char *h = opal_net_get_hostname((struct sockaddr *) &sin);
        test_verify("opal_net_get_hostname(127.0.0.1) is non-NULL",
                    NULL != h);
        if (NULL != h) {
            test_verify("opal_net_get_hostname(127.0.0.1) == \"127.0.0.1\"",
                        0 == strcmp("127.0.0.1", h));
        }
    }

    /* 8.8.8.8 -> should produce the string "8.8.8.8" */
    {
        struct sockaddr_in sin = make_sin("8.8.8.8", 0);
        const char *h = opal_net_get_hostname((struct sockaddr *) &sin);
        test_verify("opal_net_get_hostname(8.8.8.8) is non-NULL",
                    NULL != h);
        if (NULL != h) {
            test_verify("opal_net_get_hostname(8.8.8.8) == \"8.8.8.8\"",
                        0 == strcmp("8.8.8.8", h));
        }
    }

#if OPAL_ENABLE_IPV6
    /* ::1 should produce "::1" (or equivalent minimal form) */
    {
        struct sockaddr_in6 sin6 = make_sin6("::1", 0);
        const char *h = opal_net_get_hostname((struct sockaddr *) &sin6);
        test_verify("opal_net_get_hostname(::1) is non-NULL",
                    NULL != h);
        if (NULL != h) {
            test_verify("opal_net_get_hostname(::1) is non-empty",
                        '\0' != h[0]);
        }
    }

    /* fe80::1 */
    {
        struct sockaddr_in6 sin6 = make_sin6("fe80::1", 0);
        const char *h = opal_net_get_hostname((struct sockaddr *) &sin6);
        test_verify("opal_net_get_hostname(fe80::1) is non-NULL",
                    NULL != h);
    }
#endif /* OPAL_ENABLE_IPV6 */
}

/* ------------------------------------------------------------------ */
/* Test: opal_net_get_port                                             */
/* ------------------------------------------------------------------ */

static void
test_get_port(void)
{
    /* IPv4: port 80 */
    {
        struct sockaddr_in sin = make_sin("192.168.1.1", 80);
        test_verify("opal_net_get_port(sin, port=80) == 80",
                    80 == opal_net_get_port((struct sockaddr *) &sin));
    }

    /* IPv4: port 0 */
    {
        struct sockaddr_in sin = make_sin("0.0.0.0", 0);
        test_verify("opal_net_get_port(sin, port=0) == 0",
                    0 == opal_net_get_port((struct sockaddr *) &sin));
    }

    /* IPv4: port 65535 */
    {
        struct sockaddr_in sin = make_sin("127.0.0.1", 65535);
        test_verify("opal_net_get_port(sin, port=65535) == 65535",
                    65535 == opal_net_get_port((struct sockaddr *) &sin));
    }

#if OPAL_ENABLE_IPV6
    /* IPv6: port 443 */
    {
        struct sockaddr_in6 sin6 = make_sin6("::1", 443);
        test_verify("opal_net_get_port(sin6, port=443) == 443",
                    443 == opal_net_get_port((struct sockaddr *) &sin6));
    }

    /* IPv6: port 0 */
    {
        struct sockaddr_in6 sin6 = make_sin6("2001:db8::1", 0);
        test_verify("opal_net_get_port(sin6, port=0) == 0",
                    0 == opal_net_get_port((struct sockaddr *) &sin6));
    }
#endif /* OPAL_ENABLE_IPV6 */

    /* Unknown family -> returns -1 */
    {
        struct sockaddr sa;
        memset(&sa, 0, sizeof(sa));
        sa.sa_family = AF_UNSPEC;
        test_verify("opal_net_get_port(AF_UNSPEC) == -1",
                    -1 == opal_net_get_port(&sa));
    }
}

/* ------------------------------------------------------------------ */
/* main                                                                */
/* ------------------------------------------------------------------ */

int main(int argc, char *argv[])
{
    int rc;

    /* test_addr_isipv4public()'s RFC1918 assertions assume the built-in
       default private-address table.  Clear any inherited
       OMPI_MCA_opal_net_private_ipv4 so the test exercises those defaults
       rather than a table injected via the environment. */
    unsetenv("OMPI_MCA_opal_net_private_ipv4");

    rc = opal_init_util(&argc, &argv);
    test_init("opal_net");
    test_verify("opal_init_util() succeeds", OPAL_SUCCESS == rc);

    rc = opal_net_init();
    test_verify("opal_net_init() succeeds", OPAL_SUCCESS == rc);

    test_isaddr();
    test_prefix2netmask();
    test_islocalhost();
    test_samenetwork();
    test_addr_isipv4public();
#if OPAL_ENABLE_IPV6
    test_addr_isipv6linklocal();
#endif /* OPAL_ENABLE_IPV6 */
    test_get_hostname();
    test_get_port();

    {
        int r = test_finalize();
        opal_finalize_util();
        return r;
    }
}
