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
 * Unit tests for the OPAL network-interface enumeration API (opal/util/if.h
 * and the opal/mca/if framework).
 *
 * Strategy:
 *   1. opal_init() brings up the MCA if framework (opal_init_util() alone
 *      does not open it); only then is the interface list populated.
 *   2. We assert opal_ifcount() >= 1 (at minimum a loopback exists).
 *   3. We iterate all interfaces with opal_ifbegin()/opal_ifnext() and
 *      cross-check every round-trip:
 *        index -> name -> index
 *        index -> kindex -> kindex-name  (must equal the index-name)
 *        name  -> kindex
 *   4. We locate the loopback by address (127.x.x.x) rather than by
 *      name ("lo" vs "lo0"), then assert opal_ifisloopback() is true.
 *   5. opal_ifislocal() is exercised with known-local / known-remote strings.
 *
 * The loopback interface is enumerated only when the MCA parameter
 * if_base_retain_loopback is set: the BSD enumerator (opal/mca/if/bsdx_ipv4,
 * used on the *BSDs including FreeBSD) honors it and drops loopback by
 * default, while the posix_ipv4 component (used on Linux and macOS) has had
 * its loopback filter #if 0'd out since 2010 and so always keeps loopback,
 * regardless of the parameter.  To exercise the loopback-related API
 * uniformly across platforms, main() sets that parameter before opal_init()
 * (see below).  This long-standing cross-platform divergence (and the
 * options for resolving it in OPAL) is tracked in
 * https://github.com/open-mpi/ompi/issues/14029.
 *
 * NOTE: -DNDEBUG is set; assert() is a no-op.  All verification MUST
 * use test_verify() / test_failure().  Never use assert().
 */

#include "opal_config.h"

#include "support.h"

#include "opal/util/if.h"
#include "opal/constants.h"
#include "opal/runtime/opal.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <net/if.h>

/* ------------------------------------------------------------------ */
/* Helpers                                                             */
/* ------------------------------------------------------------------ */

/*
 * Return true if the given sockaddr contains an IPv4 address in the
 * 127.0.0.0/8 loopback range.
 */
static int
addr_is_ipv4_loopback(const struct sockaddr *sa)
{
    if (NULL == sa || AF_INET != sa->sa_family) {
        return 0;
    }
    {
        const struct sockaddr_in *sin = (const struct sockaddr_in *) sa;
        uint32_t h = ntohl(sin->sin_addr.s_addr);
        return ((h & 0xFF000000u) == 0x7F000000u);
    }
}

/* ------------------------------------------------------------------ */
/* Test: opal_ifcount                                                  */
/* ------------------------------------------------------------------ */

static void
test_ifcount(void)
{
    int count = opal_ifcount();
    test_verify("opal_ifcount() >= 1 (loopback always present)", count >= 1);
}

/* ------------------------------------------------------------------ */
/* Test: iterate with opal_ifbegin / opal_ifnext                       */
/* ------------------------------------------------------------------ */

static void
test_iteration(void)
{
    int idx;
    int count_iter = 0;
    int expected_count = opal_ifcount();

    idx = opal_ifbegin();
    test_verify("opal_ifbegin() > 0", idx > 0);

    while (idx > 0) {
        count_iter++;
        idx = opal_ifnext(idx);
    }

    test_verify("iteration count equals opal_ifcount()", count_iter == expected_count);
}

/* ------------------------------------------------------------------ */
/* Test: index <-> name round-trips and name -> kindex                 */
/* ------------------------------------------------------------------ */

static void
test_index_name_roundtrips(void)
{
    int idx;

    for (idx = opal_ifbegin(); idx > 0; idx = opal_ifnext(idx)) {
        char name1[OPAL_IF_NAMESIZE];
        char name2[OPAL_IF_NAMESIZE];
        char kname[OPAL_IF_NAMESIZE];
        int  back_idx;
        int  kindex;
        int  rc;

        /* index -> name */
        rc = opal_ifindextoname(idx, name1, (int) sizeof(name1));
        test_verify("opal_ifindextoname() returns OPAL_SUCCESS",
                    OPAL_SUCCESS == rc);
        test_verify("opal_ifindextoname() produces non-empty name",
                    '\0' != name1[0]);

        /* name -> index (round-trip) */
        back_idx = opal_ifnametoindex(name1);
        test_verify("opal_ifnametoindex(opal_ifindextoname()) round-trips back",
                    back_idx == idx);

        /* index -> kindex */
        kindex = opal_ifindextokindex(idx);
        test_verify("opal_ifindextokindex() returns positive kindex",
                    kindex > 0);

        /* kindex -> name */
        rc = opal_ifkindextoname(kindex, kname, (int) sizeof(kname));
        test_verify("opal_ifkindextoname() returns OPAL_SUCCESS",
                    OPAL_SUCCESS == rc);

        /* kindex-name must match index-name */
        test_verify("opal_ifkindextoname() matches opal_ifindextoname()",
                    0 == strcmp(name1, kname));

        /* name -> kindex (must match) */
        {
            int k2 = opal_ifnametokindex(name1);
            test_verify("opal_ifnametokindex() matches opal_ifindextokindex()",
                        k2 == kindex);
        }

        /* index -> name again via second call -- must be stable */
        rc = opal_ifindextoname(idx, name2, (int) sizeof(name2));
        test_verify("opal_ifindextoname() is stable across calls",
                    OPAL_SUCCESS == rc && 0 == strcmp(name1, name2));
    }
}

/* ------------------------------------------------------------------ */
/* Test: opal_ifnametoaddr / opal_ifaddrtoname round-trips             */
/* ------------------------------------------------------------------ */

static void
test_name_addr_roundtrips(void)
{
    int idx;

    /*
     * Deterministic anchor: with if_base_retain_loopback set (see main()),
     * 127.0.0.1 is in the interface list on any conforming host.
     * opal_ifaddrtoname() must return OPAL_SUCCESS for it and fill in a
     * non-empty interface name.
     */
    {
        char loopback_ifname[OPAL_IF_NAMESIZE];
        int rc = opal_ifaddrtoname("127.0.0.1",
                                   loopback_ifname,
                                   (int) sizeof(loopback_ifname));
        test_verify("opal_ifaddrtoname(\"127.0.0.1\") returns OPAL_SUCCESS",
                    OPAL_SUCCESS == rc);
        test_verify("opal_ifaddrtoname(\"127.0.0.1\") produces non-empty name",
                    '\0' != loopback_ifname[0]);
    }

    /*
     * Per-interface: for each interface with an IPv4 address, convert
     * the address to a dotted-quad string and call opal_ifaddrtoname().
     * We also verify opal_ifnametoaddr() returns a non-zero address family.
     */
    for (idx = opal_ifbegin(); idx > 0; idx = opal_ifnext(idx)) {
        char name[OPAL_IF_NAMESIZE];
        struct sockaddr_storage ss;
        int rc;

        rc = opal_ifindextoname(idx, name, (int) sizeof(name));
        if (OPAL_SUCCESS != rc) {
            continue; /* already caught in test_index_name_roundtrips */
        }

        /* name -> addr */
        rc = opal_ifnametoaddr(name,
                               (struct sockaddr *) &ss,
                               (int) sizeof(ss));
        if (OPAL_SUCCESS != rc) {
            /* some virtual interfaces may have no address; skip */
            continue;
        }

        test_verify("opal_ifnametoaddr() address family is AF_INET or AF_INET6",
                    AF_INET  == ((struct sockaddr *) &ss)->sa_family ||
                    AF_INET6 == ((struct sockaddr *) &ss)->sa_family);

        /*
         * addr -> name (dotted-quad round-trip for IPv4 only).
         * We do not assert the recovered name matches the original because
         * a single address may appear under multiple interface indices.
         * We assert the call returns OPAL_SUCCESS (the address exists in the
         * list).
         */
        if (AF_INET == ((struct sockaddr *) &ss)->sa_family) {
            char dotquad[INET_ADDRSTRLEN];
            char back_name[OPAL_IF_NAMESIZE];

            inet_ntop(AF_INET,
                      &((struct sockaddr_in *) &ss)->sin_addr,
                      dotquad, sizeof(dotquad));

            rc = opal_ifaddrtoname(dotquad, back_name, (int) sizeof(back_name));
            test_verify("opal_ifaddrtoname() succeeds for address from opal_ifnametoaddr()",
                        OPAL_SUCCESS == rc);
        }
    }
}

/* ------------------------------------------------------------------ */
/* Test: find the loopback interface; assert opal_ifisloopback         */
/* ------------------------------------------------------------------ */

static void
test_loopback(void)
{
    int idx;
    int found_loopback_index = -1;

    for (idx = opal_ifbegin(); idx > 0; idx = opal_ifnext(idx)) {
        struct sockaddr_storage ss;
        int rc;
        int is_loopback = 0;

        rc = opal_ifindextoaddr(idx,
                                (struct sockaddr *) &ss,
                                (unsigned int) sizeof(ss));
        if (OPAL_SUCCESS != rc) {
            continue;
        }

        /* Accept IPv4 loopback (127.x.x.x) */
        if (addr_is_ipv4_loopback((struct sockaddr *) &ss)) {
            is_loopback = 1;
        }

#if OPAL_ENABLE_IPV6
        /* Also accept IPv6 loopback (::1) */
        if (!is_loopback && AF_INET6 == ((struct sockaddr *) &ss)->sa_family) {
            const struct sockaddr_in6 *sin6 = (const struct sockaddr_in6 *) &ss;
            if (IN6_IS_ADDR_LOOPBACK(&sin6->sin6_addr)) {
                is_loopback = 1;
            }
        }
#endif /* OPAL_ENABLE_IPV6 */

        if (is_loopback && -1 == found_loopback_index) {
            found_loopback_index = idx;
        }
    }

    test_verify("at least one loopback interface found",
                found_loopback_index > 0);

    if (found_loopback_index > 0) {
        test_verify("opal_ifisloopback() true for loopback interface",
                    opal_ifisloopback(found_loopback_index));
    }

    /* Check that 127.0.0.1 is reported as local */
    test_verify("opal_ifislocal(\"127.0.0.1\") is true",
                opal_ifislocal("127.0.0.1"));
    test_verify("opal_ifislocal(\"localhost\") is true",
                opal_ifislocal("localhost"));
}

/* ------------------------------------------------------------------ */
/* Test: opal_ifislocal negative cases                                 */
/* ------------------------------------------------------------------ */

static void
test_ifislocal_negative(void)
{
    /*
     * foo.example.com is an IANA-reserved domain that will not resolve
     * to any local interface on any conforming host.
     */
    test_verify("opal_ifislocal(\"foo.example.com\") is false",
                !opal_ifislocal("foo.example.com"));

    /* 0.0.0.0 is not a local interface address */
    test_verify("opal_ifislocal(\"0.0.0.0\") is false",
                !opal_ifislocal("0.0.0.0"));
}

/* ------------------------------------------------------------------ */
/* Test: error / boundary cases                                        */
/* ------------------------------------------------------------------ */

static void
test_boundary_cases(void)
{
    char name[OPAL_IF_NAMESIZE];

    /* Negative index must fail gracefully */
    test_verify("opal_ifindextoname(-1) does not succeed",
                OPAL_SUCCESS != opal_ifindextoname(-1, name, (int) sizeof(name)));

    /* Zero index must fail gracefully */
    test_verify("opal_ifindextoname(0) does not succeed",
                OPAL_SUCCESS != opal_ifindextoname(0, name, (int) sizeof(name)));

    /* Non-existent name */
    test_verify("opal_ifnametoindex(\"__no_such_iface__\") returns negative",
                opal_ifnametoindex("__no_such_iface__") < 0);

    /* opal_ifnext at end must return <= 0 */
    {
        int last_idx = opal_ifbegin();
        int next;
        while ((next = opal_ifnext(last_idx)) > 0) {
            last_idx = next;
        }
        test_verify("opal_ifnext() past last interface returns <= 0",
                    opal_ifnext(last_idx) <= 0);
    }
}

/* ------------------------------------------------------------------ */
/* main                                                                */
/* ------------------------------------------------------------------ */

int main(int argc, char *argv[])
{
    int rc;

    /*
     * Ask the MCA "if" framework to retain loopback interfaces.  This
     * parameter defaults to false, and the BSD interface enumerator
     * (opal/mca/if/bsdx_ipv4, used on the *BSDs) honors it and drops the
     * loopback interface -- so without this, 127.0.0.1 / lo0 would not be in
     * the interface list and the loopback-related checks below could not run.
     * (Linux and macOS use posix_ipv4, whose loopback filter has been #if 0'd
     * out since 2010, so loopback is always present there regardless of this
     * setting.)  Set the MCA parameter via the environment before opal_init()
     * reads it; the "if" base registers the variable during opal_init(),
     * overwriting any direct assignment to the global, so the environment is
     * the reliable knob.  This platform divergence (and options for fixing it
     * in OPAL) is tracked in https://github.com/open-mpi/ompi/issues/14029.
     */
    setenv("OMPI_MCA_if_base_retain_loopback", "1", 1);

    rc = opal_init(&argc, &argv);
    test_init("opal_if");
    test_verify("opal_init() succeeds", OPAL_SUCCESS == rc);

    test_ifcount();
    test_iteration();
    test_index_name_roundtrips();
    test_name_addr_roundtrips();
    test_loopback();
    test_ifislocal_negative();
    test_boundary_cases();

    {
        int r = test_finalize();
        opal_finalize();
        return r;
    }
}
