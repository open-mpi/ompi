/*
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"
#include "opal/constants.h"
#include "opal/util/output.h"

#include <string.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <errno.h>
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif
#ifdef HAVE_SYS_SOCKIO_H
#include <sys/sockio.h>
#endif
#ifdef HAVE_SYS_IOCTL_H
#include <sys/ioctl.h>
#endif
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#ifdef HAVE_ARPA_INET_H
#include <arpa/inet.h>
#endif
#ifdef HAVE_NET_IF_H
#if defined(__APPLE__) && defined(_LP64)
/* Apple engineering suggested using options align=power as a
   workaround for a bug in OS X 10.4 (Tiger) that prevented ioctl(...,
   SIOCGIFCONF, ...) from working properly in 64 bit mode on Power PC.
   It turns out that the underlying issue is the size of struct
   ifconf, which the kernel expects to be 12 and natural 64 bit
   alignment would make 16.  The same bug appears in 64 bit mode on
   Intel macs, but align=power is a no-op there, so instead, use the
   pack pragma to instruct the compiler to pack on 4 byte words, which
   has the same effect as align=power for our needs and works on both
   Intel and Power PC Macs. */
#pragma pack(push,4)
#endif
#include <net/if.h>
#if defined(__APPLE__) && defined(_LP64)
#pragma pack(pop)
#endif
#endif
#ifdef HAVE_NETDB_H
#include <netdb.h>
#endif
#ifdef HAVE_IFADDRS_H
#include <ifaddrs.h>
#endif

#include "opal/mca/if/if.h"

static int if_bsdx_ipv6_open(void);

/* Discovers IPv6 interfaces for:
 *
 * NetBSD
 * OpenBSD
 * FreeBSD
 * 386BSD
 * bsdi
 * Apple
 */
opal_if_base_component_t mca_if_bsdx_ipv6_component = {
    /* First, the mca_component_t struct containing meta information
       about the component itself */
    {
        OPAL_IF_BASE_VERSION_2_0_0,

        /* Component name and version */
        "bsdx_ipv6",
        OPAL_MAJOR_VERSION,
        OPAL_MINOR_VERSION,
        OPAL_RELEASE_VERSION,

        /* Component open and close functions */
        if_bsdx_ipv6_open,
        NULL
    },
    {
        /* This component is checkpointable */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },
};

/* configure using getifaddrs(3) */
static int if_bsdx_ipv6_open(void)
{
    struct ifaddrs **ifadd_list;
    struct ifaddrs *cur_ifaddrs;
    struct sockaddr_in6* sin_addr;

    /* 
     * the manpage claims that getifaddrs() allocates the memory,
     * and freeifaddrs() is later used to release the allocated memory.
     * however, without this malloc the call to getifaddrs() segfaults
     */
    ifadd_list = (struct ifaddrs **) malloc(sizeof(struct ifaddrs*));

    /* create the linked list of ifaddrs structs */
    if (getifaddrs(ifadd_list) < 0) {
        opal_output(0, "opal_ifinit: getifaddrs() failed with error=%d\n",
                    errno);
        return OPAL_ERROR;
    }

    for (cur_ifaddrs = *ifadd_list; NULL != cur_ifaddrs; 
         cur_ifaddrs = cur_ifaddrs->ifa_next) {
        opal_if_t *intf;
        struct in6_addr a6;

        /* skip non-ipv6 interface addresses */
        if (AF_INET6 != cur_ifaddrs->ifa_addr->sa_family) {
#if 0
            printf("skipping non-ipv6 interface %s.\n", cur_ifaddrs->ifa_name);
#endif
            continue;
        }

        /* skip interface if it is down (IFF_UP not set) */
        if (0 == (cur_ifaddrs->ifa_flags & IFF_UP)) {
#if 0
            printf("skipping non-up interface %s.\n", cur_ifaddrs->ifa_name);
#endif
            continue;
        }

        /* skip interface if it is a loopback device (IFF_LOOPBACK set) */
        if (!opal_if_retain_loopback && 0 != (cur_ifaddrs->ifa_flags & IFF_LOOPBACK)) {
            continue;
        }

        /* or if it is a point-to-point interface */
        /* TODO: do we really skip p2p? */
        if (0!= (cur_ifaddrs->ifa_flags & IFF_POINTOPOINT)) {
#if 0
            printf("skipping loopback interface %s.\n", cur_ifaddrs->ifa_name);
#endif              
            continue;
        }

        sin_addr = (struct sockaddr_in6 *) cur_ifaddrs->ifa_addr;
        intf = OBJ_NEW(opal_if_t);
        if (NULL == intf) {
            opal_output(0, "opal_ifinit: unable to allocate %lu bytes\n",
                        sizeof(opal_if_t));
            return OPAL_ERR_OUT_OF_RESOURCE;
        }

        /* 
         * skip IPv6 address starting with fe80:, as this is supposed to be
         * link-local scope. sockaddr_in6->sin6_scope_id doesn't always work
         * TODO: test whether scope id is set to a sensible value on 
         * linux and/or bsd (including osx)
         *
         * MacOSX: fe80::... has a scope of 0, but ifconfig -a shows
         * a scope of 4 on that particular machine, 
         * so the scope returned by getifaddrs() isn't working properly
         */

        if ((IN6_IS_ADDR_LINKLOCAL (&sin_addr->sin6_addr))) {
#if 0
            printf("skipping link-local ipv6 address on interface \
                        %s with scope %d.\n", 
                   cur_ifaddrs->ifa_name, sin_addr->sin6_scope_id);
#endif
            continue;
        }

#if 0
        char *addr_name = (char *) malloc(48*sizeof(char));
        inet_ntop(AF_INET6, &sin_addr->sin6_addr, addr_name, 48*sizeof(char));
        opal_output(0, "ipv6 capable interface %s discovered, address %s.\n", 
                    cur_ifaddrs->ifa_name, addr_name);
        free(addr_name);
#endif

        /* fill values into the opal_if_t */
        memcpy(&a6, &(sin_addr->sin6_addr), sizeof(struct in6_addr));
            
        strncpy(intf->if_name, cur_ifaddrs->ifa_name, IF_NAMESIZE);
        intf->if_index = opal_list_get_size(&opal_if_list) + 1;
        ((struct sockaddr_in6*) &intf->if_addr)->sin6_addr = a6;
        ((struct sockaddr_in6*) &intf->if_addr)->sin6_family = AF_INET6;

        /* since every scope != 0 is ignored, we just set the scope to 0 */
        ((struct sockaddr_in6*) &intf->if_addr)->sin6_scope_id = 0;

        /*
         * hardcoded netmask, adrian says that's ok
         */
        intf->if_mask = 64;
        intf->if_flags = cur_ifaddrs->ifa_flags;

        /*
         * FIXME: figure out how to gain access to the kernel index
         * (or create our own), getifaddrs() does not contain such
         * data
         */
        intf->if_kernel_index = 
            (uint16_t) if_nametoindex(cur_ifaddrs->ifa_name);
        opal_list_append(&opal_if_list, &(intf->super));
    }   /*  of for loop over ifaddrs list */

    return OPAL_SUCCESS;
}


