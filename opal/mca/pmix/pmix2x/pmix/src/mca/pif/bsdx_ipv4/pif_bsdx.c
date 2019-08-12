/*
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
 * Copyright (c) 2018-2019 Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "pmix_config.h"
#include "pmix_common.h"

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
#include <net/if.h>
#endif
#ifdef HAVE_NETDB_H
#include <netdb.h>
#endif
#ifdef HAVE_IFADDRS_H
#include <ifaddrs.h>
#endif

#include "src/util/output.h"
#include "src/util/pif.h"
#include "src/mca/pif/pif.h"
#include "src/mca/pif/base/base.h"

static int if_bsdx_open(void);

/* Supports specific flavors of BSD:
 * NetBSD
 * FreeBSD
 * OpenBSD
 * DragonFly
 */
pmix_pif_base_component_t mca_pif_bsdx_ipv4_component = {
    /* First, the mca_component_t struct containing meta information
       about the component itself */
    .base = {
        PMIX_PIF_BASE_VERSION_2_0_0,

        /* Component name and version */
        "bsdx_ipv4",
        PMIX_MAJOR_VERSION,
        PMIX_MINOR_VERSION,
        PMIX_RELEASE_VERSION,

        /* Component open and close functions */
        if_bsdx_open,
        NULL
    },
    .data = {
        /* This component is checkpointable */
        PMIX_MCA_BASE_METADATA_PARAM_CHECKPOINT
    },
};

/* convert a netmask (in network byte order) to CIDR notation */
static int prefix (uint32_t netmask)
{
    uint32_t mask = ntohl(netmask);
    int plen = 0;

    if (0 == mask) {
        plen = 32;
    } else {
        while ((mask % 2) == 0) {
            plen += 1;
            mask /= 2;
        }
    }

    return (32 - plen);
}

/* configure using getifaddrs(3) */
static int if_bsdx_open(void)
{
    struct ifaddrs **ifadd_list;
    struct ifaddrs *cur_ifaddrs;
    struct sockaddr_in* sin_addr;

    /*
     * the manpage claims that getifaddrs() allocates the memory,
     * and freeifaddrs() is later used to release the allocated memory.
     * however, without this malloc the call to getifaddrs() segfaults
     */
    ifadd_list = (struct ifaddrs **) malloc(sizeof(struct ifaddrs*));

    /* create the linked list of ifaddrs structs */
    if (getifaddrs(ifadd_list) < 0) {
        pmix_output(0, "pmix_ifinit: getifaddrs() failed with error=%d\n",
                    errno);
        return PMIX_ERROR;
    }

    for (cur_ifaddrs = *ifadd_list; NULL != cur_ifaddrs;
         cur_ifaddrs = cur_ifaddrs->ifa_next) {
        pmix_pif_t *intf;
        struct in_addr a4;

        /* skip non- af_inet interface addresses */
        if (AF_INET != cur_ifaddrs->ifa_addr->sa_family) {
            continue;
        }

        /* skip interface if it is down (IFF_UP not set) */
        if (0 == (cur_ifaddrs->ifa_flags & IFF_UP)) {
            continue;
        }

        /* skip interface if it is a loopback device (IFF_LOOPBACK set) */
        if (!pmix_if_retain_loopback && 0 != (cur_ifaddrs->ifa_flags & IFF_LOOPBACK)) {
            continue;
        }

        /* or if it is a point-to-point interface */
        /* TODO: do we really skip p2p? */
        if (0 != (cur_ifaddrs->ifa_flags & IFF_POINTOPOINT)) {
            continue;
        }

        sin_addr = (struct sockaddr_in *) cur_ifaddrs->ifa_addr;

        intf = PMIX_NEW(pmix_pif_t);
        if (NULL == intf) {
            pmix_output(0, "pmix_ifinit: unable to allocate %d bytes\n",
                        (int) sizeof(pmix_pif_t));
            return PMIX_ERR_OUT_OF_RESOURCE;
        }
        intf->af_family = AF_INET;

        /* fill values into the pmix_pif_t */
        memcpy(&a4, &(sin_addr->sin_addr), sizeof(struct in_addr));

        pmix_strncpy(intf->if_name, cur_ifaddrs->ifa_name, PMIX_IF_NAMESIZE-1);
        intf->if_index = pmix_list_get_size(&pmix_if_list) + 1;
        ((struct sockaddr_in*) &intf->if_addr)->sin_addr = a4;
        ((struct sockaddr_in*) &intf->if_addr)->sin_family = AF_INET;
        ((struct sockaddr_in*) &intf->if_addr)->sin_len =  cur_ifaddrs->ifa_addr->sa_len;

        intf->if_mask = prefix( sin_addr->sin_addr.s_addr);
        intf->if_flags = cur_ifaddrs->ifa_flags;

        intf->if_kernel_index =
            (uint16_t) if_nametoindex(cur_ifaddrs->ifa_name);

        pmix_list_append(&pmix_if_list, &(intf->super));
    }   /*  of for loop over ifaddrs list */

    return PMIX_SUCCESS;
}
