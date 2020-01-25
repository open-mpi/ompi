/*
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
 * Copyright (c) 2016      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2016-2019 Intel, Inc.  All rights reserved.
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

#include "src/util/output.h"
#include "src/util/pif.h"
#include "src/mca/pif/pif.h"
#include "src/mca/pif/base/base.h"

static int if_solaris_ipv6_open(void);

/* Discovers Solaris IPv6 interfaces */
pmix_pif_base_component_t mca_pif_solaris_ipv6_component = {
    /* First, the mca_component_t struct containing meta information
       about the component itself */
    .base = {
        PMIX_PIF_BASE_VERSION_2_0_0,

        /* Component name and version */
        "solaris_ipv6",
        PMIX_MAJOR_VERSION,
        PMIX_MINOR_VERSION,
        PMIX_RELEASE_VERSION,

        /* Component open and close functions */
        if_solaris_ipv6_open,
        NULL
    },
    .data = {
        /* This component is checkpointable */
        PMIX_MCA_BASE_METADATA_PARAM_CHECKPOINT
    },
};

/* configure using getifaddrs(3) */
static int if_solaris_ipv6_open(void)
{
    int i;
    int sd;
    int error;
    uint16_t kindex;
    struct lifnum lifnum;
    struct lifconf lifconf;
    struct lifreq *lifreq, lifquery;

    sd = socket (AF_INET6, SOCK_DGRAM, 0);
    if (sd < 0) {
        pmix_output (0, "pmix_ifinit: unable to open IPv6 socket\n");
        return PMIX_ERROR;
    }

    /* we only ask for IPv6; IPv4 discovery has already been done */
    lifnum.lifn_family = AF_INET6;
    lifnum.lifn_flags = 0;
    lifnum.lifn_count = 0;

    /* get the number of interfaces in the system */
    error = ioctl (sd, SIOCGLIFNUM, &lifnum);
    if (error < 0) {
        pmix_output (0,
                     "pmix_ifinit: ioctl SIOCGLIFNUM failed with errno=%d\n", errno);
        return PMIX_ERROR;
    }

    memset (&lifconf, 0, sizeof (struct lifconf));
    memset (&lifquery, 0, sizeof (struct lifreq));
    lifconf.lifc_family = AF_INET6;
    lifconf.lifc_flags = 0;
    lifconf.lifc_len = lifnum.lifn_count * sizeof (struct lifreq) * 2;
    lifconf.lifc_buf = malloc (lifconf.lifc_len);
    if (NULL == lifconf.lifc_buf) {
        pmix_output (0, "pmix_ifinit: IPv6 discovery: malloc() failed\n");
        return PMIX_ERR_OUT_OF_RESOURCE;
    }

    memset (lifconf.lifc_buf, 0, lifconf.lifc_len);

    error = ioctl (sd, SIOCGLIFCONF, &lifconf);
    if (error < 0) {
        pmix_output (0,
                     "pmix_ifinit: IPv6 SIOCGLIFCONF failed with errno=%d\n", errno);
    }

    for (i = 0; i + sizeof (struct lifreq) <= lifconf.lifc_len;
         i += sizeof (*lifreq)) {

        lifreq = (struct lifreq *)((caddr_t)lifconf.lifc_buf + i);
        pmix_strncpy (lifquery.lifr_name, lifreq->lifr_name,
                 sizeof (lifquery.lifr_name)-1);

        /* lookup kernel index */
        error = ioctl (sd, SIOCGLIFINDEX, &lifquery);
        if (error < 0) {
            pmix_output (0,
                         "pmix_ifinit: SIOCGLIFINDEX failed with errno=%d\n", errno);
            return PMIX_ERROR;
        }
        kindex = lifquery.lifr_index;

        /* lookup interface flags */
        error = ioctl (sd, SIOCGLIFFLAGS, &lifquery);
        if (error < 0) {
            pmix_output (0,
                         "pmix_ifinit: SIOCGLIFFLAGS failed with errno=%d\n", errno);
            return PMIX_ERROR;
        }

        if (AF_INET6 == lifreq->lifr_addr.ss_family) {
            struct sockaddr_in6* my_addr = (struct sockaddr_in6*) &lifreq->lifr_addr;
            /* we surely want to check for sin6_scope_id, but Solaris
               does not set it correctly, so we have to look for
               global scope. For now, global is anything which is
               neither loopback nor link local.

               Bug, FIXME: site-local, multicast, ... missing
               Check for 2000::/3?
            */
            if ( (!pmix_if_retain_loopback && !IN6_IS_ADDR_LOOPBACK (&my_addr->sin6_addr)) &&
                 (! IN6_IS_ADDR_LINKLOCAL (&my_addr->sin6_addr))) {
                /* create interface for newly found address */
                pmix_pif_t *intf;

                intf = PMIX_NEW(pmix_pif_t);
                if (NULL == intf) {
                    pmix_output (0,
                                 "pmix_ifinit: unable to allocate %d bytes\n",
                                 sizeof (pmix_pif_t));
                    return PMIX_ERR_OUT_OF_RESOURCE;
                }
                intf->af_family = AF_INET6;

                pmix_strncpy (intf->if_name, lifreq->lifr_name, PMIX_IF_NAMESIZE-1);
                intf->if_index = pmix_list_get_size(&pmix_if_list)+1;
                memcpy(&intf->if_addr, my_addr, sizeof (*my_addr));
                intf->if_mask = 64;
                /* lifrq flags are uint64_t */
                intf->if_flags =
                    (uint32_t)(0x00000000ffffffff) & lifquery.lifr_flags;

                /* append to list */
                pmix_list_append (&pmix_if_list, &(intf->super));
            }
        }
    } /* for */

    if (NULL != lifconf.lifc_buf) {
        free (lifconf.lifc_buf);
    }

    return PMIX_SUCCESS;
}
