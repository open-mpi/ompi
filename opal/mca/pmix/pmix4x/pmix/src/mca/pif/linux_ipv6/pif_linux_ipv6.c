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

static int if_linux_ipv6_open(void);

/* Discovers Linux IPv6 interfaces */
pmix_pif_base_component_t mca_pif_linux_ipv6_component = {
    /* First, the mca_component_t struct containing meta information
       about the component itself */
    .base = {
        PMIX_PIF_BASE_VERSION_2_0_0,

        /* Component name and version */
        "linux_ipv6",
        PMIX_MAJOR_VERSION,
        PMIX_MINOR_VERSION,
        PMIX_RELEASE_VERSION,

        /* Component open and close functions */
        if_linux_ipv6_open,
        NULL
    },
    .data = {
        /* This component is checkpointable */
        PMIX_MCA_BASE_METADATA_PARAM_CHECKPOINT
    },
};

/* configure using getifaddrs(3) */
static int if_linux_ipv6_open(void)
{
    FILE *f;
    if ((f = fopen("/proc/net/if_inet6", "r"))) {
        /* IF_NAMESIZE is normally 16 on Linux,
           but the next scanf allows up to 21 bytes */
        char ifname[PMIX_IF_NAMESIZE];
        unsigned int idx, pfxlen, scope, dadstat;
        struct in6_addr a6;
        int iter;
        uint32_t flag;
        unsigned int addrbyte[PMIX_IF_NAMESIZE];

        memset(addrbyte, 0, PMIX_IF_NAMESIZE*sizeof(unsigned int));
        memset(ifname, 0, PMIX_IF_NAMESIZE*sizeof(char));

        while (fscanf(f, "%2x%2x%2x%2x%2x%2x%2x%2x%2x%2x%2x%2x%2x%2x%2x%2x %x %x %x %x %20s\n",
                      &addrbyte[0], &addrbyte[1], &addrbyte[2], &addrbyte[3],
                      &addrbyte[4], &addrbyte[5], &addrbyte[6], &addrbyte[7],
                      &addrbyte[8], &addrbyte[9], &addrbyte[10], &addrbyte[11],
                      &addrbyte[12], &addrbyte[13], &addrbyte[14], &addrbyte[15],
                      &idx, &pfxlen, &scope, &dadstat, ifname) != EOF) {
            pmix_pif_t *intf;

            pmix_output_verbose(1, pmix_pif_base_framework.framework_output,
                                "found interface %2x%2x:%2x%2x:%2x%2x:%2x%2x:%2x%2x:%2x%2x:%2x%2x:%2x%2x scope %x\n",
                                addrbyte[0], addrbyte[1], addrbyte[2], addrbyte[3],
                                addrbyte[4], addrbyte[5], addrbyte[6], addrbyte[7],
                                addrbyte[8], addrbyte[9], addrbyte[10], addrbyte[11],
                                addrbyte[12], addrbyte[13], addrbyte[14], addrbyte[15], scope);

            /* Only interested in global (0x00) scope */
            if (scope != 0x00) {
                pmix_output_verbose(1, pmix_pif_base_framework.framework_output,
                                    "skipping interface %2x%2x:%2x%2x:%2x%2x:%2x%2x:%2x%2x:%2x%2x:%2x%2x:%2x%2x scope %x\n",
                                    addrbyte[0], addrbyte[1], addrbyte[2], addrbyte[3],
                                    addrbyte[4], addrbyte[5], addrbyte[6], addrbyte[7],
                                    addrbyte[8], addrbyte[9], addrbyte[10], addrbyte[11],
                                    addrbyte[12], addrbyte[13], addrbyte[14], addrbyte[15], scope);
                continue;
            }

            intf = PMIX_NEW(pmix_pif_t);
            if (NULL == intf) {
                pmix_output(0, "pmix_ifinit: unable to allocate %lu bytes\n",
                            (unsigned long)sizeof(pmix_pif_t));
                fclose(f);
                return PMIX_ERR_OUT_OF_RESOURCE;
            }
            intf->af_family = AF_INET6;

            for (iter = 0; iter < 16; iter++) {
                a6.s6_addr[iter] = addrbyte[iter];
            }

            /* now construct the pmix_pif_t */
            pmix_strncpy(intf->if_name, ifname, PMIX_IF_NAMESIZE-1);
            intf->if_index = pmix_list_get_size(&pmix_if_list)+1;
            intf->if_kernel_index = (uint16_t) idx;
            ((struct sockaddr_in6*) &intf->if_addr)->sin6_addr = a6;
            ((struct sockaddr_in6*) &intf->if_addr)->sin6_family = AF_INET6;
            ((struct sockaddr_in6*) &intf->if_addr)->sin6_scope_id = scope;
            intf->if_mask = pfxlen;
            if (PMIX_SUCCESS == pmix_ifindextoflags(pmix_ifnametoindex (ifname), &flag)) {
                intf->if_flags = flag;
            } else {
                intf->if_flags = IFF_UP;
            }

            /* copy new interface information to heap and append
               to list */
            pmix_list_append(&pmix_if_list, &(intf->super));
            pmix_output_verbose(1, pmix_pif_base_framework.framework_output,
                                "added interface %2x%2x:%2x%2x:%2x%2x:%2x%2x:%2x%2x:%2x%2x:%2x%2x:%2x%2x\n",
                                addrbyte[0], addrbyte[1], addrbyte[2], addrbyte[3],
                                addrbyte[4], addrbyte[5], addrbyte[6], addrbyte[7],
                                addrbyte[8], addrbyte[9], addrbyte[10], addrbyte[11],
                                addrbyte[12], addrbyte[13], addrbyte[14], addrbyte[15]);
        } /* of while */
        fclose(f);
    }

    return PMIX_SUCCESS;
}
