/*
 * Copyright (c) 2010-2019 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
 * Copyright (c) 2018      Amazon.com, Inc. or its affiliates.  All Rights
 *                         reserved.
 * Copyright (c) 2019      Triad National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include <string.h>
#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif
#include <errno.h>
#ifdef HAVE_SYS_TYPES_H
#    include <sys/types.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
#    include <sys/socket.h>
#endif
#ifdef HAVE_SYS_SOCKIO_H
#    include <sys/sockio.h>
#endif
#ifdef HAVE_SYS_IOCTL_H
#    include <sys/ioctl.h>
#endif
#ifdef HAVE_NETINET_IN_H
#    include <netinet/in.h>
#endif
#ifdef HAVE_ARPA_INET_H
#    include <arpa/inet.h>
#endif
#ifdef HAVE_NET_IF_H
#    include <net/if.h>
#endif
#ifdef HAVE_NETDB_H
#    include <netdb.h>
#endif
#ifdef HAVE_IFADDRS_H
#    include <ifaddrs.h>
#endif

#include "opal/constants.h"
#include "opal/mca/if/base/base.h"
#include "opal/mca/if/if.h"
#include "opal/runtime/opal.h"
#include "opal/util/if.h"
#include "opal/util/output.h"
#include "opal/util/show_help.h"
#include "opal/util/string_copy.h"

#define LOG_PREFIX "mca: if: linux_ipv6: "

static int if_linux_ipv6_open(void);

/* Discovers Linux IPv6 interfaces */
opal_if_base_component_t mca_if_linux_ipv6_component = {
    /* First, the mca_component_t struct containing meta information
       about the component itself */
    {OPAL_IF_BASE_VERSION_2_0_0,

     /* Component name and version */
     "linux_ipv6", OPAL_MAJOR_VERSION, OPAL_MINOR_VERSION, OPAL_RELEASE_VERSION,

     /* Component open and close functions */
     if_linux_ipv6_open, NULL},
    {/* This component is checkpointable */
     MCA_BASE_METADATA_PARAM_CHECKPOINT},
};

#if OPAL_ENABLE_IPV6
static bool hex2int(char hex, int *dst)
{
    if ('0' <= hex && hex <= '9') {
        *dst = hex - '0';
    } else if ('A' <= hex && hex <= 'F') {
        *dst = hex - 'A' + 10;
    } else if ('a' <= hex && hex <= 'f') {
        *dst = hex - 'a' + 10;
    } else {
        return false;
    }
    return true;
}

static bool hexdecode(const char *src, uint8_t *dst, size_t dstsize)
{
    int hi, lo;
    for (size_t i = 0; i < dstsize; i++) {
        if (hex2int(src[i * 2], &hi) && hex2int(src[i * 2 + 1], &lo)) {
            dst[i] = 16 * hi + lo;
        } else {
            return false;
        }
    }
    return true;
}
#endif

static int if_linux_ipv6_open(void)
{
#if OPAL_ENABLE_IPV6
    FILE *f;
    if ((f = fopen("/proc/net/if_inet6", "r"))) {
        char ifname[OPAL_IF_NAMESIZE];
        unsigned int idx, pfxlen, scope, dadstat;
        struct in6_addr a6;
        uint32_t flag;
        char addrhex[sizeof a6.s6_addr * 2 + 1];
        char addrstr[INET6_ADDRSTRLEN];

        while (fscanf(f, "%s %x %x %x %x %s\n", addrhex, &idx, &pfxlen, &scope, &dadstat, ifname)
               != EOF) {
            opal_if_t *intf;

            if (!hexdecode(addrhex, a6.s6_addr, sizeof a6.s6_addr)) {
                const char *hostname;
                hostname = opal_gethostname();
                opal_show_help("help-opal-if-linux-ipv6.txt", "fail to parse if_inet6", true,
                               hostname, ifname, addrhex);
                continue;
            };
            inet_ntop(AF_INET6, a6.s6_addr, addrstr, sizeof addrstr);

            opal_output_verbose(1, opal_if_base_framework.framework_output,
                                LOG_PREFIX "found interface %s inet6 %s scope %x\n", ifname,
                                addrstr, scope);

            /* Only interested in global (0x00) scope */
            if (scope != 0x00) {
                opal_output_verbose(1, opal_if_base_framework.framework_output,
                                    LOG_PREFIX "skipped interface %s inet6 %s scope %x\n", ifname,
                                    addrstr, scope);
                continue;
            }

            intf = OBJ_NEW(opal_if_t);
            if (NULL == intf) {
                opal_output(0, LOG_PREFIX "unable to allocate %lu bytes\n",
                            (unsigned long) sizeof(opal_if_t));
                fclose(f);
                return OPAL_ERR_OUT_OF_RESOURCE;
            }
            intf->af_family = AF_INET6;

            /* now construct the opal_if_t */
            opal_string_copy(intf->if_name, ifname, OPAL_IF_NAMESIZE);
            intf->if_index = opal_list_get_size(&opal_if_list) + 1;
            intf->if_kernel_index = (uint16_t) idx;
            ((struct sockaddr_in6 *) &intf->if_addr)->sin6_addr = a6;
            ((struct sockaddr_in6 *) &intf->if_addr)->sin6_family = AF_INET6;
            ((struct sockaddr_in6 *) &intf->if_addr)->sin6_scope_id = scope;
            intf->if_mask = pfxlen;
            if (OPAL_SUCCESS == opal_ifindextoflags(opal_ifnametoindex(ifname), &flag)) {
                intf->if_flags = flag;
            } else {
                intf->if_flags = IFF_UP;
            }

            /* copy new interface information to heap and append
               to list */
            opal_list_append(&opal_if_list, &(intf->super));
            opal_output_verbose(1, opal_if_base_framework.framework_output,
                                LOG_PREFIX "added interface %s inet6 %s scope %x\n", ifname,
                                addrstr, scope);
        } /* of while */
        fclose(f);
    }
#endif /* OPAL_ENABLE_IPV6 */

    return OPAL_SUCCESS;
}
