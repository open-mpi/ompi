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

#include <stdlib.h>
#include <string.h>

#include "opal/constants.h"
#include "opal/util/output.h"
#include "opal/mca/if/if.h"

static int if_solaris_ipv6_open(void);

/* Discovers Solaris IPv6 interfaces */
opal_if_base_component_t mca_if_solaris_ipv6_component = {
    /* First, the mca_component_t struct containing meta information
       about the component itself */
    {
        OPAL_IF_BASE_VERSION_2_0_0,

        /* Component name and version */
        "solaris_ipv6",
        OPAL_MAJOR_VERSION,
        OPAL_MINOR_VERSION,
        OPAL_RELEASE_VERSION,

        /* Component open and close functions */
        if_solaris_ipv6_open,
        NULL
    },
    {
        /* This component is checkpointable */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },
};

/* configure using getifaddrs(3) */
static int if_solaris_ipv6_open(void)
{
#if OPAL_ENABLE_IPV6
    int i;
    int sd;
    int error;
    uint16_t kindex;
    struct lifnum lifnum;
    struct lifconf lifconf;
    struct lifreq *lifreq, lifquery;

    sd = socket (AF_INET6, SOCK_DGRAM, 0);
    if (sd < 0) {
        opal_output (0, "opal_ifinit: unable to open IPv6 socket\n");
        return OPAL_ERROR;
    }

    /* we only ask for IPv6; IPv4 discovery has already been done */
    lifnum.lifn_family = AF_INET6;
    lifnum.lifn_flags = 0;
    lifnum.lifn_count = 0;

    /* get the number of interfaces in the system */
    error = ioctl (sd, SIOCGLIFNUM, &lifnum);
    if (error < 0) {
        opal_output (0,
                     "opal_ifinit: ioctl SIOCGLIFNUM failed with errno=%d\n", errno);
        return OPAL_ERROR;
    }

    memset (&lifconf, 0, sizeof (struct lifconf));
    memset (&lifquery, 0, sizeof (struct lifreq));
    lifconf.lifc_family = AF_INET6;
    lifconf.lifc_flags = 0;
    lifconf.lifc_len = lifnum.lifn_count * sizeof (struct lifreq) * 2;
    lifconf.lifc_buf = malloc (lifconf.lifc_len);
    if (NULL == lifconf.lifc_buf) {
        opal_output (0, "opal_ifinit: IPv6 discovery: malloc() failed\n");
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    memset (lifconf.lifc_buf, 0, lifconf.lifc_len);

    error = ioctl (sd, SIOCGLIFCONF, &lifconf);
    if (error < 0) {
        opal_output (0,
                     "opal_ifinit: IPv6 SIOCGLIFCONF failed with errno=%d\n", errno);
    }

    for (i = 0; i + sizeof (struct lifreq) <= lifconf.lifc_len;
         i += sizeof (*lifreq)) {

        lifreq = (struct lifreq *)((caddr_t)lifconf.lifc_buf + i);
        strncpy (lifquery.lifr_name, lifreq->lifr_name,
                 sizeof (lifquery.lifr_name));

        /* lookup kernel index */
        error = ioctl (sd, SIOCGLIFINDEX, &lifquery);
        if (error < 0) {
            opal_output (0,
                         "opal_ifinit: SIOCGLIFINDEX failed with errno=%d\n", errno);
            return OPAL_ERROR;
        }
        kindex = lifquery.lifr_index;

        /* lookup interface flags */
        error = ioctl (sd, SIOCGLIFFLAGS, &lifquery);
        if (error < 0) {
            opal_output (0,
                         "opal_ifinit: SIOCGLIFFLAGS failed with errno=%d\n", errno);
            return OPAL_ERROR;
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
            if ( (!opal_if_retain_loopback && !IN6_IS_ADDR_LOOPBACK (&my_addr->sin6_addr)) &&
                 (! IN6_IS_ADDR_LINKLOCAL (&my_addr->sin6_addr))) {
                /* create interface for newly found address */
                opal_if_t *intf;

                intf = OBJ_NEW(opal_if_t);
                if (NULL == intf) {
                    opal_output (0,
                                 "opal_ifinit: unable to allocate %d bytes\n",
                                 sizeof (opal_if_t));
                    return OPAL_ERR_OUT_OF_RESOURCE;
                }
                intf->af_family = AF_INET6;

                strncpy (intf->if_name, lifreq->lifr_name, IF_NAMESIZE);
                intf->if_index = opal_list_get_size(&opal_if_list)+1;
                memcpy(&intf->if_addr, my_addr, sizeof (*my_addr));
                intf->if_mask = 64;
                /* lifrq flags are uint64_t */
                intf->if_flags =
                    (uint32_t)(0x00000000ffffffff) & lifquery.lifr_flags;

                /* append to list */
                opal_list_append (&opal_if_list, &(intf->super));
            }
        }
    } /* for */

    if (NULL != lifconf.lifc_buf) {
        free (lifconf.lifc_buf);
    }
#endif  /* OPAL_ENABLE_IPV6 */

    return OPAL_SUCCESS;
}


