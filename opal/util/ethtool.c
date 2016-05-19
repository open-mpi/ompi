/*
 * Copyright (c) 2016 Karol Mroz.  All rights reserved.
 * Copyright (c) 2016      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include <string.h>
#include <limits.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_NET_IF_H
#include <net/if.h>
#endif
#ifdef HAVE_LINUX_ETHTOOL_H
#include <linux/ethtool.h>
#endif
#ifdef HAVE_SYS_IOCTL_H
#include <sys/ioctl.h>
#endif
#ifdef HAVE_LINUX_SOCKIOS_H
#include <linux/sockios.h>
#endif

#include "opal/util/ethtool.h"
#include "opal/util/if.h"

#if ! HAVE_DECL_ETHTOOL_CMD_SPEED
static inline unsigned int
ethtool_cmd_speed(const struct ethtool_cmd *ep)
{
#ifdef HAVE_STRUCT_ETHTOOL_CMD_SPEED_HI
    return (ep->speed_hi << 16) | ep->speed;
#else
    return ep->speed;
#endif
}
#endif

/*
 * Obtain an appropriate bandwidth for the interface if_name. On Linux, we
 * get this via an ioctl(). Elsewhere or in the error case, we return the
 * speed as 0.
 */
unsigned int
opal_ethtool_get_speed (const char *if_name)
{
    unsigned int speed = 0;

#if defined(HAVE_DECL_SIOCETHTOOL) && defined(HAVE_STRUCT_IFREQ) && defined(HAVE_STRUCT_ETHTOOL_CMD)
    int sockfd;
    struct ifreq ifr;
    struct ethtool_cmd edata = {
        .cmd = ETHTOOL_GSET,
    };

    sockfd = socket(PF_INET, SOCK_DGRAM, 0);
    if (sockfd < 0) {
        goto out;
    }

    memset(&ifr, 0, sizeof(struct ifreq));
    strncpy(ifr.ifr_name, if_name, IF_NAMESIZE);
    ifr.ifr_data = (char *)&edata;

    if (ioctl(sockfd, SIOCETHTOOL, &ifr) < 0) {
        goto out;
    }

    speed = ethtool_cmd_speed(&edata);
    if (UINT_MAX == speed) {
        speed = 0;
    }

out:
    close(sockfd);
    return speed;
#else
    return speed;
#endif
}
