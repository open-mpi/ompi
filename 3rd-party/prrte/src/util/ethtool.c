/*
 * Copyright (c) 2016      Karol Mroz.  All rights reserved.
 * Copyright (c) 2016      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2016-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2019      Intel, Inc.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "prte_config.h"

#include <limits.h>
#include <string.h>

#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#    include <sys/types.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
#    include <sys/socket.h>
#endif
#ifdef HAVE_NET_IF_H
#    include <net/if.h>
#endif
#ifdef HAVE_LINUX_ETHTOOL_H
#    include <linux/ethtool.h>
#endif
#ifdef HAVE_SYS_IOCTL_H
#    include <sys/ioctl.h>
#endif
#ifdef HAVE_LINUX_SOCKIOS_H
#    include <linux/sockios.h>
#endif

#include "src/runtime/prte_globals.h"
#include "src/util/ethtool.h"
#include "src/util/pmix_if.h"
#include "src/util/pmix_string_copy.h"

/*
 * Obtain an appropriate bandwidth for the interface if_name. On Linux, we
 * get this via an ioctl(). Elsewhere or in the error case, we return the
 * speed as 0.
 */
unsigned int prte_ethtool_get_speed(const char *if_name)
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
        return 0;
    }

    memset(&ifr, 0, sizeof(struct ifreq));
    pmix_string_copy(ifr.ifr_name, if_name, PMIX_IF_NAMESIZE);
    ifr.ifr_data = (char *) &edata;

    if (ioctl(sockfd, SIOCETHTOOL, &ifr) < 0) {
        goto out;
    }

#    if HAVE_DECL_ETHTOOL_CMD_SPEED
    speed = ethtool_cmd_speed(&edata);
#    elif defined(HAVE_STRUCT_ETHTOOL_CMD_SPEED_HI)
    speed = (edata.speed_hi << 16) | edata.speed;
#    else
    speed = edata.speed;
#    endif
    if (UINT_MAX == speed) {
        speed = 0;
    }

out:
    close(sockfd);
#else
    PRTE_HIDE_UNUSED_PARAMS(if_name);
#endif

    return speed;
}
