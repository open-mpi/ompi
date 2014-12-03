/*
 * Copyright (c) 2014, Cisco Systems, Inc. All rights reserved.
 *
 * LICENSE_BEGIN
 *
 * This software is available to you under a choice of one of two
 * licenses.  You may choose to be licensed under the terms of the GNU
 * General Public License (GPL) Version 2, available from the file
 * COPYING in the main directory of this source tree, or the
 * BSD license below:
 *
 *     Redistribution and use in source and binary forms, with or
 *     without modification, are permitted provided that the following
 *     conditions are met:
 *
 *      - Redistributions of source code must retain the above
 *        copyright notice, this list of conditions and the following
 *        disclaimer.
 *
 *      - Redistributions in binary form must reproduce the above
 *        copyright notice, this list of conditions and the following
 *        disclaimer in the documentation and/or other materials
 *        provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *
 * LICENSE_END
 *
 *
 */

#include <stdio.h>
#include <errno.h>

#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <net/if.h>
#include <net/if_arp.h>
#include <netinet/in.h>

#include "usd.h"
#include "usd_util.h"
#include "usd_socket.h"

/*
 * Get the IP address and other information associated with this
 * device's interface.
 */
int
usd_get_dev_if_info(
    struct usd_device *dev)
{
    struct sockaddr_in sin;
    struct ifreq ifr;
    struct usd_device_attrs *dp;
    uint32_t netmask;
    int s;
    int ret;

    s = socket(AF_INET, SOCK_DGRAM, 0);
    if (s == -1)
        return -errno;

    dp = &dev->ud_attrs;

    dp->uda_ifindex = if_nametoindex(dp->uda_ifname);
    if (dp->uda_ifindex == 0)
        goto out;

    ifr.ifr_addr.sa_family = AF_INET;
    strncpy(ifr.ifr_name, dp->uda_ifname, IFNAMSIZ - 1);

    ret = ioctl(s, SIOCGIFADDR, &ifr);
    if (ret == 0) {
        dp->uda_ipaddr_be =
            ((struct sockaddr_in *) &ifr.ifr_addr)->sin_addr.s_addr;
    }

    ret = ioctl(s, SIOCGIFNETMASK, &ifr);
    if (ret == 0) {
        dp->uda_netmask_be =
            ((struct sockaddr_in *) &ifr.ifr_netmask)->sin_addr.s_addr;
        netmask = ntohl(dp->uda_netmask_be);
        dp->uda_prefixlen = 32 - msbit(~netmask);
    }

    ret = ioctl(s, SIOCGIFMTU, &ifr);
    if (ret == 0) {
        dp->uda_mtu = ifr.ifr_mtu;
    }

    if (dp->uda_ipaddr_be != 0) {
        memset(&sin, 0, sizeof(sin));
        sin.sin_family = AF_INET;
        sin.sin_addr.s_addr = dp->uda_ipaddr_be;
        sin.sin_port = 0;
        ret = bind(s, (struct sockaddr *) &sin, sizeof(sin));
        if (ret == -1)
            goto out;
        dev->ud_arp_sockfd = s;
    }

    return 0;
  out:
    close(s);
    return -errno;
}
