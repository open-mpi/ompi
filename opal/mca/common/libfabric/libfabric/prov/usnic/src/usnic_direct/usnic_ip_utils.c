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
#include "config.h"
 
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <errno.h>
#include <time.h>
#include <string.h>
#include <netinet/in.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <net/if.h>
#include <arpa/inet.h>
#include <net/if_arp.h>

#include "libnl_utils.h"
#include "usnic_user_utils.h"
#include "usnic_ip_utils.h"

int usnic_arp_lookup(char *ifname, uint32_t ipaddr, int sockfd, uint8_t *macaddr)
{
	struct arpreq		req;
	struct sockaddr_in	sinp;
	int			err;
	int			status;

	memset(&req, 0, sizeof req);
	strcpy(req.arp_dev, ifname);
	memset(&sinp, 0, sizeof(sinp));
	sinp.sin_family = AF_INET;
	sinp.sin_addr.s_addr = ipaddr;
	memcpy(&req.arp_pa, &sinp, sizeof(sinp));

	err = 0;
	status = ioctl(sockfd, SIOCGARP, (char *)&req, sizeof(req));
	if (status != -1 && (req.arp_flags & ATF_COM))
		memcpy(macaddr, req.arp_ha.sa_data, 6);
	else if (status != -1) /* req.arp_flags & ATF_COM == 0 */
		err = EAGAIN;
	else /* status == -1 */
		err = errno;

	return err;
}

static int usnic_arp_lookup_index(int if_index, uint32_t ipaddr, int sockfd, uint8_t *macaddr)
{
	char			ifname[IF_NAMESIZE];

	if (if_indextoname((unsigned int)if_index, ifname) == NULL) {
		usnic_perr("if_indextoname failed. ifindex: %d", if_index);
		return errno;
	}

	return usnic_arp_lookup(ifname, ipaddr, sockfd, macaddr);
}

int usnic_arp_request(uint32_t ipaddr, int sockfd)
{
	struct sockaddr_in sin;
	int err = 0;

	memset(&sin, 0, sizeof(sin));
	sin.sin_addr.s_addr = ipaddr;
	sin.sin_port = htons(9);	/* Send to Discard Protocol */
	err = sendto(sockfd, NULL, 0, 0, (struct sockaddr *)&sin, sizeof(sin));
	if (err == -1) {
		char buf[INET_ADDRSTRLEN];
		inet_ntop(AF_INET, &ipaddr, buf, sizeof(buf));
		usnic_perr("Arp triggering socket sendto() failed. ip: %s",
				buf);
	}
	else
		err = 0;

	return err;
}

static
int usnic_resolve_arp(int if_index, uint32_t ipaddr, uint8_t *macaddr)
{
	int 	sockfd;
	int	err;
	char 	buf[INET_ADDRSTRLEN];

	inet_ntop(AF_INET, &ipaddr, buf, sizeof(buf));
	sockfd = socket(AF_INET, SOCK_DGRAM, 0);
	if (sockfd == -1) {
		usnic_perr(
			"socket() failed when creating socket for arp resolution, ip: %s",
			buf);
		return ENXIO;
	}

	err = usnic_arp_lookup_index(if_index, ipaddr, sockfd, macaddr);
	if (err == EAGAIN || err == ENXIO) {
		/* entry is FAILED or INCOMPLETE or does not exist, send a dummy packet */
		err = usnic_arp_request(ipaddr, sockfd);
		if (err) /* sendto failure, abort */
			err = ENXIO;
		else
			err = EAGAIN;
	}

	close(sockfd);
	return err;
}

int usnic_resolve_dst(int if_index, uint32_t src_ip_addr,
			uint32_t dst_ip_addr, uint8_t *macaddr)
{
	uint32_t	nh_ip_addr = 0;
	int		err;

	err = usnic_nl_rt_lookup(src_ip_addr, dst_ip_addr, if_index,
				&nh_ip_addr);
	if (err) {
		char ifname[IFNAMSIZ];
		char src_buf[INET_ADDRSTRLEN];
		char dst_buf[INET_ADDRSTRLEN];

		if_indextoname((unsigned int)if_index, ifname);
		inet_ntop(AF_INET, &src_ip_addr, src_buf, sizeof(src_buf));
		inet_ntop(AF_INET, &dst_ip_addr, dst_buf, sizeof(dst_buf));

		usnic_err(
			"ip route lookup for dst: %s on if: %d device: %s src ip: %s failed\n",
			dst_buf, if_index, ifname, src_buf);
		return EHOSTUNREACH;
	}

	if (nh_ip_addr) {
		char nh_buf[INET_ADDRSTRLEN];
		char src_buf[INET_ADDRSTRLEN];
		char dst_buf[INET_ADDRSTRLEN];

		inet_ntop(AF_INET, &nh_ip_addr, nh_buf, sizeof(nh_buf));
		inet_ntop(AF_INET, &src_ip_addr, src_buf, sizeof(src_buf));
		inet_ntop(AF_INET, &dst_ip_addr, dst_buf, sizeof(dst_buf));

		usnic_info("ip route for dest %s src %s is via %s\n",
				dst_buf, src_buf, nh_buf);
	} else {
		char src_buf[INET_ADDRSTRLEN];
		char dst_buf[INET_ADDRSTRLEN];

		inet_ntop(AF_INET, &src_ip_addr, src_buf, sizeof(src_buf));
		inet_ntop(AF_INET, &dst_ip_addr, dst_buf, sizeof(dst_buf));
		usnic_info("ip route for dest %s src %s is directly connected\n",
				dst_buf, src_buf);
	}

	if (nh_ip_addr)
		return usnic_resolve_arp(if_index, nh_ip_addr, macaddr);
	else
		return usnic_resolve_arp(if_index, dst_ip_addr, macaddr);
}
