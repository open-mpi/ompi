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
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <dirent.h>
#include <pthread.h>
#include <errno.h>
#include <sys/stat.h>
#include <arpa/inet.h>

#include "usnic_ip_utils.h"
#include "libnl_utils.h"

#include "usnic_direct.h"
#include "usd.h"
#include "usd_queue.h"
#include "usd_time.h"
#include "usd_dest.h"
#include "usd_socket.h"

extern TAILQ_HEAD(, usd_device) usd_device_list;

static struct usd_dest_params usd_dest_params = {
    .dp_arp_timeout = 1000,
    .dp_max_arps = 10
};

int
usd_get_dest_distance(
    struct usd_device *dev,
    uint32_t daddr_be,
    int *metric_o)
{
    uint32_t nh_ip_addr;
    int ret;

    ret = usnic_nl_rt_lookup(dev->ud_attrs.uda_ipaddr_be, daddr_be,
            dev->ud_attrs.uda_ifindex, &nh_ip_addr);
    if (ret != 0) {
        *metric_o = -1;
        ret = 0;
    } else if (nh_ip_addr == 0) {
        *metric_o = 0;
    } else {
        *metric_o = 1;
    }

    return ret;
}

static void
usd_dest_set_complete(
    struct usd_device *dev,
    struct usd_dest_req *req)
{
    req->udr_complete = 1;
    if (req->udr_status != 0 && req->udr_dest != NULL) {
        free(req->udr_dest);
        req->udr_dest = NULL;
    }
    TAILQ_REMOVE(&dev->ud_pending_reqs, req, udr_link);
    TAILQ_INSERT_TAIL(&dev->ud_completed_reqs, req, udr_link);
}

static int
usd_dest_trigger_arp(
    struct usd_device *dev,
    struct usd_dest_req *req)
{
    int ret;

    usd_get_time(&req->udr_last_arp);
    req->udr_arps_sent++;

    ret =
        usnic_arp_request(req->udr_daddr_be, dev->ud_arp_sockfd);
    return ret;
}

static int
usd_check_dest_resolved(
    struct usd_device *dev,
    struct usd_dest_req *req)
{
    struct ether_header *eth;
    int ret;

    eth = &req->udr_dest->ds_dest.ds_udp.u_hdr.uh_eth;
    ret = usnic_arp_lookup(dev->ud_attrs.uda_ifname,
                           req->udr_daddr_be, dev->ud_arp_sockfd,
                           &eth->ether_dhost[0]);

    if (ret == EAGAIN)
        return -EAGAIN;

    /* for better or worse, resolution is complete */
    req->udr_status = -ret;
    return 0;
}

/*
 * Loop through the ordered pending create_dest request queue.
 * If an entry is complete, move it to the completed queue.
 * If the retry timeout for an entry has arrived, re-trigger the ARP
 */
static void
usd_dest_progress_dev(
    struct usd_device *dev)
{
    struct usd_dest_req *req;
    struct usd_dest_req *tmpreq;
    usd_time_t now;
    int delta;
    int ret;

    usd_get_time(&now);

    TAILQ_FOREACH_SAFE(req, tmpreq, &dev->ud_pending_reqs, udr_link) {

        /* resolution complete? */
        ret = usd_check_dest_resolved(dev, req);
        if (ret == 0) {
            usd_dest_set_complete(dev, req);
            continue;
        }


        /* time for next ARP trigger? */
        delta = usd_time_diff(req->udr_last_arp, now);
        if (delta > (int) usd_dest_params.dp_arp_timeout) {
            if (req->udr_arps_sent >= usd_dest_params.dp_max_arps) {
                req->udr_status = -EHOSTUNREACH;
                usd_dest_set_complete(dev, req);
                continue;
            }

            ret = usd_dest_trigger_arp(dev, req);
            if (ret != 0) {
                req->udr_status = ret;
                usd_dest_set_complete(dev, req);
            }
        }
    }
}

static void
usd_dest_progress()
{
    struct usd_device *dev;

    TAILQ_FOREACH(dev, &usd_device_list, ud_link) {
        usd_dest_progress_dev(dev);
    }
}

/*
 * Fill in all of a header except the dest MAC and the UDP ports
 * specified remote host
 */
void
usd_fill_udp_dest(
    struct usd_dest *dest,
    struct usd_device_attrs *dap,
    uint32_t daddr_be,
    uint16_t dport_be)
{
    struct ether_header *eth;
    struct iphdr *ip;
    struct udphdr *udp;

    eth = &dest->ds_dest.ds_udp.u_hdr.uh_eth;
    memcpy(eth->ether_shost, dap->uda_mac_addr, ETH_ALEN);
    eth->ether_type = htons(0x0800);

    ip = &dest->ds_dest.ds_udp.u_hdr.uh_ip;
    ip->ihl = 5;                       /* no options */
    ip->version = 4;
    ip->tos = 0;
    ip->frag_off = 0;
    ip->ttl = 8;
    ip->protocol = IPPROTO_UDP;
    ip->saddr = dap->uda_ipaddr_be;
    ip->daddr = daddr_be;

    udp = &dest->ds_dest.ds_udp.u_hdr.uh_udp;
    udp->dest = dport_be;
}

static int
usd_create_udp_dest_start(
    struct usd_device *dev,
    uint32_t daddr_be,
    uint16_t dport_be,
    struct usd_dest_req **req_o)
{
    struct usd_dest_req *req;
    struct usd_dest *dest;
    uint32_t first_hop_daddr_be;
    int ret;

    req = calloc(sizeof(*req), 1);
    dest = calloc(sizeof(*dest), 1);
    if (req == NULL || dest == NULL) {
        ret = -errno;
        goto fail;
    }

    ret = usnic_nl_rt_lookup(dev->ud_attrs.uda_ipaddr_be,
                             daddr_be, dev->ud_attrs.uda_ifindex,
                             &first_hop_daddr_be);
    if (ret != 0) {
        /* EHOSTUNREACH is non-fatal, but we are done with resolution */
        if (ret == EHOSTUNREACH) {
            req->udr_status = -EHOSTUNREACH;
            free(dest);
            goto complete;
        } else {
            ret = -ret;
        }
        goto fail;
    }
    if (first_hop_daddr_be == 0)
        first_hop_daddr_be = daddr_be;

    /* Fill in dest as much as we can */
    usd_fill_udp_dest(dest, &dev->ud_attrs, daddr_be, dport_be);

    /* initiate request and add to tail of pending list */
    req->udr_daddr_be = first_hop_daddr_be;
    req->udr_dest = dest;

    ret = usd_dest_trigger_arp(dev, req);
    if (ret != 0)
        goto fail;

complete:
    TAILQ_INSERT_TAIL(&dev->ud_pending_reqs, req, udr_link);
    if (req->udr_status != 0) {
        usd_dest_set_complete(dev, req);
    }
    *req_o = req;

    return 0;

  fail:
    if (req != NULL)
        free(req);
    if (dest != NULL)
        free(dest);
    return ret;
}


/*
 * synchronously create a UDP destination by initiating the
 * resolution, then waiting for it to complete
 */
static int
usd_create_udp_dest(
    struct usd_device *dev,
    uint32_t daddr_be,
    uint16_t dport_be,
    struct usd_dest **dest_o)
{
    struct usd_dest_req *req;
    int ret;

    ret = usd_create_udp_dest_start(dev, daddr_be, dport_be, &req);
    if (ret != 0)
        return ret;

    /* loop until request completes or times out */
    while (req->udr_complete == 0) {
        usd_dest_progress();
    }

    ret = req->udr_status;
    if (ret == 0)
        *dest_o = req->udr_dest;

    TAILQ_REMOVE(&dev->ud_completed_reqs, req, udr_link);
    free(req);
    return ret;
}

/*
 * Build and save a IP header appropriate for sending to the
 * specified remote host
 */
int
usd_create_ip_dest(
    struct usd_device *dev,
    uint32_t daddr_be,
    struct usd_dest **dest_o)
{
    int ret;

    ret = usd_create_udp_dest(dev, daddr_be, 0, dest_o);
    return ret;
}

void
usd_dest_set_udp_ports(
    struct usd_dest *dest,
    struct usd_qp *src_uqp,
    uint16_t dest_port_be)
{
    struct usd_qp_impl *qp;
    struct udphdr *udp;

    qp = to_qpi(src_uqp);
    udp = &dest->ds_dest.ds_udp.u_hdr.uh_udp;
    udp->source = qp->uq_attrs.uqa_local_addr.ul_addr.ul_udp.u_addr.sin_port;
    udp->dest = dest_port_be;

}

/*
 * Synchronously creates a destination
 */
int
usd_create_dest(
    struct usd_device *dev,
    uint32_t daddr_be,
    uint16_t dport_be,
    struct usd_dest **dest_o)
{
    int ret;

    ret = usd_create_udp_dest(dev, daddr_be, dport_be, dest_o);

    return ret;
}

int
usd_destroy_dest(
    struct usd_dest *dest)
{
    if (dest != NULL) {
        free(dest);
    }
    return 0;
}

/*
 * Get address resolution settings
 */
int
usd_get_dest_params(
    struct usd_dest_params *params)
{
    if (params == NULL)
        return -EINVAL;

    *params = usd_dest_params;
    return 0;
}

/*
 * Set address resolution settings
 * Settings may not be changed while any resolution requests are in progress.
 */
int
usd_set_dest_params(
    struct usd_dest_params *params)
{
    if (params == NULL)
        return -EINVAL;

    /* blindly set parameters, allowing user to shoot self if desired */
    usd_dest_params.dp_arp_timeout = params->dp_arp_timeout;
    usd_dest_params.dp_max_arps = params->dp_max_arps;

    return 0;
}

/*
 * Start destination creation
 * Resolution progress is performed in usd_create_dest_query() and
 * usd_create_dest_poll()
 */
int
usd_create_dest_start(
    struct usd_device *dev,
    uint32_t daddr_be,
    uint16_t dport_be,
    void *context)
{
    struct usd_dest_req *req;
    int ret;

    req = NULL;
    ret = usd_create_udp_dest_start(dev, daddr_be, dport_be, &req);

    if (ret == 0) {
        req->udr_context = context;
    }

    return ret;
}

/*
 * Return first completed destinatin request
 */
int
usd_create_dest_poll(
    struct usd_device *dev,
    void **context_o,
    int *status,
    struct usd_dest **dest_o)
{
    struct usd_dest_req *req;

    usd_dest_progress();

    if (!TAILQ_EMPTY(&dev->ud_completed_reqs)) {
        req = TAILQ_FIRST(&dev->ud_completed_reqs);
        TAILQ_REMOVE(&dev->ud_completed_reqs, req, udr_link);
        *context_o = req->udr_context;
        *status = req->udr_status;
        if (*status == 0)
            *dest_o = req->udr_dest;
        free(req);
        return 0;

    } else {
        return -EAGAIN;
    }
}

/*
 * Check completion of a particular request
 */
int
usd_create_dest_query(
    struct usd_device *dev,
    void *context,
    int *status,
    struct usd_dest **dest_o)
{
    struct usd_dest_req *req;

    usd_dest_progress();

    TAILQ_FOREACH(req, &dev->ud_completed_reqs, udr_link) {
        if (req->udr_context == context) {
            TAILQ_REMOVE(&dev->ud_completed_reqs, req, udr_link);
            *status = req->udr_status;
            if (*status == 0)
                *dest_o = req->udr_dest;
            free(req);
            return 0;
        }
    }

    return -EAGAIN;
}

/*
 * Cancel a destination creation in progress
 * Look through both the pending and completed queues, simply
 * squash the record if we find it.
 */
int
usd_create_dest_cancel(
    struct usd_device *dev,
    void *context)
{
    struct usd_dest_req *req;

    TAILQ_FOREACH(req, &dev->ud_pending_reqs, udr_link) {
        if (req->udr_context == context) {
            TAILQ_REMOVE(&dev->ud_pending_reqs, req, udr_link);
            goto found;
        }
    }

    TAILQ_FOREACH(req, &dev->ud_completed_reqs, udr_link) {
        if (req->udr_context == context) {
            TAILQ_REMOVE(&dev->ud_completed_reqs, req, udr_link);
            goto found;
        }
    }

    return -EINVAL;

  found:
    free(req->udr_dest);
    free(req);
    return 0;
}

/*
 * Create a destination given a MAC address
 */
int
usd_create_dest_with_mac(
    struct usd_device *dev,
    uint32_t daddr_be,
    uint16_t dport_be,
    uint8_t * dmac,
    struct usd_dest **dest_o)
{
    struct ether_header *eth;
    struct usd_dest *dest;

    dest = calloc(sizeof(*dest), 1);
    if (dest == NULL)
        return -errno;

    /* Fill in dest as much as we can */
    usd_fill_udp_dest(dest, &dev->ud_attrs, daddr_be, dport_be);

    /* copy in MAC from caller */
    eth = &dest->ds_dest.ds_udp.u_hdr.uh_eth;
    memcpy(&eth->ether_dhost[0], dmac, ETH_ALEN);

    *dest_o = dest;
    return 0;
}

/*
 * Expand a destination
 */
int
usd_expand_dest(
    struct usd_dest *dest,
    uint32_t *ip_be_o,
    uint16_t *port_be_o)
{
    if (ip_be_o != NULL) {
        *ip_be_o = dest->ds_dest.ds_udp.u_hdr.uh_ip.daddr;
    }
    if (port_be_o != NULL) {
        *port_be_o = dest->ds_dest.ds_udp.u_hdr.uh_udp.dest;
    }

    return 0;
}
