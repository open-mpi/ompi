/*
 * Copyright (c) 2014 Intel Corporation, Inc.  All rights reserved.
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
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
 * BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
 * ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#if HAVE_CONFIG_H
#  include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdlib.h>
#include <string.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <limits.h>

#include "prov.h"

#include "sock.h"
#include "sock_util.h"

#define SOCK_LOG_INFO(...) _SOCK_LOG_INFO(FI_LOG_FABRIC, __VA_ARGS__)
#define SOCK_LOG_ERROR(...) _SOCK_LOG_ERROR(FI_LOG_FABRIC, __VA_ARGS__)

const char sock_fab_name[] = "IP";
const char sock_dom_name[] = "sockets";
const char sock_prov_name[] = "sockets";

useconds_t sock_progress_thread_wait = 0;

const struct fi_fabric_attr sock_fabric_attr = {
	.fabric = NULL,
	.name = NULL,
	.prov_name = NULL,
	.prov_version = FI_VERSION(SOCK_MAJOR_VERSION, SOCK_MINOR_VERSION),
};

int sock_verify_fabric_attr(struct fi_fabric_attr *attr)
{
	if (!attr)
		return 0;

	if (attr->name &&
	    strcmp(attr->name, sock_fab_name))
		return -FI_ENODATA;

	if (attr->prov_version) {
		if (attr->prov_version != 
		   FI_VERSION(SOCK_MAJOR_VERSION, SOCK_MINOR_VERSION))
			return -FI_ENODATA;
	}

	return 0;
}

int sock_verify_info(struct fi_info *hints)
{
	uint64_t caps;
	enum fi_ep_type ep_type;
	int ret;

	if (!hints)
		return 0;

	ep_type = hints->ep_attr ? hints->ep_attr->type : FI_EP_UNSPEC;
	switch (ep_type) {
	case FI_EP_UNSPEC:
	case FI_EP_MSG:
		caps = SOCK_EP_MSG_CAP;
		ret = sock_msg_verify_ep_attr(hints->ep_attr,
					      hints->tx_attr,
					      hints->rx_attr);
		break;
	case FI_EP_DGRAM:
		caps = SOCK_EP_DGRAM_CAP;
		ret = sock_dgram_verify_ep_attr(hints->ep_attr,
						hints->tx_attr,
						hints->rx_attr);
		break;
	case FI_EP_RDM:
		caps = SOCK_EP_RDM_CAP;
		ret = sock_rdm_verify_ep_attr(hints->ep_attr,
					      hints->tx_attr,
					      hints->rx_attr);
		break;
	default:
		ret = -FI_ENODATA;
	}
	if (ret)
		return ret;

	if ((caps | hints->caps) != caps) {
		SOCK_LOG_INFO("Unsupported capabilities\n");
		return -FI_ENODATA;
	}

	switch (hints->addr_format) {
	case FI_FORMAT_UNSPEC:
	case FI_SOCKADDR:
	case FI_SOCKADDR_IN:
		break;
	default:
		return -FI_ENODATA;
	}

	ret = sock_verify_domain_attr(hints->domain_attr);
	if (ret) 
		return ret;

	ret = sock_verify_fabric_attr(hints->fabric_attr);
	if (ret) 
		return ret;

	return 0;
}

static struct fi_ops_fabric sock_fab_ops = {
	.size = sizeof(struct fi_ops_fabric),
	.domain = sock_domain,
	.passive_ep = sock_msg_passive_ep,
	.eq_open = sock_eq_open,
	.wait_open = sock_wait_open,
};

static int sock_fabric_close(fid_t fid)
{
	struct sock_fabric *fab;
	fab = container_of(fid, struct sock_fabric, fab_fid);

	if (atomic_get(&fab->ref)) {
		return -FI_EBUSY;
	}

	fastlock_destroy(&fab->lock);
	free(fab);
	return 0;
}

static struct fi_ops sock_fab_fi_ops = {
	.size = sizeof(struct fi_ops),
	.close = sock_fabric_close,
	.bind = fi_no_bind,
	.control = fi_no_control,
	.ops_open = fi_no_ops_open,
};

static int sock_fabric(struct fi_fabric_attr *attr,
		       struct fid_fabric **fabric, void *context)
{
	struct sock_fabric *fab;

	if (strcmp(attr->name, sock_fab_name))
		return -FI_ENODATA;
	
	fab = calloc(1, sizeof(*fab));
	if (!fab)
		return -FI_ENOMEM;

	fastlock_init(&fab->lock);
	dlist_init(&fab->service_list);
	
	fab->fab_fid.fid.fclass = FI_CLASS_FABRIC;
	fab->fab_fid.fid.context = context;
	fab->fab_fid.fid.ops = &sock_fab_fi_ops;
	fab->fab_fid.ops = &sock_fab_ops;
	*fabric = &fab->fab_fid;
	atomic_init(&fab->ref, 0);
	return 0;
}

static struct sock_service_entry *sock_fabric_find_service(struct sock_fabric *fab, 
							   int service)
{
	struct dlist_entry *entry;
	struct sock_service_entry *service_entry;

	for (entry = fab->service_list.next; entry != &fab->service_list;
	     entry = entry->next) {
		service_entry = container_of(entry, 
					     struct sock_service_entry, entry);
		if (service_entry->service == service) {
			return service_entry;
		}
	}
	return NULL;
}


int sock_fabric_check_service(struct sock_fabric *fab, int service)
{
	struct sock_service_entry *entry;

	fastlock_acquire(&fab->lock);
	entry = sock_fabric_find_service(fab, service);
	fastlock_release(&fab->lock);
	return (entry == NULL) ? 1 : 0;
}

void sock_fabric_add_service(struct sock_fabric *fab, int service)
{
	struct sock_service_entry *entry;

	entry = calloc(1, sizeof *entry);
	if (!entry)
		return;

	entry->service = service;
	fastlock_acquire(&fab->lock);
	dlist_insert_tail(&entry->entry, &fab->service_list);
	fastlock_release(&fab->lock);
}

void sock_fabric_remove_service(struct sock_fabric *fab, int service)
{
	struct sock_service_entry *service_entry;
	fastlock_acquire(&fab->lock);
	service_entry = sock_fabric_find_service(fab, service);
	dlist_remove(&service_entry->entry);
	free(service_entry);
	fastlock_release(&fab->lock);
}

static int sock_get_src_addr(struct sockaddr_in *dest_addr,
			     struct sockaddr_in *src_addr)
{
	int sock, ret;
	socklen_t len;

	sock = socket(AF_INET, SOCK_DGRAM, 0);
	if (sock < 0)
		return -errno;

	len = sizeof(*dest_addr);
	ret = connect(sock, (struct sockaddr*)dest_addr, len);
	if (ret) {
		SOCK_LOG_INFO("Failed to connect udp socket\n");
		ret = -errno;
		goto out;
	}

	ret = getsockname(sock, (struct sockaddr *) src_addr, &len);
	if (ret) {
		SOCK_LOG_INFO("getsockname failed\n");
		ret = -errno;
	}
out:
	close(sock);
	return ret;
}

static int sock_ep_getinfo(const char *node, const char *service, uint64_t flags,
			   struct fi_info *hints, enum fi_ep_type ep_type,
			   struct fi_info **info)
{
	struct addrinfo ai, *rai = NULL;
	struct sockaddr_in *src_addr = NULL, *dest_addr = NULL;
	struct sockaddr_in sin;
	int ret;

	memset(&ai, 0, sizeof(ai));
	ai.ai_family = AF_INET;
	ai.ai_socktype = SOCK_STREAM;
	if (flags & FI_NUMERICHOST)
		ai.ai_flags |= AI_NUMERICHOST;

	if (flags & FI_SOURCE) {
		ai.ai_flags |= AI_PASSIVE;
		ret = getaddrinfo(node, service, &ai, &rai);
		if (ret) {
			SOCK_LOG_INFO("getaddrinfo failed!\n");
			return -FI_ENODATA;
		}
		src_addr = (struct sockaddr_in *) rai->ai_addr;

		if (hints && hints->dest_addr)
			dest_addr = hints->dest_addr;
	} else {
		if (node || service) {
			ret = getaddrinfo(node, service, &ai, &rai);
			if (ret) {
				SOCK_LOG_INFO("getaddrinfo failed!\n");
				return -FI_ENODATA;
			}
			dest_addr = (struct sockaddr_in *) rai->ai_addr;
		} else {
			dest_addr = hints->dest_addr;
		}

		if (hints && hints->src_addr)
			src_addr = hints->src_addr;
	}

	if (dest_addr && !src_addr) {
		ret = sock_get_src_addr(dest_addr, &sin);
		if (!ret)
			src_addr = &sin;
	}

	if (src_addr)
		SOCK_LOG_INFO("src_addr: %s\n", inet_ntoa(src_addr->sin_addr));
	if (dest_addr)
		SOCK_LOG_INFO("dest_addr: %s\n", inet_ntoa(dest_addr->sin_addr));

	switch (ep_type) {
	case FI_EP_MSG:
		ret = sock_msg_fi_info(src_addr, dest_addr, hints, info);
		break;
	case FI_EP_DGRAM:
		ret = sock_dgram_fi_info(src_addr, dest_addr, hints, info);
		break;
	case FI_EP_RDM:
		ret = sock_rdm_fi_info(src_addr, dest_addr, hints, info);
		break;
	default:
		ret = -FI_ENODATA;
		break;
	}

	if (rai)
		freeaddrinfo(rai);
	return ret;
}

static int sock_getinfo(uint32_t version, const char *node, const char *service,
			uint64_t flags, struct fi_info *hints, struct fi_info **info)
{
	struct fi_info *cur, *tail;
	enum fi_ep_type ep_type;
	char hostname[HOST_NAME_MAX];
	int ret;

	if (version != FI_VERSION(SOCK_MAJOR_VERSION, SOCK_MINOR_VERSION))
		return -FI_ENODATA;

	if (!(flags & FI_SOURCE) && hints && hints->src_addr &&
	    (hints->src_addrlen != sizeof(struct sockaddr_in)))
		return -FI_ENODATA;

	if (((!node && !service) || (flags & FI_SOURCE)) &&
	    hints && hints->dest_addr &&
	    (hints->dest_addrlen != sizeof(struct sockaddr_in)))
		return -FI_ENODATA;

	ret = sock_verify_info(hints);
	if (ret) 
		return ret;

	if (!node && !service && !hints) {
		flags |= FI_SOURCE;
		gethostname(hostname, sizeof hostname);
		node = hostname;
	}

	if (!node && !service && !(flags & FI_SOURCE)) {
		gethostname(hostname, sizeof hostname);
		node = hostname;
	}

	if (hints && hints->ep_attr) {
		switch (hints->ep_attr->type) {
		case FI_EP_RDM:
		case FI_EP_DGRAM:
		case FI_EP_MSG:
			return sock_ep_getinfo(node, service, flags, hints,
						hints->ep_attr->type, info);
		default:
			break;
		}
	}

	*info = tail = NULL;
	for (ep_type = FI_EP_MSG; ep_type <= FI_EP_RDM; ep_type++) {
		ret = sock_ep_getinfo(node, service, flags,
					hints, ep_type, &cur);
		if (ret) {
			if (ret == -FI_ENODATA)
				continue;
			goto err;
		}

		if (!*info)
			*info = cur;
		else
			tail->next = cur;
		for (tail = cur; tail->next; tail = tail->next)
			;
	}
	return 0;

err:
	fi_freeinfo(*info);
	*info = NULL;
	return ret;
}

static void fi_sockets_fini(void)
{
}

struct fi_provider sock_prov = {
	.name = sock_prov_name,
	.version = FI_VERSION(SOCK_MAJOR_VERSION, SOCK_MINOR_VERSION), 
	.fi_version = FI_VERSION(FI_MAJOR_VERSION, FI_MINOR_VERSION),
	.getinfo = sock_getinfo,
	.fabric = sock_fabric,
	.cleanup = fi_sockets_fini
};

SOCKETS_INI
{
	char *tmp;

	tmp = getenv("OFI_SOCK_PROGRESS_YIELD_TIME");
	if (tmp)
		sock_progress_thread_wait = atoi(tmp);

	return (&sock_prov);
}
