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

#include <arpa/inet.h>
#include <netinet/in.h>
#include <netinet/ip.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <ctype.h>

#include "sock.h"
#include "sock_util.h"

fi_addr_t sock_av_lookup_key(struct sock_av *av, int key)
{
	int i;
	struct sock_av_addr *av_addr;

	for (i = 0; i < IDX_MAX_INDEX; i++) {
		av_addr = idm_lookup(&av->addr_idm, i);
		if (!av_addr)
			continue;

		if (!av_addr->key) {
			av_addr->key = sock_conn_map_match_or_connect(
				av->cmap,
				(struct sockaddr_in*)&av_addr->addr, 1);
			if (!av_addr->key) {
				continue;
			}
		}

		if (av_addr->key == key + 1) {
			return i;
		}
	}

	SOCK_LOG_INFO("Reverse-lookup failed: %d\n", key);
	return FI_ADDR_NOTAVAIL;
}

struct sock_conn *sock_av_lookup_addr(struct sock_av *av, 
		fi_addr_t addr)
{
	int index = ((uint64_t)addr & av->mask);
	struct sock_av_addr *av_addr;

	if (index >= av->stored || index < 0) {
		SOCK_LOG_ERROR("requested rank is larger than av table\n");
		errno = EINVAL;
		return NULL;
	}

	if (!av->cmap) {
		SOCK_LOG_ERROR("EP with no AV bound\n");
		errno = EINVAL;
		return NULL;
	}

	av_addr = idm_lookup(&av->addr_idm, index);
	if (!av_addr->key) {
		av_addr->key = sock_conn_map_match_or_connect(av->cmap, 
				(struct sockaddr_in*)&av_addr->addr, 0);
		if (!av_addr->key) {
			SOCK_LOG_ERROR("failed to match or connect to addr %lu\n", addr);
			errno = EINVAL;
			return NULL;
		}
	}
	return sock_conn_map_lookup_key(av->cmap, av_addr->key);
}

static int sock_check_table_in(struct sock_av *_av, struct sockaddr_in *addr,
			       fi_addr_t *fi_addr, int count)
{
	int i, ret;
	struct sock_av_addr *av_addr;
	av_addr = calloc(count, sizeof(struct sock_av_addr));
	if (!av_addr)
		return -ENOMEM;
	
	for (i=0, ret = 0; i<count; i++) {
		memcpy(&av_addr[i].addr, &addr[i], sizeof(struct sockaddr_in));
		if (idm_set(&_av->addr_idm, _av->stored, &av_addr[i]) < 0) {
			if (fi_addr)
				fi_addr[i] = FI_ADDR_NOTAVAIL;
			continue;
		}

		if (fi_addr)
			fi_addr[i] = (fi_addr_t)_av->stored;
		
		_av->stored++;
		ret++;
	}
	return ret;
}

static int sock_av_insert(struct fid_av *av, const void *addr, size_t count,
			  fi_addr_t *fi_addr, uint64_t flags, void *context)
{
	struct sock_av *_av;

	_av = container_of(av, struct sock_av, av_fid);

	switch(((struct sockaddr *)addr)->sa_family) {
	case AF_INET:
		return sock_check_table_in(_av, (struct sockaddr_in *)addr, 
					   fi_addr, count);
	default:
		SOCK_LOG_ERROR("invalid address type inserted: only IPv4 supported\n");
		return -EINVAL;
	}
}

static int sock_at_remove(struct fid_av *av, fi_addr_t *fi_addr, size_t count,
			  uint64_t flags)
{
	return 0;
}

static int sock_at_lookup(struct fid_av *av, fi_addr_t fi_addr, void *addr,
			  size_t *addrlen)
{
	int index;
	struct sock_av *_av;
	struct sock_av_addr *av_addr;

	_av = container_of(av, struct sock_av, av_fid);
	index = ((uint64_t)fi_addr & _av->mask);
	if (index >= _av->stored || index < 0) {
		SOCK_LOG_ERROR("requested address not inserted\n");
		return -EINVAL;
	}

	av_addr = idm_lookup(&_av->addr_idm, index);
	addr = &av_addr->addr;
	*addrlen = _av->addrlen;
	return 0;
}

static const char * sock_at_straddr(struct fid_av *av, const void *addr,
				    char *buf, size_t *len)
{
	return NULL;
}

int sock_av_insertsvc(struct fid_av *av, const char *node,
		   const char *service, fi_addr_t *fi_addr,
		   uint64_t flags, void *context)
{
	int ret;
	struct addrinfo sock_hints;
	struct addrinfo *result = NULL;
	
	if (!service) {
		SOCK_LOG_ERROR("Port not provided\n");
		return -FI_EINVAL;
	}

	memset(&sock_hints, 0, sizeof(struct addrinfo));
	sock_hints.ai_family = AF_INET;
	sock_hints.ai_socktype = SOCK_STREAM;
	
	ret = getaddrinfo(node, service, &sock_hints, &result);
	if (ret)
		return -ret;

	ret = sock_av_insert(av, result->ai_addr, 1, fi_addr, flags, context);
	freeaddrinfo(result); 
	return ret;
}

int sock_av_insertsym(struct fid_av *av, const char *node, size_t nodecnt,
		      const char *service, size_t svccnt, fi_addr_t *fi_addr,
		      uint64_t flags, void *context)
{
	int ret = 0;
	int var_port, var_host;
	char base_host[FI_NAME_MAX] = {0};
	char tmp_host[FI_NAME_MAX] = {0};
	char tmp_port[FI_NAME_MAX] = {0};
	int hostlen, offset = 0, fmt, i, j;

	if (!node || !service) {
		SOCK_LOG_ERROR("Node/service not provided\n");
		return -FI_EINVAL;
	}
	
	hostlen = strlen(node);
	while(isdigit(*(node + hostlen - (offset+1))))
		offset++;
	
	if (*(node + hostlen - offset) == '.')
		fmt = 0;
	else 
		fmt = offset;

	strncpy(base_host, node, hostlen - (offset));
	var_port = atoi(service);
	var_host = atoi(node + hostlen - offset);
	
	for (i = 0; i < nodecnt; i++) {
		for (j = 0; j < svccnt; j++) {
			sprintf(tmp_host, "%s%0*d", base_host, fmt, var_host + i);
			sprintf(tmp_port, "%d", var_port + j);

			if (sock_av_insertsvc(av, tmp_host, tmp_port, 
					   &fi_addr[i * nodecnt + j],
					   flags, context) == 1)
				ret++;
		}
	}
	return ret;
}


static int sock_am_remove(struct fid_av *av, fi_addr_t *fi_addr, size_t count,
			  uint64_t flags)
{
	return 0;
}

static int sock_am_lookup(struct fid_av *av, fi_addr_t fi_addr, void *addr,
			  size_t *addrlen)
{
	sock_at_lookup(av, fi_addr, addr, addrlen);
	return 0;
}

static const char * sock_am_straddr(struct fid_av *av, const void *addr,
				    char *buf, size_t *len)
{
	const struct sockaddr_in *sin;
	char straddr[24];
	int size;

	sin = addr;
	size = snprintf(straddr, sizeof straddr, "%s:%d",
			inet_ntoa(sin->sin_addr), sin->sin_port);
	snprintf(buf, *len, "%s", straddr);
	*len = size + 1;
	return buf;
}

static int sock_av_bind(struct fid *fid, struct fid *bfid, uint64_t flags)
{
	return -FI_ENOSYS;
}

static int sock_av_close(struct fid *fid)
{
	struct sock_av *av;
	void *addr;
	int i;

	av = container_of(fid, struct sock_av, av_fid.fid);
	if (atomic_get(&av->ref))
		return -FI_EBUSY;

	for (i=0; i<av->stored; i++) {
		addr = idm_clear(&av->addr_idm , i);
		if (addr)
			free(addr);
	}

	atomic_dec(&av->domain->ref);
	free(av);
	return 0;
}

static struct fi_ops sock_av_fi_ops = {
	.size = sizeof(struct fi_ops),
	.close = sock_av_close,
	.bind = sock_av_bind,
	.control = fi_no_control,
	.ops_open = fi_no_ops_open,
};

static struct fi_ops_av sock_am_ops = {
	.size = sizeof(struct fi_ops_av),
	.insert = sock_av_insert,
	.insertsvc = sock_av_insertsvc,
	.insertsym = sock_av_insertsym,
	.remove = sock_am_remove,
	.lookup = sock_am_lookup,
	.straddr = sock_am_straddr
};

static struct fi_ops_av sock_at_ops = {
	.size = sizeof(struct fi_ops_av),
	.insert = sock_av_insert,
	.insertsvc = sock_av_insertsvc,
	.insertsym = sock_av_insertsym,
	.remove = sock_at_remove,
	.lookup = sock_at_lookup,
	.straddr = sock_at_straddr
};

//static struct fi_ops_av sock_av_ops = {
//	.size = sizeof(struct fi_ops_av),
//	.insert = sock_av_insert,
//	.remove = sock_av_remove,
//	.lookup = sock_av_lookup,
//	.straddr = sock_av_straddr
//};

#if 0
static int sock_open_am(struct sock_domain *dom, struct fi_av_attr *attr,
			struct sock_av **av, void *context)
{
	struct sock_av *_av;

	_av = calloc(1, sizeof(*_av));
	if (!_av)
		return -FI_ENOMEM;

	_av->av_fid.fid.fclass = FI_CLASS_AV;
	_av->av_fid.fid.context = context;
	_av->av_fid.fid.ops = &sock_av_fi_ops;
	_av->av_fid.ops = &sock_am_ops;

	*av = _av;
	return 0;
}
#endif

int sock_av_open(struct fid_domain *domain, struct fi_av_attr *attr,
		 struct fid_av **av, void *context)
{
	struct sock_domain *dom;
	struct sock_av *_av;

	if (attr->flags)
		return -FI_ENOSYS;

	if (attr->rx_ctx_bits > SOCK_EP_MAX_CTX_BITS) {
		SOCK_LOG_ERROR("Invalid rx_ctx_bits\n");
		return -EINVAL;
	}

	dom = container_of(domain, struct sock_domain, dom_fid);

	_av = calloc(1, sizeof(*_av));
	if (!_av)
		return -FI_ENOMEM;

	_av->av_fid.fid.fclass = FI_CLASS_AV;
	_av->av_fid.fid.context = context;
	_av->av_fid.fid.ops = &sock_av_fi_ops;

	switch (attr->type) {
	case FI_AV_MAP:
//		ret = sock_open_am(dom, attr, &_av, context);
		_av->av_fid.ops = &sock_am_ops;
		break;
	case FI_AV_TABLE:
		_av->av_fid.ops = &sock_at_ops;
		break;
	default:
		goto err;
	}

	atomic_init(&_av->ref, 0);
	atomic_inc(&dom->ref);
	_av->domain = dom;
	switch (dom->info.addr_format) {
	case FI_SOCKADDR_IN:
		_av->addrlen = sizeof(struct sockaddr_in);
		break;
	default:
		SOCK_LOG_ERROR("Invalid address format: only IPv4 supported\n");
		goto err;
	}
	_av->rx_ctx_bits = attr->rx_ctx_bits;
	_av->mask = ((uint64_t)1<<(64 - attr->rx_ctx_bits + 1))-1;
	_av->attr = *attr;
	*av = &_av->av_fid;
	return 0;
err:
	free(_av);
	return -EINVAL;
}
