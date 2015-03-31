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
#include <inttypes.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <ctype.h>
#include <sys/ipc.h>
#include <sys/mman.h>

#include "sock.h"
#include "sock_util.h"

#define SOCK_LOG_INFO(...) _SOCK_LOG_INFO(FI_LOG_AV, __VA_ARGS__)
#define SOCK_LOG_ERROR(...) _SOCK_LOG_ERROR(FI_LOG_AV, __VA_ARGS__)

fi_addr_t sock_av_lookup_key(struct sock_av *av, int key)
{
	int i;
	struct sock_av_addr *av_addr;
	struct sock_conn_map *cmap;

	cmap = av->cmap;
	for (i = 0; i < av->table_hdr->stored; i++) {
		av_addr = &av->table[i];
		if (sock_compare_addr(&cmap->table[key].addr, 
				      (struct sockaddr_in*)&av_addr->addr)) {
			SOCK_LOG_INFO("LOOKUP: (%d->%d)\n", key, i);
			return i;
		}
	}
	
	SOCK_LOG_INFO("Reverse-LOOKUP failed: %d, %s:%d\n", key,
		       inet_ntoa(cmap->table[key].addr.sin_addr),
		       ntohs(cmap->table[key].addr.sin_port));
	return FI_ADDR_NOTAVAIL;
}


int sock_av_compare_addr(struct sock_av *av, 
			 fi_addr_t addr1, fi_addr_t addr2)
{
	int index1, index2;
	struct sock_av_addr *av_addr1, *av_addr2;

	index1 = ((uint64_t)addr1 & av->mask);
	index2 = ((uint64_t)addr2 & av->mask);

	if (index1 >= av->table_hdr->stored || index1 < 0 ||
	    index2 >= av->table_hdr->stored || index2 < 0) {
		SOCK_LOG_ERROR("requested rank is larger than av table\n");
		return -1;
	}
	
	av_addr1 = idm_lookup(&av->addr_idm, index1);
	av_addr2 = idm_lookup(&av->addr_idm, index2);

	return memcmp(&av_addr1->addr, &av_addr2->addr, 
		      sizeof(struct sockaddr_in));
}

struct sock_conn *sock_av_lookup_addr(struct sock_ep *ep,
				      struct sock_av *av, 
				      fi_addr_t addr)
{
	int idx;
	int index = ((uint64_t)addr & av->mask);
	struct sock_av_addr *av_addr;

	if (index >= av->table_hdr->stored || index < 0) {
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
	idx = av_addr - &av->table[0];
	if (!av->key[idx]) {
		av->key[idx] = sock_conn_map_match_or_connect(
			ep, av->domain, av->cmap, 
			(struct sockaddr_in*)&av_addr->addr);
		if (!av->key[idx]) {
			SOCK_LOG_ERROR("failed to match or connect to addr %"
					PRIu64 "\n", addr);
			errno = EINVAL;
			return NULL;
		}
	}
	return sock_conn_map_lookup_key(av->cmap, av->key[idx]);
}

static inline void sock_av_report_success(struct sock_av *av, void *context,
					  int num_done, uint64_t flags)
{
	struct fi_eq_entry eq_entry;
	
	if (!av->eq) 
		return;

	eq_entry.fid = &av->av_fid.fid;
	eq_entry.context = context;
	eq_entry.data = num_done;
	sock_eq_report_event(av->eq, FI_AV_COMPLETE, 
			     &eq_entry, sizeof(eq_entry), flags);
}

static inline void sock_av_report_error(struct sock_av *av, 
					void *context, int index)
{
	if (!av->eq) 
		return;
	
	sock_eq_report_error(av->eq, &av->av_fid.fid,
			     context, index, FI_EINVAL, -FI_EINVAL, NULL, 0);
}

static int sock_av_is_valid_address(struct sockaddr_in *addr)
{
	return addr->sin_family == AF_INET ? 1 : 0;
}

static int sock_check_table_in(struct sock_av *_av, struct sockaddr_in *addr,
			       fi_addr_t *fi_addr, int count, uint64_t flags, 
			       void *context, int index)
{
	int i, j, ret = 0;
	char sa_ip[INET_ADDRSTRLEN];
	struct sock_av_addr *av_addr;
	size_t new_count, table_sz;

	if ((_av->attr.flags & FI_EVENT) && !_av->eq)
		return -FI_ENOEQ;
	
	if (_av->attr.flags & FI_READ) {
		for (i = 0; i < count; i++) {
			for (j = 0; j < _av->table_hdr->stored; j++) {

				if (!sock_av_is_valid_address(&addr[i])) {
					if (fi_addr)
						fi_addr[i] = FI_ADDR_NOTAVAIL;
					sock_av_report_error(_av, context, i);
					continue;
				}

				av_addr = &_av->table[j];

				if (memcmp(&av_addr->addr, &addr[i], 
					   sizeof(struct sockaddr_in)) == 0) {
					SOCK_LOG_INFO("Found addr in shared av\n");
					if (idm_set(&_av->addr_idm, _av->key[j], av_addr) < 0) {
						if (fi_addr)
							fi_addr[i] = FI_ADDR_NOTAVAIL;
						sock_av_report_error(_av, context, i);
						continue;
					}
					
					if (fi_addr)
						fi_addr[i] = (fi_addr_t)j;
					
					ret++;
				}
			}
		}
		sock_av_report_success(_av, context, ret, flags);
		return ret;
	}

	for (i = 0, ret = 0; i < count; i++) {

		if (_av->table_hdr->stored == _av->table_hdr->size) {
			if (_av->table_hdr->req_sz) {
				SOCK_LOG_ERROR("Cannot insert to AV table\n");
				return -FI_EINVAL;
			} else{
				new_count = _av->table_hdr->size * 2;
				_av->key = realloc(_av->key, 
						   sizeof(uint16_t) * new_count);
				if (!_av->key)
					return -FI_ENOMEM;
				
				table_sz = sizeof(struct sock_av_table_hdr) +
					new_count * sizeof(struct sock_av_addr);

				_av->table_hdr = realloc(_av->table_hdr, table_sz);
				if (!_av->table_hdr)
					return -FI_ENOMEM;
				_av->table_hdr->size = new_count;
				_av->table = (struct sock_av_addr*)((char*)_av->table_hdr + 
								    sizeof(struct sock_av_table_hdr));
			}
		}

		if (!sock_av_is_valid_address(&addr[i])) {
			if (fi_addr)
				fi_addr[i] = FI_ADDR_NOTAVAIL;
			sock_av_report_error(_av, context, i);
			continue;
		}

		av_addr = &_av->table[_av->table_hdr->stored];		
		memcpy(sa_ip, inet_ntoa((&addr[i])->sin_addr), INET_ADDRSTRLEN);
		SOCK_LOG_INFO("AV-INSERT:src_addr: family: %d, IP is %s, port: %d\n",
			      ((struct sockaddr_in*)&addr[i])->sin_family, sa_ip,
			      ntohs(((struct sockaddr_in*)&addr[i])->sin_port));
		
		memcpy(&av_addr->addr, &addr[i], sizeof(struct sockaddr_in));
		if (idm_set(&_av->addr_idm, _av->table_hdr->stored, av_addr) < 0) {
			if (fi_addr)
				fi_addr[i] = FI_ADDR_NOTAVAIL;
			sock_av_report_error(_av, context, i);
			continue;
		}
		
		if (fi_addr)
			fi_addr[i] = (fi_addr_t)_av->table_hdr->stored;

		av_addr->valid = 1;
		_av->table_hdr->stored++;
		ret++;
	}
	sock_av_report_success(_av, context, ret, flags);
	return ret;
}

static int sock_av_insert(struct fid_av *av, const void *addr, size_t count,
			  fi_addr_t *fi_addr, uint64_t flags, void *context)
{
	struct sock_av *_av;
	_av = container_of(av, struct sock_av, av_fid);
	return sock_check_table_in(_av, (struct sockaddr_in *)addr, 
				   fi_addr, count, flags, context, 0);
}

static int sock_av_lookup(struct fid_av *av, fi_addr_t fi_addr, void *addr,
			  size_t *addrlen)
{
	int index;
	struct sock_av *_av;
	struct sock_av_addr *av_addr;

	_av = container_of(av, struct sock_av, av_fid);
	index = ((uint64_t)fi_addr & _av->mask);
	if (index >= _av->table_hdr->stored || index < 0) {
		SOCK_LOG_ERROR("requested address not inserted\n");
		return -EINVAL;
	}

	av_addr = idm_lookup(&_av->addr_idm, index);
        memcpy(addr, &av_addr->addr, MIN(*addrlen, _av->addrlen));
	*addrlen = _av->addrlen;
	return 0;
}

static int _sock_av_insertsvc(struct fid_av *av, const char *node,
			      const char *service, fi_addr_t *fi_addr,
			      uint64_t flags, void *context, int index)
{
	int ret;
	struct addrinfo sock_hints;
	struct addrinfo *result = NULL;
	struct sock_av *_av;
	
	if (!service) {
		SOCK_LOG_ERROR("Port not provided\n");
		return -FI_EINVAL;
	}
	
	_av = container_of(av, struct sock_av, av_fid);
	memset(&sock_hints, 0, sizeof(struct addrinfo));
	sock_hints.ai_family = AF_INET;
	sock_hints.ai_socktype = SOCK_STREAM;
	
	ret = getaddrinfo(node, service, &sock_hints, &result);
	if (ret) {
		if (_av->eq) {
			sock_av_report_error(_av, context, 0);
			sock_av_report_success(_av, context, 0, flags);
		}
		return -ret;
	}
	
	ret = sock_check_table_in(_av, (struct sockaddr_in*)result->ai_addr,
				  fi_addr, 1, flags, context, index);
	freeaddrinfo(result); 
	return ret;
}

static int sock_av_insertsvc(struct fid_av *av, const char *node,
		   const char *service, fi_addr_t *fi_addr,
		   uint64_t flags, void *context)
{
	return _sock_av_insertsvc(av, node, service, fi_addr, flags, context, 0);
}

static int sock_av_insertsym(struct fid_av *av, const char *node, size_t nodecnt,
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

			if (_sock_av_insertsvc(av, node, service, fi_addr, flags, 
					       context, i * nodecnt + j) == 1)
				ret++;
		}
	}
	return ret;
}


static int sock_av_remove(struct fid_av *av, fi_addr_t *fi_addr, size_t count,
			  uint64_t flags)
{
	int i;
	struct sock_av *_av;
	struct sock_av_addr *av_addr;
	_av = container_of(av, struct sock_av, av_fid);

	for (i = 0; i < count; i++) {
		av_addr = &_av->table[fi_addr[i]];
		av_addr->valid = 0;
	}
	return 0;
}

static const char * sock_av_straddr(struct fid_av *av, const void *addr,
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
	struct sock_av *av;
	struct sock_eq *eq;

	if (bfid->fclass != FI_CLASS_EQ)
		return -FI_EINVAL;
	
	av = container_of(fid, struct sock_av, av_fid.fid);
	eq = container_of(bfid, struct sock_eq, eq.fid);
	av->eq = eq;
	return 0;
}

static int sock_av_close(struct fid *fid)
{
	struct sock_av *av;
	int i;

	av = container_of(fid, struct sock_av, av_fid.fid);
	if (atomic_get(&av->ref))
		return -FI_EBUSY;

	for (i=0; i<av->table_hdr->stored; i++) {
		if(idm_lookup(&av->addr_idm, i))
			idm_clear(&av->addr_idm , i);
	}

	if (!av->name) 
		free(av->table_hdr);
	else {
		shm_unlink(av->name);
		free(av->name);
		munmap(av->table_hdr, sizeof(struct sock_av_table_hdr) +
		       av->attr.count * sizeof(struct sock_av_addr));
		close(av->shared_fd);
	}

	atomic_dec(&av->domain->ref);
	free(av->key);
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
	.remove = sock_av_remove,
	.lookup = sock_av_lookup,
	.straddr = sock_av_straddr
};

static struct fi_ops_av sock_at_ops = {
	.size = sizeof(struct fi_ops_av),
	.insert = sock_av_insert,
	.insertsvc = sock_av_insertsvc,
	.insertsym = sock_av_insertsym,
	.remove = sock_av_remove,
	.lookup = sock_av_lookup,
	.straddr = sock_av_straddr
};

static int sock_verify_av_attr(struct fi_av_attr *attr)
{
	switch (attr->type) {
	case FI_AV_MAP:
	case FI_AV_TABLE:
		break;
	default:
		return -FI_EINVAL;
	}
	
	if (attr->flags & FI_READ && !attr->name)
		return -FI_EINVAL;

	if (attr->rx_ctx_bits > SOCK_EP_MAX_CTX_BITS) {
		SOCK_LOG_ERROR("Invalid rx_ctx_bits\n");
		return -FI_EINVAL;
	}
	return 0;
}

int sock_av_open(struct fid_domain *domain, struct fi_av_attr *attr,
		 struct fid_av **av, void *context)
{
	int ret = 0;
	struct sock_domain *dom;
	struct sock_av *_av;
	size_t table_sz, i;
	uint64_t flags = O_RDWR;
	
	if (!attr || sock_verify_av_attr(attr))
		return -FI_EINVAL;
	
	dom = container_of(domain, struct sock_domain, dom_fid);
	_av = calloc(1, sizeof(*_av));
	if (!_av)
		return -FI_ENOMEM;

	_av->attr = *attr;
	_av->attr.count = (attr->count) ? attr->count : SOCK_AV_DEF_SZ;

	_av->key = calloc(_av->attr.count, sizeof(uint16_t));
	if (!_av->key) {
		ret = -FI_ENOMEM;
		goto err;
	}

	table_sz = sizeof(struct sock_av_table_hdr) +
		_av->attr.count * sizeof(struct sock_av_addr);
	
	if (attr->name) {
		_av->name = calloc(1, FI_NAME_MAX);
		if(!_av->name) {
			ret = -FI_ENOMEM;
			goto err;
		}
		strcpy(_av->name, attr->name);
		if (!(attr->flags & FI_READ))
			flags |= O_CREAT;
		
		for (i = 0; i < strlen(_av->name); i ++)
			if (_av->name[i] == ' ')
				_av->name[i] = '_';
		
		SOCK_LOG_INFO("Creating shm segment :%s (size: %lu)\n",
			      _av->name, table_sz);
		
		_av->shared_fd = shm_open(_av->name, flags, S_IRUSR | S_IWUSR);
		if (_av->shared_fd < 0) {
			SOCK_LOG_ERROR("shm_open failed\n");
			ret = -FI_EINVAL;
			goto err;
		}
		
		if (ftruncate(_av->shared_fd, table_sz) == -1) {
			SOCK_LOG_ERROR("ftruncate failed\n");
			shm_unlink(_av->name);
			ret = -FI_EINVAL;
			goto err;
		}
		
		_av->table_hdr = mmap(NULL, table_sz, PROT_READ | PROT_WRITE, 
				      MAP_SHARED, _av->shared_fd, 0);
		if (attr->flags & FI_READ) {
			if (_av->table_hdr->size != _av->attr.count) {
				ret = -FI_EINVAL;
				goto err;
			}
		} else {
			_av->table_hdr->size = _av->attr.count;
			_av->table_hdr->stored = 0;
		}

		if (_av->table_hdr == MAP_FAILED) {
			SOCK_LOG_ERROR("mmap failed\n");
			shm_unlink(_av->name);
			ret = -FI_EINVAL;
			goto err;
		}
	} else {
		_av->table_hdr = calloc(1, table_sz);
		if (!_av->table_hdr) {
			ret = -FI_ENOMEM;
			goto err;
		}
		_av->table_hdr->size = _av->attr.count;
		_av->table_hdr->req_sz = attr->count;
	}

	_av->table = (struct sock_av_addr*)((char*)_av->table_hdr + 
					    sizeof(struct sock_av_table_hdr));
	_av->av_fid.fid.fclass = FI_CLASS_AV;
	_av->av_fid.fid.context = context;
	_av->av_fid.fid.ops = &sock_av_fi_ops;

	switch (attr->type) {
	case FI_AV_MAP:
		_av->av_fid.ops = &sock_am_ops;
		break;
	case FI_AV_TABLE:
		_av->av_fid.ops = &sock_at_ops;
		break;
	default:
		ret = -FI_EINVAL;
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
		ret = -FI_EINVAL;
		goto err;
	}
	_av->rx_ctx_bits = attr->rx_ctx_bits;
	_av->mask = attr->rx_ctx_bits ? 
		((uint64_t)1<<(64 - attr->rx_ctx_bits + 1))-1 : ~0;
	*av = &_av->av_fid;
	return 0;
err:
	free(_av);
	return ret;
}
