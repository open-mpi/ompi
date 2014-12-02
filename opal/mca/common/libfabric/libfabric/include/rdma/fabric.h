/*
 * Copyright (c) 2013-2014 Intel Corporation. All rights reserved.
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

#ifndef _FABRIC_H_
#define _FABRIC_H_

#include <stdint.h>
#include <stddef.h>
#include <assert.h>
#include <sys/socket.h>

#ifdef __cplusplus
extern "C" {
#endif


#ifndef container_of
#define container_of(ptr, type, field) \
	((type *) ((char *)ptr - offsetof(type, field)))
#endif

enum {
	FI_MAJOR_VERSION	= 1,
	FI_MINOR_VERSION	= 0,
	FI_PATH_MAX		= 256,
	FI_NAME_MAX		= 64,
	FI_VERSION_MAX		= 64
};

#define FI_VERSION(major, minor) ((major << 16) | (minor))
#define FI_MAJOR(version)	(version >> 16)
#define FI_MINOR(version)	(version & 0xFFFF)
#define FI_VERSION_GE(v1, v2)   ((FI_MAJOR(v1) > FI_MAJOR(v2)) || \
				 (FI_MAJOR(v1) == FI_MAJOR(v2) && FI_MINOR(v1) == FI_MINOR(v2)) || \
				 (FI_MAJOR(v1) == FI_MAJOR(v2) && FI_MINOR(v1) > FI_MINOR(v2)))

uint32_t fi_version(void);

struct fid;
struct fid_fabric;
struct fid_domain;
struct fid_av;
struct fid_wait;
struct fid_poll;
struct fid_eq;
struct fid_cq;
struct fid_cntr;
struct fid_ep;
struct fid_pep;
struct fid_mr;

typedef struct fid *fid_t;

/*
 * Provider specific values are indicated by setting the high-order bit.
 */
#define FI_PROV_SPECIFIC	(1 << 31)

/* fi_info and operation flags - pass into endpoint ops calls.
 * A user may also set these on a endpoint by using fcntl, which has the
 * affect of applying them to all applicable operations.
 */

/* FI capabilities */
#define FI_MSG			(1ULL << 1)
#define FI_RMA			(1ULL << 2)
#define FI_TAGGED		(1ULL << 3)
#define FI_ATOMICS		(1ULL << 4)
#define FI_MULTICAST		(1ULL << 5)	/* multicast uses MSG ops */
#define FI_DYNAMIC_MR		(1ULL << 7)
#define FI_NAMED_RX_CTX		(1ULL << 8)
#define FI_BUFFERED_RECV	(1ULL << 9)

/*
 * Flags
 * The 64-bit flag field is divided as follows:
 * bits		use
 *  0 - 10	operation specific (used for a single call)
 * 11 - 32	common (usable with multiple operations)
 * 33 - 59	reserved
 * 60 - 63	provider specific
 */

#define FI_INJECT		(1ULL << 11)
#define FI_MULTI_RECV		(1ULL << 12)
#define FI_SOURCE		(1ULL << 13)
#define FI_SYMMETRIC		(1ULL << 14)

#define FI_READ			(1ULL << 16)
#define FI_WRITE		(1ULL << 17)
#define FI_RECV			(1ULL << 18)
#define FI_SEND			(1ULL << 19)
#define FI_REMOTE_READ		(1ULL << 20)
#define FI_REMOTE_WRITE		(1ULL << 21)

#define FI_REMOTE_CQ_DATA	(1ULL << 24)
#define FI_EVENT		(1ULL << 25)
#define FI_REMOTE_SIGNAL	(1ULL << 26)
#define FI_REMOTE_COMPLETE	(1ULL << 27)
#define FI_CANCEL		(1ULL << 28)
#define FI_MORE			(1ULL << 29)
#define FI_PEEK			(1ULL << 30)
#define FI_TRIGGER		(1ULL << 31)


struct fi_ioc {
	void			*addr;
	size_t			count;
};

/*
 * Format for transport addresses: sendto, writeto, etc.
 */
enum {
	FI_ADDR_UNSPEC,		/* void * */
	FI_SOCKADDR,		/* struct sockaddr */
	FI_SOCKADDR_IN,		/* struct sockaddr_in */
	FI_SOCKADDR_IN6,	/* struct sockaddr_in6 */
	FI_SOCKADDR_IB,		/* struct sockaddr_ib */
	FI_ADDR_PSMX,		/* uint64_t */
};

#define FI_ADDR_NOTAVAIL	UINT64_MAX
typedef uint64_t		fi_addr_t;
typedef void *			fi_connreq_t;

enum fi_progress {
	FI_PROGRESS_UNSPEC,
	FI_PROGRESS_AUTO,
	FI_PROGRESS_MANUAL
};

enum fi_threading {
	FI_THREAD_UNSPEC,
	FI_THREAD_SAFE,
	FI_THREAD_PROGRESS
};

#define FI_ORDER_RAR		(1 << 0)
#define FI_ORDER_RAW		(1 << 1)
#define FI_ORDER_RAS		(1 << 2)
#define FI_ORDER_WAR		(1 << 3)
#define FI_ORDER_WAW		(1 << 4)
#define FI_ORDER_WAS		(1 << 5)
#define FI_ORDER_SAR		(1 << 6)
#define FI_ORDER_SAW		(1 << 7)
#define FI_ORDER_SAS		(1 << 8)

enum fi_ep_type {
	FI_EP_UNSPEC,
	FI_EP_MSG,
	FI_EP_DGRAM,
	FI_EP_RDM,
	/* FI_EP_RAW, */
	/* FI_EP_PACKET, */
};

/* Endpoint protocol
 * If two providers support the same protocol, then they shall interoperate
 * when the protocol capabilities match.
 */
enum {
	FI_PROTO_UNSPEC,
	FI_PROTO_RDMA_CM_IB_RC,
	FI_PROTO_IWARP,
	FI_PROTO_IB_UD,
	FI_PROTO_PSMX,
	FI_PROTO_UDP,
	FI_PROTO_SOCK_RDS,
};

/* Mode bits */
#define FI_CONTEXT		(1ULL << 0)
#define FI_LOCAL_MR		(1ULL << 1)
#define FI_WRITE_NONCOHERENT	(1ULL << 2)
#define FI_PROV_MR_KEY		(1ULL << 3)
#define FI_MSG_PREFIX		(1ULL << 4)

struct fi_tx_ctx_attr {
	uint64_t		caps;
	uint64_t		op_flags;
	uint64_t		msg_order;
	size_t			inject_size;
	size_t			size;
	size_t			iov_limit;
	size_t			op_alignment;
};

struct fi_rx_ctx_attr {
	uint64_t		caps;
	uint64_t		op_flags;
	uint64_t		msg_order;
	size_t			total_buffered_recv;
	size_t			size;
	size_t			iov_limit;
	size_t			op_alignment;
};

struct fi_ep_attr {
	uint32_t		protocol;
	size_t			max_msg_size;
	size_t			inject_size;
	size_t			total_buffered_recv;
	size_t			msg_prefix_size;
	size_t			max_order_raw_size;
	size_t			max_order_war_size;
	size_t			max_order_waw_size;
	uint64_t		mem_tag_format;
	uint64_t		msg_order;
	size_t			tx_ctx_cnt;
	size_t			rx_ctx_cnt;
};

struct fi_domain_attr {
	struct fid_domain	*domain;
	char			*name;
	enum fi_threading	threading;
	enum fi_progress	control_progress;
	enum fi_progress	data_progress;
	size_t			mr_key_size;
	size_t			cq_data_size;
	size_t			ep_cnt;
	size_t			tx_ctx_cnt;
	size_t			rx_ctx_cnt;
	size_t			max_ep_tx_ctx;
	size_t			max_ep_rx_ctx;
	size_t			op_size;
	size_t			iov_size;
};

struct fi_fabric_attr {
	struct fid_fabric	*fabric;
	char			*name;
	char			*prov_name;
	uint32_t		prov_version;
};

struct fi_info {
	struct fi_info		*next;
	uint64_t		caps;
	uint64_t		mode;
	enum fi_ep_type		ep_type;
	uint32_t		addr_format;
	size_t			src_addrlen;
	size_t			dest_addrlen;
	void			*src_addr;
	void			*dest_addr;
	fi_connreq_t		connreq;
	struct fi_tx_ctx_attr	*tx_attr;
	struct fi_rx_ctx_attr	*rx_attr;
	struct fi_ep_attr	*ep_attr;
	struct fi_domain_attr	*domain_attr;
	struct fi_fabric_attr	*fabric_attr;
};

enum {
	FI_CLASS_UNSPEC,
	FI_CLASS_FABRIC,
	FI_CLASS_DOMAIN,
	FI_CLASS_EP,
	FI_CLASS_RX_CTX,
	FI_CLASS_TX_CTX,
	FI_CLASS_PEP,
	FI_CLASS_INTERFACE,
	FI_CLASS_AV,
	FI_CLASS_MR,
	FI_CLASS_EQ,
	FI_CLASS_CQ,
	FI_CLASS_CNTR,
	FI_CLASS_WAIT,
	FI_CLASS_POLL
};

struct fi_eq_attr;

struct fi_ops {
	size_t	size;
	int	(*close)(struct fid *fid);
	int	(*bind)(struct fid *fid, struct fid *bfid, uint64_t flags);
	int	(*sync)(struct fid *fid, uint64_t flags, void *context);
	int	(*control)(struct fid *fid, int command, void *arg);
	int	(*ops_open)(struct fid *fid, const char *name,
			uint64_t flags, void **ops, void *context);
};

/* All fabric interface descriptors must start with this structure */
struct fid {
	size_t			fclass;
	void			*context;
	struct fi_ops		*ops;
};

#define FI_NUMERICHOST		(1ULL << 1)

int fi_getinfo(uint32_t version, const char *node, const char *service,
	       uint64_t flags, struct fi_info *hints, struct fi_info **info);
void fi_freeinfo(struct fi_info *info);
struct fi_info *fi_dupinfo(const struct fi_info *info);

struct fi_ops_fabric {
	size_t	size;
	int	(*domain)(struct fid_fabric *fabric, struct fi_info *info,
			struct fid_domain **dom, void *context);
	int	(*endpoint)(struct fid_fabric *fabric, struct fi_info *info,
			struct fid_pep **pep, void *context);
	int	(*eq_open)(struct fid_fabric *fabric, struct fi_eq_attr *attr,
			struct fid_eq **eq, void *context);
};

struct fid_fabric {
	struct fid		fid;
	struct fi_ops_fabric	*ops;
};

int fi_fabric(struct fi_fabric_attr *attr, struct fid_fabric **fabric, void *context);

#define FI_CHECK_OP(ops, opstype, op) \
	((ops->size > offsetof(opstype, op)) && ops->op)

static inline int fi_close(struct fid *fid)
{
	return fid->ops->close(fid);
}

static inline int fi_bind(struct fid *fid, struct fid *bfid, uint64_t flags)
{
	return fid->ops->bind(fid, bfid, flags);
}

static inline int fi_sync(struct fid *fid, uint64_t flags, void *context)
{
	return fid->ops->sync(fid, flags, context);
}

struct fi_alias {
	struct fid 		**fid;
	uint64_t		flags;
};

/* control commands */
enum {
	FI_GETFIDFLAG,		/* uint64_t flags */
	FI_SETFIDFLAG,		/* uint64_t flags */
	FI_GETOPSFLAG,		/* uint64_t flags */
	FI_SETOPSFLAG,		/* uint64_t flags */

	/* Duplicate a fid_t.  This allows for 2 fids that refer to a single
	 * HW resource.  Each fid may reference functions that are optimized
	 * for different use cases.
	 */
	FI_ALIAS,		/* struct fi_alias * */
	FI_GETWAIT,		/* void * wait object */
};

static inline int fi_control(struct fid *fid, int command, void *arg)
{
	return fid->ops->control(fid, command, arg);
}

static inline int fi_alias(struct fid *fid, struct fid **alias_fid, uint64_t flags)
{
	struct fi_alias alias;
	alias.fid = alias_fid;
	alias.flags = flags;
	return fi_control(fid, FI_ALIAS, &alias);
}

static inline int
fi_open_ops(struct fid *fid, const char *name, uint64_t flags,
	    void **ops, void *context)
{
	return fid->ops->ops_open(fid, name, flags, ops, context);
}

enum fi_type {
	FI_TYPE_INFO,
	FI_TYPE_EP_TYPE,
	FI_TYPE_CAPS,
	FI_TYPE_OP_FLAGS,
	FI_TYPE_ADDR_FORMAT,
	FI_TYPE_TX_ATTR,
	FI_TYPE_RX_ATTR,
	FI_TYPE_EP_ATTR,
	FI_TYPE_DOMAIN_ATTR,
	FI_TYPE_FABRIC_ATTR,
	FI_TYPE_THREADING,
	FI_TYPE_PROGRESS,
	FI_TYPE_PROTOCOL,
	FI_TYPE_MSG_ORDER,
	FI_TYPE_MODE,
	FI_TYPE_AV_TYPE,
};

char *fi_tostr(const void *data, enum fi_type datatype);


#ifndef FABRIC_DIRECT

struct fi_context {
	void			*internal[4];
};

#else // FABRIC_DIRECT
#include <rdma/fi_direct.h>
#endif

#ifdef __cplusplus
}
#endif

#endif /* _FABRIC_H_ */
