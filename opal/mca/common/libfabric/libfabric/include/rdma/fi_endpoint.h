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

#ifndef _FI_ENDPOINT_H_
#define _FI_ENDPOINT_H_

#include <sys/socket.h>
#include <rdma/fabric.h>
#include <rdma/fi_domain.h>
#include <stddef.h>


#ifdef __cplusplus
extern "C" {
#endif


struct fi_msg {
	const struct iovec	*msg_iov;
	void			**desc;
	size_t			iov_count;
	fi_addr_t		addr;
	void			*context;
	uint64_t		data;
};

/* Endpoint option levels */
enum {
	FI_OPT_ENDPOINT
};

/* FI_OPT_ENDPOINT option names */
enum {
	FI_OPT_MIN_MULTI_RECV,		/* size_t */
	FI_OPT_CM_DATA_SIZE,		/* size_t */
};

struct fi_ops_ep {
	size_t	size;
	ssize_t	(*cancel)(fid_t fid, void *context);
	int	(*getopt)(fid_t fid, int level, int optname,
			void *optval, size_t *optlen);
	int	(*setopt)(fid_t fid, int level, int optname,
			const void *optval, size_t optlen);
	int	(*tx_ctx)(struct fid_ep *sep, int index,
			struct fi_tx_attr *attr, struct fid_ep **tx_ep,
			void *context);
	int	(*rx_ctx)(struct fid_ep *sep, int index,
			struct fi_rx_attr *attr, struct fid_ep **rx_ep,
			void *context);
	ssize_t (*rx_size_left)(struct fid_ep *ep);
	ssize_t (*tx_size_left)(struct fid_ep *ep);
};

struct fi_ops_msg {
	size_t	size;
	ssize_t (*recv)(struct fid_ep *ep, void *buf, size_t len, void *desc,
			fi_addr_t src_addr, void *context);
	ssize_t (*recvv)(struct fid_ep *ep, const struct iovec *iov, void **desc,
			size_t count, fi_addr_t src_addr, void *context);
	ssize_t (*recvmsg)(struct fid_ep *ep, const struct fi_msg *msg,
			uint64_t flags);
	ssize_t (*send)(struct fid_ep *ep, const void *buf, size_t len, void *desc,
			fi_addr_t dest_addr, void *context);
	ssize_t (*sendv)(struct fid_ep *ep, const struct iovec *iov, void **desc,
			size_t count, fi_addr_t dest_addr, void *context);
	ssize_t (*sendmsg)(struct fid_ep *ep, const struct fi_msg *msg,
			uint64_t flags);
	ssize_t	(*inject)(struct fid_ep *ep, const void *buf, size_t len,
			fi_addr_t dest_addr);
	ssize_t (*senddata)(struct fid_ep *ep, const void *buf, size_t len, void *desc,
			uint64_t data, fi_addr_t dest_addr, void *context);
	ssize_t	(*injectdata)(struct fid_ep *ep, const void *buf, size_t len,
			uint64_t data, fi_addr_t dest_addr);
};

struct fi_ops_cm;
struct fi_ops_rma;
struct fi_ops_tagged;
struct fi_ops_atomic;
/* struct fi_ops_collectives; */

/*
 * Calls which modify the properties of a endpoint (control, setopt, bind, ...)
 * must be serialized against all other operations.  Those calls may modify the
 * operations referenced by a endpoint in order to optimize the data transfer code
 * paths.
 *
 * A provider may allocate the minimal size structure needed to support the
 * ops requested by the user.
 */
struct fid_ep {
	struct fid		fid;
	struct fi_ops_ep	*ops;
	struct fi_ops_cm	*cm;
	struct fi_ops_msg	*msg;
	struct fi_ops_rma	*rma;
	struct fi_ops_tagged	*tagged;
	struct fi_ops_atomic	*atomic;
};

struct fid_pep {
	struct fid		fid;
	struct fi_ops_ep	*ops;
	struct fi_ops_cm	*cm;
};

struct fid_stx {
	struct fid		fid;
	struct fi_ops_ep	*ops;
};

#ifndef FABRIC_DIRECT

static inline int
fi_passive_ep(struct fid_fabric *fabric, struct fi_info *info,
	     struct fid_pep **pep, void *context)
{
	return fabric->ops->passive_ep(fabric, info, pep, context);
}

static inline int
fi_endpoint(struct fid_domain *domain, struct fi_info *info,
	    struct fid_ep **ep, void *context)
{
	return domain->ops->endpoint(domain, info, ep, context);
}

static inline int
fi_scalable_ep(struct fid_domain *domain, struct fi_info *info,
	    struct fid_ep **sep, void *context)
{
	return domain->ops->scalable_ep(domain, info, sep, context);
}

static inline int fi_ep_bind(struct fid_ep *ep, struct fid *bfid, uint64_t flags)
{
	return ep->fid.ops->bind(&ep->fid, bfid, flags);
}

static inline int fi_pep_bind(struct fid_pep *pep, struct fid *bfid, uint64_t flags)
{
	return pep->fid.ops->bind(&pep->fid, bfid, flags);
}

static inline int fi_scalable_ep_bind(struct fid_ep *sep, struct fid *bfid, uint64_t flags)
{
	return sep->fid.ops->bind(&sep->fid, bfid, flags);
}

static inline int fi_enable(struct fid_ep *ep)
{
	return ep->fid.ops->control(&ep->fid, FI_ENABLE, NULL);
}

static inline ssize_t fi_cancel(fid_t fid, void *context)
{
	struct fid_ep *ep = container_of(fid, struct fid_ep, fid);
	return ep->ops->cancel(fid, context);
}

static inline int
fi_setopt(fid_t fid, int level, int optname,
	  const void *optval, size_t optlen)
{
	struct fid_ep *ep = container_of(fid, struct fid_ep, fid);
	return ep->ops->setopt(fid, level, optname, optval, optlen);
}

static inline int
fi_getopt(fid_t fid, int level, int optname,
	  void *optval, size_t *optlen)
{
	struct fid_ep *ep = container_of(fid, struct fid_ep, fid);
	return ep->ops->getopt(fid, level, optname, optval, optlen);
}

static inline int
fi_tx_context(struct fid_ep *ep, int index, struct fi_tx_attr *attr,
	      struct fid_ep **tx_ep, void *context)
{
	return ep->ops->tx_ctx(ep, index, attr, tx_ep, context);
}

static inline int
fi_rx_context(struct fid_ep *ep, int index, struct fi_rx_attr *attr,
	      struct fid_ep **rx_ep, void *context)
{
	return ep->ops->rx_ctx(ep, index, attr, rx_ep, context);
}

static inline ssize_t
fi_rx_size_left(struct fid_ep *ep)
{
	return ep->ops->rx_size_left(ep);
}

static inline ssize_t
fi_tx_size_left(struct fid_ep *ep)
{
	return ep->ops->tx_size_left(ep);
}

static inline int
fi_stx_context(struct fid_domain *domain, struct fi_tx_attr *attr,
	       struct fid_stx **stx, void *context)
{
	return domain->ops->stx_ctx(domain, attr, stx, context);
}

static inline int
fi_srx_context(struct fid_domain *domain, struct fi_rx_attr *attr,
	       struct fid_ep **rx_ep, void *context)
{
	return domain->ops->srx_ctx(domain, attr, rx_ep, context);
}

static inline ssize_t
fi_recv(struct fid_ep *ep, void *buf, size_t len, void *desc, fi_addr_t src_addr,
	void *context)
{
	return ep->msg->recv(ep, buf, len, desc, src_addr, context);
}

static inline ssize_t
fi_recvv(struct fid_ep *ep, const struct iovec *iov, void **desc,
	 size_t count, fi_addr_t src_addr, void *context)
{
	return ep->msg->recvv(ep, iov, desc, count, src_addr, context);
}

static inline ssize_t
fi_recvmsg(struct fid_ep *ep, const struct fi_msg *msg, uint64_t flags)
{
	return ep->msg->recvmsg(ep, msg, flags);
}

static inline ssize_t
fi_send(struct fid_ep *ep, const void *buf, size_t len, void *desc,
	fi_addr_t dest_addr, void *context)
{
	return ep->msg->send(ep, buf, len, desc, dest_addr, context);
}

static inline ssize_t
fi_sendv(struct fid_ep *ep, const struct iovec *iov, void **desc,
	 size_t count, fi_addr_t dest_addr, void *context)
{
	return ep->msg->sendv(ep, iov, desc, count, dest_addr, context);
}

static inline ssize_t
fi_sendmsg(struct fid_ep *ep, const struct fi_msg *msg, uint64_t flags)
{
	return ep->msg->sendmsg(ep, msg, flags);
}

static inline ssize_t
fi_inject(struct fid_ep *ep, const void *buf, size_t len, fi_addr_t dest_addr)
{
	return ep->msg->inject(ep, buf, len, dest_addr);
}

static inline ssize_t
fi_senddata(struct fid_ep *ep, const void *buf, size_t len, void *desc,
	      uint64_t data, fi_addr_t dest_addr, void *context)
{
	return ep->msg->senddata(ep, buf, len, desc, data, dest_addr, context);
}

static inline ssize_t
fi_injectdata(struct fid_ep *ep, const void *buf, size_t len,
		uint64_t data, fi_addr_t dest_addr)
{
	return ep->msg->injectdata(ep, buf, len, data, dest_addr);
}

#else // FABRIC_DIRECT
#include <rdma/fi_direct_endpoint.h>
#endif

#ifdef __cplusplus
}
#endif

#endif /* _FI_ENDPOINT_H_ */
