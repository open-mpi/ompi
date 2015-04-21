/*
 * Copyright (c) 2014 Intel Corporation. All rights reserved.
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

#include <rdma/fi_errno.h>
#include "fi_enosys.h"


/*
 * struct fi_ops
 */
int fi_no_bind(struct fid *fid, struct fid *bfid, uint64_t flags)
{
	return -FI_ENOSYS;
}
int fi_no_control(struct fid *fid, int command, void *arg)
{
	return -FI_ENOSYS;
}
int fi_no_ops_open(struct fid *fid, const char *name,
		uint64_t flags, void **ops, void *context)
{
	return -FI_ENOSYS;
}

/*
 * struct fi_ops_fabric
 */
int fi_no_domain(struct fid_fabric *fabric, struct fi_domain_attr *attr,
		struct fid_domain **dom, void *context)
{
	return -FI_ENOSYS;
}
int fi_no_passive_ep(struct fid_fabric *fabric, struct fi_info *info,
		struct fid_pep **pep, void *context)
{
	return -FI_ENOSYS;
}
int fi_no_eq_open(struct fid_fabric *fabric, struct fi_eq_attr *attr,
		struct fid_eq **eq, void *context)
{
	return -FI_ENOSYS;
}
int fi_no_wait_open(struct fid_fabric *fabric, struct fi_wait_attr *attr,
		struct fid_wait **waitset)
{
	return -FI_ENOSYS;
}

/*
 * struct fi_ops_atomic
 */
ssize_t fi_no_atomic_write(struct fid_ep *ep,
		const void *buf, size_t count, void *desc,
		fi_addr_t dest_addr, uint64_t addr, uint64_t key,
		enum fi_datatype datatype, enum fi_op op, void *context)
{
	return -FI_ENOSYS;
}
ssize_t fi_no_atomic_writev(struct fid_ep *ep,
		const struct fi_ioc *iov, void **desc, size_t count,
		fi_addr_t dest_addr, uint64_t addr, uint64_t key,
		enum fi_datatype datatype, enum fi_op op, void *context)
{
	return -FI_ENOSYS;
}
ssize_t fi_no_atomic_writemsg(struct fid_ep *ep,
		const struct fi_msg_atomic *msg, uint64_t flags)
{
	return -FI_ENOSYS;
}
ssize_t fi_no_atomic_inject(struct fid_ep *ep, const void *buf, size_t count,
		fi_addr_t dest_addr, uint64_t addr, uint64_t key,
		enum fi_datatype datatype, enum fi_op op)
{
	return -FI_ENOSYS;
}
ssize_t fi_no_atomic_readwrite(struct fid_ep *ep,
		const void *buf, size_t count, void *desc,
		void *result, void *result_desc,
		fi_addr_t dest_addr, uint64_t addr, uint64_t key,
		enum fi_datatype datatype, enum fi_op op, void *context)
{
	return -FI_ENOSYS;
}
ssize_t fi_no_atomic_readwritev(struct fid_ep *ep,
		const struct fi_ioc *iov, void **desc, size_t count,
		struct fi_ioc *resultv, void **result_desc, size_t result_count,
		fi_addr_t dest_addr, uint64_t addr, uint64_t key,
		enum fi_datatype datatype, enum fi_op op, void *context)
{
	return -FI_ENOSYS;
}
ssize_t fi_no_atomic_readwritemsg(struct fid_ep *ep,
		const struct fi_msg_atomic *msg,
		struct fi_ioc *resultv, void **result_desc, size_t result_count,
		uint64_t flags)
{
	return -FI_ENOSYS;
}
ssize_t fi_no_atomic_compwrite(struct fid_ep *ep,
		const void *buf, size_t count, void *desc,
		const void *compare, void *compare_desc,
		void *result, void *result_desc,
		fi_addr_t dest_addr, uint64_t addr, uint64_t key,
		enum fi_datatype datatype, enum fi_op op, void *context)
{
	return -FI_ENOSYS;
}
ssize_t fi_no_atomic_compwritev(struct fid_ep *ep,
		const struct fi_ioc *iov, void **desc, size_t count,
		const struct fi_ioc *comparev, void **compare_desc, size_t compare_count,
		struct fi_ioc *resultv, void **result_desc, size_t result_count,
		fi_addr_t dest_addr, uint64_t addr, uint64_t key,
		enum fi_datatype datatype, enum fi_op op, void *context)
{
	return -FI_ENOSYS;
}
ssize_t fi_no_atomic_compwritemsg(struct fid_ep *ep,
		const struct fi_msg_atomic *msg,
		const struct fi_ioc *comparev, void **compare_desc, size_t compare_count,
		struct fi_ioc *resultv, void **result_desc, size_t result_count,
		uint64_t flags)
{
	return -FI_ENOSYS;
}
int fi_no_atomic_writevalid(struct fid_ep *ep,
		enum fi_datatype datatype, enum fi_op op, size_t *count)
{
	return -FI_ENOSYS;
}
int fi_no_atomic_readwritevalid(struct fid_ep *ep,
		enum fi_datatype datatype, enum fi_op op, size_t *count)
{
	return -FI_ENOSYS;
}
int fi_no_atomic_compwritevalid(struct fid_ep *ep,
		enum fi_datatype datatype, enum fi_op op, size_t *count)
{
	return -FI_ENOSYS;
}

/*
 * struct fi_ops_cm
 */
int fi_no_getname(fid_t fid, void *addr, size_t *addrlen)
{
	return -FI_ENOSYS;
}
int fi_no_getpeer(struct fid_ep *ep, void *addr, size_t *addrlen)
{
	return -FI_ENOSYS;
}
int fi_no_connect(struct fid_ep *ep, const void *addr,
		const void *param, size_t paramlen)
{
	return -FI_ENOSYS;
}
int fi_no_listen(struct fid_pep *pep)
{
	return -FI_ENOSYS;
}
int fi_no_accept(struct fid_ep *ep, const void *param, size_t paramlen)
{
	return -FI_ENOSYS;
}
int fi_no_reject(struct fid_pep *pep, fi_connreq_t connreq,
		const void *param, size_t paramlen)
{
	return -FI_ENOSYS;
}
int fi_no_shutdown(struct fid_ep *ep, uint64_t flags)
{
	return -FI_ENOSYS;
}

/*
 * struct fi_ops_av
 */

/*
 * struct fi_ops_domain
 */
int fi_no_av_open(struct fid_domain *domain, struct fi_av_attr *attr,
		struct fid_av **av, void *context)
{
	return -FI_ENOSYS;
}
int fi_no_cq_open(struct fid_domain *domain, struct fi_cq_attr *attr,
		struct fid_cq **cq, void *context)
{
	return -FI_ENOSYS;
}
int fi_no_endpoint(struct fid_domain *domain, struct fi_info *info,
		struct fid_ep **ep, void *context)
{
	return -FI_ENOSYS;
}
int fi_no_scalable_ep(struct fid_domain *domain, struct fi_info *info,
		struct fid_ep **sep, void *context)
{
	return -FI_ENOSYS;
}
int fi_no_cntr_open(struct fid_domain *domain, struct fi_cntr_attr *attr,
		struct fid_cntr **cntr, void *context)
{
	return -FI_ENOSYS;
}
int fi_no_poll_open(struct fid_domain *domain, struct fi_poll_attr *attr,
		struct fid_poll **pollset)
{
	return -FI_ENOSYS;
}
int fi_no_stx_context(struct fid_domain *domain, struct fi_tx_attr *attr,
		struct fid_stx **stx, void *context)
{
	return -FI_ENOSYS;
}
int fi_no_srx_context(struct fid_domain *domain, struct fi_rx_attr *attr,
		struct fid_ep **rx_ep, void *context)
{
	return -FI_ENOSYS;
}

/*
 * struct fi_ops_mr
 */
int fi_no_mr_reg(struct fid *fid, const void *buf, size_t len,
		uint64_t access, uint64_t offset, uint64_t requested_key,
		uint64_t flags, struct fid_mr **mr, void *context)
{
	return -FI_ENOSYS;
}
int fi_no_mr_regv(struct fid *fid, const struct iovec *iov,
		size_t count, uint64_t access,
		uint64_t offset, uint64_t requested_key,
		uint64_t flags, struct fid_mr **mr, void *context)
{
	return -FI_ENOSYS;
}
int fi_no_mr_regattr(struct fid *fid, const struct fi_mr_attr *attr,
		uint64_t flags, struct fid_mr **mr)
{
	return -FI_ENOSYS;
}

/*
 * struct fi_ops_ep
 */
ssize_t fi_no_cancel(fid_t fid, void *context)
{
	return -FI_ENOSYS;
}
int fi_no_getopt(fid_t fid, int level, int optname,
		void *optval, size_t *optlen)
{
	return -FI_ENOSYS;
}
int fi_no_setopt(fid_t fid, int level, int optname,
		const void *optval, size_t optlen)
{
	return -FI_ENOSYS;
}
int fi_no_tx_ctx(struct fid_ep *sep, int index,
		struct fi_tx_attr *attr, struct fid_ep **tx_ep,
		void *context)
{
	return -FI_ENOSYS;
}
int fi_no_rx_ctx(struct fid_ep *sep, int index,
		struct fi_rx_attr *attr, struct fid_ep **rx_ep,
		void *context)
{
	return -FI_ENOSYS;
}
ssize_t fi_no_rx_size_left(struct fid_ep *ep)
{
	return -FI_ENOSYS;
}
ssize_t fi_no_tx_size_left(struct fid_ep *ep)
{
	return -FI_ENOSYS;
}

/*
 * struct fi_ops_msg
 */
ssize_t fi_no_msg_recv(struct fid_ep *ep, void *buf, size_t len, void *desc,
		fi_addr_t src_addr, void *context)
{
	return -FI_ENOSYS;
}
ssize_t fi_no_msg_recvv(struct fid_ep *ep, const struct iovec *iov, void **desc,
		size_t count, fi_addr_t src_addr, void *context)
{
	return -FI_ENOSYS;
}
ssize_t fi_no_msg_recvmsg(struct fid_ep *ep, const struct fi_msg *msg,
		uint64_t flags)
{
	return -FI_ENOSYS;
}
ssize_t fi_no_msg_send(struct fid_ep *ep, const void *buf, size_t len, void *desc,
		fi_addr_t dest_addr, void *context)
{
	return -FI_ENOSYS;
}
ssize_t fi_no_msg_sendv(struct fid_ep *ep, const struct iovec *iov, void **desc,
		size_t count, fi_addr_t dest_addr, void *context)
{
	return -FI_ENOSYS;
}
ssize_t fi_no_msg_sendmsg(struct fid_ep *ep, const struct fi_msg *msg,
		uint64_t flags)
{
	return -FI_ENOSYS;
}
ssize_t fi_no_msg_inject(struct fid_ep *ep, const void *buf, size_t len,
		fi_addr_t dest_addr)
{
	return -FI_ENOSYS;
}
ssize_t fi_no_msg_senddata(struct fid_ep *ep, const void *buf, size_t len, void *desc,
		uint64_t data, fi_addr_t dest_addr, void *context)
{
	return -FI_ENOSYS;
}
ssize_t fi_no_msg_injectdata(struct fid_ep *ep, const void *buf, size_t len,
		uint64_t data, fi_addr_t dest_addr)
{
	return -FI_ENOSYS;
}

/*
 * struct fi_ops_wait
 */

/*
 * struct fi_ops_poll
 */

/*
 * struct fi_ops_eq
 */
ssize_t fi_no_eq_write(struct fid_eq *eq, uint32_t event,
		const void *buf, size_t len, uint64_t flags)
{
	return -FI_ENOSYS;
}
ssize_t fi_no_eq_sread(struct fid_eq *eq, uint32_t *event,
		void *buf, size_t len, int timeout, uint64_t flags)
{
	return -FI_ENOSYS;
}

/*
 * struct fi_ops_cq
 */
ssize_t fi_no_cq_readfrom(struct fid_cq *cq, void *buf, size_t count,
		fi_addr_t *src_addr)
{
	return -FI_ENOSYS;
}
ssize_t fi_no_cq_sread(struct fid_cq *cq, void *buf, size_t count,
		const void *cond, int timeout)
{
	return -FI_ENOSYS;
}
ssize_t fi_no_cq_sreadfrom(struct fid_cq *cq, void *buf, size_t count,
		fi_addr_t *src_addr, const void *cond, int timeout)
{
	return -FI_ENOSYS;
}
int fi_no_cq_signal(struct fid_cq *cq)
{
	return -FI_ENOSYS;
}

/*
 * struct fi_ops_cntr
 */
int fi_no_cntr_add(struct fid_cntr *cntr, uint64_t value)
{
	return -FI_ENOSYS;
}
int fi_no_cntr_set(struct fid_cntr *cntr, uint64_t value)
{
	return -FI_ENOSYS;
}
int fi_no_cntr_wait(struct fid_cntr *cntr, uint64_t threshold, int timeout)
{
	return -FI_ENOSYS;
}

/*
 * struct fi_ops_rma
 */
ssize_t fi_no_rma_read(struct fid_ep *ep, void *buf, size_t len, void *desc,
		fi_addr_t src_addr, uint64_t addr, uint64_t key, void *context)
{
	return -FI_ENOSYS;
}
ssize_t fi_no_rma_readv(struct fid_ep *ep, const struct iovec *iov, void **desc,
		size_t count, fi_addr_t src_addr, uint64_t addr, uint64_t key,
		void *context)
{
	return -FI_ENOSYS;
}
ssize_t fi_no_rma_readmsg(struct fid_ep *ep, const struct fi_msg_rma *msg,
		uint64_t flags)
{
	return -FI_ENOSYS;
}
ssize_t fi_no_rma_write(struct fid_ep *ep, const void *buf, size_t len, void *desc,
		fi_addr_t dest_addr, uint64_t addr, uint64_t key, void *context)
{
	return -FI_ENOSYS;
}
ssize_t fi_no_rma_writev(struct fid_ep *ep, const struct iovec *iov, void **desc,
		size_t count, fi_addr_t dest_addr, uint64_t addr, uint64_t key,
		void *context)
{
	return -FI_ENOSYS;
}
ssize_t fi_no_rma_writemsg(struct fid_ep *ep, const struct fi_msg_rma *msg,
		uint64_t flags)
{
	return -FI_ENOSYS;
}
ssize_t fi_no_rma_inject(struct fid_ep *ep, const void *buf, size_t len,
		fi_addr_t dest_addr, uint64_t addr, uint64_t key)
{
	return -FI_ENOSYS;
}
ssize_t fi_no_rma_writedata(struct fid_ep *ep, const void *buf, size_t len, void *desc,
		uint64_t data, fi_addr_t dest_addr, uint64_t addr, uint64_t key,
		void *context)
{
	return -FI_ENOSYS;
}
ssize_t fi_no_rma_injectdata(struct fid_ep *ep, const void *buf, size_t len,
		uint64_t data, fi_addr_t dest_addr, uint64_t addr, uint64_t key)
{
	return -FI_ENOSYS;
}

/*
 * struct fi_ops_tagged
 */
ssize_t fi_no_tagged_recv(struct fid_ep *ep, void *buf, size_t len, void *desc,
		fi_addr_t src_addr, uint64_t tag, uint64_t ignore, void *context)
{
	return -FI_ENOSYS;
}
ssize_t fi_no_tagged_recvv(struct fid_ep *ep, const struct iovec *iov, void **desc,
		size_t count, fi_addr_t src_addr, uint64_t tag, uint64_t ignore,
		void *context)
{
	return -FI_ENOSYS;
}
ssize_t fi_no_tagged_recvmsg(struct fid_ep *ep, const struct fi_msg_tagged *msg,
		uint64_t flags)
{
	return -FI_ENOSYS;
}
ssize_t fi_no_tagged_send(struct fid_ep *ep, const void *buf, size_t len, void *desc,
		fi_addr_t dest_addr, uint64_t tag, void *context)
{
	return -FI_ENOSYS;
}
ssize_t fi_no_tagged_sendv(struct fid_ep *ep, const struct iovec *iov, void **desc,
		size_t count, fi_addr_t dest_addr, uint64_t tag, void *context)
{
	return -FI_ENOSYS;
}
ssize_t fi_no_tagged_sendmsg(struct fid_ep *ep, const struct fi_msg_tagged *msg,
		uint64_t flags)
{
	return -FI_ENOSYS;
}
ssize_t fi_no_tagged_inject(struct fid_ep *ep, const void *buf, size_t len,
		fi_addr_t dest_addr, uint64_t tag)
{
	return -FI_ENOSYS;
}
ssize_t fi_no_tagged_senddata(struct fid_ep *ep, const void *buf, size_t len, void *desc,
		uint64_t data, fi_addr_t dest_addr, uint64_t tag, void *context)
{
	return -FI_ENOSYS;
}
ssize_t fi_no_tagged_injectdata(struct fid_ep *ep, const void *buf, size_t len,
		uint64_t data, fi_addr_t dest_addr, uint64_t tag)
{
	return -FI_ENOSYS;
}
ssize_t fi_no_tagged_search(struct fid_ep *ep, uint64_t *tag, uint64_t ignore,
		uint64_t flags, fi_addr_t *src_addr, size_t *len, void *context)
{
	return -FI_ENOSYS;
}

/*
 * fi_ops_av
 */
int fi_no_av_insert(struct fid_av *av, const void *addr, size_t count,
			fi_addr_t *fi_addr, uint64_t flags, void *context)
{
	return -FI_ENOSYS;
}
int fi_no_av_insertsvc(struct fid_av *av, const char *node,
		const char *service, fi_addr_t *fi_addr, uint64_t flags,
		void *context)
{
	return -FI_ENOSYS;
}
int fi_no_av_insertsym(struct fid_av *av, const char *node, size_t nodecnt,
			const char *service, size_t svccnt, fi_addr_t *fi_addr,
			uint64_t flags, void *context)
{
	return -FI_ENOSYS;
}
int fi_no_av_remove(struct fid_av *av, fi_addr_t *fi_addr, size_t count,
			uint64_t flags)
{
	return -FI_ENOSYS;
}
