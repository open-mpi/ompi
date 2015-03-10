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

#ifndef _FI_ATOMIC_H_
#define _FI_ATOMIC_H_

#include <rdma/fabric.h>
#include <rdma/fi_endpoint.h>
#include <rdma/fi_rma.h>


#ifdef __cplusplus
extern "C" {
#endif

#ifndef FABRIC_DIRECT

enum fi_datatype {
	FI_INT8,
	FI_UINT8,
	FI_INT16,
	FI_UINT16,
	FI_INT32,
	FI_UINT32,
	FI_INT64,
	FI_UINT64,
	FI_FLOAT,
	FI_DOUBLE,
	FI_FLOAT_COMPLEX,
	FI_DOUBLE_COMPLEX,
	FI_LONG_DOUBLE,
	FI_LONG_DOUBLE_COMPLEX,
	FI_DATATYPE_LAST
};

enum fi_op {
	FI_MIN,
	FI_MAX,
	FI_SUM,
	FI_PROD,
	FI_LOR,
	FI_LAND,
	FI_BOR,
	FI_BAND,
	FI_LXOR,
	FI_BXOR,
	FI_ATOMIC_READ,
	FI_ATOMIC_WRITE,
	FI_CSWAP,
	FI_CSWAP_NE,
	FI_CSWAP_LE,
	FI_CSWAP_LT,
	FI_CSWAP_GE,
	FI_CSWAP_GT,
	FI_MSWAP,
	FI_ATOMIC_OP_LAST
};

#else
#include <rdma/fi_direct_atomic_def.h>
#endif /* FABRIC_DIRECT */

struct fi_msg_atomic {
	const struct fi_ioc	*msg_iov;
	void			**desc;
	size_t			iov_count;
	fi_addr_t		addr;
	const struct fi_rma_ioc	*rma_iov;
	size_t			rma_iov_count;
	enum fi_datatype	datatype;
	enum fi_op		op;
	void			*context;
	uint64_t		data;
};

struct fi_ops_atomic {
	size_t	size;
	ssize_t	(*write)(struct fid_ep *ep,
			const void *buf, size_t count, void *desc,
			fi_addr_t dest_addr,
			uint64_t addr, uint64_t key,
			enum fi_datatype datatype, enum fi_op op, void *context);
	ssize_t	(*writev)(struct fid_ep *ep,
			const struct fi_ioc *iov, void **desc, size_t count,
			fi_addr_t dest_addr,
			uint64_t addr, uint64_t key,
			enum fi_datatype datatype, enum fi_op op, void *context);
	ssize_t	(*writemsg)(struct fid_ep *ep,
			const struct fi_msg_atomic *msg, uint64_t flags);
	ssize_t	(*inject)(struct fid_ep *ep, const void *buf, size_t count,
			fi_addr_t dest_addr, uint64_t addr, uint64_t key,
			enum fi_datatype datatype, enum fi_op op);

	ssize_t	(*readwrite)(struct fid_ep *ep,
			const void *buf, size_t count, void *desc,
			void *result, void *result_desc,
			fi_addr_t dest_addr,
			uint64_t addr, uint64_t key,
			enum fi_datatype datatype, enum fi_op op, void *context);
	ssize_t	(*readwritev)(struct fid_ep *ep,
			const struct fi_ioc *iov, void **desc, size_t count,
			struct fi_ioc *resultv, void **result_desc, size_t result_count,
			fi_addr_t dest_addr,
			uint64_t addr, uint64_t key,
			enum fi_datatype datatype, enum fi_op op, void *context);
	ssize_t	(*readwritemsg)(struct fid_ep *ep,
			const struct fi_msg_atomic *msg,
			struct fi_ioc *resultv, void **result_desc, size_t result_count,
			uint64_t flags);

	ssize_t	(*compwrite)(struct fid_ep *ep,
			const void *buf, size_t count, void *desc,
			const void *compare, void *compare_desc,
			void *result, void *result_desc,
			fi_addr_t dest_addr,
			uint64_t addr, uint64_t key,
			enum fi_datatype datatype, enum fi_op op, void *context);
	ssize_t	(*compwritev)(struct fid_ep *ep,
			const struct fi_ioc *iov, void **desc, size_t count,
			const struct fi_ioc *comparev, void **compare_desc, size_t compare_count,
			struct fi_ioc *resultv, void **result_desc, size_t result_count,
			fi_addr_t dest_addr,
			uint64_t addr, uint64_t key,
			enum fi_datatype datatype, enum fi_op op, void *context);
	ssize_t	(*compwritemsg)(struct fid_ep *ep,
			const struct fi_msg_atomic *msg,
			const struct fi_ioc *comparev, void **compare_desc, size_t compare_count,
			struct fi_ioc *resultv, void **result_desc, size_t result_count,
			uint64_t flags);

	int	(*writevalid)(struct fid_ep *ep,
			enum fi_datatype datatype, enum fi_op op, size_t *count);
	int	(*readwritevalid)(struct fid_ep *ep,
			enum fi_datatype datatype, enum fi_op op, size_t *count);
	int	(*compwritevalid)(struct fid_ep *ep,
			enum fi_datatype datatype, enum fi_op op, size_t *count);
};

#ifndef FABRIC_DIRECT

static inline ssize_t
fi_atomic(struct fid_ep *ep,
	  const void *buf, size_t count, void *desc,
	  fi_addr_t dest_addr,
	  uint64_t addr, uint64_t key,
	  enum fi_datatype datatype, enum fi_op op, void *context)
{
	return ep->atomic->write(ep, buf, count, desc, dest_addr, addr, key,
			datatype, op, context);
}

static inline ssize_t
fi_atomicv(struct fid_ep *ep,
	   const struct fi_ioc *iov, void **desc, size_t count,
	   fi_addr_t dest_addr,
	   uint64_t addr, uint64_t key,
	   enum fi_datatype datatype, enum fi_op op, void *context)
{
	return ep->atomic->writev(ep, iov, desc, count, dest_addr, addr, key,
			datatype, op, context);
}

static inline ssize_t
fi_atomicmsg(struct fid_ep *ep,
	     const struct fi_msg_atomic *msg, uint64_t flags)
{
	return ep->atomic->writemsg(ep, msg, flags);
}

static inline ssize_t
fi_inject_atomic(struct fid_ep *ep, const void *buf, size_t count,
		 fi_addr_t dest_addr, uint64_t addr, uint64_t key,
		 enum fi_datatype datatype, enum fi_op op)
{
	return ep->atomic->inject(ep, buf, count, dest_addr, addr,
			key, datatype, op);
}

static inline ssize_t
fi_fetch_atomic(struct fid_ep *ep,
		const void *buf, size_t count, void *desc,
		void *result, void *result_desc,
		fi_addr_t dest_addr,
		uint64_t addr, uint64_t key,
		enum fi_datatype datatype, enum fi_op op, void *context)
{
	return ep->atomic->readwrite(ep, buf, count, desc, result, result_desc,
			dest_addr, addr, key, datatype, op, context);
}

static inline ssize_t
fi_fetch_atomicv(struct fid_ep *ep,
		 const struct fi_ioc *iov, void **desc, size_t count,
		 struct fi_ioc *resultv, void **result_desc, size_t result_count,
		 fi_addr_t dest_addr,
		 uint64_t addr, uint64_t key,
		 enum fi_datatype datatype, enum fi_op op, void *context)
{
	return ep->atomic->readwritev(ep, iov, desc, count,
			resultv, result_desc, result_count,
			dest_addr, addr, key, datatype, op, context);
}

static inline ssize_t
fi_fetch_atomicmsg(struct fid_ep *ep,
		   const struct fi_msg_atomic *msg,
		   struct fi_ioc *resultv, void **result_desc, size_t result_count,
		   uint64_t flags)
{
	return ep->atomic->readwritemsg(ep, msg, resultv, result_desc,
			result_count, flags);
}

static inline ssize_t
fi_compare_atomic(struct fid_ep *ep,
		  const void *buf, size_t count, void *desc,
		  const void *compare, void *compare_desc,
		  void *result, void *result_desc,
		  fi_addr_t dest_addr,
		  uint64_t addr, uint64_t key,
		  enum fi_datatype datatype, enum fi_op op, void *context)
{
	return ep->atomic->compwrite(ep, buf, count, desc,
			compare, compare_desc, result, result_desc,
			dest_addr, addr, key, datatype, op, context);
}

static inline ssize_t
fi_compare_atomicv(struct fid_ep *ep,
		   const struct fi_ioc *iov, void **desc, size_t count,
		   const struct fi_ioc *comparev, void **compare_desc, size_t compare_count,
		   struct fi_ioc *resultv, void **result_desc, size_t result_count,
		   fi_addr_t dest_addr,
		   uint64_t addr, uint64_t key,
		   enum fi_datatype datatype, enum fi_op op, void *context)
{
	return ep->atomic->compwritev(ep, iov, desc, count,
			comparev, compare_desc, compare_count,
			resultv, result_desc, result_count,
			dest_addr, addr, key, datatype, op, context);
}

static inline ssize_t
fi_compare_atomicmsg(struct fid_ep *ep,
		     const struct fi_msg_atomic *msg,
		     const struct fi_ioc *comparev, void **compare_desc, size_t compare_count,
		     struct fi_ioc *resultv, void **result_desc, size_t result_count,
		     uint64_t flags)
{
	return ep->atomic->compwritemsg(ep, msg,
			comparev, compare_desc, compare_count,
			resultv, result_desc, result_count, flags);
}

static inline int
fi_atomicvalid(struct fid_ep *ep,
	       enum fi_datatype datatype, enum fi_op op, size_t *count)
{
	return ep->atomic->writevalid(ep, datatype, op, count);
}

static inline int
fi_fetch_atomicvalid(struct fid_ep *ep,
		     enum fi_datatype datatype, enum fi_op op, size_t *count)
{
	return ep->atomic->readwritevalid(ep, datatype, op, count);
}

static inline int
fi_compare_atomicvalid(struct fid_ep *ep,
		       enum fi_datatype datatype, enum fi_op op, size_t *count)
{
	return ep->atomic->compwritevalid(ep, datatype, op, count);
}

#else // FABRIC_DIRECT
#include <rdma/fi_direct_atomic.h>
#endif

#ifdef __cplusplus
}
#endif

#endif /* _FI_ATOMIC_H_ */
