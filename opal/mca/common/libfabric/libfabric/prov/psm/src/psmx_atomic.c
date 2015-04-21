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

#include "psmx.h"

/* Atomics protocol:
 *
 * Atomics REQ:
 *	args[0].u32w0	cmd
 *	args[0].u32w1	count
 *	args[1].u64	req
 *	args[2].u64	addr
 *	args[3].u64	key
 *	args[4].u32w0	datatype
 *	args[4].u32w1	op
 *
 * Atomics REP:
 *	args[0].u32w0	cmd
 *	args[0].u32w1	error
 *	args[1].u64	req
 */

static pthread_mutex_t	psmx_atomic_lock = PTHREAD_MUTEX_INITIALIZER;

#define CASE_INT_TYPE(FUNC,...) \
		case FI_INT8:	FUNC(__VA_ARGS__,int8_t); break; \
		case FI_UINT8:	FUNC(__VA_ARGS__,uint8_t); break; \
		case FI_INT16:	FUNC(__VA_ARGS__,int16_t); break; \
		case FI_UINT16: FUNC(__VA_ARGS__,uint16_t); break; \
		case FI_INT32:	FUNC(__VA_ARGS__,int32_t); break; \
		case FI_UINT32: FUNC(__VA_ARGS__,uint32_t); break; \
		case FI_INT64:	FUNC(__VA_ARGS__,int64_t); break; \
		case FI_UINT64: FUNC(__VA_ARGS__,uint64_t); break;

#define CASE_FP_TYPE(FUNC,...) \
		case FI_FLOAT:	FUNC(__VA_ARGS__,float); break; \
		case FI_DOUBLE:	FUNC(__VA_ARGS__,double); break; \
		case FI_LONG_DOUBLE: FUNC(__VA_ARGS__,long double); break;

#define CASE_COMPLEX_TYPE(FUNC,...) \
		case FI_FLOAT_COMPLEX:	FUNC(__VA_ARGS__,float complex); break; \
		case FI_DOUBLE_COMPLEX:	FUNC(__VA_ARGS__,double complex); break; \
		case FI_LONG_DOUBLE_COMPLEX: FUNC(__VA_ARGS__,long double complex); break;

#define SWITCH_INT_TYPE(type,...) \
		switch (type) { \
		CASE_INT_TYPE(__VA_ARGS__) \
		default: return -FI_EOPNOTSUPP; \
		}

#define SWITCH_ORD_TYPE(type,...) \
		switch (type) { \
		CASE_INT_TYPE(__VA_ARGS__) \
		CASE_FP_TYPE(__VA_ARGS__) \
		default: return -FI_EOPNOTSUPP; \
		}

#define SWITCH_ALL_TYPE(type,...) \
		switch (type) { \
		CASE_INT_TYPE(__VA_ARGS__) \
		CASE_FP_TYPE(__VA_ARGS__) \
		CASE_COMPLEX_TYPE(__VA_ARGS__) \
		default: return -FI_EOPNOTSUPP; \
		}

#define PSMX_MIN(dst,src)	if ((dst) > (src)) (dst) = (src)
#define PSMX_MAX(dst,src)	if ((dst) < (src)) (dst) = (src)
#define PSMX_SUM(dst,src)	(dst) += (src)
#define PSMX_PROD(dst,src)	(dst) *= (src)
#define PSMX_LOR(dst,src)	(dst) = (dst) || (src)
#define PSMX_LAND(dst,src)	(dst) = (dst) && (src)
#define PSMX_BOR(dst,src)	(dst) |= (src)
#define PSMX_BAND(dst,src)	(dst) &= (src)
#define PSMX_LXOR(dst,src)	(dst) = ((dst) && !(src)) || (!(dst) && (src))
#define PSMX_BXOR(dst,src)	(dst) ^= (src)
#define PSMX_COPY(dst,src)	(dst) = (src)

#define PSMX_ATOMIC_READ(dst,res,cnt,TYPE) \
		do { \
			int i; \
			TYPE *d = (dst); \
			TYPE *r = (res); \
			pthread_mutex_lock(&psmx_atomic_lock); \
			for (i=0; i<(cnt); i++) \
				r[i] = d[i]; \
			pthread_mutex_unlock(&psmx_atomic_lock); \
		} while (0)

#define PSMX_ATOMIC_WRITE(dst,src,cnt,OP,TYPE) \
		do { \
			int i; \
			TYPE *d = (dst); \
			TYPE *s = (src); \
			pthread_mutex_lock(&psmx_atomic_lock); \
			for (i=0; i<cnt; i++) \
				OP(d[i],s[i]); \
			pthread_mutex_unlock(&psmx_atomic_lock); \
		} while (0)

#define PSMX_ATOMIC_READWRITE(dst,src,res,cnt,OP,TYPE) \
		do { \
			int i; \
			TYPE *d = (dst); \
			TYPE *s = (src); \
			TYPE *r = (res); \
			pthread_mutex_lock(&psmx_atomic_lock); \
			for (i=0; i<(cnt); i++) {\
				r[i] = d[i]; \
				OP(d[i],s[i]); \
			} \
			pthread_mutex_unlock(&psmx_atomic_lock); \
		} while (0)

#define PSMX_ATOMIC_CSWAP(dst,src,cmp,res,cnt,CMP_OP,TYPE) \
		do { \
			int i; \
			TYPE *d = (dst); \
			TYPE *s = (src); \
			TYPE *c = (cmp); \
			TYPE *r = (res); \
			pthread_mutex_lock(&psmx_atomic_lock); \
			for (i=0; i<(cnt); i++) { \
				r[i] = d[i]; \
				if (d[i] CMP_OP c[i]) \
					d[i] = s[i]; \
			} \
			pthread_mutex_unlock(&psmx_atomic_lock); \
		} while (0)

#define PSMX_ATOMIC_MSWAP(dst,src,cmp,res,cnt,TYPE) \
		do { \
			int i; \
			TYPE *d = (dst); \
			TYPE *s = (src); \
			TYPE *c = (cmp); \
			TYPE *r = (res); \
			pthread_mutex_lock(&psmx_atomic_lock); \
			for (i=0; i<(cnt); i++) { \
				r[i] = d[i]; \
				d[i] = (s[i] & c[i]) | (d[i] & ~c[i]); \
			} \
			pthread_mutex_unlock(&psmx_atomic_lock); \
		} while (0)

static int psmx_atomic_do_write(void *dest, void *src,
				int datatype, int op, int count)
{
	switch (op) {
	case FI_MIN:
		SWITCH_ORD_TYPE(datatype,PSMX_ATOMIC_WRITE,
				dest,src,count,PSMX_MIN);
		break;

	case FI_MAX:
		SWITCH_ORD_TYPE(datatype,PSMX_ATOMIC_WRITE,
				dest,src,count,PSMX_MAX);
		break;

	case FI_SUM:
		SWITCH_ALL_TYPE(datatype,PSMX_ATOMIC_WRITE,
				dest,src,count,PSMX_SUM);
		break;

	case FI_PROD:
		SWITCH_ALL_TYPE(datatype,PSMX_ATOMIC_WRITE,
				dest,src,count,PSMX_PROD);
		break;

	case FI_LOR:
		SWITCH_INT_TYPE(datatype,PSMX_ATOMIC_WRITE,
				dest,src,count,PSMX_LOR);
		break;

	case FI_LAND:
		SWITCH_INT_TYPE(datatype,PSMX_ATOMIC_WRITE,
				dest,src,count,PSMX_LAND);
		break;

	case FI_BOR:
		SWITCH_INT_TYPE(datatype,PSMX_ATOMIC_WRITE,
				dest,src,count,PSMX_BOR);
		break;

	case FI_BAND:
		SWITCH_INT_TYPE(datatype,PSMX_ATOMIC_WRITE,
				dest,src,count,PSMX_BAND);
		break;

	case FI_LXOR:
		SWITCH_INT_TYPE(datatype,PSMX_ATOMIC_WRITE,
				dest,src,count,PSMX_LXOR);
		break;

	case FI_BXOR:
		SWITCH_INT_TYPE(datatype,PSMX_ATOMIC_WRITE,
				dest,src,count,PSMX_BXOR);
		break;

	case FI_ATOMIC_READ:
		/* do nothing */
		break;

	case FI_ATOMIC_WRITE:
		SWITCH_ALL_TYPE(datatype,PSMX_ATOMIC_WRITE,
				dest,src,count,PSMX_COPY);
		break;

	default:
		return -FI_EOPNOTSUPP;
	}

	return 0;
}

static int psmx_atomic_do_readwrite(void *dest, void *src, void *result,
				    int datatype, int op, int count)
{
	switch (op) {
	case FI_MIN:
		SWITCH_ORD_TYPE(datatype,PSMX_ATOMIC_READWRITE,
				dest,src,result,count,PSMX_MIN);
		break;

	case FI_MAX:
		SWITCH_ORD_TYPE(datatype,PSMX_ATOMIC_READWRITE,
				dest,src,result,count,PSMX_MAX);
		break;

	case FI_SUM:
		SWITCH_ALL_TYPE(datatype,PSMX_ATOMIC_READWRITE,
				dest,src,result,count,PSMX_SUM);
		break;

	case FI_PROD:
		SWITCH_ALL_TYPE(datatype,PSMX_ATOMIC_READWRITE,
				dest,src,result,count,PSMX_PROD);
		break;

	case FI_LOR:
		SWITCH_INT_TYPE(datatype,PSMX_ATOMIC_READWRITE,
				dest,src,result,count,PSMX_LOR);
		break;

	case FI_LAND:
		SWITCH_INT_TYPE(datatype,PSMX_ATOMIC_READWRITE,
				dest,src,result,count,PSMX_LAND);
		break;

	case FI_BOR:
		SWITCH_INT_TYPE(datatype,PSMX_ATOMIC_READWRITE,
				dest,src,result,count,PSMX_BOR);
		break;

	case FI_BAND:
		SWITCH_INT_TYPE(datatype,PSMX_ATOMIC_READWRITE,
				dest,src,result,count,PSMX_BAND);
		break;

	case FI_LXOR:
		SWITCH_INT_TYPE(datatype,PSMX_ATOMIC_READWRITE,
				dest,src,result,count,PSMX_LXOR);
		break;

	case FI_BXOR:
		SWITCH_INT_TYPE(datatype,PSMX_ATOMIC_READWRITE,
				dest,src,result,count,PSMX_BXOR);
		break;

	case FI_ATOMIC_READ:
		SWITCH_ALL_TYPE(datatype,PSMX_ATOMIC_READ,
				dest,result,count);
		break;

	case FI_ATOMIC_WRITE:
		SWITCH_ALL_TYPE(datatype,PSMX_ATOMIC_READWRITE,
				dest,src,result,count,PSMX_COPY);
		break;

	default:
		return -FI_EOPNOTSUPP;
	}

	return 0;
}

static int psmx_atomic_do_compwrite(void *dest, void *src, void *compare,
				    void *result, int datatype, int op,
				    int count)
{
	switch (op) {
	case FI_CSWAP:
		SWITCH_ALL_TYPE(datatype,PSMX_ATOMIC_CSWAP,
				dest,src,compare,result,count,==);
		break;

	case FI_CSWAP_NE:
		SWITCH_ALL_TYPE(datatype,PSMX_ATOMIC_CSWAP,
				dest,src,compare,result,count,!=);
		break;

	case FI_CSWAP_LE:
		SWITCH_ORD_TYPE(datatype,PSMX_ATOMIC_CSWAP,
				dest,src,compare,result,count,<=);
		break;

	case FI_CSWAP_LT:
		SWITCH_ORD_TYPE(datatype,PSMX_ATOMIC_CSWAP,
				dest,src,compare,result,count,<);
		break;

	case FI_CSWAP_GE:
		SWITCH_ORD_TYPE(datatype,PSMX_ATOMIC_CSWAP,
				dest,src,compare,result,count,>=);
		break;

	case FI_CSWAP_GT:
		SWITCH_ORD_TYPE(datatype,PSMX_ATOMIC_CSWAP,
				dest,src,compare,result,count,>);
		break;

	case FI_MSWAP:
		SWITCH_INT_TYPE(datatype,PSMX_ATOMIC_MSWAP,
				dest,src,compare,result,count);
		break;

	default:
		return -FI_EOPNOTSUPP;
	}

	return 0;
}

static void psmx_am_atomic_completion(void *buf)
{
	if (buf)
		free(buf);
}

int psmx_am_atomic_handler(psm_am_token_t token, psm_epaddr_t epaddr,
			   psm_amarg_t *args, int nargs, void *src,
			   uint32_t len)
{
	psm_amarg_t rep_args[8];
	int count;
	void *addr;
	uint64_t key;
	int datatype, op;
	int err = 0;
	int op_error = 0;
	struct psmx_am_request *req;
	struct psmx_cq_event *event;
	struct psmx_fid_mr *mr;
	struct psmx_fid_ep *target_ep;
	struct psmx_fid_cntr *cntr = NULL;
	struct psmx_fid_cntr *mr_cntr = NULL;
	void *tmp_buf;
	uint64_t cq_flags;

	switch (args[0].u32w0 & PSMX_AM_OP_MASK) {
	case PSMX_AM_REQ_ATOMIC_WRITE:
		count = args[0].u32w1;
		addr = (void *)(uintptr_t)args[2].u64;
		key = args[3].u64;
		datatype = args[4].u32w0;
		op = args[4].u32w1;
		assert(len == fi_datatype_size(datatype) * count);

		mr = psmx_mr_hash_get(key);
		op_error = mr ?
			psmx_mr_validate(mr, (uint64_t)addr, len, FI_REMOTE_WRITE) :
			-FI_EINVAL;

		if (!op_error) {
			addr += mr->offset;
			psmx_atomic_do_write(addr, src, datatype, op, count);

			target_ep = mr->domain->atomics_ep;
			cntr = target_ep->remote_write_cntr;
			mr_cntr = mr->cntr;

			if (cntr)
				psmx_cntr_inc(cntr);

			if (mr_cntr && mr_cntr != cntr)
				psmx_cntr_inc(mr_cntr);
		}

		rep_args[0].u32w0 = PSMX_AM_REP_ATOMIC_WRITE;
		rep_args[0].u32w1 = op_error;
		rep_args[1].u64 = args[1].u64;
		err = psm_am_reply_short(token, PSMX_AM_ATOMIC_HANDLER,
				rep_args, 2, NULL, 0, 0,
				NULL, NULL );
		break;

	case PSMX_AM_REQ_ATOMIC_READWRITE:
		count = args[0].u32w1;
		addr = (void *)(uintptr_t)args[2].u64;
		key = args[3].u64;
		datatype = args[4].u32w0;
		op = args[4].u32w1;
		cq_flags = FI_REMOTE_WRITE | FI_ATOMIC;

		if (op == FI_ATOMIC_READ) {
			len = fi_datatype_size(datatype) * count;
			cq_flags = FI_REMOTE_READ | FI_ATOMIC;
		}

		assert(len == fi_datatype_size(datatype) * count);

		mr = psmx_mr_hash_get(key);
		op_error = mr ?
			psmx_mr_validate(mr, (uint64_t)addr, len, FI_REMOTE_READ|FI_REMOTE_WRITE) :
			-FI_EINVAL;

		if (!op_error) {
			addr += mr->offset;
			tmp_buf = malloc(len);
			if (tmp_buf)
				psmx_atomic_do_readwrite(addr, src, tmp_buf,
							 datatype, op, count);
			else
				err = -FI_ENOMEM;

			target_ep = mr->domain->atomics_ep;
			if (op == FI_ATOMIC_READ) {
				cntr = target_ep->remote_read_cntr;
			}
			else {
				cntr = target_ep->remote_write_cntr;
				mr_cntr = mr->cntr;
			}

			if (cntr)
				psmx_cntr_inc(cntr);

			if (mr_cntr && mr_cntr != cntr)
				psmx_cntr_inc(mr_cntr);
		}
		else {
			tmp_buf = NULL;
		}

		rep_args[0].u32w0 = PSMX_AM_REP_ATOMIC_READWRITE;
		rep_args[0].u32w1 = op_error;
		rep_args[1].u64 = args[1].u64;
		err = psm_am_reply_short(token, PSMX_AM_ATOMIC_HANDLER,
				rep_args, 2, tmp_buf, (tmp_buf?len:0), 0,
				psmx_am_atomic_completion, tmp_buf );
		break;

	case PSMX_AM_REQ_ATOMIC_COMPWRITE:
		count = args[0].u32w1;
		addr = (void *)(uintptr_t)args[2].u64;
		key = args[3].u64;
		datatype = args[4].u32w0;
		op = args[4].u32w1;
		len /= 2;
		assert(len == fi_datatype_size(datatype) * count);

		mr = psmx_mr_hash_get(key);
		op_error = mr ?
			psmx_mr_validate(mr, (uint64_t)addr, len, FI_REMOTE_READ|FI_REMOTE_WRITE) :
			-FI_EINVAL;

		if (!op_error) {
			addr += mr->offset;
			tmp_buf = malloc(len);
			if (tmp_buf)
				psmx_atomic_do_compwrite(addr, src, src + len,
							 tmp_buf, datatype, op, count);
			else
				err = -FI_ENOMEM;

			target_ep = mr->domain->atomics_ep;
			cntr = target_ep->remote_write_cntr;
			mr_cntr = mr->cntr;

			if (cntr)
				psmx_cntr_inc(cntr);

			if (mr_cntr && mr_cntr != cntr)
				psmx_cntr_inc(mr_cntr);
		}
		else {
			tmp_buf = NULL;
		}

		rep_args[0].u32w0 = PSMX_AM_REP_ATOMIC_READWRITE;
		rep_args[0].u32w1 = op_error;
		rep_args[1].u64 = args[1].u64;
		err = psm_am_reply_short(token, PSMX_AM_ATOMIC_HANDLER,
				rep_args, 2, tmp_buf, (tmp_buf?len:0), 0,
				psmx_am_atomic_completion, tmp_buf );
		break;

	case PSMX_AM_REP_ATOMIC_WRITE:
		req = (struct psmx_am_request *)(uintptr_t)args[1].u64;
		op_error = (int)args[0].u32w1;
		assert(req->op == PSMX_AM_REQ_ATOMIC_WRITE);
		if (req->ep->send_cq && !req->no_event) {
			event = psmx_cq_create_event(
					req->ep->send_cq,
					req->atomic.context,
					req->atomic.buf,
					req->cq_flags,
					req->atomic.len,
					0, /* data */
					0, /* tag */
					0, /* olen */
					op_error);
			if (event)
				psmx_cq_enqueue_event(req->ep->send_cq, event);
			else
				err = -FI_ENOMEM;
		}

		if (req->ep->write_cntr)
			psmx_cntr_inc(req->ep->write_cntr);

		free(req);
		break;

	case PSMX_AM_REP_ATOMIC_READWRITE:
	case PSMX_AM_REP_ATOMIC_COMPWRITE:
		req = (struct psmx_am_request *)(uintptr_t)args[1].u64;
		op_error = (int)args[0].u32w1;
		assert(op_error || req->atomic.len == len);

		if (!op_error)
			memcpy(req->atomic.result, src, len);

		if (req->ep->send_cq && !req->no_event) {
			event = psmx_cq_create_event(
					req->ep->send_cq,
					req->atomic.context,
					req->atomic.buf,
					req->cq_flags,
					req->atomic.len,
					0, /* data */
					0, /* tag */
					0, /* olen */
					op_error);
			if (event)
				psmx_cq_enqueue_event(req->ep->send_cq, event);
			else
				err = -FI_ENOMEM;
		}

		if (req->ep->read_cntr)
			psmx_cntr_inc(req->ep->read_cntr);

		free(req);
		break;

	default:
		err = -FI_EINVAL;
	}
	return err;
}

static int psmx_atomic_self(int am_cmd,
			    struct psmx_fid_ep *ep,
			    const void *buf,
			    size_t count, void *desc,
			    const void *compare, void *compare_desc,
			    void *result, void *result_desc,
			    uint64_t addr, uint64_t key,
			    enum fi_datatype datatype,
			    enum fi_op op, void *context,
			    uint64_t flags)
{
	struct psmx_fid_mr *mr;
	struct psmx_cq_event *event;
	struct psmx_fid_ep *target_ep;
	struct psmx_fid_cntr *cntr = NULL;
	struct psmx_fid_cntr *mr_cntr = NULL;
	size_t len;
	int no_event;
	int err = 0;
	int op_error;
	int access;
	uint64_t cq_flags = 0;

	if (am_cmd == PSMX_AM_REQ_ATOMIC_WRITE)
		access = FI_REMOTE_WRITE;
	else
		access = FI_REMOTE_READ | FI_REMOTE_WRITE;

	len = fi_datatype_size(datatype) * count;
	mr = psmx_mr_hash_get(key);
	op_error = mr ?  psmx_mr_validate(mr, addr, len, access) : -FI_EINVAL;

	if (op_error)
		goto gen_local_event;

	addr += mr->offset;

	switch (am_cmd) {
	case PSMX_AM_REQ_ATOMIC_WRITE:
		err = psmx_atomic_do_write((void *)addr, (void *)buf,
					   (int)datatype, (int)op, (int)count);
		cq_flags = FI_WRITE | FI_ATOMIC;
		break;

	case PSMX_AM_REQ_ATOMIC_READWRITE:
		err = psmx_atomic_do_readwrite((void *)addr, (void *)buf,
					       (void *)result, (int)datatype,
					       (int)op, (int)count);
		if (op == FI_ATOMIC_READ)
			cq_flags = FI_READ | FI_ATOMIC;
		else
			cq_flags = FI_WRITE | FI_ATOMIC;
		break;

	case PSMX_AM_REQ_ATOMIC_COMPWRITE:
		err = psmx_atomic_do_compwrite((void *)addr, (void *)buf,
					       (void *)compare, (void *)result,
					       (int)datatype, (int)op, (int)count);
		cq_flags = FI_WRITE | FI_ATOMIC;
		break;
	}

	target_ep = mr->domain->atomics_ep;
	if (op == FI_ATOMIC_READ) {
		cntr = target_ep->remote_read_cntr;
	}
	else {
		cntr = target_ep->remote_write_cntr;
		mr_cntr = mr->cntr;
	}

	if (cntr)
		psmx_cntr_inc(cntr);

	if (mr_cntr && mr_cntr != cntr)
		psmx_cntr_inc(mr_cntr);

gen_local_event:
	no_event = ((flags & PSMX_NO_COMPLETION) ||
		    (ep->send_cq_event_flag && !(flags & FI_COMPLETION)));
	if (ep->send_cq && !no_event) {
		event = psmx_cq_create_event(
				ep->send_cq,
				context,
				(void *)buf,
				cq_flags,
				len,
				0, /* data */
				0, /* tag */
				0, /* olen */
				op_error);
		if (event)
			psmx_cq_enqueue_event(ep->send_cq, event);
		else
			err = -FI_ENOMEM;
	}

	switch (am_cmd) {
	case PSMX_AM_REQ_ATOMIC_WRITE:
		if (ep->write_cntr)
			psmx_cntr_inc(ep->write_cntr);
		break;
	case PSMX_AM_REQ_ATOMIC_READWRITE:
	case PSMX_AM_REQ_ATOMIC_COMPWRITE:
		if (ep->read_cntr)
			psmx_cntr_inc(ep->read_cntr);
		break;
	}

	return err;
}

ssize_t _psmx_atomic_write(struct fid_ep *ep,
			   const void *buf,
			   size_t count, void *desc,
			   fi_addr_t dest_addr,
			   uint64_t addr, uint64_t key,
			   enum fi_datatype datatype,
			   enum fi_op op, void *context,
			   uint64_t flags)
{
	struct psmx_fid_ep *ep_priv;
	struct psmx_fid_av *av;
	struct psmx_epaddr_context *epaddr_context;
	struct psmx_am_request *req;
	psm_amarg_t args[8];
	int am_flags = PSM_AM_FLAG_ASYNC;
	int chunk_size, len;
	size_t idx;

	ep_priv = container_of(ep, struct psmx_fid_ep, ep);

	if (flags & FI_TRIGGER) {
		struct psmx_trigger *trigger;
		struct fi_triggered_context *ctxt = context;

		trigger = calloc(1, sizeof(*trigger));
		if (!trigger)
			return -FI_ENOMEM;

		trigger->op = PSMX_TRIGGERED_ATOMIC_WRITE;
		trigger->cntr = container_of(ctxt->trigger.threshold.cntr,
					     struct psmx_fid_cntr, cntr);
		trigger->threshold = ctxt->trigger.threshold.threshold;
		trigger->atomic_write.ep = ep;
		trigger->atomic_write.buf = buf;
		trigger->atomic_write.count = count;
		trigger->atomic_write.desc = desc;
		trigger->atomic_write.dest_addr = dest_addr;
		trigger->atomic_write.addr = addr;
		trigger->atomic_write.key = key;
		trigger->atomic_write.datatype = datatype;
		trigger->atomic_write.atomic_op = op;
		trigger->atomic_write.context = context;
		trigger->atomic_write.flags = flags & ~FI_TRIGGER;

		psmx_cntr_add_trigger(trigger->cntr, trigger);
		return 0;
	}

	if (!buf)
		return -FI_EINVAL;

	if (datatype < 0 || datatype >= FI_DATATYPE_LAST)
		return -FI_EINVAL;

	if (op < 0 || op >= FI_ATOMIC_OP_LAST)
		return -FI_EINVAL;

	av = ep_priv->av;
	if (av && av->type == FI_AV_TABLE) {
		idx = dest_addr;
		if (idx >= av->last)
			return -FI_EINVAL;

		dest_addr = (fi_addr_t) av->psm_epaddrs[idx];
	}
	else if (!dest_addr) {
		return -FI_EINVAL;
	}

	epaddr_context = psm_epaddr_getctxt((void *)dest_addr);
	if (epaddr_context->epid == ep_priv->domain->psm_epid)
		return psmx_atomic_self(PSMX_AM_REQ_ATOMIC_WRITE,
					ep_priv, buf, count, desc,
					NULL, NULL, NULL, NULL,
					addr, key, datatype, op,
					context, flags);

	chunk_size = MIN(PSMX_AM_CHUNK_SIZE, psmx_am_param.max_request_short);
	len = fi_datatype_size(datatype)* count;
	if (len > chunk_size)
		return -FI_EMSGSIZE;

	if (flags & FI_INJECT) {
		req = malloc(sizeof(*req) + len);
		memset((void *)req, 0, sizeof(*req));
		memcpy((void *)req+sizeof(*req), (void *)buf, len);
		buf = (void *)req + sizeof(*req);
	}
	else {
		req = calloc(1, sizeof(*req));
		if (!req)
			return -FI_ENOMEM;
	}

	req->no_event = (flags & PSMX_NO_COMPLETION) ||
			(ep_priv->send_cq_event_flag && !(flags & FI_COMPLETION));

	req->op = PSMX_AM_REQ_ATOMIC_WRITE;
	req->atomic.buf = (void *)buf;
	req->atomic.len = len;
	req->atomic.addr = addr;
	req->atomic.key = key;
	req->atomic.context = context;
	req->ep = ep_priv;
	req->cq_flags = FI_WRITE | FI_ATOMIC;

	args[0].u32w0 = PSMX_AM_REQ_ATOMIC_WRITE;
	args[0].u32w1 = count;
	args[1].u64 = (uint64_t)(uintptr_t)req;
	args[2].u64 = addr;
	args[3].u64 = key;
	args[4].u32w0 = datatype;
	args[4].u32w1 = op;
	psm_am_request_short((psm_epaddr_t) dest_addr,
				PSMX_AM_ATOMIC_HANDLER, args, 5,
				(void *)buf, len, am_flags, NULL, NULL);

	return 0;
}

static ssize_t psmx_atomic_write(struct fid_ep *ep,
			       const void *buf,
			       size_t count, void *desc,
			       fi_addr_t dest_addr,
			       uint64_t addr, uint64_t key,
			       enum fi_datatype datatype,
			       enum fi_op op, void *context)
{
	struct psmx_fid_ep *ep_priv;

	ep_priv = container_of(ep, struct psmx_fid_ep, ep);
	return _psmx_atomic_write(ep, buf, count, desc,
				  dest_addr, addr, key,
				  datatype, op, context, ep_priv->flags);
}

static ssize_t psmx_atomic_writemsg(struct fid_ep *ep,
				const struct fi_msg_atomic *msg,
				uint64_t flags)
{
	if (!msg || msg->iov_count != 1)
		return -FI_EINVAL;

	return _psmx_atomic_write(ep, msg->msg_iov[0].addr,
				  msg->msg_iov[0].count,
				  msg->desc ? msg->desc[0] : NULL,
				  msg->addr, msg->rma_iov[0].addr,
				  msg->rma_iov[0].key, msg->datatype,
				  msg->op, msg->context, flags);
}

static ssize_t psmx_atomic_writev(struct fid_ep *ep,
			      const struct fi_ioc *iov,
			      void **desc, size_t count,
			      fi_addr_t dest_addr,
			      uint64_t addr, uint64_t key,
			      enum fi_datatype datatype,
			      enum fi_op op, void *context)
{
	if (!iov || count != 1)
		return -FI_EINVAL;

	return psmx_atomic_write(ep, iov->addr, iov->count,
				 desc ? desc[0] : NULL, dest_addr, addr, key,
				 datatype, op, context);
}

static ssize_t psmx_atomic_inject(struct fid_ep *ep,
			       const void *buf,
			       size_t count, /*void *desc,*/
			       fi_addr_t dest_addr,
			       uint64_t addr, uint64_t key,
			       enum fi_datatype datatype,
			       enum fi_op op)
{
	struct psmx_fid_ep *ep_priv;

	ep_priv = container_of(ep, struct psmx_fid_ep, ep);
	return _psmx_atomic_write(ep, buf, count, NULL/*desc*/,
				  dest_addr, addr, key,
				  datatype, op, NULL,
				  ep_priv->flags | FI_INJECT | PSMX_NO_COMPLETION);
}

ssize_t _psmx_atomic_readwrite(struct fid_ep *ep,
				const void *buf,
				size_t count, void *desc,
				void *result, void *result_desc,
				fi_addr_t dest_addr,
				uint64_t addr, uint64_t key,
				enum fi_datatype datatype,
				enum fi_op op, void *context,
				uint64_t flags)
{
	struct psmx_fid_ep *ep_priv;
	struct psmx_fid_av *av;
	struct psmx_epaddr_context *epaddr_context;
	struct psmx_am_request *req;
	psm_amarg_t args[8];
	int am_flags = PSM_AM_FLAG_ASYNC;
	int chunk_size, len;
	size_t idx;

	ep_priv = container_of(ep, struct psmx_fid_ep, ep);

	if (flags & FI_TRIGGER) {
		struct psmx_trigger *trigger;
		struct fi_triggered_context *ctxt = context;

		trigger = calloc(1, sizeof(*trigger));
		if (!trigger)
			return -FI_ENOMEM;

		trigger->op = PSMX_TRIGGERED_ATOMIC_READWRITE;
		trigger->cntr = container_of(ctxt->trigger.threshold.cntr,
					     struct psmx_fid_cntr, cntr);
		trigger->threshold = ctxt->trigger.threshold.threshold;
		trigger->atomic_readwrite.ep = ep;
		trigger->atomic_readwrite.buf = buf;
		trigger->atomic_readwrite.count = count;
		trigger->atomic_readwrite.desc = desc;
		trigger->atomic_readwrite.result = result;
		trigger->atomic_readwrite.result_desc = result_desc;
		trigger->atomic_readwrite.dest_addr = dest_addr;
		trigger->atomic_readwrite.addr = addr;
		trigger->atomic_readwrite.key = key;
		trigger->atomic_readwrite.datatype = datatype;
		trigger->atomic_readwrite.atomic_op = op;
		trigger->atomic_readwrite.context = context;
		trigger->atomic_readwrite.flags = flags & ~FI_TRIGGER;

		psmx_cntr_add_trigger(trigger->cntr, trigger);
		return 0;
	}

	if (!buf && op != FI_ATOMIC_READ)
		return -FI_EINVAL;

	if (datatype < 0 || datatype >= FI_DATATYPE_LAST)
		return -FI_EINVAL;

	if (op < 0 || op >= FI_ATOMIC_OP_LAST)
		return -FI_EINVAL;

	av = ep_priv->av;
	if (av && av->type == FI_AV_TABLE) {
		idx = dest_addr;
		if (idx >= av->last)
			return -FI_EINVAL;

		dest_addr = (fi_addr_t) av->psm_epaddrs[idx];
	}
	else if (!dest_addr) {
		return -FI_EINVAL;
	}

	epaddr_context = psm_epaddr_getctxt((void *)dest_addr);
	if (epaddr_context->epid == ep_priv->domain->psm_epid)
		return psmx_atomic_self(PSMX_AM_REQ_ATOMIC_READWRITE,
					ep_priv, buf, count, desc,
					NULL, NULL, result, result_desc,
					addr, key, datatype, op,
					context, flags);

	chunk_size = MIN(PSMX_AM_CHUNK_SIZE, psmx_am_param.max_request_short);
	len = fi_datatype_size(datatype) * count;
	if (len > chunk_size)
		return -FI_EMSGSIZE;

	if (flags & FI_INJECT) {
		req = malloc(sizeof(*req) + len);
		memset((void *)req, 0, sizeof(*req));
		memcpy((void *)req+sizeof(*req), (void *)buf, len);
		buf = (void *)req + sizeof(*req);
	}
	else {
		req = calloc(1, sizeof(*req));
		if (!req)
			return -FI_ENOMEM;
	}

	req->no_event = (flags & PSMX_NO_COMPLETION) ||
			(ep_priv->send_cq_event_flag && !(flags & FI_COMPLETION));

	req->op = PSMX_AM_REQ_ATOMIC_READWRITE;
	req->atomic.buf = (void *)buf;
	req->atomic.len = len;
	req->atomic.addr = addr;
	req->atomic.key = key;
	req->atomic.context = context;
	req->atomic.result = result;
	req->ep = ep_priv;
	if (op == FI_ATOMIC_READ)
		req->cq_flags = FI_READ | FI_ATOMIC;
	else
		req->cq_flags = FI_WRITE | FI_ATOMIC;

	args[0].u32w0 = PSMX_AM_REQ_ATOMIC_READWRITE;
	args[0].u32w1 = count;
	args[1].u64 = (uint64_t)(uintptr_t)req;
	args[2].u64 = addr;
	args[3].u64 = key;
	args[4].u32w0 = datatype;
	args[4].u32w1 = op;
	psm_am_request_short((psm_epaddr_t) dest_addr,
				PSMX_AM_ATOMIC_HANDLER, args, 5,
				(void *)buf, (buf?len:0), am_flags, NULL, NULL);

	return 0;
}

static ssize_t psmx_atomic_readwrite(struct fid_ep *ep,
				   const void *buf,
				   size_t count, void *desc,
				   void *result, void *result_desc,
				   fi_addr_t dest_addr,
				   uint64_t addr, uint64_t key,
				   enum fi_datatype datatype,
				   enum fi_op op, void *context)
{
	struct psmx_fid_ep *ep_priv;

	ep_priv = container_of(ep, struct psmx_fid_ep, ep);
	return _psmx_atomic_readwrite(ep, buf, count, desc,
					result, result_desc, dest_addr,
					addr, key, datatype, op,
					context, ep_priv->flags);
}

static ssize_t psmx_atomic_readwritemsg(struct fid_ep *ep,
				    const struct fi_msg_atomic *msg,
				    struct fi_ioc *resultv,
				    void **result_desc,
				    size_t result_count,
				    uint64_t flags)
{
	void *buf;
	size_t count;

	if (!msg)
		return -FI_EINVAL;

	if (msg->op == FI_ATOMIC_READ) {
		if (result_count != 1)
			return -FI_EINVAL;

		buf = NULL;
		count = resultv[0].count;
	}
	else {
		if (msg->iov_count != 1)
			return -FI_EINVAL;

		buf = msg->msg_iov[0].addr;
		count = msg->msg_iov[0].count;
	}

	return _psmx_atomic_readwrite(ep, buf, count,
					msg->desc ? msg->desc[0] : NULL,
					resultv[0].addr,
					result_desc ? result_desc[0] : NULL,
					msg->addr, msg->rma_iov[0].addr,
					msg->rma_iov[0].key, msg->datatype,
					msg->op, msg->context, flags);
}

static ssize_t psmx_atomic_readwritev(struct fid_ep *ep,
				  const struct fi_ioc *iov,
				  void **desc, size_t count,
				  struct fi_ioc *resultv,
				  void **result_desc, size_t result_count,
				  fi_addr_t dest_addr,
				  uint64_t addr, uint64_t key,
				  enum fi_datatype datatype,
				  enum fi_op op, void *context)
{
	if (!iov || count != 1)
		return -FI_EINVAL;

	return psmx_atomic_readwrite(ep, iov->addr, iov->count,
				     desc ? desc[0] : NULL,
				     resultv[0].addr,
				     result_desc ? result_desc[0] : NULL,
				     dest_addr, addr, key, datatype, op, context);
}

ssize_t _psmx_atomic_compwrite(struct fid_ep *ep,
				const void *buf,
				size_t count, void *desc,
				const void *compare, void *compare_desc,
				void *result, void *result_desc,
				fi_addr_t dest_addr,
				uint64_t addr, uint64_t key,
				enum fi_datatype datatype,
				enum fi_op op, void *context,
				uint64_t flags)
{
	struct psmx_fid_ep *ep_priv;
	struct psmx_fid_av *av;
	struct psmx_epaddr_context *epaddr_context;
	struct psmx_am_request *req;
	psm_amarg_t args[8];
	int am_flags = PSM_AM_FLAG_ASYNC;
	int chunk_size, len;
	void *tmp_buf = NULL;
	size_t idx;

	ep_priv = container_of(ep, struct psmx_fid_ep, ep);

	if (flags & FI_TRIGGER) {
		struct psmx_trigger *trigger;
		struct fi_triggered_context *ctxt = context;

		trigger = calloc(1, sizeof(*trigger));
		if (!trigger)
			return -FI_ENOMEM;

		trigger->op = PSMX_TRIGGERED_ATOMIC_COMPWRITE;
		trigger->cntr = container_of(ctxt->trigger.threshold.cntr,
					     struct psmx_fid_cntr, cntr);
		trigger->threshold = ctxt->trigger.threshold.threshold;
		trigger->atomic_compwrite.ep = ep;
		trigger->atomic_compwrite.buf = buf;
		trigger->atomic_compwrite.count = count;
		trigger->atomic_compwrite.desc = desc;
		trigger->atomic_compwrite.compare = compare;
		trigger->atomic_compwrite.compare_desc = compare_desc;
		trigger->atomic_compwrite.result = result;
		trigger->atomic_compwrite.result_desc = result_desc;
		trigger->atomic_compwrite.dest_addr = dest_addr;
		trigger->atomic_compwrite.addr = addr;
		trigger->atomic_compwrite.key = key;
		trigger->atomic_compwrite.datatype = datatype;
		trigger->atomic_compwrite.atomic_op = op;
		trigger->atomic_compwrite.context = context;
		trigger->atomic_compwrite.flags = flags & ~FI_TRIGGER;

		psmx_cntr_add_trigger(trigger->cntr, trigger);
		return 0;
	}

	if (!buf)
		return -FI_EINVAL;

	if (datatype < 0 || datatype >= FI_DATATYPE_LAST)
		return -FI_EINVAL;

	if (op < 0 || op >= FI_ATOMIC_OP_LAST)
		return -FI_EINVAL;

	av = ep_priv->av;
	if (av && av->type == FI_AV_TABLE) {
		idx = dest_addr;
		if (idx >= av->last)
			return -FI_EINVAL;

		dest_addr = (fi_addr_t) av->psm_epaddrs[idx];
	}
	else if (!dest_addr) {
		return -FI_EINVAL;
	}

	epaddr_context = psm_epaddr_getctxt((void *)dest_addr);
	if (epaddr_context->epid == ep_priv->domain->psm_epid)
		return psmx_atomic_self(PSMX_AM_REQ_ATOMIC_COMPWRITE,
					ep_priv, buf, count, desc,
					compare, compare_desc,
					result, result_desc,
					addr, key, datatype, op,
					context, flags);

	chunk_size = MIN(PSMX_AM_CHUNK_SIZE, psmx_am_param.max_request_short);
	len = fi_datatype_size(datatype) * count;
	if (len * 2 > chunk_size)
		return -FI_EMSGSIZE;

	if (flags & FI_INJECT) {
		req = malloc(sizeof(*req) + len + len);
		memset((void *)req, 0, sizeof(*req));
		memcpy((void *)req + sizeof(*req), (void *)buf, len);
		memcpy((void *)req + sizeof(*req) + len, (void *)compare, len);
		buf = (void *)req + sizeof(*req);
		compare = buf + len;
	}
	else {
		req = calloc(1, sizeof(*req));
		if (!req)
			return -FI_ENOMEM;

		if (compare != buf + len) {
			tmp_buf = malloc(len * 2);
			if (!tmp_buf)
				return -FI_ENOMEM;

			memcpy(tmp_buf, buf, len);
			memcpy(tmp_buf + len, compare, len);
		}
	}

	req->no_event = (flags & PSMX_NO_COMPLETION) ||
			(ep_priv->send_cq_event_flag && !(flags & FI_COMPLETION));

	req->op = PSMX_AM_REQ_ATOMIC_COMPWRITE;
	req->atomic.buf = (void *)buf;
	req->atomic.len = len;
	req->atomic.addr = addr;
	req->atomic.key = key;
	req->atomic.context = context;
	req->atomic.result = result;
	req->ep = ep_priv;
	req->cq_flags = FI_WRITE | FI_ATOMIC;

	args[0].u32w0 = PSMX_AM_REQ_ATOMIC_COMPWRITE;
	args[0].u32w1 = count;
	args[1].u64 = (uint64_t)(uintptr_t)req;
	args[2].u64 = addr;
	args[3].u64 = key;
	args[4].u32w0 = datatype;
	args[4].u32w1 = op;
	psm_am_request_short((psm_epaddr_t) dest_addr,
				PSMX_AM_ATOMIC_HANDLER, args, 5,
				tmp_buf ? tmp_buf : (void *)buf,
				len * 2, am_flags,
				psmx_am_atomic_completion, tmp_buf);

	return 0;
}

static ssize_t psmx_atomic_compwrite(struct fid_ep *ep,
				   const void *buf,
				   size_t count, void *desc,
				   const void *compare, void *compare_desc,
				   void *result, void *result_desc,
				   fi_addr_t dest_addr,
				   uint64_t addr, uint64_t key,
				   enum fi_datatype datatype,
				   enum fi_op op, void *context)
{
	struct psmx_fid_ep *ep_priv;

	ep_priv = container_of(ep, struct psmx_fid_ep, ep);
	return _psmx_atomic_compwrite(ep, buf, count, desc,
					compare, compare_desc,
					result, result_desc,
					dest_addr, addr, key,
				        datatype, op, context, ep_priv->flags);
}

static ssize_t psmx_atomic_compwritemsg(struct fid_ep *ep,
				    const struct fi_msg_atomic *msg,
				    const struct fi_ioc *comparev,
				    void **compare_desc,
				    size_t compare_count,
				    struct fi_ioc *resultv,
				    void **result_desc,
				    size_t result_count,
				    uint64_t flags)
{
	if (!msg || msg->iov_count != 1)
		return -FI_EINVAL;

	return _psmx_atomic_compwrite(ep, msg->msg_iov[0].addr,
					msg->msg_iov[0].count,
					msg->desc ? msg->desc[0] : NULL,
					comparev[0].addr,
					compare_desc ? compare_desc[0] : NULL,
					resultv[0].addr,
					result_desc ? result_desc[0] : NULL,
					msg->addr, msg->rma_iov[0].addr,
					msg->rma_iov[0].key, msg->datatype,
					msg->op, msg->context, flags);
}

static ssize_t psmx_atomic_compwritev(struct fid_ep *ep,
				  const struct fi_ioc *iov,
				  void **desc, size_t count,
				  const struct fi_ioc *comparev,
				  void **compare_desc,
				  size_t compare_count,
				  struct fi_ioc *resultv,
				  void **result_desc,
				  size_t result_count,
				  fi_addr_t dest_addr,
				  uint64_t addr, uint64_t key,
				  enum fi_datatype datatype,
				  enum fi_op op, void *context)
{
	if (!iov || count != 1)
		return -FI_EINVAL;

	return psmx_atomic_compwrite(ep, iov->addr, iov->count,
				     desc ? desc[0] : NULL,
				     comparev[0].addr,
				     compare_desc ? compare_desc[0] : NULL,
				     resultv[0].addr,
				     result_desc ? result_desc[0] : NULL,
				     dest_addr, addr, key, datatype, op, context);
}

static int psmx_atomic_writevalid(struct fid_ep *ep,
				  enum fi_datatype datatype,
				  enum fi_op op, size_t *count)
{
	int chunk_size;

	if (datatype < 0 || datatype >= FI_DATATYPE_LAST)
		return -FI_EOPNOTSUPP;

	switch (op) {
	case FI_MIN:
	case FI_MAX:
	case FI_SUM:
	case FI_PROD:
	case FI_LOR:
	case FI_LAND:
	case FI_BOR:
	case FI_BAND:
	case FI_LXOR:
	case FI_BXOR:
	case FI_ATOMIC_READ:
	case FI_ATOMIC_WRITE:
		break;

	default:
		return -FI_EOPNOTSUPP;
	}

	if (count) {
		chunk_size = MIN(PSMX_AM_CHUNK_SIZE,
				 psmx_am_param.max_request_short);
		*count = chunk_size / fi_datatype_size(datatype);
	}
	return 0;
}

static int psmx_atomic_readwritevalid(struct fid_ep *ep,
				      enum fi_datatype datatype,
				      enum fi_op op, size_t *count)
{
	int chunk_size;

	if (datatype < 0 || datatype >= FI_DATATYPE_LAST)
		return -FI_EOPNOTSUPP;

	switch (op) {
	case FI_MIN:
	case FI_MAX:
	case FI_SUM:
	case FI_PROD:
	case FI_LOR:
	case FI_LAND:
	case FI_BOR:
	case FI_BAND:
	case FI_LXOR:
	case FI_BXOR:
	case FI_ATOMIC_READ:
	case FI_ATOMIC_WRITE:
		break;

	default:
		return -FI_EOPNOTSUPP;
	}

	if (count) {
		chunk_size = MIN(PSMX_AM_CHUNK_SIZE,
				 psmx_am_param.max_request_short);
		*count = chunk_size / fi_datatype_size(datatype);
	}
	return 0;
}

static int psmx_atomic_compwritevalid(struct fid_ep *ep,
				      enum fi_datatype datatype,
				      enum fi_op op, size_t *count)
{
	int chunk_size;

	if (datatype < 0 || datatype >= FI_DATATYPE_LAST)
		return -FI_EOPNOTSUPP;

	switch (op) {
	case FI_CSWAP:
	case FI_CSWAP_NE:
		break;

	case FI_CSWAP_LE:
	case FI_CSWAP_LT:
	case FI_CSWAP_GE:
	case FI_CSWAP_GT:
		if (datatype == FI_FLOAT_COMPLEX ||
		    datatype == FI_DOUBLE_COMPLEX ||
		    datatype == FI_LONG_DOUBLE_COMPLEX)
			return -FI_EOPNOTSUPP;
		break;

	case FI_MSWAP:
		if (datatype == FI_FLOAT_COMPLEX ||
		    datatype == FI_DOUBLE_COMPLEX ||
		    datatype == FI_LONG_DOUBLE_COMPLEX ||
		    datatype == FI_FLOAT ||
		    datatype == FI_DOUBLE ||
		    datatype == FI_LONG_DOUBLE)
			return -FI_EOPNOTSUPP;
		break;

	default:
		return -FI_EOPNOTSUPP;
	}

	if (count) {
		chunk_size = MIN(PSMX_AM_CHUNK_SIZE,
				 psmx_am_param.max_request_short);
		*count = chunk_size / (2 * fi_datatype_size(datatype));
	}
	return 0;
}

struct fi_ops_atomic psmx_atomic_ops = {
	.size = sizeof(struct fi_ops_atomic),
	.write = psmx_atomic_write,
	.writev = psmx_atomic_writev,
	.writemsg = psmx_atomic_writemsg,
	.inject = psmx_atomic_inject,
	.readwrite = psmx_atomic_readwrite,
	.readwritev = psmx_atomic_readwritev,
	.readwritemsg = psmx_atomic_readwritemsg,
	.compwrite = psmx_atomic_compwrite,
	.compwritev = psmx_atomic_compwritev,
	.compwritemsg = psmx_atomic_compwritemsg,
	.writevalid = psmx_atomic_writevalid,
	.readwritevalid = psmx_atomic_readwritevalid,
	.compwritevalid = psmx_atomic_compwritevalid,
};

