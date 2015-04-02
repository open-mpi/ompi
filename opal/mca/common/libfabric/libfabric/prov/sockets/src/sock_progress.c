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

#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <netdb.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <poll.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/select.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/time.h>
#include <unistd.h>
#include <pthread.h>
#include <inttypes.h>

#include "sock.h"
#include "sock_util.h"

#define SOCK_LOG_INFO(...) _SOCK_LOG_INFO(FI_LOG_EP_DATA, __VA_ARGS__)
#define SOCK_LOG_ERROR(...) _SOCK_LOG_ERROR(FI_LOG_EP_DATA, __VA_ARGS__)

#define PE_INDEX(_pe, _e) (_e - &_pe->pe_table[0])
#define SOCK_GET_RX_ID(_addr, _bits) ((_bits) == 0) ? 0 : \
	(((uint64_t)_addr) >> (64 - _bits))

static inline ssize_t sock_pe_send_field(struct sock_pe_entry *pe_entry,
					 void * field, size_t field_len, 
					 size_t start_offset)
{
	int ret;
	size_t offset, data_len;
	
	if (pe_entry->done_len >= start_offset + field_len)
		return 0;

	offset = pe_entry->done_len - start_offset;
	data_len = field_len - offset;
	ret = sock_comm_send(pe_entry->conn, (char*)field + offset, data_len);
	
	if (ret <= 0)
		return -1;
	
	pe_entry->done_len += ret;
	return (ret == data_len) ? 0 : -1;
}

static inline ssize_t sock_pe_recv_field(struct sock_pe_entry *pe_entry,
					 void * field, size_t field_len, 
					 size_t start_offset)
{
	int ret;
	size_t offset, data_len;

	if (pe_entry->done_len >= start_offset + field_len)
		return 0;

	offset = pe_entry->done_len - start_offset;
	data_len = field_len - offset;
	ret = sock_comm_recv(pe_entry->conn, (char*)field + offset, data_len);
	if (ret <= 0)
		return -1;
	
	pe_entry->done_len += ret;
	return (ret == data_len) ? 0 : -1;
}

static void sock_pe_release_entry(struct sock_pe *pe, 
				  struct sock_pe_entry *pe_entry)
{
	dlist_remove(&pe_entry->ctx_entry);

	if (pe_entry->type == SOCK_PE_TX) {
		if (pe_entry->conn->tx_pe_entry == pe_entry)
			pe_entry->conn->tx_pe_entry = NULL;
	} else {
		if (pe_entry->conn->rx_pe_entry == pe_entry)
			pe_entry->conn->rx_pe_entry = NULL;
	}

	pe->num_free_entries++;
	pe_entry->conn = NULL;

	memset(&pe_entry->pe.rx, 0, sizeof(pe_entry->pe.rx));
	memset(&pe_entry->pe.tx, 0, sizeof(pe_entry->pe.tx));
	memset(&pe_entry->msg_hdr, 0, sizeof(pe_entry->msg_hdr));
	memset(&pe_entry->response, 0, sizeof(pe_entry->response));

	pe_entry->type =0;
	pe_entry->is_complete = 0;
	pe_entry->done_len = 0;
	pe_entry->total_len = 0;
	pe_entry->data_len = 0;
	pe_entry->buf = 0;
	pe_entry->flags = 0;
	pe_entry->context = 0L;

	dlist_remove(&pe_entry->entry);
	dlist_insert_head(&pe_entry->entry, &pe->free_list);
	SOCK_LOG_INFO("progress entry %p released\n", pe_entry);
}

static struct sock_pe_entry *sock_pe_acquire_entry(struct sock_pe *pe)
{
	struct dlist_entry *entry;
	struct sock_pe_entry *pe_entry;

	if (dlist_empty(&pe->free_list))
		return NULL;

	pe->num_free_entries--;
	entry = pe->free_list.next;
	pe_entry = container_of(entry, struct sock_pe_entry, entry);
	dlist_remove(&pe_entry->entry);
	dlist_insert_tail(&pe_entry->entry, &pe->busy_list);
	SOCK_LOG_INFO("progress entry %p acquired : %lu\n", pe_entry,
		      PE_INDEX(pe, pe_entry));
	return pe_entry;
}

static void sock_pe_report_tx_completion(struct sock_pe_entry *pe_entry)
{
	int ret1 = 0, ret2 = 0;
	if (!(pe_entry->flags & SOCK_NO_COMPLETION)) {
		if (pe_entry->comp->send_cq && 
		    (!pe_entry->comp->send_cq_event || 
		     (pe_entry->comp->send_cq_event && 
		      (pe_entry->msg_hdr.flags & FI_COMPLETION)))) 
			ret1 = pe_entry->comp->send_cq->report_completion(
				pe_entry->comp->send_cq, pe_entry->addr, pe_entry);
	}

	if (pe_entry->comp->send_cntr)
		ret2 = sock_cntr_inc(pe_entry->comp->send_cntr);
		
	if (ret1 < 0 || ret2 < 0) {
		SOCK_LOG_ERROR("Failed to report completion %p\n",
			       pe_entry);
		if (pe_entry->comp->eq) {
			sock_eq_report_error(
				pe_entry->comp->eq, 
				&pe_entry->comp->send_cntr->cntr_fid.fid, 
				pe_entry->comp->send_cntr->cntr_fid.fid.context, 
				0, FI_ENOSPC, -FI_ENOSPC, NULL, 0);
		}				
	}
}

static void sock_pe_report_rx_completion(struct sock_pe_entry *pe_entry)
{
	int ret1 = 0, ret2 = 0;

	if (pe_entry->comp->recv_cq && 
	    (!pe_entry->comp->recv_cq_event || 
	     (pe_entry->comp->recv_cq_event && 
	      (pe_entry->flags & FI_COMPLETION)))) 
		ret1 = pe_entry->comp->recv_cq->report_completion(
			pe_entry->comp->recv_cq, pe_entry->addr,
			pe_entry);

	if (pe_entry->comp->recv_cntr) 
		ret2 = sock_cntr_inc(pe_entry->comp->recv_cntr);
	

	if (ret1 < 0 || ret2 < 0) {
		SOCK_LOG_ERROR("Failed to report completion %p\n", pe_entry);
		if (pe_entry->comp->eq) {
			sock_eq_report_error(
				pe_entry->comp->eq, 
				&pe_entry->comp->recv_cq->cq_fid.fid, 
				pe_entry->comp->recv_cq->cq_fid.fid.context, 
				0, FI_ENOSPC, -FI_ENOSPC, NULL, 0);
		}
	}
}

static void sock_pe_report_mr_completion(struct sock_domain *domain,
				  struct sock_pe_entry *pe_entry)
{
	int i;
	struct sock_mr *mr;

	for (i = 0; i < pe_entry->msg_hdr.dest_iov_len; i++) {
		mr = sock_mr_get_entry(domain, pe_entry->pe.rx.rx_iov[i].iov.key);
		if (!mr || (!mr->cq && !mr->cntr))
			continue;
		
		pe_entry->buf = pe_entry->pe.rx.rx_iov[i].iov.addr;
		pe_entry->data_len = pe_entry->pe.rx.rx_iov[i].iov.len;
		
		if (mr->cq)
			mr->cq->report_completion(mr->cq, 
						  pe_entry->addr, pe_entry);
		if (mr->cntr)
			sock_cntr_inc(mr->cntr);
	}
}

static void sock_pe_report_remote_write(struct sock_rx_ctx *rx_ctx,
				 struct sock_pe_entry *pe_entry)
{
	pe_entry->buf = pe_entry->pe.rx.rx_iov[0].iov.addr;
	pe_entry->data_len = pe_entry->pe.rx.rx_iov[0].iov.len;
	
	if ((!pe_entry->comp->rem_write_cq && !pe_entry->comp->rem_write_cntr &&
	     !(rx_ctx->attr.op_flags & FI_REMOTE_WRITE)))
		return;
	
	if (pe_entry->comp->rem_write_cq) {
		if (pe_entry->comp->rem_write_cq_event) {
			if ( pe_entry->flags & FI_COMPLETION)
				pe_entry->comp->rem_write_cq->report_completion(
					pe_entry->comp->rem_write_cq, 
					pe_entry->addr, pe_entry);
		} else {
			pe_entry->comp->rem_write_cq->report_completion(
				pe_entry->comp->rem_write_cq, 
				pe_entry->addr, pe_entry);
		}
	}
	
	if (pe_entry->comp->rem_write_cntr)
		sock_cntr_inc(pe_entry->comp->rem_write_cntr);
}

static void sock_pe_report_write_completion(struct sock_pe_entry *pe_entry)
{
	if (!(pe_entry->flags & SOCK_NO_COMPLETION)) {
		sock_pe_report_tx_completion(pe_entry);
	
		if (pe_entry->comp->write_cq && 
		    (pe_entry->comp->send_cq != pe_entry->comp->write_cq) &&
		    (!pe_entry->comp->write_cq_event || 
		     (pe_entry->comp->write_cq_event && 
		      (pe_entry->msg_hdr.flags & FI_COMPLETION)))) 
			pe_entry->comp->write_cq->report_completion(
				pe_entry->comp->write_cq, pe_entry->addr, pe_entry);
	}
	
	if (pe_entry->comp->write_cntr && 
	    pe_entry->comp->write_cntr != pe_entry->comp->send_cntr)
		sock_cntr_inc(pe_entry->comp->write_cntr);
}

static void sock_pe_report_remote_read(struct sock_rx_ctx *rx_ctx,
				struct sock_pe_entry *pe_entry)
{
	pe_entry->buf = pe_entry->pe.rx.rx_iov[0].iov.addr;
	pe_entry->data_len = pe_entry->pe.rx.rx_iov[0].iov.len;
	
	if ((!pe_entry->comp->rem_read_cq && !pe_entry->comp->rem_read_cntr &&
	     !(rx_ctx->attr.op_flags & FI_REMOTE_READ)))
		return;
	
	if (pe_entry->comp->rem_read_cq) {
		if (pe_entry->comp->rem_read_cq_event) {
			if ( pe_entry->flags & FI_COMPLETION)
				pe_entry->comp->rem_read_cq->report_completion(
					pe_entry->comp->rem_read_cq, 
					pe_entry->addr, pe_entry);
		} else {
			pe_entry->comp->rem_read_cq->report_completion(
				pe_entry->comp->rem_read_cq, 
				pe_entry->addr, pe_entry);
		}
	}
	
	if (pe_entry->comp->rem_read_cntr)
		sock_cntr_inc(pe_entry->comp->rem_read_cntr);
}

static void sock_pe_report_read_completion(struct sock_pe_entry *pe_entry)
{
	if (!(pe_entry->flags & SOCK_NO_COMPLETION)) {
		sock_pe_report_tx_completion(pe_entry);
		
		if (pe_entry->comp->read_cq && 
		    (pe_entry->comp->read_cq != pe_entry->comp->send_cq) &&
		    (!pe_entry->comp->read_cq_event || 
		     (pe_entry->comp->read_cq_event && 
		      (pe_entry->msg_hdr.flags & FI_COMPLETION)))) 
			pe_entry->comp->read_cq->report_completion(
				pe_entry->comp->read_cq, pe_entry->addr, pe_entry);
	}
	
	if (pe_entry->comp->read_cntr &&
	    pe_entry->comp->read_cntr != pe_entry->comp->send_cntr)
		sock_cntr_inc(pe_entry->comp->read_cntr);
}

static void sock_pe_report_error(struct sock_pe_entry *pe_entry, int rem)
{
	if (pe_entry->comp->recv_cntr)
		sock_cntr_err_inc(pe_entry->comp->recv_cntr);
	if (pe_entry->comp->recv_cq)
		sock_cq_report_error(pe_entry->comp->recv_cq, pe_entry, rem, 
				     -FI_ENOSPC, -FI_ENOSPC, NULL);
}

static void sock_pe_progress_pending_ack(struct sock_pe *pe, 
					 struct sock_pe_entry *pe_entry)
{
	int len, data_len, i;
	struct sock_conn *conn = pe_entry->conn;

	if (!conn)
		return;

	if (conn->tx_pe_entry != NULL && conn->tx_pe_entry != pe_entry) {
		SOCK_LOG_INFO("Cannot progress %p as conn %p is being used by %p\n",
			      pe_entry, conn, conn->tx_pe_entry);
		return;
	}

	if (conn->tx_pe_entry == NULL) {
		SOCK_LOG_INFO("Connection %p grabbed by %p\n", conn, pe_entry);
		conn->tx_pe_entry = pe_entry;
	}

	if (sock_pe_send_field(pe_entry, &pe_entry->response, 
			       sizeof(pe_entry->response), 0))
		return;	
	len = sizeof(struct sock_msg_response);

	switch (pe_entry->response.msg_hdr.op_type) {
	case SOCK_OP_READ_COMPLETE:
		for (i = 0; i < pe_entry->msg_hdr.dest_iov_len; i++) {
			if (sock_pe_send_field(
				    pe_entry,
				    (char *) (uintptr_t) pe_entry->pe.rx.rx_iov[i].iov.addr,
				    pe_entry->pe.rx.rx_iov[i].iov.len, len))
				return;
			len += pe_entry->pe.rx.rx_iov[i].iov.len;
		}
		break;

	case SOCK_OP_ATOMIC_COMPLETE:
		data_len = pe_entry->total_len - len;
		if (data_len) {
			if (sock_pe_send_field(pe_entry,
					       &pe_entry->pe.rx.atomic_cmp[0],
					       data_len, len))
				return;
			len += data_len;
		}
		break;
		
	default:
		break;
	}
	
	if (pe_entry->total_len == pe_entry->done_len) {
		pe_entry->is_complete = 1;
		pe_entry->pe.rx.pending_send = 0;
		sock_comm_flush(pe_entry->conn);
		pe_entry->conn->tx_pe_entry = NULL;
	}
}

static void sock_pe_send_response(struct sock_pe *pe, 
				  struct sock_rx_ctx *rx_ctx,
				  struct sock_pe_entry *pe_entry, 
				  size_t data_len, uint8_t op_type)
{
	struct sock_msg_response *response = &pe_entry->response;
	memset(response, 0, sizeof(struct sock_msg_response));

	response->pe_entry_id = htons(pe_entry->msg_hdr.pe_entry_id);
	response->msg_hdr.dest_iov_len = 0;
	response->msg_hdr.flags = 0;
	response->msg_hdr.msg_len = sizeof(*response) + data_len;
	response->msg_hdr.version = SOCK_WIRE_PROTO_VERSION;
	response->msg_hdr.op_type = op_type;
	response->msg_hdr.msg_len = htonll(response->msg_hdr.msg_len);
	response->msg_hdr.rx_id = pe_entry->msg_hdr.rx_id;

	pe->pe_atomic = NULL;
	pe_entry->done_len = 0;
	pe_entry->pe.rx.pending_send = 1;
	pe_entry->conn->rx_pe_entry = NULL;
	pe_entry->total_len = sizeof(*response) + data_len;

	sock_pe_progress_pending_ack(pe, pe_entry);
}

inline static int sock_pe_read_response(struct sock_pe_entry *pe_entry)
{
	int ret, len, data_len;

	if (pe_entry->done_len >= sizeof(struct sock_msg_response))
		return 0;
	
	len = sizeof(struct sock_msg_hdr);
	data_len = sizeof(struct sock_msg_response) - len;
	if ((ret = sock_pe_recv_field(pe_entry, 
				      &pe_entry->response.pe_entry_id,
				      data_len, len)))
		return ret;
	pe_entry->response.pe_entry_id = ntohs(pe_entry->response.pe_entry_id);
	return 0;
}

static int sock_pe_handle_ack(struct sock_pe *pe, struct sock_pe_entry *pe_entry)
{
	struct sock_pe_entry *waiting_entry;
	struct sock_msg_response *response;

	if (sock_pe_read_response(pe_entry))
		return 0;

	response = &pe_entry->response;
	assert(response->pe_entry_id <= SOCK_PE_MAX_ENTRIES);
	waiting_entry = &pe->pe_table[response->pe_entry_id];
	SOCK_LOG_INFO("Received ack for PE entry %p (index: %d)\n", 
		      waiting_entry, response->pe_entry_id);
	
	assert(waiting_entry->type == SOCK_PE_TX);
	sock_pe_report_tx_completion(waiting_entry);
	waiting_entry->is_complete = 1;
	pe_entry->is_complete = 1;
	return 0;
}

static int sock_pe_handle_read_complete(struct sock_pe *pe, 
					struct sock_pe_entry *pe_entry)
{
	struct sock_pe_entry *waiting_entry;
	struct sock_msg_response *response;
	int len, i;

	if (sock_pe_read_response(pe_entry))
		return 0;

	response = &pe_entry->response;
	assert(response->pe_entry_id <= SOCK_PE_MAX_ENTRIES);
	waiting_entry = &pe->pe_table[response->pe_entry_id];
	SOCK_LOG_INFO("Received read complete for PE entry %p (index: %d)\n", 
		      waiting_entry, response->pe_entry_id);
	
	waiting_entry = &pe->pe_table[response->pe_entry_id];
	assert(waiting_entry->type == SOCK_PE_TX);
	
	len = sizeof(struct sock_msg_response);
	for (i=0; i < waiting_entry->pe.tx.tx_op.dest_iov_len; i++) {
		if (sock_pe_recv_field(
			    pe_entry,
			    (char *) (uintptr_t) waiting_entry->pe.tx.data.tx_iov[i].dst.iov.addr,
			    waiting_entry->pe.tx.data.tx_iov[i].dst.iov.len, len))
			return 0;
		len += waiting_entry->pe.tx.data.tx_iov[i].dst.iov.len;
	}

	sock_pe_report_read_completion(waiting_entry);
	waiting_entry->is_complete = 1;
	pe_entry->is_complete = 1;
	return 0;
}

static int sock_pe_handle_write_complete(struct sock_pe *pe, 
					struct sock_pe_entry *pe_entry)
{
	struct sock_pe_entry *waiting_entry;
	struct sock_msg_response *response;

	if (sock_pe_read_response(pe_entry))
		return 0;

	response = &pe_entry->response;
	assert(response->pe_entry_id <= SOCK_PE_MAX_ENTRIES);
	waiting_entry = &pe->pe_table[response->pe_entry_id];
	SOCK_LOG_INFO("Received ack for PE entry %p (index: %d)\n", 
		      waiting_entry, response->pe_entry_id);
	
	assert(waiting_entry->type == SOCK_PE_TX);
	sock_pe_report_write_completion(waiting_entry);
	waiting_entry->is_complete = 1;
	pe_entry->is_complete = 1;
	return 0;
}

static int sock_pe_handle_atomic_complete(struct sock_pe *pe, 
					  struct sock_pe_entry *pe_entry)
{
	size_t datatype_sz;
	struct sock_pe_entry *waiting_entry;
	struct sock_msg_response *response;
	int len, i;

	if (sock_pe_read_response(pe_entry))
		return 0;
	
	response = &pe_entry->response;
	assert(response->pe_entry_id <= SOCK_PE_MAX_ENTRIES);
	waiting_entry = &pe->pe_table[response->pe_entry_id];
	SOCK_LOG_INFO("Received atomic complete for PE entry %p (index: %d)\n", 
		      waiting_entry, response->pe_entry_id);
	
	waiting_entry = &pe->pe_table[response->pe_entry_id];
	assert(waiting_entry->type == SOCK_PE_TX);

	len = sizeof(struct sock_msg_response);
	datatype_sz = fi_datatype_size(waiting_entry->pe.tx.tx_op.atomic.datatype);
	for (i=0; i < waiting_entry->pe.tx.tx_op.atomic.res_iov_len; i++) {
		if (sock_pe_recv_field(
			    pe_entry,
			    (char *) (uintptr_t) waiting_entry->pe.tx.data.tx_iov[i].res.ioc.addr,
			    waiting_entry->pe.tx.data.tx_iov[i].res.ioc.count * datatype_sz,
			    len))
			return 0;
		len += (waiting_entry->pe.tx.data.tx_iov[i].res.ioc.count * datatype_sz);
	}

	if (waiting_entry->pe.rx.rx_op.atomic.res_iov_len)
		sock_pe_report_read_completion(waiting_entry);
	else
		sock_pe_report_write_completion(waiting_entry);

	waiting_entry->is_complete = 1;
	pe_entry->is_complete = 1;
	return 0;
}

static int sock_pe_process_rx_read(struct sock_pe *pe, struct sock_rx_ctx *rx_ctx,
				   struct sock_pe_entry *pe_entry)
{
	int i;
	struct sock_mr *mr;
	uint64_t len, entry_len, data_len;

	len = sizeof(struct sock_msg_hdr);
	entry_len = sizeof(union sock_iov) * pe_entry->msg_hdr.dest_iov_len;
	if (sock_pe_recv_field(pe_entry, &pe_entry->pe.rx.rx_iov[0],
			       entry_len, len))
		return 0;
	len += entry_len;

	/* verify mr */
	data_len = 0;
	for (i = 0; i < pe_entry->msg_hdr.dest_iov_len; i++) {
		
		mr = sock_mr_verify_key(rx_ctx->domain, 
					pe_entry->pe.rx.rx_iov[i].iov.key,
					(void *) (uintptr_t) pe_entry->pe.rx.rx_iov[i].iov.addr,
					pe_entry->pe.rx.rx_iov[i].iov.len,
					FI_REMOTE_READ);
		if (!mr) {
			SOCK_LOG_ERROR("Remote memory access error: %p, %lu, %" PRIu64 "\n",
				       (void *) (uintptr_t) pe_entry->pe.rx.rx_iov[i].iov.addr,
				       pe_entry->pe.rx.rx_iov[i].iov.len,
				       pe_entry->pe.rx.rx_iov[i].iov.key);
			sock_pe_send_response(pe, rx_ctx, pe_entry, 0, 
					      SOCK_OP_READ_ERROR);
			return -FI_EINVAL;
		}
		
		if (mr->flags & FI_MR_OFFSET)
			pe_entry->pe.rx.rx_iov[i].iov.addr += mr->offset;
		data_len += pe_entry->pe.rx.rx_iov[i].iov.len;
	}

	pe_entry->buf = pe_entry->pe.rx.rx_iov[0].iov.addr;
	pe_entry->data_len = data_len;

	sock_pe_report_remote_read(rx_ctx, pe_entry);
	sock_pe_send_response(pe, rx_ctx, pe_entry, data_len, 
			      SOCK_OP_READ_COMPLETE);	
	return 0;
}

static int sock_pe_process_rx_write(struct sock_pe *pe, struct sock_rx_ctx *rx_ctx,
				   struct sock_pe_entry *pe_entry)
{
	int i, ret = 0;
	struct sock_mr *mr;
	uint64_t rem, len, entry_len;

	len = sizeof(struct sock_msg_hdr);
	if (pe_entry->msg_hdr.flags & FI_REMOTE_CQ_DATA) {
		if (sock_pe_recv_field(pe_entry, &pe_entry->data,
				       SOCK_CQ_DATA_SIZE, len))
			return 0;
		len += SOCK_CQ_DATA_SIZE;
	}

	entry_len = sizeof(union sock_iov) * pe_entry->msg_hdr.dest_iov_len;
	if (sock_pe_recv_field(pe_entry, &pe_entry->pe.rx.rx_iov[0], entry_len, len))
		return 0;
	len += entry_len;

	rem = pe_entry->msg_hdr.msg_len - len;
	for (i = 0; rem > 0 && i < pe_entry->msg_hdr.dest_iov_len; i++) {

		if ((len - pe_entry->done_len) == pe_entry->pe.rx.rx_iov[i].iov.addr) {
			mr = sock_mr_verify_key(rx_ctx->domain, 
						pe_entry->pe.rx.rx_iov[i].iov.key,
						(void *) (uintptr_t) pe_entry->pe.rx.rx_iov[i].iov.addr,
						pe_entry->pe.rx.rx_iov[i].iov.len,
						FI_REMOTE_WRITE);
			if (!mr) {
				SOCK_LOG_ERROR("Remote memory access error: %p, %lu, %" PRIu64 "\n",
					       (void *) (uintptr_t) pe_entry->pe.rx.rx_iov[i].iov.addr,
					       pe_entry->pe.rx.rx_iov[i].iov.len,
					       pe_entry->pe.rx.rx_iov[i].iov.key);
				sock_pe_send_response(pe, rx_ctx, pe_entry, 0, 
						      SOCK_OP_WRITE_ERROR);
				break;
			}
			
			if (mr->flags & FI_MR_OFFSET)
				pe_entry->pe.rx.rx_iov[i].iov.addr += mr->offset;
		}
			
		if (sock_pe_recv_field(pe_entry, 
					(void *) (uintptr_t) pe_entry->pe.rx.rx_iov[i].iov.addr,
					pe_entry->pe.rx.rx_iov[i].iov.len, len))
			return 0;
		len += pe_entry->pe.rx.rx_iov[i].iov.len;
		rem -= pe_entry->pe.rx.rx_iov[i].iov.len;
	}
	pe_entry->buf = pe_entry->pe.rx.rx_iov[0].iov.addr;
	pe_entry->data_len = 0;
	for (i = 0; i < pe_entry->msg_hdr.dest_iov_len; i++) {
		pe_entry->data_len += pe_entry->pe.rx.rx_iov[i].iov.len;
	}
	
	/* report error, if any */
	if (rem) {
		sock_pe_report_error(pe_entry, rem);
		goto out;
	} else {
		if (pe_entry->flags & FI_REMOTE_CQ_DATA) {
			sock_pe_report_rx_completion(pe_entry);
		}
	}
	
out:
	sock_pe_report_remote_write(rx_ctx, pe_entry);
	sock_pe_report_mr_completion(rx_ctx->domain, pe_entry);
	sock_pe_send_response(pe, rx_ctx, pe_entry, 0, 
			      SOCK_OP_WRITE_COMPLETE);	
	return ret;
}

#define SOCK_ATOMIC_UPDATE_INT(_cmp, _src, _dst, _tmp) do {		\
        _cmp = cmp, _dst = dst, _src = src;			        \
	switch (op) {							\
	case FI_MIN:							\
		*_cmp = *_dst;						\
		if (*_src < *_dst)					\
			*_dst = *_src;					\
		break;							\
									\
	case FI_MAX:							\
		*_cmp = *_dst;						\
		if (*_src > *_dst)					\
			*_dst = *_src;					\
		break;							\
									\
	case FI_SUM:							\
		*_cmp = *_dst;						\
		*_dst = *_dst + *_src;					\
		break;							\
									\
	case FI_PROD:							\
		*_cmp = *_dst;						\
		*_dst = *_dst * *_src;					\
		break;							\
									\
	case FI_LOR:							\
		*_cmp = *_dst;						\
		*_dst = *_dst || *_src;					\
		break;							\
									\
	case FI_LAND:							\
		*_cmp = *_dst;						\
		*_dst = *_dst && *_src;					\
		break;							\
									\
	case FI_BOR:							\
		*_cmp = *_dst;						\
		*_dst = *_dst | *_src;					\
		break;							\
									\
	case FI_BAND:							\
		*_cmp = *_dst;						\
		*_dst = *_dst & *_src;					\
		break;							\
									\
	case FI_LXOR:							\
		*_cmp = *_dst;						\
									\
		*_dst = ((*_dst && !*_src) || (!*_dst && *_src));	\
		break;							\
									\
	case FI_BXOR:							\
		*_cmp = *_dst;						\
		*_dst = *_dst ^ *_src;					\
		break;							\
									\
	case FI_ATOMIC_READ:						\
		*_cmp = *_dst;						\
		break;							\
									\
	case FI_ATOMIC_WRITE:						\
		*_cmp = *_dst;						\
		*_dst = *_src;						\
		break;							\
									\
	case FI_CSWAP:							\
		if (*_cmp == *_dst)					\
			*_dst = *_src;					\
		else							\
			*_cmp = *_dst;					\
		break;							\
									\
	case FI_CSWAP_NE:						\
		if (*_cmp != *_dst)					\
			*_dst = *_src;					\
		else							\
			*_cmp = *_dst;					\
		break;							\
									\
	case FI_CSWAP_LE:						\
		if (*_cmp <= *_dst)					\
			*_dst = *_src;					\
		else							\
			*_cmp = *_dst;					\
		break;							\
									\
	case FI_CSWAP_LT:						\
		if (*_cmp < *_dst)					\
			*_dst = *_src;					\
		else							\
			*_cmp = *_dst;					\
		break;							\
									\
	case FI_CSWAP_GE:						\
		if (*_cmp >= *_dst)					\
			*_dst = *_src;					\
		else							\
			*_cmp = *_dst;					\
		break;							\
									\
	case FI_CSWAP_GT:						\
		if (*_cmp > *_dst)					\
			*_dst = *_src;					\
		else							\
			*_cmp = *_dst;					\
		break;							\
									\
	case FI_MSWAP:							\
		_tmp = *_dst;						\
		*_dst = (*_src & *_cmp) | (*_dst & ~(*_cmp));		\
		*_cmp = _tmp;						\
		break;							\
									\
	default:							\
		SOCK_LOG_ERROR("Atomic operation type not supported\n"); \
		break;							\
	}								\
	}while(0)								

#define SOCK_ATOMIC_UPDATE_FLOAT(_cmp, _src, _dst) do {			\
        _cmp = cmp, _dst = dst, _src = src;			        \
	switch (op) {							\
	case FI_MIN:							\
		*_cmp = *_dst;						\
		if (*_src < *_dst)					\
			*_dst = *_src;					\
		break;							\
									\
	case FI_MAX:							\
		*_cmp = *_dst;						\
		if (*_src > *_dst)					\
			*_dst = *_src;					\
		break;							\
									\
	case FI_SUM:							\
		*_cmp = *_dst;						\
		*_dst = *_dst + *_src;					\
		break;							\
									\
	case FI_PROD:							\
		*_cmp = *_dst;						\
		*_dst = *_dst * *_src;					\
		break;							\
									\
	case FI_LOR:							\
		*_cmp = *_dst;						\
		*_dst = *_dst || *_src;					\
		break;							\
									\
	case FI_LAND:							\
		*_cmp = *_dst;						\
		*_dst = *_dst && *_src;					\
		break;							\
									\
	case FI_ATOMIC_READ:						\
		*_cmp = *_dst;						\
		break;							\
									\
	case FI_ATOMIC_WRITE:						\
		*_cmp = *_dst;						\
		*_dst = *_src;						\
		break;							\
									\
	case FI_CSWAP:							\
		if (*_cmp == *_dst)					\
			*_dst = *_src;					\
		else							\
			*_cmp = *_dst;					\
		break;							\
									\
	case FI_CSWAP_NE:						\
		if (*_cmp != *_dst)					\
			*_dst = *_src;					\
		else							\
			*_cmp = *_dst;					\
		break;							\
									\
	case FI_CSWAP_LE:						\
		if (*_cmp <= *_dst)					\
			*_dst = *_src;					\
		else							\
			*_cmp = *_dst;					\
		break;							\
									\
	case FI_CSWAP_LT:						\
		if (*_cmp < *_dst)					\
			*_dst = *_src;					\
		else							\
			*_cmp = *_dst;					\
		break;							\
									\
	case FI_CSWAP_GE:						\
		if (*_cmp >= *_dst)					\
			*_dst = *_src;					\
		else							\
			*_cmp = *_dst;					\
		break;							\
									\
	case FI_CSWAP_GT:						\
		if (*_cmp > *_dst)					\
			*_dst = *_src;					\
		else							\
			*_cmp = *_dst;					\
		break;							\
									\
	default:							\
		SOCK_LOG_ERROR("Atomic operation type not supported\n"); \
		break;							\
	}								\
	}while(0)								


static int sock_pe_update_atomic(void *cmp, void *dst, void *src,
				 enum fi_datatype datatype, enum fi_op op)
{
	switch (datatype) {
	case FI_INT8:
	{
		int8_t *_cmp, *_dst, *_src, _tmp;
		_cmp = cmp, _src = src, _dst = dst;
		SOCK_ATOMIC_UPDATE_INT(_cmp, _src, _dst, _tmp);
		break;		
	}

	case FI_UINT8:
	{
		uint8_t *_cmp, *_dst, *_src, _tmp;
		_cmp = cmp, _src = src, _dst = dst;
		SOCK_ATOMIC_UPDATE_INT(_cmp, _src, _dst, _tmp);
		break;
	}

	case FI_INT16:
	{
		int16_t *_cmp, *_dst, *_src, _tmp;
		_cmp = cmp, _src = src, _dst = dst;
		SOCK_ATOMIC_UPDATE_INT(_cmp, _src, _dst, _tmp);
		break;
	}

	case FI_UINT16:
	{
		uint16_t *_cmp, *_dst, *_src, _tmp;
		_cmp = cmp, _src = src, _dst = dst;
		SOCK_ATOMIC_UPDATE_INT(_cmp, _src, _dst, _tmp);
		break;
	}

	case FI_INT32:
	{
		int32_t *_cmp, *_dst, *_src, _tmp;
		_cmp = cmp, _src = src, _dst = dst;
		SOCK_ATOMIC_UPDATE_INT(_cmp, _src, _dst, _tmp);
		break;
	}

	case FI_UINT32:
	{
		uint32_t *_cmp, *_dst, *_src, _tmp;
		_cmp = cmp, _src = src, _dst = dst;
		SOCK_ATOMIC_UPDATE_INT(_cmp, _src, _dst, _tmp);
		break;
	}

	case FI_INT64:
	{
		int64_t *_cmp, *_dst, *_src, _tmp;
		_cmp = cmp, _src = src, _dst = dst;
		SOCK_ATOMIC_UPDATE_INT(_cmp, _src, _dst, _tmp);
		break;
	}

	case FI_UINT64:
	{
		uint64_t *_cmp, *_dst, *_src, _tmp;
		_cmp = cmp, _src = src, _dst = dst;
		SOCK_ATOMIC_UPDATE_INT(_cmp, _src, _dst, _tmp);
		break;
	}

	case FI_FLOAT:
	{
		float *_cmp, *_dst, *_src;
		_cmp = cmp, _src = src, _dst = dst;
		SOCK_ATOMIC_UPDATE_FLOAT(_cmp, _src, _dst);
		break;
	}

	case FI_DOUBLE:
	{
		double *_cmp, *_dst, *_src;
		_cmp = cmp, _src = src, _dst = dst;
		SOCK_ATOMIC_UPDATE_FLOAT(_cmp, _src, _dst);
		break;
	}

	case FI_LONG_DOUBLE:
	{
		long double *_cmp, *_dst, *_src;
		_cmp = cmp, _src = src, _dst = dst;
		SOCK_ATOMIC_UPDATE_FLOAT(_cmp, _src, _dst);
		break;
	}

	default:
		SOCK_LOG_ERROR("Atomic datatype not supported\n");
		break;
	}
	return 0;
}


static int sock_pe_process_rx_atomic(struct sock_pe *pe, struct sock_rx_ctx *rx_ctx,
				     struct sock_pe_entry *pe_entry)
{
	int i, j, ret = 0;
	size_t datatype_sz;
	struct sock_mr *mr;
	uint64_t offset, len, entry_len;
	

	if (pe->pe_atomic){
		if (pe->pe_atomic != pe_entry)
			return 0;
	} else {
		pe->pe_atomic = pe_entry;
	}

	len = sizeof(struct sock_msg_hdr);
	if (sock_pe_recv_field(pe_entry, &pe_entry->pe.rx.rx_op, 
			       sizeof(struct sock_op), len))
		return 0;
	len += sizeof(struct sock_op);

	if (pe_entry->msg_hdr.flags & FI_REMOTE_CQ_DATA) {
		if (sock_pe_recv_field(pe_entry, &pe_entry->data,
				       SOCK_CQ_DATA_SIZE, len))
			return 0;
		len += SOCK_CQ_DATA_SIZE;
	}

	/* dst iocs */
	entry_len = sizeof(union sock_iov) * pe_entry->pe.rx.rx_op.dest_iov_len;
	if (sock_pe_recv_field(pe_entry, &pe_entry->pe.rx.rx_iov[0],
			       entry_len, len))
		return 0;
	len += entry_len;

	entry_len = 0;
	datatype_sz = fi_datatype_size(pe_entry->pe.rx.rx_op.atomic.datatype);
	for (i = 0; i < pe_entry->pe.rx.rx_op.dest_iov_len; i++) {
		entry_len += pe_entry->pe.rx.rx_iov[i].ioc.count;
	}
	entry_len *= datatype_sz;

	/* cmp data */
	if (pe_entry->pe.rx.rx_op.atomic.cmp_iov_len) {
		if (sock_pe_recv_field(pe_entry, &pe_entry->pe.rx.atomic_cmp[0],
				       entry_len, len))
			return 0;
		len += entry_len;
			
		/* compare */
		for (i = 0; i < pe_entry->pe.rx.rx_op.dest_iov_len; i++) {
			mr = sock_mr_verify_key(rx_ctx->domain, 
						pe_entry->pe.rx.rx_iov[i].ioc.key,
						(void *) (uintptr_t) pe_entry->pe.rx.rx_iov[i].ioc.addr,
						pe_entry->pe.rx.rx_iov[i].ioc.count * datatype_sz,
						FI_REMOTE_WRITE);
			if (!mr) {
				SOCK_LOG_ERROR("Remote memory access error: %p, %lu, %" PRIu64 "\n",
					       (void *) (uintptr_t) pe_entry->pe.rx.rx_iov[i].ioc.addr,
					       pe_entry->pe.rx.rx_iov[i].ioc.count * datatype_sz,
					       pe_entry->pe.rx.rx_iov[i].ioc.key);
				sock_pe_send_response(pe, rx_ctx, pe_entry, 0, 
						      SOCK_OP_ATOMIC_ERROR);
				goto err;
			}
			if (mr->flags & FI_MR_OFFSET)
				pe_entry->pe.rx.rx_iov[i].ioc.addr += mr->offset;
		}
	}

	/* src data */
	if (sock_pe_recv_field(pe_entry, &pe_entry->pe.rx.atomic_src[0], 
			       entry_len, len))
		return 0;
	len += entry_len;

	offset = 0;
	for (i = 0; i < pe_entry->pe.rx.rx_op.dest_iov_len; i++) {
		for (j = 0; j < pe_entry->pe.rx.rx_iov[i].ioc.count; j++) {
			sock_pe_update_atomic((char *) &pe_entry->pe.rx.atomic_cmp[0] + offset,
					      (char *) (uintptr_t) pe_entry->pe.rx.rx_iov[i].ioc.addr + j * datatype_sz,
					      (char *) &pe_entry->pe.rx.atomic_src[0] + offset,
					      pe_entry->pe.rx.rx_op.atomic.datatype,
					      pe_entry->pe.rx.rx_op.atomic.op);
			offset += datatype_sz;
		}
	}

	pe_entry->buf = pe_entry->pe.rx.rx_iov[0].iov.addr;
	pe_entry->data_len = offset;
	
	if (pe_entry->flags & FI_REMOTE_CQ_DATA) {
		sock_pe_report_rx_completion(pe_entry);
	}
	
	sock_pe_report_remote_write(rx_ctx, pe_entry);
	sock_pe_report_mr_completion(rx_ctx->domain, pe_entry);
	sock_pe_send_response(pe, rx_ctx, pe_entry, 
			      pe_entry->pe.rx.rx_op.atomic.res_iov_len ? 
			      entry_len : 0, SOCK_OP_ATOMIC_COMPLETE);
	return ret;
	
err:
	sock_pe_report_error(pe_entry, 0);
	return -FI_EINVAL;
}

static int sock_pe_progress_buffered_rx(struct sock_rx_ctx *rx_ctx)
{
	struct dlist_entry *entry;
	struct sock_pe_entry pe_entry;
	struct sock_rx_entry *rx_buffered, *rx_posted;
	int i, rem = 0, offset, len, used_len, dst_offset;
	
	if (dlist_empty(&rx_ctx->rx_entry_list) ||
	    dlist_empty(&rx_ctx->rx_buffered_list)) 
		return 0;

	for (entry = rx_ctx->rx_buffered_list.next; 
	     entry != &rx_ctx->rx_buffered_list;) {

		rx_buffered = container_of(entry, struct sock_rx_entry, entry);
		entry = entry->next;
		
		if (!rx_buffered->is_complete)
			continue;

		rx_posted = sock_rx_get_entry(rx_ctx, rx_buffered->addr,
						rx_buffered->tag, 
						rx_buffered->is_tagged);
		if (!rx_posted) 
			continue;
		
		SOCK_LOG_INFO("Consuming buffered entry: %p, ctx: %p\n", 
			      rx_buffered, rx_ctx);
		SOCK_LOG_INFO("Consuming posted entry: %p, ctx: %p\n", 
			      rx_posted, rx_ctx);

		offset = 0;
		rem = rx_buffered->iov[0].iov.len;
		rx_ctx->buffered_len -= rem;
		used_len = rx_posted->used;
		pe_entry.data_len = 0;
		pe_entry.buf = 0L;
		for (i = 0; i < rx_posted->rx_op.dest_iov_len && rem > 0; i++) {
			if (used_len >= rx_posted->rx_op.dest_iov_len) {
				used_len -= rx_posted->rx_op.dest_iov_len;
				continue;
			}

			dst_offset = used_len;
			len = MIN(rx_posted->iov[i].iov.len, rem);
			pe_entry.buf = rx_posted->iov[i].iov.addr + dst_offset;
			memcpy((char *) (uintptr_t) rx_posted->iov[i].iov.addr + dst_offset,
			       (char *) (uintptr_t) rx_buffered->iov[0].iov.addr + offset, len);
			offset += len;
			rem -= len;
			dst_offset = used_len = 0;
			rx_posted->used += len;
			pe_entry.data_len = rx_buffered->used;
		}
		
		pe_entry.done_len = offset;
		pe_entry.data = rx_buffered->data;
		pe_entry.tag = rx_buffered->tag;
		pe_entry.context = (uint64_t)rx_posted->context;
		pe_entry.pe.rx.rx_iov[0].iov.addr = rx_posted->iov[0].iov.addr;
		pe_entry.type = SOCK_PE_RX;
		pe_entry.comp = rx_buffered->comp;
		pe_entry.flags = rx_posted->flags;
		pe_entry.flags &= ~FI_MULTI_RECV;

		if (rx_posted->flags & FI_MULTI_RECV) {
			if (sock_rx_avail_len(rx_posted) < rx_ctx->min_multi_recv) {
				pe_entry.flags |= FI_MULTI_RECV;
				dlist_remove(&rx_posted->entry);
			}
		} else {
			dlist_remove(&rx_posted->entry);
		}
	
		if (rem) {
			SOCK_LOG_INFO("Not enough space in posted recv buffer\n");
			sock_pe_report_error(&pe_entry, rem);
			return 0;
		} else {
			sock_pe_report_rx_completion(&pe_entry);
		}

		dlist_remove(&rx_buffered->entry);
		sock_rx_release_entry(rx_buffered);

		if ((!(rx_posted->flags & FI_MULTI_RECV) ||
		     (pe_entry.flags & FI_MULTI_RECV))) {
			sock_rx_release_entry(rx_posted);
			rx_ctx->num_left++;
		}
	}
	return 0;
}

static int sock_pe_process_rx_send(struct sock_pe *pe, struct sock_rx_ctx *rx_ctx,
				   struct sock_pe_entry *pe_entry)
{
	ssize_t i, ret = 0;
	struct sock_rx_entry *rx_entry;
	uint64_t len, rem, offset, data_len, done_data, used;

	offset = 0;
	len = sizeof(struct sock_msg_hdr);

	if (pe_entry->msg_hdr.op_type == SOCK_OP_TSEND) {
		if (sock_pe_recv_field(pe_entry, &pe_entry->tag,
				       SOCK_TAG_SIZE, len))
			return 0;
		len += SOCK_TAG_SIZE;
	}

	if (pe_entry->msg_hdr.flags & FI_REMOTE_CQ_DATA) {
		if (sock_pe_recv_field(pe_entry, &pe_entry->data,
				       SOCK_CQ_DATA_SIZE, len))
			return 0;
		len += SOCK_CQ_DATA_SIZE;
	}

	if (pe_entry->done_len == len && !pe_entry->pe.rx.rx_entry) {
		data_len = pe_entry->msg_hdr.msg_len - len;

		fastlock_acquire(&rx_ctx->lock);
		sock_pe_progress_buffered_rx(rx_ctx);
		
		rx_entry = sock_rx_get_entry(rx_ctx, pe_entry->addr, pe_entry->tag, pe_entry->msg_hdr.op_type);
		SOCK_LOG_INFO("Consuming posted entry: %p\n", rx_entry);	

		if (!rx_entry) {
			SOCK_LOG_INFO("%p: No matching recv, buffering recv (len=%llu)\n", 
				      pe_entry, (long long unsigned int)data_len);

			rx_entry = sock_rx_new_buffered_entry(rx_ctx, data_len);
			if (!rx_entry) {
				fastlock_release(&rx_ctx->lock);
				return -FI_ENOMEM;
			}
			
			rx_entry->addr = pe_entry->addr;
			rx_entry->tag = pe_entry->tag;
			rx_entry->data = pe_entry->data;
			rx_entry->ignore = 0;
			rx_entry->comp = pe_entry->comp;
			pe_entry->context = rx_entry->context;

			if (pe_entry->msg_hdr.flags & FI_REMOTE_CQ_DATA)
				rx_entry->flags |= FI_REMOTE_CQ_DATA;
			
			if (pe_entry->msg_hdr.op_type == SOCK_OP_TSEND) {
				rx_entry->is_tagged = 1;
			}
		}
		fastlock_release(&rx_ctx->lock);
		pe_entry->context = rx_entry->context;
		pe_entry->pe.rx.rx_entry = rx_entry;
	}
	
	rx_entry = pe_entry->pe.rx.rx_entry;
	done_data = pe_entry->done_len - len;
	pe_entry->data_len = pe_entry->msg_hdr.msg_len - len;
	rem = pe_entry->data_len - done_data;
	used = rx_entry->used;

	for (i = 0; rem > 0 && i < rx_entry->rx_op.dest_iov_len; i++) {

		/* skip used contents in rx_entry */
		if (used >= rx_entry->iov[i].iov.len) {
			used -= rx_entry->iov[i].iov.len;
			continue;
		}

		offset = used;
		data_len = MIN(rx_entry->iov[i].iov.len - used, rem);
		ret = sock_comm_recv(pe_entry->conn, 
				     (char *) (uintptr_t) rx_entry->iov[i].iov.addr + offset, 
				     data_len);
		if (ret <= 0)
			return ret;

		if (!pe_entry->buf)
			pe_entry->buf = rx_entry->iov[i].iov.addr + offset;
		rem -= ret;
		used = 0;
		pe_entry->done_len += ret;
		rx_entry->used += ret;
		if (ret != data_len)
			return 0;
	}

	pe_entry->is_complete = 1;
	rx_entry->is_complete = 1;
	rx_entry->is_busy = 0;
	pe_entry->flags = rx_entry->flags;
	if (pe_entry->msg_hdr.flags & FI_REMOTE_CQ_DATA)
		pe_entry->flags |= FI_REMOTE_CQ_DATA;
	pe_entry->flags &= ~FI_MULTI_RECV;

	fastlock_acquire(&rx_ctx->lock);
	if (rx_entry->flags & FI_MULTI_RECV) {
		if (sock_rx_avail_len(rx_entry) < rx_ctx->min_multi_recv) {
			pe_entry->flags |= FI_MULTI_RECV;
			dlist_remove(&rx_entry->entry);
		}
	} else {
		if (!rx_entry->is_buffered)
			dlist_remove(&rx_entry->entry);
	}
	fastlock_release(&rx_ctx->lock);

	/* report error, if any */
	if (rem) {
		SOCK_LOG_ERROR("Not enough space in posted recv buffer\n");
		sock_pe_report_error(pe_entry, rem);
		goto out;
	} else {
		if (!rx_entry->is_buffered)
			sock_pe_report_rx_completion(pe_entry);
	}

out:
	if (pe_entry->msg_hdr.flags & FI_TRANSMIT_COMPLETE) {
		sock_pe_send_response(pe, rx_ctx, pe_entry, 0, 
				      SOCK_OP_SEND_COMPLETE);
	}
		
	if (!rx_entry->is_buffered &&
	    (!(rx_entry->flags & FI_MULTI_RECV) ||
	     (pe_entry->flags & FI_MULTI_RECV))) {
		sock_rx_release_entry(rx_entry);
		fastlock_acquire(&rx_ctx->lock);
		rx_ctx->num_left++;
		fastlock_release(&rx_ctx->lock);
	}
	return ret;
}

static int sock_pe_process_recv(struct sock_pe *pe, struct sock_rx_ctx *rx_ctx,
				struct sock_pe_entry *pe_entry)
{
	int ret;
	struct sock_msg_hdr *msg_hdr;

	msg_hdr = &pe_entry->msg_hdr;
	if (msg_hdr->version != SOCK_WIRE_PROTO_VERSION) {
		SOCK_LOG_ERROR("Invalid wire protocol\n");
		ret = -FI_EINVAL;
		goto out;
	}
		
	switch (pe_entry->msg_hdr.op_type) {
	case SOCK_OP_SEND:
	case SOCK_OP_TSEND:
		ret = sock_pe_process_rx_send(pe, rx_ctx, pe_entry);
		break;
	case SOCK_OP_WRITE:
		ret = sock_pe_process_rx_write(pe, rx_ctx, pe_entry);
		break;
	case SOCK_OP_READ:
		ret = sock_pe_process_rx_read(pe, rx_ctx, pe_entry);
		break;
	case SOCK_OP_ATOMIC:
		ret = sock_pe_process_rx_atomic(pe, rx_ctx, pe_entry);
		break;
	case SOCK_OP_SEND_COMPLETE:
		ret = sock_pe_handle_ack(pe, pe_entry);
		break;
	case SOCK_OP_WRITE_COMPLETE:
		ret = sock_pe_handle_write_complete(pe, pe_entry);
		break;
	case SOCK_OP_READ_COMPLETE:
		ret = sock_pe_handle_read_complete(pe, pe_entry);
		break;
	case SOCK_OP_ATOMIC_COMPLETE:
		ret = sock_pe_handle_atomic_complete(pe, pe_entry);
		break;
	case SOCK_OP_WRITE_ERROR:
	case SOCK_OP_READ_ERROR:
	case SOCK_OP_ATOMIC_ERROR:
		ret = sock_pe_handle_ack(pe, pe_entry);
		break;
	default:
		ret = -FI_ENOSYS;
		SOCK_LOG_ERROR("Operation not supported\n");
		break;
	}

out:
	return ret;
}

static int sock_pe_peek_hdr(struct sock_pe *pe, 
			     struct sock_pe_entry *pe_entry)
{
	int len;
	struct sock_msg_hdr *msg_hdr;
	struct sock_conn *conn = pe_entry->conn;
	
	if (conn->rx_pe_entry != NULL && conn->rx_pe_entry != pe_entry)
		return -1;
	
	if (conn->rx_pe_entry == NULL) {
		conn->rx_pe_entry = pe_entry;
	}

	len = sizeof(struct sock_msg_hdr);
	msg_hdr = &pe_entry->msg_hdr;
	if (sock_comm_peek(pe_entry->conn, (void*)msg_hdr, len) != len)
		return -1;
	
	msg_hdr->msg_len = ntohll(msg_hdr->msg_len);
	msg_hdr->flags = ntohll(msg_hdr->flags);
	msg_hdr->pe_entry_id = ntohs(msg_hdr->pe_entry_id);
	
	SOCK_LOG_INFO("PE RX (Hdr peek): MsgLen:  %" PRIu64 ", TX-ID: %d, Type: %d\n", 
		      msg_hdr->msg_len, msg_hdr->rx_id, msg_hdr->op_type);
	return 0;
}

static int sock_pe_read_hdr(struct sock_pe *pe, struct sock_rx_ctx *rx_ctx,
			     struct sock_pe_entry *pe_entry)
{
	struct sock_msg_hdr *msg_hdr;
	struct sock_conn *conn = pe_entry->conn;

	if (conn->rx_pe_entry != NULL && conn->rx_pe_entry != pe_entry)
		return 0;

	if (conn->rx_pe_entry == NULL) {
		conn->rx_pe_entry = pe_entry;
	}

	msg_hdr = &pe_entry->msg_hdr;
	if (sock_pe_peek_hdr(pe, pe_entry))
		return 0;
	
	if (msg_hdr->rx_id != rx_ctx->rx_id)
		return -1;
	
	if (sock_pe_recv_field(pe_entry, (void*)msg_hdr, 
			       sizeof(struct sock_msg_hdr), 0)) {
		SOCK_LOG_ERROR("Failed to recv header\n");
		return -1;
	}
	
	msg_hdr->msg_len = ntohll(msg_hdr->msg_len);
	msg_hdr->flags = ntohll(msg_hdr->flags);
	msg_hdr->pe_entry_id = ntohs(msg_hdr->pe_entry_id);
	pe_entry->pe.rx.header_read = 1;
	pe_entry->flags = msg_hdr->flags;
	
	SOCK_LOG_INFO("PE RX (Hdr read): MsgLen:  %" PRIu64 ", TX-ID: %d, Type: %d\n", 
		      msg_hdr->msg_len, msg_hdr->rx_id, msg_hdr->op_type);
	return 0;
}

static int sock_pe_progress_tx_atomic(struct sock_pe *pe, 
				      struct sock_pe_entry *pe_entry, 
				      struct sock_conn *conn)
{
	int datatype_sz;
	union sock_iov iov[SOCK_EP_MAX_IOV_LIMIT];
	ssize_t len, i, entry_len;

	if (pe_entry->pe.tx.send_done)
		return 0;

	len = sizeof(struct sock_msg_hdr);
	entry_len = sizeof(struct sock_atomic_req) - sizeof(struct sock_msg_hdr); 
	if (sock_pe_send_field(pe_entry, &pe_entry->pe.tx.tx_op, entry_len, len))
		return 0;
	len += entry_len;
	
	if (pe_entry->flags & FI_REMOTE_CQ_DATA) {
		if (sock_pe_send_field(pe_entry, &pe_entry->data, 
				       SOCK_CQ_DATA_SIZE, len))
			return 0;
		len += SOCK_CQ_DATA_SIZE;
	}
	
	/* dest iocs */
	entry_len = sizeof(union sock_iov) * pe_entry->pe.tx.tx_op.dest_iov_len;
	for (i=0; i < pe_entry->pe.tx.tx_op.dest_iov_len; i++) {
		iov[i].ioc.addr = pe_entry->pe.tx.data.tx_iov[i].dst.ioc.addr;
		iov[i].ioc.count = pe_entry->pe.tx.data.tx_iov[i].dst.ioc.count;
		iov[i].ioc.key = pe_entry->pe.tx.data.tx_iov[i].dst.ioc.key;
	}

	if (sock_pe_send_field(pe_entry, &iov[0], entry_len, len))
		return 0;
	len += entry_len;
	
	/* cmp data */
	datatype_sz = fi_datatype_size(pe_entry->pe.tx.tx_op.atomic.datatype);
	for (i=0; i < pe_entry->pe.tx.tx_op.atomic.cmp_iov_len; i++) {
		if (sock_pe_send_field(pe_entry, 
				       (void *) (uintptr_t) pe_entry->pe.tx.data.tx_iov[i].cmp.ioc.addr,
				       pe_entry->pe.tx.data.tx_iov[i].cmp.ioc.count * 
				       datatype_sz, len))
			return 0;
		len += (pe_entry->pe.tx.data.tx_iov[i].cmp.ioc.count * datatype_sz);
	}

	/* data */
	if (pe_entry->flags & FI_INJECT) {
		if (sock_pe_send_field(pe_entry, 
				       &pe_entry->pe.tx.data.inject[0],
				       pe_entry->pe.tx.tx_op.src_iov_len, len))
			return 0;
		len += pe_entry->pe.tx.tx_op.src_iov_len;
	} else {
		for (i=0; i < pe_entry->pe.tx.tx_op.src_iov_len; i++) {
			if (sock_pe_send_field(pe_entry,
				    (void *) (uintptr_t) pe_entry->pe.tx.data.tx_iov[i].src.ioc.addr,
				    pe_entry->pe.tx.data.tx_iov[i].src.ioc.count * 
				    datatype_sz, len))
				return 0;
			len += (pe_entry->pe.tx.data.tx_iov[i].src.ioc.count * datatype_sz);
		}
	}
	
	if (pe_entry->done_len == pe_entry->total_len) {
		pe_entry->pe.tx.send_done = 1;
		pe_entry->conn->tx_pe_entry = NULL;
		SOCK_LOG_INFO("Send complete\n");		
	}
	sock_comm_flush(pe_entry->conn);
	pe_entry->msg_hdr.flags = pe_entry->flags;
	return 0;
}

static int sock_pe_progress_tx_write(struct sock_pe *pe, 
				     struct sock_pe_entry *pe_entry, 
				     struct sock_conn *conn)
{
	union sock_iov dest_iov[SOCK_EP_MAX_IOV_LIMIT];
	ssize_t len, i, dest_iov_len;

	if (pe_entry->pe.tx.send_done)
		return 0;

	len = sizeof(struct sock_msg_hdr);
	if (pe_entry->flags & FI_REMOTE_CQ_DATA) {
		if (sock_pe_send_field(pe_entry, &pe_entry->data, 
				       SOCK_CQ_DATA_SIZE, len))
			return 0;
		len += SOCK_CQ_DATA_SIZE;
	}
	
	/* dest iovs */
	dest_iov_len = sizeof(union sock_iov) * pe_entry->pe.tx.tx_op.dest_iov_len;
	for (i=0; i < pe_entry->pe.tx.tx_op.dest_iov_len; i++) {
		dest_iov[i].iov.addr = pe_entry->pe.tx.data.tx_iov[i].dst.iov.addr;
		dest_iov[i].iov.len = pe_entry->pe.tx.data.tx_iov[i].dst.iov.len;
		dest_iov[i].iov.key = pe_entry->pe.tx.data.tx_iov[i].dst.iov.key;
	}
	if (sock_pe_send_field(pe_entry, &dest_iov[0], dest_iov_len, len))
		return 0;
	len += dest_iov_len;
	
	/* data */
	if (pe_entry->flags & FI_INJECT) {
		if (sock_pe_send_field(pe_entry, &pe_entry->pe.tx.data.inject[0],
				       pe_entry->pe.tx.tx_op.src_iov_len, len))
			return 0;
		len += pe_entry->pe.tx.tx_op.src_iov_len;
		pe_entry->data_len = pe_entry->pe.tx.tx_op.src_iov_len;
	} else {
		pe_entry->data_len = 0;
		for (i=0; i < pe_entry->pe.tx.tx_op.src_iov_len; i++) {
			if (sock_pe_send_field(
				    pe_entry,
				    (void *) (uintptr_t) pe_entry->pe.tx.data.tx_iov[i].src.iov.addr,
				    pe_entry->pe.tx.data.tx_iov[i].src.iov.len, len))
				return 0;
			len += pe_entry->pe.tx.data.tx_iov[i].src.iov.len;
			pe_entry->data_len += pe_entry->pe.tx.data.tx_iov[i].src.iov.len;
		}
	}

	if (pe_entry->done_len == pe_entry->total_len) {
		pe_entry->pe.tx.send_done = 1;
		pe_entry->conn->tx_pe_entry = NULL;
		SOCK_LOG_INFO("Send complete\n");		
	}
	sock_comm_flush(pe_entry->conn);
	pe_entry->msg_hdr.flags = pe_entry->flags;
	return 0;
}

static int sock_pe_progress_tx_read(struct sock_pe *pe, 
				    struct sock_pe_entry *pe_entry, 
				    struct sock_conn *conn)
{
	union sock_iov src_iov[SOCK_EP_MAX_IOV_LIMIT];
	ssize_t len, i, src_iov_len;

	if (pe_entry->pe.tx.send_done)
		return 0;

	len = sizeof(struct sock_msg_hdr);

	/* src iovs */		
	src_iov_len = sizeof(union sock_iov) * pe_entry->pe.tx.tx_op.src_iov_len;
	pe_entry->data_len = 0;
	for (i=0; i < pe_entry->pe.tx.tx_op.src_iov_len; i++) {
		src_iov[i].iov.addr = pe_entry->pe.tx.data.tx_iov[i].src.iov.addr;
		src_iov[i].iov.len = pe_entry->pe.tx.data.tx_iov[i].src.iov.len;
		src_iov[i].iov.key = pe_entry->pe.tx.data.tx_iov[i].src.iov.key;
		pe_entry->data_len += pe_entry->pe.tx.data.tx_iov[i].src.iov.len;
	}

	if (sock_pe_send_field(pe_entry, &src_iov[0], src_iov_len, len))
		return 0;
	len += src_iov_len;

	if (pe_entry->done_len == pe_entry->total_len) {
		pe_entry->pe.tx.send_done = 1;
		pe_entry->conn->tx_pe_entry = NULL;
		SOCK_LOG_INFO("Send complete\n");		
	}
	sock_comm_flush(pe_entry->conn);
	pe_entry->msg_hdr.flags = pe_entry->flags;
	return 0;
}


static int sock_pe_progress_tx_send(struct sock_pe *pe, 
				    struct sock_pe_entry *pe_entry, 
				    struct sock_conn *conn)
{
	size_t len, i;
	if (pe_entry->pe.tx.send_done)
		return 0;

	len = sizeof(struct sock_msg_hdr);
	if (pe_entry->pe.tx.tx_op.op == SOCK_OP_TSEND) {
		if (sock_pe_send_field(pe_entry, &pe_entry->tag,
				       SOCK_TAG_SIZE, len))
			return 0;
		len += SOCK_TAG_SIZE;
	}
					 
	if (pe_entry->flags & FI_REMOTE_CQ_DATA) {
		if (sock_pe_send_field(pe_entry, &pe_entry->data,
				       SOCK_CQ_DATA_SIZE, len))
			return 0;
		len += SOCK_CQ_DATA_SIZE;
	}

	if (pe_entry->flags & FI_INJECT) {
		if (sock_pe_send_field(pe_entry, pe_entry->pe.tx.data.inject,
				       pe_entry->pe.tx.tx_op.src_iov_len, len))
			return 0;
		len += pe_entry->pe.tx.tx_op.src_iov_len;
		pe_entry->data_len = pe_entry->pe.tx.tx_op.src_iov_len;
	} else {
		pe_entry->data_len = 0;
		for (i = 0; i < pe_entry->pe.tx.tx_op.src_iov_len; i++) {
			if (sock_pe_send_field(pe_entry,
				    (void *) (uintptr_t) pe_entry->pe.tx.data.tx_iov[i].src.iov.addr,
				    pe_entry->pe.tx.data.tx_iov[i].src.iov.len, len))
				return 0;
			len += pe_entry->pe.tx.data.tx_iov[i].src.iov.len;
			pe_entry->data_len += pe_entry->pe.tx.data.tx_iov[i].src.iov.len;
		}
	}
	
	sock_comm_flush(pe_entry->conn);
	pe_entry->msg_hdr.flags = pe_entry->flags;
	if (pe_entry->done_len == pe_entry->total_len) {
		pe_entry->pe.tx.send_done = 1;
		pe_entry->conn->tx_pe_entry = NULL;
		SOCK_LOG_INFO("Send complete\n");
		
		if (pe_entry->flags & FI_INJECT_COMPLETE) {
			sock_pe_report_tx_completion(pe_entry);
			pe_entry->is_complete = 1;
		}
	}
	
	return 0;
}

static int sock_pe_progress_tx_entry(struct sock_pe *pe,
				     struct sock_tx_ctx *tx_ctx,
				     struct sock_pe_entry *pe_entry)
{
	int ret; 
	struct sock_conn *conn = pe_entry->conn;

	if (!pe_entry->conn || pe_entry->pe.tx.send_done)
		return 0;

	if (conn->tx_pe_entry != NULL && conn->tx_pe_entry != pe_entry) {
		SOCK_LOG_INFO("Cannot progress %p as conn %p is being used by %p\n",
			      pe_entry, conn, conn->tx_pe_entry);
		return 0;
	}

	if (conn->tx_pe_entry == NULL) {
		SOCK_LOG_INFO("Connection %p grabbed by %p\n", conn, pe_entry);
		conn->tx_pe_entry = pe_entry;
	}

	if ((pe_entry->flags & FI_FENCE) && 
	    (tx_ctx->pe_entry_list.next != &pe_entry->ctx_entry)) {
		SOCK_LOG_INFO("Waiting for FI_FENCE\n");
		return 0;
	}

	if (!pe_entry->pe.tx.header_sent) {
		if (sock_pe_send_field(pe_entry, &pe_entry->msg_hdr,
				       sizeof(struct sock_msg_hdr), 0))
			return 0;
		pe_entry->pe.tx.header_sent = 1;
	}

	switch (pe_entry->msg_hdr.op_type) {
	case SOCK_OP_SEND:
	case SOCK_OP_TSEND:
		ret = sock_pe_progress_tx_send(pe, pe_entry, conn);
		break;
	case SOCK_OP_WRITE:
		ret = sock_pe_progress_tx_write(pe, pe_entry, conn);
		break;
	case SOCK_OP_READ:
		ret = sock_pe_progress_tx_read(pe, pe_entry, conn);
		break;
	case SOCK_OP_ATOMIC:
		ret = sock_pe_progress_tx_atomic(pe, pe_entry, conn);
		break;
	default:
		ret = -FI_ENOSYS;
		SOCK_LOG_ERROR("Operation not supported\n");
		break;
	}
	
	return ret;
}

static int sock_pe_progress_rx_pe_entry(struct sock_pe *pe,
					struct sock_pe_entry *pe_entry,
					struct sock_rx_ctx *rx_ctx)
{
	int ret; 

	if (pe_entry->pe.rx.pending_send) {
		sock_pe_progress_pending_ack(pe, pe_entry);
		goto out;
	}

	if (!pe_entry->pe.rx.header_read) {
		if (sock_pe_read_hdr(pe, rx_ctx, pe_entry) == -1) {
			sock_pe_release_entry(pe, pe_entry);
			return 0;
		}
	}
		
	if (pe_entry->pe.rx.header_read) {
		ret = sock_pe_process_recv(pe, rx_ctx, pe_entry);
		if (ret < 0) 
			return ret;
	}
	
out:
	if (pe_entry->is_complete && !pe_entry->pe.rx.pending_send) {
		sock_pe_release_entry(pe, pe_entry);
		SOCK_LOG_INFO("[%p] RX done\n", pe_entry);
	}
	return 0;
}

static int sock_pe_new_rx_entry(struct sock_pe *pe, struct sock_rx_ctx *rx_ctx,
				struct sock_ep *ep, struct sock_conn *conn, 
				int key)
{
	int ret;
	struct sock_pe_entry *pe_entry;	

	pe_entry = sock_pe_acquire_entry(pe);
	if (!pe_entry) {
		SOCK_LOG_INFO("Cannot get PE entry\n");
		return 0;
	}

	memset(&pe_entry->pe.rx, 0, sizeof(pe_entry->pe.rx));

	pe_entry->conn = conn;
	pe_entry->type = SOCK_PE_RX;
	pe_entry->ep = ep;
	pe_entry->is_complete = 0;
	pe_entry->done_len = 0;

	if (ep->ep_type == FI_EP_MSG || !ep->av)
		pe_entry->addr = FI_ADDR_NOTAVAIL;
	else
		pe_entry->addr = sock_av_lookup_key(ep->av, key);

	if (rx_ctx->ctx.fid.fclass == FI_CLASS_SRX_CTX) 
		pe_entry->comp = &ep->comp;
	else
		pe_entry->comp = &rx_ctx->comp;

	SOCK_LOG_INFO("New RX on PE entry %p (%ld)\n", 
		      pe_entry, PE_INDEX(pe, pe_entry));

	SOCK_LOG_INFO("Inserting rx_entry to PE entry %p, conn: %p\n",
		      pe_entry, pe_entry->conn);

	dlist_insert_tail(&pe_entry->ctx_entry, &rx_ctx->pe_entry_list);
	ret = sock_pe_progress_rx_pe_entry(pe, pe_entry, rx_ctx);
	return ret;
}

static int sock_pe_new_tx_entry(struct sock_pe *pe, struct sock_tx_ctx *tx_ctx)
{
	int i, datatype_sz;
	struct sock_msg_hdr *msg_hdr;
	struct sock_pe_entry *pe_entry;
	struct sock_ep *ep;

	pe_entry = sock_pe_acquire_entry(pe);
	if (!pe_entry) {
		SOCK_LOG_INFO("Cannot get free PE entry \n");
		return 0;
	}

	memset(&pe_entry->pe.tx, 0, sizeof(pe_entry->pe.tx));
	memset(&pe_entry->msg_hdr, 0, sizeof(pe_entry->msg_hdr));

	pe_entry->type = SOCK_PE_TX;
	pe_entry->is_complete = 0;
	pe_entry->done_len = 0;
	pe_entry->conn = NULL;
	pe_entry->ep = tx_ctx->ep;
	pe_entry->pe.tx.tx_ctx = tx_ctx;

	dlist_insert_tail(&pe_entry->ctx_entry, &tx_ctx->pe_entry_list);

	/* fill in PE tx entry */
	msg_hdr = &pe_entry->msg_hdr;
	msg_hdr->msg_len = sizeof(*msg_hdr);

	msg_hdr->pe_entry_id = PE_INDEX(pe, pe_entry);
	SOCK_LOG_INFO("New TX on PE entry %p (%d)\n", 
		      pe_entry, msg_hdr->pe_entry_id);

	sock_tx_ctx_read_op_send(tx_ctx, &pe_entry->pe.tx.tx_op,
			&pe_entry->flags, &pe_entry->context, &pe_entry->addr,
			&pe_entry->buf, &ep, &pe_entry->conn);

	if (pe_entry->pe.tx.tx_op.op == SOCK_OP_TSEND) {
		rbfdread(&tx_ctx->rbfd, &pe_entry->tag, sizeof(pe_entry->tag));
		msg_hdr->msg_len += sizeof(pe_entry->tag);
	}

	if (ep && tx_ctx->fid.stx.fid.fclass == FI_CLASS_STX_CTX)
		pe_entry->comp = &ep->comp;
	else
		pe_entry->comp = &tx_ctx->comp;

	if (pe_entry->flags & FI_REMOTE_CQ_DATA) {
		rbfdread(&tx_ctx->rbfd, &pe_entry->data, sizeof(pe_entry->data));
		msg_hdr->msg_len += sizeof(pe_entry->data);
	}

	msg_hdr->op_type = pe_entry->pe.tx.tx_op.op;
	switch (pe_entry->pe.tx.tx_op.op) {
	case SOCK_OP_SEND:
	case SOCK_OP_TSEND:
		if (pe_entry->flags & FI_INJECT) {
			rbfdread(&tx_ctx->rbfd, &pe_entry->pe.tx.data.inject[0],
				 pe_entry->pe.tx.tx_op.src_iov_len);
			msg_hdr->msg_len += pe_entry->pe.tx.tx_op.src_iov_len;
		} else {
			for (i = 0; i < pe_entry->pe.tx.tx_op.src_iov_len; i++) {
				rbfdread(&tx_ctx->rbfd, &pe_entry->pe.tx.data.tx_iov[i].src, 
					 sizeof(pe_entry->pe.tx.data.tx_iov[i].src));
				msg_hdr->msg_len += pe_entry->pe.tx.data.tx_iov[i].src.iov.len;
			}
		}
		break;
	case SOCK_OP_WRITE:
		if (pe_entry->flags & FI_INJECT) {
			rbfdread(&tx_ctx->rbfd, &pe_entry->pe.tx.data.inject[0],
				 pe_entry->pe.tx.tx_op.src_iov_len);
			msg_hdr->msg_len += pe_entry->pe.tx.tx_op.src_iov_len;
		} else {
			for (i = 0; i < pe_entry->pe.tx.tx_op.src_iov_len; i++) {
				rbfdread(&tx_ctx->rbfd, &pe_entry->pe.tx.data.tx_iov[i].src, 
					 sizeof(pe_entry->pe.tx.data.tx_iov[i].src));
				msg_hdr->msg_len += pe_entry->pe.tx.data.tx_iov[i].src.iov.len;
			}
		}

		for (i = 0; i < pe_entry->pe.tx.tx_op.dest_iov_len; i++) {
			rbfdread(&tx_ctx->rbfd, &pe_entry->pe.tx.data.tx_iov[i].dst, 
				 sizeof(pe_entry->pe.tx.data.tx_iov[i].dst));
		}
		msg_hdr->msg_len += sizeof(union sock_iov) * i;
		break;
	case SOCK_OP_READ:
		for (i = 0; i < pe_entry->pe.tx.tx_op.src_iov_len; i++) {
			rbfdread(&tx_ctx->rbfd, &pe_entry->pe.tx.data.tx_iov[i].src, 
				 sizeof(pe_entry->pe.tx.data.tx_iov[i].src));
		}
		msg_hdr->msg_len += sizeof(union sock_iov) * i;

		for (i = 0;  i <pe_entry->pe.tx.tx_op.dest_iov_len; i++) {
			rbfdread(&tx_ctx->rbfd, &pe_entry->pe.tx.data.tx_iov[i].dst, 
				 sizeof(pe_entry->pe.tx.data.tx_iov[i].dst));
		}
		break;
	case SOCK_OP_ATOMIC:			
		msg_hdr->msg_len += sizeof(struct sock_op);
		datatype_sz = fi_datatype_size(pe_entry->pe.tx.tx_op.atomic.datatype);
		if (pe_entry->flags & FI_INJECT) {
			rbfdread(&tx_ctx->rbfd, &pe_entry->pe.tx.data.inject[0],
				 pe_entry->pe.tx.tx_op.src_iov_len);
			msg_hdr->msg_len += pe_entry->pe.tx.tx_op.src_iov_len;
		} else {
			for (i = 0; i < pe_entry->pe.tx.tx_op.src_iov_len; i++) {
				rbfdread(&tx_ctx->rbfd, &pe_entry->pe.tx.data.tx_iov[i].src, 
					 sizeof(pe_entry->pe.tx.data.tx_iov[i].src));
				msg_hdr->msg_len += datatype_sz *
					pe_entry->pe.tx.data.tx_iov[i].src.ioc.count;
			}
		}

		for (i = 0; i < pe_entry->pe.tx.tx_op.dest_iov_len; i++) {
			rbfdread(&tx_ctx->rbfd, &pe_entry->pe.tx.data.tx_iov[i].dst, 
				 sizeof(pe_entry->pe.tx.data.tx_iov[i].dst));
		}
		msg_hdr->msg_len += sizeof(union sock_iov) * i;

		for (i = 0; i < pe_entry->pe.tx.tx_op.atomic.res_iov_len; i++) {
			rbfdread(&tx_ctx->rbfd, &pe_entry->pe.tx.data.tx_iov[i].res, 
				 sizeof(pe_entry->pe.tx.data.tx_iov[i].res));
		}
		
		for (i = 0; i < pe_entry->pe.tx.tx_op.atomic.cmp_iov_len; i++) {
			rbfdread(&tx_ctx->rbfd, &pe_entry->pe.tx.data.tx_iov[i].cmp, 
				 sizeof(pe_entry->pe.tx.data.tx_iov[i].cmp));
			msg_hdr->msg_len += datatype_sz *
				pe_entry->pe.tx.data.tx_iov[i].cmp.ioc.count;
		}
		break;
	default:
		SOCK_LOG_ERROR("Invalid operation type\n");
		return -FI_EINVAL;
	}

	SOCK_LOG_INFO("Inserting TX-entry to PE entry %p, conn: %p\n",
		      pe_entry, pe_entry->conn);
	
	/* prepare message header */
	msg_hdr->version = SOCK_WIRE_PROTO_VERSION;

	if (tx_ctx->av) {
		msg_hdr->rx_id = (uint16_t) SOCK_GET_RX_ID(pe_entry->addr,
							   tx_ctx->av->rx_ctx_bits);
	} else {
		msg_hdr->rx_id = 0;
	}

	if (pe_entry->flags & FI_INJECT_COMPLETE) {
		pe_entry->flags &= ~FI_TRANSMIT_COMPLETE;
	}

	msg_hdr->dest_iov_len = pe_entry->pe.tx.tx_op.dest_iov_len;
	msg_hdr->flags = htonll(pe_entry->flags);
	pe_entry->total_len = msg_hdr->msg_len;
	msg_hdr->msg_len = htonll(msg_hdr->msg_len);
	msg_hdr->pe_entry_id = htons(msg_hdr->pe_entry_id);
	return sock_pe_progress_tx_entry(pe, tx_ctx, pe_entry);
}

void sock_pe_add_tx_ctx(struct sock_pe *pe, struct sock_tx_ctx *ctx)
{
	pthread_mutex_lock(&pe->list_lock);
	dlistfd_insert_tail(&ctx->pe_entry, &pe->tx_list);
	pthread_mutex_unlock(&pe->list_lock);
	SOCK_LOG_INFO("TX ctx added to PE\n");
}

void sock_pe_add_rx_ctx(struct sock_pe *pe, struct sock_rx_ctx *ctx)
{
	pthread_mutex_lock(&pe->list_lock);
	dlistfd_insert_tail(&ctx->pe_entry, &pe->rx_list);
	pthread_mutex_unlock(&pe->list_lock);
	SOCK_LOG_INFO("RX ctx added to PE\n");
}

void sock_pe_remove_tx_ctx(struct sock_tx_ctx *tx_ctx)
{
	pthread_mutex_lock(&tx_ctx->domain->pe->list_lock);
	dlist_remove(&tx_ctx->pe_entry);
	pthread_mutex_unlock(&tx_ctx->domain->pe->list_lock);
}

void sock_pe_remove_rx_ctx(struct sock_rx_ctx *rx_ctx)
{
	pthread_mutex_lock(&rx_ctx->domain->pe->list_lock);
	dlist_remove(&rx_ctx->pe_entry);
	pthread_mutex_unlock(&rx_ctx->domain->pe->list_lock);
}

static int sock_pe_progress_rx_ep(struct sock_pe *pe, struct sock_ep *ep,
			   struct sock_rx_ctx *rx_ctx)
{
	struct sock_conn *conn;
	struct sock_conn_map *map;
	int i, ret, data_avail;
	
	map = &ep->domain->r_cmap;
	assert(map != NULL);

	for (i = 0; i < map->used; i++) {
		conn = &map->table[i];
		
		if (rbused(&conn->outbuf))
			sock_comm_flush(conn);
		
		if (ep != conn->ep)
			continue;

		data_avail = 0;
		if (rbused(&conn->inbuf) > 0) {
			data_avail = 1;
		} else {
			ret = fi_poll_fd(conn->sock_fd, 0);
			if (ret < 0 && errno != EINTR) {
				SOCK_LOG_INFO("Error polling fd: %d\n", 
					      conn->sock_fd);
				return ret;
			}
			data_avail = (ret == 1 && sock_comm_data_avail(conn));
		}
		
		if (data_avail && conn->rx_pe_entry == NULL &&
		    !dlist_empty(&pe->free_list)) {
			/* new RX PE entry */
			ret = sock_pe_new_rx_entry(pe, rx_ctx, ep, conn, i);
			if (ret < 0)
				return ret;
		}
	}
	return 0;
}

int sock_pe_progress_rx_ctx(struct sock_pe *pe, struct sock_rx_ctx *rx_ctx)
{
	int ret = 0;
	struct sock_ep *ep;
	struct dlist_entry *entry;
	struct sock_pe_entry *pe_entry;

	if (fastlock_acquire(&pe->lock))
		return 0;

	fastlock_acquire(&rx_ctx->lock);
	sock_pe_progress_buffered_rx(rx_ctx);
	fastlock_release(&rx_ctx->lock);

	/* check for incoming data */
	if (rx_ctx->ctx.fid.fclass == FI_CLASS_SRX_CTX) {
		for (entry = rx_ctx->ep_list.next;
		     entry != &rx_ctx->ep_list; ) {
			
			ep = container_of(entry, struct sock_ep, rx_ctx_entry);
			entry = entry->next;
			ret = sock_pe_progress_rx_ep(pe, ep, rx_ctx);
			if (ret < 0)
				goto out;
		}
	} else {
		ep = rx_ctx->ep;
		ret = sock_pe_progress_rx_ep(pe, ep, rx_ctx);
		if (ret < 0)
			goto out;
	}

	for (entry = rx_ctx->pe_entry_list.next;
	     entry != &rx_ctx->pe_entry_list;) {
		
		pe_entry = container_of(entry, struct sock_pe_entry, ctx_entry);
		entry = entry->next;
		ret = sock_pe_progress_rx_pe_entry(pe, pe_entry, rx_ctx);
		if (ret < 0)
			goto out;
	}
		
out:	
	if (ret < 0) 
		SOCK_LOG_ERROR("failed to progress RX ctx\n");
	fastlock_release(&pe->lock);
	return ret;
}

int sock_pe_progress_tx_ctx(struct sock_pe *pe, struct sock_tx_ctx *tx_ctx)
{
	int ret = 0;
	struct dlist_entry *entry;
	struct sock_pe_entry *pe_entry;

	if (fastlock_acquire(&pe->lock))
		return 0;

	fastlock_acquire(&tx_ctx->rlock);
	if (!rbfdempty(&tx_ctx->rbfd) && 
	    pe->num_free_entries > SOCK_PE_MIN_ENTRIES) {
		ret = sock_pe_new_tx_entry(pe, tx_ctx);
	}
	fastlock_release(&tx_ctx->rlock);
	if (ret < 0)
		goto out;

	/* progress tx_ctx in PE table */
	for (entry = tx_ctx->pe_entry_list.next;
	     entry != &tx_ctx->pe_entry_list;) {
		
		pe_entry = container_of(entry, struct sock_pe_entry, ctx_entry);
		entry = entry->next;

		ret = sock_pe_progress_tx_entry(pe, tx_ctx, pe_entry);
		if (ret < 0) {
			SOCK_LOG_ERROR("Error in progressing %p\n", pe_entry);
			goto out;
		}
		
		if (pe_entry->is_complete) {
			sock_pe_release_entry(pe, pe_entry);
			SOCK_LOG_INFO("[%p] TX done\n", pe_entry);
		}
	}
		
out:	
	if (ret < 0) 
		SOCK_LOG_ERROR("failed to progress TX ctx\n");
	fastlock_release(&pe->lock);
	return ret;
}

static void *sock_pe_progress_thread(void *data)
{
	int ret;
	struct dlist_entry *entry;
	struct sock_tx_ctx *tx_ctx;
	struct sock_rx_ctx *rx_ctx;
	struct sock_pe *pe = (struct sock_pe *)data;

	SOCK_LOG_INFO("Progress thread started\n");
	while (*((volatile int*)&pe->do_progress)) {

		/* FIXME */
		if (sock_progress_thread_wait) {
			pthread_yield();
			usleep(sock_progress_thread_wait * 1000);
		}

		pthread_mutex_lock(&pe->list_lock);		
		if (!dlistfd_empty(&pe->tx_list)) {
			for (entry = pe->tx_list.list.next;
			     entry != &pe->tx_list.list; entry = entry->next) {
				tx_ctx = container_of(entry, struct sock_tx_ctx,
						      pe_entry);
				ret = sock_pe_progress_tx_ctx(pe, tx_ctx);
				if (ret < 0) {
					SOCK_LOG_ERROR("failed to progress TX\n");
					pthread_mutex_unlock(&pe->list_lock);
					return NULL;
				}
			}
		}

		if (!dlistfd_empty(&pe->rx_list)) {
			for (entry = pe->rx_list.list.next;
			     entry != &pe->rx_list.list; entry = entry->next) {
				rx_ctx = container_of(entry, struct sock_rx_ctx,
						      pe_entry);
				ret = sock_pe_progress_rx_ctx(pe, rx_ctx);
				if (ret < 0) {
					SOCK_LOG_ERROR("failed to progress RX\n");
					pthread_mutex_unlock(&pe->list_lock);
					return NULL;
				}
			}
		}
		pthread_mutex_unlock(&pe->list_lock);
	}
	
	SOCK_LOG_INFO("Progress thread terminated\n");
	return NULL;
}

static void sock_pe_init_table(struct sock_pe *pe)
{
	int i;
	
	memset(&pe->pe_table, 0, 
	       sizeof(struct sock_pe_entry) * SOCK_PE_MAX_ENTRIES);

	dlist_init(&pe->free_list);
	dlist_init(&pe->busy_list);

	for (i=0; i<SOCK_PE_MAX_ENTRIES; i++) {
		dlist_insert_head(&pe->pe_table[i].entry, &pe->free_list);
	}

	pe->num_free_entries = SOCK_PE_MAX_ENTRIES;
	SOCK_LOG_INFO("PE table init: OK\n");
}

struct sock_pe *sock_pe_init(struct sock_domain *domain)
{
	struct sock_pe *pe;

	pe = calloc(1, sizeof(*pe));
	if (!pe)
		return NULL;

	sock_pe_init_table(pe);
	dlistfd_head_init(&pe->tx_list);
	dlistfd_head_init(&pe->rx_list);
	fastlock_init(&pe->lock);
	pthread_mutex_init(&pe->list_lock, NULL);
	pe->domain = domain;

	if (domain->progress_mode == FI_PROGRESS_AUTO) {
		pe->do_progress = 1;
		if (pthread_create(&pe->progress_thread, NULL, 
				   sock_pe_progress_thread, (void *)pe)) {
			SOCK_LOG_ERROR("Couldn't create progress thread\n");
			goto err;
		}
	}
	SOCK_LOG_INFO("PE init: OK\n");
	return pe;

err:
	fastlock_destroy(&pe->lock);
	dlistfd_head_free(&pe->tx_list);
	dlistfd_head_free(&pe->rx_list);
	free(pe);
	return NULL;
}

void sock_pe_finalize(struct sock_pe *pe)
{
	if (pe->domain->progress_mode == FI_PROGRESS_AUTO) {
		pe->do_progress = 0;
		pthread_join(pe->progress_thread, NULL);
	}
	
	fastlock_destroy(&pe->lock);
	pthread_mutex_destroy(&pe->list_lock);
	dlistfd_head_free(&pe->tx_list);
	dlistfd_head_free(&pe->rx_list);
	free(pe);
	SOCK_LOG_INFO("Progress engine finalize: OK\n");
}

