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

#include "sock.h"
#include "sock_util.h"


#define PE_INDEX(_pe, _e) (_e - &_pe->pe_table[0])
#define SOCK_GET_RX_ID(_addr, _bits) (((uint64_t)_addr) >> (64 - _bits))


static void sock_pe_release_entry(struct sock_pe *pe, 
				  struct sock_pe_entry *pe_entry)
{
	dlist_remove(&pe_entry->ctx_entry);

	if (pe_entry->type == SOCK_PE_TX)
		pe_entry->conn->tx_pe_entry = NULL;
	else
		pe_entry->conn->rx_pe_entry = NULL;

	pe_entry->conn = NULL;
	memset(&pe_entry->rx, 0, sizeof(struct sock_rx_pe_entry));
	memset(&pe_entry->tx, 0, sizeof(struct sock_tx_pe_entry));

	pe_entry->type =0;
	pe_entry->is_complete = 0;
	pe_entry->done_len = 0;
	pe_entry->total_len = 0;
	pe_entry->data_len = 0;
	pe_entry->buf = 0;

	dlist_remove(&pe_entry->entry);
	dlist_insert_tail(&pe_entry->entry, &pe->free_list);
	SOCK_LOG_INFO("progress entry %p released\n", pe_entry);
}

static struct sock_pe_entry *sock_pe_acquire_entry(struct sock_pe *pe)
{
	struct dlist_entry *entry;
	struct sock_pe_entry *pe_entry;

	entry = pe->free_list.next;
	pe_entry = container_of(entry, struct sock_pe_entry, entry);
	dlist_remove(&pe_entry->entry);
	dlist_insert_tail(&pe_entry->entry, &pe->busy_list);
	SOCK_LOG_INFO("progress entry %p acquired \n", pe_entry);
	return pe_entry;
}

static void sock_pe_report_tx_completion(struct sock_pe_entry *pe_entry)
{
	int ret1 = 0, ret2 = 0;

	if (pe_entry->comp->send_cq && 
	    (!pe_entry->comp->send_cq_event || 
	    (pe_entry->comp->send_cq_event && 
	     (pe_entry->msg_hdr.flags & FI_COMPLETION)))) 
		ret1 = pe_entry->comp->send_cq->report_completion(
			pe_entry->comp->send_cq, pe_entry->addr, pe_entry);

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
				-FI_ENOSPC, -FI_ENOSPC, NULL);
		}				
	}
}

static void sock_pe_report_rx_completion(struct sock_pe_entry *pe_entry)
{
	int ret1 = 0, ret2 = 0;

	if (pe_entry->comp->recv_cq && 
	    (!pe_entry->comp->recv_cq_event || 
	    (pe_entry->comp->recv_cq_event && 
	     (pe_entry->msg_hdr.flags & FI_COMPLETION)))) 
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
				-FI_ENOSPC, -FI_ENOSPC, NULL);
		}
	}
}

void sock_pe_report_mr_completion(struct sock_domain *domain,
				  struct sock_pe_entry *pe_entry)
{
	int i;
	struct sock_mr *mr;

	for (i = 0; i < pe_entry->msg_hdr.dest_iov_len; i++) {
		mr = sock_mr_get_entry(domain, pe_entry->rx.rx_iov[i].iov.key);
		if (!mr || (!mr->cq && !mr->cntr))
			continue;
		
		pe_entry->buf = pe_entry->rx.rx_iov[i].iov.addr;
		pe_entry->data_len = pe_entry->rx.rx_iov[i].iov.len;
		
		if (mr->cq)
			mr->cq->report_completion(mr->cq, 
						  pe_entry->addr, pe_entry);
		if (mr->cntr)
			sock_cntr_inc(mr->cntr);
	}
}

void sock_pe_report_remote_write(struct sock_rx_ctx *rx_ctx,
				 struct sock_pe_entry *pe_entry)
{
	pe_entry->buf = pe_entry->rx.rx_iov[0].iov.addr;
	pe_entry->data_len = pe_entry->rx.rx_iov[0].iov.len;
	
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

void sock_pe_report_remote_read(struct sock_rx_ctx *rx_ctx,
				struct sock_pe_entry *pe_entry)
{
	pe_entry->buf = pe_entry->rx.rx_iov[0].iov.addr;
	pe_entry->data_len = pe_entry->rx.rx_iov[0].iov.len;
	
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
	int ret, offset, len, data_len, done_data, i;
	struct sock_conn *conn = pe_entry->conn;

	assert(conn);
	if (conn->tx_pe_entry != NULL && conn->tx_pe_entry != pe_entry) {
		SOCK_LOG_INFO("Cannot progress %p as conn %p is being used by %p\n",
			      pe_entry, conn, conn->tx_pe_entry);
		return;
	}

	if (conn->tx_pe_entry == NULL) {
		SOCK_LOG_INFO("Connection %p grabbed by %p\n", conn, pe_entry);
		conn->tx_pe_entry = pe_entry;
	}

	len = sizeof(struct sock_msg_response);
	if (pe_entry->done_len < len) {
		offset = pe_entry->done_len;
		
		ret = sock_comm_send(conn, 
				     (char*)&pe_entry->rx.response + offset,
				     sizeof(struct sock_msg_response) - offset);
		if (ret <= 0) 
			return;
		pe_entry->done_len += ret;
		if (pe_entry->done_len != len)
			return;
	}

	switch (pe_entry->rx.response.msg_hdr.op_type) {
	case SOCK_OP_READ_COMPLETE:

		done_data = pe_entry->done_len - len;
		
		for (i = 0; i < pe_entry->msg_hdr.dest_iov_len; i++) {
			if (done_data >= pe_entry->rx.rx_iov[i].iov.len) {
				done_data -= pe_entry->rx.rx_iov[i].iov.len;
				continue;
			}

			offset = done_data;
			data_len = pe_entry->rx.rx_iov[i].iov.len - done_data;

			ret = sock_comm_send(conn, 
					     (char*)pe_entry->rx.rx_iov[i].iov.addr 
					     + offset, data_len);
			if (ret <= 0)
				return;
			done_data = 0;
			pe_entry->done_len += ret;
			if (ret != data_len)
				return;
		}
		
		break;

	case SOCK_OP_ATOMIC_COMPLETE:

		offset = pe_entry->done_len - len;
		data_len = (pe_entry->total_len - len) - offset;

		if (data_len) {
			ret = sock_comm_send(conn, 
					     (char*)&pe_entry->rx.atomic_cmp[0] + offset,
					     data_len);
			if (ret <= 0)
				return;
			pe_entry->done_len += ret;
			if (ret != data_len)
				return;
		}
		break;
		
	default:
		break;
	}

	if (pe_entry->total_len == pe_entry->done_len) {
		pe_entry->is_complete = 1;
		pe_entry->rx.pending_send = 0;
		sock_comm_flush(pe_entry->conn);
		pe_entry->conn->tx_pe_entry = NULL;
	}
}

static void sock_pe_send_response(struct sock_pe *pe, 
				  struct sock_pe_entry *pe_entry, 
				  size_t data_len, uint8_t op_type)
{
	struct sock_msg_response *response = &pe_entry->rx.response;
	memset(response, 0, sizeof(struct sock_msg_response));

	response->pe_entry_id = htons(pe_entry->msg_hdr.pe_entry_id);
	response->msg_hdr.dest_iov_len = 0;
	response->msg_hdr.flags = 0;
	response->msg_hdr.msg_len = sizeof(*response) + data_len;
	response->msg_hdr.version = SOCK_WIRE_PROTO_VERSION;
	response->msg_hdr.op_type = op_type;
	response->msg_hdr.msg_len = htonll(response->msg_hdr.msg_len);
	response->msg_hdr.rx_id = htons(pe_entry->msg_hdr.rx_id);

	pe->pe_atomic = NULL;
	pe_entry->done_len = 0;
	pe_entry->rx.pending_send = 1;
	pe_entry->conn->rx_pe_entry = NULL;
	pe_entry->total_len = sizeof(*response) + data_len;

	sock_pe_progress_pending_ack(pe, pe_entry);
}

static int sock_pe_handle_ack(struct sock_pe *pe, struct sock_pe_entry *pe_entry)
{
	struct sock_pe_entry *waiting_entry;
	struct sock_msg_response response;
	int ret, len, offset, data_len;

	len = sizeof(struct sock_msg_hdr);
	offset = pe_entry->done_len - len;
	data_len = sizeof(response) - len;
	
	ret = sock_comm_recv(pe_entry->conn,
			     (char*)&response.pe_entry_id + offset, 
			     data_len - offset);
	if (ret <= 0)
		return ret;
	
	pe_entry->done_len += ret;
	if (pe_entry->done_len != sizeof(response))
		return 0;

	response.pe_entry_id = ntohs(response.pe_entry_id);
	assert(response.pe_entry_id <= SOCK_PE_MAX_ENTRIES);
	waiting_entry = &pe->pe_table[response.pe_entry_id];
	SOCK_LOG_INFO("Received ack for PE entry %p (index: %d)\n", 
		      waiting_entry, response.pe_entry_id);

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
	struct sock_msg_response response;
	int ret, len, offset, done_data, i, data_len;

	len = sizeof(struct sock_msg_hdr);
	offset = pe_entry->done_len - len;
	data_len = sizeof(response) - len;
	len += data_len;

	if (pe_entry->done_len < len) {
		ret = sock_comm_recv(pe_entry->conn,
				     (char*)&response.pe_entry_id + offset, 
				     data_len - offset);
		if (ret <= 0)
			return ret;
	
		pe_entry->done_len += ret;
		if (pe_entry->done_len != len)
			return 0;

		response.pe_entry_id = ntohs(response.pe_entry_id);
		assert(response.pe_entry_id <= SOCK_PE_MAX_ENTRIES);
		waiting_entry = &pe->pe_table[response.pe_entry_id];
		SOCK_LOG_INFO("Received read complete for PE entry %p (index: %d)\n", 
			      waiting_entry, response.pe_entry_id);
	}

	waiting_entry = &pe->pe_table[response.pe_entry_id];
	assert(waiting_entry->type == SOCK_PE_TX);

	done_data = pe_entry->done_len - len;
	for (i=0; i < waiting_entry->tx.tx_op.dest_iov_len; i++) {

		if (done_data >= waiting_entry->tx.tx_iov[i].dst.iov.len) {
			done_data -= waiting_entry->tx.tx_iov[i].dst.iov.len;
			continue;
		}

		data_len = waiting_entry->tx.tx_iov[i].dst.iov.len - done_data;
		offset = done_data;

		ret = sock_comm_recv(pe_entry->conn, 
				     (char*)waiting_entry->tx.tx_iov[i].dst.iov.addr + 
				     offset, data_len);
		if (ret <= 0) 
			return 0;
			
		done_data = 0;
		pe_entry->done_len += ret;
		if ( ret != data_len)
			return 0;
	}

	sock_pe_report_tx_completion(waiting_entry);
	waiting_entry->is_complete = 1;
	pe_entry->is_complete = 1;
	return 0;
}

static int sock_pe_handle_atomic_complete(struct sock_pe *pe, 
					  struct sock_pe_entry *pe_entry)
{
	size_t datatype_sz;
	struct sock_pe_entry *waiting_entry;
	struct sock_msg_response response;
	int ret, len, offset, done_data, i, data_len;

	len = sizeof(struct sock_msg_hdr);
	offset = pe_entry->done_len - len;
	data_len = sizeof(response) - len;
	len += data_len;

	if (pe_entry->done_len < len) {
		ret = sock_comm_recv(pe_entry->conn,
				     (char*)&response.pe_entry_id + offset, 
				     data_len - offset);
		if (ret <= 0)
			return ret;
		
		pe_entry->done_len += ret;
		if (pe_entry->done_len != len)
			return 0;
		
		response.pe_entry_id = ntohs(response.pe_entry_id);
		assert(response.pe_entry_id <= SOCK_PE_MAX_ENTRIES);
		waiting_entry = &pe->pe_table[response.pe_entry_id];
		SOCK_LOG_INFO("Received atomic complete for PE entry %p (index: %d)\n", 
			      waiting_entry, response.pe_entry_id);
	}
	
	waiting_entry = &pe->pe_table[response.pe_entry_id];
	assert(waiting_entry->type == SOCK_PE_TX);
	
	done_data = pe_entry->done_len - len;
	datatype_sz = fi_datatype_size(waiting_entry->tx.tx_op.atomic.datatype);

	for (i=0; i < waiting_entry->tx.tx_op.atomic.res_iov_len; i++) {
		if (done_data >= waiting_entry->tx.tx_iov[i].res.ioc.count * 
		    datatype_sz) {
			done_data -= waiting_entry->tx.tx_iov[i].res.ioc.count * datatype_sz;
			continue;
		}
		
		data_len = (waiting_entry->tx.tx_iov[i].res.ioc.count * datatype_sz) -
			done_data;
		offset = done_data;
		ret = sock_comm_recv(pe_entry->conn, 
				     (char*)waiting_entry->tx.tx_iov[i].res.ioc.addr + 
				     offset, data_len);
		if (ret <= 0) 
			return 0;
			
		done_data = 0;
		pe_entry->done_len += ret;
		if ( ret != data_len)
			return 0;
	}

	sock_pe_report_tx_completion(waiting_entry);
	waiting_entry->is_complete = 1;
	pe_entry->is_complete = 1;
	return 0;
}


static int sock_pe_process_rx_read(struct sock_pe *pe, struct sock_rx_ctx *rx_ctx,
				   struct sock_pe_entry *pe_entry)
{
	int i, ret;
	struct sock_mr *mr;
	uint64_t offset, len, entry_len, data_len;

	offset = 0;
	len = sizeof(struct sock_msg_hdr);

	entry_len = sizeof(union sock_iov) * pe_entry->msg_hdr.dest_iov_len;
	offset = pe_entry->done_len - len;
	len += entry_len;

	if (pe_entry->done_len < len) {
		ret = sock_comm_recv(pe_entry->conn,
				     (char *)&pe_entry->rx.rx_iov[0] + offset,
				     entry_len - offset);
		if (ret <= 0)
			return ret;
		pe_entry->done_len += ret;
		if (ret != entry_len - offset) {
			SOCK_LOG_INFO("Incomplete Recv: %d\n", ret);
			return 0;
		}
	} else {
		return 0;
	}

	if (pe_entry->done_len != len)
		return 0;

	/* verify mr */
	data_len = 0;
	for (i = 0; i < pe_entry->msg_hdr.dest_iov_len; i++) {
		
		mr = sock_mr_verify_key(rx_ctx->domain, 
					pe_entry->rx.rx_iov[i].iov.key,
					(void*)pe_entry->rx.rx_iov[i].iov.addr,
					pe_entry->rx.rx_iov[i].iov.len,
					FI_REMOTE_READ);
		if (!mr) {
			SOCK_LOG_ERROR("Remote memory access error: %p, %lu, %lu\n",
				       (void*)pe_entry->rx.rx_iov[i].iov.addr,
				       pe_entry->rx.rx_iov[i].iov.len,
				       pe_entry->rx.rx_iov[i].iov.key);
			sock_pe_send_response(pe, pe_entry, 0, SOCK_OP_READ_ERROR);
			return -FI_EINVAL;
		}
		
		if (mr->flags & FI_MR_OFFSET)
			pe_entry->rx.rx_iov[i].iov.addr += mr->offset;
		data_len += pe_entry->rx.rx_iov[i].iov.len;
	}

	pe_entry->buf = pe_entry->rx.rx_iov[0].iov.addr;
	pe_entry->data_len = data_len;

	if (pe_entry->flags & FI_REMOTE_SIGNAL) {
		sock_pe_report_rx_completion(pe_entry);
	}

	sock_pe_report_remote_read(rx_ctx, pe_entry);
	sock_pe_send_response(pe, pe_entry, data_len, 
				       SOCK_OP_READ_COMPLETE);	
	return ret;
}

static int sock_pe_process_rx_write(struct sock_pe *pe, struct sock_rx_ctx *rx_ctx,
				   struct sock_pe_entry *pe_entry)
{
	int i, ret = 0;
	struct sock_mr *mr;
	uint64_t offset, rem, len, entry_len, done_data, data_len;

	offset = 0;
	len = sizeof(struct sock_msg_hdr);
	if (pe_entry->msg_hdr.flags & FI_REMOTE_CQ_DATA) {
		offset = pe_entry->done_len - len;
		len += SOCK_CQ_DATA_SIZE;
		if (pe_entry->done_len < len) {
			ret = sock_comm_recv(pe_entry->conn, 
					   (char*)&pe_entry->data + offset, 
					   SOCK_CQ_DATA_SIZE - offset);
			if (ret <= 0)
				return ret;
			pe_entry->done_len += ret;
			if (pe_entry->done_len != len)
				return 0;
		}
	}

	entry_len = sizeof(union sock_iov) * pe_entry->msg_hdr.dest_iov_len;
	offset = pe_entry->done_len - len;
	len += entry_len;
	if (pe_entry->done_len < len) {

		ret = sock_comm_recv(pe_entry->conn,
				   (char *)&pe_entry->rx.rx_iov[0] + offset,
				   entry_len - offset);
		if (ret <= 0)
			return ret;
		pe_entry->done_len += ret;
		if (ret != entry_len - offset) {
			SOCK_LOG_INFO("Incomplete Recv: %d\n", ret);
			return 0;
		}
	}

	done_data = pe_entry->done_len - len;
	rem = pe_entry->msg_hdr.msg_len - (len + done_data);

	for (i = 0; rem > 0 && i < pe_entry->msg_hdr.dest_iov_len; i++) {
		
		if (done_data >= pe_entry->rx.rx_iov[i].iov.len) {
			done_data -= pe_entry->rx.rx_iov[i].iov.len;
			continue;
		}

		data_len = pe_entry->rx.rx_iov[i].iov.len - done_data;
		offset = done_data;

		mr = sock_mr_verify_key(rx_ctx->domain, 
					pe_entry->rx.rx_iov[i].iov.key,
					(void*)pe_entry->rx.rx_iov[i].iov.addr,
					pe_entry->rx.rx_iov[i].iov.len,
					FI_REMOTE_WRITE);
		if (!mr) {
			SOCK_LOG_ERROR("Remote memory access error: %p, %lu, %lu\n",
				       (void*)pe_entry->rx.rx_iov[i].iov.addr,
				       pe_entry->rx.rx_iov[i].iov.len,
				       pe_entry->rx.rx_iov[i].iov.key);
			sock_pe_send_response(pe, pe_entry, 0, SOCK_OP_WRITE_ERROR);
			break;
		}
		if (mr->flags & FI_MR_OFFSET)
			pe_entry->rx.rx_iov[i].iov.addr += mr->offset;

		ret = sock_comm_recv(pe_entry->conn,
				     (char*)pe_entry->rx.rx_iov[i].iov.addr + offset,
				     data_len);
		if (ret <= 0)
			return ret;

		done_data = 0;
		rem -= ret;
		pe_entry->done_len += ret;
		if (ret != data_len){
			SOCK_LOG_INFO("Incomplete Recv\n");
			return 0;
		}
	}
	pe_entry->buf = pe_entry->rx.rx_iov[0].iov.addr;

	pe_entry->data_len = 0;
	for (i = 0; i < pe_entry->msg_hdr.dest_iov_len; i++) {
		pe_entry->data_len += pe_entry->rx.rx_iov[i].iov.len;
	}
				   
	/* report error, if any */
	if (rem) {
		sock_pe_report_error(pe_entry, rem);
		goto out;
	} else {
		if (pe_entry->flags & FI_REMOTE_SIGNAL) {
			sock_pe_report_rx_completion(pe_entry);
		}
	}

out:
	sock_pe_report_remote_write(rx_ctx, pe_entry);
	sock_pe_report_mr_completion(rx_ctx->domain, pe_entry);
	sock_pe_send_response(pe, pe_entry, 0, SOCK_OP_WRITE_COMPLETE);	
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
	uint64_t offset, len, entry_len, data_len;
	

	if (pe->pe_atomic){
		if (pe->pe_atomic != pe_entry)
			return 0;
	} else {
		pe->pe_atomic = pe_entry;
	}

	len = sizeof(struct sock_msg_hdr);
	offset = pe_entry->done_len - len;
	len = sizeof(struct sock_atomic_req);

	if (pe_entry->done_len < len) {
		data_len = sizeof(struct sock_atomic_req) - pe_entry->done_len;
		ret = sock_comm_recv(pe_entry->conn,
				     (char *)&pe_entry->rx.rx_op + offset,
				     data_len);
		if (ret <= 0)
			return ret;
		pe_entry->done_len += ret;
		if (pe_entry->done_len != len)
			return 0;
	}

	if (pe_entry->msg_hdr.flags & FI_REMOTE_CQ_DATA) {
		offset = pe_entry->done_len - len;
		len += SOCK_CQ_DATA_SIZE;
		data_len = SOCK_CQ_DATA_SIZE - offset;
		if (pe_entry->done_len < len) {
			ret = sock_comm_recv(pe_entry->conn, 
					     (char*)&pe_entry->data + offset, 
					     data_len);
			if (ret <= 0)
				return ret;
			pe_entry->done_len += ret;
			if (pe_entry->done_len != len)
				return 0;
		}
	}

	/* dst iocs */
	entry_len = sizeof(union sock_iov) * pe_entry->rx.rx_op.dest_iov_len;
	offset = pe_entry->done_len - len;
	len += entry_len;
	if (pe_entry->done_len < len) {
		data_len = entry_len - offset;
		ret = sock_comm_recv(pe_entry->conn,
				     (char *)&pe_entry->rx.rx_iov[0] + offset,
				     data_len);
		if (ret <= 0)
			return ret;
		pe_entry->done_len += ret;
		if (ret != data_len) {
			SOCK_LOG_INFO("Incomplete Recv: %d\n", ret);
			return 0;
		}
	}

	entry_len = 0;
	datatype_sz = fi_datatype_size(pe_entry->rx.rx_op.atomic.datatype);
	for (i = 0; i < pe_entry->rx.rx_op.dest_iov_len; i++) {
		entry_len += pe_entry->rx.rx_iov[i].ioc.count;
	}
	entry_len *= datatype_sz;

	/* cmp data */
	if (pe_entry->rx.rx_op.atomic.cmp_iov_len) {
		offset = pe_entry->done_len - len;
		len += entry_len;
	
		if (pe_entry->done_len < len) {
			data_len = entry_len - offset;
			
			ret = sock_comm_recv(pe_entry->conn,
					     &pe_entry->rx.atomic_cmp[0] + offset,
					     data_len);
			
			if (ret <= 0)
				return ret;
			pe_entry->done_len += ret;
			if (ret != data_len) {
				SOCK_LOG_INFO("Incomplete Recv: %d\n", ret);
				return 0;
			}
			
			/* compare */
			offset = 0;
			for (i = 0; i < pe_entry->rx.rx_op.dest_iov_len; i++) {
				
				mr = sock_mr_verify_key(rx_ctx->domain, 
							 pe_entry->rx.rx_iov[i].ioc.key,
							(void*)pe_entry->rx.rx_iov[i].ioc.addr,
							 pe_entry->rx.rx_iov[i].ioc.count * datatype_sz,
							 FI_REMOTE_WRITE);
				if (!mr) {
					SOCK_LOG_ERROR("Remote memory access error: %p, %lu, %lu\n",
						       (void*)pe_entry->rx.rx_iov[i].ioc.addr,
						       pe_entry->rx.rx_iov[i].ioc.count * datatype_sz,
						       pe_entry->rx.rx_iov[i].ioc.key);
					sock_pe_send_response(pe, pe_entry, 0, SOCK_OP_ATOMIC_ERROR);
					goto err;
				}
				if (mr->flags & FI_MR_OFFSET)
					pe_entry->rx.rx_iov[i].ioc.addr += mr->offset;
			}
		}
	}

	/* src data */
	offset = pe_entry->done_len - len;
	len += entry_len;
	if (pe_entry->done_len < len) {
		data_len = entry_len - offset;
		ret = sock_comm_recv(pe_entry->conn,
				     &pe_entry->rx.atomic_src[0] + offset, data_len);
		
		if (ret <= 0)
			return ret;
		pe_entry->done_len += ret;
		if (ret != data_len) {
			SOCK_LOG_INFO("Incomplete Recv: %d\n", ret);
			return 0;
		}
	}

	offset = 0;
	for (i = 0; i < pe_entry->rx.rx_op.dest_iov_len; i++) {
		for (j = 0; j < pe_entry->rx.rx_iov[i].ioc.count; j++) {
			sock_pe_update_atomic((char*)&pe_entry->rx.atomic_cmp[0] + offset,
					      (char *)pe_entry->rx.rx_iov[i].ioc.addr + j * datatype_sz,
					      (char*)&pe_entry->rx.atomic_src[0] + offset,
					      pe_entry->rx.rx_op.atomic.datatype,
					      pe_entry->rx.rx_op.atomic.op);
			offset += datatype_sz;
		}
	}

	pe_entry->buf = pe_entry->rx.rx_iov[0].iov.addr;
	pe_entry->data_len = offset;

	if (pe_entry->flags & FI_REMOTE_SIGNAL) {
		sock_pe_report_rx_completion(pe_entry);
	}

	sock_pe_report_remote_write(rx_ctx, pe_entry);
	sock_pe_report_mr_completion(rx_ctx->domain, pe_entry);
	sock_pe_send_response(pe, pe_entry, 
					pe_entry->rx.rx_op.atomic.res_iov_len ? 
					entry_len : 0, SOCK_OP_ATOMIC_COMPLETE);
	return ret;
	
err:
	sock_pe_report_error(pe_entry, 0);
	return -FI_EINVAL;
}


int sock_pe_progress_buffered_rx(struct sock_rx_ctx *rx_ctx)
{
	struct dlist_entry *entry;
	struct sock_pe_entry pe_entry;
	struct sock_rx_entry *rx_buffered, *rx_posted;
	int i, rem, offset, len, used_len, dst_offset;

	if (dlist_empty(&rx_ctx->rx_entry_list) ||
	    dlist_empty(&rx_ctx->rx_buffered_list)) 
		goto out;

	for (entry = rx_ctx->rx_buffered_list.next; 
	     entry != &rx_ctx->rx_buffered_list;) {

		rx_buffered = container_of(entry, struct sock_rx_entry, entry);
		entry = entry->next;

		rx_posted = sock_rx_get_entry(rx_ctx, rx_buffered->addr, 
					      rx_buffered->tag);
		if (!rx_posted) 
			continue;

		rx_ctx->buffered_len -= rem;
		SOCK_LOG_INFO("Consuming buffered entry: %p, ctx: %p\n", 
			      rx_buffered, rx_ctx);
		SOCK_LOG_INFO("Consuming posted entry: %p, ctx: %p\n", 
			      rx_posted, rx_ctx);

		offset = 0;
		rem = rx_buffered->iov[0].iov.len;
		used_len = rx_posted->used;
		for (i = 0; i < rx_posted->rx_op.dest_iov_len && rem > 0; i++) {
			if (used_len >= rx_posted->rx_op.dest_iov_len) {
				used_len -= rx_posted->rx_op.dest_iov_len;
				continue;
			}

			dst_offset = used_len;
			len = MIN(rx_posted->iov[i].iov.len, rem);
			pe_entry.buf = (uint64_t)
				(char*)rx_posted->iov[i].iov.addr + dst_offset;
			memcpy((char*)rx_posted->iov[i].iov.addr + dst_offset,
			       (char*)rx_buffered->iov[0].iov.addr + offset, len);
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
		pe_entry.rx.rx_iov[0].iov.addr = rx_posted->iov[0].iov.addr;
		pe_entry.type = SOCK_PE_RX;
		pe_entry.comp = rx_buffered->comp;

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
			goto out;
		} else {
			sock_pe_report_rx_completion(&pe_entry);
		}

		dlist_remove(&rx_buffered->entry);
		sock_rx_release_entry(rx_buffered);

		if (pe_entry.flags & FI_MULTI_RECV)
			sock_rx_release_entry(rx_posted);
	}
	
out:
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
		offset = pe_entry->done_len - len;
		len += SOCK_TAG_SIZE;
		if (pe_entry->done_len < len) {
			ret = sock_comm_recv(pe_entry->conn, 
					   (char*)&pe_entry->tag + offset, 
					   SOCK_TAG_SIZE - offset);
			if (ret <= 0)
				return ret;
			pe_entry->done_len += ret;
			if (pe_entry->done_len != len)
				return 0;
		}
	}

	if (pe_entry->msg_hdr.flags & FI_REMOTE_CQ_DATA) {
		offset = pe_entry->done_len - len;
		len += SOCK_CQ_DATA_SIZE;
		if (pe_entry->done_len < len) {
			ret = sock_comm_recv(pe_entry->conn, 
					     (char*)&pe_entry->data + offset, 
					     SOCK_CQ_DATA_SIZE - offset);
			if (ret <= 0)
				return ret;
			pe_entry->done_len += ret;
			if (pe_entry->done_len != len)
				return 0;
		}
	}

	if (pe_entry->done_len == len && !pe_entry->rx.rx_entry) {

		data_len = pe_entry->msg_hdr.msg_len - len;
		fastlock_acquire(&rx_ctx->lock);

		/* progress buffered recvs, if any  */
		sock_pe_progress_buffered_rx(rx_ctx);

		rx_entry = sock_rx_get_entry(rx_ctx, pe_entry->addr, pe_entry->tag);
		SOCK_LOG_INFO("Consuming posted entry: %p\n", rx_entry);

		if (!rx_entry) {
			SOCK_LOG_INFO("%p: No matching recv, buffering recv (len=%llu)\n", 
				      pe_entry, (long long unsigned int)data_len);

			rx_entry = sock_rx_new_buffered_entry(rx_ctx, data_len);
			if (!rx_entry)
				return -FI_ENOMEM;
			
			rx_entry->addr = pe_entry->addr;
			rx_entry->tag = pe_entry->tag;
			rx_entry->data = pe_entry->data;
			rx_entry->ignore = 0;
			rx_entry->comp = pe_entry->comp;
			pe_entry->context = rx_entry->context;
		}
		pe_entry->context = rx_entry->context;
		pe_entry->rx.rx_entry = rx_entry;
		rx_entry->is_busy = 1;
		fastlock_release(&rx_ctx->lock);
	}
	
	rx_entry = pe_entry->rx.rx_entry;
	done_data = pe_entry->done_len - len;
	pe_entry->data_len = pe_entry->msg_hdr.msg_len - len;
	rem = pe_entry->msg_hdr.msg_len - (len + done_data);
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
				     (char *)rx_entry->iov[i].iov.addr + offset, 
				     data_len);
		if (ret <= 0)
			return ret;

		if (!pe_entry->buf)
			pe_entry->buf = (uint64_t)
				((char *)rx_entry->iov[i].iov.addr + offset);
		rem -= ret;
		used = 0;
		pe_entry->done_len += ret;
		rx_entry->used += ret;
		if (ret != data_len)
			return 0;
	}

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

	pe_entry->is_complete = 1;
	rx_entry->is_busy = 0;

	/* report error, if any */
	if (rem) {
		SOCK_LOG_ERROR("Not enough space in posted recv buffer\n");
		sock_pe_report_error(pe_entry, rem);
		goto out;
	} else {
		if (!rx_entry->is_buffered)
			sock_pe_report_rx_completion(pe_entry);
	}
	
	if (pe_entry->msg_hdr.flags & FI_REMOTE_COMPLETE) {
		sock_pe_send_response(pe, pe_entry, 0, SOCK_OP_SEND_COMPLETE);
	}
	
out:
	if (!rx_entry->is_buffered &&
	    (!(rx_entry->flags & FI_MULTI_RECV) ||
	     (pe_entry->flags & FI_MULTI_RECV)))
		sock_rx_release_entry(rx_entry);
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
		
	/* process rx entry */
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

	case SOCK_OP_ATOMIC_WRITE:
	case SOCK_OP_ATOMIC_READ_WRITE:
	case SOCK_OP_ATOMIC_COMP_WRITE:
		ret = sock_pe_process_rx_atomic(pe, rx_ctx, pe_entry);
		break;

	case SOCK_OP_SEND_COMPLETE:
	case SOCK_OP_WRITE_COMPLETE:
	case SOCK_OP_WRITE_ERROR:
	case SOCK_OP_READ_ERROR:
	case SOCK_OP_ATOMIC_ERROR:
		ret = sock_pe_handle_ack(pe, pe_entry);
		break;

	case SOCK_OP_READ_COMPLETE:
		ret = sock_pe_handle_read_complete(pe, pe_entry);
		break;

	case SOCK_OP_ATOMIC_COMPLETE:
		ret = sock_pe_handle_atomic_complete(pe, pe_entry);
		break;

	default:
		ret = -FI_ENOSYS;
		SOCK_LOG_ERROR("Operation not supported\n");
		break;
	}

out:
	return ret;
}

static int sock_pe_read_hdr(struct sock_pe *pe, 
			    struct sock_pe_entry *pe_entry)
{
	int ret; 
	struct sock_msg_hdr *msg_hdr;
	struct sock_conn *conn = pe_entry->conn;

	if (conn->rx_pe_entry != NULL && conn->rx_pe_entry != pe_entry)
		return 0;

	if (conn->rx_pe_entry == NULL) {
		conn->rx_pe_entry = pe_entry;
	}

	msg_hdr = &pe_entry->msg_hdr;
	if (pe_entry->done_len < sizeof(struct sock_msg_hdr)) {
		ret = sock_comm_recv(conn, 
				     (char*)msg_hdr + pe_entry->done_len, 
				     sizeof(struct sock_msg_hdr) -
				     pe_entry->done_len);
		if (ret <= 0) 
			return ret;
		
		pe_entry->done_len += ret;
		if (pe_entry->done_len == sizeof(struct sock_msg_hdr)) {

			msg_hdr->msg_len = ntohll(msg_hdr->msg_len);
			msg_hdr->rx_id = ntohs(msg_hdr->rx_id);
			msg_hdr->flags = ntohll(msg_hdr->flags);
			msg_hdr->pe_entry_id = ntohs(msg_hdr->pe_entry_id);
			pe_entry->rx.header_read = 1;

			SOCK_LOG_INFO("PE RX (Hdr read): MsgLen: %lu, TX-ID: %d, Type: %d\n", 
				      msg_hdr->msg_len, msg_hdr->rx_id, msg_hdr->op_type);
		}
	}
	return 0;
}

static int sock_pe_progress_tx_atomic(struct sock_pe *pe, 
				      struct sock_pe_entry *pe_entry, 
				      struct sock_conn *conn)
{
	int ret, datatype_sz;
	union sock_iov iov[SOCK_EP_MAX_IOV_LIMIT];
	ssize_t len, i, offset, done_data, data_len, iov_len;
	struct sock_atomic_req  req;

	if (pe_entry->tx.send_done)
		return 0;
	
	len = sizeof(struct sock_msg_hdr);
	offset = pe_entry->done_len;
	len = sizeof(struct sock_atomic_req);
	if (pe_entry->done_len < len) {
		req.op.src_iov_len = 0;
		req.op.dest_iov_len = pe_entry->tx.tx_op.dest_iov_len;
		req.op.atomic.op = pe_entry->tx.tx_op.atomic.op;
		req.op.atomic.datatype = pe_entry->tx.tx_op.atomic.datatype;
		req.op.atomic.res_iov_len = pe_entry->tx.tx_op.atomic.res_iov_len;
		req.op.atomic.cmp_iov_len = pe_entry->tx.tx_op.atomic.cmp_iov_len;

		data_len = sizeof(req) - pe_entry->done_len;
		ret = sock_comm_send(conn, (char*)&req + offset, data_len);
		if (ret <= 0)
			return ret;
		pe_entry->done_len += ret;
		if (pe_entry->done_len != len)
			return 0;
	}
	
	if (pe_entry->flags & FI_REMOTE_CQ_DATA) {
		offset = pe_entry->done_len - len;
		len += SOCK_CQ_DATA_SIZE;
		if (pe_entry->done_len < len) {
			data_len = SOCK_CQ_DATA_SIZE - offset;
			ret = sock_comm_send(conn, 
					     (char*)pe_entry->data + offset,
					     data_len);
			if (ret <= 0)
				return ret;
			pe_entry->done_len += ret;
			if (pe_entry->done_len != data_len)
				return 0;
		}
	}

	/* dest iocs */
	offset = pe_entry->done_len - len;
	iov_len = sizeof(union sock_iov) * pe_entry->tx.tx_op.dest_iov_len;
	len += iov_len;
		
	if (pe_entry->done_len < len && iov_len) {
		for (i=0; i < pe_entry->tx.tx_op.dest_iov_len; i++) {
			iov[i].ioc.addr = pe_entry->tx.tx_iov[i].dst.ioc.addr;
			iov[i].ioc.count = pe_entry->tx.tx_iov[i].dst.ioc.count;
			iov[i].ioc.key = pe_entry->tx.tx_iov[i].dst.ioc.key;
		}

		ret = sock_comm_send(conn,
				     (char*)&iov[0] + offset, iov_len - offset);
		if (ret <= 0)
			return ret;
		pe_entry->done_len += ret;
		if (pe_entry->done_len != len)
			return 0;
	}
	
	/* cmp data */
	datatype_sz = fi_datatype_size(pe_entry->tx.tx_op.atomic.datatype);
	done_data = pe_entry->done_len - len;
	for (i=0; i < pe_entry->tx.tx_op.atomic.cmp_iov_len; i++) {
		len += pe_entry->tx.tx_iov[i].cmp.ioc.count * datatype_sz;
	}

	if (pe_entry->done_len < len) {
		
		for (i=0; i < pe_entry->tx.tx_op.atomic.cmp_iov_len; i++) {
			if (done_data >= pe_entry->tx.tx_iov[i].cmp.ioc.count * datatype_sz) {
				done_data -= pe_entry->tx.tx_iov[i].cmp.ioc.count * datatype_sz;
				continue;
			}
			
			offset = done_data;
			data_len = (pe_entry->tx.tx_iov[i].cmp.ioc.count * 
				    datatype_sz) - done_data;

			ret = sock_comm_send(conn, 
					     (char*)pe_entry->tx.tx_iov[i].cmp.ioc.addr + 
					     offset, data_len);
			if (ret <= 0)
				return ret;
			
			done_data = 0;
			pe_entry->done_len += ret;
			if ( ret != data_len)
				return 0;
		}
	}

	/* data */
	if (pe_entry->flags & FI_INJECT) {
		done_data = pe_entry->done_len - len;
		len += pe_entry->tx.tx_op.src_iov_len;
		pe_entry->data_len = pe_entry->tx.tx_op.src_iov_len;
		
		data_len = pe_entry->tx.tx_op.src_iov_len - done_data;
		if (pe_entry->done_len < len) {
			ret = sock_comm_send(conn, 
					     (char*)pe_entry->tx.inject_data + done_data,
					     data_len);
			if (ret <= 0)
				return ret;
			
			pe_entry->done_len += ret;
			if (pe_entry->done_len <= len)
				return 0;
		}
	} else {
		done_data = pe_entry->done_len - len;
		for (i=0; i < pe_entry->tx.tx_op.src_iov_len; i++) {
			
			if (done_data >= pe_entry->tx.tx_iov[i].src.ioc.count * datatype_sz) {
				done_data -= pe_entry->tx.tx_iov[i].src.ioc.count * datatype_sz;
				continue;
			}
			
			offset = done_data;
			data_len = (pe_entry->tx.tx_iov[i].src.ioc.count * 
				    datatype_sz) - done_data;

			ret = sock_comm_send(conn, 
					     (char*)pe_entry->tx.tx_iov[i].src.ioc.addr + 
					     offset, data_len);
			if (ret <= 0)
				return ret;
			
			done_data = 0;
			pe_entry->done_len += ret;
			pe_entry->data_len += ret;
			if ( ret != data_len)
				return 0;
		}
	}

	if (pe_entry->done_len == pe_entry->total_len) {
		pe_entry->tx.send_done = 1;
		pe_entry->conn->tx_pe_entry = NULL;
		SOCK_LOG_INFO("Send complete\n");		
	}
	sock_comm_flush(pe_entry->conn);
	return 0;
}

static int sock_pe_progress_tx_write(struct sock_pe *pe, 
				     struct sock_pe_entry *pe_entry, 
				     struct sock_conn *conn)
{
	int ret;
	union sock_iov dest_iov[SOCK_EP_MAX_IOV_LIMIT];
	ssize_t len, i, offset, done_data, data_len, dest_iov_len;

	if (pe_entry->tx.send_done)
		return 0;

	len = sizeof(struct sock_msg_hdr);
	if (pe_entry->flags & FI_REMOTE_CQ_DATA) {

		offset = pe_entry->done_len - len;
		len += SOCK_CQ_DATA_SIZE;
		if (pe_entry->done_len < len) {
			ret = sock_comm_send(conn, 
					   (char*)pe_entry->data + offset,
					   SOCK_CQ_DATA_SIZE - offset);
			if (ret <= 0)
				return ret;
			pe_entry->done_len += ret;
			if (pe_entry->done_len != len)
				return 0;
		}
	}

	/* dest iovs */
	offset = pe_entry->done_len - len;
	dest_iov_len = sizeof(union sock_iov) * pe_entry->tx.tx_op.dest_iov_len;
	len += dest_iov_len;
		
	if (pe_entry->done_len < len) {
		for (i=0; i < pe_entry->tx.tx_op.dest_iov_len; i++) {
			dest_iov[i].iov.addr = pe_entry->tx.tx_iov[i].dst.iov.addr;
			dest_iov[i].iov.len = pe_entry->tx.tx_iov[i].dst.iov.len;
			dest_iov[i].iov.key = pe_entry->tx.tx_iov[i].dst.iov.key;
		}

		ret = sock_comm_send(conn,
				   (char*)&dest_iov[0] + offset, 
				   dest_iov_len - offset);
		if (ret <= 0)
			return ret;
		pe_entry->done_len += ret;
		if (pe_entry->done_len != len)
			return 0;
	}

	/* data */
	if (pe_entry->flags & FI_INJECT) {
		offset = pe_entry->done_len - len;
		len += pe_entry->tx.tx_op.src_iov_len;
		pe_entry->data_len = pe_entry->tx.tx_op.src_iov_len;
		
		if (pe_entry->done_len < len) {
			ret = sock_comm_send(conn, 
					   (char*)pe_entry->tx.inject_data + offset,
					   pe_entry->tx.tx_op.src_iov_len - offset);
			if (ret <= 0)
				return ret;
			
			pe_entry->done_len += ret;
			if (pe_entry->done_len <= len)
				return 0;
		}
	} else {
		done_data = pe_entry->done_len - len;

		for (i=0; i < pe_entry->tx.tx_op.src_iov_len; i++) {
			
			if (done_data >= pe_entry->tx.tx_iov[i].src.iov.len) {
				done_data -= pe_entry->tx.tx_iov[i].src.iov.len;
				continue;
			}

			offset = done_data;
			data_len = pe_entry->tx.tx_iov[i].src.iov.len - done_data;

			ret = sock_comm_send(conn, 
					   (char*)pe_entry->tx.tx_iov[i].src.iov.addr + 
					   offset, data_len);
			if (ret <= 0)
				return ret;

			done_data = 0;
			pe_entry->done_len += ret;
			pe_entry->data_len += ret;
			if ( ret != data_len)
				return 0;
		}
	}

	if (pe_entry->done_len == pe_entry->total_len) {
		pe_entry->tx.send_done = 1;
		pe_entry->conn->tx_pe_entry = NULL;
		SOCK_LOG_INFO("Send complete\n");		
	}
	sock_comm_flush(pe_entry->conn);
	return 0;
}

static int sock_pe_progress_tx_read(struct sock_pe *pe, 
				     struct sock_pe_entry *pe_entry, 
				     struct sock_conn *conn)
{
	int ret;
	union sock_iov src_iov[SOCK_EP_MAX_IOV_LIMIT];
	ssize_t len, i, offset, src_iov_len;

	if (pe_entry->tx.send_done)
		return 0;

	len = sizeof(struct sock_msg_hdr);
	offset = pe_entry->done_len - len;
	src_iov_len = sizeof(union sock_iov) * pe_entry->tx.tx_op.src_iov_len;
	len += src_iov_len;

	pe_entry->data_len = 0;
	for (i=0; i < pe_entry->tx.tx_op.src_iov_len; i++) 
		pe_entry->data_len += pe_entry->tx.tx_iov[i].src.iov.len;

	/* src iovs */		
	if (pe_entry->done_len < len) {
		for (i=0; i < pe_entry->tx.tx_op.src_iov_len; i++) {
			src_iov[i].iov.addr = pe_entry->tx.tx_iov[i].src.iov.addr;
			src_iov[i].iov.len = pe_entry->tx.tx_iov[i].src.iov.len;
			src_iov[i].iov.key = pe_entry->tx.tx_iov[i].src.iov.key;
		}

		ret = sock_comm_send(conn,
				     (char*)&src_iov[0] + offset, 
				     src_iov_len - offset);
		if (ret <= 0)
			return ret;
		pe_entry->done_len += ret;
		if (pe_entry->done_len != len)
			return 0;
	}

	if (pe_entry->done_len == pe_entry->total_len) {
		pe_entry->tx.send_done = 1;
		pe_entry->conn->tx_pe_entry = NULL;
		SOCK_LOG_INFO("Send complete\n");		
	}
	sock_comm_flush(pe_entry->conn);
	return 0;
}


static int sock_pe_progress_tx_send(struct sock_pe *pe, 
				    struct sock_pe_entry *pe_entry, 
				    struct sock_conn *conn)
{
	int ret;
	ssize_t len, i, offset, done_data, data_len;

	if (pe_entry->tx.send_done)
		return 0;

	len = sizeof(struct sock_msg_hdr);
	if (pe_entry->tx.tx_op.op == SOCK_OP_TSEND) {

		offset = pe_entry->done_len - len;

		len += SOCK_TAG_SIZE;
		if (pe_entry->done_len < len) {
			ret = sock_comm_send(conn, 
					     (char*)&pe_entry->tag + offset,
					     SOCK_TAG_SIZE - offset);
			if (ret <= 0) 
				return ret;

			pe_entry->done_len += ret;
			if (pe_entry->done_len != len)
				return 0;
		}
	}

	if (pe_entry->flags & FI_REMOTE_CQ_DATA) {

		offset = pe_entry->done_len - len;
		len += SOCK_CQ_DATA_SIZE;
		if (pe_entry->done_len < len) {
			ret = sock_comm_send(conn, 
					     (char*)pe_entry->data + offset,
					     SOCK_CQ_DATA_SIZE - offset);
			if (ret <= 0) 
				return ret;

			pe_entry->done_len += ret;
			if (pe_entry->done_len != len)
				return 0;
		}
	}

	if (pe_entry->flags & FI_INJECT) {
		offset = pe_entry->done_len - len;
		len += pe_entry->tx.tx_op.src_iov_len;
		pe_entry->data_len = pe_entry->tx.tx_op.src_iov_len;
		
		if (pe_entry->done_len < len) {
			ret = sock_comm_send(conn, 
					   (char*)pe_entry->tx.inject_data + offset,
					   pe_entry->tx.tx_op.src_iov_len - offset);
			if (ret <= 0) 
				return ret;
			
			pe_entry->done_len += ret;
			if (pe_entry->done_len <= len)
				return 0;
		}
	} else {
		done_data = pe_entry->done_len - len;
		pe_entry->data_len = pe_entry->total_len - len;

		for (i=0; i < pe_entry->tx.tx_op.src_iov_len; i++) {

			if (done_data >= pe_entry->tx.tx_iov[i].src.iov.len) {
				done_data -= pe_entry->tx.tx_iov[i].src.iov.len;
				continue;
			}

			offset = done_data;
			data_len = pe_entry->tx.tx_iov[i].src.iov.len - done_data;

			ret = sock_comm_send(conn, 
					   (char*)pe_entry->tx.tx_iov[i].src.iov.addr + 
					   offset, data_len);
			if (ret <= 0) 
				return ret;
			
			done_data = 0;
			pe_entry->done_len += ret;
			if ( ret != data_len)
				return 0;
		}
	}

	sock_comm_flush(pe_entry->conn);
	if (pe_entry->done_len == pe_entry->total_len) {
		pe_entry->tx.send_done = 1;
		pe_entry->conn->tx_pe_entry = NULL;
		SOCK_LOG_INFO("Send complete\n");

		if (!(pe_entry->flags & FI_REMOTE_COMPLETE)) {
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

	if (pe_entry->tx.send_done)
		return 0;

	assert(pe_entry->conn);
	if (conn->tx_pe_entry != NULL && conn->tx_pe_entry != pe_entry) {
		SOCK_LOG_INFO("Cannot progress %p as conn %p is being used by %p\n",
			      pe_entry, conn, conn->tx_pe_entry);
		return 0;
	}

	if (conn->tx_pe_entry == NULL) {
		SOCK_LOG_INFO("Connection %p grabbed by %p\n", conn, pe_entry);
		conn->tx_pe_entry = pe_entry;
	}

	if (!pe_entry->tx.header_sent) {
		ret = sock_comm_send(conn, 
				   (char*)&pe_entry->msg_hdr + pe_entry->done_len,
				   sizeof(struct sock_msg_hdr) - pe_entry->done_len);
		if (ret <= 0) 
			return ret;
		
		pe_entry->done_len += ret;
		if (pe_entry->done_len == sizeof(struct sock_msg_hdr)) {
			pe_entry->tx.header_sent = 1;
			SOCK_LOG_INFO("[%p] Header sent\n", pe_entry);
		}else {
			return 0;
		}
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

	case SOCK_OP_ATOMIC_WRITE:
	case SOCK_OP_ATOMIC_READ_WRITE:
	case SOCK_OP_ATOMIC_COMP_WRITE:
		ret = sock_pe_progress_tx_atomic(pe, pe_entry, conn);
		break;
		
	default:
		ret = -FI_ENOSYS;
		SOCK_LOG_ERROR("Operation not supported\n");
		break;
	}
	
	return ret;
}

static int sock_pe_new_rx_entry(struct sock_pe *pe, struct sock_rx_ctx *rx_ctx,
				struct sock_ep *ep, struct sock_conn *conn, 
				int key)
{
	struct sock_pe_entry *pe_entry;	
	pe_entry = sock_pe_acquire_entry(pe);
	if (!pe_entry) {
		SOCK_LOG_ERROR("Error in getting PE entry\n");
		return -FI_EINVAL;
	}

	memset(&pe_entry->rx, 0, sizeof(struct sock_rx_pe_entry));

	pe_entry->conn = conn;
	pe_entry->type = SOCK_PE_RX;
	pe_entry->ep = ep;
	pe_entry->is_complete = 0;
	pe_entry->done_len = 0;

	if (ep->ep_type == FI_EP_MSG)
		pe_entry->addr = FI_ADDR_NOTAVAIL;
	else
		pe_entry->addr = sock_av_lookup_key(ep->av, key);

	if (ep->ep_attr.rx_ctx_cnt == FI_SHARED_CONTEXT)
		pe_entry->comp = &ep->comp;
	else
		pe_entry->comp = &rx_ctx->comp;

	SOCK_LOG_INFO("New RX on PE entry %p (%ld)\n", 
		      pe_entry, PE_INDEX(pe, pe_entry));

	SOCK_LOG_INFO("Inserting rx_entry to PE entry %p, conn: %p\n",
		      pe_entry, pe_entry->conn);

	/* link to tracking list in rx_ctx */
	dlist_init(&pe_entry->ctx_entry);
	dlist_insert_tail(&pe_entry->ctx_entry, &rx_ctx->pe_entry_list);
	return 0;
}

static int sock_pe_new_tx_entry(struct sock_pe *pe, struct sock_tx_ctx *tx_ctx)
{
	int i, datatype_sz;
	struct sock_msg_hdr *msg_hdr;
	struct sock_pe_entry *pe_entry;
	struct sock_ep *ep;
	uint16_t rx_id;

	pe_entry = sock_pe_acquire_entry(pe);
	if (!pe_entry) {
		SOCK_LOG_ERROR("Failed to get free PE entry \n");
		return -FI_EINVAL;
	}

	memset(&pe_entry->tx, 0, sizeof(struct sock_tx_pe_entry));
	memset(&pe_entry->msg_hdr, 0, sizeof(struct sock_msg_hdr));

	pe_entry->type = SOCK_PE_TX;
	pe_entry->is_complete = 0;
	pe_entry->done_len = 0;
	pe_entry->conn = NULL;
	pe_entry->ep = tx_ctx->ep;
	pe_entry->tx.tx_ctx = tx_ctx;

	dlist_insert_tail(&pe_entry->ctx_entry, &tx_ctx->pe_entry_list);

	/* fill in PE tx entry */
	memset(&pe_entry->msg_hdr, 0, sizeof(struct sock_msg_hdr));
	msg_hdr = &pe_entry->msg_hdr;
	msg_hdr->msg_len = sizeof(struct sock_msg_hdr);

	msg_hdr->pe_entry_id = PE_INDEX(pe, pe_entry);
	SOCK_LOG_INFO("New TX on PE entry %p (%d)\n", 
		      pe_entry, msg_hdr->pe_entry_id);

	rbfdread(&tx_ctx->rbfd, &pe_entry->tx.tx_op, sizeof(struct sock_op));
	rbfdread(&tx_ctx->rbfd, &pe_entry->flags, sizeof(uint64_t));
	rbfdread(&tx_ctx->rbfd, &pe_entry->context, sizeof(uint64_t));
	rbfdread(&tx_ctx->rbfd, &pe_entry->addr, sizeof(uint64_t));
	rbfdread(&tx_ctx->rbfd, &pe_entry->conn, sizeof(uint64_t));
	rbfdread(&tx_ctx->rbfd, &pe_entry->buf, sizeof(uint64_t));
	rbfdread(&tx_ctx->rbfd, &ep, sizeof(uint64_t));

	if (ep && ep->ep_attr.tx_ctx_cnt == FI_SHARED_CONTEXT)
		pe_entry->comp = &ep->comp;
	else 
		pe_entry->comp = &tx_ctx->comp;

	if (pe_entry->flags & FI_REMOTE_CQ_DATA) {
		rbfdread(&tx_ctx->rbfd, &pe_entry->data, SOCK_CQ_DATA_SIZE);
		msg_hdr->msg_len += SOCK_CQ_DATA_SIZE;
	}

	if (pe_entry->tx.tx_op.op == SOCK_OP_TSEND) {
		rbfdread(&tx_ctx->rbfd, &pe_entry->tag, SOCK_TAG_SIZE);
		msg_hdr->msg_len += SOCK_TAG_SIZE;
	}

	msg_hdr->op_type = pe_entry->tx.tx_op.op;
	switch (pe_entry->tx.tx_op.op) {
	
	case SOCK_OP_SEND:
	case SOCK_OP_TSEND:

		if (pe_entry->flags & FI_INJECT) {
			rbfdread(&tx_ctx->rbfd, &pe_entry->tx.inject_data[0],
				 pe_entry->tx.tx_op.src_iov_len);
			msg_hdr->msg_len += pe_entry->tx.tx_op.src_iov_len;
		} else {
			/* read src iov(s)*/
			for (i = 0; i<pe_entry->tx.tx_op.src_iov_len; i++) {
				rbfdread(&tx_ctx->rbfd, &pe_entry->tx.tx_iov[i].src, 
					 sizeof(union sock_iov));
				msg_hdr->msg_len += pe_entry->tx.tx_iov[i].src.iov.len;
			}
		}
		break;
		
	case SOCK_OP_WRITE:
		
		if (pe_entry->flags & FI_INJECT) {
			rbfdread(&tx_ctx->rbfd, &pe_entry->tx.inject_data[0],
				 pe_entry->tx.tx_op.src_iov_len);
			msg_hdr->msg_len += pe_entry->tx.tx_op.src_iov_len;
		} else {
			/* read src iov(s)*/
			for (i = 0; i<pe_entry->tx.tx_op.src_iov_len; i++) {
				rbfdread(&tx_ctx->rbfd, &pe_entry->tx.tx_iov[i].src, 
					 sizeof(union sock_iov));
				msg_hdr->msg_len += pe_entry->tx.tx_iov[i].src.iov.len;
			}
		}
		
		/* read dst iov(s)*/
		for (i = 0; i<pe_entry->tx.tx_op.dest_iov_len; i++) {
			rbfdread(&tx_ctx->rbfd, &pe_entry->tx.tx_iov[i].dst, 
				 sizeof(union sock_iov));
		}
		msg_hdr->msg_len += sizeof(union sock_iov) * 
			pe_entry->tx.tx_op.dest_iov_len;
		break;

	case SOCK_OP_READ:

		/* read src iov(s)*/
		for (i = 0; i<pe_entry->tx.tx_op.src_iov_len; i++) {
			rbfdread(&tx_ctx->rbfd, &pe_entry->tx.tx_iov[i].src, 
				 sizeof(union sock_iov));
		}
		msg_hdr->msg_len += sizeof(union sock_iov) * 
			pe_entry->tx.tx_op.src_iov_len;

		/* read dst iov(s)*/
		for (i = 0; i<pe_entry->tx.tx_op.dest_iov_len; i++) {
			rbfdread(&tx_ctx->rbfd, &pe_entry->tx.tx_iov[i].dst, 
				 sizeof(union sock_iov));
		}
		break;

	case SOCK_OP_ATOMIC_WRITE:			
	case SOCK_OP_ATOMIC_READ_WRITE:
	case SOCK_OP_ATOMIC_COMP_WRITE:

		msg_hdr->msg_len += sizeof(struct sock_op);
		datatype_sz = fi_datatype_size(pe_entry->tx.tx_op.atomic.datatype);
		if (pe_entry->flags & FI_INJECT) {
			rbfdread(&tx_ctx->rbfd, &pe_entry->tx.inject_data[0],
				 pe_entry->tx.tx_op.src_iov_len);
			msg_hdr->msg_len += pe_entry->tx.tx_op.src_iov_len;
		} else {
			/* read src ioc(s)*/
			for (i = 0; i<pe_entry->tx.tx_op.src_iov_len; i++) {
				rbfdread(&tx_ctx->rbfd, &pe_entry->tx.tx_iov[i].src, 
					 sizeof(union sock_iov));
				msg_hdr->msg_len += 
					(pe_entry->tx.tx_iov[i].src.ioc.count * datatype_sz);
			}
		}

		/* read dst ioc(s)*/
		for (i = 0; i<pe_entry->tx.tx_op.dest_iov_len; i++) {
			rbfdread(&tx_ctx->rbfd, &pe_entry->tx.tx_iov[i].dst, 
				 sizeof(union sock_iov));
		}
		msg_hdr->msg_len += sizeof(union sock_iov) * 
			pe_entry->tx.tx_op.dest_iov_len;

		/* read result ioc(s)*/
		for (i = 0; i<pe_entry->tx.tx_op.atomic.res_iov_len; i++) {
			rbfdread(&tx_ctx->rbfd, &pe_entry->tx.tx_iov[i].res, 
				 sizeof(union sock_iov));
		}
		
		/* read comp ioc(s)*/
		for (i = 0; i<pe_entry->tx.tx_op.atomic.cmp_iov_len; i++) {
			rbfdread(&tx_ctx->rbfd, &pe_entry->tx.tx_iov[i].cmp, 
				 sizeof(union sock_iov));
			msg_hdr->msg_len += (pe_entry->tx.tx_iov[i].cmp.ioc.count * 
					     datatype_sz);
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

	rx_id = (uint16_t)SOCK_GET_RX_ID(pe_entry->addr, tx_ctx->av->rx_ctx_bits);
	msg_hdr->rx_id = htons(rx_id);
	msg_hdr->dest_iov_len = pe_entry->tx.tx_op.src_iov_len;
	msg_hdr->flags = htonll(pe_entry->flags);
	pe_entry->total_len = msg_hdr->msg_len;
	msg_hdr->msg_len = htonll(msg_hdr->msg_len);
	msg_hdr->pe_entry_id = htons(msg_hdr->pe_entry_id);
	return 0;
}

void sock_pe_add_tx_ctx(struct sock_pe *pe, struct sock_tx_ctx *ctx)
{
	fastlock_acquire(&pe->lock);
	dlistfd_insert_tail(&ctx->pe_entry, &pe->tx_list);
	fastlock_release(&pe->lock);
	SOCK_LOG_INFO("TX ctx added to PE\n");
}

void sock_pe_add_rx_ctx(struct sock_pe *pe, struct sock_rx_ctx *ctx)
{
	fastlock_acquire(&pe->lock);
	dlistfd_insert_tail(&ctx->pe_entry, &pe->rx_list);
	fastlock_release(&pe->lock);
	SOCK_LOG_INFO("RX ctx added to PE\n");
}

int sock_pe_progress_rx_ctx(struct sock_pe *pe, struct sock_rx_ctx *rx_ctx)
{
	int i, ret = 0, data_avail;
	struct sock_ep *ep;
	struct pollfd poll_fd;
	struct sock_conn *conn;
	struct dlist_entry *entry;
	struct sock_pe_entry *pe_entry;
	struct sock_conn_map *map;

	poll_fd.events = POLLIN;
	fastlock_acquire(&pe->lock);

	/* progress buffered recvs */
	fastlock_acquire(&rx_ctx->lock);
	sock_pe_progress_buffered_rx(rx_ctx);
	fastlock_release(&rx_ctx->lock);

	/* check for incoming data */
	for (entry = rx_ctx->ep_list.next;
	    entry != &rx_ctx->ep_list; entry = entry->next) {

		ep = container_of(entry, struct sock_ep, rx_ctx_entry);
		map = &ep->domain->r_cmap;
		assert(map != NULL);

		for (i=0; i<map->used; i++) {
			conn = &map->table[i];

			data_avail = 0;
			if (rbused(&conn->inbuf) > 0) {
				data_avail = 1;
			} else {
				poll_fd.fd = conn->sock_fd;
				ret = poll(&poll_fd, 1, 0);
				if (ret < 0) {
					SOCK_LOG_INFO("Error polling fd: %d\n", 
						      conn->sock_fd);
					goto out;
				}
				data_avail = (ret == 1);
			}

			if (data_avail && conn->rx_pe_entry == NULL) {
				/* new RX PE entry */
				ret = sock_pe_new_rx_entry(pe, rx_ctx, ep, conn, i);
				if (ret < 0) 
					goto out;
			}
		}
	}

	/* progress tx_ctx in PE table */
	for (entry = rx_ctx->pe_entry_list.next;
	    entry != &rx_ctx->pe_entry_list;) {
		
		pe_entry = container_of(entry, struct sock_pe_entry, ctx_entry);
		entry = entry->next;

		if (pe_entry->rx.pending_send) {
			sock_pe_progress_pending_ack(pe, pe_entry);
			if (pe_entry->is_complete) {
				sock_pe_release_entry(pe, pe_entry);
				SOCK_LOG_INFO("[%p] RX done\n", pe_entry);
			}
			continue;
		}


		if (!pe_entry->rx.header_read) {
			ret = sock_pe_read_hdr(pe, pe_entry);
			if (ret < 0) 
				goto out;
		}

		if (pe_entry->rx.header_read) {
			ret = sock_pe_process_recv(pe, rx_ctx, pe_entry);
			if (ret < 0) 
				goto out;
		}

		if (pe_entry->is_complete) {
			sock_pe_release_entry(pe, pe_entry);
			SOCK_LOG_INFO("[%p] RX done\n", pe_entry);
		}
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

	fastlock_acquire(&pe->lock);

	/* check tx_ctx rbuf */
	fastlock_acquire(&tx_ctx->rlock);
	while (!rbfdempty(&tx_ctx->rbfd) && 
	      !dlist_empty(&pe->free_list)) {
		/* new TX PE entry */
		ret = sock_pe_new_tx_entry(pe, tx_ctx);
		if (ret < 0) {
			fastlock_release(&tx_ctx->rlock);
			goto out;
		}
	}
	fastlock_release(&tx_ctx->rlock);

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
	while (pe->do_progress) {

		/* progress tx */
		if (!dlistfd_empty(&pe->tx_list)) {
			for (entry = pe->tx_list.list.next;
			    entry != &pe->tx_list.list; entry = entry->next) {
				tx_ctx = container_of(entry, struct sock_tx_ctx,
						      pe_entry);
				ret = sock_pe_progress_tx_ctx(pe, tx_ctx);
				if (ret < 0) {
					SOCK_LOG_ERROR(
						   "failed to progress TX\n");
					return NULL;
				}
			}
		}

		/* progress rx */
		if (!dlistfd_empty(&pe->rx_list)) {
			for (entry = pe->rx_list.list.next;
			    entry != &pe->rx_list.list; entry = entry->next) {
				rx_ctx = container_of(entry, struct sock_rx_ctx,
						      pe_entry);
				ret = sock_pe_progress_rx_ctx(pe, rx_ctx);
				if (ret < 0) {
					SOCK_LOG_ERROR(
						   "failed to progress RX\n");
					return NULL;
				}
			}
		}
	}
	
	SOCK_LOG_INFO("Progress thread terminated\n");
	return NULL;
}

static void sock_pe_init_table(
	struct sock_pe *pe)
{
	int i;
	
	memset(&pe->pe_table, 0, 
	       sizeof(struct sock_pe_entry) * SOCK_PE_MAX_ENTRIES);

	dlist_init(&pe->free_list);
	dlist_init(&pe->busy_list);

	for (i=0; i<SOCK_PE_MAX_ENTRIES; i++) {
		dlist_insert_tail(&pe->pe_table[i].entry, &pe->free_list);
	}

	SOCK_LOG_INFO("PE table init: OK\n");
}

struct sock_pe *sock_pe_init(struct sock_domain *domain)
{
	struct sock_pe *pe = calloc(1, sizeof(struct sock_pe));
	if (!pe)
		return NULL;

	sock_pe_init_table(pe);

	dlistfd_head_init(&pe->tx_list);
	dlistfd_head_init(&pe->rx_list);
	fastlock_init(&pe->lock);
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

	dlistfd_head_free(&pe->tx_list);
	dlistfd_head_free(&pe->rx_list);

	free(pe);
	SOCK_LOG_INFO("Progress engine finalize: OK\n");
}

