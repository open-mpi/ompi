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

#include <pthread.h>

#include <rdma/fabric.h>
#include <rdma/fi_atomic.h>
#include <rdma/fi_cm.h>
#include <rdma/fi_domain.h>
#include <rdma/fi_endpoint.h>
#include <rdma/fi_eq.h>
#include <rdma/fi_errno.h>
#include <rdma/fi_prov.h>
#include <rdma/fi_rma.h>
#include <rdma/fi_tagged.h>
#include <rdma/fi_trigger.h>
#include <netdb.h>

#include <fi.h>
#include <fi_enosys.h>
#include <fi_indexer.h>
#include <fi_rbuf.h>
#include <fi_list.h>

#ifndef _SOCK_H_
#define _SOCK_H_

#define SOCK_EP_MAX_MSG_SZ (1<<23)
#define SOCK_EP_MAX_INJECT_SZ ((1<<8) - 1)
#define SOCK_EP_MAX_BUFF_RECV (1<<20)
#define SOCK_EP_MAX_ORDER_RAW_SZ SOCK_EP_MAX_MSG_SZ
#define SOCK_EP_MAX_ORDER_WAR_SZ SOCK_EP_MAX_MSG_SZ
#define SOCK_EP_MAX_ORDER_WAW_SZ SOCK_EP_MAX_MSG_SZ
#define SOCK_EP_MEM_TAG_FMT (0)
#define SOCK_EP_MAX_EP_CNT (128)
#define SOCK_EP_MAX_TX_CNT (16)
#define SOCK_EP_MAX_RX_CNT (16)
#define SOCK_EP_MAX_IOV_LIMIT (8)
#define SOCK_EP_TX_SZ (256)
#define SOCK_EP_RX_SZ (256)
#define SOCK_EP_TX_ENTRY_SZ (256)
#define SOCK_EP_RX_ENTRY_SZ (256)
#define SOCK_EP_MIN_MULTI_RECV (64)
#define SOCK_EP_MAX_ATOMIC_SZ (256)
#define SOCK_EP_MAX_CTX_BITS (16)

#define SOCK_PE_POLL_TIMEOUT (100000)
#define SOCK_PE_MAX_ENTRIES (128)
#define SOCK_PE_MIN_ENTRIES (1)

#define SOCK_EQ_DEF_SZ (1<<8)
#define SOCK_CQ_DEF_SZ (1<<8)
#define SOCK_AV_DEF_SZ (1<<8)

#define SOCK_CQ_DATA_SIZE (sizeof(uint64_t))
#define SOCK_TAG_SIZE (sizeof(uint64_t))

#define SOCK_PEP_LISTENER_TIMEOUT (10000)
#define SOCK_CM_COMM_TIMEOUT (5000)
#define SOCK_EP_MAX_RETRY (5)
#define SOCK_EP_MAX_CM_DATA_SZ (256)

#define SOCK_EP_RDM_CAP (FI_MSG | FI_RMA | FI_TAGGED | FI_ATOMICS |	\
			 FI_DYNAMIC_MR | FI_NAMED_RX_CTX | \
			 FI_DIRECTED_RECV | FI_MULTI_RECV | \
			 FI_SOURCE | FI_READ | FI_WRITE | FI_RECV | FI_SEND | \
			 FI_REMOTE_READ | FI_REMOTE_WRITE | \
			 FI_COMPLETION | FI_REMOTE_SIGNAL | FI_REMOTE_COMPLETE | \
			 FI_MORE | FI_CANCEL | FI_FENCE)

#define SOCK_EP_MSG_CAP SOCK_EP_RDM_CAP

#define SOCK_EP_DGRAM_CAP (FI_MSG | FI_TAGGED | FI_DYNAMIC_MR | \
			   FI_NAMED_RX_CTX | FI_DIRECTED_RECV | \
			   FI_MULTI_RECV | FI_SOURCE | FI_RECV | FI_SEND | \
			   FI_COMPLETION | FI_REMOTE_SIGNAL | \
			   FI_REMOTE_COMPLETE | FI_MORE | FI_CANCEL | \
			   FI_FENCE)

#define SOCK_DEF_OPS (FI_SEND | FI_RECV )

#define SOCK_EP_MSG_ORDER (FI_ORDER_RAR | FI_ORDER_RAW | FI_ORDER_RAS|	\
			   FI_ORDER_WAR | FI_ORDER_WAW | FI_ORDER_WAS |	\
			   FI_ORDER_SAR | FI_ORDER_SAW | FI_ORDER_SAS)

#define SOCK_MODE (0)

#define SOCK_COMM_BUF_SZ (SOCK_EP_MAX_MSG_SZ)
#define SOCK_COMM_THRESHOLD (128 * 1024)

#define SOCK_MAJOR_VERSION 1
#define SOCK_MINOR_VERSION 0

#define SOCK_INJECT_OK(_flgs)  ((_flgs) & FI_INJECT)

struct sock_fabric{
	struct fid_fabric fab_fid;
	atomic_t ref;
};

struct sock_conn {
        int sock_fd;
        struct sockaddr addr;
        struct sock_pe_entry *rx_pe_entry;
        struct sock_pe_entry *tx_pe_entry;
	struct ringbuf inbuf;
	struct ringbuf outbuf;
};

struct sock_conn_map {
        struct sock_conn *table;
        int used;
        int size;
	struct sock_domain *domain;
	fastlock_t lock;
	struct sockaddr_storage curr_addr;
};

struct sock_domain {
	struct fi_info info;
	struct fid_domain dom_fid;
	struct sock_fabric *fab;
	fastlock_t lock;
	atomic_t ref;
	short ep_count;
	
	struct sock_eq *eq;
	struct sock_eq *mr_eq;

	enum fi_progress progress_mode;
	struct index_map mr_idm;
	struct sock_pe *pe;
	struct sock_conn_map r_cmap;
	pthread_t listen_thread;
	int listening;
	char service[NI_MAXSERV];
	int signal_fds[2];
	struct sockaddr_storage src_addr;
};

struct sock_cntr {
	struct fid_cntr cntr_fid;
	struct sock_domain *domain;
	atomic_t value;
	atomic_t threshold;
	atomic_t ref;
	atomic_t err_cnt;
	pthread_cond_t 	cond;
	pthread_mutex_t mut;
	struct fi_cntr_attr attr;

	struct dlist_entry rx_list;
	struct dlist_entry tx_list;
	fastlock_t list_lock;

	struct fid_wait *waitset;
	int signal;
	int is_waiting;
};

struct sock_mr {
	struct fid_mr mr_fid;
	struct sock_domain *domain;
	uint64_t access;
	uint64_t offset;
	uint64_t key;
	uint64_t flags;
	size_t iov_count;
	struct iovec mr_iov[1];

	struct sock_cntr *cntr;
	struct sock_cq *cq;
};

struct sock_av_addr {
	struct sockaddr_storage addr;
	uint8_t valid;
	uint16_t rem_ep_id;
	uint8_t reserved[5];
};

struct sock_av_table_hdr {
	uint64_t size;
	uint64_t stored;
	uint64_t req_sz;
};

struct sock_av {
	struct fid_av av_fid;
	struct sock_domain *domain;
	atomic_t ref;
	struct fi_av_attr attr;
	uint64_t mask;
	int rx_ctx_bits;
	struct index_map addr_idm;
	socklen_t addrlen;
	struct sock_conn_map *cmap;
	struct sock_eq *eq;
	struct sock_av_table_hdr *table_hdr;
	struct sock_av_addr *table;
	uint16_t *key;
	char *name;
	int shared_fd;
};

struct sock_fid_list {
	struct dlist_entry entry;
	struct fid *fid;
};

struct sock_poll {
	struct fid_poll poll_fid;
	struct sock_domain *domain;
	struct dlist_entry fid_list;
};

struct sock_wait {
	struct fid_wait wait_fid;
	struct sock_fabric *fab;
	struct dlist_entry fid_list;
	enum fi_wait_obj type;
	union {
		int fd[2];
		struct sock_mutex_cond {
			pthread_mutex_t	mutex;
			pthread_cond_t	cond;
		} mutex_cond;
	} wobj;
};

enum {
	/* wire protocol */
	SOCK_OP_SEND = 0,
	SOCK_OP_TSEND = 1,
	SOCK_OP_SEND_COMPLETE = 2,

	SOCK_OP_WRITE = 3,
	SOCK_OP_WRITE_COMPLETE = 4,
	SOCK_OP_WRITE_ERROR = 5,

	SOCK_OP_READ = 6,
	SOCK_OP_READ_COMPLETE = 7,
	SOCK_OP_READ_ERROR = 8,

	SOCK_OP_ATOMIC = 9,
	SOCK_OP_ATOMIC_COMPLETE = 10,
	SOCK_OP_ATOMIC_ERROR = 11,

	/* internal */
	SOCK_OP_RECV,
	SOCK_OP_TRECV,
};

/*
 * Transmit context - ring buffer data:
 *    tx_op + flags + context + dest_addr + conn + [data] + [tag] + tx_iov
 *     8B       8B      8B         8B         8B       8B      24B+
 * data - only present if flags indicate
 * tag - only present for TSEND op
 */
struct sock_op {
	uint8_t op;
	uint8_t src_iov_len;
	uint8_t	dest_iov_len;
	struct {
		uint8_t	op;
		uint8_t	datatype;
		uint8_t	res_iov_len;
		uint8_t	cmp_iov_len;
	} atomic;
	uint8_t	reserved[1];
};

struct sock_op_send {
	struct sock_op op;
	uint64_t flags;
	uint64_t context;
	uint64_t dest_addr;
	struct sock_conn *conn;
	uint64_t buf;
	struct sock_ep *ep;
};

struct sock_op_tsend {
	struct sock_op op;
	uint64_t flags;
	uint64_t context;
	uint64_t dest_addr;
	struct sock_conn *conn;
	uint64_t tag;
	uint64_t buf;
	struct sock_ep *ep;
};

union sock_iov {
	struct fi_rma_iov iov;
	struct fi_rma_ioc ioc;
};

struct sock_eq_entry{
	uint32_t type;
	size_t len;
	uint64_t flags;
	struct dlist_entry entry;
	char event[0];
};

struct sock_eq{
	struct fid_eq eq;
	struct fi_eq_attr attr;
	struct sock_fabric *sock_fab;

	struct dlistfd_head list;
	struct dlistfd_head err_list;
	fastlock_t lock;

	struct fid_wait *waitset;
	int signal;
	int wait_fd;
	char service[NI_MAXSERV];
};

struct sock_comp {
	uint8_t send_cq_event;
	uint8_t recv_cq_event;
	uint8_t read_cq_event;
	uint8_t write_cq_event;
	uint8_t rem_read_cq_event;
	uint8_t rem_write_cq_event;
	char reserved[2];

	struct sock_cq	*send_cq;
	struct sock_cq	*recv_cq;
	struct sock_cq	*read_cq;
	struct sock_cq	*write_cq;
	struct sock_cq *rem_read_cq;
	struct sock_cq *rem_write_cq;

	struct sock_cntr *send_cntr;
	struct sock_cntr *recv_cntr;
	struct sock_cntr *read_cntr;
	struct sock_cntr *write_cntr;
	struct sock_cntr *rem_read_cntr;
	struct sock_cntr *rem_write_cntr;

	struct sock_eq *eq;
};

struct sock_cm_entry {
	int sock;
	int do_listen;
	int signal_fds[2];
	fastlock_t lock;
	int shutdown_received;
	pthread_t listener_thread;
	struct dlist_entry msg_list;
};

struct sock_ep {
	struct fid_ep ep;
	size_t fclass;
	uint64_t op_flags;

	uint8_t connected;
	uint8_t tx_shared;
	uint8_t rx_shared;
	uint16_t ep_id;
	uint16_t rem_ep_id;
	uint16_t buffered_len;
	uint16_t min_multi_recv;

	atomic_t ref;
	struct sock_comp comp;

	struct sock_eq *eq;
	struct sock_av *av;
	struct sock_domain *domain;	

	struct sock_rx_ctx *rx_ctx;
	struct sock_tx_ctx *tx_ctx;

	struct sock_rx_ctx **rx_array;
	struct sock_tx_ctx **tx_array;
	atomic_t num_rx_ctx;
	atomic_t num_tx_ctx;

	struct dlist_entry rx_ctx_entry;
	struct dlist_entry tx_ctx_entry;

	struct fi_info info;
	struct fi_ep_attr ep_attr;
	struct fi_tx_attr tx_attr;
	struct fi_rx_attr rx_attr;

	enum fi_ep_type ep_type;
	struct sockaddr_in *src_addr;
	struct sockaddr_in *dest_addr;

	struct sockaddr_in cm_addr;
	fid_t peer_fid;
	uint16_t key;
	int is_disabled;
	struct sock_cm_entry cm;
};

struct sock_pep {
	struct fid_pep	pep;
	struct sock_fabric *sock_fab;

	struct sock_cm_entry cm;
	struct sockaddr_in src_addr;
	struct fi_info info;
	struct sock_eq *eq;
};

struct sock_rx_entry {
	struct sock_op rx_op;
	uint8_t is_buffered;
	uint8_t is_busy;
	uint8_t is_claimed;
	uint8_t is_complete;
	uint8_t reserved[5];

	uint64_t used;
	uint64_t total_len;

	uint64_t flags;
	uint64_t context;
	uint64_t addr;
	uint64_t data;
	uint64_t tag;
	uint64_t ignore;
	struct sock_comp *comp;
	
	union sock_iov iov[SOCK_EP_MAX_IOV_LIMIT];
	struct dlist_entry entry;
};

struct sock_rx_ctx {
	struct fid_ep ctx;

	uint16_t rx_id;
	uint8_t enabled;
	uint8_t progress;

	uint8_t recv_cq_event;
	uint8_t rem_read_cq_event;
	uint8_t rem_write_cq_event;
	uint16_t buffered_len;
	uint16_t min_multi_recv;
	uint8_t reserved[7];

	uint64_t addr;
	struct sock_comp comp;

	struct sock_ep *ep;
	struct sock_av *av;
	struct sock_eq *eq;
 	struct sock_domain *domain;

	struct dlist_entry pe_entry;
	struct dlist_entry cq_entry;
	struct dlist_entry cntr_entry;

	struct dlist_entry pe_entry_list;
	struct dlist_entry rx_entry_list;
	struct dlist_entry rx_buffered_list;
	struct dlist_entry ep_list;
	fastlock_t lock;

	struct fi_rx_attr attr;
};

struct sock_tx_ctx {
	union {
		struct fid_ep ctx;
		struct fid_stx stx;
	} fid;
	size_t fclass;

	struct ringbuffd	rbfd;
	fastlock_t		wlock;
	fastlock_t		rlock;

	uint16_t tx_id;
	uint8_t enabled;
	uint8_t progress;

	uint64_t addr;
	struct sock_comp comp;

	struct sock_ep *ep;
	struct sock_av *av;
	struct sock_eq *eq;
 	struct sock_domain *domain;

	struct dlist_entry pe_entry;
	struct dlist_entry cq_entry;
	struct dlist_entry cntr_entry;

	struct dlist_entry pe_entry_list;
	struct dlist_entry ep_list;
	fastlock_t lock;

	struct fi_tx_attr attr;
};

#define SOCK_WIRE_PROTO_VERSION (0)

struct sock_msg_hdr{
	uint8_t version;
	uint8_t op_type;
	uint8_t rx_id;
	uint8_t dest_iov_len;
	uint16_t ep_id;
	uint16_t pe_entry_id;

	uint64_t flags;
	uint64_t msg_len;
};

struct sock_msg_send{
	struct sock_msg_hdr msg_hdr;
	/* user data */
	/* data */
};

struct sock_msg_tsend{
	struct sock_msg_hdr msg_hdr;
	uint64_t tag;
	/* user data */
	/* data */
};

struct sock_rma_write_req {
	struct sock_msg_hdr msg_hdr;
	/* user data */
	/* dst iov(s)*/
	/* data */
};

struct sock_atomic_req {
	struct sock_msg_hdr msg_hdr;
	struct sock_op op;

	/* user data */
	/* dst ioc(s)*/
	/* cmp iov(s) */
	/* data */
};

struct sock_msg_response {
	struct sock_msg_hdr msg_hdr;
	uint16_t pe_entry_id;
	uint8_t reserved[6];
};

struct sock_rma_read_req {
	struct sock_msg_hdr msg_hdr;
	/* src iov(s)*/
};

struct sock_rma_read_response {
	struct sock_msg_hdr msg_hdr;
	uint16_t pe_entry_id;
	uint8_t reserved[6];
	/* data */
};

struct sock_atomic_response {
	struct sock_msg_hdr msg_hdr;
	uint16_t pe_entry_id;
	uint8_t reserved[6];
	/* data */
};

struct sock_tx_iov {
	union sock_iov src;
	union sock_iov dst;
	union sock_iov res;
	union sock_iov cmp;
};

struct sock_tx_pe_entry{
	struct sock_op tx_op;
	struct sock_comp *comp;
	uint8_t header_sent;
	uint8_t send_done;
	uint8_t reserved[6];

	struct sock_tx_ctx *tx_ctx;
	union {
		struct sock_tx_iov tx_iov[SOCK_EP_MAX_IOV_LIMIT];
		char inject[SOCK_EP_MAX_INJECT_SZ];
	} data;
};

struct sock_rx_pe_entry{
	struct sock_op rx_op;

	struct sock_comp *comp;
	uint8_t header_read;
	uint8_t pending_send;
	uint8_t reserved[6];
	struct sock_rx_entry *rx_entry;
	union sock_iov rx_iov[SOCK_EP_MAX_IOV_LIMIT];
	char atomic_cmp[SOCK_EP_MAX_ATOMIC_SZ];
	char atomic_src[SOCK_EP_MAX_ATOMIC_SZ];
};

/* PE entry type */
enum{
	SOCK_PE_RX,
	SOCK_PE_TX,
};

struct sock_pe_entry{
	union {
		struct sock_tx_pe_entry tx;
		struct sock_rx_pe_entry rx;
	} pe;

	struct sock_msg_hdr msg_hdr;
	struct sock_msg_response response;

	uint64_t flags;
	uint64_t context;
	uint64_t addr;
	uint64_t data;
	uint64_t tag;
	uint64_t buf;

	uint8_t type;
	uint8_t is_complete;
	uint8_t reserved[6];

	uint64_t done_len;
	uint64_t total_len;
	uint64_t data_len;
	struct sock_ep *ep;
	struct sock_conn *conn;
	struct sock_comp *comp;

	struct dlist_entry entry;
	struct dlist_entry ctx_entry;
};

struct sock_pe{
	struct sock_domain *domain;
	int num_free_entries;
	struct sock_pe_entry pe_table[SOCK_PE_MAX_ENTRIES];
	fastlock_t lock;

	struct dlist_entry free_list;
	struct dlist_entry busy_list;

	struct dlistfd_head tx_list;
	struct dlistfd_head rx_list;

	pthread_t progress_thread;
	volatile int do_progress;
	struct sock_pe_entry *pe_atomic;
};

typedef int (*sock_cq_report_fn) (struct sock_cq *cq, fi_addr_t addr,
				  struct sock_pe_entry *pe_entry);

struct sock_cq {
	struct fid_cq cq_fid;
	struct sock_domain *domain;
	ssize_t cq_entry_size;
	atomic_t ref;
	struct fi_cq_attr attr;

	struct ringbuf addr_rb;
	struct ringbuffd cq_rbfd;
	struct ringbuf cqerr_rb;
	fastlock_t lock;
	fastlock_t list_lock;

	struct fid_wait *waitset;
	int signal;

	struct dlist_entry ep_list;
	struct dlist_entry rx_list;
	struct dlist_entry tx_list;

	sock_cq_report_fn report_completion;
};

struct sock_cm_msg_list_entry {
	size_t msg_len;
	struct sockaddr_in addr;
	struct dlist_entry entry;
	char msg[0];
};

struct sock_conn_hdr {
	uint8_t type;
	uint8_t reserved[3];
	int32_t s_port;
	fid_t c_fid;
	fid_t s_fid;
};

struct sock_conn_req {
	struct sock_conn_hdr hdr;
	uint16_t ep_id;
	struct fi_info info;
	struct sockaddr_in src_addr;
	struct sockaddr_in dest_addr;
	struct fi_tx_attr	tx_attr;
	struct fi_rx_attr	rx_attr;
	struct fi_ep_attr	ep_attr;
	struct fi_domain_attr	domain_attr;
	struct fi_fabric_attr	fabric_attr;
	struct sockaddr_in from_addr;
	char user_data[0];
};

struct sock_conn_response {
	struct sock_conn_hdr hdr;
	char user_data[0];
};

enum {
	SOCK_CONN_REQ,
	SOCK_CONN_ACCEPT,
	SOCK_CONN_REJECT,
	SOCK_CONN_SHUTDOWN,
};

int sock_verify_info(struct fi_info *hints);
int sock_verify_fabric_attr(struct fi_fabric_attr *attr);
int sock_verify_domain_attr(struct fi_domain_attr *attr);

int sock_rdm_verify_ep_attr(struct fi_ep_attr *ep_attr, struct fi_tx_attr *tx_attr,
			    struct fi_rx_attr *rx_attr);
int sock_dgram_verify_ep_attr(struct fi_ep_attr *ep_attr, struct fi_tx_attr *tx_attr,
			      struct fi_rx_attr *rx_attr);
int sock_msg_verify_ep_attr(struct fi_ep_attr *ep_attr, struct fi_tx_attr *tx_attr,
			    struct fi_rx_attr *rx_attr);


struct fi_info *sock_fi_info(enum fi_ep_type ep_type, 
			     struct fi_info *hints, void *src_addr, void *dest_addr);
int sock_rdm_getinfo(uint32_t version, const char *node, const char *service,
		     uint64_t flags, struct fi_info *hints, struct fi_info **info);
int sock_dgram_getinfo(uint32_t version, const char *node, const char *service,
		       uint64_t flags, struct fi_info *hints, struct fi_info **info);
int sock_msg_getinfo(uint32_t version, const char *node, const char *service,
		     uint64_t flags, struct fi_info *hints, struct fi_info **info);
void free_fi_info(struct fi_info *info);

int sock_msg_getinfo(uint32_t version, const char *node, const char *service,
		uint64_t flags, struct fi_info *hints, struct fi_info **info);

int sock_domain(struct fid_fabric *fabric, struct fi_info *info,
		struct fid_domain **dom, void *context);


int sock_alloc_endpoint(struct fid_domain *domain, struct fi_info *info,
			struct sock_ep **ep, void *context, size_t fclass);
struct sock_conn *sock_ep_lookup_conn(struct sock_ep *ep);
int sock_rdm_ep(struct fid_domain *domain, struct fi_info *info,
		struct fid_ep **ep, void *context);
int sock_rdm_sep(struct fid_domain *domain, struct fi_info *info,
		 struct fid_ep **sep, void *context);

int sock_dgram_ep(struct fid_domain *domain, struct fi_info *info,
		  struct fid_ep **ep, void *context);
int sock_dgram_sep(struct fid_domain *domain, struct fi_info *info,
		   struct fid_ep **sep, void *context);

int sock_msg_ep(struct fid_domain *domain, struct fi_info *info,
		struct fid_ep **ep, void *context);
int sock_msg_sep(struct fid_domain *domain, struct fi_info *info,
		 struct fid_ep **sep, void *context);
int sock_msg_passive_ep(struct fid_fabric *fabric, struct fi_info *info,
			struct fid_pep **pep, void *context);
int sock_ep_enable(struct fid_ep *ep);
int sock_ep_disable(struct fid_ep *ep);


int sock_stx_ctx(struct fid_domain *domain,
		 struct fi_tx_attr *attr, struct fid_stx **stx, void *context);
int sock_srx_ctx(struct fid_domain *domain,
		 struct fi_rx_attr *attr, struct fid_ep **srx, void *context);


int sock_cq_open(struct fid_domain *domain, struct fi_cq_attr *attr,
		 struct fid_cq **cq, void *context);
int sock_cq_report_error(struct sock_cq *cq, struct sock_pe_entry *entry,
			 size_t olen, int err, int prov_errno, void *err_data);
int sock_cq_progress(struct sock_cq *cq);


int sock_eq_open(struct fid_fabric *fabric, struct fi_eq_attr *attr,
		struct fid_eq **eq, void *context);
ssize_t sock_eq_report_event(struct sock_eq *sock_eq, uint32_t event, 
			     const void *buf, size_t len, uint64_t flags);
ssize_t sock_eq_report_error(struct sock_eq *sock_eq, fid_t fid, void *context,
			     int err, int prov_errno, void *err_data);
int sock_eq_openwait(struct sock_eq *eq, const char *service);

int sock_cntr_open(struct fid_domain *domain, struct fi_cntr_attr *attr,
		   struct fid_cntr **cntr, void *context);
int sock_cntr_inc(struct sock_cntr *cntr);
int sock_cntr_err_inc(struct sock_cntr *cntr);
int sock_cntr_progress(struct sock_cntr *cntr);


int sock_eq_open(struct fid_fabric *fabric, struct fi_eq_attr *attr,
		 struct fid_eq **eq, void *context);
ssize_t sock_eq_report_event(struct sock_eq *sock_eq, uint32_t event, 
			     const void *buf, size_t len, uint64_t flags);
ssize_t sock_eq_report_error(struct sock_eq *sock_eq, fid_t fid, void *context,
			     int err, int prov_errno, void *err_data);


struct sock_mr *sock_mr_verify_key(struct sock_domain *domain, uint16_t key, 
				   void *buf, size_t len, uint64_t access);
struct sock_mr *sock_mr_verify_desc(struct sock_domain *domain, void *desc, 
				    void *buf, size_t len, uint64_t access);
struct sock_mr * sock_mr_get_entry(struct sock_domain *domain, uint16_t key);


struct sock_rx_ctx *sock_rx_ctx_alloc(const struct fi_rx_attr *attr, void *context);
void sock_rx_ctx_free(struct sock_rx_ctx *rx_ctx);

struct sock_tx_ctx *sock_tx_ctx_alloc(const struct fi_tx_attr *attr, void *context);
void sock_tx_ctx_free(struct sock_tx_ctx *tx_ctx);
void sock_tx_ctx_start(struct sock_tx_ctx *tx_ctx);
void sock_tx_ctx_write(struct sock_tx_ctx *tx_ctx, const void *buf, size_t len);
void sock_tx_ctx_commit(struct sock_tx_ctx *tx_ctx);
void sock_tx_ctx_abort(struct sock_tx_ctx *tx_ctx);


int sock_poll_open(struct fid_domain *domain, struct fi_poll_attr *attr,
		   struct fid_poll **pollset);
int sock_wait_open(struct fid_fabric *fabric, struct fi_wait_attr *attr,
		   struct fid_wait **waitset);
void sock_wait_signal(struct fid_wait *wait_fid);
int sock_wait_get_obj(struct fid_wait *fid, void *arg);
int sock_wait_close(fid_t fid);


int sock_av_open(struct fid_domain *domain, struct fi_av_attr *attr,
		 struct fid_av **av, void *context);
fi_addr_t _sock_av_lookup(struct sock_av *av, struct sockaddr *addr);
fi_addr_t sock_av_get_fiaddr(struct sock_av *av, struct sock_conn *conn);
fi_addr_t sock_av_lookup_key(struct sock_av *av, int key);
struct sock_conn *sock_av_lookup_addr(struct sock_av *av, fi_addr_t addr);
int sock_av_compare_addr(struct sock_av *av, fi_addr_t addr1, fi_addr_t addr2);
uint16_t sock_av_lookup_ep_id(struct sock_av *av, fi_addr_t addr);


struct sock_conn *sock_conn_map_lookup_key(struct sock_conn_map *conn_map, 
					   uint16_t key);
uint16_t sock_conn_map_connect(struct sock_domain *dom,
			       struct sock_conn_map *map, 
			       struct sockaddr_in *addr);
uint16_t sock_conn_map_lookup(struct sock_conn_map *map,
			      struct sockaddr_in *addr);
uint16_t sock_conn_map_match_or_connect(struct sock_domain *dom,
					struct sock_conn_map *map, 
					struct sockaddr_in *addr);
int sock_conn_listen(struct sock_domain *domain);
int sock_conn_map_clear_pe_entry(struct sock_conn *conn_entry, uint16_t key);
void sock_conn_map_destroy(struct sock_conn_map *cmap);


struct sock_pe *sock_pe_init(struct sock_domain *domain);
void sock_pe_add_tx_ctx(struct sock_pe *pe, struct sock_tx_ctx *ctx);
void sock_pe_add_rx_ctx(struct sock_pe *pe, struct sock_rx_ctx *ctx);
int sock_pe_progress_rx_ctx(struct sock_pe *pe, struct sock_rx_ctx *rx_ctx);
int sock_pe_progress_tx_ctx(struct sock_pe *pe, struct sock_tx_ctx *tx_ctx);
void sock_pe_remove_tx_ctx(struct sock_tx_ctx *tx_ctx);
void sock_pe_remove_rx_ctx(struct sock_rx_ctx *rx_ctx);
void sock_pe_finalize(struct sock_pe *pe);


struct sock_rx_entry *sock_rx_new_entry(struct sock_rx_ctx *rx_ctx);
struct sock_rx_entry *sock_rx_new_buffered_entry(struct sock_rx_ctx *rx_ctx,
						 size_t len);
struct sock_rx_entry *sock_rx_get_entry(struct sock_rx_ctx *rx_ctx, 
					uint64_t addr, uint64_t tag);
size_t sock_rx_avail_len(struct sock_rx_entry *rx_entry);
void sock_rx_release_entry(struct sock_rx_entry *rx_entry);


int sock_comm_buffer_init(struct sock_conn *conn);
void sock_comm_buffer_finalize(struct sock_conn *conn);
ssize_t sock_comm_send(struct sock_conn *conn, const void *buf, size_t len);
ssize_t sock_comm_recv(struct sock_conn *conn, void *buf, size_t len);
ssize_t sock_comm_peek(struct sock_conn *conn, void *buf, size_t len);
ssize_t sock_comm_flush(struct sock_conn *conn);

#endif
