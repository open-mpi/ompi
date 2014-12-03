#ifndef _FI_PSM_H
#define _FI_PSM_H

#ifdef __cplusplus
extern "C" {
#endif

#if HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <fcntl.h>
#include <pthread.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <complex.h>
#include <rdma/fabric.h>
#include <rdma/fi_prov.h>
#include <rdma/fi_domain.h>
#include <rdma/fi_endpoint.h>
#include <rdma/fi_tagged.h>
#include <rdma/fi_rma.h>
#include <rdma/fi_atomic.h>
#include <rdma/fi_trigger.h>
#include <rdma/fi_cm.h>
#include <rdma/fi_errno.h>
#include <psm.h>
#include <psm_mq.h>
#include "psm_am.h"
#include "fi.h"
#include "fi_enosys.h"
#include "fi_list.h"

#define PSM_PFX "libfabric:psm"

#define PSMX_FREE_LIST_INIT(head, tail, type, count) \
	do { \
		int i; \
		type *item; \
		head = tail = NULL; \
		for (i=0; i<count; i++) { \
			item = calloc(sizeof(type), 1); \
			if (!item) {\
				fprintf(stderr, "%s: out of memory.\n", __func__); \
				exit(-1); \
			} \
			item->next = head; \
			head = item; \
			if (!tail) \
				tail = head; \
		} \
	} while (0)

#define PSMX_FREE_LIST_GET(head, tail, type, item) \
	do { \
		if (head) { \
			item = head; \
			head = head->next; \
			if (!head) \
				tail = head; \
			item->next = NULL; \
		} \
		else { \
			item = calloc(sizeof(type), 1); \
			if (!item) {\
				fprintf(stderr, "%s: out of memory.\n", __func__); \
				exit(-1); \
			} \
		} \
	} while (0)

#define PSMX_FREE_LIST_PUT(head, tail, type, item) \
	do { \
		memset(item, 0, sizeof(type)); \
		if (tail) \
			tail->next = item; \
		else \
			head = tail = item; \
	} while (0)

#define PSMX_FREE_LIST_FINALIZE(head, tail, type) \
	do { \
		type *next; \
		while (head) { \
			next = head->next; \
			free(head); \
			head = next; \
		} \
		tail = NULL; \
	} while (0)

#define PSMX_TIME_OUT	120

#define PSMX_OP_FLAGS	(FI_INJECT | FI_MULTI_RECV | FI_EVENT | \
			 FI_TRIGGER | FI_REMOTE_SIGNAL | FI_REMOTE_COMPLETE)

#define PSMX_CAP_EXT	(0)

#define PSMX_CAPS	(FI_TAGGED | FI_MSG | FI_ATOMICS | FI_INJECT | \
			 FI_RMA | FI_BUFFERED_RECV | FI_MULTI_RECV | \
                         FI_READ | FI_WRITE | FI_SEND | FI_RECV | \
                         FI_REMOTE_READ | FI_REMOTE_WRITE | \
                         FI_REMOTE_COMPLETE | FI_REMOTE_SIGNAL | \
			 FI_CANCEL | FI_TRIGGER | \
			 FI_DYNAMIC_MR | \
			 PSMX_CAP_EXT)

#define PSMX_MODE	(FI_CONTEXT)

#define PSMX_MAX_MSG_SIZE	((0x1ULL << 32) - 1)
#define PSMX_INJECT_SIZE	(64)

#define PSMX_MSG_BIT	(0x1ULL << 63)
#define PSMX_RMA_BIT	(0x1ULL << 62)

enum psmx_context_type {
	PSMX_NOCOMP_SEND_CONTEXT = 1,
	PSMX_NOCOMP_RECV_CONTEXT,
	PSMX_NOCOMP_WRITE_CONTEXT,
	PSMX_NOCOMP_READ_CONTEXT,
	PSMX_SEND_CONTEXT,
	PSMX_RECV_CONTEXT,
	PSMX_MULTI_RECV_CONTEXT,
	PSMX_WRITE_CONTEXT,
	PSMX_READ_CONTEXT,
	PSMX_INJECT_CONTEXT,
	PSMX_INJECT_WRITE_CONTEXT,
	PSMX_REMOTE_WRITE_CONTEXT,
	PSMX_REMOTE_READ_CONTEXT,
};

#define PSMX_CTXT_REQ(fi_context)	((fi_context)->internal[0])
#define PSMX_CTXT_TYPE(fi_context)	(*(int *)&(fi_context)->internal[1])
#define PSMX_CTXT_USER(fi_context)	((fi_context)->internal[2])
#define PSMX_CTXT_EP(fi_context)	((fi_context)->internal[3])

#define PSMX_AM_RMA_HANDLER	0
#define PSMX_AM_MSG_HANDLER	1
#define PSMX_AM_ATOMIC_HANDLER	2
#define PSMX_AM_CHUNK_SIZE	2032	/* The maximum that's actually working:
					 * 2032 for inter-node, 2072 for intra-node.
					 */

#define PSMX_AM_OP_MASK		0x0000FFFF
#define PSMX_AM_FLAG_MASK	0xFFFF0000
#define PSMX_AM_EOM		0x40000000
#define PSMX_AM_DATA		0x20000000

#ifndef PSMX_AM_USE_SEND_QUEUE
#define PSMX_AM_USE_SEND_QUEUE	0
#endif

enum {
	PSMX_AM_REQ_WRITE = 1,
	PSMX_AM_REQ_WRITE_LONG,
	PSMX_AM_REP_WRITE,
	PSMX_AM_REQ_READ,
	PSMX_AM_REQ_READ_LONG,
	PSMX_AM_REP_READ,
	PSMX_AM_REQ_SEND,
	PSMX_AM_REP_SEND,
	PSMX_AM_REQ_ATOMIC_WRITE,
	PSMX_AM_REP_ATOMIC_WRITE,
	PSMX_AM_REQ_ATOMIC_READWRITE,
	PSMX_AM_REP_ATOMIC_READWRITE,
	PSMX_AM_REQ_ATOMIC_COMPWRITE,
	PSMX_AM_REP_ATOMIC_COMPWRITE,
};

enum {
	PSMX_AM_STATE_NEW,
	PSMX_AM_STATE_QUEUED,
	PSMX_AM_STATE_PROCESSED,
	PSMX_AM_STATE_DONE
};

struct psmx_am_request {
	int op;
	union {
		struct {
			void	*buf;
			size_t	len;
			uint64_t addr;
			uint64_t key;
			void	*context;
			uint64_t data;
		} write;
		struct {
			void	*buf;
			size_t	len;
			uint64_t addr;
			uint64_t key;
			void	*context;
			void	*peer_addr;
			size_t	len_read;
		} read;
		struct {
			void	*buf;
			size_t	len;
			void	*context;
			void	*peer_context;
			volatile int peer_ready;
			void	*dest_addr;
			size_t	len_sent;
		} send;
		struct {
			void	*buf;
			size_t	len;
			void	*context;
			void	*src_addr;
			size_t  len_received;
		} recv;
		struct {
			void	*buf;
			size_t	len;
			uint64_t addr;
			uint64_t key;
			void	*context;
			void 	*result;
		} atomic;
	};
	struct fi_context fi_context;
	struct psmx_fid_ep *ep;
	struct psmx_am_request *next;
	int state;
	int no_event;
	int error;
};

struct psmx_req_queue {
	pthread_mutex_t		lock;
	struct psmx_am_request	*head;
	struct psmx_am_request	*tail;
};

struct psmx_multi_recv {
	uint64_t	tag;
	uint64_t	tagsel;
	void		*buf;
	size_t		len;
	size_t		offset;
	int		min_buf_size;
	int		flag;
	void		*context;
};

struct psmx_fid_fabric {
	struct fid_fabric	fabric;
};

struct psmx_fid_domain {
	struct fid_domain	domain;
	psm_ep_t		psm_ep;
	psm_epid_t		psm_epid;
	psm_mq_t		psm_mq;
	pthread_t		ns_thread;
	int			ns_port;
	struct psmx_fid_ep	*tagged_ep;
	struct psmx_fid_ep	*msg_ep;
	struct psmx_fid_ep	*rma_ep;
	struct psmx_fid_ep	*atomics_ep;
	uint64_t		mode;

	int			am_initialized;

#if PSMX_AM_USE_SEND_QUEUE
	pthread_cond_t		progress_cond;
	pthread_mutex_t		progress_mutex;
	pthread_t		progress_thread;
#endif

	/* incoming req queue for AM based RMA request. */
	struct psmx_req_queue	rma_queue;

#if PSMX_AM_USE_SEND_QUEUE
	/* send queue for AM based messages. */
	struct psmx_req_queue	send_queue;
#endif

	/* recv queue for AM based messages. */
	struct psmx_req_queue	recv_queue;
	struct psmx_req_queue	unexp_queue;

	/* certain bits in the tag space can be reserved for non tag-matching
	 * purpose. The tag-matching functions automatically treat these bits
	 * as 0. This field is a bit mask, with reserved bits valued as "1".
	 */
	uint64_t		reserved_tag_bits; 
};

struct psmx_cq_event {
	union {
		struct fi_cq_entry		context;
		struct fi_cq_msg_entry		msg;
		struct fi_cq_data_entry		data;
		struct fi_cq_tagged_entry	tagged;
		struct fi_cq_err_entry		err;
	} cqe;
	int error;
	uint64_t source;
	struct psmx_cq_event *next;
};

struct psmx_cq_event_queue {
	struct psmx_cq_event	*head;
	struct psmx_cq_event	*tail;
	size_t	count;
};

struct psmx_fid_wait {
	struct fid_wait			wait;
	struct psmx_fid_domain		*domain;
	int				type;
	union {
		int			fd[2];
		struct {
			pthread_mutex_t	mutex;
			pthread_cond_t	cond;
		};
	};
};

struct psmx_poll_list {
	struct dlist_entry		entry;
	struct fid			*fid;
};

struct psmx_fid_poll {
	struct fid_poll			poll;
	struct psmx_fid_domain		*domain;
	struct dlist_entry		poll_list_head;
};

struct psmx_fid_cq {
	struct fid_cq			cq;
	struct psmx_fid_domain		*domain;
	int 				format;
	int				entry_size;
	struct psmx_cq_event_queue	event_queue;
	struct psmx_cq_event_queue	free_list;
	struct psmx_cq_event		*pending_error;
	struct psmx_fid_wait		*wait;
	int				wait_cond;
};

enum psmx_triggered_op {
	PSMX_TRIGGERED_SEND,
	PSMX_TRIGGERED_RECV,
	PSMX_TRIGGERED_TSEND,
	PSMX_TRIGGERED_TRECV,
	PSMX_TRIGGERED_WRITE,
	PSMX_TRIGGERED_READ,
	PSMX_TRIGGERED_ATOMIC_WRITE,
	PSMX_TRIGGERED_ATOMIC_READWRITE,
	PSMX_TRIGGERED_ATOMIC_COMPWRITE,
};

struct psmx_trigger {
	enum psmx_triggered_op	op;
	struct psmx_fid_cntr	*cntr;
	size_t			threshold;
	union {
		struct {
			struct fid_ep	*ep;
			const void	*buf;
			size_t		len;
			void		*desc;
			fi_addr_t	dest_addr;
			void		*context;
			uint64_t	flags;
		} send;
		struct {
			struct fid_ep	*ep;
			void		*buf;
			size_t		len;
			void		*desc;
			fi_addr_t	src_addr;
			void		*context;
			uint64_t	flags;
		} recv;
		struct {
			struct fid_ep	*ep;
			const void	*buf;
			size_t		len;
			void		*desc;
			fi_addr_t	dest_addr;
			uint64_t	tag;
			void		*context;
			uint64_t	flags;
		} tsend;
		struct {
			struct fid_ep	*ep;
			void		*buf;
			size_t		len;
			void		*desc;
			fi_addr_t	src_addr;
			uint64_t	tag;
			uint64_t	ignore;
			void		*context;
			uint64_t	flags;
		} trecv;
		struct {
			struct fid_ep	*ep;
			const void	*buf;
			size_t		len;
			void		*desc;
			fi_addr_t	dest_addr;
			uint64_t	addr;
			uint64_t	key;
			void		*context;
			uint64_t	flags;
			uint64_t	data;
		} write;
		struct {
			struct fid_ep	*ep;
			void		*buf;
			size_t		len;
			void		*desc;
			fi_addr_t	src_addr;
			uint64_t	addr;
			uint64_t	key;
			void		*context;
			uint64_t	flags;
		} read;
		struct {
			struct fid_ep	*ep;
			const void	*buf;
			size_t		count;
			void		*desc;
			fi_addr_t	dest_addr;
			uint64_t	addr;
			uint64_t	key;
			enum fi_datatype datatype;
			enum fi_op	atomic_op;
			void		*context;
			uint64_t	flags;
		} atomic_write;
		struct {
			struct fid_ep	*ep;
			const void	*buf;
			size_t		count;
			void		*desc;
			void		*result;
			void		*result_desc;
			fi_addr_t	dest_addr;
			uint64_t	addr;
			uint64_t	key;
			enum fi_datatype datatype;
			enum fi_op	atomic_op;
			void		*context;
			uint64_t	flags;
		} atomic_readwrite;
		struct {
			struct fid_ep	*ep;
			const void	*buf;
			size_t		count;
			void		*desc;
			const void	*compare;
			void		*compare_desc;
			void		*result;
			void		*result_desc;
			fi_addr_t	dest_addr;
			uint64_t	addr;
			uint64_t	key;
			enum fi_datatype datatype;
			enum fi_op	atomic_op;
			void		*context;
			uint64_t	flags;
		} atomic_compwrite;
	};
	struct psmx_trigger *next;
};

struct psmx_fid_cntr {
	struct fid_cntr		cntr;
	struct psmx_fid_domain	*domain;
	int			events;
	uint64_t		flags;
	volatile uint64_t	counter;
	volatile uint64_t	error_counter;
	uint64_t		counter_last_read;
	uint64_t		error_counter_last_read;
	struct psmx_fid_wait	*wait;
	struct psmx_trigger	*trigger;
	pthread_mutex_t		trigger_lock;
};

struct psmx_fid_av {
	struct fid_av		av;
	struct psmx_fid_domain	*domain;
	int			type;
	size_t			addrlen;
	size_t			count;
	size_t			last;
	psm_epid_t		*psm_epids;
	psm_epaddr_t		*psm_epaddrs;
};

struct psmx_fid_ep {
	struct fid_ep		ep;
	struct psmx_fid_domain	*domain;
	struct psmx_fid_av	*av;
	struct psmx_fid_cq	*send_cq;
	struct psmx_fid_cq	*recv_cq;
	struct psmx_fid_cntr	*send_cntr;
	struct psmx_fid_cntr	*recv_cntr;
	struct psmx_fid_cntr	*write_cntr;
	struct psmx_fid_cntr	*read_cntr;
	struct psmx_fid_cntr	*remote_write_cntr;
	struct psmx_fid_cntr	*remote_read_cntr;
	int			send_cq_event_flag:1;
	int			recv_cq_event_flag:1;
	uint64_t		flags;
	uint64_t		caps;
	int			connected;
	psm_epid_t		peer_psm_epid;
	psm_epaddr_t		peer_psm_epaddr;
	struct fi_context	nocomp_send_context;
	struct fi_context	nocomp_recv_context;
	struct fi_context	sendimm_context;
	struct fi_context	writeimm_context;
	size_t			min_multi_recv;
};

struct psmx_fid_mr {
	struct fid_mr		mr;
	struct psmx_fid_domain	*domain;
	struct psmx_fid_cq	*cq;
	struct psmx_fid_cntr	*cntr;
	uint64_t		access;
	uint64_t		flags;
	uint64_t		offset;
	size_t			iov_count;
	struct iovec		iov[0];	/* must be the last field */
};

struct psmx_epaddr_context {
	struct psmx_fid_domain	*domain;
	psm_epid_t		epid;
};

struct psmx_env {
	int name_server;
	int am_msg;
	int tagged_rma;
	int debug;
	int warning;
	char *uuid;
};

extern struct fi_ops_mr		psmx_mr_ops;
extern struct fi_ops_cm		psmx_cm_ops;
extern struct fi_ops_tagged	psmx_tagged_ops;
extern struct fi_ops_tagged	psmx_tagged_ops_no_flag_av_map;
extern struct fi_ops_tagged	psmx_tagged_ops_no_flag_av_table;
extern struct fi_ops_tagged	psmx_tagged_ops_no_event_av_map;
extern struct fi_ops_tagged	psmx_tagged_ops_no_event_av_table;
extern struct fi_ops_tagged	psmx_tagged_ops_no_send_event_av_map;
extern struct fi_ops_tagged	psmx_tagged_ops_no_send_event_av_table;
extern struct fi_ops_tagged	psmx_tagged_ops_no_recv_event_av_map;
extern struct fi_ops_tagged	psmx_tagged_ops_no_recv_event_av_table;
extern struct fi_ops_msg	psmx_msg_ops;
extern struct fi_ops_msg	psmx_msg2_ops;
extern struct fi_ops_rma	psmx_rma_ops;
extern struct fi_ops_atomic	psmx_atomic_ops;
extern struct psm_am_parameters psmx_am_param;
extern struct psmx_env		psmx_env;

int	psmx_domain_open(struct fid_fabric *fabric, struct fi_info *info,
			 struct fid_domain **domain, void *context);
int	psmx_ep_open(struct fid_domain *domain, struct fi_info *info,
		     struct fid_ep **ep, void *context);
int	psmx_cq_open(struct fid_domain *domain, struct fi_cq_attr *attr,
		     struct fid_cq **cq, void *context);
int	psmx_av_open(struct fid_domain *domain, struct fi_av_attr *attr,
		     struct fid_av **av, void *context);
int	psmx_cntr_open(struct fid_domain *domain, struct fi_cntr_attr *attr,
		       struct fid_cntr **cntr, void *context);
int	psmx_wait_open(struct fid_domain *domain, struct fi_wait_attr *attr,
		       struct fid_wait **waitset);
int	psmx_poll_open(struct fid_domain *domain, struct fi_poll_attr *attr,
		       struct fid_poll **pollset);

int	psmx_domain_check_features(struct psmx_fid_domain *domain, int ep_cap);
int	psmx_domain_enable_ep(struct psmx_fid_domain *domain, struct psmx_fid_ep *ep);
void	psmx_domain_disable_ep(struct psmx_fid_domain *domain, struct psmx_fid_ep *ep);
void 	*psmx_name_server(void *args);
void	*psmx_resolve_name(const char *servername, int port);
void	psmx_get_uuid(psm_uuid_t uuid);
int	psmx_uuid_to_port(psm_uuid_t uuid);
int	psmx_errno(int err);
int	psmx_epid_to_epaddr(struct psmx_fid_domain *domain,
			    psm_epid_t epid, psm_epaddr_t *epaddr);
void	psmx_query_mpi(void);
void	psmx_debug(char *fmt, ...);

void	psmx_cq_enqueue_event(struct psmx_fid_cq *cq, struct psmx_cq_event *event);
struct	psmx_cq_event *psmx_cq_create_event(struct psmx_fid_cq *cq,
					void *op_context, void *buf,
					uint64_t flags, size_t len,
					uint64_t data, uint64_t tag,
					size_t olen, int err);
int	psmx_cq_poll_mq(struct psmx_fid_cq *cq, struct psmx_fid_domain *domain,
			struct psmx_cq_event *event, int count, fi_addr_t *src_addr);
int	psmx_wait_get_obj(struct psmx_fid_wait *wait, void *arg);
int	psmx_wait_wait(struct fid_wait *wait, int timeout);
void	psmx_wait_signal(struct fid_wait *wait);

int	psmx_am_init(struct psmx_fid_domain *domain);
int	psmx_am_fini(struct psmx_fid_domain *domain);
int	psmx_am_enqueue_recv(struct psmx_fid_domain *domain,
				struct psmx_am_request *req);
struct psmx_am_request *
	psmx_am_search_and_dequeue_recv(struct psmx_fid_domain *domain,
					const void *src_addr);
#if PSMX_AM_USE_SEND_QUEUE
int	psmx_am_enqueue_send(struct psmx_fid_domain *domain,
				  struct psmx_am_request *req);
#endif
int	psmx_am_enqueue_rma(struct psmx_fid_domain *domain,
				  struct psmx_am_request *req);
int	psmx_am_progress(struct psmx_fid_domain *domain);
int	psmx_am_process_send(struct psmx_fid_domain *domain,
				struct psmx_am_request *req);
int	psmx_am_process_rma(struct psmx_fid_domain *domain,
				struct psmx_am_request *req);
int	psmx_am_msg_handler(psm_am_token_t token, psm_epaddr_t epaddr,
				psm_amarg_t *args, int nargs, void *src, uint32_t len);
int	psmx_am_rma_handler(psm_am_token_t token, psm_epaddr_t epaddr,
				psm_amarg_t *args, int nargs, void *src, uint32_t len);
int	psmx_am_atomic_handler(psm_am_token_t token, psm_epaddr_t epaddr,
				psm_amarg_t *args, int nargs, void *src, uint32_t len);

struct	psmx_fid_mr *psmx_mr_hash_get(uint64_t key);
int	psmx_mr_validate(struct psmx_fid_mr *mr, uint64_t addr, size_t len, uint64_t access);
void	psmx_cntr_check_trigger(struct psmx_fid_cntr *cntr);
void	psmx_cntr_add_trigger(struct psmx_fid_cntr *cntr, struct psmx_trigger *trigger);

static inline void psmx_cntr_inc(struct psmx_fid_cntr *cntr)
{
	cntr->counter++;
	psmx_cntr_check_trigger(cntr);
	if (cntr->wait)
		psmx_wait_signal((struct fid_wait *)cntr->wait);
}

ssize_t _psmx_send(struct fid_ep *ep, const void *buf, size_t len,
		   void *desc, fi_addr_t dest_addr, void *context,
		   uint64_t flags);
ssize_t _psmx_recv(struct fid_ep *ep, void *buf, size_t len,
		   void *desc, fi_addr_t src_addr, void *context,
		   uint64_t flags);
ssize_t _psmx_tagged_send(struct fid_ep *ep, const void *buf, size_t len,
			  void *desc, fi_addr_t dest_addr, uint64_t tag,
			  void *context, uint64_t flags);
ssize_t _psmx_tagged_recv(struct fid_ep *ep, void *buf, size_t len,
			  void *desc, fi_addr_t src_addr, uint64_t tag,
			  uint64_t ignore, void *context, uint64_t flags);
ssize_t _psmx_write(struct fid_ep *ep, const void *buf, size_t len,
		    void *desc, fi_addr_t dest_addr,
		    uint64_t addr, uint64_t key, void *context,
		    uint64_t flags, uint64_t data);
ssize_t _psmx_read(struct fid_ep *ep, void *buf, size_t len,
		   void *desc, fi_addr_t src_addr,
		   uint64_t addr, uint64_t key, void *context,
		   uint64_t flags);
ssize_t _psmx_atomic_write(struct fid_ep *ep,
			   const void *buf,
			   size_t count, void *desc,
			   fi_addr_t dest_addr,
			   uint64_t addr, uint64_t key,
			   enum fi_datatype datatype,
			   enum fi_op op, void *context,
			   uint64_t flags);
ssize_t _psmx_atomic_readwrite(struct fid_ep *ep,
				const void *buf,
				size_t count, void *desc,
				void *result, void *result_desc,
				fi_addr_t dest_addr,
				uint64_t addr, uint64_t key,
				enum fi_datatype datatype,
				enum fi_op op, void *context,
				uint64_t flags);
ssize_t _psmx_atomic_compwrite(struct fid_ep *ep,
				const void *buf,
				size_t count, void *desc,
				const void *compare, void *compare_desc,
				void *result, void *result_desc,
				fi_addr_t dest_addr,
				uint64_t addr, uint64_t key,
				enum fi_datatype datatype,
				enum fi_op op, void *context,
				uint64_t flags);

#ifdef __cplusplus
}
#endif

#endif

