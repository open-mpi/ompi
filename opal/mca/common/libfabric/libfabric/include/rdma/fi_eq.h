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

#ifndef _FI_EQ_H_
#define _FI_EQ_H_

#include <rdma/fabric.h>


#ifdef __cplusplus
extern "C" {
#endif



/*
 * Wait Set
 * Allows associating multiple EQs and counters with a single wait object.
 */

/* Use fi_control GETWAIT to get underlying wait object(s) */
enum fi_wait_obj {
	FI_WAIT_NONE,
	FI_WAIT_UNSPEC,
	FI_WAIT_SET,
	FI_WAIT_FD,
	FI_WAIT_MUTEX_COND,	/* pthread mutex & cond */
};

struct fi_wait_attr {
	enum fi_wait_obj	wait_obj;
	uint64_t		flags;
};

struct fi_ops_wait {
	size_t	size;
	int	(*wait)(struct fid_wait *waitset, int timeout);
};

struct fid_wait {
	struct fid		fid;
	struct fi_ops_wait	*ops;
};
	
struct fi_mutex_cond {
	pthread_mutex_t		*mutex;
	pthread_cond_t		*cond;
};

/*
 * Poll Set
 * Allows polling multiple event queues and counters for progress
 */

struct fi_poll_attr {
	uint64_t		flags;
};

struct fi_ops_poll {
	size_t	size;
	int	(*poll)(struct fid_poll *pollset, void **context, int count);
	int	(*poll_add)(struct fid_poll *pollset, struct fid *event_fid, 
			uint64_t flags);
	int	(*poll_del)(struct fid_poll *pollset, struct fid *event_fid, 
			uint64_t flags);
};

struct fid_poll {
	struct fid		fid;
	struct fi_ops_poll	*ops;
};


/*
 * EQ = Event Queue
 * Used to report various control (not data transfer) events and operations.
 */

struct fi_eq_attr {
	size_t			size;
	uint64_t		flags;
	enum fi_wait_obj	wait_obj;
	int			signaling_vector;
	struct fid_wait		*wait_set;
};

/* Standard EQ events */
enum {
	FI_NOTIFY,
	FI_CONNREQ,
	FI_CONNECTED,
	FI_SHUTDOWN,
	FI_MR_COMPLETE,
	FI_AV_COMPLETE,
};

struct fi_eq_entry {
	fid_t			fid;
	void			*context;
	uint64_t		data;
};

struct fi_eq_err_entry {
	fid_t			fid;
	void			*context;
	uint64_t		data;
	int			err;
	int			prov_errno;
	/* err_data is available until the next time the CQ is read */
	void			*err_data;
	size_t			err_data_size;
};

struct fi_eq_cm_entry {
	fid_t			fid;
	/* user must call fi_freeinfo to release info */
	struct fi_info		*info;
	/* connection data placed here, up to space provided */
	uint8_t			data[];
};

struct fi_ops_eq {
	size_t	size;
	ssize_t	(*read)(struct fid_eq *eq, uint32_t *event,
			void *buf, size_t len, uint64_t flags);
	ssize_t	(*readerr)(struct fid_eq *eq, struct fi_eq_err_entry *buf,
			uint64_t flags);
	ssize_t	(*write)(struct fid_eq *eq, uint32_t event,
			const void *buf, size_t len, uint64_t flags);
	ssize_t	(*sread)(struct fid_eq *eq, uint32_t *event,
			void *buf, size_t len, int timeout, uint64_t flags);
	const char * (*strerror)(struct fid_eq *eq, int prov_errno,
			const void *err_data, char *buf, size_t len);
};

struct fid_eq {
	struct fid		fid;
	struct fi_ops_eq	*ops;
};


/*
 * CQ = Complete Queue
 * Used to report the completion of data transfer operations.
 */

enum fi_cq_format {
	FI_CQ_FORMAT_UNSPEC,
	FI_CQ_FORMAT_CONTEXT,
	FI_CQ_FORMAT_MSG,
	FI_CQ_FORMAT_DATA,
	FI_CQ_FORMAT_TAGGED,
};

struct fi_cq_entry {
	void			*op_context;
};

struct fi_cq_msg_entry {
	void			*op_context;
	uint64_t		flags;
	size_t			len;
};

struct fi_cq_data_entry {
	void			*op_context;
	uint64_t		flags;
	size_t			len;
	void			*buf;
	/* data depends on operation and/or flags - e.g. remote EQ data */
	uint64_t		data;
};

struct fi_cq_tagged_entry {
	void			*op_context;
	uint64_t		flags;
	size_t			len;
	void			*buf;
	uint64_t		data;
	uint64_t		tag;
};

struct fi_cq_err_entry {
	void			*op_context;
	uint64_t		flags;
	size_t			len;
	void			*buf;
	uint64_t		data;
	uint64_t		tag;
	size_t			olen;
	int			err;
	int			prov_errno;
	/* err_data is available until the next time the CQ is read */
	void			*err_data;
};

enum fi_cq_wait_cond {
	FI_CQ_COND_NONE,
	FI_CQ_COND_THRESHOLD	/* size_t threshold */
};

struct fi_cq_attr {
	size_t			size;
	uint64_t		flags;
	enum fi_cq_format	format;
	enum fi_wait_obj	wait_obj;
	int			signaling_vector;
	enum fi_cq_wait_cond	wait_cond;
	struct fid_wait		*wait_set;
};

struct fi_ops_cq {
	size_t	size;
	ssize_t	(*read)(struct fid_cq *cq, void *buf, size_t count);
	ssize_t	(*readfrom)(struct fid_cq *cq, void *buf, size_t count,
			fi_addr_t *src_addr);
	ssize_t	(*readerr)(struct fid_cq *cq, struct fi_cq_err_entry *buf,
			uint64_t flags);
	ssize_t	(*write)(struct fid_cq *cq, const void *buf, size_t len);
	ssize_t	(*writeerr)(struct fid_cq *cq, struct fi_cq_err_entry *buf,
			size_t len, uint64_t flags);
	ssize_t	(*sread)(struct fid_cq *cq, void *buf, size_t count,
			const void *cond, int timeout);
	ssize_t	(*sreadfrom)(struct fid_cq *cq, void *buf, size_t count,
			fi_addr_t *src_addr, const void *cond, int timeout);
	const char * (*strerror)(struct fid_cq *cq, int prov_errno,
			const void *err_data, char *buf, size_t len);
};

struct fid_cq {
	struct fid		fid;
	struct fi_ops_cq	*ops;
};


/*
 * CNTR = Counter
 * Used to report the number of completed of asynchronous operations.
 */

enum fi_cntr_events {
	FI_CNTR_EVENTS_COMP
};

struct fi_cntr_attr {
	enum fi_cntr_events	events;
	enum fi_wait_obj	wait_obj;
	struct fid_wait		*wait_set;
	uint64_t		flags;
};

struct fi_ops_cntr {
	size_t	size;
	uint64_t (*read)(struct fid_cntr *cntr);
	uint64_t (*readerr)(struct fid_cntr *cntr);
	int	(*add)(struct fid_cntr *cntr, uint64_t value);
	int	(*set)(struct fid_cntr *cntr, uint64_t value);
	int	(*wait)(struct fid_cntr *cntr, uint64_t threshold, int timeout);
};

struct fid_cntr {
	struct fid		fid;
	struct fi_ops_cntr	*ops;
};


#ifndef FABRIC_DIRECT

static inline int
fi_wait(struct fid_wait *waitset, int timeout)
{
	return waitset->ops->wait(waitset, timeout);
}

static inline int
fi_poll(struct fid_poll *pollset, void **context, int count)
{
	return pollset->ops->poll(pollset, context, count);
}

static inline int
fi_poll_add(struct fid_poll *pollset, struct fid *event_fid, uint64_t flags)
{
	return pollset->ops->poll_add(pollset, event_fid, flags);
}

static inline int
fi_poll_del(struct fid_poll *pollset, struct fid *event_fid, uint64_t flags)
{
	return pollset->ops->poll_del(pollset, event_fid, flags);
}

static inline int
fi_eq_open(struct fid_fabric *fabric, struct fi_eq_attr *attr,
	   struct fid_eq **eq, void *context)
{
	return fabric->ops->eq_open(fabric, attr, eq, context);
}

static inline ssize_t
fi_eq_read(struct fid_eq *eq, uint32_t *event, void *buf,
	   size_t len, uint64_t flags)
{
	return eq->ops->read(eq, event, buf, len, flags);
}

static inline ssize_t
fi_eq_readerr(struct fid_eq *eq, struct fi_eq_err_entry *buf, uint64_t flags)
{
	return eq->ops->readerr(eq, buf, flags);
}

static inline ssize_t
fi_eq_write(struct fid_eq *eq, uint32_t event, const void *buf,
	    size_t len, uint64_t flags)
{
	return eq->ops->write(eq, event, buf, len, flags);
}

static inline ssize_t
fi_eq_sread(struct fid_eq *eq, uint32_t *event, void *buf, size_t len,
	    int timeout, uint64_t flags)
{
	return eq->ops->sread(eq, event, buf, len, timeout, flags);
}

static inline const char *
fi_eq_strerror(struct fid_eq *eq, int prov_errno, const void *err_data,
	       char *buf, size_t len)
{
	return eq->ops->strerror(eq, prov_errno, err_data, buf, len);
}


static inline ssize_t fi_cq_read(struct fid_cq *cq, void *buf, size_t count)
{
	return cq->ops->read(cq, buf, count);
}

static inline ssize_t
fi_cq_readfrom(struct fid_cq *cq, void *buf, size_t count, fi_addr_t *src_addr)
{
	return cq->ops->readfrom(cq, buf, count, src_addr);
}

static inline ssize_t
fi_cq_readerr(struct fid_cq *cq, struct fi_cq_err_entry *buf, uint64_t flags)
{
	return cq->ops->readerr(cq, buf, flags);
}

static inline ssize_t fi_cq_write(struct fid_cq *cq, const void *buf, size_t len)
{
	return cq->ops->write(cq, buf, len);
}

static inline ssize_t fi_cq_writeerr(struct fid_cq *cq, struct fi_cq_err_entry *buf,
				     size_t len, uint64_t flags)
{
	return cq->ops->writeerr(cq, buf, len, flags);
}

static inline ssize_t
fi_cq_sread(struct fid_cq *cq, void *buf, size_t count, const void *cond, int timeout)
{
	return cq->ops->sread(cq, buf, count, cond, timeout);
}

static inline ssize_t
fi_cq_sreadfrom(struct fid_cq *cq, void *buf, size_t count,
		fi_addr_t *src_addr, const void *cond, int timeout)
{
	return cq->ops->sreadfrom(cq, buf, count, src_addr, cond, timeout);
}

static inline const char *
fi_cq_strerror(struct fid_cq *cq, int prov_errno, const void *err_data,
	       char *buf, size_t len)
{
	return cq->ops->strerror(cq, prov_errno, err_data, buf, len);
}


static inline uint64_t fi_cntr_read(struct fid_cntr *cntr)
{
	return cntr->ops->read(cntr);
}

static inline uint64_t fi_cntr_readerr(struct fid_cntr *cntr)
{
	return cntr->ops->readerr(cntr);
}

static inline int fi_cntr_add(struct fid_cntr *cntr, uint64_t value)
{
	return cntr->ops->add(cntr, value);
}

static inline int fi_cntr_set(struct fid_cntr *cntr, uint64_t value)
{
	return cntr->ops->set(cntr, value);
}

static inline int
fi_cntr_wait(struct fid_cntr *cntr, uint64_t threshold, int timeout)
{
	return cntr->ops->wait(cntr, threshold, timeout);
}


#else // FABRIC_DIRECT
#include <rdma/fi_direct_eq.h>
#endif

#ifdef __cplusplus
}
#endif

#endif /* _FI_EQ_H_ */
