/*
 * Copyright 2008-2010 Cisco Systems, Inc.  All rights reserved.
 * Copyright 2007 Nuova Systems, Inc.  All rights reserved.
 *
 * LICENSE_BEGIN
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
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *
 * LICENSE_END
 *
 *
 */

#ifndef _VNIC_RQ_H_
#define _VNIC_RQ_H_

#ifndef ENIC_PMD
#include <linux/pci.h>
#endif

#include "vnic_dev.h"
#include "vnic_cq.h"

/* Receive queue control */
struct vnic_rq_ctrl {
	u64 ring_base;			/* 0x00 */
	u32 ring_size;			/* 0x08 */
	u32 pad0;
	u32 posted_index;		/* 0x10 */
	u32 pad1;
	u32 cq_index;			/* 0x18 */
	u32 pad2;
	u32 enable;			/* 0x20 */
	u32 pad3;
	u32 running;			/* 0x28 */
	u32 pad4;
	u32 fetch_index;		/* 0x30 */
	u32 pad5;
	u32 error_interrupt_enable;	/* 0x38 */
	u32 pad6;
	u32 error_interrupt_offset;	/* 0x40 */
	u32 pad7;
	u32 error_status;		/* 0x48 */
	u32 pad8;
	u32 dropped_packet_count;	/* 0x50 */
	u32 pad9;
	u32 dropped_packet_count_rc;	/* 0x58 */
	u32 pad10;
};

/* Break the vnic_rq_buf allocations into blocks of 32/64 entries */
#define VNIC_RQ_BUF_MIN_BLK_ENTRIES 32
#define VNIC_RQ_BUF_DFLT_BLK_ENTRIES 64
#define VNIC_RQ_BUF_BLK_ENTRIES(entries) \
	((unsigned int)((entries < VNIC_RQ_BUF_DFLT_BLK_ENTRIES) ? \
	VNIC_RQ_BUF_MIN_BLK_ENTRIES : VNIC_RQ_BUF_DFLT_BLK_ENTRIES))
#define VNIC_RQ_BUF_BLK_SZ(entries) \
	(VNIC_RQ_BUF_BLK_ENTRIES(entries) * sizeof(struct vnic_rq_buf))
#define VNIC_RQ_BUF_BLKS_NEEDED(entries) \
	DIV_ROUND_UP(entries, VNIC_RQ_BUF_BLK_ENTRIES(entries))
#define VNIC_RQ_BUF_BLKS_MAX VNIC_RQ_BUF_BLKS_NEEDED(4096)

struct vnic_rq_buf {
	struct vnic_rq_buf *next;
	dma_addr_t dma_addr;
	void *os_buf;
	unsigned int os_buf_index;
	unsigned int len;
	unsigned int index;
	void *desc;
	uint64_t wr_id;
};

struct vnic_rq {
	unsigned int index;
	struct vnic_dev *vdev;
	struct vnic_rq_ctrl __iomem *ctrl;              /* memory-mapped */
	struct vnic_dev_ring ring;
	struct vnic_rq_buf *bufs[VNIC_RQ_BUF_BLKS_MAX];
	struct vnic_rq_buf *to_use;
	struct vnic_rq_buf *to_clean;
	void *os_buf_head;
	unsigned int pkts_outstanding;
#if defined(ENIC_NETQ)
	unsigned int state;
#endif
#if defined(__VMKLNX__) && defined(ENIC_UPT)
	int enabled;
	unsigned int rxcons2;
#endif
#if defined(__LIBUSNIC__)
	uint32_t qp_num;
#endif

#ifdef CONFIG_NET_RX_BUSY_POLL
#define ENIC_POLL_STATE_IDLE		0
#define ENIC_POLL_STATE_NAPI		(1 << 0) /* NAPI owns this poll */
#define ENIC_POLL_STATE_POLL		(1 << 1) /* poll owns this poll */
#define ENIC_POLL_STATE_NAPI_YIELD	(1 << 2) /* NAPI yielded this poll */
#define ENIC_POLL_STATE_POLL_YIELD	(1 << 3) /* poll yielded this poll */
#define ENIC_POLL_YIELD			(ENIC_POLL_STATE_NAPI_YIELD |	\
					 ENIC_POLL_STATE_POLL_YIELD)
#define ENIC_POLL_LOCKED		(ENIC_POLL_STATE_NAPI |		\
					 ENIC_POLL_STATE_POLL)
#define ENIC_POLL_USER_PEND		(ENIC_POLL_STATE_POLL |		\
					 ENIC_POLL_STATE_POLL_YIELD)
	unsigned int bpoll_state;
	spinlock_t bpoll_lock;
#endif /*CONFIG_NET_RX_BUSY_POLL*/
#ifdef ENIC_PMD
	unsigned int socket_id;
	struct rte_mempool *mp;
#endif
};

static inline unsigned int vnic_rq_desc_avail(struct vnic_rq *rq)
{
	/* how many does SW own? */
	return rq->ring.desc_avail;
}

static inline unsigned int vnic_rq_desc_used(struct vnic_rq *rq)
{
	/* how many does HW own? */
	return rq->ring.desc_count - rq->ring.desc_avail - 1;
}

static inline void *vnic_rq_next_desc(struct vnic_rq *rq)
{
	return rq->to_use->desc;
}

static inline unsigned int vnic_rq_next_index(struct vnic_rq *rq)
{
	return rq->to_use->index;
}

static inline void vnic_rq_post(struct vnic_rq *rq,
	void *os_buf, unsigned int os_buf_index,
	dma_addr_t dma_addr, unsigned int len,
	uint64_t wrid)
{
	struct vnic_rq_buf *buf = rq->to_use;

	buf->os_buf = os_buf;
	buf->os_buf_index = os_buf_index;
	buf->dma_addr = dma_addr;
	buf->len = len;
	buf->wr_id = wrid;

	buf = buf->next;
	rq->to_use = buf;
	rq->ring.desc_avail--;

	/* Move the posted_index every nth descriptor
	 */
#if defined(__LIBUSNIC__)
#define VNIC_RQ_RETURN_RATE		0x0
#endif

#ifndef VNIC_RQ_RETURN_RATE
#define VNIC_RQ_RETURN_RATE		0xf	/* keep 2^n - 1 */
#endif

	if ((buf->index & VNIC_RQ_RETURN_RATE) == 0) {
		/* Adding write memory barrier prevents compiler and/or CPU
		 * reordering, thus avoiding descriptor posting before
		 * descriptor is initialized. Otherwise, hardware can read
		 * stale descriptor fields.
		 */
		wmb();
		iowrite32(buf->index, &rq->ctrl->posted_index);
	}
}

#ifndef NOT_FOR_OPEN_SOURCE
static inline void vnic_rq_post_commit(struct vnic_rq *rq,
	void *os_buf, unsigned int os_buf_index,
	dma_addr_t dma_addr, unsigned int len)
{
	struct vnic_rq_buf *buf = rq->to_use;

	buf->os_buf = os_buf;
	buf->os_buf_index = os_buf_index;
	buf->dma_addr = dma_addr;
	buf->len = len;

	buf = buf->next;
	rq->to_use = buf;
	rq->ring.desc_avail--;

	/* Move the posted_index every descriptor
	 */

	/* Adding write memory barrier prevents compiler and/or CPU
	 * reordering, thus avoiding descriptor posting before
	 * descriptor is initialized. Otherwise, hardware can read
	 * stale descriptor fields.
	 */
	wmb();
	iowrite32(buf->index, &rq->ctrl->posted_index);
}

#endif /* NOT_FOR_OPEN_SOURCE */
static inline void vnic_rq_return_descs(struct vnic_rq *rq, unsigned int count)
{
	rq->ring.desc_avail += count;
}

enum desc_return_options {
	VNIC_RQ_RETURN_DESC,
	VNIC_RQ_DEFER_RETURN_DESC,
};

#ifdef ENIC_PMD
static inline int vnic_rq_service(struct vnic_rq *rq,
	struct cq_desc *cq_desc, u16 completed_index,
	int desc_return, int (*buf_service)(struct vnic_rq *rq,
	struct cq_desc *cq_desc, struct vnic_rq_buf *buf,
	int skipped, void *opaque), void *opaque)
#else
static inline void vnic_rq_service(struct vnic_rq *rq,
	struct cq_desc *cq_desc, u16 completed_index,
	int desc_return, void (*buf_service)(struct vnic_rq *rq,
	struct cq_desc *cq_desc, struct vnic_rq_buf *buf,
	int skipped, void *opaque), void *opaque)
#endif
{
	struct vnic_rq_buf *buf;
	int skipped;
#ifdef ENIC_PMD
	int eop = 0;
#endif

	buf = rq->to_clean;
	while (1) {

		skipped = (buf->index != completed_index);

#ifdef ENIC_PMD
		if ((*buf_service)(rq, cq_desc, buf, skipped, opaque))
			eop++;
#else
		(*buf_service)(rq, cq_desc, buf, skipped, opaque);
#endif

		if (desc_return == VNIC_RQ_RETURN_DESC)
			rq->ring.desc_avail++;

		rq->to_clean = buf->next;

		if (!skipped)
			break;

		buf = rq->to_clean;
	}
#ifdef ENIC_PMD
	return eop;
#endif
}

static inline int vnic_rq_fill(struct vnic_rq *rq,
	int (*buf_fill)(struct vnic_rq *rq))
{
	int err;

	while (vnic_rq_desc_avail(rq) > 0) {

		err = (*buf_fill)(rq);
		if (err)
			return err;
	}

	return 0;
}

#ifndef NOT_FOR_OPEN_SOURCE
static inline int vnic_rq_fill_count(struct vnic_rq *rq,
	int (*buf_fill)(struct vnic_rq *rq), unsigned int count)
{
	int err;

	while ((vnic_rq_desc_avail(rq) > 0) && (count--)) {

		err = (*buf_fill)(rq);
		if (err)
			return err;
	}

	return 0;
}

#endif /* NOT_FOR_OPEN_SOURCE */
void vnic_rq_free(struct vnic_rq *rq);
int vnic_rq_alloc(struct vnic_dev *vdev, struct vnic_rq *rq, unsigned int index,
	unsigned int desc_count, unsigned int desc_size);
#ifndef FOR_UPSTREAM_KERNEL
void vnic_rq_init_start(struct vnic_rq *rq, unsigned int cq_index,
	unsigned int fetch_index, unsigned int posted_index,
	unsigned int error_interrupt_enable,
	unsigned int error_interrupt_offset);
#endif
void vnic_rq_init(struct vnic_rq *rq, unsigned int cq_index,
	unsigned int error_interrupt_enable,
	unsigned int error_interrupt_offset);
void vnic_rq_error_out(struct vnic_rq *rq, unsigned int error);
unsigned int vnic_rq_error_status(struct vnic_rq *rq);
void vnic_rq_enable(struct vnic_rq *rq);
int vnic_rq_disable(struct vnic_rq *rq);
void vnic_rq_clean(struct vnic_rq *rq,
	void (*buf_clean)(struct vnic_rq *rq, struct vnic_rq_buf *buf));
#ifndef NOT_FOR_OPEN_ENIC
int vnic_rq_mem_size(struct vnic_rq *rq, unsigned int desc_count,
	unsigned int desc_size);
#endif

#ifndef ENIC_PMD
#ifdef CONFIG_NET_RX_BUSY_POLL
static inline void enic_busy_poll_init_lock(struct vnic_rq *rq)
{
	spin_lock_init(&rq->bpoll_lock);
	rq->bpoll_state = ENIC_POLL_STATE_IDLE;
}

static inline bool enic_poll_lock_napi(struct vnic_rq *rq)
{
	bool rc = true;

	spin_lock(&rq->bpoll_lock);
	if (rq->bpoll_state & ENIC_POLL_LOCKED) {
		WARN_ON(rq->bpoll_state & ENIC_POLL_STATE_NAPI);
		rq->bpoll_state |= ENIC_POLL_STATE_NAPI_YIELD;
		rc = false;
	} else {
		rq->bpoll_state = ENIC_POLL_STATE_NAPI;
	}
	spin_unlock(&rq->bpoll_lock);
	return rc;
}

static inline bool enic_poll_unlock_napi(struct vnic_rq *rq)
{
	bool rc = false;

	spin_lock(&rq->bpoll_lock);
	WARN_ON(rq->bpoll_state &
		(ENIC_POLL_STATE_POLL | ENIC_POLL_STATE_NAPI_YIELD));

	if (rq->bpoll_state & ENIC_POLL_STATE_POLL_YIELD)
		rc = true;
	rq->bpoll_state = ENIC_POLL_STATE_IDLE;
	spin_unlock(&rq->bpoll_lock);
	return rc;
}

static inline bool enic_poll_lock_poll(struct vnic_rq *rq)
{
	bool rc = true;

	spin_lock_bh(&rq->bpoll_lock);
	if (rq->bpoll_state & ENIC_POLL_LOCKED) {
		rq->bpoll_state |= ENIC_POLL_STATE_POLL_YIELD;
		rc = false;
	} else {
		rq->bpoll_state |= ENIC_POLL_STATE_POLL;
	}
	spin_unlock_bh(&rq->bpoll_lock);
	return rc;
}

static inline bool enic_poll_unlock_poll(struct vnic_rq *rq)
{
	bool rc = false;

	spin_lock_bh(&rq->bpoll_lock);
	WARN_ON(rq->bpoll_state & ENIC_POLL_STATE_NAPI);

	if (rq->bpoll_state & ENIC_POLL_STATE_POLL_YIELD)
		rc = true;
	rq->bpoll_state = ENIC_POLL_STATE_IDLE;
	spin_unlock_bh(&rq->bpoll_lock);
	return rc;
}

static inline bool enic_poll_busy_polling(struct vnic_rq *rq)
{
	WARN_ON(!(rq->bpoll_state & ENIC_POLL_LOCKED));
	return rq->bpoll_state & ENIC_POLL_USER_PEND;
}
#else
static inline void enic_busy_poll_init_lock(struct vnic_rq *UNUSED(rq))
{
}

static inline bool enic_poll_lock_napi(struct vnic_rq *UNUSED(rq))
{
	return true;
}

static inline bool enic_poll_unlock_napi(struct vnic_rq *UNUSED(rq))
{
	return false;
}

static inline bool enic_poll_lock_poll(struct vnic_rq *UNUSED(rq))
{
	return false;
}

static inline bool enic_poll_unlock_poll(struct vnic_rq *UNUSED(rq))
{
	return false;
}

static inline bool enic_poll_busy_polling(struct vnic_rq *UNUSED(rq))
{
	return false;
}
#endif /*CONFIG_NET_RX_BUSY_POLL*/
#endif /* ENIC_PMD */
#endif /* _VNIC_RQ_H_ */
