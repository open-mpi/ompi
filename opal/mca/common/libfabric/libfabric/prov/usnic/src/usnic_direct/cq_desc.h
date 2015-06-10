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

#ifndef _CQ_DESC_H_
#define _CQ_DESC_H_

/*
 * Completion queue descriptor types
 */
enum cq_desc_types {
	CQ_DESC_TYPE_WQ_ENET = 0,
	CQ_DESC_TYPE_DESC_COPY = 1,
	CQ_DESC_TYPE_WQ_EXCH = 2,
	CQ_DESC_TYPE_RQ_ENET = 3,
	CQ_DESC_TYPE_RQ_FCP = 4,
#ifndef NOT_FOR_OPEN_SOURCE
	CQ_DESC_TYPE_IOMMU_MISS = 5,
	CQ_DESC_TYPE_SGL = 6,
	CQ_DESC_TYPE_CLASSIFIER = 7,
	CQ_DESC_TYPE_TEST = 127,
#endif
};

/* Completion queue descriptor: 16B
 *
 * All completion queues have this basic layout.  The
 * type_specfic area is unique for each completion
 * queue type.
 */
struct cq_desc {
	__le16 completed_index;
	__le16 q_number;
	u8 type_specfic[11];
	u8 type_color;
};

#define CQ_DESC_TYPE_BITS        4
#define CQ_DESC_TYPE_MASK        ((1 << CQ_DESC_TYPE_BITS) - 1)
#define CQ_DESC_COLOR_MASK       1
#define CQ_DESC_COLOR_SHIFT      7
#define CQ_DESC_Q_NUM_BITS       10
#define CQ_DESC_Q_NUM_MASK       ((1 << CQ_DESC_Q_NUM_BITS) - 1)
#define CQ_DESC_COMP_NDX_BITS    12
#define CQ_DESC_COMP_NDX_MASK    ((1 << CQ_DESC_COMP_NDX_BITS) - 1)

#ifndef NOT_FOR_OPEN_SOURCE
static inline void cq_color_enc(struct cq_desc *desc, const u8 color)
{
	if (color)
		desc->type_color |=  (1 << CQ_DESC_COLOR_SHIFT);
	else
		desc->type_color &= ~(1 << CQ_DESC_COLOR_SHIFT);
}

static inline void cq_desc_enc(struct cq_desc *desc,
	const u8 type, const u8 color, const u16 q_number,
	const u16 completed_index)
{
	desc->type_color = (type & CQ_DESC_TYPE_MASK) |
		((color & CQ_DESC_COLOR_MASK) << CQ_DESC_COLOR_SHIFT);
	desc->q_number = cpu_to_le16(q_number & CQ_DESC_Q_NUM_MASK);
	desc->completed_index = cpu_to_le16(completed_index &
		CQ_DESC_COMP_NDX_MASK);
}

#endif
static inline void cq_desc_dec(const struct cq_desc *desc_arg,
	u8 *type, u8 *color, u16 *q_number, u16 *completed_index)
{
	const struct cq_desc *desc = desc_arg;
	const u8 type_color = desc->type_color;

	*color = (type_color >> CQ_DESC_COLOR_SHIFT) & CQ_DESC_COLOR_MASK;

#if !defined(__LIBUSNIC__)
	/*
	 * Make sure color bit is read from desc *before* other fields
	 * are read from desc.  Hardware guarantees color bit is last
	 * bit (byte) written.  Adding the rmb() prevents the compiler
	 * and/or CPU from reordering the reads which would potentially
	 * result in reading stale values.
	 */

	rmb();
#endif

	*type = type_color & CQ_DESC_TYPE_MASK;
	*q_number = le16_to_cpu(desc->q_number) & CQ_DESC_Q_NUM_MASK;
	*completed_index = le16_to_cpu(desc->completed_index) &
		CQ_DESC_COMP_NDX_MASK;
}

#ifndef NOT_FOR_OPEN_SOURCE
static inline void cq_color_dec(const struct cq_desc *desc_arg, u8 *color)
{
	volatile const struct cq_desc *desc = desc_arg;

	*color = (desc->type_color >> CQ_DESC_COLOR_SHIFT) & CQ_DESC_COLOR_MASK;
}

#endif
#endif /* _CQ_DESC_H_ */
