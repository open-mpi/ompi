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

#ifndef _FI_TRIGGER_H_
#define _FI_TRIGGER_H_

#include <stdint.h>
#include <stddef.h>
#include <rdma/fabric.h>

#ifdef __cplusplus
extern "C" {
#endif


enum fi_trigger_event {
	FI_TRIGGER_THRESHOLD,
};

struct fi_trigger_threshold {
	struct fid_cntr		*cntr;
	size_t			threshold;
};

#ifndef FABRIC_DIRECT

/* Size must match struct fi_context */
struct fi_triggered_context {
	enum fi_trigger_event	event_type;
	union {
		struct fi_trigger_threshold	threshold;
		void				*internal[3];
	};
};

#else // FABRIC_DIRECT
#include <rdma/fi_direct_trigger.h>
#endif


#ifdef __cplusplus
}
#endif

#endif /* _FI_TRIGGER_H_ */
