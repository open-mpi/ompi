/*
 * Copyright (c) 2015, Cisco Systems, Inc. All rights reserved.
 * Copyright (c) 2015, Intel Corp., Inc.  All rights reserved.
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
 *
 */

#include <errno.h>
#include <limits.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>

#include <rdma/fi_errno.h>
#include "fi.h"
#include <rdma/fi_log.h>


static const char * const log_subsys[] = {
	[FI_LOG_CORE] = "core",
	[FI_LOG_FABRIC] = "fabric",
	[FI_LOG_DOMAIN] = "domain",
	[FI_LOG_EP_CTRL] = "ep_ctrl",
	[FI_LOG_EP_DATA] = "ep_data",
	[FI_LOG_AV] = "av",
	[FI_LOG_CQ] = "cq",
	[FI_LOG_EQ] = "eq",
	[FI_LOG_MR] = "mr",
	[FI_LOG_SUBSYS_MAX] = NULL
};

static const char * const log_levels[] = {
	[FI_LOG_WARN] = "warn",
	[FI_LOG_TRACE] = "trace",
	[FI_LOG_INFO] = "info",
	[FI_LOG_DEBUG] = "debug",
	[FI_LOG_MAX] = NULL
};

enum {
	FI_LOG_SUBSYS_OFFSET	= FI_LOG_MAX,
	FI_LOG_PROV_OFFSET	= FI_LOG_SUBSYS_OFFSET + FI_LOG_SUBSYS_MAX,
	FI_LOG_LEVEL_MASK	= ((1 << FI_LOG_MAX) - 1),
	FI_LOG_SUBSYS_MASK	= (((1 << FI_LOG_SUBSYS_MAX) - 1) <<
				   FI_LOG_SUBSYS_OFFSET),
//	FI_LOG_PROV_MASK	= (((1 << (64 - FI_LOG_PROV_OFFSET)) - 1) <<
//				   FI_LOG_PROV_OFFSET)
};

#define FI_LOG_TAG(prov, level, subsys) \
	(((uint64_t) prov << FI_LOG_PROV_OFFSET) | \
	 ((uint64_t) (1 << (subsys + FI_LOG_SUBSYS_OFFSET))) | \
	 ((uint64_t) (1 << level)))

uint64_t log_mask;
struct fi_filter prov_log_filter;


static int fi_read_value(const char *env_name, const char * const names[])
{
	const char *value;
	int i;

	value = getenv(env_name);
	if (!value)
		return -1;

	for (i = 0; names[i]; i++) {
		if (!strcasecmp(value, names[i]))
			return i;
	}
	return 0;
}

void fi_log_init(void)
{
	struct fi_filter subsys_filter;
	int level, i;

	level = fi_read_value("FI_LOG_LEVEL", log_levels);
	if (level >= 0)
		log_mask = ((1 << (level + 1)) - 1);

	fi_create_filter(&prov_log_filter, "FI_LOG_PROV");
	/* providers are selectively disabled */

	fi_create_filter(&subsys_filter, "FI_LOG_SUBSYS");
	for (i = 0; i < FI_LOG_SUBSYS_MAX; i++) {
		if (!fi_apply_filter(&subsys_filter, log_subsys[i]))
			log_mask |= (1 << (i + FI_LOG_SUBSYS_OFFSET));
	}
	fi_free_filter(&subsys_filter);
}

void fi_log_fini(void)
{
	fi_free_filter(&prov_log_filter);
}

int fi_log_enabled(const struct fi_provider *prov, enum fi_log_level level,
		   enum fi_log_subsys subsys)
{
	struct fi_prov_context *ctx;

	ctx = (struct fi_prov_context *) &prov->context;
	return ((FI_LOG_TAG(ctx->disable_logging, level, subsys) & log_mask) ==
		FI_LOG_TAG(ctx->disable_logging, level, subsys));
}

void fi_log(const struct fi_provider *prov, enum fi_log_level level,
	    enum fi_log_subsys subsys, const char *func, int line,
	    const char *fmt, ...)
{
	char buf[1024];
	int size;

	va_list vargs;

	size = snprintf(buf, sizeof(buf), "%s:%s:%s:%s():%d<%s> ", PACKAGE,
			prov->name, log_subsys[subsys], func, line,
			log_levels[level]);

	va_start(vargs, fmt);
	vsnprintf(buf + size, sizeof(buf) - size, fmt, vargs);
	va_end(vargs);

	fprintf(stderr, "%s", buf);
}
