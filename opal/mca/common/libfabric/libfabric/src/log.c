/*
 * Copyright (c) 2015, Cisco Systems, Inc. All rights reserved.
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

#include "fi.h"
#include "fi_log.h"

/* General implementation note: these functions currently use multiple fprintfs
 * in a row, which can render in an ugly fashion for multithreaded code and for
 * some mpirun implementations.  If this bugs anyone enough then we can convert
 * them to snprintf to build up the printout in a single buffer.
 */

int fi_log_level = INT_MIN;

void fi_log_init(void)
{
	int ret;

	if (getenv("FI_LOG_LEVEL") != NULL) {
		errno = 0;
		ret = strtol(getenv("FI_LOG_LEVEL"), NULL, 10);
		if (errno != 0)
			fprintf(stderr,
				"%s: invalid value specified for FI_LOG_LEVEL (%s)\n",
				PACKAGE, strerror(errno));
		else
			fi_log_level = (int)ret;
	}
}

void fi_warn_impl(const char *prov, const char *fmt, ...)
{
	va_list vargs;

	if (prov != NULL)
		fprintf(stderr, "%s:%s: ", PACKAGE, prov);
	else
		fprintf(stderr, "%s: ", PACKAGE);
	va_start(vargs, fmt);
	vfprintf(stderr, fmt, vargs);
	va_end(vargs);
}

void fi_log_impl(int level, const char *prov, const char *func, int line, 
		 const char *fmt, ...)
{
	va_list vargs;

	if (prov != NULL)
		fprintf(stderr, "%s:%s:%s():%d<%d> ", PACKAGE, prov, 
			func, line, level);
	else
		fprintf(stderr, "%s:%s():%d<%d> ", PACKAGE, func, line, level);
	va_start(vargs, fmt);
	vfprintf(stderr, fmt, vargs);
	va_end(vargs);
}

void fi_debug_impl(const char *prov, const char *func, int line, const char *fmt, ...)
{
	va_list vargs;

	if (prov != NULL)
		fprintf(stderr, "%s:%s:%s():%d<DBG> ", PACKAGE, prov, func, line);
	else
		fprintf(stderr, "%s:%s():%d<DBG> ", PACKAGE, func, line);
	va_start(vargs, fmt);
	vfprintf(stderr, fmt, vargs);
	va_end(vargs);
}
