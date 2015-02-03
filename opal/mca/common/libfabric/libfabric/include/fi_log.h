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

#if !defined(FI_LOG_H)
#define FI_LOG_H

#if HAVE_CONFIG_H
#  include <config.h>
#endif /* HAVE_CONFIG_H */

extern int fi_log_level;

void fi_log_init(void);
void fi_warn_impl(const char *prov, const char *fmt, ...);
void fi_log_impl(int level, const char *prov, const char *fmt, ...);
void fi_debug_impl(const char *prov, const char *fmt, ...);

/* Callers are responsible for including their own trailing "\n".  Non-provider
 * code should pass prov=NULL.
 */
#define FI_WARN(prov, ...) fi_warn_impl(prov, __VA_ARGS__)

#define FI_LOG(level, prov, ...) \
	do { \
		if ((level) <= fi_log_level) \
			fi_log_impl(level, prov, __VA_ARGS__); \
	} while (0)

#if ENABLE_DEBUG
#  define FI_DEBUG(prov, ...) fi_debug_impl(prov, __VA_ARGS__)
#else
#  define FI_DEBUG(prov, ...) do {} while (0)
#endif

#endif /* !defined(FI_LOG_H) */
