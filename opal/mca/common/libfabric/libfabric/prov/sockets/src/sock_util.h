/*
 * Copyright (c) 2014 Intel Corporation, Inc.  All rights reserved.
 *
 * This software is available to you under a choice of one of two
 * licenses.  You may choose to be licensed under the terms of the GNU
 * General Public License (GPL) Version 2, available from the file
 * COPYING in the main directory of this source tree, or the
 * OpenIB.org BSD license below:
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

#ifndef _SOCK_UTIL_H_
#define _SOCK_UTIL_H_

#include <stdio.h>

#define SOCK_ERROR (1)
#define SOCK_WARN  (2)
#define SOCK_INFO  (3)

extern int sock_log_level;

#define SOCK_LOG_INFO(...) do {					\
		if (sock_log_level <= SOCK_INFO) {			\
			fprintf(stderr, "[SOCK_INFO - %s]: ", __func__); \
			fprintf(stderr, __VA_ARGS__);			\
		}							\
	} while (0)

#define SOCK_LOG_WARN(...) do {					\
		if (sock_log_level <= SOCK_WARN) {		\
			fprintf(stderr, "[SOCK_WARN - %s]: ", __func__); \
			fprintf(stderr, __VA_ARGS__);			\
		}							\
	} while (0)

#define SOCK_LOG_ERROR(...) do {					\
		if (sock_log_level <= SOCK_ERROR) {			\
			fprintf(stderr, "[SOCK_ERROR - %s]: ", __func__); \
			fprintf(stderr, __VA_ARGS__);			\
		}							\
	} while (0)

#endif
