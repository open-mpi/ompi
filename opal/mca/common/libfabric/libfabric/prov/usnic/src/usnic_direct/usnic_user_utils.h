/*
 * Copyright (c) 2014, Cisco Systems, Inc. All rights reserved.
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

#ifndef USNIC_USER_UTILS_H
#define USNIC_USER_UTILS_H

#include <stdio.h>
#include <stddef.h>
#include <sys/param.h>

#ifndef __CHAR_BIT__
#define __CHAR_BIT__ 8
#endif
#define CHAR_BIT __CHAR_BIT__

#define USNIC_LOG_LVL_NONE		0
#define USNIC_LOG_LVL_ERR		1
#define USNIC_LOG_LVL_INFO		2
#define USNIC_LOG_LVL_VERBOSE		3

#if WANT_DEBUG_MSGS
#define USNIC_LOG_LVL			USNIC_LOG_LVL_INFO
#else
#define USNIC_LOG_LVL			USNIC_LOG_LVL_NONE
#endif

#define usnic_printf(fd, args...) \
	do { \
		fprintf(fd, "usnic:%-22s:%5d: ", __func__, __LINE__); \
		fprintf(fd, args); \
	} while (0)

#if USNIC_LOG_LVL >= USNIC_LOG_LVL_ERR
#define usnic_err(args...) usnic_printf(stderr, args)
#else
#define usnic_err(args...) {}
#endif

#if USNIC_LOG_LVL >= USNIC_LOG_LVL_ERR
#define usnic_strerror(err, args, ...) \
	do { \
		char err_buf[50]; \
		char *estr = strerror_r(err, err_buf, sizeof(err_buf)); \
		fprintf(stderr, "usnic:%-22s:%5d: ", __func__, __LINE__); \
		fprintf(stderr, args " error: %s\n", ## __VA_ARGS__, \
					estr); \
	} while (0)
#else
#define usnic_strerror(err, args, ...)
#endif

#if USNIC_LOG_LVL >= USNIC_LOG_LVL_ERR
#define usnic_perr(args, ...) \
	do { \
		char err_buf[50]; \
		char *estr = strerror_r(errno, err_buf, sizeof(err_buf)); \
		fprintf(stderr, "usnic:%-22s:%5d: ", __func__, __LINE__); \
		fprintf(stderr, args " error: %s\n", ## __VA_ARGS__, \
					estr); \
	} while (0)
#else
#define usnic_perr(args, ...) {}
#endif

#if USNIC_LOG_LVL >= USNIC_LOG_LVL_INFO
#define usnic_info(args...) usnic_printf(stdout, args)
#else
#define usnic_info(args...) {}
#endif

#if USNIC_LOG_LVL >= USNIC_LOG_LVL_VERBOSE
#define usnic_verbose(args...) usnic_printf(stdout, args)
#else
#define usnic_verbose(args...) {}
#endif

#endif /* USNIC_USER_UTILS_H */
