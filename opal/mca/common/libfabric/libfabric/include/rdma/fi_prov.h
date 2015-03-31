/*
 * Copyright (c) 2004, 2005 Topspin Communications.  All rights reserved.
 * Copyright (c) 2005, 2006 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2005 PathScale, Inc.  All rights reserved.
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

#ifndef _FI_PROV_H_
#define _FI_PROV_H_

#include <rdma/fabric.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Extension that dl-loaded providers should add to their .so filename
 * (probably via libtool "-release" option). For example a provider
 * driver named "foo" should build a plug-in named "libfoo-fi.so", and
 * place it in $prefix/$libdir/libfabric/
 */
#define FI_LIB_EXTENSION "fi"
#define FI_LIB_SUFFIX FI_LIB_EXTENSION ".so"

/*
 * Dynamically loaded providers must export the following entry point.
 * This is invoked by the libfabric framework when the provider library
 * is loaded.
 */
#define FI_EXT_INI \
	__attribute__((visibility ("default"))) struct fi_provider* fi_prov_ini(void)

struct fi_provider {
	uint32_t version;
	uint32_t fi_version;
	struct fi_context context;
	const char *name;
	int	(*getinfo)(uint32_t version, const char *node, const char *service,
			uint64_t flags, struct fi_info *hints, struct fi_info **info);
	int	(*fabric)(struct fi_fabric_attr *attr, struct fid_fabric **fabric,
			void *context);
	void	(*cleanup)(void);
};

#ifdef __cplusplus
}
#endif

#endif /* _FI_PROV_H_ */
