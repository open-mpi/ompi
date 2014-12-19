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

#ifndef _PROV_H_
#define _PROV_H_

#if HAVE_CONFIG_H
#  include <config.h>
#endif /* HAVE_CONFIG_H */

#include <rdma/fi_prov.h>

/* Provider initialization function signature that built-in providers
 * must specify. */
#define INI_SIG(name) struct fi_provider* name(void)

/* for each provider defines for three scenarios:
 * dl: externally visible ctor with known name (see fi_prov.h)
 * built-in: ctor function def, don't export symbols
 * not built: no-op call for ctor
*/

#if (HAVE_VERBS) && (HAVE_VERBS_DL)
#  define VERBS_INI FI_EXT_INI
#  define VERBS_INIT NULL
#elif (HAVE_VERBS)
#  define VERBS_INI INI_SIG(fi_verbs_ini)
#  define VERBS_INIT fi_verbs_ini()
VERBS_INI ;
#else
#  define VERBS_INIT NULL
#endif

#if (HAVE_PSM) && (HAVE_PSM_DL)
#  define PSM_INI FI_EXT_INI
#  define PSM_INIT NULL
#elif (HAVE_PSM)
#  define PSM_INI INI_SIG(fi_psm_ini)
#  define PSM_INIT fi_psm_ini()
PSM_INI ;
#else
#  define PSM_INIT NULL
#endif

#if (HAVE_SOCKETS) && (HAVE_SOCKETS_DL)
#  define SOCKETS_INI FI_EXT_INI
#  define SOCKETS_INIT NULL
#elif (HAVE_SOCKETS)
#  define SOCKETS_INI INI_SIG(fi_sockets_ini)
#  define SOCKETS_INIT fi_sockets_ini()
SOCKETS_INI ;
#else
#  define SOCKETS_INIT NULL
#endif

#if (HAVE_USNIC) && (HAVE_USNIC_DL)
#  define USNIC_INI FI_EXT_INI
#  define USNIC_INIT NULL
#elif (HAVE_USNIC)
#  define USNIC_INI INI_SIG(fi_usnic_ini)
#  define USNIC_INIT fi_usnic_ini()
USNIC_INI ;
#else
#  define USNIC_INIT NULL
#endif

#endif /* _PROV_H_ */
