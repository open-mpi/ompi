/*
 * Copyright (c) 2014-2015 Cisco Systems, Inc. All rights reserved.
 * Copyright (c) 2016      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2017      Amazon.com, Inc. or its affiliates.
 *                         All Rights reserved.
 *
 * Portions of this software copied from libfabric
 * (https://github.com/ofiwg/libfabric)
 *
 * LICENSE_BEGIN
 *
 * BSD license:
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

#ifndef LIBNL_UTILS_H
#define LIBNL_UTILS_H

#include "libnl3_utils.h"

struct opal_reachable_netlink_sk {
    NL_HANDLE	*nlh;
    uint32_t	seq;
};

/* returns 0 if host is reachable, EHOSTUNREACH if the host
 * is not reachable, non-zero in other errors.
 *
 * If the route to the destination is through a gateway, *has_gateway
 * is set to 1.  Otherwise, it is set to 0.
 */
int opal_reachable_netlink_rt_lookup(uint32_t src_addr,
				     uint32_t dst_addr, int oif,
				     int *has_gateway);

#if OPAL_ENABLE_IPV6
/* returns 0 if host is reachable, EHOSTUNREACH if the host
 * is not reachable, non-zero in other errors.
 *
 * If the route to the destination is through a gateway, *has_gateway
 * is set to 1.  Otherwise, it is set to 0.
 */
int opal_reachable_netlink_rt_lookup6(struct in6_addr *src_addr,
				      struct in6_addr *dst_addr, int oif,
				      int *has_gateway);
#endif

#endif /* LIBNL_UTILS_H */
