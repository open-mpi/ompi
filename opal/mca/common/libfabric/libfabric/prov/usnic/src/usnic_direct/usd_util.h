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

#ifndef _USD_UTIL_H_
#define _USD_UTIL_H_

#include <stdio.h>
#include <stdint.h>

static uint8_t bittab[] = {
  0,  1,  2,  2,  3,  3,  3,  3,  4,  4,  4,  4,  4,  4,  4,  4,
  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,
  6,  6,  6,  6,  6,  6,  6,  6,  6,  6,  6,  6,  6,  6,  6,  6,
  6,  6,  6,  6,  6,  6,  6,  6,  6,  6,  6,  6,  6,  6,  6,  6,
  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,
  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,
  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,
  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,
  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,
  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,
  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,
  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,
  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,
  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,
  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,
  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,
};

static inline int
msbit(
    uint32_t val)
{
    int bit;

    bit = 0;

    if (val & (0xffff << 16)) {
        val >>= 16;
        bit += 16;
    }
    if (val & (0xff << 8)) {
        val >>= 8;
        bit += 8;
    }
    return bittab[val] + bit;
}

#define usd_offset_of(type, memb) \
        ((unsigned long)(&((type *)0)->memb))
#define usd_container_of(obj, type, memb) \
        ((type *)(((char *)obj) - usd_offset_of(type, memb)))

static inline void hex(void *vcp, int len)
{
    uint8_t *cp = vcp;
    int i;
    for (i = 0; i < len; ++i) {
        printf("%02x%c", *cp++, ((i & 15) == 15) ? 10 : 32);
    }
    if (i & 15)
        puts("");
}

/*
 * 48-bit type.  Byte aligned.
 */
typedef struct {
    unsigned char   net_data[6];
} net48_t;

/**
 * net48_get(net) - fetch from a network-order 48-bit field.
 *
 * @param net pointer to type net48_t, network-order 48-bit data.
 * @return the host-order value.
 */
static inline u_int64_t net48_get(const net48_t * net)
{
    return ((u_int64_t) net->net_data[0] << 40) |
        ((u_int64_t) net->net_data[1] << 32) |
        ((u_int64_t) net->net_data[2] << 24) |
        ((u_int64_t) net->net_data[3] << 16) |
        ((u_int64_t) net->net_data[4] << 8) |
        (u_int64_t) net->net_data[5];
}

/**
 * net48_put(net, val) - store to a network-order 48-bit field.
 *
 * @param net pointer to a net48_t, network-order 48-bit data.
 * @param val host-order value to be stored at net.
 */
static inline void net48_put(net48_t * net, u_int64_t val)
{
    net->net_data[0] = (u_int8_t)((val >> 40) & 0xFF);
    net->net_data[1] = (u_int8_t)((val >> 32) & 0xFF);
    net->net_data[2] = (u_int8_t)((val >> 24) & 0xFF);
    net->net_data[3] = (u_int8_t)((val >> 16) & 0xFF);
    net->net_data[4] = (u_int8_t)((val >> 8) & 0xFF);
    net->net_data[5] = (u_int8_t)(val & 0xFF);
}

static inline void usd_perror(const char *s)
{
    if (USD_DEBUG) {
        perror(s);
    }
}
#endif /* _USD_UTIL_H_ */
