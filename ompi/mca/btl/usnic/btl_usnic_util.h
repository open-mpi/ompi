/*
 * Copyright (c) 2013 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef BTL_USNIC_UTIL_H
#define BTL_USNIC_UTIL_H

#include "btl_usnic.h"
#include "btl_usnic_module.h"

/* Linux kernel fls() */
static __always_inline int fls(int x)
{
    int r = 32;

    if (!x)
        return 0;
    if (!(x & 0xffff0000u)) {
        x <<= 16;
        r -= 16;
    }
    if (!(x & 0xff000000u)) {
        x <<= 8;
        r -= 8;
    }
    if (!(x & 0xf0000000u)) {
        x <<= 4;
        r -= 4;
    }
    if (!(x & 0xc0000000u)) {
        x <<= 2;
        r -= 2;
    }
    if (!(x & 0x80000000u)) {
        r -= 1;
    }
    return r;
}

/*
 * Safely (but abnornmally) exit this process without abort()'ing (and
 * leaving a corefile).
 */
void ompi_btl_usnic_exit(void);

void ompi_btl_usnic_sprintf_mac(char *out, const uint8_t mac[6]);

void ompi_btl_usnic_sprintf_gid_mac(char *out, union ibv_gid *gid);

int ompi_btl_usnic_find_ip(ompi_btl_usnic_module_t *module, uint8_t mac[6]);

void ompi_btl_usnic_gid_to_mac(union ibv_gid *gid, uint8_t mac[6]);

void ompi_btl_usnic_dump_hex(uint8_t *addr, int len);

uint32_t ompi_btl_usnic_get_ipv4_subnet(uint32_t addrn, uint32_t cidr_len);

void ompi_btl_usnic_util_abort(const char *msg, const char *file, int line,
                               int ret);

#endif /* BTL_USNIC_UTIL_H */
