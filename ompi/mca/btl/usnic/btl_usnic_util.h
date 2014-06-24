/*
 * Copyright (c) 2013-2014 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef BTL_USNIC_UTIL_H
#define BTL_USNIC_UTIL_H

#include "opal/datatype/opal_convertor.h"

#include "btl_usnic.h"
#include "btl_usnic_module.h"

#ifndef MIN
#  define MIN(a,b)                ((a) < (b) ? (a) : (b))
#endif

/* avoid "defined but not used" warnings */
static inline int __opal_attribute_always_inline__ usnic_fls(int x)
    __opal_attribute_unused__;

static inline int __opal_attribute_always_inline__ usnic_fls(int x)
{
    int r = 32;

    if (!x) {
        return 0;
    }
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

/* a helper function that just declutters convertor packing */
static inline void
usnic_convertor_pack_simple(
    opal_convertor_t *convertor,
    void *dest,
    size_t max_bytes_to_pack,
    size_t *bytes_packed)
{
    int rc;
    struct iovec iov;
    uint32_t iov_count;

    iov.iov_base = (IOVBASE_TYPE*)dest;
    iov.iov_len = max_bytes_to_pack;
    iov_count = 1;
    *bytes_packed = max_bytes_to_pack;
    rc = opal_convertor_pack(convertor, &iov, &iov_count, bytes_packed);
    if (OPAL_UNLIKELY(rc < 0)) {
        BTL_ERROR(("opal_convertor_pack error"));
        abort();    /* XXX */
    }
}

/*
 * Safely (but abnornmally) exit this process without abort()'ing (and
 * leaving a corefile).
 */
void ompi_btl_usnic_exit(void);

/*
 * Long enough to hold "xxx.xxx.xxx.xxx/xx"
 */
#define IPV4STRADDRLEN 20

/*
 * Long enough to hold "xx:xx:xx:xx:xx:xx"
 */
#define MACSTRLEN 18

/*
 * If cidrmask==0, it is not included in the output string.  addr is
 * expected to be in network byte order.
 */
void ompi_btl_usnic_snprintf_ipv4_addr(char *out, size_t maxlen,
                                       uint32_t addr, uint32_t cidrmask);

void ompi_btl_usnic_sprintf_mac(char *out, const uint8_t mac[6]);

void ompi_btl_usnic_sprintf_gid_mac(char *out, union ibv_gid *gid);

void ompi_btl_usnic_snprintf_bool_array(char *s, size_t slen, bool a[], size_t alen);

int ompi_btl_usnic_find_ip(ompi_btl_usnic_module_t *module, uint8_t mac[6]);

void ompi_btl_usnic_gid_to_mac(union ibv_gid *gid, uint8_t mac[6]);

void ompi_btl_usnic_dump_hex(uint8_t *addr, int len);

uint32_t ompi_btl_usnic_get_ipv4_subnet(uint32_t addrn, uint32_t cidr_len);

void ompi_btl_usnic_util_abort(const char *msg, const char *file, int line,
                               int ret);

size_t ompi_btl_usnic_convertor_pack_peek(const opal_convertor_t *conv,
                                          size_t max_len);

#endif /* BTL_USNIC_UTIL_H */
