/*
 * Copyright (c) 2013-2016 Cisco Systems, Inc.  All rights reserved.
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

static inline int
usnic_netmask_to_cidrlen(
    uint32_t netmask_be)
{
    return 33 - ffs(ntohl(netmask_be));
}

static inline uint32_t
usnic_cidrlen_to_netmask(
    int cidrlen)
{
    uint32_t mask;

    mask = ~0 << (32 - cidrlen);
    return htonl(mask);
}

/*
 * Safely (but abnornmally) exit this process without abort()'ing (and
 * leaving a corefile).
 */
struct opal_btl_usnic_module_t;
void opal_btl_usnic_exit(struct opal_btl_usnic_module_t *module);

/*
 * Print a show_help message and then call opal_btl_usnic_exit().
 */
void opal_btl_usnic_util_abort(const char *msg, const char *file, int line);

/*
 * Long enough to hold "xxx.xxx.xxx.xxx/xx"
 */
#define IPV4STRADDRLEN 20

/*
 * If netmask==0, it is not included in the output string.  addr is
 * expected to be in network byte order.
 */
void opal_btl_usnic_snprintf_ipv4_addr(char *out, size_t maxlen,
                                       uint32_t addr_be, uint32_t netmask_be);

void opal_btl_usnic_snprintf_bool_array(char *s, size_t slen, bool a[], size_t alen);

void opal_btl_usnic_dump_hex(void *vaddr, int len);

size_t opal_btl_usnic_convertor_pack_peek(const opal_convertor_t *conv,
                                          size_t max_len);

#endif /* BTL_USNIC_UTIL_H */
