/*
 * Copyright (c) 2014 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OPAL_BTL_USNIC_EXT_H
#define OPAL_BTL_USNIC_EXT_H

#include "opal_config.h"

#include <infiniband/verbs.h>

#include "opal_stdint.h"

typedef void *(*opal_btl_usnic_dlsym_fn_t)(const char *name);

typedef struct {
    int lookup_version;
    uint64_t magic;
    opal_btl_usnic_dlsym_fn_t lookup;
} opal_btl_usnic_query_port_table_t;

#define USNIC_PORT_QUERY_MAGIC (0x43494e7375534355ULL)

/*
 * Tells libusnic_verbs to enable UDP support.
 */
typedef int (*opal_btl_usnic_enable_udp_fn_t)(struct ibv_context *context);

/*
 * Find out what the UD header length is
 */
typedef int (*opal_btl_usnic_get_ud_header_len_fn_t)(struct ibv_context *context,
                                                     uint8_t port_num);

/*
 * Struct usnic extension function pointers
 */
typedef struct {
    opal_btl_usnic_query_port_table_t qpt;

    opal_btl_usnic_enable_udp_fn_t enable_udp;
    opal_btl_usnic_get_ud_header_len_fn_t get_ud_header_len;
} opal_btl_usnic_ext_fns_t;

/*
 * Global variable of usnic extension function pointers
 */
extern opal_btl_usnic_ext_fns_t opal_btl_usnic_ext;

/*
 * Function to initialze the global variable of usnic extension
 * function pointers
 */
void opal_btl_usnic_ext_init(struct ibv_context *ctx);

#endif /* OPAL_BTL_USNIC_EXT_H */

