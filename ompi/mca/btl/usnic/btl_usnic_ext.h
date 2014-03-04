/*
 * Copyright (c) 2014 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OMPI_BTL_USNIC_EXT_H
#define OMPI_BTL_USNIC_EXT_H

#include "ompi_config.h"

#include <infiniband/verbs.h>

#include "opal_stdint.h"

typedef void *(*ompi_btl_usnic_dlsym_fn_t)(const char *name);

typedef struct {
    int lookup_version;
    uint64_t magic;
    ompi_btl_usnic_dlsym_fn_t lookup;
} ompi_btl_usnic_query_port_table_t;

#define USNIC_PORT_QUERY_MAGIC (0x43494e7375534355ULL)

/*
 * Tells libusnic_verbs to enable UDP support.
 */
typedef int (*ompi_btl_usnic_enable_udp_fn_t)(struct ibv_context *context);

/*
 * Find out what the UD header length is
 */
typedef int (*ompi_btl_usnic_get_ud_header_len_fn_t)(struct ibv_context *context,
                                                     uint8_t port_num);

/*
 * Struct usnic extension function pointers
 */
typedef struct {
    ompi_btl_usnic_query_port_table_t qpt;

    ompi_btl_usnic_enable_udp_fn_t enable_udp;
    ompi_btl_usnic_get_ud_header_len_fn_t get_ud_header_len;
} ompi_btl_usnic_ext_fns_t;

/*
 * Global variable of usnic extension function pointers
 */
extern ompi_btl_usnic_ext_fns_t ompi_btl_usnic_ext;

/*
 * Function to initialze the global variable of usnic extension
 * function pointers
 */
void ompi_btl_usnic_ext_init(struct ibv_context *ctx);

#endif /* OMPI_BTL_USNIC_EXT_H */

