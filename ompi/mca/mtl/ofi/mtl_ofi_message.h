/*
 * Copyright (c) 2013-2014 Intel, Inc. All rights reserved
 * Copyright (c) 2014      Cisco Systems, Inc. All rights reserved
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MTL_OFI_MESSAGE_H
#define MTL_OFI_MESSAGE_H

#include "mtl_ofi_types.h"

struct ompi_mtl_ofi_message_t {
    opal_free_list_item_t super;
    struct fi_cq_tagged_entry wc;
    void *buffer;
};
typedef struct ompi_mtl_ofi_message_t ompi_mtl_ofi_message_t;
OBJ_CLASS_DECLARATION(ompi_mtl_ofi_message_t);


static inline ompi_mtl_ofi_message_t*
ompi_mtl_ofi_message_alloc(const struct fi_cq_tagged_entry *wc)
{
    int rc __opal_attribute_unused__;
    opal_free_list_item_t *tmp;
    ompi_mtl_ofi_message_t *message;

    OPAL_FREE_LIST_GET(&ompi_mtl_ofi.free_messages,
                       tmp,
                       rc);
    if (NULL == tmp) return NULL;

    message = (ompi_mtl_ofi_message_t*) tmp;

    message->wc = *wc;

    return message;
}

static inline void
ompi_mtl_ofi_message_free(ompi_mtl_ofi_message_t *message)
{
    OPAL_FREE_LIST_RETURN(&ompi_mtl_ofi.free_messages,
                          &message->super);
}

#endif
