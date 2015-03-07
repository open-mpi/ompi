/*
 * Copyright (c) 2013-2014 Intel, Inc. All rights reserved
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/constants.h"

#include "mtl_ofi.h"
#include "mtl_ofi_message.h"

static void
ompi_mtl_ofi_message_construct(ompi_mtl_ofi_message_t *message)
{
    message->buffer = message + 1;
}

OBJ_CLASS_INSTANCE(
    ompi_mtl_ofi_message_t,
    opal_free_list_item_t,
    ompi_mtl_ofi_message_construct,
    NULL
);
