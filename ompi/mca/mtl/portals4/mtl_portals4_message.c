/*
 * Copyright (c) 2012      Sandia National Laboratories.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/constants.h"

#include "mtl_portals4.h"
#include "mtl_portals4_message.h"

static void
ompi_mtl_portals4_message_construct(ompi_mtl_portals4_message_t *message)
{
    message->buffer = message + 1;
}

OBJ_CLASS_INSTANCE(ompi_mtl_portals4_message_t,
                   opal_free_list_item_t,
                   ompi_mtl_portals4_message_construct, NULL);

