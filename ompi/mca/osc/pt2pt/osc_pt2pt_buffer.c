/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "osc_pt2pt.h"
#include "osc_pt2pt_buffer.h"

#include "opal/class/opal_free_list.h"

static void ompi_osc_pt2pt_buffer_construct(ompi_osc_pt2pt_buffer_t *buf)
{
    buf->payload = buf + 1;
}


static void ompi_osc_pt2pt_buffer_destruct(ompi_osc_pt2pt_buffer_t *buf)
{
    buf->payload = NULL;
}


OBJ_CLASS_INSTANCE(ompi_osc_pt2pt_buffer_t, opal_free_list_item_t,
                   ompi_osc_pt2pt_buffer_construct, 
                   ompi_osc_pt2pt_buffer_destruct);

