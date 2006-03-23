/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include <stdlib.h>
#include "mpi.h"
#include "ompi/peruse/peruse.h"
#include "ompi/peruse/peruse-internal.h"
#include "ompi/constants.h"
#include "class/ompi_pointer_array.h"


void ompi_peruse_handle_construct (ompi_peruse_handle_t* p);
void ompi_peruse_handle_destruct (ompi_peruse_handle_t* p);

static opal_list_t peruse_handle_list;
static int ompi_peruse_initialized = 0;

OBJ_CLASS_INSTANCE(
    ompi_peruse_handle_t,
    opal_list_item_t,
    ompi_peruse_handle_construct,
    ompi_peruse_handle_destruct);

void ompi_peruse_handle_construct (ompi_peruse_handle_t* p)
{
    OBJ_CONSTRUCT (&(p->lock), opal_mutex_t);
    p->active = 0;
    p->event = PERUSE_EVENT_INVALID;
    p->type = PERUSE_TYPE_INVALID;
    p->comm = MPI_COMM_NULL;
    /* p->win = MPI_WIN_NULL; */
    /* p->file = MPI_FILE_NULL */
    p->fn = NULL;
    p->param = NULL;

    OPAL_THREAD_LOCK (&peruse_handle_list_lock);
    opal_list_append (&peruse_handle_list, (opal_list_item_t*)p);
    OPAL_THREAD_UNLOCK (&peruse_handle_list_lock);
}


void ompi_peruse_handle_destruct (ompi_peruse_handle_t* p)
{
    OPAL_THREAD_LOCK (&peruse_handle_list_lock);
    opal_list_remove_item (&peruse_handle_list, (opal_list_item_t*)p);
    OPAL_THREAD_UNLOCK (&peruse_handle_list_lock);

    OBJ_DESTRUCT (&(p->lock));
}


int ompi_peruse_init (void)
{
    if (ompi_peruse_initialized)
        return OMPI_SUCCESS;
    ompi_peruse_initialized = 1;

    OBJ_CONSTRUCT (&peruse_handle_list, opal_list_t);

    return OMPI_SUCCESS;
}


int ompi_peruse_finalize (void)
{
    OBJ_DESTRUCT (&peruse_handle_list);
    return OMPI_SUCCESS;
}
