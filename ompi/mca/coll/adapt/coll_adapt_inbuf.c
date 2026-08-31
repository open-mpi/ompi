/*
 * Copyright (c) 2014-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 */

#include "coll_adapt.h"
#include "coll_adapt_inbuf.h"

OBJ_CLASS_INSTANCE(ompi_coll_adapt_inbuf_t, opal_free_list_item_t,
                   NULL, NULL);
