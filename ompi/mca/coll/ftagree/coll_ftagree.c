/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2012-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "ompi_config.h"

#include "ompi/constants.h"
#include "opal/util/bit_ops.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/base.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/proc/proc.h"

#include "ompi/mca/coll/ftagree/coll_ftagree.h"
#include "ompi/mca/coll/ftagree/coll_ftagree_era.h"

/*************************************
 * Local Functions
 *************************************/

/*************************************
 * Agreement Object Support
 *************************************/
static void mca_coll_ftagree_construct(mca_coll_ftagree_t *v_info)
{
    v_info->agreement_seq_num = 0;
}

OBJ_CLASS_INSTANCE(mca_coll_ftagree_t,
                   opal_object_t,
                   mca_coll_ftagree_construct,
                   NULL);

