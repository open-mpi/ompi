/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2006-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2011      Sandia National Laboratories. All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2020      Sandia National Laboratories. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/communicator/communicator.h"
#include "ompi/mca/part/base/part_base_prequest.h"
#include "ompi/mca/part/base/base.h"

#include "part_rma.h"
#include "part_rma_sendreq.h"
#include "part_rma_recvreq.h"

ompi_part_rma_t ompi_part_rma = {
    {
        mca_part_rma_progress,
        mca_part_rma_precv_init,
        mca_part_rma_psend_init,
        mca_part_rma_start,
        mca_part_rma_pready,
        mca_part_rma_parrived, 
    }
};


int mca_part_rma_create_pcomm(ompi_communicator_t* comm,
                               int rank_count,
                               const int ranks[],
                               int tag,
                               ompi_communicator_t** new_comm)
{
    int err = OMPI_SUCCESS;
    MPI_Group group_super, group_sub;

    err = MPI_Comm_group(comm, &group_super);
    if(OMPI_SUCCESS != err) return err;

    err = MPI_Group_incl(group_super, rank_count, ranks, &group_sub);
    if(OMPI_SUCCESS != err) return err;

    err = MPI_Comm_create_group(comm, group_sub, tag, new_comm);
    return err;
}


OBJ_CLASS_INSTANCE(mca_part_rma_list_t,
                   opal_list_item_t,
                   NULL,
                   NULL);

