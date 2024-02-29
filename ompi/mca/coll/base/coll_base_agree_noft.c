/*
 * Copyright (c) 2012-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "opal/util/bit_ops.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/coll/base/base.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/mca/coll/base/coll_base_functions.h"

int
ompi_coll_base_agree_noft(void *contrib,
                         int dt_count,
                         struct ompi_datatype_t *dt,
                         struct ompi_op_t *op,
                         struct ompi_group_t **group, bool update_grp,
                         struct ompi_communicator_t* comm,
                         mca_coll_base_module_t *module)
{
    void *sendbuf = OMPI_COMM_IS_INTER(comm) ? contrib : MPI_IN_PLACE;
    return comm->c_coll->coll_allreduce(sendbuf, contrib, dt_count, dt, op,
                                       comm, comm->c_coll->coll_allreduce_module);
}

int
ompi_coll_base_iagree_noft(void *contrib,
                          int dt_count,
                          struct ompi_datatype_t *dt,
                          struct ompi_op_t *op,
                          struct ompi_group_t **group, bool update_grp,
                          struct ompi_communicator_t* comm,
                          ompi_request_t **request,
                          mca_coll_base_module_t *module)
{
    void *sendbuf = OMPI_COMM_IS_INTER(comm) ? contrib : MPI_IN_PLACE;
    return comm->c_coll->coll_iallreduce(sendbuf, contrib, dt_count, dt, op,
                                        comm, request, comm->c_coll->coll_iallreduce_module);
}
