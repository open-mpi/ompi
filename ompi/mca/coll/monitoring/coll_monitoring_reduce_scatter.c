/*
 * Copyright (c) 2016-2018 Inria. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <ompi_config.h>
#include <ompi/op/op.h>
#include <ompi/request/request.h>
#include <ompi/datatype/ompi_datatype.h>
#include <ompi/communicator/communicator.h>
#include "coll_monitoring.h"

int mca_coll_monitoring_reduce_scatter(const void *sbuf, void *rbuf,
                                       const int *rcounts,
                                       struct ompi_datatype_t *dtype,
                                       struct ompi_op_t *op,
                                       struct ompi_communicator_t *comm,
                                       mca_coll_base_module_t *module)
{
    mca_coll_monitoring_module_t*monitoring_module = (mca_coll_monitoring_module_t*) module;
    size_t type_size, data_size, data_size_aggreg = 0;
    const int comm_size = ompi_comm_size(comm);
    const int my_rank = ompi_comm_rank(comm);
    int i, rank;
    ompi_datatype_type_size(dtype, &type_size);
    for( i = 0; i < comm_size; ++i ) {
        if( my_rank == i ) continue; /* No communication for self */
        data_size = rcounts[i] * type_size;
        /**
         * If this fails the destination is not part of my MPI_COM_WORLD
         * Lookup its name in the rank hastable to get its MPI_COMM_WORLD rank
         */
        if( OPAL_SUCCESS == mca_common_monitoring_get_world_rank(i, comm->c_remote_group, &rank) ) {
            mca_common_monitoring_record_coll(rank, data_size);
        }
        data_size_aggreg += data_size;
    }
    mca_common_monitoring_coll_a2a(data_size_aggreg, monitoring_module->data);
    return monitoring_module->real.coll_reduce_scatter(sbuf, rbuf, rcounts, dtype, op, comm, monitoring_module->real.coll_reduce_scatter_module);
}

int mca_coll_monitoring_ireduce_scatter(const void *sbuf, void *rbuf,
                                        const int *rcounts,
                                        struct ompi_datatype_t *dtype,
                                        struct ompi_op_t *op,
                                        struct ompi_communicator_t *comm,
                                        ompi_request_t ** request,
                                        mca_coll_base_module_t *module)
{
    mca_coll_monitoring_module_t*monitoring_module = (mca_coll_monitoring_module_t*) module;
    size_t type_size, data_size, data_size_aggreg = 0;
    const int comm_size = ompi_comm_size(comm);
    const int my_rank = ompi_comm_rank(comm);
    int i, rank;
    ompi_datatype_type_size(dtype, &type_size);
    for( i = 0; i < comm_size; ++i ) {
        if( my_rank == i ) continue; /* No communication for self */
        data_size = rcounts[i] * type_size;
        /**
         * If this fails the destination is not part of my MPI_COM_WORLD
         * Lookup its name in the rank hastable to get its MPI_COMM_WORLD rank
         */
        if( OPAL_SUCCESS == mca_common_monitoring_get_world_rank(i, comm->c_remote_group, &rank) ) {
            mca_common_monitoring_record_coll(rank, data_size);
        }
        data_size_aggreg += data_size;
    }
    mca_common_monitoring_coll_a2a(data_size_aggreg, monitoring_module->data);
    return monitoring_module->real.coll_ireduce_scatter(sbuf, rbuf, rcounts, dtype, op, comm, request, monitoring_module->real.coll_ireduce_scatter_module);
}
