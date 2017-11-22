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

int mca_coll_monitoring_reduce(const void *sbuf, void *rbuf, int count,
                               struct ompi_datatype_t *dtype,
                               struct ompi_op_t *op,
                               int root,
                               struct ompi_communicator_t *comm,
                               mca_coll_base_module_t *module)
{
    mca_coll_monitoring_module_t*monitoring_module = (mca_coll_monitoring_module_t*) module;
    if( root == ompi_comm_rank(comm) ) {
        int i, rank;
        size_t type_size, data_size;
        const int comm_size = ompi_comm_size(comm);
        ompi_datatype_type_size(dtype, &type_size);
        data_size = count * type_size;
        for( i = 0; i < comm_size; ++i ) {
            if( root == i ) continue; /* No communication for self */
            /**
             * If this fails the destination is not part of my MPI_COM_WORLD
             * Lookup its name in the rank hastable to get its MPI_COMM_WORLD rank
             */
            if( OPAL_SUCCESS == mca_common_monitoring_get_world_rank(i, comm->c_remote_group, &rank) ) {
                mca_common_monitoring_record_coll(rank, data_size);
            }
        }
        mca_common_monitoring_coll_a2o(data_size * (comm_size - 1), monitoring_module->data);
    }
    return monitoring_module->real.coll_reduce(sbuf, rbuf, count, dtype, op, root, comm, monitoring_module->real.coll_reduce_module);
}

int mca_coll_monitoring_ireduce(const void *sbuf, void *rbuf, int count,
                                struct ompi_datatype_t *dtype,
                                struct ompi_op_t *op,
                                int root,
                                struct ompi_communicator_t *comm,
                                ompi_request_t ** request,
                                mca_coll_base_module_t *module)
{
    mca_coll_monitoring_module_t*monitoring_module = (mca_coll_monitoring_module_t*) module;
    if( root == ompi_comm_rank(comm) ) {
        int i, rank;
        size_t type_size, data_size;
        const int comm_size = ompi_comm_size(comm);
        ompi_datatype_type_size(dtype, &type_size);
        data_size = count * type_size;
        for( i = 0; i < comm_size; ++i ) {
            if( root == i ) continue; /* No communication for self */
            /**
             * If this fails the destination is not part of my MPI_COM_WORLD
             * Lookup its name in the rank hastable to get its MPI_COMM_WORLD rank
             */
            if( OPAL_SUCCESS == mca_common_monitoring_get_world_rank(i, comm->c_remote_group, &rank) ) {
                mca_common_monitoring_record_coll(rank, data_size);
            }
        }
        mca_common_monitoring_coll_a2o(data_size * (comm_size - 1), monitoring_module->data);
    }
    return monitoring_module->real.coll_ireduce(sbuf, rbuf, count, dtype, op, root, comm, request, monitoring_module->real.coll_ireduce_module);
}
