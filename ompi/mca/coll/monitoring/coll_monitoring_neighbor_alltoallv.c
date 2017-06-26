/*
 * Copyright (c) 2016-2017 Inria. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <ompi_config.h>
#include <ompi/request/request.h>
#include <ompi/datatype/ompi_datatype.h>
#include <ompi/communicator/communicator.h>
#include <ompi/mca/topo/base/base.h>
#include "coll_monitoring.h"

int mca_coll_monitoring_neighbor_alltoallv(const void *sbuf, const int *scounts,
                                           const int *sdisps, struct ompi_datatype_t *sdtype,
                                           void *rbuf, const int *rcounts, const int *rdisps,
                                           struct ompi_datatype_t *rdtype,
                                           struct ompi_communicator_t *comm,
                                           mca_coll_base_module_t *module)
{
    mca_coll_monitoring_module_t*monitoring_module = (mca_coll_monitoring_module_t*) module;
    size_t type_size, data_size, data_size_aggreg = 0;
    const mca_topo_base_comm_cart_t *cart = comm->c_topo->mtc.cart;
    int dim, i, srank, drank, world_rank;

    ompi_datatype_type_size(sdtype, &type_size);

    for( dim = 0, i = 0; dim < cart->ndims; ++dim ) {
        srank = MPI_PROC_NULL, drank = MPI_PROC_NULL;

        if (cart->dims[dim] > 1) {
            mca_topo_base_cart_shift (comm, dim, 1, &srank, &drank);
        } else if (1 == cart->dims[dim] && cart->periods[dim]) {
            /* Don't record exchanges with self */
            continue;
        }

        if (MPI_PROC_NULL != srank) {
            data_size = scounts[i] * type_size;
            /**
             * If this fails the destination is not part of my MPI_COM_WORLD
             * Lookup its name in the rank hastable to get its MPI_COMM_WORLD rank
             */
            if( OPAL_SUCCESS == mca_common_monitoring_get_world_rank(srank, comm, &world_rank) ) {
                mca_common_monitoring_record_coll(world_rank, data_size);
                data_size_aggreg += data_size;
            }
            ++i;
        }

        if (MPI_PROC_NULL != drank) {
            data_size = scounts[i] * type_size;
            /**
             * If this fails the destination is not part of my MPI_COM_WORLD
             * Lookup its name in the rank hastable to get its MPI_COMM_WORLD rank
             */
            if( OPAL_SUCCESS == mca_common_monitoring_get_world_rank(drank, comm, &world_rank) ) {
                mca_common_monitoring_record_coll(world_rank, data_size);
                data_size_aggreg += data_size;
            }
            ++i;
        }
    }

    mca_common_monitoring_coll_a2a(data_size_aggreg, monitoring_module->data);

    return monitoring_module->real.coll_neighbor_alltoallv(sbuf, scounts, sdisps, sdtype, rbuf, rcounts, rdisps, rdtype, comm, monitoring_module->real.coll_neighbor_alltoallv_module);
}

int mca_coll_monitoring_ineighbor_alltoallv(const void *sbuf, const int *scounts,
                                            const int *sdisps,
                                            struct ompi_datatype_t *sdtype,
                                            void *rbuf, const int *rcounts,
                                            const int *rdisps,
                                            struct ompi_datatype_t *rdtype,
                                            struct ompi_communicator_t *comm,
                                            ompi_request_t ** request,
                                            mca_coll_base_module_t *module)
{
    mca_coll_monitoring_module_t*monitoring_module = (mca_coll_monitoring_module_t*) module;
    size_t type_size, data_size, data_size_aggreg = 0;
    const mca_topo_base_comm_cart_t *cart = comm->c_topo->mtc.cart;
    int dim, i, srank, drank, world_rank;

    ompi_datatype_type_size(sdtype, &type_size);

    for( dim = 0, i = 0; dim < cart->ndims; ++dim ) {
        srank = MPI_PROC_NULL, drank = MPI_PROC_NULL;

        if (cart->dims[dim] > 1) {
            mca_topo_base_cart_shift (comm, dim, 1, &srank, &drank);
        } else if (1 == cart->dims[dim] && cart->periods[dim]) {
            /* Don't record exchanges with self */
            continue;
        }

        if (MPI_PROC_NULL != srank) {
            data_size = scounts[i] * type_size;
            /**
             * If this fails the destination is not part of my MPI_COM_WORLD
             * Lookup its name in the rank hastable to get its MPI_COMM_WORLD rank
             */
            if( OPAL_SUCCESS == mca_common_monitoring_get_world_rank(srank, comm, &world_rank) ) {
                mca_common_monitoring_record_coll(world_rank, data_size);
                data_size_aggreg += data_size;
            }
            ++i;
        }

        if (MPI_PROC_NULL != drank) {
            data_size = scounts[i] * type_size;
            /**
             * If this fails the destination is not part of my MPI_COM_WORLD
             * Lookup its name in the rank hastable to get its MPI_COMM_WORLD rank
             */
            if( OPAL_SUCCESS == mca_common_monitoring_get_world_rank(drank, comm, &world_rank) ) {
                mca_common_monitoring_record_coll(world_rank, data_size);
                data_size_aggreg += data_size;
            }
            ++i;
        }
    }

    mca_common_monitoring_coll_a2a(data_size_aggreg, monitoring_module->data);

    return monitoring_module->real.coll_ineighbor_alltoallv(sbuf, scounts, sdisps, sdtype, rbuf, rcounts, rdisps, rdtype, comm, request, monitoring_module->real.coll_ineighbor_alltoallv_module);
}
