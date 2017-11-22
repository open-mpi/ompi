/*
 * Copyright (c) 2016-2018 Inria. All rights reserved.
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
#include "coll_monitoring.h"

int mca_coll_monitoring_alltoallw(const void *sbuf, const int *scounts,
                                  const int *sdisps,
                                  struct ompi_datatype_t * const *sdtypes,
                                  void *rbuf, const int *rcounts,
                                  const int *rdisps,
                                  struct ompi_datatype_t * const *rdtypes,
                                  struct ompi_communicator_t *comm,
                                  mca_coll_base_module_t *module)
{
    mca_coll_monitoring_module_t*monitoring_module = (mca_coll_monitoring_module_t*) module;
    size_t type_size, data_size, data_size_aggreg = 0;
    const int comm_size = ompi_comm_size(comm);
    const int my_rank = ompi_comm_rank(comm);
    int i, rank;
    for( i = 0; i < comm_size; ++i ) {
        if( my_rank == i ) continue; /* No communication for self */
        ompi_datatype_type_size(sdtypes[i], &type_size);
        data_size = scounts[i] * type_size;
        /**
         * If this fails the destination is not part of my MPI_COM_WORLD
         * Lookup its name in the rank hastable to get its MPI_COMM_WORLD rank
         */
        if( OPAL_SUCCESS == mca_common_monitoring_get_world_rank(i, comm->c_remote_group, &rank) ) {
            mca_common_monitoring_record_coll(rank, data_size);
            data_size_aggreg += data_size;
        }
    }
    mca_common_monitoring_coll_a2a(data_size_aggreg, monitoring_module->data);
    return monitoring_module->real.coll_alltoallw(sbuf, scounts, sdisps, sdtypes, rbuf, rcounts, rdisps, rdtypes, comm, monitoring_module->real.coll_alltoallw_module);
}

int mca_coll_monitoring_ialltoallw(const void *sbuf, const int *scounts,
                                   const int *sdisps,
                                   struct ompi_datatype_t * const *sdtypes,
                                   void *rbuf, const int *rcounts,
                                   const int *rdisps,
                                   struct ompi_datatype_t * const *rdtypes,
                                   struct ompi_communicator_t *comm,
                                   ompi_request_t ** request,
                                   mca_coll_base_module_t *module)
{
    mca_coll_monitoring_module_t*monitoring_module = (mca_coll_monitoring_module_t*) module;
    size_t type_size, data_size, data_size_aggreg = 0;
    const int comm_size = ompi_comm_size(comm);
    const int my_rank = ompi_comm_rank(comm);
    int i, rank;
    for( i = 0; i < comm_size; ++i ) {
        if( my_rank == i ) continue; /* No communication for self */
        ompi_datatype_type_size(sdtypes[i], &type_size);
        data_size = scounts[i] * type_size;
        /**
         * If this fails the destination is not part of my MPI_COM_WORLD
         * Lookup its name in the rank hastable to get its MPI_COMM_WORLD rank
         */
        if( OPAL_SUCCESS == mca_common_monitoring_get_world_rank(i, comm->c_remote_group, &rank) ) {
            mca_common_monitoring_record_coll(rank, data_size);
            data_size_aggreg += data_size;
        }
    }
    mca_common_monitoring_coll_a2a(data_size_aggreg, monitoring_module->data);
    return monitoring_module->real.coll_ialltoallw(sbuf, scounts, sdisps, sdtypes, rbuf, rcounts, rdisps, rdtypes, comm, request, monitoring_module->real.coll_ialltoallw_module);
}
