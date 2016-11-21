/*
 * Copyright (c) 2016 Inria. All rights reserved.
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
#include <coll_monitoring.h>

int mca_coll_monitoring_alltoallv(const void *sbuf, const int *scounts, const int *sdisps,
                                  struct ompi_datatype_t *sdtype,
                                  void *rbuf, const int *rcounts, const int *rdisps,
                                  struct ompi_datatype_t *rdtype,
                                  struct ompi_communicator_t *comm,
                                  mca_coll_base_module_t *module)
{
    mca_coll_monitoring_module_t*monitoring_module = (mca_coll_monitoring_module_t*) module;
    size_t type_size, data_size, rank = ompi_comm_rank(comm);
    ompi_datatype_type_size(sdtype, &type_size);
    data_size = scounts[rank]*type_size;
    mca_common_monitoring_coll_a2a(data_size, monitoring_module->data);
    return monitoring_module->real.coll_alltoallv(sbuf, scounts, sdisps, sdtype, rbuf, rcounts, rdisps, rdtype, comm, monitoring_module->real.coll_alltoallv_module);
}

int mca_coll_monitoring_ialltoallv(const void *sbuf, const int *scounts,
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
    size_t type_size, data_size, rank = ompi_comm_rank(comm);
    ompi_datatype_type_size(sdtype, &type_size);
    data_size = scounts[rank]*type_size;
    mca_common_monitoring_coll_a2a(data_size, monitoring_module->data);
    return monitoring_module->real.coll_ialltoallv(sbuf, scounts, sdisps, sdtype, rbuf, rcounts, rdisps, rdtype, comm, request, monitoring_module->real.coll_ialltoallv_module);
}

int mca_coll_monitoring_neighbor_alltoallv(const void *sbuf, const int *scounts,
                                           const int *sdisps, struct ompi_datatype_t *sdtype,
                                           void *rbuf, const int *rcounts, const int *rdisps,
                                           struct ompi_datatype_t *rdtype,
                                           struct ompi_communicator_t *comm,
                                           mca_coll_base_module_t *module)
{
    mca_coll_monitoring_module_t*monitoring_module = (mca_coll_monitoring_module_t*) module;
    size_t type_size, data_size, rank = ompi_comm_rank(comm);
    ompi_datatype_type_size(sdtype, &type_size);
    data_size = scounts[rank]*type_size;
    mca_common_monitoring_coll_a2a(data_size, monitoring_module->data);
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
    size_t type_size, data_size, rank = ompi_comm_rank(comm);
    ompi_datatype_type_size(sdtype, &type_size);
    data_size = scounts[rank]*type_size;
    mca_common_monitoring_coll_a2a(data_size, monitoring_module->data);
    return monitoring_module->real.coll_ineighbor_alltoallv(sbuf, scounts, sdisps, sdtype, rbuf, rcounts, rdisps, rdtype, comm, request, monitoring_module->real.coll_ineighbor_alltoallv_module);
}
