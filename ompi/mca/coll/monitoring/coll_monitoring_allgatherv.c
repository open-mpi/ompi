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

int mca_coll_monitoring_allgatherv(const void *sbuf, int scount,
                                   struct ompi_datatype_t *sdtype,
                                   void * rbuf, const int *rcounts, const int *disps,
                                   struct ompi_datatype_t *rdtype,
                                   struct ompi_communicator_t *comm,
                                   mca_coll_base_module_t *module)
{
    mca_coll_monitoring_module_t*monitoring_module = (mca_coll_monitoring_module_t*) module;
    size_t type_size, data_size;
    ompi_datatype_type_size(sdtype, &type_size);
    data_size = scount*type_size;
    mca_common_monitoring_coll_a2a(data_size, monitoring_module->data);
    return monitoring_module->real.coll_allgatherv(sbuf, scount, sdtype, rbuf, rcounts, disps, rdtype, comm, monitoring_module->real.coll_allgatherv_module);
}

int mca_coll_monitoring_iallgatherv(const void *sbuf, int scount,
                                    struct ompi_datatype_t *sdtype,
                                    void * rbuf, const int *rcounts, const int *disps,
                                    struct ompi_datatype_t *rdtype,
                                    struct ompi_communicator_t *comm,
                                    ompi_request_t ** request,
                                    mca_coll_base_module_t *module)
{
    mca_coll_monitoring_module_t*monitoring_module = (mca_coll_monitoring_module_t*) module;
    size_t type_size, data_size;
    ompi_datatype_type_size(sdtype, &type_size);
    data_size = scount*type_size;
    mca_common_monitoring_coll_a2a(data_size, monitoring_module->data);
    return monitoring_module->real.coll_iallgatherv(sbuf, scount, sdtype, rbuf, rcounts, disps, rdtype, comm, request, monitoring_module->real.coll_iallgatherv_module);
}

int mca_coll_monitoring_neighbor_allgatherv(const void *sbuf, int scount,
                                            struct ompi_datatype_t *sdtype,
                                            void * rbuf, const int *rcounts, const int *disps,
                                            struct ompi_datatype_t *rdtype,
                                            struct ompi_communicator_t *comm,
                                            mca_coll_base_module_t *module)
{
    mca_coll_monitoring_module_t*monitoring_module = (mca_coll_monitoring_module_t*) module;
    size_t type_size, data_size;
    ompi_datatype_type_size(sdtype, &type_size);
    data_size = scount*type_size;
    mca_common_monitoring_coll_a2a(data_size, monitoring_module->data);
    return monitoring_module->real.coll_neighbor_allgatherv(sbuf, scount, sdtype, rbuf, rcounts, disps, rdtype, comm, monitoring_module->real.coll_neighbor_allgatherv_module);
}

int mca_coll_monitoring_ineighbor_allgatherv(const void *sbuf, int scount,
                                             struct ompi_datatype_t *sdtype,
                                             void * rbuf, const int *rcounts, const int *disps,
                                             struct ompi_datatype_t *rdtype,
                                             struct ompi_communicator_t *comm,
                                             ompi_request_t ** request,
                                             mca_coll_base_module_t *module)
{
    mca_coll_monitoring_module_t*monitoring_module = (mca_coll_monitoring_module_t*) module;
    size_t type_size, data_size;
    ompi_datatype_type_size(sdtype, &type_size);
    data_size = scount*type_size;
    mca_common_monitoring_coll_a2a(data_size, monitoring_module->data);
    return monitoring_module->real.coll_ineighbor_allgatherv(sbuf, scount, sdtype, rbuf, rcounts, disps, rdtype, comm, request, monitoring_module->real.coll_ineighbor_allgatherv_module);
}
