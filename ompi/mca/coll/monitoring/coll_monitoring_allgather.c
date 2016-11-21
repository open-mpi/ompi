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

int mca_coll_monitoring_allgather(const void *sbuf, int scount,
                                  struct ompi_datatype_t *sdtype,
                                  void *rbuf, int rcount,
                                  struct ompi_datatype_t *rdtype,
                                  struct ompi_communicator_t *comm,
                                  mca_coll_base_module_t *module)
{
    mca_coll_monitoring_module_t*monitoring_module = (mca_coll_monitoring_module_t*) module;
    size_t type_size, data_size;
    ompi_datatype_type_size(sdtype, &type_size);
    data_size = scount*type_size;
    mca_common_monitoring_coll_a2a(data_size, monitoring_module->data);
    return monitoring_module->real.coll_allgather(sbuf, scount, sdtype, rbuf, rcount, rdtype, comm, monitoring_module->real.coll_allgather_module);
}

int mca_coll_monitoring_iallgather(const void *sbuf, int scount,
                                   struct ompi_datatype_t *sdtype,
                                   void *rbuf, int rcount,
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
    return monitoring_module->real.coll_iallgather(sbuf, scount, sdtype, rbuf, rcount, rdtype, comm, request, monitoring_module->real.coll_iallgather_module);
}

int mca_coll_monitoring_neighbor_allgather(const void *sbuf, int scount,
                                           struct ompi_datatype_t *sdtype, void *rbuf,
                                           int rcount, struct ompi_datatype_t *rdtype,
                                           struct ompi_communicator_t *comm,
                                           mca_coll_base_module_t *module)
{
    mca_coll_monitoring_module_t*monitoring_module = (mca_coll_monitoring_module_t*) module;
    size_t type_size, data_size;
    ompi_datatype_type_size(sdtype, &type_size);
    data_size = scount*type_size;
    mca_common_monitoring_coll_a2a(data_size, monitoring_module->data);
    return monitoring_module->real.coll_neighbor_allgather(sbuf, scount, sdtype, rbuf, rcount, rdtype, comm, monitoring_module->real.coll_neighbor_allgather_module);
}

int mca_coll_monitoring_ineighbor_allgather(const void *sbuf, int scount,
                                            struct ompi_datatype_t *sdtype, void *rbuf,
                                            int rcount, struct ompi_datatype_t *rdtype,
                                            struct ompi_communicator_t *comm,
                                            ompi_request_t ** request,
                                            mca_coll_base_module_t *module)
{
    mca_coll_monitoring_module_t*monitoring_module = (mca_coll_monitoring_module_t*) module;
    size_t type_size, data_size;
    ompi_datatype_type_size(sdtype, &type_size);
    data_size = scount*type_size;
    mca_common_monitoring_coll_a2a(data_size, monitoring_module->data);
    return monitoring_module->real.coll_ineighbor_allgather(sbuf, scount, sdtype, rbuf, rcount, rdtype, comm, request, monitoring_module->real.coll_ineighbor_allgather_module);
}
