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

int mca_coll_monitoring_scatterv(const void *sbuf, const int *scounts, const int *disps,
                                 struct ompi_datatype_t *sdtype,
                                 void* rbuf, int rcount, struct ompi_datatype_t *rdtype,
                                 int root, struct ompi_communicator_t *comm,
                                 mca_coll_base_module_t *module)
{
    mca_coll_monitoring_module_t*monitoring_module = (mca_coll_monitoring_module_t*) module;
    size_t type_size, data_size, rank = ompi_comm_rank(comm);
    ompi_datatype_type_size(sdtype, &type_size);
    data_size = scounts[rank]*type_size;
    if( root != rank ) {
        mca_common_monitoring_coll_a2o(data_size, monitoring_module->data);
    }
    return monitoring_module->real.coll_scatterv(sbuf, scounts, disps, sdtype, rbuf, rcount, rdtype, root, comm, monitoring_module->real.coll_scatterv_module);
}

int mca_coll_monitoring_iscatterv(const void *sbuf, const int *scounts, const int *disps,
                                  struct ompi_datatype_t *sdtype,
                                  void *rbuf, int rcount, struct ompi_datatype_t *rdtype,
                                  int root, struct ompi_communicator_t *comm,
                                  ompi_request_t ** request,
                                  mca_coll_base_module_t *module)
{
    mca_coll_monitoring_module_t*monitoring_module = (mca_coll_monitoring_module_t*) module;
    size_t type_size, data_size, rank = ompi_comm_rank(comm);
    ompi_datatype_type_size(sdtype, &type_size);
    data_size = scounts[rank]*type_size;
    if( root != rank ) {
        mca_common_monitoring_coll_a2o(data_size, monitoring_module->data);
    }
    return monitoring_module->real.coll_iscatterv(sbuf, scounts, disps, sdtype, rbuf, rcount, rdtype, root, comm, request, monitoring_module->real.coll_iscatterv_module);
}
