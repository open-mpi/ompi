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

int mca_coll_monitoring_gather(const void *sbuf, int scount,
                               struct ompi_datatype_t *sdtype,
                               void *rbuf, int rcount, struct ompi_datatype_t *rdtype,
                               int root, struct ompi_communicator_t *comm,
                               mca_coll_base_module_t *module)
{
    mca_coll_monitoring_module_t*monitoring_module = (mca_coll_monitoring_module_t*) module;
    size_t type_size, data_size;
    ompi_datatype_type_size(sdtype, &type_size);
    data_size = scount*type_size;
    if( root != ompi_comm_rank(comm) ) {
        mca_common_monitoring_coll_a2o(data_size, monitoring_module->data);
    }
    return monitoring_module->real.coll_gather(sbuf, scount, sdtype, rbuf, rcount, rdtype, root, comm, monitoring_module->real.coll_gather_module);
}

int mca_coll_monitoring_igather(const void *sbuf, int scount,
                                struct ompi_datatype_t *sdtype,
                                void *rbuf, int rcount, struct ompi_datatype_t *rdtype,
                                int root, struct ompi_communicator_t *comm,
                                ompi_request_t ** request,
                                mca_coll_base_module_t *module)
{
    mca_coll_monitoring_module_t*monitoring_module = (mca_coll_monitoring_module_t*) module;
    size_t type_size, data_size;
    ompi_datatype_type_size(sdtype, &type_size);
    data_size = scount*type_size;
    if( root != ompi_comm_rank(comm) ) {
        mca_common_monitoring_coll_a2o(data_size, monitoring_module->data);
    }
    return monitoring_module->real.coll_igather(sbuf, scount, sdtype, rbuf, rcount, rdtype, root, comm, request, monitoring_module->real.coll_igather_module);
}
