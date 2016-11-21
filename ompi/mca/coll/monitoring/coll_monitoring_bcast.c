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

int mca_coll_monitoring_bcast(void *buff, int count,
                               struct ompi_datatype_t *datatype,
                               int root,
                               struct ompi_communicator_t *comm,
                               mca_coll_base_module_t *module)
{
    mca_coll_monitoring_module_t*monitoring_module = (mca_coll_monitoring_module_t*) module;
    size_t type_size, data_size, rank;
    ompi_datatype_type_size(datatype, &type_size);
    data_size = count*type_size;
    if( root == ompi_comm_rank(comm) ) {
        mca_common_monitoring_coll_o2a(data_size, monitoring_module->data);
    }
    return monitoring_module->real.coll_bcast(buff, count, datatype, root, comm, monitoring_module->real.coll_bcast_module);
}

int mca_coll_monitoring_ibcast(void *buff, int count,
                               struct ompi_datatype_t *datatype,
                               int root,
                               struct ompi_communicator_t *comm,
                               ompi_request_t ** request,
                               mca_coll_base_module_t *module)
{
    mca_coll_monitoring_module_t*monitoring_module = (mca_coll_monitoring_module_t*) module;
    size_t type_size, data_size, rank;
    ompi_datatype_type_size(datatype, &type_size);
    data_size = count*type_size;
    if( root == ompi_comm_rank(comm) ) {
        mca_common_monitoring_coll_o2a(data_size, monitoring_module->data);
    }
    return monitoring_module->real.coll_ibcast(buff, count, datatype, root, comm, request, monitoring_module->real.coll_ibcast_module);
}
