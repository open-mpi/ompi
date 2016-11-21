/*
 * Copyright (c) 2016 Inria. All rights reserved.
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
#include <coll_monitoring.h>

int mca_coll_monitoring_allreduce(const void *sbuf, void *rbuf, int count,
                                          struct ompi_datatype_t *dtype,
                                          struct ompi_op_t *op,
                                          struct ompi_communicator_t *comm,
                                          mca_coll_base_module_t *module)
{
    mca_coll_monitoring_module_t*monitoring_module = (mca_coll_monitoring_module_t*) module;
    size_t type_size, data_size;
    ompi_datatype_type_size(dtype, &type_size);
    data_size = count*type_size;
    mca_common_monitoring_coll_a2a(data_size, monitoring_module->data);
    return monitoring_module->real.coll_allreduce(sbuf, rbuf, count, dtype, op, comm, monitoring_module->real.coll_allreduce_module);
}

int mca_coll_monitoring_iallreduce(const void *sbuf, void *rbuf, int count,
                                          struct ompi_datatype_t *dtype,
                                          struct ompi_op_t *op,
                                          struct ompi_communicator_t *comm,
                                          ompi_request_t ** request,
                                          mca_coll_base_module_t *module)
{
    mca_coll_monitoring_module_t*monitoring_module = (mca_coll_monitoring_module_t*) module;
    size_t type_size, data_size;
    ompi_datatype_type_size(dtype, &type_size);
    data_size = count*type_size;
    mca_common_monitoring_coll_a2a(data_size, monitoring_module->data);
    return monitoring_module->real.coll_iallreduce(sbuf, rbuf, count, dtype, op, comm, request, monitoring_module->real.coll_iallreduce_module);
}
