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

int mca_coll_monitoring_reduce_scatter_block(const void *sbuf, void *rbuf,
                                             int rcount,
                                             struct ompi_datatype_t *dtype,
                                             struct ompi_op_t *op,
                                             struct ompi_communicator_t *comm,
                                             mca_coll_base_module_t *module)
{
    mca_coll_monitoring_module_t*monitoring_module = (mca_coll_monitoring_module_t*) module;
    /* size_t type_size, data_size, rank = ompi_comm_rank(comm); */
    /* ompi_datatype_type_size(dtype, &type_size); */
    /* data_size = rcounts[rank]*type_size; */
    /* mca_common_monitoring_coll_o2a(data_size, monitoring_module->data); */
    return monitoring_module->real.coll_reduce_scatter_block(sbuf, rbuf, rcount, dtype, op, comm, monitoring_module->real.coll_reduce_scatter_block_module);
}

int mca_coll_monitoring_ireduce_scatter_block(const void *sbuf, void *rbuf,
                                              int rcount,
                                              struct ompi_datatype_t *dtype,
                                              struct ompi_op_t *op,
                                              struct ompi_communicator_t *comm,
                                              ompi_request_t ** request,
                                              mca_coll_base_module_t *module)
{
    mca_coll_monitoring_module_t*monitoring_module = (mca_coll_monitoring_module_t*) module;
    /* size_t type_size, data_size, rank = ompi_comm_rank(comm); */
    /* ompi_datatype_type_size(dtype, &type_size); */
    /* data_size = rcounts[rank]*type_size; */
    /* mca_common_monitoring_coll_o2a(data_size, monitoring_module->data); */
    return monitoring_module->real.coll_ireduce_scatter_block(sbuf, rbuf, rcount, dtype, op, comm, request, monitoring_module->real.coll_ireduce_scatter_block_module);
}
