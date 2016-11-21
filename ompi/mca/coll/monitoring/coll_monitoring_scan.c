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

int mca_coll_monitoring_scan(const void *sbuf, void *rbuf, int count,
                             struct ompi_datatype_t *dtype,
                             struct ompi_op_t *op,
                             struct ompi_communicator_t *comm,
                             mca_coll_base_module_t *module)
{
    mca_coll_monitoring_module_t*monitoring_module = (mca_coll_monitoring_module_t*) module;
    return monitoring_module->real.coll_scan(sbuf, rbuf, count, dtype, op, comm, monitoring_module->real.coll_scan_module);
}

int mca_coll_monitoring_iscan(const void *sbuf, void *rbuf, int count,
                              struct ompi_datatype_t *dtype,
                              struct ompi_op_t *op,
                              struct ompi_communicator_t *comm,
                              ompi_request_t ** request,
                              mca_coll_base_module_t *module)
{
    mca_coll_monitoring_module_t*monitoring_module = (mca_coll_monitoring_module_t*) module;
    return monitoring_module->real.coll_iscan(sbuf, rbuf, count, dtype, op, comm, request, monitoring_module->real.coll_iscan_module);
}
