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
#include <ompi/communicator/communicator.h>
#include <coll_monitoring.h>

int mca_coll_monitoring_barrier(struct ompi_communicator_t *comm,
                                mca_coll_base_module_t *module)
{
    mca_coll_monitoring_module_t*monitoring_module = (mca_coll_monitoring_module_t*) module;
    mca_common_monitoring_coll_a2a(0, monitoring_module->data);
    return monitoring_module->real.coll_barrier(comm, monitoring_module->real.coll_barrier_module);
}

int mca_coll_monitoring_ibarrier(struct ompi_communicator_t *comm,
                                 ompi_request_t ** request,
                                 mca_coll_base_module_t *module)
{
    mca_coll_monitoring_module_t*monitoring_module = (mca_coll_monitoring_module_t*) module;
    mca_common_monitoring_coll_a2a(0, monitoring_module->data);
    return monitoring_module->real.coll_ibarrier(comm, request, monitoring_module->real.coll_ibarrier_module);
}
