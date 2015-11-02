/*
 * Copyright (c) 2013-2015 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2013-2015 Inria.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <ompi_config.h>
#include <pml_monitoring.h>

int mca_pml_monitoring_add_comm(struct ompi_communicator_t* comm)
{
    return pml_selected_module.pml_add_comm(comm);
}

int mca_pml_monitoring_del_comm(struct ompi_communicator_t* comm)
{
    return pml_selected_module.pml_del_comm(comm);
}
