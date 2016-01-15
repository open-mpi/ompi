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


/* EJ: nothing to do here */

int mca_pml_monitoring_iprobe( int dst,
                               int tag,
                               struct ompi_communicator_t* comm,
                               int *matched,
                               ompi_status_public_t* status )
{
    return pml_selected_module.pml_iprobe(dst, tag, comm,
                                          matched, status);
}

int mca_pml_monitoring_probe( int dst,
                              int tag,
                              struct ompi_communicator_t* comm,
                              ompi_status_public_t* status )
{
    return pml_selected_module.pml_probe(dst, tag, comm, status);
}

int mca_pml_monitoring_improbe(int dst,
                               int tag,
                               struct ompi_communicator_t* comm,
                               int *matched,
                               struct ompi_message_t **message,
                               ompi_status_public_t* status)
{
    return pml_selected_module.pml_improbe(dst, tag, comm,
                                           matched, message, status);
}


int mca_pml_monitoring_mprobe(int dst,
                              int tag,
                              struct ompi_communicator_t* comm,
                              struct ompi_message_t **message,
                              ompi_status_public_t* status)
{
    return pml_selected_module.pml_mprobe(dst, tag, comm, message, status);
}

