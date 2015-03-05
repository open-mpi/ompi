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


/* EJ: loging is done on the sender. Nothing to do here */

int mca_pml_monitoring_irecv_init(void *buf,
                                  size_t count,
                                  ompi_datatype_t *datatype,
                                  int src,
                                  int tag,
                                  struct ompi_communicator_t* comm,
                                  struct ompi_request_t **request)
{
    return pml_selected_module.pml_irecv_init(buf, count, datatype,
                                              src, tag, comm, request);
}


int mca_pml_monitoring_irecv(void *buf,
                             size_t count,
                             ompi_datatype_t *datatype,
                             int src,
                             int tag,
                             struct ompi_communicator_t* comm,
                             struct ompi_request_t **request)
{
    return pml_selected_module.pml_irecv(buf, count, datatype,
                                         src, tag, comm, request);
}


int mca_pml_monitoring_recv(void *buf,
                            size_t count,
                            ompi_datatype_t *datatype,
                            int src,
                            int tag,
                            struct ompi_communicator_t* comm,
                            ompi_status_public_t* status)
{
    return pml_selected_module.pml_recv(buf, count, datatype,
                                        src, tag, comm, status);
}


int mca_pml_monitoring_imrecv(void *buf,
                              size_t count,
                              ompi_datatype_t *datatype,
                              struct ompi_message_t **message,
                              struct ompi_request_t **request)
{
    return pml_selected_module.pml_imrecv(buf, count, datatype,
                                          message, request);
}


int mca_pml_monitoring_mrecv(void *buf,
                             size_t count,
                             ompi_datatype_t *datatype,
                             struct ompi_message_t **message,
                             ompi_status_public_t* status)

{
    return pml_selected_module.pml_mrecv(buf, count, datatype,
                                         message, status);
}


