/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#include "ompi_config.h"
#include "pml_portals.h"
#include "ompi/request/request.h"
#include "ompi/datatype/datatype.h"
#include "ompi/communicator/communicator.h"
#include "ompi/datatype/convertor.h"
#include "pml_portals_datatype.h"

int
ompi_pml_portals_irecv_init(void *addr,
                           size_t count,
                           ompi_datatype_t * datatype,
                           int src,
                           int tag,
                           struct ompi_communicator_t *comm,
                           struct ompi_request_t **request)
{
    return OMPI_ERR_NOT_IMPLEMENTED;
}


int
ompi_pml_portals_irecv(void *addr,
                       size_t count,
                       ompi_datatype_t * datatype,
                       int src,
                       int tag,
                       struct ompi_communicator_t *comm,
                       struct ompi_request_t **request)
{
    return OMPI_ERR_NOT_IMPLEMENTED;
}


int
ompi_pml_portals_recv(void *buf,
                      size_t count,
                      ompi_datatype_t * datatype,
                      int src,
                      int tag,
                      struct ompi_communicator_t *comm,
                      ompi_status_public_t * status)
{
    ptl_process_id_t source;

    if (MPI_ANY_SOURCE == src) {
        source.nid = PTL_NID_ANY;
        source.pid = PTL_PID_ANY;
    } else {
        source = ((ompi_pml_portals_proc_t*) comm->c_pml_procs[src])->proc_id;
    }

    return OMPI_ERR_NOT_IMPLEMENTED;
}

