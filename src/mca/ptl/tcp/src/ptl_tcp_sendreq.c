/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include "include/ompi_socket_errno.h"
#include "include/types.h"
#include "mca/ptl/base/ptl_base_sendreq.h"
#include "ptl_tcp.h"
#include "ptl_tcp_sendreq.h"


static void mca_ptl_tcp_send_request_construct(mca_ptl_tcp_send_request_t*);
static void mca_ptl_tcp_send_request_destruct(mca_ptl_tcp_send_request_t*);
                                                                                                                

ompi_class_t  mca_ptl_tcp_send_request_t_class = {
    "mca_ptl_tcp_send_request_t",
    OBJ_CLASS(mca_ptl_base_send_request_t),
    (ompi_construct_t)mca_ptl_tcp_send_request_construct,
    (ompi_destruct_t)mca_ptl_tcp_send_request_destruct
};


void mca_ptl_tcp_send_request_construct(mca_ptl_tcp_send_request_t* request)
{
    OBJ_CONSTRUCT(&request->req_frag, mca_ptl_tcp_send_frag_t);
}


void mca_ptl_tcp_send_request_destruct(mca_ptl_tcp_send_request_t* request)
{
    OBJ_DESTRUCT(&request->req_frag);
}

