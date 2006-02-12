/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
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
/**
 * @file
 */
#ifndef ORTE_IOF_SVC_PROXY_H
#define ORTE_IOF_SVC_PROXY_H

#include "orte/mca/iof/iof.h"
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

#include "orte_config.h"
#include "orte/mca/iof/iof.h"
#include "orte/mca/iof/base/iof_base_header.h"


/**
 *  Callback function from OOB on receipt of IOF request.
 *
 *  @param status (IN)  Completion status.
 *  @param peer (IN)    Opaque name of peer process.
 *  @param msg (IN)     Array of iovecs describing user buffers and lengths.
 *  @param count (IN)   Number of elements in iovec array.
 *  @param tag (IN)     User defined tag for matching send/recv.
 *  @param cbdata (IN)  User data.
*/
                                                                                                               
void orte_iof_svc_proxy_recv(
    int status,
    orte_process_name_t* peer,
    struct iovec* msg,
    int count,
    orte_rml_tag_t tag,
    void* cbdata);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

