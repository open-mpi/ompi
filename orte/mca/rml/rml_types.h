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
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/** @file:
 *
 * Contains the typedefs for the use of the rml
 */

#ifndef MCA_RML_TYPES_H_
#define MCA_RML_TYPES_H_

#include "orte_config.h"

#include <limits.h>
#ifdef HAVE_SYS_UIO_H
/* for struct iovec */
#include <sys/uio.h>
#endif

#include "orte/orte_constants.h"
#include "orte/util/proc_info.h"

/**
 * Constant tag values for well-known services
 */

typedef uint32_t orte_rml_tag_t;
#define ORTE_RML_TAG    ORTE_UINT32

#define ORTE_RML_TAG_NS              1
#define ORTE_RML_TAG_GPR             2
#define ORTE_RML_TAG_GPR_NOTIFY      3
#define ORTE_RML_TAG_DAEMON          4
#define ORTE_RML_TAG_IOF_SVC         5
#define ORTE_RML_TAG_IOF_CLNT        6
#define ORTE_RML_TAG_XCAST           7
#define ORTE_RML_TAG_RMGR            8
#define ORTE_RML_TAG_PROBE           9
#define ORTE_RML_TAG_RDS            10
#define ORTE_RML_TAG_RAS            11
#define ORTE_RML_TAG_RMAPS          12
#define ORTE_RML_TAG_PLS            13
#define ORTE_RML_TAG_PLS_ORTED      14
#define ORTE_RML_TAG_PLS_ORTED_ACK  15
#define ORTE_RML_TAG_ERRMGR         16
#define ORTE_RML_TAG_BPROC          17
#define ORTE_RML_TAG_BPROC_ABORT    18
#define ORTE_RML_TAG_SM_BACK_FILE_CREATED    19
#define ORTE_RML_TAG_WIREUP         20

#define ORTE_RML_TAG_DYNAMIC     2000

#define ORTE_RML_TAG_MAX UINT32_MAX

                                                                                                                                        
/**
 * Flags to send/recv
 */

#define ORTE_RML_PEEK  0x01   /**< flag to oob_recv to allow caller to peek a portion of the next available
                               * message w/out removing the message from the queue.  */
#define ORTE_RML_TRUNC 0x02   /**< flag to oob_recv to return the actual size of the message even if
                               * the receive buffer is smaller than the number of bytes available */
#define ORTE_RML_ALLOC 0x04   /**< flag to oob_recv to request the oob to allocate a buffer of the appropriate
                               * size for the receive and return the allocated buffer and size in the first
                               * element of the iovec array. */
#define ORTE_RML_PERSISTENT 0x08 /**< posted non-blocking recv is persistent */
#define ORTE_RML_NON_PERSISTENT  0x00
                                                                                                                                        

#endif  /* RML_TYPES */
