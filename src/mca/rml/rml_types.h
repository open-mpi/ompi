/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
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
#include "include/orte_constants.h"
#include "util/proc_info.h"
#include <limits.h>

/**
 * Constant tag values for well-known services
 */

typedef uint32_t orte_rml_tag_t;

#define ORTE_RML_TAG_NS          1
#define ORTE_RML_TAG_GPR         2
#define ORTE_RML_TAG_GPR_NOTIFY  3
#define ORTE_RML_TAG_DAEMON      4
#define ORTE_RML_TAG_IOF_SVC     5
#define ORTE_RML_TAG_IOF_CLT     6
#define ORTE_RML_TAG_XCAST       7
#define ORTE_RML_TAG_BPROC_SVC   8
#define ORTE_RML_TAG_BPROC_CLT   9
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
                                                                                                                                        
/**
 * The wildcard for receives from any peer.
 */
#define ORTE_RML_NAME_ANY  &orte_rml_name_any

/**
 * Process name of seed
 */
#define ORTE_RML_NAME_SEED &orte_rml_name_seed

/**
 * Process name of self
 */
#define ORTE_RML_NAME_SELF orte_process_info.my_name


#endif  /* RML_TYPES */
