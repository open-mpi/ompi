/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
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
#ifdef HAVE_NET_UIO_H
#include <net/uio.h>
#endif

#include "orte/orte_constants.h"
#include "orte/util/proc_info.h"


BEGIN_C_DECLS


/* ******************************************************************** */


/**
 * Constant tag values for well-known services
 */

#define ORTE_RML_TAG_T    ORTE_UINT32
#define ORTE_RML_TAG_MAX  UINT32_MAX

#define ORTE_RML_TAG_NS                      1
#define ORTE_RML_TAG_GPR                     2
#define ORTE_RML_TAG_GPR_NOTIFY              3
#define ORTE_RML_TAG_DAEMON                  4
#define ORTE_RML_TAG_IOF_SVC                 5
#define ORTE_RML_TAG_IOF_CLNT                6
#define ORTE_RML_TAG_XCAST_BARRIER           7
#define ORTE_RML_TAG_ORTED_ROUTED            8
#define ORTE_RML_TAG_RMGR                    9
#define ORTE_RML_TAG_PROBE                  10
#define ORTE_RML_TAG_RDS                    11
#define ORTE_RML_TAG_RAS                    12
#define ORTE_RML_TAG_RMAPS                  13
#define ORTE_RML_TAG_PLS                    14
#define ORTE_RML_TAG_ERRMGR                 15
#define ORTE_RML_TAG_BPROC                  16
#define ORTE_RML_TAG_BPROC_ABORT            17
#define ORTE_RML_TAG_SM_BACK_FILE_CREATED   18
#define ORTE_RML_TAG_WIREUP                 19
#define ORTE_RML_TAG_RML_INFO_UPDATE        20
#define ORTE_RML_TAG_ORTED_CALLBACK         21

#define ORTE_RML_TAG_FILEM                  22
#define ORTE_RML_TAG_CKPT                   23

#define ORTE_RML_TAG_RML_ROUTE              24

#define ORTE_RML_TAG_UDAPL                  25
#define ORTE_RML_TAG_OPENIB                 26
#define ORTE_RML_TAG_MVAPI                  27

#define ORTE_RML_TAG_COMM_CID_INTRA         28

#define ORTE_RML_TAG_ALLGATHER              29
#define ORTE_RML_TAG_ALLGATHER_LIST         30
#define ORTE_RML_TAG_BARRIER                31

#define ORTE_RML_TAG_INIT_ROUTES            32
#define ORTE_RML_TAG_UPDATE_ROUTES          33
#define ORTE_RML_TAG_SYNC                   34

/* For FileM RSH Component */
#define ORTE_RML_TAG_FILEM_RSH              35

/* For SnapC Full Component */
#define ORTE_RML_TAG_SNAPC_FULL             36

/* For CRCP Coord Component */
#define OMPI_CRCP_COORD_BOOKMARK_TAG      4242


#define ORTE_RML_TAG_DYNAMIC              2000


/** 
 * Message matching tag
 *
 * Message matching tag.  Unlike MPI, there is no wildcard receive,
 * all messages must match exactly. Tag values less than
 * ORTE_RML_TAG_DYNAMIC are reserved and may only be referenced using
 * a defined constant.
 */
typedef uint32_t orte_rml_tag_t;


/* ******************************************************************** */


/*
 * RML proxy commands
 */
typedef uint8_t orte_rml_cmd_flag_t;
#define ORTE_RML_CMD    ORTE_UINT8
#define ORTE_RML_UPDATE_CMD    1


/* ******************************************************************** */
/* Flags to send/recv */

/**
 * Non-persistent request that can be deleted when the request is
 * completed.  This is the default behavior.
 */
#define ORTE_RML_NON_PERSISTENT          0x00000000

/**
 * flag to oob_recv to allow caller to peek a portion of the next
 * available message w/out removing the message from the queue.
 */
#define ORTE_RML_PEEK                    0x00000001

/** 
 * flag to oob_recv to return the actual size of the message even if
 * the receive buffer is smaller than the number of bytes available 
 */
#define ORTE_RML_TRUNC                   0x00000002

/** 
 * flag to oob_recv to request the oob to allocate a buffer of the
 * appropriate size for the receive and return the allocated buffer
 * and size in the first element of the iovec array.
 */
#define ORTE_RML_ALLOC                   0x00000004

/**
 * posted non-blocking recv is persistent 
 */
#define ORTE_RML_PERSISTENT              0x00000008

/**
 * The request is a non-blocking request that can have its callback
 * triggered as soon as the request is completed, even if the OOB is
 * currently in the middle of another non-blocking request callback.
 */
#define ORTE_RML_FLAG_RECURSIVE_CALLBACK 0x00000010


typedef enum {
    ORTE_RML_PEER_UNREACH,
    ORTE_RML_PEER_DISCONNECTED
} orte_rml_exception_t;


END_C_DECLS


#endif  /* RML_TYPES */
