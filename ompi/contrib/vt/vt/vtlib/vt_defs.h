/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2010, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#ifndef _VT_DEFS_H
#define _VT_DEFS_H

#include <limits.h>
#include "vt_inttypes.h"

/* macro for one-step declaration and definition of functions */
#define VT_DECLDEF(function)  \
function; /* declaration */   \
function  /* definition */


#define VT_MAX_GETHOSTID_RETRIES 10

/*
 *-----------------------------------------------------------------------------
 * Buffer
 *-----------------------------------------------------------------------------
 */

#define VT_MIN_BUFSIZE    100*1024       /* 100KB */
#define VT_DEF_BUFSIZE    32*(1024*1024) /* 32MB */

typedef unsigned char* buffer_t;

#define VT_DEFAULT_COPY_BUFFER_SIZE 1048576  /* 1MB */

/*
 *-----------------------------------------------------------------------------
 * Trace statuses
 *-----------------------------------------------------------------------------
 */

#define VT_TRACE_ON             0
#define VT_TRACE_OFF            1
#define VT_TRACE_OFF_PERMANENT  2

/*
 *-----------------------------------------------------------------------------
 * Trace mode
 *-----------------------------------------------------------------------------
 */

#define VT_MODE_TRACE  1<<0
#define VT_MODE_STAT   1<<1

/*
 *-----------------------------------------------------------------------------
 * Statistics properties
 *-----------------------------------------------------------------------------
 */

#define VT_SUM_PROP_FUNC    1<<0
#define VT_SUM_PROP_MSG     1<<1
#define VT_SUM_PROP_COLLOP  1<<2
#define VT_SUM_PROP_FILEOP  1<<3

/*
 *-----------------------------------------------------------------------------
 * Message statistics details
 *-----------------------------------------------------------------------------
 */

#define VT_SUM_MSG_DTL_PEER  1<<0
#define VT_SUM_MSG_DTL_COMM  1<<1
#define VT_SUM_MSG_DTL_TAG   1<<2

/*
 *-----------------------------------------------------------------------------
 * Collop. statistics details
 *-----------------------------------------------------------------------------
 */

#define VT_SUM_COLLOP_DTL_OP    1<<0
#define VT_SUM_COLLOP_DTL_COMM  1<<1

/*
 *-----------------------------------------------------------------------------
 * Absent information
 *-----------------------------------------------------------------------------
 */

#define VT_NO_ID                  0xFFFFFFFF
#define VT_NO_LNO                 0xFFFFFFFF

#define VT_DEF_GROUP              "Application"
#define VT_MAX_COMMENT_LEN        4096
#define VT_MAX_MARKER_LEN         4096
#define VT_MAX_THREAD_NAME_LEN    100
#define VT_MAX_THREADS            65536

/*
 *-----------------------------------------------------------------------------
 * Internal regions
 *-----------------------------------------------------------------------------
 */

#define VT__TRC_USER              0
#define VT__TRC_SYNC              1
#define VT__TRC_SYNCTIME          2
#define VT__TRC_FLUSH             3
#define VT__TRC_STAT              4
#define VT__TRC_OFF               5
#define VT__TRC_OMPPREG           6
#define VT__TRC_REGID_NUM         7

/*
 *-----------------------------------------------------------------------------
 * Region type
 *-----------------------------------------------------------------------------
 */

#define VT_UNKNOWN                0

#define VT_INTERNAL               1
#define VT_FUNCTION               2
#define VT_LOOP                   3
#define VT_USER_REGION            4

#define VT_LIBC                   5
#define VT_LIBC_IO                6

#define VT_MEMORY                 7

#define VT_MPI_FUNCTION           8
#define VT_MPI_COLL_BARRIER       9
#define VT_MPI_COLL_ONE2ALL      10
#define VT_MPI_COLL_ALL2ONE      11
#define VT_MPI_COLL_ALL2ALL      12
#define VT_MPI_COLL_OTHER        13

#define VT_OMP_FUNCTION          14
#define VT_OMP_PARALLEL          15
#define VT_OMP_PARALLEL_REGION   16
#define VT_OMP_LOOP              17
#define VT_OMP_SECTIONS          18
#define VT_OMP_SECTION           19
#define VT_OMP_WORKSHARE         20
#define VT_OMP_SINGLE            21
#define VT_OMP_MASTER            22
#define VT_OMP_CRITICAL          23
#define VT_OMP_ATOMIC            24
#define VT_OMP_BARRIER           25
#define VT_OMP_IBARRIER          26
#define VT_OMP_FLUSH             27
#define VT_OMP_CRITICAL_SBLOCK   28
#define VT_OMP_SINGLE_SBLOCK     29

#define VT_PTHRD_FUNCTION        30

/*
 *-----------------------------------------------------------------------------
 * Performance metrics
 *-----------------------------------------------------------------------------
 */

#define VT_INTEGER                0
#define VT_FLOAT                  1

#define VT_COUNTER                0
#define VT_RATE                   1
#define VT_SAMPLE                 2

#define VT_START                  0
#define VT_LAST                   1
#define VT_NEXT                   2

/*
 *-----------------------------------------------------------------------------
 * I/O operations and flags
 *-----------------------------------------------------------------------------
 */

#define VT_IOOP_BITS            0x0000001f
#define VT_IOOP_OPEN                     0
#define VT_IOOP_CLOSE                    1
#define VT_IOOP_READ                     2
#define VT_IOOP_WRITE                    3
#define VT_IOOP_SEEK                     4
#define VT_IOOP_UNLINK                   5
#define VT_IOOP_RENAME                   6
#define VT_IOOP_DUP                      7
#define VT_IOOP_SYNC                     8
#define VT_IOOP_LOCK                     9
#define VT_IOOP_UNLOCK                  10
#define VT_IOOP_OTHER                   31
#define VT_IOFLAGS_BITS         0xffffffe0
#define VT_IOFLAG_IOFAILED              32
#define VT_IOFLAG_ASYNC                 64
#define VT_IOFLAG_COLL                 128
#define VT_IOFLAG_DIRECT               256
#define VT_IOFLAG_SYNC                 512
#define VT_IOFLAG_ISREADLOCK          1024

#endif /* _VT_DEFS_H */

