/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2008, ZIH, TU Dresden, Federal Republic of Germany
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

/*
 *-----------------------------------------------------------------------------
 * Buffer
 *-----------------------------------------------------------------------------
 */

#define VT_MIN_BUFSIZE    100000
#define VT_DEF_BUFSIZE  32000000

typedef unsigned char* buffer_t;

/*
 *-----------------------------------------------------------------------------
 * Absent information
 *-----------------------------------------------------------------------------
 */

#define VT_NO_ID                  0xFFFFFFFF
#define VT_NO_LNO                 0xFFFFFFFF

#define VT_DEF_GROUP              "Application"

/*
 *-----------------------------------------------------------------------------
 * Region type
 *-----------------------------------------------------------------------------
 */

#define VT_UNKNOWN                0

#define VT_FUNCTION               1
#define VT_LOOP                   2
#define VT_USER_REGION            3

#define VT_FUNCTION_COLL_BARRIER  4
#define VT_FUNCTION_COLL_ONE2ALL  5
#define VT_FUNCTION_COLL_ALL2ONE  6
#define VT_FUNCTION_COLL_ALL2ALL  7
#define VT_FUNCTION_COLL_OTHER    8

#define VT_OMP_PARALLEL          11
#define VT_OMP_LOOP              12
#define VT_OMP_SECTIONS          13
#define VT_OMP_SECTION           14
#define VT_OMP_WORKSHARE         15
#define VT_OMP_SINGLE            16
#define VT_OMP_MASTER            17
#define VT_OMP_CRITICAL          18
#define VT_OMP_ATOMIC            19
#define VT_OMP_BARRIER           20
#define VT_OMP_IBARRIER          21
#define VT_OMP_FLUSH             22
#define VT_OMP_CRITICAL_SBLOCK   23
#define VT_OMP_SINGLE_SBLOCK     24

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

#endif

