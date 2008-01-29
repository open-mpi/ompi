/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2008, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich GmbH, Federal
 * Republic of Germany
 *
 * See the file COPYRIGHT in the package base directory for details
 **/

#ifndef _VT_THRD_H
#define _VT_THRD_H

#ifdef __cplusplus
#   define EXTERN extern "C" 
#else
#   define EXTERN extern 
#endif

#include "vt_defs.h"
#include "vt_error.h"

#include "vt_otf_gen.h"

#if (defined (RFG))
#include "rfg.h"
#endif


/*
 *-----------------------------------------------------------------------------
 * VTThrd struct holds all thread-specific data:
 * - Trace buffer and file including file name
 * - Event sets and value vector
 *-----------------------------------------------------------------------------
 */

typedef struct 
{

  uint8_t is_trace_on;          /* flag: is tracing enabled? */

  VTGen* gen;                   /* trace file and buffer */
  char*  tmp_name;              /* base file name for temporary files */

  uint64_t omp_collop_stime;    /* last timestamp of OMP collop. begin event */

#if (defined (VT_MEMHOOK))

  uint64_t mem_app_alloc;       /* memory usage by application */

#endif

#if (defined (VT_METR)) 
  
  uint64_t*       valv;         /* vector of counter values */
  struct vt_metv* metv;         /* vector of metric objects 
				   (i.e., the event set) */
  
#endif

#if (defined (RFG))

  RFG_Regions* rfg_regions;

#endif

} VTThrd;


/* create thread object */
EXTERN VTThrd*  VTThrd_create( uint32_t tid );

/* free thread object */
EXTERN void     VTThrd_delete( VTThrd* thrd, uint32_t tid );

/* open associated trace file */
EXTERN void     VTThrd_open( VTThrd* thrd, uint32_t tid );

/* close associated trace file */
EXTERN void     VTThrd_close( VTThrd* thrd );

/* get total number of thread objects created */
EXTERN uint32_t VTThrd_get_num_thrds( void );


/* Accessor macros */

/* flag: is tracing enabled? */
#define VTTHRD_IS_TRACE_ON(thrd)         thrd->is_trace_on

/* trace file and buffer */
#define VTTHRD_GEN(thrd)                 thrd->gen

/* base name of the temporary files */
#define VTTHRD_TMP_NAME(thrd)            thrd->tmp_name

/* last timestamp of OMP collop. begin event */
#define VTTHRD_OMP_COLLOP_STIME(thrd)    thrd->omp_collop_stime

#if (defined (VT_MEMHOOK))

/* memory allocation by application */
#define VTTHRD_MEM_APP_ALLOC(thrd)       thrd->mem_app_alloc

#endif

#if (defined (VT_METR)) 

/* vector of metric values */
#define VTTHRD_VALV(thrd)                thrd->valv

/* vector of metric objects (i.e., event sets) */
#define VTTHRD_METV(thrd)                thrd->metv

#endif

#if (defined (RFG))

/* RFG regions */
#define VTTHRD_RFGREGIONS(thrd)          thrd->rfg_regions

#endif

#endif
