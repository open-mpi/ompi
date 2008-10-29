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

#ifndef _VT_TRC_H
#define _VT_TRC_H

#ifdef __cplusplus
#   define EXTERN extern "C" 
#else
#   define EXTERN extern 
#endif

#include "vt_defs.h"
#include "vt_error.h"

EXTERN void vt_open               ( void );

EXTERN void vt_close              ( void );

EXTERN void vt_trace_on           ( void );

EXTERN void vt_trace_off          ( uint8_t permanent );

EXTERN uint8_t vt_is_trace_on     ( void );

/*
  The next two functions have to be called immediately after
  initializing and immediately before finalizing the communication
  middle-ware, e.g. after MPI_Init() and before MPI_Finalize().  
*/

EXTERN void vt_mpi_init           ( void );
EXTERN void vt_mpi_finalize       ( void );

/*
  For non-MPI and non-OpenMP programs, the next function has to be
  called after vt_open() to register a unique id (0..num-1) for the
  calling process and the number of processes
*/

EXTERN void vt_init_trc_id        ( int my_id, int num_procs );

/*
 *-----------------------------------------------------------------------------
 * Definition records
 *-----------------------------------------------------------------------------
 */

/*
  The records needed for describing the machine are generated
  automatically.

  The control of hardware performance counters to be monitored is not
  supported by this interface. A way of controlling performance
  counters will be provided in a later version.  
*/
 
EXTERN void vt_def_comment           ( const char* comment );

EXTERN uint32_t vt_def_file          ( const char* fname );

EXTERN uint32_t vt_def_fileio_group  ( const char* gname );

EXTERN uint32_t vt_def_fileio        ( const char* fname,
                                       uint32_t gid );

EXTERN uint32_t vt_def_region        ( const char* rname,
				       uint32_t fid,
				       uint32_t begln,
				       uint32_t endln,
				       const char* rdesc,
				       uint8_t rtype );

EXTERN uint32_t vt_def_counter_group ( const char* gname );

EXTERN uint32_t vt_def_counter       (const char* cname,
				      uint32_t cprop,
				      uint32_t gid,
				      const char* cunit);

EXTERN void vt_def_mpi_comm          ( uint32_t cid,
				       uint32_t grpc,
				       uint8_t grpv[] );

/*
 *-----------------------------------------------------------------------------
 * Event records
 *-----------------------------------------------------------------------------
 */

/* for MPI, Process ids of message sources and destinations are mapped onto
   location ids by the merging process. */

/* -- Region -- */

EXTERN void vt_enter(uint64_t* time, uint32_t rid);

EXTERN void vt_exit(uint64_t* time);

/* -- File I/O -- */

EXTERN void vt_ioexit(uint64_t* time, uint64_t* etime, uint32_t fid,
       uint64_t hid, uint32_t op, uint64_t bytes);

/* -- Memory -- */

#if defined(VT_MEMHOOK)

EXTERN void vt_mem_alloc(uint64_t* time, uint64_t bytes);

EXTERN void vt_mem_free(uint64_t* time, uint64_t bytes);

#endif

/* -- Counter -- */

EXTERN void vt_count(uint64_t* time, uint32_t cid, uint64_t cval);

/* -- Comment -- */

EXTERN void vt_comment(uint64_t* time, const char* comment);

/* -- MPI-1 -- */

EXTERN void vt_mpi_send(uint64_t* time, uint32_t dpid, uint32_t cid,
       uint32_t tag, uint32_t sent);

EXTERN void vt_mpi_recv(uint64_t* time, uint32_t spid, uint32_t cid,
       uint32_t tag, uint32_t recvd);

EXTERN void vt_mpi_collexit(uint64_t* time, uint64_t* etime, uint32_t rid,
       uint32_t rpid, uint32_t cid, uint32_t sent, uint32_t recvd);

/* -- OpenMP -- */

EXTERN void vt_omp_fork(uint64_t* time);

EXTERN void vt_omp_join(uint64_t* time);

EXTERN void vt_omp_alock(uint64_t* time, uint32_t lkid);

EXTERN void vt_omp_rlock(uint64_t* time, uint32_t lkid);

EXTERN void vt_omp_collenter(uint64_t* time, uint32_t rid);

EXTERN void vt_omp_collexit(uint64_t* etime);

EXTERN void vt_omp_parallel_begin(void);

EXTERN void vt_omp_parallel_end(void);

/* -- VampirTrace Internal -- */

EXTERN void vt_enter_user(uint64_t* time);

EXTERN void vt_exit_user(uint64_t* time);

EXTERN void vt_enter_stat(uint64_t* time);

EXTERN void vt_exit_stat(uint64_t* time);

EXTERN void vt_enter_flush(uint64_t* time);

EXTERN void vt_exit_flush(uint64_t* time);

EXTERN uint8_t vt_is_alive;

#endif
