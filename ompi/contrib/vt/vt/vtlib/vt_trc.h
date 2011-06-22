/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2011, ZIH, TU Dresden, Federal Republic of Germany
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

#include "vt_inttypes.h"

#include <stdlib.h>

/* id indices for internal regions */
#define VT__TRC_USER       0
#define VT__TRC_SYNC       1
#define VT__TRC_SYNCTIME   2
#define VT__TRC_FLUSH      3
#define VT__TRC_STAT       4
#define VT__TRC_OFF        5
#define VT__TRC_REWIND     6
#define VT__TRC_OMPPREG    7
#define VT__TRC_REGID_NUM  8

/* id indices for internal markers */
#define VT__TRC_MARKER_ERROR    0
#define VT__TRC_MARKER_WARNING  1
#define VT__TRC_MARKER_HINT     2

/**
 * Description
 */
EXTERN void vt_open(void);

/**
 * Description
 */
EXTERN void vt_reset(void);
/**
 * Description
 *
 * @param signum  signal number
 */
EXTERN void vt_close_by_signal(int signum);

/**
 * Description
 */
EXTERN void vt_close(void);

/**
 * Description
 *
 * @param tid   thread id
 * @param mark  flag: mark trace status as function enter/exit?
 *              (0 - no, 1 - yes)
 */
EXTERN void vt_trace_on(uint32_t tid, uint8_t mark);

/**
 * Description
 *
 * @param tid        thread id
 * @param mark       flag: mark trace status as function enter/exit?
 *                   (0 - no, 1 - yes)
 * @param permanent  flag: trace switched off permanently?
 *                   (e.g. if max. buffer flushes reached)
 *                   (0 - no, 1 - yes)
 */
EXTERN void vt_trace_off(uint32_t tid, uint8_t mark, uint8_t permanent);

/**
 * Description
 *
 * @param tid  thread id
 *
 * @return     current trace status
 *             (0 - off, 1 - on)
 */
EXTERN uint8_t vt_is_trace_on(uint32_t tid);

/**
 * Description
 *
 * @param tid   thread id
 * @param size  buffer size to be guaranteed
 */
EXTERN void vt_guarantee_buffer(uint32_t tid, size_t size);

/**
 * Description
 *
 * @param tid  thread id
 */
EXTERN void vt_buffer_flush(uint32_t tid);

/**
 * Description
 *
 * @param tid   thread id
 * @param time  timestamp
 */
EXTERN void vt_update_counter(uint32_t tid, uint64_t* time);

/**
 * Description
 * This function have to be called immediately after initializing the
 * communication middle-ware, e.g. atfer MPI_Init().
 */
EXTERN void vt_mpi_init(void);

/**
 * Description
 * This function have to be called immediately before finalizing the
 * communication middle-ware, e.g. before MPI_Finalize().
 */
EXTERN void vt_mpi_finalize(void);

/**
 * Description
 *
 * @param tid   thread id
 * @param time  timestamp
 * @param comm  MPI communicator
 */
EXTERN void vt_mpi_sync(uint32_t tid, uint64_t* time, void* comm);

/**
 * Retrieve and increment current id counter.
 *
 * @return  new id
 */
EXTERN uint32_t vt_get_curid(void);

/*
 *-----------------------------------------------------------------------------
 * Definition records
 *-----------------------------------------------------------------------------
 */

/**
 * Description
 *
 * @param tid  thread id
 * @param fmt  comment as format string like printf
 */
EXTERN void vt_def_comment(uint32_t tid, const char* fmt, ...);

/**
 * Description
 *
 * @param tid    thread id
 * @param fname  source file name
 *
 * @return       source file id
 */
EXTERN uint32_t vt_def_scl_file(uint32_t tid, const char* fname);

/**
 * Description
 *
 * @param tid    thread id
 * @param fid    source file id (created by vt_def_scl_file)
 * @param begln  begin line number
 * @param endln  end line number
 * @param fname  source file name
 *
 * @return       source code location id
 */
EXTERN uint32_t vt_def_scl(uint32_t tid, uint32_t fid, uint32_t begln,
                           uint32_t endln);

/**
 * Description
 *
 * @param tid    thread id
 * @param gname  file group name
 *
 * @return       file group id
 */
EXTERN uint32_t vt_def_file_group(uint32_t tid, const char* gname);

/**
 * Description
 *
 * @param tid    thread id
 * @param fname  file name
 * @param gid    file group id (created by vt_def_file_group)
 *
 * @return       file id
 */
EXTERN uint32_t vt_def_file(uint32_t tid, const char* fname, uint32_t gid);

/**
 * Description
 *
 * @param tid    thread id
 * @param gname  region group name
 *
 * @return       region group id
 */
EXTERN uint32_t vt_def_region_group(uint32_t tid, const char* gname);

/**
 * Description
 *
 * @param tid     thread id
 * @param rname   region name
 * @param fid     source file id (created by vt_def_scl_file)
 * @param begln   begin line number
 * @param endln   end line number
 * @param rgroup  region group name
 * @param rtype   region type
 *
 * @return        region id
 */
EXTERN uint32_t vt_def_region(uint32_t tid, const char* rname, uint32_t fid,
                              uint32_t begln, uint32_t endln,
                              const char* rdesc, uint8_t rtype);

/**
 * Description
 *
 * @param tid    thread id
 * @param gname  counter group name
 *
 * @return       counter group id
 */
EXTERN uint32_t vt_def_counter_group(uint32_t tid, const char* gname);

/**
 * Description
 *
 * @param tid    thread id
 * @param cname  counter name
 * @param cprop  counter properties
 * @param cgid   counter group id (created by vt_def_counter_group)
 * @param cunit  counter unit
 *
 * @return       counter id
 */
EXTERN uint32_t vt_def_counter(uint32_t tid, const char* cname, uint32_t cprop,
                               uint32_t gid, const char* cunit);

/**
 * Description
 *
 * @param tid    thread id
 * @param mname  marker name
 * @param mtype  marker type
 *
 * @return       marker id
 */
EXTERN uint32_t vt_def_marker(uint32_t tid, const char* mname,
                              uint32_t mtype);

/**
 * Defines a marker with the given name.
 *
 * @param grpc number of GPU thread ids the array contains
 * @param grpv array of GPU thread ids
 * @param name name of process/thread group to be identified in vt_unify
 * @param cid the communictor id for this process/thread group
 */
EXTERN void vt_def_gpu_comm(uint32_t grpc, uint32_t grpv[], const char* name,
                            uint32_t cid);

/**
 * Description
 *
 * @param tid    thread id
 * @param ctype  MPI communicator type
 *               (VT_MPI_COMM_WORLD, VT_MPI_COMM_SELF, or VT_MPI_COMM_OTHER)
 * @param grpc   number of members
 * @param grpv   bit-vector of members
 *
 * @return       MPI communicator id
 */
EXTERN uint32_t vt_def_mpi_comm(uint32_t tid, uint8_t ctype, uint32_t grpc,
                                uint8_t grpv[]);

/**
 * Description
 *
 * @param tid    thread id
 * @param cname  communicator name
 *
 * @return       User communicator id
 */
EXTERN uint32_t vt_def_user_comm(uint32_t tid, const char* cname);

/**
 * Description
 *
 * @param tid    thread id
 * @param vtype  value type
 * @param kname  key name
 *
 * @return       key id
 */
EXTERN uint32_t vt_def_keyval(uint32_t tid, uint8_t vtype, const char* kname);

/**
 * Description
 *
 * @param tid    thread id
 * @param sname  unique async. source name
 *
 * @return       async. source key id
 */
EXTERN uint32_t vt_def_async_source(uint32_t tid, const char* sname);

/*
 *-----------------------------------------------------------------------------
 * Event records
 *-----------------------------------------------------------------------------
 */

/* -- Region -- */

/**
 * Description
 *
 * @param tid   thread id
 * @param time  timestamp
 * @param rid   region id (created by vt_def_region)
 *
 * @return      flag: enter was recorded?
 *              (0 - no, 1 - yes)
 */
EXTERN uint8_t vt_enter(uint32_t tid, uint64_t* time, uint32_t rid);

/**
 * Description
 *
 * @param tid   thread id
 * @param time  timestamp
 */
EXTERN void vt_exit(uint32_t tid, uint64_t* time);

/* -- File I/O -- */

/**
 * DEPRECATED
 * Description
 *
 * @param tid    thread id
 * @param time   begin timestamp
 * @param etime  end timestamp
 * @param fid    file id (created by vt_def_file)
 * @param hid    handle id
 * @param op     file operation
 * @param bytes  read/wrote bytes
 */
EXTERN void vt_ioexit(uint32_t tid, uint64_t* time, uint64_t* etime,
                      uint32_t fid, uint64_t hid, uint32_t op, uint64_t bytes );

/**
 * Description
 *
 * @param tid   thread id
 * @param time  timestamp
 * @param mid   matching id
 */
EXTERN void vt_iobegin( uint32_t tid, uint64_t* time, uint64_t mid );

/**
 * Description
 *
 * @param tid    thread id
 * @param time   timestamp
 * @param fid    file id (created by vt_def_file)
 * @param mid    matching id
 * @param hid    handle id
 * @param op     file operation
 * @param bytes  read/wrote bytes
 */
EXTERN void vt_ioend(uint32_t tid, uint64_t* time, uint32_t fid, uint64_t mid,
                     uint64_t hid, uint32_t op, uint64_t bytes);

/* -- Counter -- */

/**
 * Description
 *
 * @param tid   thread id
 * @param time  timestamp
 * @param hid   counter id (created by vt_def_counter)
 * @param cval  counter value
 */
EXTERN void vt_count(uint32_t tid, uint64_t* time, uint32_t cid,
                     uint64_t cval);

/* -- Comment -- */

/**
 * Description
 *
 * @param tid   thread id
 * @param time  timestamp
 * @param fmt   comment as format string like printf
 */
EXTERN void vt_comment(uint32_t tid, uint64_t* time, const char* fmt, ... );

/* -- Rewind -- */

/**
 * Description
 *
 * @param tid   thread id
 * @param time  timestamp
 */
EXTERN void vt_rewind(uint32_t tid, uint64_t* time);

/**
 * Description
 *
 * @param tid   thread id
 * @param time  timestamp
 */
EXTERN void vt_set_rewind_mark(uint32_t tid, uint64_t* time);

/* -- Marker -- */

/**
 * Description
 *
 * @param tid   thread id
 * @param time  timestamp
 * @param mid   marker id (created by vt_def_marker)
 * @param fmt   marker text as format string like printf
 */
EXTERN void vt_marker(uint32_t tid, uint64_t* time, uint32_t mid,
                      const char* fmt, ...);

/**
 * Description
 *
 * @param tid   thread id
 * @param time  timestamp
 * @param fmt   marker text as format string like printf
 */
EXTERN void vt_marker_error(uint32_t tid, uint64_t* time,
                            const char* fmt, ...);

/**
 * Description
 *
 * @param tid   thread id
 * @param time  timestamp
 * @param fmt   marker text as format string like printf
 */
EXTERN void vt_marker_warning(uint32_t tid, uint64_t* time,
                              const char* fmt, ...);

/**
 * Description
 *
 * @param tid   thread id
 * @param time  timestamp
 * @param fmt   marker text as format string like printf
 */
EXTERN void vt_marker_hint(uint32_t tid, uint64_t* time,
                           const char* fmt, ...);

/* -- Key-Value -- */

/**
 * Description
 *
 * @param tid    thread id
 * @param time   timestamp
 * @param kid    key id
 * @param vtype  value type
 * @param kval   value
 */
EXTERN void vt_keyval(uint32_t tid, uint32_t kid, uint8_t vtype, void* kvalue);

/**
 * Description
 *
 * @param tid   thread id
 * @param kid   async. source key id
 * @param time  actual time of next async. event
 */
EXTERN void vt_next_async_time(uint32_t tid, uint32_t kid, uint64_t atime);

/* -- MPI-1 -- */

/**
 * Description
 *
 * @param tid   thread id
 * @param time  timestamp
 * @param dpid  destination process id
 * @param cid   communicator id (created by vt_def_mpi_comm)
 * @param tag   message tag
 * @param sent  sent bytes
 */
EXTERN void vt_mpi_send(uint32_t tid, uint64_t* time, uint32_t dpid,
                        uint32_t cid, uint32_t tag, uint32_t sent);

/**
 * Description
 *
 * @param tid    thread id
 * @param time   timestamp
 * @param spid   source process id
 * @param cid    communicator id (created by vt_def_mpi_comm)
 * @param tag    message tag
 * @param recvd  received bytes
 */
EXTERN void vt_mpi_recv(uint32_t tid, uint64_t* time, uint32_t spid,
                        uint32_t cid, uint32_t tag, uint32_t recvd);


/**
 * DEPRECATED
 * Description
 *
 * @param tid    thread id
 * @param time   timestamp
 * @param etime  end timestamp
 * @param rid    region id of coll. operation (created by vt_def_region)
 * @param rpid   root process id
 * @param cid    communicator id (created by vt_def_mpi_comm)
 * @param comm   MPI communicator
 * @param sent   sent bytes
 * @param recvd  received bytes
 */
EXTERN void vt_mpi_collexit(uint32_t tid, uint64_t* time, uint64_t* etime,
                            uint32_t rid, uint32_t rpid, uint32_t cid,
                            void* comm, uint32_t sent, uint32_t recvd);

/**
 * Description
 *
 * @param tid    thread id
 * @param time   timestamp
 * @param rid    region id of coll. operation (created by vt_def_region)
 * @param mid    matching id
 * @param rpid   root process id
 * @param cid    communicator id (created by vt_def_mpi_comm)
 * @param sent   sent bytes
 * @param recvd  received bytes
 */
EXTERN void vt_mpi_collbegin(uint32_t tid, uint64_t* time, uint32_t rid,
                             uint64_t mid, uint32_t rpid, uint32_t cid,
                             uint64_t sent, uint64_t recvd);

/**
 * Description
 *
 * @param tid    thread id
 * @param time   timestamp
 * @param mid    matching id
 * @param comm   MPI communicator
 */
EXTERN void vt_mpi_collend(uint32_t tid, uint64_t* time, uint64_t mid,
                           void* comm);

/* -- MPI2 - 1sided -- */

/**
 * Description
 *
 * @param tid   thread id
 * @param time  timestamp
 * @param tpid  process id
 * @param cid   communicator id (created by vt_def_mpi_comm)
 * @param tag   message tag
 * @param sent  sent bytes
 */
EXTERN void vt_mpi_rma_put(uint32_t tid, uint64_t* time, uint32_t tpid,
                           uint32_t cid, uint32_t tag, uint64_t sent);

/**
 * Description
 *
 * @param tid   thread id
 * @param time  timestamp
 * @param tpid  process id
 * @param cid   communicator id (created by vt_def_mpi_comm)
 * @param tag   message tag
 * @param sent  sent bytes
 */
EXTERN void vt_mpi_rma_putre(uint32_t tid, uint64_t* time, uint32_t tpid,
                             uint32_t cid, uint32_t tag, uint64_t sent);

/**
 * Description
 *
 * @param tid    thread id
 * @param time   timestamp
 * @param tpid   process id
 * @param cid    communicator id (created by vt_def_mpi_comm)
 * @param tag    message tag
 * @param recvd  received bytes
 */
EXTERN void vt_mpi_rma_get(uint32_t tid, uint64_t* time, uint32_t tpid,
                           uint32_t cid, uint32_t tag, uint64_t recvd);

/**
 * Description
 *
 * @param tid   thread id
 * @param time  timestamp
 * @param cid   communicator id (created by vt_def_mpi_comm)
 * @param tag   message tag
 */
EXTERN void vt_mpi_rma_end(uint32_t tid, uint64_t* time, uint32_t cid,
                           uint32_t tag);

/* -- OpenMP -- */

/**
 * Description
 *
 * @param tid  thread id
 */
EXTERN void vt_omp_fork(uint32_t tid);

/**
 * Description
 *
 * @param tid   thread id
 * @param ptid  parent thread id
 */
EXTERN void vt_omp_fork2(uint32_t tid, uint32_t* ptid);

/**
 * Description
 *
 * @param tid  thread id
 */
EXTERN void vt_omp_join(uint32_t tid);

/**
 * Description
 *
 * @param tid  thread id
 */
EXTERN void vt_omp_parallel_begin(uint32_t tid);

/**
 * Description
 *
 * @param tid   thread id
 * @param ptid  parent thread id
 */
EXTERN void vt_omp_parallel_begin2(uint32_t tid, uint32_t ptid);

/**
 * Description
 *
 * @param tid  thread id
 */
EXTERN void vt_omp_parallel_end(uint32_t tid);

/* -- User Point-to-Point Communication -- */

/**
 *  Description
 *
 *  @param tid   thread id
 *  @param time  timestamp
 *  @param cid   communicator id (created by vt_def_user_comm)
 *  @param tag   message tag (has to be unique per communication pair)
 *  @param sent  sent bytes
 *
 */

EXTERN void vt_user_send(uint32_t tid, uint64_t* time, uint32_t cid,
                         uint32_t tag, uint32_t sent);

/**
 *  Description
 *
 *  @param tid   thread id
 *  @param time  timestamp
 *  @param cid   communicator id (created by vt_def_user_comm)
 *  @param tag   message tag (has to be unique per communication pair)
 *  @param sent  received bytes
 *
 */

EXTERN void vt_user_recv(uint32_t tid, uint64_t* time, uint32_t cid,
                         uint32_t tag, uint32_t recvd);

/* -- VampirTrace Internal -- */

/**
 * Description
 *
 * @param tid   thread id
 * @param time  timestamp
 */
EXTERN void vt_enter_user(uint32_t tid, uint64_t* time);

/**
 * Description
 *
 * @param tid   thread id
 * @param time  timestamp
 */
EXTERN void vt_exit_user(uint32_t tid, uint64_t* time);

/**
 * Description
 *
 * @param tid   thread id
 * @param time  timestamp
 */
EXTERN void vt_enter_stat(uint32_t tid, uint64_t* time);

/**
 * Description
 *
 * @param tid   thread id
 * @param time  timestamp
 */
EXTERN void vt_exit_stat(uint32_t tid, uint64_t* time);

/**
 * Description
 *
 * @param tid   thread id
 * @param time  timestamp
 */
EXTERN void vt_enter_flush(uint32_t tid, uint64_t* time);

/**
 * Description
 *
 * @param tid   thread id
 * @param time  timestamp
 */
EXTERN void vt_exit_flush(uint32_t tid, uint64_t* time);

/**
 * Description
 *
 * @param tid   thread id
 * @param time  timestamp
 */
EXTERN void vt_enter_rewind(uint32_t tid, uint64_t* time);

/**
 * Description
 *
 * @param tid   thread id
 * @param time  timestamp
 */
EXTERN void vt_exit_rewind(uint32_t tid, uint64_t* time);

/*
 *-----------------------------------------------------------------------------
 * Global variables
 *-----------------------------------------------------------------------------
 */

EXTERN int vt_num_traces; /** number of processes */
EXTERN int vt_my_trace;   /** current process id (i.e. MPI-rank) */
EXTERN int vt_my_ptrace;  /** parent process id */
EXTERN uint8_t vt_my_trace_is_master; /** 1st process on local node? */
EXTERN uint8_t vt_my_trace_is_disabled; /** process disabled? */
EXTERN uint8_t vt_my_trace_is_first_avail; /** 1st not disabled process? */
EXTERN int vt_my_funique; /** unique file id */

/** array of indices for internal regions */
EXTERN uint32_t vt_trc_regid[VT__TRC_REGID_NUM];

/** array of induces for internal markers (error, warnings, hints) */
EXTERN uint32_t vt_trc_mid[3];

/** counter group id for miscellaneous counters (e.g. cpu id) */
EXTERN uint32_t vt_misc_cgid;

/** flag: indicates whether VampirTrace is initialized and ready to trace */
EXTERN uint8_t vt_is_alive;

/** flag: indicates whether VampirTrace shall be closed if MPI_Finalize is
          called */
EXTERN uint8_t vt_close_on_mpi_finalize;

/** start time (set during vt_open() with vt_pform_wtime() */
EXTERN uint64_t vt_start_time;

/** start time (µs after 00:00:00 UTC 1 January 1970) */
EXTERN uint64_t vt_start_time_epoch;

#endif /* _VT_TRC_H */
