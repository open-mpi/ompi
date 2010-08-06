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

#ifndef _VT_UNIFY_H_
#define _VT_UNIFY_H_

#include "config.h"

#ifdef VT_MPI
#  include "vt_unify_mpi.h"
#endif // VT_MPI

#ifdef VT_ETIMESYNC
#  include "vt_unify_sync.h"
#endif // VT_ETIMESYNC

#include "vt_inttypes.h"

#include <map>
#include <string>
#include <vector>

#if defined(HAVE_OMP) && HAVE_OMP
#  include <omp.h>
#endif // HAVE_OMP

#define STRBUFSIZE 1024

#if SIZEOF_LONG == 4
#  define ATOL8 atoll
#else
#  define ATOL8 atol
#endif

#ifdef VT_MPI
#  define MASTER if( g_iMPIRank == 0 )
#else // VT_MPI
#  define MASTER
#endif // VT_MPI

//
// unify parameter structure (contains the program options)
//
struct Params_struct
{
   Params_struct() 
      : in_file_prefix(""), out_file_prefix(""), stats_out_file(""),
   uctl_files_num(0), verbose_level(0), stats_sort_flags(0), docompress(true),
   doclean(true), showusage(false), showprogress(false), bequiet(false) {}

   std::string in_file_prefix;
   std::string out_file_prefix;
   std::string stats_out_file;
   uint32_t    uctl_files_num;
   uint8_t     verbose_level;
   int         stats_sort_flags;
   bool        docompress;
   bool        doclean;
   bool        showusage;
   bool        showprogress;
   bool        bequiet;
};

//
// unify control structure for each file stream
//
struct UnifyControl_struct
{
#ifndef VT_ETIMESYNC
   UnifyControl_struct()
      : streamid(0)
      {
	 ltime[0] = ltime[1] = offset[0] = offset[1] = 0;
      }
   UnifyControl_struct(uint32_t _streamid,
		       int64_t * _ltime, int64_t * _offset)
      : streamid(_streamid)
      {
	 ltime[0] = _ltime[0]; ltime[1] = _ltime[1];
	 offset[0] = _offset[0]; offset[1] = _offset[1];
      }
#else // VT_ETIMESYNC
   UnifyControl_struct()
      : streamid(0), sync_offset(0), sync_drift(0.0),
   p_vec_sync_phases(0), p_vec_sync_times(0), p_vec_sync_pairs(0)
      {
	 ltime[0] = ltime[1] = offset[0] = offset[1] = 0;
      }
   UnifyControl_struct(uint32_t _streamid,
		       int64_t * _ltime, int64_t * _offset,
		       std::vector<Synchronization::SyncPhase_struct> * _p_vec_sync_phases,
		       std::vector<Synchronization::SyncTime_struct> * _p_vec_sync_times,
		       std::vector<std::pair<uint32_t, uint32_t> > * _p_vec_sync_pairs)
      : streamid(_streamid), sync_offset(0), sync_drift(1.0),
   p_vec_sync_phases(_p_vec_sync_phases), p_vec_sync_times(_p_vec_sync_times),
   p_vec_sync_pairs(_p_vec_sync_pairs)
      {
	 ltime[0] = _ltime[0]; ltime[1] = _ltime[1];
	 offset[0] = _offset[0]; offset[1] = _offset[1];
      }
#endif // VT_ETIMESYNC

   uint32_t    streamid;   // id of input stream
   int64_t     ltime[2];   // local times ...
   int64_t     offset[2];  // ... and chronological offsets to global time
#ifdef VT_ETIMESYNC
   uint64_t    sync_offset;
   double      sync_drift;
   std::vector<Synchronization::SyncPhase_struct> * p_vec_sync_phases;
   std::vector<Synchronization::SyncTime_struct> * p_vec_sync_times;
   std::vector<std::pair<uint32_t, uint32_t> > * p_vec_sync_pairs;
#endif // VT_ETIMESYNC
};

void VPrint( uint8_t level, const char * fmt, ... );

void PVPrint( uint8_t level, const char * fmt, ... );

uint64_t CorrectTime( uint32_t loccpuid, uint64_t time );

extern const std::string ExeName;
extern const std::string TmpFileSuffix;
extern const std::string UniFilePrefix;

#ifdef VT_MPI
   extern VT_MPI_INT g_iMPISize;
   extern VT_MPI_INT g_iMPIRank;
#endif // VT_MPI

#ifdef VT_ETIMESYNC
   extern bool HaveETimeSync;
#endif // VT_ETIMESYNC

// unify parameters
extern struct Params_struct Params;

// minimal start time
extern uint64_t g_uMinStartTime;

// minimal start time (usec since Epoch)
extern uint64_t g_uMinStartTimeEpoch;

// maximum stop time (usec since Epoch)
extern uint64_t g_uMaxStopTimeEpoch;

// unify control vector
extern std::vector<UnifyControl_struct*> g_vecUnifyCtls;

// map stream id <-> unify control index
extern std::map<uint32_t, uint32_t> g_mapStreamIdUnifyCtlIdx;

#endif // _VT_UNIFY_H_
