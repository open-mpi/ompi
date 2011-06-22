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

#ifndef _VT_UNIFY_H_
#define _VT_UNIFY_H_

#include "config.h"

#ifdef VT_MPI
#  include "vt_unify_mpi.h"
#endif // VT_MPI

#ifdef VT_ETIMESYNC
#  include "vt_unify_esync.h"
#endif // VT_ETIMESYNC

#include "vt_defs.h"
#include "vt_inttypes.h"

#include <map>
#include <string>
#include <vector>

#include <assert.h>

#if defined(HAVE_OMP) && HAVE_OMP
#  ifdef VT_LIB
#     undef HAVE_OMP
   // disable OpenMP on NEC SX platforms to work around a compiler error
#  elif defined(_SX)
#     undef HAVE_OMP
#  else // VT_LIB || _SX
#     include <omp.h>
#  endif // VT_LIB
#endif // HAVE_OMP

#define STRBUFSIZE 1024

// define to prefix each verbose message with a timestamp
//#define TIME_VERBOSE

#ifdef VT_MPI
#  define MASTER if( MyRank == 0 )
#  define SLAVE  if( MyRank != 0 )
   // define to synchronize the error indicator between all ranks
//#  define SYNC_ERROR
#else // VT_MPI
#  define MASTER
#endif // VT_MPI

//
// unify parameter structure (contains the program options)
//
struct ParamsS
{
   ParamsS()
      : verbose_level( 0 ), docompress( true ), doclean( true ),
        showusage( false ), showversion( false ), showprogress( false ),
        bequiet( false ), domsgmatch( true ), droprecvs( false ),
        prof_sort_flags( 0x22 ) {}

   std::string in_file_prefix;  // input trace file prefix
   std::string out_file_prefix; // output trace file prefix
   uint8_t     verbose_level;   // verbose level
   bool        docompress;      // flag: do compress output trace?
   bool        doclean;         // flag: do remove local trace?
   bool        showusage;       // flag: show usage text?
   bool        showversion;     // flag: show VampirTrace version?
   bool        showprogress;    // flag: show progress?
   bool        bequiet;         // flag: print no messages?

   // HooksMsgMatchC's parameters
   //
   bool        domsgmatch;      // flag: do match messages?
   bool        droprecvs;       // flag: drop receive messages, if matching?

   // HooksProfC's parameters
   //
   std::string prof_out_file;   // profile output file
   int         prof_sort_flags; // profile sort flags

};

//
// unify control structure for each input stream
//
struct UnifyControlS
{
#ifndef VT_ETIMESYNC
   UnifyControlS()
      : streamid( 0 ), pstreamid( 0 ), stream_avail( true )
   {
      ltime[0] = ltime[1] = offset[0] = offset[1] = 0;
   }

   UnifyControlS( const uint32_t & _streamid, const uint32_t & _pstreamid,
                  const bool & _stream_avail, const int64_t * _ltime,
                  const int64_t * _offset )
      : streamid( _streamid ), pstreamid( _pstreamid ),
        stream_avail( _stream_avail )
   {
      ltime[0] = _ltime[0]; ltime[1] = _ltime[1];
      offset[0] = _offset[0]; offset[1] = _offset[1];
   }
#else // VT_ETIMESYNC
   UnifyControlS()
      : streamid( 0 ), pstreamid( 0 ), stream_avail( true ), sync_offset( 0 ),
        sync_drift( 0.0 )
   {
      ltime[0] = ltime[1] = offset[0] = offset[1] = 0;
   }

   UnifyControlS( const uint32_t & _streamid, const uint32_t & _pstreamid,
                  const bool & _stream_avail, const int64_t * _ltime,
                  const int64_t * _offset,
                  const std::vector<ETimeSyncC::SyncPhaseS> & _sync_phases,
                  const std::vector<ETimeSyncC::SyncTimeS> & _sync_times,
                  const std::vector<std::pair<uint32_t, uint32_t> > & _sync_pairs)
      : streamid( _streamid ), pstreamid( _pstreamid ),
        stream_avail( _stream_avail ), sync_offset( 0 ), sync_drift( 1.0 ),
        sync_phases( _sync_phases ), sync_times( _sync_times ),
        sync_pairs( _sync_pairs )
   {
      ltime[0] = _ltime[0]; ltime[1] = _ltime[1];
      offset[0] = _offset[0]; offset[1] = _offset[1];
   }
#endif // VT_ETIMESYNC

   uint32_t    streamid;     // id of input stream
   uint32_t    pstreamid;    // id of parent input stream
   bool        stream_avail; // is stream available?

   // time sync. information
   //
   int64_t     ltime[2];     // local times ...
   int64_t     offset[2];    // ... and chronological offsets to global time
#ifdef VT_ETIMESYNC
   uint64_t    sync_offset;
   double      sync_drift;
   std::vector<ETimeSyncC::SyncPhaseS>         sync_phases;
   std::vector<ETimeSyncC::SyncTimeS>          sync_times;
   std::vector<std::pair<uint32_t, uint32_t> > sync_pairs;
#endif // VT_ETIMESYNC

};

// print verbose message
void VPrint( uint8_t level, const char * fmt, ... );

// print verbose message in a parallel region
void PVPrint( uint8_t level, const char * fmt, ... );

// synchronize error indicator between all ranks
bool SyncError( bool * error );

// global variables
//

// name of program's executable
extern const std::string ExeName;

// temporary output file suffix
extern const std::string TmpFileSuffix;

// output file prefix which used if local input files shall be kept
extern const std::string UniFilePrefix;

// unify parameters
extern ParamsS Params;

// vector of unify controls
extern std::vector<UnifyControlS*> UnifyCtls;

// map stream id <-> unify control
extern std::map<uint32_t, UnifyControlS*> StreamId2UnifyCtl;

// vector of stream ids to process by my rank
extern std::vector<uint32_t> MyStreamIds;

#ifdef VT_MPI
   // number of MPI-ranks
   extern VT_MPI_INT NumRanks;

   // MPI-rank of calling process
   extern VT_MPI_INT MyRank;

   // map stream id <-> processing MPI-rank
   extern std::map<uint32_t, VT_MPI_INT> StreamId2Rank;
#endif // VT_MPI

#endif // _VT_UNIFY_H_
