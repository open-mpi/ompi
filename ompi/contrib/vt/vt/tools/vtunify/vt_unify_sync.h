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

#ifndef _VT_UNIFY_SYNC_H_
#define _VT_UNIFY_SYNC_H_

#include "vt_unify.h"

#ifdef VT_ETIMESYNC
#  include "vt_unify_esync.h"
#endif // VT_ETIMESYNC

#include "vt_inttypes.h"

#include <map>

#include <assert.h>

//
// TimeSyncC class
//
class TimeSyncC
{
public:

   // type for time synchronization methods
   typedef enum { METHOD_OFFSET, METHOD_ENHANCED } MethodTypeT;

   // type for time ranges
   typedef std::pair<uint64_t, uint64_t> TimeRangeT;

   // constructor
   TimeSyncC();

   // destructor
   ~TimeSyncC();

   // initialize time synchronization
   bool initialize();

   // set time sync. method
   void setSyncMethod( const MethodTypeT & method )
   {
#ifndef VT_ETIMESYNC
      assert( method != METHOD_ENHANCED );
#endif // VT_ETIMESYNC

      m_syncMethod = method;
   }

   // get time sync. method
   MethodTypeT getSyncMethod() const
   {
      return m_syncMethod;
   }

   // set time range of certain process and update minimum start time
   void setTimeRange( const uint32_t & proc, const uint64_t & minTime,
           const uint64_t & maxTime )
   {
      assert( proc != 0 );

      // set time range of certain process
      m_proc2TimeRange[proc] = TimeRangeT( minTime, maxTime );
   }

   // get time range of certain process
   // (if proc is 0, the global time range will be returned)
   TimeRangeT getTimeRange( const uint32_t & proc = 0 ) const
   {
      // search for time range of given process
      std::map<uint32_t, TimeRangeT>::const_iterator it =
         m_proc2TimeRange.find( proc );
      assert( it != m_proc2TimeRange.end() );

      // return time range
      return it->second;
   }

   // get minimum start time
   uint64_t getMinStartTime() const
   {
      return m_minStartTime;
   }

#ifdef VT_ETIMESYNC

   // update timer parameters of certain process
   void updateSyncParam( const uint32_t & proc )
   {
      assert( m_eTimeSync );
      m_eTimeSync->updateSyncParam( proc );
   }

   // reset timer parameters of certain process
   void resetSyncParam( const uint32_t & proc )
   {
      assert( m_eTimeSync );
      m_eTimeSync->resetSyncParam( proc );
   }

#endif // VT_ETIMESYNC

   // translate local timestamp to global
   // (defined here to become inlined)
   uint64_t correctTime( const uint32_t & process, const uint64_t & time ) const
   {
      // get master process id
      uint32_t mprocess = process & VT_TRACEID_BITMASK;

      std::map<uint32_t, UnifyControlS*>::const_iterator it =
         StreamId2UnifyCtl.find( mprocess );
      assert( it != StreamId2UnifyCtl.end() );

#ifdef VT_ETIMESYNC
      if( m_syncMethod == METHOD_ENHANCED )
      {
         const int64_t & offset = it->second->sync_offset;
         const double & drift = it->second->sync_drift;

         return (uint64_t)( offset + (uint64_t)( drift * (double)time ) );
      }
      else
#endif // VT_ETIMESYNC
      {
         const int64_t * ltime = it->second->ltime;
         const int64_t * offset = it->second->offset;

         return
            (uint64_t)( ( (double)time +
                        ( ( ( (double)offset[1] - (double)offset[0] ) /
                            ( (double)ltime[1] - (double)ltime[0] ) )
                          * ( (double)time - (double)ltime[0]) )
                        + (double)offset[0] ) - m_minStartTime );
      }
   }

private:

   // time sync. method to use
   MethodTypeT m_syncMethod;

   // map process id <-> time range
   // (on rank 0 the first map entry (process id 0) holds the global time range)
   std::map<uint32_t, TimeRangeT> m_proc2TimeRange;

   // minimum start time
   uint64_t m_minStartTime;

#ifdef VT_ETIMESYNC
   // instance of class ETimeSyncC which cares about enhanced time sync.
   ETimeSyncC * m_eTimeSync;
#endif // VT_ETIMESYNC

};

// instance of class TimeSyncC
extern TimeSyncC * theTimeSync;

#endif // _VT_UNIFY_SYNC_H_
