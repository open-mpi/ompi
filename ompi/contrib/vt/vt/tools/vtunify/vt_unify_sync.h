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

#ifndef _VT_UNIFY_SYNC_H_
#define _VT_UNIFY_SYNC_H_

#include "vt_inttypes.h"

#include <list>
#include <map>
#include <string>
#include <vector>

#include <assert.h>

//
// Synchronization class
//
class Synchronization
{
public:

  struct SyncPhase_struct
  {
  SyncPhase_struct(): mapid(0),time(0),duration(0){}
    SyncPhase_struct(uint32_t _mapid, uint64_t _time, uint64_t _duration)
    : mapid(_mapid), time( _time), duration( _duration)
    {}

    uint32_t mapid;
    uint64_t time;
    uint64_t duration;
  };

  //
  // synchronization timestamp structure
  //
  struct SyncTime_struct
  {
    SyncTime_struct() : phase_idx(0) { t[0] = t[1] = t[2] = t[3] = 0; }
    ~SyncTime_struct() {}
    uint64_t t[4];
    uint32_t phase_idx;
  };

  // constructor
  Synchronization();
  
  // destructor
  ~Synchronization();

  bool run();

  void setMinStartTimeForStreamId( uint32_t streamId, uint64_t minStartTime );

  uint64_t getMinStartTimeForStreamId( uint32_t streamId );

  bool updateSyncParam( uint32_t procId );

private:

   //
   // sychronization parameter structure
   //
   struct SyncParam_struct
   {
     SyncParam_struct() : offset(0), drift(1.0){}
     SyncParam_struct(int64_t _offset, double _drift) 
     : offset(_offset), drift(_drift) 
      {}
  
     ~SyncParam_struct() {}
     
     int64_t offset;
     double drift;
   };

   bool calcSync( uint32_t round,
		  std::map<std::pair<uint32_t, uint32_t>, SyncTime_struct*> &first_time_stamps, 
		  std::map<std::pair<uint32_t, uint32_t>, SyncTime_struct*> &last_time_stamps );

   void print(double *a, int m, int n, char* info);

   std::map<uint32_t, std::map<std::pair<uint32_t, uint32_t>, SyncTime_struct*>*> m_mapSyncPhasemapProcessIdsTimestamps;
   std::map<uint32_t, std::vector<struct SyncParam_struct*>*> m_mapIdxvecSyncParam;
   std::map<uint32_t, uint64_t> m_mapIdxMinStartTime;
   std::vector<double> m_vecSyncPreCorrection;
   std::vector<uint32_t> m_vecRoundNum;
   uint32_t m_uProcNum;
   uint32_t m_uRoundMax;
};

// instance of class Synchronization
extern Synchronization * theSynchronization;

#endif // _VT_UNIFY_SYNC_H_
