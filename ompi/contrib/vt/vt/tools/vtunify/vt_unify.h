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

#ifndef _VT_UNIFY_H_
#define _VT_UNIFY_H_

#include "config.h"

#include "vt_inttypes.h"

#include <map>
#include <string>
#include <vector>

#define STRBUFSIZE 1024

#if SIZEOF_LONG == 4
#  define ATOL8 atoll
#else
#  define ATOL8 atol
#endif

extern const std::string ExeName;
extern const std::string TmpFileSuffix;
extern const std::string UniFilePrefix;

//
// unify parameter structure (contains the program options)
//
struct Params_struct
{
   Params_struct() 
      : uctl_files_num(0), in_file_prefix(""), out_file_prefix(""),
	stats_out_file(""), stats_sort_flags(0), showstats(true),
	docompress(true), doclean(true), beverbose(false) {}

   uint32_t    uctl_files_num;
   std::string in_file_prefix;
   std::string out_file_prefix;
   std::string stats_out_file;
   int         stats_sort_flags;
   bool        showstats;
   bool        docompress;
   bool        doclean;
   bool        beverbose;
};

//
// unify control structure for each file stream
//
struct UnifyControl_struct
{
   UnifyControl_struct() : streamid(0)
      {
	 ltime[0] = 0; ltime[1] = 1;
	 offset[0] = 0; offset[1] = 0;
      }
   
   UnifyControl_struct(uint32_t _streamid,
		       int64_t * _ltime, int64_t * _offset)
      : streamid(_streamid)
      {
	 ltime[0] = _ltime[0]; ltime[1] = _ltime[1];
	 offset[0] = _offset[0]; offset[1] = _offset[1];
      }
   
   uint32_t    streamid;   // id of input stream
   int64_t     ltime[2];   // local times ...
   int64_t     offset[2];  // ... and chronological offsets to global time
};

uint64_t CorrectTime( uint32_t loccpuid, uint64_t time );

// unify parameters
extern struct Params_struct Params;

// minimal start time
extern uint64_t g_uMinStartTime;

// unify control vector
extern std::vector<UnifyControl_struct*> g_vecUnifyCtls;

// map stream id <-> unify control index
extern std::map<uint32_t, uint32_t> g_mapStreamIdUnifyCtlIdx;

#endif // _VT_UNIFY_H_
