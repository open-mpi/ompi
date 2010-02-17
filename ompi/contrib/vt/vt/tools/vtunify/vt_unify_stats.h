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

#ifndef _VT_UNIFY_STATS_H_
#define _VT_UNIFY_STATS_H_

#include "vt_inttypes.h"

#include <algorithm>
#include <cctype>
#include <map>
#include <string>
#include <vector>

// instance of class Statistics
class Statistics;
extern Statistics * theStatistics;

//
// Statistics class
//
class Statistics
{
public:

   //
   // function statistics sort flags
   //
   enum { STAT_SORT_FLAG_DIR_UP    = 0x1,
	  STAT_SORT_FLAG_DIR_DOWN  = 0x2,
	  STAT_SORT_FLAG_FUNCNAME  = 0x4,
	  STAT_SORT_FLAG_COUNT     = 0x8,
	  STAT_SORT_FLAG_INCL      = 0x10,
	  STAT_SORT_FLAG_EXCL      = 0x20,
	  STAT_SORT_FLAG_INCL_CALL = 0x40,
	  STAT_SORT_FLAG_EXCL_CALL = 0x80
   };

   //
   // function statistics structure
   //
   struct FuncStat_struct
   {
      FuncStat_struct()
	 : funcid(0), funcname(""), count(0.0), incl(0), excl(0) {}
      FuncStat_struct(uint32_t _funcid, std::string _funcname)
	 : funcid(_funcid), funcname(_funcname), count(0.0), incl(0), excl(0) {}
      FuncStat_struct(uint32_t _funcid, std::string _funcname, double _count,
		      uint64_t _incl, uint64_t _excl) 
	 : funcid(_funcid), funcname(_funcname), count(_count),
      incl(_incl), excl(_excl) {}

      uint32_t    funcid;   // function identifier
      std::string funcname; // function name
      double      count;    // number of calls
      uint64_t    incl;     // inclusive time
      uint64_t    excl;     // exclusive time
      
      bool operator<(const struct FuncStat_struct & a) const
      {
	 int flags = theStatistics->m_iFuncStatSortFlags;

	 if( (flags & STAT_SORT_FLAG_FUNCNAME) &&
	     (flags & STAT_SORT_FLAG_DIR_UP ) )
	 {
	    std::string _a = funcname, _b = a.funcname;
	    uint32_t i;

	    for( i = 0; i < funcname.length(); i++ )
	       _a[i] = tolower( funcname[i] );
	    for( i = 0; i < a.funcname.length(); i++ )
	       _b[i] = tolower( a.funcname[i] );
	    
	    return _a < _b;
	 }
	 else if( (flags & STAT_SORT_FLAG_FUNCNAME) &&
		  (flags & STAT_SORT_FLAG_DIR_DOWN ) )
	 {
	    return funcname > a.funcname;
	 }
	 else if( (flags & STAT_SORT_FLAG_COUNT) &&
		  (flags & STAT_SORT_FLAG_DIR_UP ) )
	 {
	    return count < a.count;
	 }
	 else if( (flags & STAT_SORT_FLAG_COUNT) &&
		  (flags & STAT_SORT_FLAG_DIR_DOWN ) )
	 {
	    return count > a.count;
	 }
	 else if( (flags & STAT_SORT_FLAG_INCL) &&
		  (flags & STAT_SORT_FLAG_DIR_UP ) )
	 {
	    return incl < a.incl;
	 }
	 else if( (flags & STAT_SORT_FLAG_INCL) &&
		  (flags & STAT_SORT_FLAG_DIR_DOWN ) )
	 {
	    return incl > a.incl;
	 }
	 else if( (flags & STAT_SORT_FLAG_EXCL) &&
		  (flags & STAT_SORT_FLAG_DIR_UP ) )
	 {
	    return excl < a.excl;
	 }
	 else if( (flags & STAT_SORT_FLAG_EXCL) &&
		  (flags & STAT_SORT_FLAG_DIR_DOWN ) )
	 {
	    return excl > a.excl;
	 }
	 else if( (flags & STAT_SORT_FLAG_INCL_CALL) &&
		  (flags & STAT_SORT_FLAG_DIR_UP ) )
	 {
	    return incl / count < a.incl / a.count;
	 }
	 else if( (flags & STAT_SORT_FLAG_INCL_CALL) &&
		  (flags & STAT_SORT_FLAG_DIR_DOWN ) )
	 {
	    return incl / count > a.incl / a.count;
	 }
	 else if( (flags & STAT_SORT_FLAG_EXCL_CALL) &&
		  (flags & STAT_SORT_FLAG_DIR_UP ) )
	 {
	    return excl / count < a.excl / a.count;
	 }
	 else if( (flags & STAT_SORT_FLAG_EXCL_CALL) &&
		  (flags & STAT_SORT_FLAG_DIR_DOWN ) )
	 {
	    return excl / count > a.excl / a.count;
	 }
	 else
	 {
	    return true;
	 }
      }
   };

   // class for compare function identifier
   //
   class FuncStat_funcId_eq :
      public std::unary_function<struct FuncStat_struct, bool>
   {
      uint32_t funcid;
   public:
      explicit FuncStat_funcId_eq(const uint32_t & _funcid)
	 : funcid(_funcid) {}
      bool operator()(const struct FuncStat_struct & a) const 
      {
	 return a.funcid == funcid;
      } 
   };

   // contructor
   Statistics();

   // destructor
   ~Statistics();

   bool run();

   bool addFunc( uint32_t funcId, std::string funcName );

   bool addFuncStat( uint32_t procId, uint32_t funcId, uint64_t count,
		     uint64_t incl, uint64_t excl );

   bool isFuncStatAvail();
   bool isFuncStatAvail( uint32_t procId );

   bool printFuncStat( std::string outFile,
		       int sortFlags = 0 );
   bool printFuncStat( std::string outFile,
		       uint32_t procId, int sortFlags = 0 );
   bool printFuncStat( std::string outFile,
		       std::vector<struct FuncStat_struct> & vecFuncStat,
		       int sortFlags = 0 );

   void setTimerRes( uint64_t res ) { m_lTimerRes = res; }

private:

   std::vector<struct FuncStat_struct> getFuncStat();
   std::vector<struct FuncStat_struct> getFuncStat( uint32_t procId );

   std::string getFuncNameById( uint32_t funcId );

   std::string shortName( const std::string & longName, uint32_t len = 20 );

   std::string formatTime( uint64_t time );

   // map function id -> function name
   std::map<uint32_t, std::string> m_mapFuncIdName;

   // map process id -> map function id -> function statistics
   std::map<uint32_t, std::map<uint32_t, struct FuncStat_struct*>*>
      m_mapProcIdFuncStat;

   uint64_t m_lTimerRes;

   // sort flags for function statistics output
   int m_iFuncStatSortFlags;

};

#endif // _VT_UNIFY_STATS_H_
