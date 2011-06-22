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

#ifndef _VT_UNIFY_DEFS_RECS_H_
#define _VT_UNIFY_DEFS_RECS_H_

#include "vt_unify.h"

#include "vt_inttypes.h"

#include "otf.h"

#include <string>
#include <vector>

//
// definition record types
//
typedef enum
{
   DEF_REC_TYPE__DefCreator,
   DEF_REC_TYPE__DefTimerResolution,
   DEF_REC_TYPE__DefTimeRange,
   DEF_REC_TYPE__DefProcess,
   DEF_REC_TYPE__DefProcessGroup,
   DEF_REC_TYPE__DefSclFile,
   DEF_REC_TYPE__DefScl,
   DEF_REC_TYPE__DefFileGroup,
   DEF_REC_TYPE__DefFile,
   DEF_REC_TYPE__DefFunctionGroup,
   DEF_REC_TYPE__DefFunction,
   DEF_REC_TYPE__DefCollOp,
   DEF_REC_TYPE__DefCounterGroup,
   DEF_REC_TYPE__DefCounter,
   DEF_REC_TYPE__DefKeyValue,
   DEF_REC_TYPE__DefMarker,
   DEF_REC_TYPE__DefComment,
   DEF_REC_TYPE__Num
} DefRecTypeT;

//
// DefRec_BaseS
//
struct DefRec_BaseS
{
   DefRec_BaseS( DefRecTypeT _dtype )
      : dtype( _dtype ), loccpuid( 0 ), deftoken( 0 ) {}
   DefRec_BaseS( const DefRecTypeT & _dtype, const uint32_t & _loccpuid,
      const uint32_t & _deftoken )
      : dtype( _dtype ), loccpuid( _loccpuid ), deftoken( _deftoken ) {}
   virtual ~DefRec_BaseS() {}

#ifdef VT_MPI
   virtual VT_MPI_INT getPackSize();
   virtual void pack( char *& buffer, const VT_MPI_INT & bufferSize,
                      VT_MPI_INT & bufferPos );
   virtual void unpack( char *& buffer, const VT_MPI_INT & bufferSize,
                        VT_MPI_INT & bufferPos );
#endif // VT_MPI

   bool operator<( const DefRec_BaseS & a ) const
   {
      return deftoken < a.deftoken;
   }

   DefRecTypeT dtype;
   uint32_t    loccpuid;
   uint32_t    deftoken;

};

//
// DefRec_DefCommentS
//
struct DefRec_DefCommentS : DefRec_BaseS
{
   typedef enum
   {
      TYPE_START_TIME, TYPE_STOP_TIME, TYPE_VT, TYPE_USER,
      TYPE_USRCOM_SEND, TYPE_USRCOM_RECV
   } CommentTypeT;

   DefRec_DefCommentS()
      : DefRec_BaseS( DEF_REC_TYPE__DefComment ) {}
   DefRec_DefCommentS( const uint32_t & _loccpuid, const uint32_t _orderidx,
       const CommentTypeT & _type, const std::string & _comment )
      : DefRec_BaseS( DEF_REC_TYPE__DefComment, _loccpuid, _orderidx ),
        type( _type ), comment( _comment ) {}

#ifdef VT_MPI
   VT_MPI_INT getPackSize();
   void pack( char *& buffer, const VT_MPI_INT & bufferSize,
              VT_MPI_INT & bufferPos );
   void unpack( char *& buffer, const VT_MPI_INT & bufferSize,
                VT_MPI_INT & bufferPos );
#endif // VT_MPI

   bool operator==( const DefRec_DefCommentS & a ) const
   {
      return ( type == a.type &&
               comment.compare( a.comment ) == 0 );
   }

   bool operator<( const DefRec_DefCommentS & a ) const
   {
      if( type == a.type )
         return deftoken < a.deftoken; // order index
      else
         return type < a.type;
   }

   CommentTypeT type;
   std::string  comment;

};

//
// DefRec_DefCreatorS
//
struct DefRec_DefCreatorS : DefRec_BaseS
{
   DefRec_DefCreatorS()
      : DefRec_BaseS( DEF_REC_TYPE__DefCreator ) {}
   DefRec_DefCreatorS( const std::string & _creator )
      : DefRec_BaseS( DEF_REC_TYPE__DefCreator ),
        creator( _creator ) {}

#ifdef VT_MPI
   VT_MPI_INT getPackSize();
   void pack( char *& buffer, const VT_MPI_INT & bufferSize,
              VT_MPI_INT & bufferPos );
   void unpack( char *& buffer, const VT_MPI_INT & bufferSize,
                VT_MPI_INT & bufferPos );
#endif // VT_MPI

   std::string creator;

};

//
// DefRec_DefTimerResolutionS
//
struct DefRec_DefTimerResolutionS : DefRec_BaseS
{
   DefRec_DefTimerResolutionS()
      : DefRec_BaseS( DEF_REC_TYPE__DefTimerResolution ),
        ticksPerSecond( 0 ) {}
   DefRec_DefTimerResolutionS( const uint64_t & _ticksPerSecond )
      : DefRec_BaseS( DEF_REC_TYPE__DefTimerResolution ),
        ticksPerSecond( _ticksPerSecond ) {}

#ifdef VT_MPI
   VT_MPI_INT getPackSize();
   void pack( char *& buffer, const VT_MPI_INT & bufferSize,
              VT_MPI_INT & bufferPos );
   void unpack( char *& buffer, const VT_MPI_INT & bufferSize,
                VT_MPI_INT & bufferPos );
#endif // VT_MPI

   uint64_t ticksPerSecond;

};

//
// DefRec_DefTimeRangeS
//
struct DefRec_DefTimeRangeS : DefRec_BaseS
{
   DefRec_DefTimeRangeS()
      : DefRec_BaseS( DEF_REC_TYPE__DefTimeRange ),
        minTime( 0 ), maxTime( 0 ) {}
   DefRec_DefTimeRangeS( const uint32_t & _loccpuid, const uint64_t & _minTime,
      const uint64_t & _maxTime )
      : DefRec_BaseS( DEF_REC_TYPE__DefTimeRange, _loccpuid, 0 ),
        minTime( _minTime ), maxTime( _maxTime ) {}

#ifdef VT_MPI
   VT_MPI_INT getPackSize();
   void pack( char *& buffer, const VT_MPI_INT & bufferSize,
              VT_MPI_INT & bufferPos );
   void unpack( char *& buffer, const VT_MPI_INT & bufferSize,
                VT_MPI_INT & bufferPos );
#endif // VT_MPI

   uint64_t minTime;
   uint64_t maxTime;

};

//
// DefRec_DefProcessS
//
struct DefRec_DefProcessS : DefRec_BaseS
{
   DefRec_DefProcessS()
      : DefRec_BaseS( DEF_REC_TYPE__DefProcess ),
        parent( 0 ) {}
   DefRec_DefProcessS( const uint32_t & _deftoken, const std::string & _name,
      const uint32_t & _parent )
      : DefRec_BaseS( DEF_REC_TYPE__DefProcess, 0, _deftoken ),
        name( _name ), parent( _parent ) {}

#ifdef VT_MPI
   VT_MPI_INT getPackSize();
   void pack( char *& buffer, const VT_MPI_INT & bufferSize,
              VT_MPI_INT & bufferPos );
   void unpack( char *& buffer, const VT_MPI_INT & bufferSize,
                VT_MPI_INT & bufferPos );
#endif // VT_MPI

   bool operator<( const DefRec_DefProcessS & a ) const
   {
      if( ( deftoken & VT_TRACEID_BITMASK ) ==
          ( a.deftoken & VT_TRACEID_BITMASK ) )
         return deftoken < a.deftoken;
      else
         return ( deftoken & VT_TRACEID_BITMASK ) <
                ( a.deftoken & VT_TRACEID_BITMASK );
   }

   std::string name;
   uint32_t    parent;

};

//
// DefRec_DefProcessGroupS
//
struct DefRec_DefProcessGroupS : DefRec_BaseS
{
   typedef enum
   {
      TYPE_NODE, TYPE_MPI_COMM_WORLD, TYPE_MPI_COMM_SELF, TYPE_MPI_COMM_OTHER,
      TYPE_OMP_TEAM, TYPE_GPU_COMM, TYPE_GPU_GROUP, TYPE_USER_COMM, TYPE_OTHER
   } ProcessGroupTypeT;

   DefRec_DefProcessGroupS()
      : DefRec_BaseS( DEF_REC_TYPE__DefProcessGroup ) {}
   DefRec_DefProcessGroupS( const uint32_t & _loccpuid,
      const uint32_t & _deftoken, const ProcessGroupTypeT & _type,
      const std::string & _name, const uint32_t & _nmembers,
      const uint32_t * _members )
      : DefRec_BaseS( DEF_REC_TYPE__DefProcessGroup, _loccpuid, _deftoken ),
        type( _type ), name( _name )
   {
      if( _nmembers > 0 )
      {
         members.resize( _nmembers );
         members.assign( _members, _members + _nmembers );
      }
   }

   DefRec_DefProcessGroupS( const uint32_t & _loccpuid,
      const uint32_t & _deftoken, const ProcessGroupTypeT & _type,
      const std::string & _name, const std::vector<uint32_t> & _members )
      : DefRec_BaseS( DEF_REC_TYPE__DefProcessGroup, _loccpuid, _deftoken ),
        type( _type ), name( _name ), members( _members ) {}

#ifdef VT_MPI
   VT_MPI_INT getPackSize();
   void pack( char *& buffer, const VT_MPI_INT & bufferSize,
              VT_MPI_INT & bufferPos );
   void unpack( char *& buffer, const VT_MPI_INT & bufferSize,
                VT_MPI_INT & bufferPos );
#endif // VT_MPI

   bool operator==( const DefRec_DefProcessGroupS & a ) const
   {
      return ( type == a.type &&
               members == a.members &&
               name.compare( a.name ) == 0 );
   }

   bool operator<( const DefRec_DefProcessGroupS & a ) const
   {
      if( type == a.type )
         return deftoken < a.deftoken;
      else
         return type < a.type;
   }

   ProcessGroupTypeT     type;
   std::string           name;
   std::vector<uint32_t> members;

};

//
// DefRec_DefSclFileS
//
struct DefRec_DefSclFileS : DefRec_BaseS
{
   DefRec_DefSclFileS()
      : DefRec_BaseS( DEF_REC_TYPE__DefSclFile ) {}
   DefRec_DefSclFileS( const uint32_t & _loccpuid, const uint32_t & _deftoken,
      std::string _filename)
      : DefRec_BaseS( DEF_REC_TYPE__DefSclFile, _loccpuid, _deftoken ),
        filename( _filename ) {}

#ifdef VT_MPI
   VT_MPI_INT getPackSize();
   void pack( char *& buffer, const VT_MPI_INT & bufferSize,
              VT_MPI_INT & bufferPos );
   void unpack( char *& buffer, const VT_MPI_INT & bufferSize,
                VT_MPI_INT & bufferPos );
#endif // VT_MPI

   bool operator==( const DefRec_DefSclFileS & a ) const
   {
      return filename.compare( a.filename ) == 0;
   }

   std::string filename;

};

//
// DefRec_DefSclS
//
struct DefRec_DefSclS : DefRec_BaseS
{
   DefRec_DefSclS()
      : DefRec_BaseS( DEF_REC_TYPE__DefScl ),
        sclfile( 0 ), sclline( 0 ) {}
   DefRec_DefSclS( const uint32_t & _loccpuid, const uint32_t & _deftoken,
      const uint32_t & _sclfile, const uint32_t & _sclline )
      : DefRec_BaseS( DEF_REC_TYPE__DefScl, _loccpuid, _deftoken ),
        sclfile( _sclfile ), sclline( _sclline ) {}

#ifdef VT_MPI
   VT_MPI_INT getPackSize();
   void pack( char *& buffer, const VT_MPI_INT & bufferSize,
              VT_MPI_INT & bufferPos );
   void unpack( char *& buffer, const VT_MPI_INT & bufferSize,
                VT_MPI_INT & bufferPos );
#endif // VT_MPI

   bool operator==( const DefRec_DefSclS & a ) const
   {
      return ( sclfile == a.sclfile &&
               sclline == a.sclline );
   }

   uint32_t sclfile;
   uint32_t sclline;

};

//
// DefRec_DefFileGroupS
//
struct DefRec_DefFileGroupS : DefRec_BaseS
{
   DefRec_DefFileGroupS()
      : DefRec_BaseS( DEF_REC_TYPE__DefFileGroup ) {}
   DefRec_DefFileGroupS( const uint32_t & _loccpuid, const uint32_t & _deftoken,
      const std::string & _name)
      : DefRec_BaseS( DEF_REC_TYPE__DefFileGroup, _loccpuid, _deftoken ),
        name( _name ) {}

#ifdef VT_MPI
   VT_MPI_INT getPackSize();
   void pack( char *& buffer, const VT_MPI_INT & bufferSize,
              VT_MPI_INT & bufferPos );
   void unpack( char *& buffer, const VT_MPI_INT & bufferSize,
                VT_MPI_INT & bufferPos );
#endif // VT_MPI

   bool operator==( const DefRec_DefFileGroupS & a ) const
   {
      return name.compare( a.name ) == 0;
   }

   std::string name;

};

//
// DefRec_DefFileS
//
struct DefRec_DefFileS : DefRec_BaseS
{
   DefRec_DefFileS()
      : DefRec_BaseS( DEF_REC_TYPE__DefFile ),
        group( 0 ) {}
   DefRec_DefFileS( const uint32_t & _loccpuid, const uint32_t & _deftoken,
      const std::string & _name, const uint32_t & _group )
      : DefRec_BaseS( DEF_REC_TYPE__DefFile, _loccpuid, _deftoken ),
        name( _name ), group( _group ) {}

#ifdef VT_MPI
   VT_MPI_INT getPackSize();
   void pack( char *& buffer, const VT_MPI_INT & bufferSize,
              VT_MPI_INT & bufferPos );
   void unpack( char *& buffer, const VT_MPI_INT & bufferSize,
                VT_MPI_INT & bufferPos );
#endif // VT_MPI

   bool operator==( const DefRec_DefFileS & a ) const
   {
      return ( group == a.group &&
               name.compare( a.name ) == 0 );
   }

   std::string name;
   uint32_t    group;

};

//
// DefRec_DefFunctionGroupS
//
struct DefRec_DefFunctionGroupS : DefRec_BaseS
{
   DefRec_DefFunctionGroupS()
      : DefRec_BaseS( DEF_REC_TYPE__DefFunctionGroup ) {}
   DefRec_DefFunctionGroupS( const uint32_t & _loccpuid,
      const uint32_t & _deftoken, const std::string & _name )
      : DefRec_BaseS( DEF_REC_TYPE__DefFunctionGroup, _loccpuid, _deftoken ),
        name( _name ) {}

#ifdef VT_MPI
   VT_MPI_INT getPackSize();
   void pack( char *& buffer, const VT_MPI_INT & bufferSize,
              VT_MPI_INT & bufferPos );
   void unpack( char *& buffer, const VT_MPI_INT & bufferSize,
                VT_MPI_INT & bufferPos );
#endif // VT_MPI

   bool operator==( const DefRec_DefFunctionGroupS & a ) const
   {
      return name.compare( a.name ) == 0;
   }

   std::string name;

};

//
// DefRec_DefFunctionS
//
struct DefRec_DefFunctionS : DefRec_BaseS
{
   DefRec_DefFunctionS()
      : DefRec_BaseS( DEF_REC_TYPE__DefFunction ),
        group( 0 ), scltoken( 0 ) {}
   DefRec_DefFunctionS(const uint32_t & _loccpuid, const uint32_t & _deftoken,
      const std::string & _name, const uint32_t & _group,
      const uint32_t & _scltoken )
      : DefRec_BaseS( DEF_REC_TYPE__DefFunction, _loccpuid, _deftoken ),
        name( _name ), group( _group ), scltoken( _scltoken ) {}

#ifdef VT_MPI
   VT_MPI_INT getPackSize();
   void pack( char *& buffer, const VT_MPI_INT & bufferSize,
              VT_MPI_INT & bufferPos );
   void unpack( char *& buffer, const VT_MPI_INT & bufferSize,
                VT_MPI_INT & bufferPos );
#endif // VT_MPI

   bool operator==( const DefRec_DefFunctionS & a ) const
   {
      return ( group == a.group &&
               scltoken == a.scltoken &&
               name.compare( a.name ) == 0 );
   }

   std::string name;
   uint32_t    group;
   uint32_t    scltoken;

};

//
// DefRec_DefCollOpS
//
struct DefRec_DefCollOpS : DefRec_BaseS
{
   DefRec_DefCollOpS()
      : DefRec_BaseS( DEF_REC_TYPE__DefCollOp ),
      type( 0 ) {}
   DefRec_DefCollOpS( const uint32_t & _loccpuid, const uint32_t & _deftoken,
      const std::string & _name, const uint32_t & _type)
      : DefRec_BaseS( DEF_REC_TYPE__DefCollOp, _loccpuid, _deftoken ),
        name( _name ), type( _type ) {}

#ifdef VT_MPI
   VT_MPI_INT getPackSize();
   void pack( char *& buffer, const VT_MPI_INT & bufferSize,
              VT_MPI_INT & bufferPos );
   void unpack( char *& buffer, const VT_MPI_INT & bufferSize,
                VT_MPI_INT & bufferPos );
#endif // VT_MPI

   bool operator==( const DefRec_DefCollOpS & a ) const
   {
      return ( type == a.type &&
               name.compare( a.name ) == 0 );
   }

   std::string name;
   uint32_t    type;

};

//
// DefRec_DefCounterGroupS
//
struct DefRec_DefCounterGroupS : DefRec_BaseS
{
   DefRec_DefCounterGroupS()
      : DefRec_BaseS( DEF_REC_TYPE__DefCounterGroup ) {}
   DefRec_DefCounterGroupS( const uint32_t & _loccpuid,
      const uint32_t & _deftoken, const std::string & _name)
      : DefRec_BaseS( DEF_REC_TYPE__DefCounterGroup, _loccpuid, _deftoken ),
        name( _name ) {}

#ifdef VT_MPI
   VT_MPI_INT getPackSize();
   void pack( char *& buffer, const VT_MPI_INT & bufferSize,
              VT_MPI_INT & bufferPos );
   void unpack( char *& buffer, const VT_MPI_INT & bufferSize,
                VT_MPI_INT & bufferPos );
#endif // VT_MPI

   bool operator==( const DefRec_DefCounterGroupS & a ) const
   {
      return name.compare( a.name ) == 0;
   }

   std::string name;

};

//
// DefRec_DefCounterS
//
struct DefRec_DefCounterS : DefRec_BaseS
{
   DefRec_DefCounterS()
      : DefRec_BaseS( DEF_REC_TYPE__DefCounter ),
        properties( 0 ), group( 0 ) {}
   DefRec_DefCounterS( const uint32_t & _loccpuid, const uint32_t & _deftoken,
      const std::string & _name, const uint32_t & _properties,
      const uint32_t & _group, const std::string & _unit)
      : DefRec_BaseS( DEF_REC_TYPE__DefCounter, _loccpuid, _deftoken ),
        name( _name ), properties( _properties ), group( _group ),
        unit( _unit ) {}

#ifdef VT_MPI
   VT_MPI_INT getPackSize();
   void pack( char *& buffer, const VT_MPI_INT & bufferSize,
              VT_MPI_INT & bufferPos );
   void unpack( char *& buffer, const VT_MPI_INT & bufferSize,
                VT_MPI_INT & bufferPos );
#endif // VT_MPI

   bool operator==( const DefRec_DefCounterS & a ) const
   {
      return ( properties == a.properties &&
               group == a.group &&
               name.compare( a.name ) == 0 &&
               unit.compare( a.unit ) == 0 );
   }

   std::string name;
   uint32_t    properties;
   uint32_t    group;
   std::string unit;

};

//
// DefRec_DefKeyValueS
//
struct DefRec_DefKeyValueS : DefRec_BaseS
{
   DefRec_DefKeyValueS()
      : DefRec_BaseS( DEF_REC_TYPE__DefKeyValue ),
        type( OTF_UNKNOWN ) {}
   DefRec_DefKeyValueS( const uint32_t & _loccpuid, const uint32_t & _deftoken,
      const OTF_Type & _type, const std::string & _name )
      : DefRec_BaseS( DEF_REC_TYPE__DefKeyValue, _loccpuid, _deftoken ),
        type( _type ), name( _name ) {}

#ifdef VT_MPI
   VT_MPI_INT getPackSize();
   void pack( char *& buffer, const VT_MPI_INT & bufferSize,
              VT_MPI_INT & bufferPos );
   void unpack( char *& buffer, const VT_MPI_INT & bufferSize,
                VT_MPI_INT & bufferPos );
#endif // VT_MPI

   bool operator==( const DefRec_DefKeyValueS & a ) const
   {
      return ( type == a.type &&
               name.compare( a.name ) == 0 );
   }

   OTF_Type type;
   std::string name;

};

//
// DefRec_DefMarkerS
//
struct DefRec_DefMarkerS: DefRec_BaseS
{
   DefRec_DefMarkerS()
      : DefRec_BaseS( DEF_REC_TYPE__DefMarker ),
        type( 0 ) {}
   DefRec_DefMarkerS( const uint32_t & _loccpuid, const uint32_t & _deftoken,
      const uint32_t & _type, const std::string & _name)
      : DefRec_BaseS( DEF_REC_TYPE__DefMarker, _loccpuid, _deftoken ),
        type( _type ), name( _name ) {}

#ifdef VT_MPI
   VT_MPI_INT getPackSize();
   void pack( char *& buffer, const VT_MPI_INT & bufferSize,
              VT_MPI_INT & bufferPos );
   void unpack( char *& buffer, const VT_MPI_INT & bufferSize,
                VT_MPI_INT & bufferPos );
#endif // VT_MPI

   bool operator==( const DefRec_DefMarkerS & a ) const
   {
      return ( type == a.type &&
               name.compare( a.name ) == 0 );
   }

   bool operator<( const DefRec_DefMarkerS & a ) const
   {
      if( type == a.type )
         return deftoken < a.deftoken;
      else
         return type < a.type;
   }

   uint32_t    type;
   std::string name;

};

// compare function for pre-sorting local definitions
inline bool DefRec_LocCmp( const DefRec_BaseS * a, const DefRec_BaseS * b )
{
   // if both are process group definitions (i.e. user created MPI comms.),
   // sort by its local token to preserve the order of creation
   if( a->dtype == DEF_REC_TYPE__DefProcessGroup &&
       b->dtype == DEF_REC_TYPE__DefProcessGroup )
      return a->deftoken < b->deftoken;
   // otherwise, sort by definition type
   else
      return a->dtype < b->dtype;
}

#endif // _VT_UNIFY_DEFS_RECS_H_
