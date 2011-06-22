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

#ifndef _VT_UNIFY_HANDLERS_H_
#define _VT_UNIFY_HANDLERS_H_

#include "vt_unify_defs_recs.h"
#include "vt_unify_markers.h"

#include "otf.h"

// key-value list "record handler"
// translate local key tokens to global tokens
void Handle_KeyValueList( const uint32_t & proc, OTF_KeyValueList * kvs );

// definition record handlers
//

int Handle_DefComment( std::vector<DefRec_BaseS*> * locDefs,
       uint32_t streamid, const char * comment );

int Handle_DefCreator( std::vector<DefRec_BaseS*> * locDefs,
       uint32_t streamid, const char * creator );

int Handle_DefTimerResolution( std::vector<DefRec_BaseS*> * locDefs,
       uint32_t streamid, uint64_t ticksPerSecond );

int Handle_DefTimeRange( std::vector<DefRec_BaseS*> * locDefs,
       uint32_t streamid, uint64_t minTime, uint64_t maxTime );

int Handle_DefProcess( std::vector<DefRec_BaseS*> * locDefs,
       uint32_t streamid, uint32_t deftoken, const char * name,
       uint32_t parent );

int Handle_DefProcessGroup( std::vector<DefRec_BaseS*> * locDefs,
       uint32_t streamid, uint32_t deftoken, const char * name,
       uint32_t n, uint32_t * array );

int Handle_DefSclFile( std::vector<DefRec_BaseS*> * locDefs,
       uint32_t streamid, uint32_t deftoken, const char * filename );

int Handle_DefScl( std::vector<DefRec_BaseS*> * locDefs,
       uint32_t streamid, uint32_t deftoken, uint32_t sclfile,
       uint32_t sclline );

int Handle_DefFileGroup( std::vector<DefRec_BaseS*> * locDefs,
       uint32_t streamid, uint32_t deftoken, const char * name );

int Handle_DefFile( std::vector<DefRec_BaseS*> * locDefs,
       uint32_t streamid, uint32_t deftoken, const char * name,
       uint32_t group );

int Handle_DefFunctionGroup( std::vector<DefRec_BaseS*> * locDefs,
       uint32_t streamid, uint32_t deftoken, const char * name );

int Handle_DefFunction( std::vector<DefRec_BaseS*> * locDefs,
       uint32_t streamid, uint32_t deftoken, const char * name, uint32_t group,
       uint32_t scltoken );

int Handle_DefCollOp( std::vector<DefRec_BaseS*> * locDefs,
       uint32_t streamid, uint32_t collOp, const char * name, uint32_t type );

int Handle_DefCounterGroup( std::vector<DefRec_BaseS*> * locDefs,
       uint32_t streamid, uint32_t deftoken, const char * name );

int Handle_DefCounter( std::vector<DefRec_BaseS*> * locDefs,
       uint32_t streamid, uint32_t deftoken, const char * name,
       uint32_t properties, uint32_t countergroup, const char * unit );

int Handle_DefKeyValue( std::vector<DefRec_BaseS*> * locDefs,
       uint32_t streamid, uint32_t key, OTF_Type type, const char * name,
       const char * description );

// marker record handlers
//

int Handle_DefMarker( std::vector<DefRec_DefMarkerS*> * locDefs,
       uint32_t streamid, uint32_t deftoken, const char * name, uint32_t type );

int Handle_MarkerSpot( std::vector<MarkersC::MarkerSpotS*> * locSpots,
       uint64_t time, uint32_t proc, uint32_t marker, const char * text );

// event record handlers
//

int Handle_EventComment( OTF_WStream * wstream,
       uint64_t time, uint32_t proc, const char * comment,
       OTF_KeyValueList * kvs );

int Handle_Enter( OTF_WStream * wstream,
       uint64_t time, uint32_t func, uint32_t proc, uint32_t scl,
       OTF_KeyValueList * kvs );

int Handle_Leave( OTF_WStream * wstream,
       uint64_t time, uint32_t func, uint32_t proc, uint32_t scl,
       OTF_KeyValueList * kvs );

int Handle_Counter( OTF_WStream * wstream,
       uint64_t time, uint32_t proc, uint32_t counter, uint64_t value,
       OTF_KeyValueList * kvs );

int Handle_BeginFileOp( OTF_WStream * wstream,
       uint64_t time, uint32_t proc, uint64_t matchid, uint32_t scl,
       OTF_KeyValueList * kvs );

int Handle_EndFileOp( OTF_WStream * wstream,
       uint64_t time, uint32_t proc, uint32_t file, uint64_t matchid,
       uint64_t handleid, uint32_t operation, uint64_t bytes, uint32_t scl,
       OTF_KeyValueList * kvs );

int Handle_SendMsg( OTF_WStream * wstream,
       uint64_t time, uint32_t sender, uint32_t receiver, uint32_t comm,
       uint32_t tag, uint32_t length, uint32_t scl, OTF_KeyValueList * kvs );

int Handle_RecvMsg( OTF_WStream * wstream,
       uint64_t time, uint32_t receiver, uint32_t sender, uint32_t comm,
       uint32_t tag, uint32_t length, uint32_t scl, OTF_KeyValueList * kvs );

int Handle_BeginCollOp( OTF_WStream * wstream,
       uint64_t time, uint32_t proc, uint32_t operation, uint64_t matchid,
       uint32_t comm, uint32_t root, uint64_t sent, uint64_t recvd,
       uint32_t scl, OTF_KeyValueList * kvs );

int Handle_EndCollOp( OTF_WStream * wstream,
       uint64_t time, uint32_t proc, uint64_t matchid, OTF_KeyValueList * kvs );

int Handle_RMAPut( OTF_WStream * wstream,
       uint64_t time, uint32_t proc, uint32_t origin, uint32_t dest,
       uint32_t comm, uint32_t tag, uint64_t bytes, uint32_t scl,
       OTF_KeyValueList * kvs );

int Handle_RMAPutRemoteEnd( OTF_WStream * wstream,
       uint64_t time, uint32_t proc, uint32_t origin, uint32_t dest,
       uint32_t comm, uint32_t tag, uint64_t bytes, uint32_t scl,
       OTF_KeyValueList * kvs );

int Handle_RMAGet( OTF_WStream * wstream,
       uint64_t time, uint32_t proc, uint32_t origin, uint32_t dest,
       uint32_t comm, uint32_t tag, uint64_t bytes, uint32_t scl,
       OTF_KeyValueList * kvs );

int Handle_RMAEnd( OTF_WStream * wstream,
       uint64_t time, uint32_t proc, uint32_t remote, uint32_t comm,
       uint32_t tag, uint32_t scl, OTF_KeyValueList * kvs );

// summary record handlers
//

int Handle_FunctionSummary( OTF_WStream * wstream,
       uint64_t time, uint32_t func, uint32_t proc, uint64_t invocations,
       uint64_t exclTime, uint64_t inclTime );

int Handle_MessageSummary( OTF_WStream * wstream,
       uint64_t time, uint32_t proc, uint32_t peer, uint32_t comm,
       uint32_t type, uint64_t sentNum, uint64_t recvNum,
       uint64_t sentBytes, uint64_t recvBytes );

int Handle_CollOpSummary( OTF_WStream * wstream,
       uint64_t time, uint32_t proc, uint32_t comm, uint32_t collop,
       uint64_t sentNum, uint64_t recvNum, uint64_t sentBytes,
       uint64_t recvBytes );

int Handle_FileOpSummary( OTF_WStream * wstream,
       uint64_t time, uint32_t file, uint32_t proc, uint64_t nopen,
       uint64_t nclose, uint64_t nread, uint64_t nwrite, uint64_t nseek,
       uint64_t bytesRead, uint64_t bytesWrite );

#endif // _VT_UNIFY_HANDLERS_H_
