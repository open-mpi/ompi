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

#ifndef _VT_UNIFY_DEFS_HDLR_H_
#define _VT_UNIFY_DEFS_HDLR_H_

#include "vt_unify_defs.h"

#include "vt_inttypes.h"

#include <vector>

int Handle_DefinitionComment(
   std::vector<Definitions::DefRec_Base_struct*>* p_vecLocDefRecs,
   uint32_t streamid, const char* comment );
int Handle_DefCreator(
   std::vector<Definitions::DefRec_Base_struct*>* p_vecLocDefRecs,
   uint32_t streamid, const char* creator );

int Handle_DefTimerResolution(
   std::vector<Definitions::DefRec_Base_struct*>* p_vecLocDefRecs,
   uint32_t streamid, uint64_t ticksPerSecond );

int Handle_DefProcessGroup(
   std::vector<Definitions::DefRec_Base_struct*>* p_vecLocDefRecs,
   uint32_t streamid, uint32_t deftoken, const char* name,
   uint32_t n, uint32_t* array ); 

int Handle_DefProcess(
   std::vector<Definitions::DefRec_Base_struct*>* p_vecLocDefRecs,
   uint32_t streamid, uint32_t deftoken, const char* name,
   uint32_t parent );

int Handle_DefSclFile(
   std::vector<Definitions::DefRec_Base_struct*>* p_vecLocDefRecs,
   uint32_t streamid, uint32_t deftoken, const char* filename );

int Handle_DefScl(
   std::vector<Definitions::DefRec_Base_struct*>* p_vecLocDefRecs,
   uint32_t streamid, uint32_t deftoken, uint32_t sclfile,
   uint32_t sclline );

int Handle_DefFileGroup(
   std::vector<Definitions::DefRec_Base_struct*>* p_vecLocDefRecs,
   uint32_t streamid, uint32_t deftoken, const char* name );

int Handle_DefFile(
   std::vector<Definitions::DefRec_Base_struct*>* p_vecLocDefRecs,
   uint32_t streamid, uint32_t deftoken, const char* name,
   uint32_t group );

int Handle_DefFunctionGroup(
   std::vector<Definitions::DefRec_Base_struct*>* p_vecLocDefRecs,
   uint32_t streamid, uint32_t deftoken, const char* name );

int Handle_DefFunction(
   std::vector<Definitions::DefRec_Base_struct*>* p_vecLocDefRecs,
   uint32_t streamid, uint32_t deftoken, const char* name,
   uint32_t group, uint32_t scltoken );

int Handle_DefCollectiveOperation(
   std::vector<Definitions::DefRec_Base_struct*>* p_vecLocDefRecs,
   uint32_t streamid, uint32_t collOp, const char* name,
   uint32_t type );

int Handle_DefCounterGroup(
   std::vector<Definitions::DefRec_Base_struct*>* p_vecLocDefRecs,
   uint32_t streamid, uint32_t deftoken, const char* name );

int Handle_DefCounter(
   std::vector<Definitions::DefRec_Base_struct*>* p_vecLocDefRecs,
   uint32_t streamid, uint32_t deftoken, const char* name,
   uint32_t properties, uint32_t countergroup, const char* unit );

#endif // _VT_UNIFY_DEFS_HDLR_H_
