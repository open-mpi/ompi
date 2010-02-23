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

#include "vt_unify.h"
#include "vt_unify_defs.h"
#include "vt_unify_stats.h"

#include "vt_inttypes.h"

#include "otf.h"

#include <vector>

#include <string.h>

int
Handle_DefinitionComment( std::vector<Definitions::DefRec_Base_struct*>*
			  p_vecLocDefRecs, uint32_t streamid,
			  const char* comment )
{
   static uint32_t orderidx = 4; // 0-3 reserved for time comments
   static bool first_user = true;

   // Start-time comment(s)
   if( strlen( comment ) >= 14 &&
       strncmp( comment, "__STARTTIME__", 13 ) == 0 )
   {
      uint64_t starttime = ATOL8(comment+14);
      if( starttime < g_uMinStartTimeEpoch )
	 g_uMinStartTimeEpoch = starttime;
   }
   // Stop-time comment(s)
   else if( strlen( comment ) >= 13 &&
	    strncmp( comment, "__STOPTIME__", 12 ) == 0 )
   {
      uint64_t stoptime = ATOL8(comment+13);
      if( stoptime > g_uMaxStopTimeEpoch )
	 g_uMaxStopTimeEpoch = stoptime;
   }
   // VampirTrace comments
   else if( strlen( comment ) >= 15 &&
	    strncmp( comment, "__VT_COMMENT__", 14 ) == 0 )
   {
      p_vecLocDefRecs->push_back( new Definitions::DefRec_DefinitionComment_struct(
				     orderidx++,
				     comment+15 ) );
   }
   // User comments
   else
   {
      // first user comment?
      if( first_user )
      {
	 // yes -> add headline for user comments to vector of
	 // local definitions
	 //
	 p_vecLocDefRecs->push_back(
	    new Definitions::DefRec_DefinitionComment_struct(
	       100,
	       "User Comments:" ) );
	 first_user = false;
      }

      // add user comment to vector of local definitions
      p_vecLocDefRecs->push_back(
	 new Definitions::DefRec_DefinitionComment_struct(
	    100 + orderidx++,
	    (std::string(" ") + std::string(comment) ) ) );
   }

   return OTF_RETURN_OK;
}

int
Handle_DefCreator( std::vector<Definitions::DefRec_Base_struct*>*
		   p_vecLocDefRecs, uint32_t streamid, const char* creator )
{
   static bool creator_wrote = false;

   if( !creator_wrote )
   {
      p_vecLocDefRecs->push_back( new Definitions::DefRec_DefCreator_struct(
				     creator ) );
      creator_wrote = true;
   }

   return OTF_RETURN_OK;
}

int
Handle_DefTimerResolution( std::vector<Definitions::DefRec_Base_struct*>*
			   p_vecLocDefRecs, uint32_t streamid,
			   uint64_t ticksPerSecond )
{
   static bool timerres_wrote = false;

   if( !timerres_wrote )
   {
      p_vecLocDefRecs->push_back( new Definitions::DefRec_DefTimerResolution_struct(
				     ticksPerSecond ) );
      timerres_wrote = true;

      theStatistics->setTimerRes( ticksPerSecond );
   }

   return OTF_RETURN_OK;
}

int
Handle_DefProcessGroup( std::vector<Definitions::DefRec_Base_struct*>*
			p_vecLocDefRecs, uint32_t streamid, uint32_t deftoken,
			const char* name, uint32_t n, uint32_t* array )
{
   Definitions::DefRec_DefProcessGroup_struct::ProcessGroupTypeT type;

   if( strncmp( name, "__NODE__", 8 ) == 0 )
      type = Definitions::DefRec_DefProcessGroup_struct::TYPE_NODE;
   else if( strcmp( name, "__MPI_COMM_USER__" ) == 0 )
      type = Definitions::DefRec_DefProcessGroup_struct::TYPE_MPI_COMM_USER;
   else if( strcmp( name, "__MPI_COMM_WORLD__" ) == 0 )
      type = Definitions::DefRec_DefProcessGroup_struct::TYPE_MPI_COMM_WORLD;
   else if( strcmp( name, "__MPI_COMM_SELF__" ) == 0 )
      type = Definitions::DefRec_DefProcessGroup_struct::TYPE_MPI_COMM_SELF;
   else if( strcmp( name, "__OMP_TEAM__" ) == 0 )
      type = Definitions::DefRec_DefProcessGroup_struct::TYPE_OMP_TEAM;
   else
      type = Definitions::DefRec_DefProcessGroup_struct::TYPE_OTHER;

   p_vecLocDefRecs->push_back( new Definitions::DefRec_DefProcessGroup_struct(
			streamid % 65536,
			deftoken,
			type,
			name,
			n,
			array ) );

   return OTF_RETURN_OK;
}

int
Handle_DefProcess( std::vector<Definitions::DefRec_Base_struct*>*
		   p_vecLocDefRecs, uint32_t streamid, uint32_t deftoken,
		   const char* name, uint32_t parent )
{
   p_vecLocDefRecs->push_back( new Definitions::DefRec_DefProcess_struct(
				  deftoken,
				  name,
				  parent ) );
   return OTF_RETURN_OK;
}

int
Handle_DefSclFile( std::vector<Definitions::DefRec_Base_struct*>*
		   p_vecLocDefRecs, uint32_t streamid, uint32_t deftoken,
		   const char* filename )
{
   p_vecLocDefRecs->push_back( new Definitions::DefRec_DefSclFile_struct(
				  streamid % 65536,
				  deftoken,
				  filename ) );

   return OTF_RETURN_OK;
}

int
Handle_DefScl( std::vector<Definitions::DefRec_Base_struct*>* p_vecLocDefRecs,
	       uint32_t streamid, uint32_t deftoken, uint32_t sclfile,
	       uint32_t sclline )
{
   p_vecLocDefRecs->push_back( new Definitions::DefRec_DefScl_struct(
				  streamid % 65536,
				  deftoken,
				  sclfile,
				  sclline ) );

   return OTF_RETURN_OK;
}

int
Handle_DefFileGroup( std::vector<Definitions::DefRec_Base_struct*>*
		     p_vecLocDefRecs, uint32_t streamid, uint32_t deftoken,
		     const char* name )
{
   p_vecLocDefRecs->push_back( new Definitions::DefRec_DefFileGroup_struct(
				  streamid % 65536,
				  deftoken, name ) );

   return OTF_RETURN_OK;
}

int
Handle_DefFile( std::vector<Definitions::DefRec_Base_struct*>* p_vecLocDefRecs,
		uint32_t streamid, uint32_t deftoken, const char* name,
		uint32_t group )
{
   p_vecLocDefRecs->push_back( new Definitions::DefRec_DefFile_struct(
				  streamid % 65536,
				  deftoken,
				  name,
				  group ) );

   return OTF_RETURN_OK;
}

int
Handle_DefFunctionGroup( std::vector<Definitions::DefRec_Base_struct*>*
			 p_vecLocDefRecs, uint32_t streamid, uint32_t deftoken,
			 const char* name )
{
   p_vecLocDefRecs->push_back( new Definitions::DefRec_DefFunctionGroup_struct(
				  streamid % 65536,
				  deftoken, name ) );

   return OTF_RETURN_OK;
}

int
Handle_DefFunction( std::vector<Definitions::DefRec_Base_struct*>*
		    p_vecLocDefRecs, uint32_t streamid, uint32_t deftoken,
		    const char* name, uint32_t group, uint32_t scltoken )
{
   p_vecLocDefRecs->push_back( new Definitions::DefRec_DefFunction_struct(
				  streamid % 65536,
				  deftoken,
				  name,
				  group,
				  scltoken ) );

   return OTF_RETURN_OK;
}

int
Handle_DefCollectiveOperation( std::vector<Definitions::DefRec_Base_struct*>*
			       p_vecLocDefRecs, uint32_t streamid,
			       uint32_t collOp, const char* name,
			       uint32_t type )
{
   p_vecLocDefRecs->push_back( new Definitions::DefRec_DefCollectiveOperation_struct(
				  streamid % 65536,
				  collOp,
				  name,
				  type ) );

   return OTF_RETURN_OK;
}

int
Handle_DefCounterGroup( std::vector<Definitions::DefRec_Base_struct*>*
			p_vecLocDefRecs, uint32_t streamid, uint32_t deftoken,
			const char* name )
{
   p_vecLocDefRecs->push_back( new Definitions::DefRec_DefCounterGroup_struct(
				  streamid % 65536,
				  deftoken,
				  name ) );
   
   return OTF_RETURN_OK;
}

int
Handle_DefCounter( std::vector<Definitions::DefRec_Base_struct*>*
		   p_vecLocDefRecs, uint32_t streamid, uint32_t deftoken,
		   const char* name, uint32_t properties,
		   uint32_t countergroup, const char* unit )
{
   p_vecLocDefRecs->push_back( new Definitions::DefRec_DefCounter_struct(
				  streamid % 65536,
				  deftoken,
				  name,
				  properties,
				  countergroup,
				  unit ) );

   return OTF_RETURN_OK;
}
