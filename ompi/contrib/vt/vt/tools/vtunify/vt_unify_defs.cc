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
#include "vt_unify_defs_hdlr.h"
#include "vt_unify_stats.h"
#include "vt_unify_tkfac.h"

#include "otf.h"

#include <iostream>
#include <list>
#include <map>
#include <string>
#include <vector>

#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

Definitions * theDefinitions; // instance of class Definitions

bool
LocDefsCmp( Definitions::DefRec_Base_struct * a,
            Definitions::DefRec_Base_struct * b )
{
   // both record types are Definitions::DEF_REC_TYPE__DefProcess ? ...
   //
   if( a->etype == Definitions::DEF_REC_TYPE__DefProcess &&
       b->etype == Definitions::DEF_REC_TYPE__DefProcess )
   {
      // do nothing, 'cause the process definition records will be sorted later
      return false;
   }
   // both records have the same type ? ...
   //
   else if( a->etype == b->etype &&
            ( a->etype != Definitions::DEF_REC_TYPE__DefCreator &&
              a->etype != Definitions::DEF_REC_TYPE__DefTimerResolution ) )
   {
      if( a->deftoken == b->deftoken )
      {
         // ... sort by local process id if local tokens are equal
         return a->loccpuid < b->loccpuid;
      }
      else
      {
         // ... sort by local token
         return a->deftoken < b->deftoken;
      }
   }
   // otherwise ...
   else
   {
      // ... sort by type
      return a->etype < b->etype;
   }
}

bool
GlobDefsCmp( Definitions::DefRec_Base_struct * a,
             Definitions::DefRec_Base_struct * b )
{
   // both record types are Definitions::DEF_REC_TYPE__DefProcess ? ...
   //
   if( a->etype == Definitions::DEF_REC_TYPE__DefProcess &&
       b->etype == Definitions::DEF_REC_TYPE__DefProcess )
   {
      // do nothing, 'cause the process definition records were already
      // been sorted
      return false;
   }
   // both record types are Definitions::DEF_REC_TYPE__DefProcessGroup ? ...
   //
   else if( a->etype == Definitions::DEF_REC_TYPE__DefProcessGroup &&
            b->etype == Definitions::DEF_REC_TYPE__DefProcessGroup )
   {
      Definitions::DefRec_DefProcessGroup_struct * p1 =
         static_cast<Definitions::DefRec_DefProcessGroup_struct*>(a);
      Definitions::DefRec_DefProcessGroup_struct * p2 =
         static_cast<Definitions::DefRec_DefProcessGroup_struct*>(b);

      // ... sort to this order:
      // Nodes
      // MPI_COMM_WORLD
      // MPI_COMM_SELFs
      // remaining MPI communicators
      // OpenMP Thread Teams
      // Rest

      // p1 == TYPE_NODE && p2 != TYPE_NODE
      if( p1->type == Definitions::DefRec_DefProcessGroup_struct::TYPE_NODE
	  && p2->type != Definitions::DefRec_DefProcessGroup_struct::TYPE_NODE )
      {
	 return true;
      }
      // p1 != TYPE_NODE && p2 == TYPE_NODE
      else if(
	 p1->type != Definitions::DefRec_DefProcessGroup_struct::TYPE_NODE
	 && p2->type == Definitions::DefRec_DefProcessGroup_struct::TYPE_NODE )
      {
	 return false;
      }
      // p1 == TYPE_MPI_COMM_WORLD && p2 != TYPE_MPI_COMM_WORLD
      else if( 
	 p1->type == Definitions::DefRec_DefProcessGroup_struct::TYPE_MPI_COMM_WORLD
	 && p2->type != Definitions::DefRec_DefProcessGroup_struct::TYPE_MPI_COMM_WORLD )
      {
	 return true;
      }
      // p1 != TYPE_MPI_COMM_WORLD && p2 == TYPE_MPI_COMM_WORLD
      else if(
	 p1->type != Definitions::DefRec_DefProcessGroup_struct::TYPE_MPI_COMM_WORLD
	 && p2->type == Definitions::DefRec_DefProcessGroup_struct::TYPE_MPI_COMM_WORLD )
      {
	 return false;
      }
      // p1 == TYPE_MPI_COMM_SELF && p2 != TYPE_MPI_COMM_SELF
      else if( 
	 p1->type == Definitions::DefRec_DefProcessGroup_struct::TYPE_MPI_COMM_SELF
	 && p2->type != Definitions::DefRec_DefProcessGroup_struct::TYPE_MPI_COMM_SELF )
      {
	 return true;
      }
      // p1 != TYPE_MPI_COMM_SELF && p2 == TYPE_MPI_COMM_SELF
      else if(
	 p1->type != Definitions::DefRec_DefProcessGroup_struct::TYPE_MPI_COMM_SELF
	 && p2->type == Definitions::DefRec_DefProcessGroup_struct::TYPE_MPI_COMM_SELF )
      {
	 return false;
      }
      // p1 == TYPE_MPI_COMM_USER && p2 != TYPE_MPI_COMM_USER
      else if( 
	 p1->type == Definitions::DefRec_DefProcessGroup_struct::TYPE_MPI_COMM_USER
	 && p2->type != Definitions::DefRec_DefProcessGroup_struct::TYPE_MPI_COMM_USER )
      {
	 return true;
      }
      // p1 != TYPE_MPI_COMM_USER && p2 == TYPE_MPI_COMM_USER
      else if(
	 p1->type != Definitions::DefRec_DefProcessGroup_struct::TYPE_MPI_COMM_USER
	 && p2->type == Definitions::DefRec_DefProcessGroup_struct::TYPE_MPI_COMM_USER )
      {
	 return false;
      }
      // p1 == TYPE_OMP_TEAM && p2 != TYPE_OMP_TEAM
      else if( 
	 p1->type == Definitions::DefRec_DefProcessGroup_struct::TYPE_OMP_TEAM
	 && p2->type != Definitions::DefRec_DefProcessGroup_struct::TYPE_OMP_TEAM )
      {
	 return true;
      }
      // p1 != TYPE_OMP_TEAM && p2 == TYPE_OMP_TEAM
      else if(
	 p1->type != Definitions::DefRec_DefProcessGroup_struct::TYPE_OMP_TEAM
	 && p2->type == Definitions::DefRec_DefProcessGroup_struct::TYPE_OMP_TEAM )
      {
	 return false;
      }
      else
      {	 
	 // sort by token, if process group types are equal
	 return p1->deftoken < p2->deftoken;
      }
   }
   // both record types are Definitions::DEF_REC_TYPE__DefinitionComment ? ...
   //
   else if( a->etype == Definitions::DEF_REC_TYPE__DefinitionComment &&
            b->etype == Definitions::DEF_REC_TYPE__DefinitionComment )
   {
      Definitions::DefRec_DefinitionComment_struct * c1 =
	 static_cast<Definitions::DefRec_DefinitionComment_struct*>(a);
      Definitions::DefRec_DefinitionComment_struct * c2 =
	 static_cast<Definitions::DefRec_DefinitionComment_struct*>(b);

      // ... sort by trace order
      return c1->orderidx < c2->orderidx;
   }
   // both records have the same type ? ...
   //
   else if( a->etype == b->etype &&
	    ( a->etype != Definitions::DEF_REC_TYPE__DefCreator &&
	      a->etype != Definitions::DEF_REC_TYPE__DefTimerResolution ) )
       
   {
      // ... sort by defined token
      return a->deftoken < b->deftoken;
   }
   // otherwise ...
   else
   {
      // ... sort by type
      return a->etype < b->etype;
   }
}

//////////////////// class Definitions ////////////////////

// public methods
//

Definitions::Definitions()
{
   // Empty
}

Definitions::~Definitions()
{
   // Empty
}

bool
Definitions::run()
{
   VPrint( 1, "Unifying definitions\n" );

   bool error = false;

   // allocate vector for local definitions
   std::vector<DefRec_Base_struct*> * p_vec_loc_defs =
      new std::vector<DefRec_Base_struct*>();
   assert( p_vec_loc_defs );

   // allocate vector for global definitions
   std::vector<DefRec_Base_struct*> * p_vec_glob_defs =
      new std::vector<DefRec_Base_struct*>();
   assert( p_vec_glob_defs );

   // read local definitions
   if( !readLocal( p_vec_loc_defs ) )
      error = true;

   // create global definitions
   if( !error && !createGlobal( p_vec_loc_defs, p_vec_glob_defs ) )
      error = true;

   // write global definitions
   if( !error && !writeGlobal( p_vec_glob_defs ) )
      error = true;
       
   // free local/global definition record vector
   //
   for( uint32_t i = 0; i < p_vec_loc_defs->size(); i++ )
      delete (*p_vec_loc_defs)[i];
   delete p_vec_loc_defs;
   for( uint32_t i = 0; i < p_vec_glob_defs->size(); i++ )
      delete (*p_vec_glob_defs)[i];
   delete p_vec_glob_defs;
   
   return !error;
}

// private methods
//

bool
Definitions::readLocal( std::vector<DefRec_Base_struct*> * p_vecLocDefs )
{
   VPrint( 1, " Reading local definitions\n" );

   bool error = false;

   // create record handler and set the local definition
   // record vector as first handler argument for ...
   //
   OTF_HandlerArray * p_handler_array =
      OTF_HandlerArray_open();
   assert( p_handler_array );

   // ... OTF_DEFINITIONCOMMENT_RECORD
   OTF_HandlerArray_setHandler( p_handler_array,
       (OTF_FunctionPointer*)Handle_DefinitionComment,
       OTF_DEFINITIONCOMMENT_RECORD );
   OTF_HandlerArray_setFirstHandlerArg( p_handler_array,
      p_vecLocDefs,
      OTF_DEFINITIONCOMMENT_RECORD );	

   // ... OTF_DEFCREATOR_RECORD
   OTF_HandlerArray_setHandler( p_handler_array,
      (OTF_FunctionPointer*)Handle_DefCreator,
      OTF_DEFCREATOR_RECORD );		
   OTF_HandlerArray_setFirstHandlerArg( p_handler_array,
      p_vecLocDefs,
      OTF_DEFCREATOR_RECORD );	

   // ... OTF_DEFTIMERRESOLUTION_RECORD
   OTF_HandlerArray_setHandler( p_handler_array,
      (OTF_FunctionPointer*)Handle_DefTimerResolution,
      OTF_DEFTIMERRESOLUTION_RECORD );
   OTF_HandlerArray_setFirstHandlerArg( p_handler_array,
      p_vecLocDefs,
      OTF_DEFTIMERRESOLUTION_RECORD );	

   // ... OTF_DEFPROCESSGROUP_RECORD
   OTF_HandlerArray_setHandler( p_handler_array,
      (OTF_FunctionPointer*)Handle_DefProcessGroup,
      OTF_DEFPROCESSGROUP_RECORD );
   OTF_HandlerArray_setFirstHandlerArg( p_handler_array,
      p_vecLocDefs,
      OTF_DEFPROCESSGROUP_RECORD );

   // ... OTF_DEFPROCESS_RECORD
   OTF_HandlerArray_setHandler( p_handler_array,
      (OTF_FunctionPointer*)Handle_DefProcess,
      OTF_DEFPROCESS_RECORD );
   OTF_HandlerArray_setFirstHandlerArg( p_handler_array,
      p_vecLocDefs,
      OTF_DEFPROCESS_RECORD );

   // ... OTF_DEFSCLFILE_RECORD
   OTF_HandlerArray_setHandler( p_handler_array,
      (OTF_FunctionPointer*)Handle_DefSclFile,
      OTF_DEFSCLFILE_RECORD );
   OTF_HandlerArray_setFirstHandlerArg( p_handler_array,
      p_vecLocDefs,
      OTF_DEFSCLFILE_RECORD );

   // ... OTF_DEFSCL_RECORD
   OTF_HandlerArray_setHandler( p_handler_array,
      (OTF_FunctionPointer*)Handle_DefScl,
      OTF_DEFSCL_RECORD );
   OTF_HandlerArray_setFirstHandlerArg( p_handler_array,
      p_vecLocDefs,
      OTF_DEFSCL_RECORD );

   // ... OTF_DEFFILEGROUP_RECORD
   OTF_HandlerArray_setHandler( p_handler_array,
      (OTF_FunctionPointer*)Handle_DefFileGroup,
      OTF_DEFFILEGROUP_RECORD );
   OTF_HandlerArray_setFirstHandlerArg( p_handler_array,
      p_vecLocDefs,
      OTF_DEFFILEGROUP_RECORD );

   // ... OTF_DEFFILE_RECORD
   OTF_HandlerArray_setHandler( p_handler_array,
      (OTF_FunctionPointer*)Handle_DefFile,
      OTF_DEFFILE_RECORD );
   OTF_HandlerArray_setFirstHandlerArg( p_handler_array,
      p_vecLocDefs,
      OTF_DEFFILE_RECORD );

   // ... OTF_DEFFUNCTIONGROUP_RECORD
   OTF_HandlerArray_setHandler( p_handler_array,
      (OTF_FunctionPointer*)Handle_DefFunctionGroup,
      OTF_DEFFUNCTIONGROUP_RECORD );
   OTF_HandlerArray_setFirstHandlerArg( p_handler_array,
      p_vecLocDefs,
      OTF_DEFFUNCTIONGROUP_RECORD );

   // ... OTF_DEFFUNCTION_RECORD
   OTF_HandlerArray_setHandler( p_handler_array,
      (OTF_FunctionPointer*)Handle_DefFunction,
      OTF_DEFFUNCTION_RECORD );
   OTF_HandlerArray_setFirstHandlerArg( p_handler_array,
      p_vecLocDefs,
      OTF_DEFFUNCTION_RECORD );

   // ... OTF_DEFCOLLOP_RECORD
   OTF_HandlerArray_setHandler( p_handler_array,
      (OTF_FunctionPointer*)Handle_DefCollectiveOperation,
      OTF_DEFCOLLOP_RECORD );
   OTF_HandlerArray_setFirstHandlerArg( p_handler_array,
      p_vecLocDefs,
      OTF_DEFCOLLOP_RECORD );

   // ... OTF_DEFCOUNTERGROUP_RECORD
   OTF_HandlerArray_setHandler( p_handler_array,
      (OTF_FunctionPointer*)Handle_DefCounterGroup,
      OTF_DEFCOUNTERGROUP_RECORD );
   OTF_HandlerArray_setFirstHandlerArg( p_handler_array,
      p_vecLocDefs,
      OTF_DEFCOUNTERGROUP_RECORD );

   // ... OTF_DEFCOUNTER_RECORD
   OTF_HandlerArray_setHandler( p_handler_array,
      (OTF_FunctionPointer*)Handle_DefCounter,
      OTF_DEFCOUNTER_RECORD );
   OTF_HandlerArray_setFirstHandlerArg( p_handler_array,
      p_vecLocDefs,
      OTF_DEFCOUNTER_RECORD );

   // read local definitions
   //
   for( uint32_t i = 0; i < g_vecUnifyCtls.size(); i++ )
   {
      // open file manager for reader stream
      OTF_FileManager * p_loc_def_manager 
	 = OTF_FileManager_open( 1 );
      assert( p_loc_def_manager );

      // open stream for reading
      OTF_RStream * p_loc_def_rstream =
	 OTF_RStream_open( Params.in_file_prefix.c_str(),
			   g_vecUnifyCtls[i]->streamid,
			   p_loc_def_manager );
      assert( p_loc_def_rstream );

      VPrint( 2, "  Opened OTF reader stream [namestub %s id %x]\n",
	      Params.in_file_prefix.c_str(),
	      g_vecUnifyCtls[i]->streamid );

      if( !OTF_RStream_getDefBuffer( p_loc_def_rstream ) )
      {
	 VPrint( 2, "   No definitions found in this OTF reader stream "
		    "- Ignored\n" );
      }
      else
      {
	 // close definitions buffer
	 OTF_RStream_closeDefBuffer( p_loc_def_rstream );

	 // read definitions
	 if( OTF_RStream_readDefinitions( p_loc_def_rstream, p_handler_array )
	     == OTF_READ_ERROR )
	 {
	    std::cerr << ExeName << ": Error: "
		      << "Could not read definitions of OTF stream [namestub "
		      << Params.in_file_prefix << " id "
		      << std::hex << g_vecUnifyCtls[i]->streamid << "]"
		      << std::dec << std::endl;
	    error = true;
	 }
      }

      // close reader stream 
      OTF_RStream_close( p_loc_def_rstream );
      // close file manager for reader stream
      OTF_FileManager_close( p_loc_def_manager );
      
      VPrint( 2, "  Closed OTF reader stream [namestub %s id %x]\n",
	      Params.in_file_prefix.c_str(),
	      g_vecUnifyCtls[i]->streamid );

      if( error ) break;
   }

   // close record handler
   OTF_HandlerArray_close( p_handler_array );

   if( error )
   {
      std::cerr << ExeName << ": "
		<< "An error occurred during unifying definitions. Aborting"
		<< std::endl;
   }
   else
   {
      // sort local definitions
      std::stable_sort( p_vecLocDefs->begin(), p_vecLocDefs->end(),
                        LocDefsCmp );
   }

   return !error;
}

bool
Definitions::createGlobal( const std::vector<DefRec_Base_struct*> *
			   p_vecLocDefs,
			   std::vector<DefRec_Base_struct*> * p_vecGlobDefs )
{
   VPrint( 1, " Creating global definitions\n" );

   assert( p_vecLocDefs->size() > 0 );

   bool error = false;

   uint32_t omp_comm_idx = 0;
   uint32_t mpi_comm_self_idx = 0;

   // if available, write definition comment records for start-time, stop-time,
   // and elapsed time
   //
   if( g_uMinStartTimeEpoch != (uint64_t)-1 && g_uMaxStopTimeEpoch > 0 )
   {
      time_t tt;
      struct tm elapsed_tm;
      char comment[STRBUFSIZE];

      // headline for time comments
      //
      p_vecGlobDefs->push_back( new DefRec_DefinitionComment_struct(
				   0,
				   "Trace Times:" ) );

      // start-time 
      //
      tt = (time_t)(g_uMinStartTimeEpoch / 1e6);
      snprintf( comment, sizeof( comment ) - 1, " Start: %s (%llu)",
                asctime(localtime(&tt)),
                (unsigned long long)g_uMinStartTimeEpoch );
      p_vecGlobDefs->push_back( new DefRec_DefinitionComment_struct(
				   1,
				   comment ) );

      // stop-time
      //
      tt = (time_t)(g_uMaxStopTimeEpoch / 1e6);
      snprintf( comment, sizeof( comment ) - 1, " Stop: %s (%llu)",
                asctime(localtime(&tt)),
                (unsigned long long)g_uMaxStopTimeEpoch );
      p_vecGlobDefs->push_back( new DefRec_DefinitionComment_struct(
				   2,
				   comment ) );

      // elapsed time
      //
      tt = (time_t)((g_uMaxStopTimeEpoch - g_uMinStartTimeEpoch) / 1e6);
      gmtime_r(&tt, &elapsed_tm);
      snprintf( comment, sizeof( comment ) - 1,
         " Elapsed: %s%d:%s%d:%s%d (%llu)",
         elapsed_tm.tm_hour < 10 ? "0" : "", elapsed_tm.tm_hour,
         elapsed_tm.tm_min  < 10 ? "0" : "", elapsed_tm.tm_min,
         elapsed_tm.tm_sec  < 10 ? "0" : "", elapsed_tm.tm_sec,
         (unsigned long long)(g_uMaxStopTimeEpoch - g_uMinStartTimeEpoch) );
      p_vecGlobDefs->push_back( new DefRec_DefinitionComment_struct(
				   3,
				   comment ) );
   }

   for( uint32_t i = 0; i < p_vecLocDefs->size(); i++ )
   {
      switch( (*p_vecLocDefs)[i]->etype )
      {
	 // DefinitionComment
	 //
         case DEF_REC_TYPE__DefinitionComment:
	 {
	    // get local definition entry
	    DefRec_DefinitionComment_struct * p_loc_def_entry =
	       (DefRec_DefinitionComment_struct*)((*p_vecLocDefs)[i]);

            addDefComment( p_loc_def_entry );

	    break;
	 }
	 // DefCreator
	 //
         case DEF_REC_TYPE__DefCreator:
	 {
	    // get local definition entry
	    DefRec_DefCreator_struct * p_loc_def_entry =
	       (DefRec_DefCreator_struct*)((*p_vecLocDefs)[i]);

	    // add definition without any changes to vector of
	    // global definitions
	    p_vecGlobDefs->push_back( new DefRec_DefCreator_struct(
					 p_loc_def_entry->creator ) );
	    
	    break;
	 }
	 // DefTimerResolution
	 //
         case DEF_REC_TYPE__DefTimerResolution:
	 {
	    // get local definition entry
	    DefRec_DefTimerResolution_struct * p_loc_def_entry =
	       (DefRec_DefTimerResolution_struct*)((*p_vecLocDefs)[i]);

	    // add definition without any changes to vector of
	    // global definitions
	    p_vecGlobDefs->push_back( new DefRec_DefTimerResolution_struct(
					 p_loc_def_entry->ticksPerSecond ) );

	       break;
	 }
	 // DefProcess
	 //
         case DEF_REC_TYPE__DefProcess:
	 {
	    // get local definition entry
	    DefRec_DefProcess_struct *p_loc_def_entry =
	       (DefRec_DefProcess_struct*)((*p_vecLocDefs)[i]);

            addDefProcess( p_loc_def_entry );

	    break;
	 }
	 // DefProcessGroup
	 //
         case DEF_REC_TYPE__DefProcessGroup:
	 {
	    // get local definition entry
	    DefRec_DefProcessGroup_struct * p_loc_def_entry =
	       (DefRec_DefProcessGroup_struct*)((*p_vecLocDefs)[i]);

	    // node definition?
	    if( p_loc_def_entry->type ==
		DefRec_DefProcessGroup_struct::TYPE_NODE )
	    {
	       addProc2NodeGroup( p_loc_def_entry->name.substr(9),
				  p_loc_def_entry->members[0] );
	    }
	    // MPI communicator (except MPI_COMM_WORLD and MPI_COMM_SELF)
	    else if( p_loc_def_entry->type ==
		     DefRec_DefProcessGroup_struct::TYPE_MPI_COMM_USER )
	    {
	       addMPIComm( p_loc_def_entry->loccpuid,
			   p_loc_def_entry->deftoken,
			   p_loc_def_entry->members );
	    }
	    else
	    {
	       // get global token factory for this definition type
	       TokenFactory_DefProcessGroup * p_tkfac_defprocessgroup =
		  static_cast<TokenFactory_DefProcessGroup*>(theTokenFactory[TKFAC__DEF_PROCESS_GROUP]);
	       
	       // get global token
	       uint32_t global_token =
		  p_tkfac_defprocessgroup->getGlobalToken(
		     p_loc_def_entry->name,
		     p_loc_def_entry->members );
	       
	       // global token found ?
	       if( global_token == 0 )
	       {
		  // no -> create it
		  global_token =
		     p_tkfac_defprocessgroup->createGlobalToken(
			p_loc_def_entry->loccpuid,
			p_loc_def_entry->deftoken,
			p_loc_def_entry->name,
			p_loc_def_entry->members );

		  char new_name[256];
		  if( p_loc_def_entry->type ==
		      DefRec_DefProcessGroup_struct::TYPE_OMP_TEAM )
		  {
		     snprintf( new_name, sizeof( new_name ) - 1,
			       "OMP Thread Team %d", omp_comm_idx++ );
		  }
		  else if( p_loc_def_entry->type ==
			   DefRec_DefProcessGroup_struct::TYPE_MPI_COMM_WORLD )
		  {
		     strncpy( new_name, "MPI_COMM_WORLD",
			      sizeof( new_name ) - 1 );
		     new_name[sizeof( new_name ) - 1] = '\0';
		  }
		  else if( p_loc_def_entry->type ==
			   DefRec_DefProcessGroup_struct::TYPE_MPI_COMM_SELF )
		  {
		     snprintf( new_name, sizeof( new_name ) - 1,
			       "MPI_COMM_SELF %d", mpi_comm_self_idx++ );
		  }
		  else
		  {
		     strncpy( new_name, p_loc_def_entry->name.c_str(),
			      sizeof( new_name ) - 1 );
		     new_name[sizeof( new_name ) - 1] = '\0';
		  }

		  // add new definition to vector of global definitions
		  p_vecGlobDefs->push_back( new DefRec_DefProcessGroup_struct(
					       0,
					       global_token,
					       p_loc_def_entry->type,
					       new_name,
					       p_loc_def_entry->members ) );
	       }
	       else
	       {
		  // yes -> (global definition already exists in vector)
		  
		  // set translation for this local process id, if necessary
		  //
		  if( p_tkfac_defprocessgroup->translateLocalToken(
			 p_loc_def_entry->loccpuid,
			 p_loc_def_entry->deftoken ) == 0 )
		  {
		     p_tkfac_defprocessgroup->setTranslation(
			p_loc_def_entry->loccpuid,
			p_loc_def_entry->deftoken,
			global_token );
		  }
	       }
	    }

	    break;
	 }
	 // DefSclFile
	 //
         case DEF_REC_TYPE__DefSclFile:
	 {
	    // get local definition entry
	    DefRec_DefSclFile_struct * p_loc_def_entry =
	       (DefRec_DefSclFile_struct*)((*p_vecLocDefs)[i]);

	    // get global token factory for this definition type
	    TokenFactory_DefSclFile * p_tkfac_defsclfile =
	       static_cast<TokenFactory_DefSclFile*>(theTokenFactory[TKFAC__DEF_SCL_FILE]);

	    // get global token
	    uint32_t global_token =
	       p_tkfac_defsclfile->getGlobalToken(
		  p_loc_def_entry->filename );

	    // global token found ?
	    if( global_token == 0 )
	    {
	       // no -> create it
	       global_token = 
		  p_tkfac_defsclfile->createGlobalToken(
		     p_loc_def_entry->loccpuid,
		     p_loc_def_entry->deftoken,
		     p_loc_def_entry->filename );

	       // add new definition to vector of global definitions
	       p_vecGlobDefs->push_back( new DefRec_DefSclFile_struct(
					    0,
					    global_token,
					    p_loc_def_entry->filename ) );
	    }
	    else
	    {
	       // yes -> (global definition already exists in vector)

	       // set translation for this local process id, if necessary
	       //
	       if( p_tkfac_defsclfile->translateLocalToken(
		      p_loc_def_entry->loccpuid,
		      p_loc_def_entry->deftoken ) == 0 )
	       {
		  p_tkfac_defsclfile->setTranslation(
		     p_loc_def_entry->loccpuid,
		     p_loc_def_entry->deftoken,
		     global_token );
	       }
	    }

	    break;
	 }
	 // DefScl
	 //
         case DEF_REC_TYPE__DefScl:
	 {
	    // get local definition entry
	    DefRec_DefScl_struct * p_loc_def_entry =
	       (DefRec_DefScl_struct*)((*p_vecLocDefs)[i]);

	    // get global token factory for DefSclFile
	    TokenFactory_DefSclFile * p_tkfac_defsclfile =
	       static_cast<TokenFactory_DefSclFile*>(theTokenFactory[TKFAC__DEF_SCL_FILE]);

	    // get global token factory for this definition type
	    TokenFactory_DefScl * p_tkfac_defscl = 
	       static_cast<TokenFactory_DefScl*>(theTokenFactory[TKFAC__DEF_SCL]);

	    // get global token for DefSclFile (exit if not exists)
	    uint32_t global_sclfile =
	       p_tkfac_defsclfile->translateLocalToken(
		  p_loc_def_entry->loccpuid,
		  p_loc_def_entry->sclfile );
	    assert( global_sclfile != 0 );

	    // get global token
	    uint32_t global_token =
	       p_tkfac_defscl->getGlobalToken( global_sclfile,
					       p_loc_def_entry->sclline );

	    // global token found ?
	    if( global_token == 0 )
	    {
	       // no -> create it
	       global_token =
		  p_tkfac_defscl->createGlobalToken( p_loc_def_entry->loccpuid,
						     p_loc_def_entry->deftoken,
						     global_sclfile,
						     p_loc_def_entry->sclline );

	       // add new definition to vector of global definitions
	       p_vecGlobDefs->push_back( new DefRec_DefScl_struct(
					    0,
					    global_token,
					    global_sclfile,
					    p_loc_def_entry->sclline ) );
	    }
	    else
	    {
	       // yes -> (global definition already exists in vector)

	       // set translation for this local process id, if necessary
	       //
	       if( p_tkfac_defscl->translateLocalToken(
		      p_loc_def_entry->loccpuid,
		      p_loc_def_entry->deftoken ) == 0 )
	       {
		  p_tkfac_defscl->setTranslation(
		     p_loc_def_entry->loccpuid,
		     p_loc_def_entry->deftoken,
		     global_token );
	       }
	    }

	    break;
	 }
	 // DefFileGroup
	 //
         case DEF_REC_TYPE__DefFileGroup:
	 {
	    // get local definition entry
	    DefRec_DefFileGroup_struct * p_loc_def_entry =
	       (DefRec_DefFileGroup_struct*)((*p_vecLocDefs)[i]);

	    // get global token factory for this definition type
	    TokenFactory_DefFileGroup * p_tkfac_deffilegroup = 
	       static_cast<TokenFactory_DefFileGroup*>(theTokenFactory[TKFAC__DEF_FILE_GROUP]);

	    // get global token
	    uint32_t global_token =
	       p_tkfac_deffilegroup->getGlobalToken(
		  p_loc_def_entry->name );

	    // global token found ?
	    if( global_token == 0 )
	    {
	       // no -> create it
	       global_token = 
		  p_tkfac_deffilegroup->createGlobalToken(
		     p_loc_def_entry->loccpuid,
		     p_loc_def_entry->deftoken,
		     p_loc_def_entry->name );

	       // add new definition to vector of global definitions
	       p_vecGlobDefs->push_back( new DefRec_DefFileGroup_struct(
					    0,
					    global_token,
					    p_loc_def_entry->name ) );
	    }
	    else
	    {
	       // yes -> (global definition already exists in vector)

	       // set translation for this local process id, if necessary
	       //
	       if( p_tkfac_deffilegroup->translateLocalToken(
		      p_loc_def_entry->loccpuid,
		      p_loc_def_entry->deftoken ) == 0 )
	       {
		  p_tkfac_deffilegroup->setTranslation(
		     p_loc_def_entry->loccpuid,
		     p_loc_def_entry->deftoken,
		     global_token );
	       }
	    }

	    break;
	 }
	 // DefFile
	 //
	 case DEF_REC_TYPE__DefFile:
	 {
	    // get local definition entry
	    DefRec_DefFile_struct * p_loc_def_entry =
	       (DefRec_DefFile_struct*)((*p_vecLocDefs)[i]);

	    // get global token factory for DefFileGroup
	    TokenFactory_DefFileGroup * p_tkfac_deffilegroup = 
	       static_cast<TokenFactory_DefFileGroup*>(theTokenFactory[TKFAC__DEF_FILE_GROUP]);

	    // get global token factory for this definition type
	    TokenFactory_DefFile * p_tkfac_deffile =
	       static_cast<TokenFactory_DefFile*>(theTokenFactory[TKFAC__DEF_FILE]);

	    // get global token for DefFileGroup (exit if not exists)
	    uint32_t global_group =
	       p_tkfac_deffilegroup->translateLocalToken(
		  p_loc_def_entry->loccpuid,
		  p_loc_def_entry->group );
	    assert( global_group != 0 );

	    // get global token
	    uint32_t global_token =
	       p_tkfac_deffile->getGlobalToken(
		  p_loc_def_entry->name,
		  global_group );

	    // global token found ?
	    if( global_token == 0 )
	    {
	       // no -> create it
	       global_token =
		  p_tkfac_deffile->createGlobalToken(
		     p_loc_def_entry->loccpuid,
		     p_loc_def_entry->deftoken,
		     p_loc_def_entry->name,
		     global_group );

	       // add new definition to vector of global definitions
	       p_vecGlobDefs->push_back( new DefRec_DefFile_struct(
					    0,
					    global_token,
					    p_loc_def_entry->name,
					    global_group ) );
	    }
	    else
	    {
	       // yes -> (global definition already exists in vector)

	       // set translation for this local process id, if necessary
	       //
	       if( p_tkfac_deffile->translateLocalToken(
		      p_loc_def_entry->loccpuid,
		      p_loc_def_entry->deftoken ) == 0 )
	       {
		  p_tkfac_deffile->setTranslation(
		     p_loc_def_entry->loccpuid,
		     p_loc_def_entry->deftoken,
		     global_token );
	       }
	    }

	    break;
	 }
	 // DefFunctionGroup
	 //
         case DEF_REC_TYPE__DefFunctionGroup:
	 {
	    // get local definition entry
	    DefRec_DefFunctionGroup_struct * p_loc_def_entry =
	       (DefRec_DefFunctionGroup_struct*)((*p_vecLocDefs)[i]);

	    // get global token factory for this definition type
	    TokenFactory_DefFunctionGroup * p_tkfac_deffunctiongroup = 
	       static_cast<TokenFactory_DefFunctionGroup*>(theTokenFactory[TKFAC__DEF_FUNCTION_GROUP]);

	    // get global token
	    uint32_t global_token =
	       p_tkfac_deffunctiongroup->getGlobalToken(
		  p_loc_def_entry->name );

	    // global token found ?
	    if( global_token == 0 )
	    {
	       // no -> create it
	       global_token = 
		  p_tkfac_deffunctiongroup->createGlobalToken(
		     p_loc_def_entry->loccpuid,
		     p_loc_def_entry->deftoken,
		     p_loc_def_entry->name );

	       // add new definition to vector of global definitions
	       p_vecGlobDefs->push_back( new DefRec_DefFunctionGroup_struct(
					    0,
					    global_token,
					    p_loc_def_entry->name ) );
	    }
	    else
	    {
	       // yes -> (global definition already exists in vector)

	       // set translation for this local process id, if necessary
	       //
	       if( p_tkfac_deffunctiongroup->translateLocalToken(
		      p_loc_def_entry->loccpuid,
		      p_loc_def_entry->deftoken ) == 0 )
	       {
		  p_tkfac_deffunctiongroup->setTranslation(
		     p_loc_def_entry->loccpuid,
		     p_loc_def_entry->deftoken,
		     global_token );
	       }
	    }

	    break;
	 }
	 // DefFunction
	 //
         case DEF_REC_TYPE__DefFunction:
	 {
	    // get local definition entry
	    DefRec_DefFunction_struct * p_loc_def_entry =
	       (DefRec_DefFunction_struct*)((*p_vecLocDefs)[i]);

	    // get global token factory for DefFunctionGroup
	    TokenFactory_DefFunctionGroup * p_tkfac_deffunctiongroup = 
	       static_cast<TokenFactory_DefFunctionGroup*>(theTokenFactory[TKFAC__DEF_FUNCTION_GROUP]);

	    // get global token factory for DefScl
	    TokenFactory_DefScl * p_tkfac_defscl = 
	       static_cast<TokenFactory_DefScl*>(theTokenFactory[TKFAC__DEF_SCL]);

	    // get global token factory for this definition type
	    TokenFactory_DefFunction * p_tkfac_deffunction =
	       static_cast<TokenFactory_DefFunction*>(theTokenFactory[TKFAC__DEF_FUNCTION]);

	    // get global token for DefFunctionGroup (exit if not exists)
	    uint32_t global_group =
	       p_tkfac_deffunctiongroup->translateLocalToken(
		  p_loc_def_entry->loccpuid,
		  p_loc_def_entry->group );
	    assert( global_group != 0 );

	    // get global token for DefScl (exit if not exists)
	    uint32_t global_scltoken = p_loc_def_entry->scltoken;
	    if( p_loc_def_entry->scltoken != 0 )
	    {
	       global_scltoken = 
		  p_tkfac_defscl->translateLocalToken(
		     p_loc_def_entry->loccpuid,
		     p_loc_def_entry->scltoken );
	       assert( global_scltoken != 0 );
	    }

	    // get global token
	    uint32_t global_token =
	       p_tkfac_deffunction->getGlobalToken(
		  p_loc_def_entry->name,
		  global_group,
		  global_scltoken );

	    // global token found ?
	    if( global_token == 0 )
	    {
	       // no -> create it
	       global_token =
		  p_tkfac_deffunction->createGlobalToken(
		     p_loc_def_entry->loccpuid,
		     p_loc_def_entry->deftoken,
		     p_loc_def_entry->name,
		     global_group,
		     global_scltoken );

	       // add new definition to vector of global definitions
	       p_vecGlobDefs->push_back( new DefRec_DefFunction_struct(
					    0,
					    global_token,
					    p_loc_def_entry->name,
					    global_group,
					    global_scltoken ) );

	       // add new function definition to statistics
	       theStatistics->addFunc( global_token,
				       std::string( p_loc_def_entry->name ) );
	    }
	    else
	    {
	       // yes -> (global definition already exists in vector)

	       // set translation for this local process id, if necessary
	       //
	       if( p_tkfac_deffunction->translateLocalToken(
		      p_loc_def_entry->loccpuid,
		      p_loc_def_entry->deftoken ) == 0 )
	       {
		  p_tkfac_deffunction->setTranslation(
		     p_loc_def_entry->loccpuid,
		     p_loc_def_entry->deftoken,
		     global_token );
	       }
	    }

	    break;
	 }
	 // DefCollectiveOperation
	 //
         case DEF_REC_TYPE__DefCollectiveOperation:
	 {
	    // get local definition entry
	    DefRec_DefCollectiveOperation_struct * p_loc_def_entry =
	       (DefRec_DefCollectiveOperation_struct*)((*p_vecLocDefs)[i]);

	    // get global token factory for this definition type
	    TokenFactory_DefCollectiveOperation * p_tkfac_defcollop =
	       static_cast<TokenFactory_DefCollectiveOperation*>(theTokenFactory[TKFAC__DEF_COLL_OP]);

	    // get global token
	    uint32_t global_token =
	       p_tkfac_defcollop->getGlobalToken(
		  p_loc_def_entry->name,
		  p_loc_def_entry->type );

	    // global token found ?
	    if( global_token == 0 )
	    {
	       // no -> create it
	       global_token =
		  p_tkfac_defcollop->createGlobalToken(
		     p_loc_def_entry->loccpuid,
		     p_loc_def_entry->deftoken,
		     p_loc_def_entry->name,
		     p_loc_def_entry->type );

	       // add new definition to vector of global definitions
	       p_vecGlobDefs->push_back( new DefRec_DefCollectiveOperation_struct(
					    0,
					    global_token,
					    p_loc_def_entry->name,
					    p_loc_def_entry->type ) );
	    }
	    else
	    {
	       // yes -> (global definition already exists in vector)

	       // set translation for this local process id, if necessary
	       //
	       if( p_tkfac_defcollop->translateLocalToken(
		      p_loc_def_entry->loccpuid,
		      p_loc_def_entry->deftoken ) == 0 )
	       {
		  p_tkfac_defcollop->setTranslation(
		     p_loc_def_entry->loccpuid,
		     p_loc_def_entry->deftoken,
		     global_token );
	       }
	    }

	    break;
	 }
	 // DefCounterGroup
	 //
         case DEF_REC_TYPE__DefCounterGroup:
	 {
	    // get local definition entry
	    DefRec_DefCounterGroup_struct * p_loc_def_entry =
	       (DefRec_DefCounterGroup_struct*)((*p_vecLocDefs)[i]);

	    // get global token factory for this definition type
	    TokenFactory_DefCounterGroup * p_tkfac_defcountergroup =
	       static_cast<TokenFactory_DefCounterGroup*>(theTokenFactory[TKFAC__DEF_COUNTER_GROUP]);

	    // get global token
	    uint32_t global_token =
	       p_tkfac_defcountergroup->getGlobalToken(
		  p_loc_def_entry->name );

	    // global token found ?
	    if( global_token == 0 )
	    {
	       // no -> create it
	       global_token = 
		  p_tkfac_defcountergroup->createGlobalToken(
		     p_loc_def_entry->loccpuid,
		     p_loc_def_entry->deftoken,
		     p_loc_def_entry->name );
	       
	       // add new definition to vector of global definitions
	       p_vecGlobDefs->push_back( new DefRec_DefCounterGroup_struct(
					    0,
					    global_token,
					    p_loc_def_entry->name ) );
	    }
	    else
	    {
	       // yes -> (global definition already exists in vector)
	       
	       // set translation for this local process id, if necessary
	       //
	       if( p_tkfac_defcountergroup->translateLocalToken(
		      p_loc_def_entry->loccpuid,
		      p_loc_def_entry->deftoken ) == 0 )
	       {
		  p_tkfac_defcountergroup->setTranslation(
		     p_loc_def_entry->loccpuid,
		     p_loc_def_entry->deftoken,
		     global_token );
	       }
	    }

	    break;
	 }
	 // DefCounter
	 //
         case DEF_REC_TYPE__DefCounter:
	 {
	    // get local definition entry
	    DefRec_DefCounter_struct * p_loc_def_entry =
	       (DefRec_DefCounter_struct*)((*p_vecLocDefs)[i]);

	    // get global token factory for DefCounterGroup
	    TokenFactory_DefCounterGroup * p_tkfac_defcountergroup =
	       static_cast<TokenFactory_DefCounterGroup*>(theTokenFactory[TKFAC__DEF_COUNTER_GROUP]);

	    // get global token factory for this definition type
	    TokenFactory_DefCounter * p_tkfac_defcounter = 
	       static_cast<TokenFactory_DefCounter*>(theTokenFactory[TKFAC__DEF_COUNTER]);

	    // get global token for DefCounterGroup (exit if not exists)
	    uint32_t global_countergroup =
	       p_tkfac_defcountergroup->translateLocalToken(
		  p_loc_def_entry->loccpuid,
		  p_loc_def_entry->countergroup );
	    assert( global_countergroup != 0 );

	    // get global token
	    uint32_t global_token =
	       p_tkfac_defcounter->getGlobalToken(
		  p_loc_def_entry->name,
		  p_loc_def_entry->properties,
		  global_countergroup,
		  p_loc_def_entry->unit );

	    // global token found ?
	    if( global_token == 0 )
	    {
	       // no -> create it
	       global_token =
		  p_tkfac_defcounter->createGlobalToken(
		     p_loc_def_entry->loccpuid,
		     p_loc_def_entry->deftoken,
		     p_loc_def_entry->name,
		     p_loc_def_entry->properties,
		     global_countergroup,
		     p_loc_def_entry->unit );

	       // add new definition to vector of global definitions
	       p_vecGlobDefs->push_back( new DefRec_DefCounter_struct(
					    0,
					    global_token,
					    p_loc_def_entry->name,
					    p_loc_def_entry->properties,
					    global_countergroup,
					    p_loc_def_entry->unit ) );
	    }
	    else
	    {
	       // yes -> (global definition already exists in vector)

	       // set translation for this local process id, if necessary
	       //
	       if( p_tkfac_defcounter->translateLocalToken(
		      p_loc_def_entry->loccpuid,
		      p_loc_def_entry->deftoken ) == 0 )
	       {
		  p_tkfac_defcounter->setTranslation(
		     p_loc_def_entry->loccpuid,
		     p_loc_def_entry->deftoken,
		     global_token );
	       }
	    }

	    break;
	 }
         default: // DEF_REC_TYPE__Unknown
	 {
	    assert( 0 );
	 }
      }
   }

   // add definition comments to global definition records
   addDefComments2Global( p_vecGlobDefs );

   // add processes to global definition records
   addDefProcesses2Global( p_vecGlobDefs );

   // add process groups for nodes to global definition records
   addNodeGroups2Global( p_vecGlobDefs );

   // add process groups for MPI communicators to global def. records
   addMPIComms2Global( p_vecGlobDefs );

   // sort global definition records
   std::stable_sort( p_vecGlobDefs->begin(), p_vecGlobDefs->end(),
                     GlobDefsCmp );

   return !error;
}

bool
Definitions::writeGlobal( const std::vector<DefRec_Base_struct*> *
			  p_vecGlobDefs )
{
   VPrint( 1, " Writing global definitions\n" );

   assert( p_vecGlobDefs->size() > 0 );

   bool error = false;

   std::string tmp_out_file_prefix =
      Params.out_file_prefix + TmpFileSuffix;

   // open file manager for writer stream
   OTF_FileManager * p_glob_def_manager 
      = OTF_FileManager_open( 1 );
   assert( p_glob_def_manager );

   // open stream for writing (stream id = 0)
   OTF_WStream * p_glob_def_wstream =
      OTF_WStream_open( tmp_out_file_prefix.c_str(), 0, p_glob_def_manager );
   assert( p_glob_def_wstream );

   // set file compression
   if( Params.docompress )
   {
      OTF_WStream_setCompression( p_glob_def_wstream,
				  OTF_FILECOMPRESSION_COMPRESSED );
   }

   // try to get def. buffer
   if( !OTF_WStream_getDefBuffer( p_glob_def_wstream ) )
   {
      std::cerr << ExeName << ": Error: "
		<< "Could not open OTF writer stream [namestub "
		<< tmp_out_file_prefix.c_str() << " id 0]" << std::endl;
      OTF_WStream_close( p_glob_def_wstream );
      OTF_FileManager_close( p_glob_def_manager );
      return false;
   }

   VPrint( 2, "  Opened OTF writer stream [namestub %s id 0]\n",
	   tmp_out_file_prefix.c_str() );

   // write OTF version record
   OTF_WStream_writeOtfVersion( p_glob_def_wstream );

   // write global definition records
   //
   for( uint32_t i = 0; i < p_vecGlobDefs->size(); i++ )
   {
      switch( (*p_vecGlobDefs)[i]->etype )
      {
         case DEF_REC_TYPE__DefinitionComment:
	 {
	    DefRec_DefinitionComment_struct * p_entry =
	       (DefRec_DefinitionComment_struct*)((*p_vecGlobDefs)[i]);

	    OTF_WStream_writeDefinitionComment( p_glob_def_wstream,
               p_entry->comment.c_str() );

	    break;
	 }
         case DEF_REC_TYPE__DefCreator:
	 {
	    DefRec_DefCreator_struct * p_entry =
	       (DefRec_DefCreator_struct*)((*p_vecGlobDefs)[i]);

	    OTF_WStream_writeDefCreator( p_glob_def_wstream,
               p_entry->creator.c_str() );

	    break;
	 }
         case DEF_REC_TYPE__DefTimerResolution:
	 {
	    DefRec_DefTimerResolution_struct * p_entry =
	       (DefRec_DefTimerResolution_struct*)((*p_vecGlobDefs)[i]);

	    OTF_WStream_writeDefTimerResolution( p_glob_def_wstream,
	       p_entry->ticksPerSecond );

	    break;
	 }
         case DEF_REC_TYPE__DefProcess:
	 {
	    DefRec_DefProcess_struct *p_entry =
	       (DefRec_DefProcess_struct*)((*p_vecGlobDefs)[i]);

	    OTF_WStream_writeDefProcess( p_glob_def_wstream,
	       p_entry->deftoken,
	       p_entry->name.c_str(),
	       p_entry->parent );

	    break;
	 }
         case DEF_REC_TYPE__DefProcessGroup:
	 {
	    DefRec_DefProcessGroup_struct * p_entry =
	       (DefRec_DefProcessGroup_struct*)((*p_vecGlobDefs)[i]);

	    uint32_t n = p_entry->members.size();
	    uint32_t * array = new uint32_t[n];
	    for( uint32_t j = 0; j < n; j++ )
	       array[j] = p_entry->members[j];

	    OTF_WStream_writeDefProcessGroup( p_glob_def_wstream,
	       p_entry->deftoken,
	       p_entry->name.c_str(),
	       n,
	       array );

	    delete[] array;

	    break;
	 }
         case DEF_REC_TYPE__DefSclFile:
	 {
	    DefRec_DefSclFile_struct * p_entry =
	       (DefRec_DefSclFile_struct*)((*p_vecGlobDefs)[i]);

	    OTF_WStream_writeDefSclFile( p_glob_def_wstream,
	       p_entry->deftoken,
	       p_entry->filename.c_str() );

	    break;
	 }
         case DEF_REC_TYPE__DefScl:
	 {
	    DefRec_DefScl_struct * p_entry =
	       (DefRec_DefScl_struct*)((*p_vecGlobDefs)[i]);

	    OTF_WStream_writeDefScl( p_glob_def_wstream,
	       p_entry->deftoken,
	       p_entry->sclfile,
	       p_entry->sclline );

	    break;
	 }
         case DEF_REC_TYPE__DefFileGroup:
	 {
	    DefRec_DefFileGroup_struct * p_entry =
	       (DefRec_DefFileGroup_struct*)((*p_vecGlobDefs)[i]);

	    OTF_WStream_writeDefFileGroup( p_glob_def_wstream,
	       p_entry->deftoken,
	       p_entry->name.c_str() );

	    break;
	 }
         case DEF_REC_TYPE__DefFile:
	 {
	    DefRec_DefFile_struct * p_entry =
	       (DefRec_DefFile_struct*)((*p_vecGlobDefs)[i]);

	    OTF_WStream_writeDefFile( p_glob_def_wstream,
	       p_entry->deftoken,
	       p_entry->name.c_str(),
	       p_entry->group );

	    break;
	 }
         case DEF_REC_TYPE__DefFunctionGroup:
	 {
	    DefRec_DefFunctionGroup_struct * p_entry =
	       (DefRec_DefFunctionGroup_struct*)((*p_vecGlobDefs)[i]);
	    
	    OTF_WStream_writeDefFunctionGroup( p_glob_def_wstream,
	       p_entry->deftoken,
	       p_entry->name.c_str() );

	    break;
	 }
         case DEF_REC_TYPE__DefFunction:
	 { 
	    DefRec_DefFunction_struct * p_entry =
	       (DefRec_DefFunction_struct*)((*p_vecGlobDefs)[i]);

	    OTF_WStream_writeDefFunction( p_glob_def_wstream,
	       p_entry->deftoken,
	       p_entry->name.c_str(),
	       p_entry->group,
	       p_entry->scltoken );

	    break;
	 }
         case DEF_REC_TYPE__DefCollectiveOperation:
	 {
	    DefRec_DefCollectiveOperation_struct * p_entry =
	       (DefRec_DefCollectiveOperation_struct*)((*p_vecGlobDefs)[i]);

	    OTF_WStream_writeDefCollectiveOperation( p_glob_def_wstream,
	       p_entry->deftoken,
	       p_entry->name.c_str(),
	       p_entry->type );

	    break;
	 }
         case DEF_REC_TYPE__DefCounterGroup:
	 {
	    DefRec_DefCounterGroup_struct * p_entry =
	       (DefRec_DefCounterGroup_struct*)((*p_vecGlobDefs)[i]);

	    OTF_WStream_writeDefCounterGroup( p_glob_def_wstream,
	       p_entry->deftoken,
	       p_entry->name.c_str() );

	    break;
	 }
         case DEF_REC_TYPE__DefCounter:
	 {
	    DefRec_DefCounter_struct * p_entry =
	       (DefRec_DefCounter_struct*)((*p_vecGlobDefs)[i]);
	    
	    OTF_WStream_writeDefCounter( p_glob_def_wstream,
	       p_entry->deftoken,
	       p_entry->name.c_str(),
	       p_entry->properties,
	       p_entry->countergroup,
	       p_entry->unit.c_str() );

	    break;
	 }
         default: // DEF_REC_TYPE__Unknown
	 {
	    assert( 0 );
	 }
      }
   }

   // close writer stream
   OTF_WStream_close( p_glob_def_wstream );
   // close file manager for writer stream
   OTF_FileManager_close( p_glob_def_manager );

   VPrint( 2, "  Closed OTF writer stream [namestub %s id 0]\n",
	   tmp_out_file_prefix.c_str() );

   return !error;
}

void
Definitions::addDefComment( const DefRec_DefinitionComment_struct * p_comment )
{
   uint32_t i;

   // definition comment already exists?
   //
   for( i = 0; i < m_vecDefComments.size(); i++ )
   {
      if( m_vecDefComments[i]->comment.compare(p_comment->comment) == 0 )
         break;
   }

   // no -> add comment
   //
   if( i == m_vecDefComments.size() )
      m_vecDefComments.push_back( p_comment );
}

void
Definitions::addDefComments2Global( std::vector<DefRec_Base_struct*> *
				    p_vecGlobDefs )
{
   for( uint32_t i = 0; i < m_vecDefComments.size(); i++ )
   {
      p_vecGlobDefs->push_back( new DefRec_DefinitionComment_struct(
				   m_vecDefComments[i]->orderidx,
				   m_vecDefComments[i]->comment ) );
   }
}

void
Definitions::addDefProcess( const DefRec_DefProcess_struct * p_proc )
{
   // add process definition
   m_vecDefProcesses.push_back( p_proc );
}

void
Definitions::addDefProcesses2Global( std::vector<DefRec_Base_struct*> *
                                     p_vecGlobDefs, uint32_t i )
{
   // first (non-recursive) call?
   if( i == (uint32_t)-1 )
   {
      // call recursive for all master processes
      //
      for( i = 0; i < m_vecDefProcesses.size(); i++ )
      {
         if( m_vecDefProcesses[i]->parent == 0 )
            addDefProcesses2Global( p_vecGlobDefs, i );
      }
   }
   // otherwise, add process definition to global definition records
   // in correct order
   else
   {
      p_vecGlobDefs->push_back( new DefRec_DefProcess_struct(
                                   m_vecDefProcesses[i]->deftoken,
                                   m_vecDefProcesses[i]->name,
                                   m_vecDefProcesses[i]->parent ) );

      // call recursive for all child processes
      //
      for( uint32_t j = i+1; j < m_vecDefProcesses.size(); j++ )
      {
         if( m_vecDefProcesses[j]->parent == m_vecDefProcesses[i]->deftoken )
            addDefProcesses2Global( p_vecGlobDefs, j );
      }
   }
}

void
Definitions::addProc2NodeGroup( const std::string & nodeName,
				const uint32_t & nodeProc )
{
   // process id already exists for this node?
   std::vector<NodeProc_struct>::iterator it =
      std::find( m_mapNodeProcs[nodeName].begin(),
		 m_mapNodeProcs[nodeName].end(),
		 NodeProc_struct( nodeProc ) );

   // no -> add process id
   if( it == m_mapNodeProcs[nodeName].end() )
   {
      m_mapNodeProcs[nodeName].push_back( NodeProc_struct( nodeProc ) );
      std::sort( m_mapNodeProcs[nodeName].begin(),
		 m_mapNodeProcs[nodeName].end(),
		 std::less<NodeProc_struct>() );
   }
}

void
Definitions::addNodeGroups2Global( std::vector<DefRec_Base_struct*> *
				   p_vecGlobDefs )
{
   uint32_t seq_node_group_token = 1500000000;

   for( std::map<std::string, std::vector<NodeProc_struct> >::iterator it =
	   m_mapNodeProcs.begin(); it != m_mapNodeProcs.end(); it++ )
   {
      std::vector<uint32_t> vec_procids;
      for( uint32_t i = 0; i < it->second.size(); i++ )
	 vec_procids.push_back( it->second[i].procid );

      p_vecGlobDefs->push_back( new DefRec_DefProcessGroup_struct(
				   0,
				   seq_node_group_token++,
				   DefRec_DefProcessGroup_struct::TYPE_NODE,
				   it->first,
				   vec_procids ) );
   }
}

void
Definitions::addMPIComm( const uint32_t proc, const uint32_t defToken,
			 const std::vector<uint32_t> & vecMembers )
{
   uint32_t comm_id = getMPICommIdByMembers( vecMembers );
   uint32_t index;

   // search MPI comm. entry with same members for proc
   //
   std::list<MPIComm_struct>::iterator it =
      std::find( m_mapProcMPIComms[proc].begin(),
		 m_mapProcMPIComms[proc].end(),
		 MPIComm_struct( comm_id ) );

   // if found -> increment index; otherwise init index
   if( it != m_mapProcMPIComms[proc].end() )
      index = (it->index) + 1;
   else
      index = 0;

   // create new MPI comm. entry for proc
   //
   MPIComm_struct new_comm( comm_id, proc, defToken, index );
   m_mapProcMPIComms[proc].push_front( new_comm );
}

void
Definitions::addMPIComms2Global( std::vector<DefRec_Base_struct*> *
				 p_vecGlobDefs )
{
   std::list<MPIComm_struct> list_mpi_comms;

   // convert local MPI comm. map to list
   //
   for( std::map<uint32_t, std::list<MPIComm_struct> >::iterator map_it =
      m_mapProcMPIComms.begin(); map_it != m_mapProcMPIComms.end();
      map_it++ )
   {
      list_mpi_comms.insert( list_mpi_comms.end(),
                             map_it->second.begin(), map_it->second.end() );
   }

   // get global token factory for this definition type
   TokenFactory_DefProcessGroup * p_tkfac_defprocessgroup =
      static_cast<TokenFactory_DefProcessGroup*>(theTokenFactory[TKFAC__DEF_PROCESS_GROUP]);

   char comm_name[256];
   uint32_t comm_name_idx = 0;

   std::map<std::pair<uint32_t, uint32_t>, uint32_t> map_global_token;

   // unify local MPI comms.
   //
   for( std::list<MPIComm_struct>::iterator list_it =
        list_mpi_comms.begin(); list_it != list_mpi_comms.end(); list_it++ )
   {
      // look for already created global token by commid/index
      std::map<std::pair<uint32_t, uint32_t>, uint32_t>::iterator map_globtok_it =
         map_global_token.find( std::make_pair( list_it->commid, list_it->index ) );

      // if not found -> create global token for commid/index
      //
      if( map_globtok_it == map_global_token.end() )
      {
         std::vector<uint32_t> vec_members =
            m_mapMPICommId2Members[list_it->commid];

         // add index to comm's name
         snprintf( comm_name, sizeof( comm_name ) - 1,
                   "MPI Communicator %d", comm_name_idx++ );

         // create token for global comm.
         uint32_t global_token =
            p_tkfac_defprocessgroup->createGlobalToken(
            list_it->loccpuid,
            list_it->deftoken,
            comm_name,
            vec_members );

         // save global token for commid/index
         map_global_token.insert( std::make_pair(
            (std::pair<uint32_t, uint32_t>)std::make_pair(
               (uint32_t)list_it->commid, (uint32_t)list_it->index ),
            (uint32_t)global_token ) );

         // add process group definition to vector of global definitions
         p_vecGlobDefs->push_back( new DefRec_DefProcessGroup_struct(
            0,
            global_token,
            DefRec_DefProcessGroup_struct::TYPE_MPI_COMM_USER,
            comm_name,
            vec_members ) );
      }
      // otherwise, set translation for this local process id, if necessary
      //
      else
      {
         if( p_tkfac_defprocessgroup->translateLocalToken(
                list_it->loccpuid, list_it->deftoken ) == 0 )
         {
            p_tkfac_defprocessgroup->setTranslation(
            list_it->loccpuid,
            list_it->deftoken,
            map_globtok_it->second );
         }
      }
   }
}

uint32_t
Definitions::getMPICommIdByMembers( const std::vector<uint32_t> & vecMembers )
{
   uint32_t comm_id = (uint32_t)-1;

   // linear search of comm. id in map
   //
   for( std::map<uint32_t, std::vector<uint32_t> >::iterator it =
	   m_mapMPICommId2Members.begin(); it != m_mapMPICommId2Members.end();
	it++ )
   {
      if( it->second == vecMembers )
      {
	 comm_id = it->first;
	 break;
      }
   }

   // create new map entry, if not exists
   //
   if( comm_id == (uint32_t)-1 )
   {
      comm_id = m_mapMPICommId2Members.size();
      m_mapMPICommId2Members.insert( std::make_pair( (uint32_t)comm_id,
				        (std::vector<uint32_t>) vecMembers ) );
   }

   return comm_id;
}
