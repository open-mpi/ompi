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

#include "vt_unify.h"
#include "vt_unify_defs.h"
#include "vt_unify_handlers.h"
#include "vt_unify_hooks.h"
#include "vt_unify_sync.h"
#include "vt_unify_tkfac.h"

#include "otf.h"

#include <iostream>
#include <sstream>

#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

DefinitionsC * theDefinitions = 0; // instance of class DefinitionsC

//////////////////// class DefinitionsC ////////////////////

// public methods
//

DefinitionsC::DefinitionsC()
{
   // create instance of sub-class CommentsC
   //
   m_comments = new CommentsC( *this );
   assert( m_comments );

   // create instance of sub-class ProcessGroupsC
   //
   m_procGrps = new ProcessGroupsC( *this );
   assert( m_procGrps );

   // create token factory scopes for def. record type ...
   //

   assert( theTokenFactory );

   // ... DEF_REC_TYPE__DefProcessGroup
   theTokenFactory->addScope(
      DEF_REC_TYPE__DefProcessGroup,
      new TokenFactoryScopeC<DefRec_DefProcessGroupS>
         ( &(m_globDefs.procGrps), 1000000000 ) );

   // ... DEF_REC_TYPE__DefSclFile
   theTokenFactory->addScope(
      DEF_REC_TYPE__DefSclFile,
      new TokenFactoryScopeC<DefRec_DefSclFileS>
         ( &(m_globDefs.sclFiles) ) );

   // ... DEF_REC_TYPE__DefScl
   theTokenFactory->addScope(
      DEF_REC_TYPE__DefScl,
      new TokenFactoryScopeC<DefRec_DefSclS>( &(m_globDefs.scls) ) );

   // ... DEF_REC_TYPE__DefFileGroup
   theTokenFactory->addScope(
      DEF_REC_TYPE__DefFileGroup,
      new TokenFactoryScopeC<DefRec_DefFileGroupS>
         ( &(m_globDefs.fileGrps) ) );

   // ... DEF_REC_TYPE__DefFile
   theTokenFactory->addScope(
      DEF_REC_TYPE__DefFile,
      new TokenFactoryScopeC<DefRec_DefFileS>( &(m_globDefs.files) ) );

   // ... DEF_REC_TYPE__DefFunctionGroup
   theTokenFactory->addScope(
      DEF_REC_TYPE__DefFunctionGroup,
      new TokenFactoryScopeC<DefRec_DefFunctionGroupS>
         ( &(m_globDefs.funcGrps) ) );

   // ... DEF_REC_TYPE__DefFunction
   theTokenFactory->addScope(
      DEF_REC_TYPE__DefFunction,
      new TokenFactoryScopeC<DefRec_DefFunctionS>
         ( &(m_globDefs.funcs) ) );

   // ... DEF_REC_TYPE__DefCollOp
   theTokenFactory->addScope(
      DEF_REC_TYPE__DefCollOp,
      new TokenFactoryScopeC<DefRec_DefCollOpS>
         ( &(m_globDefs.collops) ) );

   // ... DEF_REC_TYPE__DefCounterGroup
   theTokenFactory->addScope(
      DEF_REC_TYPE__DefCounterGroup,
      new TokenFactoryScopeC<DefRec_DefCounterGroupS>
         ( &(m_globDefs.cntrGrps) ) );

   // ... DEF_REC_TYPE__DefCounter
   theTokenFactory->addScope(
      DEF_REC_TYPE__DefCounter,
      new TokenFactoryScopeC<DefRec_DefCounterS>
         ( &(m_globDefs.cntrs) ) );

   // ... DEF_REC_TYPE__DefKeyValue
   theTokenFactory->addScope(
      DEF_REC_TYPE__DefKeyValue,
      new TokenFactoryScopeC<DefRec_DefKeyValueS>
         ( &(m_globDefs.keyVals) ) );
}

DefinitionsC::~DefinitionsC()
{
   // delete instance of sub-class CommentsC
   delete m_comments;

   // delete instance of sub-class ProcessGroupsC
   delete m_procGrps;

   // delete token factory scopes of def. record types ...
   //

   assert( theTokenFactory );

   // ... DEF_REC_TYPE__DefProcessGroup
   theTokenFactory->deleteScope( DEF_REC_TYPE__DefProcessGroup );

   // ... DEF_REC_TYPE__DefSclFile
   theTokenFactory->deleteScope( DEF_REC_TYPE__DefSclFile );

   // ... DEF_REC_TYPE__DefScl
   theTokenFactory->deleteScope( DEF_REC_TYPE__DefScl );

   // ... DEF_REC_TYPE__DefFileGroup
   theTokenFactory->deleteScope( DEF_REC_TYPE__DefFileGroup );

   // ... DEF_REC_TYPE__DefFile
   theTokenFactory->deleteScope( DEF_REC_TYPE__DefFile );

   // ... DEF_REC_TYPE__DefFunctionGroup
   theTokenFactory->deleteScope( DEF_REC_TYPE__DefFunctionGroup );

   // ... DEF_REC_TYPE__DefFunction
   theTokenFactory->deleteScope( DEF_REC_TYPE__DefFunction );

   // ... DEF_REC_TYPE__DefCollOp
   theTokenFactory->deleteScope( DEF_REC_TYPE__DefCollOp );

   // ... DEF_REC_TYPE__DefCounterGroup
   theTokenFactory->deleteScope( DEF_REC_TYPE__DefCounterGroup );

   // ... DEF_REC_TYPE__DefCounter
   theTokenFactory->deleteScope( DEF_REC_TYPE__DefCounter );

   // ... DEF_REC_TYPE__DefKeyValue
   theTokenFactory->deleteScope( DEF_REC_TYPE__DefKeyValue );
}

bool
DefinitionsC::run()
{
   bool error = false;

#ifdef VT_MPI
   // block until all ranks have reached this point
   if( NumRanks > 1 )
      CALL_MPI( MPI_Barrier( MPI_COMM_WORLD ) );
#endif // VT_MPI

   VPrint( 1, "Unifying definitions\n" );

   // trigger phase pre hook
   theHooks->triggerPhaseHook( HooksC::Phase_UnifyDefinitions_pre );

   do
   {
      // get stream ids to read
      //
      std::vector<uint32_t> streamids;
      getStreamIds( streamids );

      // read local definitions
      //
      error = !readLocal( streamids );
      if( SyncError( &error ) )
         break;

      // all local time ranges are known at this point;
      // initialize time synchronization
      //
      theTimeSync->initialize();
      if( SyncError( &error ) )
         break;

      MASTER
      {
         do
         {
            // finish global process group definitions
            if( (error = !m_procGrps->finish()) )
               break;

            // finish global definition comments
            if( (error = !m_comments->finish()) )
               break;

            // set content of global time range definition record
            //
            TimeSyncC::TimeRangeT time_range = theTimeSync->getTimeRange();
            m_globDefs.timerange.minTime = time_range.first;
            m_globDefs.timerange.maxTime = time_range.second;

            // write global definitions
            if( (error = !writeGlobal()) )
               break;

         } while( false );
      }
      SyncError( &error );

   } while( false );

   // show an error message, if necessary
   //
   MASTER
   {
      if( error )
      {
         std::cerr << ExeName << ": "
                   << "An error occurred during unifying definitions. Aborting."
                   << std::endl;
      }
   }

   // trigger phase post hook, if no error occurred
   //
   if( !error )
      theHooks->triggerPhaseHook( HooksC::Phase_UnifyDefinitions_post );

   return !error;
}

bool
DefinitionsC::cleanUp()
{
   bool error = false;

   char filename1[STRBUFSIZE];
   char filename2[STRBUFSIZE];

   MASTER
   {
      // rename temporary definition output file
      //

      // get temporary output file prefix
      const std::string tmp_out_file_prefix =
         Params.out_file_prefix + TmpFileSuffix;

      // get output file type
      const OTF_FileType out_file_type = OTF_FILETYPE_DEF |
         ( Params.docompress ? OTF_FILECOMPRESSION_COMPRESSED : 0 );

      // get temporary file name
      OTF_getFilename( tmp_out_file_prefix.c_str(), 0, out_file_type,
                       STRBUFSIZE, filename1 );
      // get new file name
      OTF_getFilename( Params.out_file_prefix.c_str(), 0, out_file_type,
                       STRBUFSIZE, filename2 );

      // rename file
      //
      if( rename( filename1, filename2 ) != 0 )
      {
         std::cerr << ExeName << ": Error: Could not rename "
                   << filename1 << " to " << filename2 << std::endl;
         error = true;
      }
      else
      {
         VPrint( 3, " Renamed %s to %s\n", filename1, filename2 );
      }
   }

#ifdef VT_MPI
   SyncError( &error );
#endif // VT_MPI

   // remove local definition files, if necessary
   //
   if( !error && Params.doclean )
   {
      int streams_num = (int)MyStreamIds.size();
      int i;

#if defined(HAVE_OMP) && HAVE_OMP
#     pragma omp parallel for private(i, filename1)
#endif // HAVE_OMP
      for( i = 0; i < streams_num; i++ )
      {
         const uint32_t & streamid = MyStreamIds[i];

         bool removed = false;

         // get file name without compression suffix
         OTF_getFilename( Params.in_file_prefix.c_str(), streamid,
            OTF_FILETYPE_DEF, STRBUFSIZE, filename1 );

         // try to remove file
         if( !( removed = ( remove( filename1 ) == 0 ) ) )
         {
            // if failed, get file name with compression suffix
            OTF_getFilename( Params.in_file_prefix.c_str(), streamid,
               OTF_FILETYPE_DEF | OTF_FILECOMPRESSION_COMPRESSED, STRBUFSIZE,
               filename1 );

            // try to remove file again
            removed = ( remove( filename1 ) == 0 );
         }

         if( removed )
            PVPrint( 3, " Removed %s\n", filename1 );
      }
   }

   return !error;
}

// private methods
//

void
DefinitionsC::getStreamIds( std::vector<uint32_t> & streamIds )
{
#ifdef VT_MPI
   if( NumRanks > 1 )
   {
      // distribute stream ids to ranks, whereas childs will not be
      // separated from its parent stream id
      //

      VT_MPI_INT rank = 0;
      for( uint32_t i = 0; i < UnifyCtls.size(); i++ )
      {
         // ignore stream, if it isn't available
         if( !UnifyCtls[i]->stream_avail )
            continue;

         // add stream id to vector, if it's for my rank
         if( rank == MyRank )
            streamIds.push_back( UnifyCtls[i]->streamid );

         // get rank for the next stream id
         //
         if( i < UnifyCtls.size() - 1 && UnifyCtls[i+1]->pstreamid == 0 )
         {
            if( rank + 1 < NumRanks )
            {
               rank++;
            }
            else
            {
               rank = 0;
            }
         }
      }
   }
   else
#endif // VT_MPI
   {
      // add all available stream ids into vector, if serial
      streamIds.assign( MyStreamIds.begin(), MyStreamIds.end() );
   }
}

bool
DefinitionsC::readLocal( const std::vector<uint32_t> & streamIds )
{
   bool error = false;

   VPrint( 2, " Reading local definitions\n" );

#ifdef VT_MPI
   // list of request handles and send buffers which are in use
   std::list<std::pair<MPI_Request, char*> > send_buffers;

   // message tag to use for p2p communication
   const VT_MPI_INT msg_tag = 100;

   // minimum message size
   const VT_MPI_INT min_msg_size = 1024 * 1024;
#endif // VT_MPI

   // read local definitions of each given stream
   //

   // vector of local definitions
   std::vector<DefRec_BaseS*> loc_defs;

   for( uint32_t i = 0; i < streamIds.size(); i++ )
   {
      uint32_t defs_read = loc_defs.size(); // N defs. read in this iteration
      bool presort = false; // flag: pre-sort subset of local definitions?

      // put local definitions of streams which belonging together into
      // one vector
      //
      for( ; i < streamIds.size(); i++ )
      {
         // read local definitions of stream
         if( (error = !readLocal( streamIds[i], loc_defs )) )
            break;

         if( i < streamIds.size() - 1 )
         {
            // abort loop, if next stream isn't a child
            if( StreamId2UnifyCtl[streamIds[i+1]]->pstreamid == 0 )
               break;
            // otherwise, set flag to pre-sort subset of local definitions
            else
               presort = true;
         }
      }
      if( error )
         break;

      // calculate number of local definitions read
      defs_read = loc_defs.size() - defs_read;

      // continue, if nothing is read
      if( ( i >= streamIds.size() - 1 && loc_defs.empty() ) ||
          ( i < streamIds.size() - 1 && defs_read == 0 ) )
         continue;

      // pre-sort subset of local definitions, if necessary
      //
      if( presort )
      {
         // get begin iterator of subset
         //
         std::vector<DefRec_BaseS*>::iterator sort_begin_it = loc_defs.begin();
         if( loc_defs.size() != defs_read )
            sort_begin_it += ( loc_defs.size() - defs_read - 1 );

         // pre-sort
         std::sort( sort_begin_it, loc_defs.end(), DefRec_LocCmp );
      }

      MASTER
      {
         // add local to global definitions
         if( (error = !processLocal( loc_defs )) )
            break;
      }
#ifdef VT_MPI
      else // SLAVE
      {
         // send local definitions to rank 0
         //

         // remove request handles and send buffers from list which are
         // not in use
         //
         VT_MPI_INT not_in_use = 1;
         while( send_buffers.size() > 0 && not_in_use )
         {
            // get the first request handle and send buffer from list
            //
            MPI_Request & request = send_buffers.front().first;
            char *& buffer = send_buffers.front().second;

            // test for completed send
            //
            MPI_Status status;
            CALL_MPI( MPI_Test( &request, &not_in_use, &status ) );

            // free send buffer, if it isn't in use
            //
            if( not_in_use )
            {
               delete [] buffer;
               send_buffers.pop_front();
            }
         }

         char * buffer;
         VT_MPI_INT buffer_pos;
         VT_MPI_INT buffer_size;
         VT_MPI_INT size;

         // get size needed for the send buffer
         //

         // loc_defs.size()
         CALL_MPI( MPI_Pack_size( 1, MPI_UNSIGNED, MPI_COMM_WORLD,
                                  &buffer_size ) );

         // loc_defs
         //
         for( uint32_t j = 0; j < loc_defs.size(); j++ )
         {
            // definition type (loc_defs[j]->dtype)
            CALL_MPI( MPI_Pack_size( 1, MPI_UNSIGNED, MPI_COMM_WORLD, &size ) );
            buffer_size += size;

            // loc_defs[j]
            buffer_size += loc_defs[j]->getPackSize();
         }

         // continue reading, if minimum buffer size isn't reached
         if( i < streamIds.size() && buffer_size < min_msg_size )
            continue;

         // allocate memory for the send buffer
         //
         buffer = new char[buffer_size];
         assert( buffer );

         // pack send buffer
         //

         buffer_pos = 0;

         // loc_defs.size()
         uint32_t loc_defs_size = loc_defs.size();
         CALL_MPI( MPI_Pack( &loc_defs_size, 1, MPI_UNSIGNED, buffer,
                             buffer_size, &buffer_pos, MPI_COMM_WORLD ) );

         // loc_defs
         //
         for( uint32_t j = 0; j < loc_defs.size(); j++ )
         {
            // definition type (loc_defs[j]->dtype)
            CALL_MPI( MPI_Pack( &(loc_defs[j]->dtype), 1, MPI_UNSIGNED,
                                buffer, buffer_size, &buffer_pos,
                                MPI_COMM_WORLD ) );

            // loc_defs[j]
            loc_defs[j]->pack( buffer, buffer_size, buffer_pos );
         }

         // send buffer to rank 0
         //

         PVPrint( 3, "  Sending local definitions to rank 0\n" );

         MPI_Request request;
         CALL_MPI( MPI_Isend( buffer, buffer_size, MPI_PACKED, 0, msg_tag,
                              MPI_COMM_WORLD, &request ) );

         // add request handle and send buffer to list
         send_buffers.push_back( std::make_pair( request, buffer ) );
      }
#endif // VT_MPI

      // free vector of local definitions
      //
      for( uint32_t j = 0; j < loc_defs.size(); j++ )
         delete loc_defs[j];
      loc_defs.clear();
   }

#ifdef VT_MPI
   SyncError( &error );

   if( !error && NumRanks > 1 )
   {
      MASTER
      {
         // receive local definitions from all participating ranks
         //

         // number of ranks finished
         VT_MPI_INT finished_ranks_num = 1; // 1=me

         // repeat until all ranks are finished reading local definitions
         //
         while( finished_ranks_num < NumRanks )
         {
            // source rank finished?
            bool finished = false;

            char * buffer;
            VT_MPI_INT buffer_size;
            VT_MPI_INT buffer_pos;
            VT_MPI_INT rank;
            MPI_Status status;

            // test for a message from any source rank
            CALL_MPI( MPI_Probe( MPI_ANY_SOURCE, msg_tag, MPI_COMM_WORLD,
                                 &status ) );

            // get source rank
            rank = status.MPI_SOURCE;

            // get size needed for the receive buffer
            CALL_MPI( MPI_Get_count( &status, MPI_PACKED, &buffer_size ) );

            // allocate memory for the receive buffer
            //
            buffer = new char[buffer_size];
            assert( buffer );

            // receive buffer
            CALL_MPI( MPI_Recv( buffer, buffer_size, MPI_PACKED, rank, msg_tag,
                                MPI_COMM_WORLD, &status ) );

            // unpack receive buffer
            //

            buffer_pos = 0;

            // loc_defs.size()
            uint32_t loc_defs_size;
            CALL_MPI( MPI_Unpack( buffer, buffer_size, &buffer_pos,
                                  &loc_defs_size, 1, MPI_UNSIGNED,
                                  MPI_COMM_WORLD ) );

            // is source rank finished?
            if( loc_defs_size == 0 )
            {
               finished = true;
               finished_ranks_num++;
            }
            else
            {
               PVPrint( 3, "  Receiving local definitions from rank %d\n",
                        rank );
            }

            for( uint32_t i = 0; i < loc_defs_size; i++ )
            {
               // definition type
               DefRecTypeT def_type;
               CALL_MPI( MPI_Unpack( buffer, buffer_size, &buffer_pos,
                                     &def_type, 1, MPI_UNSIGNED,
                                     MPI_COMM_WORLD ) );

               // create instance for local definition relating to its type
               //

               DefRec_BaseS * new_loc_def;

               switch( def_type )
               {
                  case DEF_REC_TYPE__DefCreator:
                  {
                     new_loc_def = new DefRec_DefCreatorS();
                     break;
                  }
                  case DEF_REC_TYPE__DefComment:
                  {
                     new_loc_def = new DefRec_DefCommentS();
                     break;
                  }
                  case DEF_REC_TYPE__DefTimerResolution:
                  {
                     new_loc_def = new DefRec_DefTimerResolutionS();
                     break;
                  }
                  case DEF_REC_TYPE__DefTimeRange:
                  {
                     new_loc_def = new DefRec_DefTimeRangeS();
                     break;
                  }
                  case DEF_REC_TYPE__DefProcess:
                  {
                     new_loc_def = new DefRec_DefProcessS();
                     break;
                  }
                  case DEF_REC_TYPE__DefProcessGroup:
                  {
                     new_loc_def = new DefRec_DefProcessGroupS();
                     break;
                  }
                  case DEF_REC_TYPE__DefSclFile:
                  {
                     new_loc_def = new DefRec_DefSclFileS();
                     break;
                  }
                  case DEF_REC_TYPE__DefScl:
                  {
                     new_loc_def = new DefRec_DefSclS();
                     break;
                  }
                  case DEF_REC_TYPE__DefFileGroup:
                  {
                     new_loc_def = new DefRec_DefFileGroupS();
                     break;
                  }
                  case DEF_REC_TYPE__DefFile:
                  {
                     new_loc_def = new DefRec_DefFileS();
                     break;
                  }
                  case DEF_REC_TYPE__DefFunctionGroup:
                  {
                     new_loc_def = new DefRec_DefFunctionGroupS();
                     break;
                  }
                  case DEF_REC_TYPE__DefFunction:
                  {
                     new_loc_def = new DefRec_DefFunctionS();
                     break;
                  }
                  case DEF_REC_TYPE__DefCollOp:
                  {
                     new_loc_def = new DefRec_DefCollOpS();
                     break;
                  }
                  case DEF_REC_TYPE__DefCounterGroup:
                  {
                     new_loc_def = new DefRec_DefCounterGroupS();
                     break;
                  }
                  case DEF_REC_TYPE__DefCounter:
                  {
                     new_loc_def = new DefRec_DefCounterS();
                     break;
                  }
                  case DEF_REC_TYPE__DefKeyValue:
                  {
                     new_loc_def = new DefRec_DefKeyValueS();
                     break;
                  }
                  default:
                  {
                     assert( 0 );
                  }
               }
               assert( new_loc_def );

               // unpack local definition from receive buffer
               new_loc_def->unpack( buffer, buffer_size, buffer_pos );

               // add local definition to vector
               loc_defs.push_back( new_loc_def );
            }

            // free memory of receive buffer
            delete [] buffer;

            if( !finished )
            {
               // add local to global definitions
               error = !processLocal( loc_defs );

               // free vector of local definitions
               //
               for( uint32_t i = 0; i < loc_defs.size(); i++ )
                  delete loc_defs[i];
               loc_defs.clear();
            }
         }
      }
      else // SLAVE
      {
         // send a notification to rank 0 that my rank is finished reading
         // local definitions
         // (empty vector of local definitions)
         {
            char * buffer;
            VT_MPI_INT buffer_size;
            VT_MPI_INT buffer_pos = 0;

            // get size needed for the send buffer
            CALL_MPI( MPI_Pack_size( 1, MPI_UNSIGNED, MPI_COMM_WORLD,
                                     &buffer_size ) );

            // allocate memory for the send buffer
            //
            buffer = new char[buffer_size];
            assert( buffer );

            // pack send buffer
            //

            uint32_t finished = 0;
            CALL_MPI( MPI_Pack( &finished, 1, MPI_UNSIGNED, buffer, buffer_size,
                                &buffer_pos, MPI_COMM_WORLD ) );

            // send buffer to rank 0
            //

            MPI_Request request;
            CALL_MPI( MPI_Isend( buffer, buffer_size, MPI_PACKED, 0, msg_tag,
                                 MPI_COMM_WORLD, &request ) );

            // add request handle and send buffer to list
            send_buffers.push_back( std::make_pair( request, buffer ) );
         }

         // complete all sends and remove request handles and send buffers
         // from list
         while( send_buffers.size() > 0 )
         {
            // get the first request handle and send buffer from list
            //
            MPI_Request & request = send_buffers.front().first;
            char *& buffer = send_buffers.front().second;

            // wait until send is completed
            //
            MPI_Status status;
            CALL_MPI( MPI_Wait( &request, &status ) );

            // free memory of send buffer
            delete [] buffer;
            // remove request handle and send buffer from list
            send_buffers.pop_front();
         }
      }
   }
#endif // VT_MPI

   return !error;
}

bool
DefinitionsC::readLocal( const uint32_t & streamId,
                         std::vector<DefRec_BaseS*> & locDefs )
{
   bool error = false;

   // open file manager for reader stream
   //
   OTF_FileManager * manager = OTF_FileManager_open( 1 );
   assert( manager );

   // open stream for reading
   //
   OTF_RStream * rstream =
      OTF_RStream_open( Params.in_file_prefix.c_str(), streamId, manager );
   assert( rstream );

   PVPrint( 3, "  Opened OTF reader stream [namestub %s id %x]\n",
            Params.in_file_prefix.c_str(), streamId );

   do
   {
      // try to get def. buffer
      //
      if( !OTF_RStream_getDefBuffer( rstream ) )
      {
         PVPrint( 3, "   No definitions found in this OTF reader stream "
                     "- Ignored\n" );
         break;
      }

      // close definitions buffer
      OTF_RStream_closeDefBuffer( rstream );

      // create record handler and set the local definition
      // vector as first handler argument for ...
      //

      OTF_HandlerArray * handler_array = OTF_HandlerArray_open();
      assert( handler_array );

      // ... OTF_DEFINITIONCOMMENT_RECORD
      OTF_HandlerArray_setHandler( handler_array,
         (OTF_FunctionPointer*)Handle_DefComment,
         OTF_DEFINITIONCOMMENT_RECORD );
      OTF_HandlerArray_setFirstHandlerArg( handler_array,
         &locDefs,
         OTF_DEFINITIONCOMMENT_RECORD );

      // ... OTF_DEFCREATOR_RECORD
      OTF_HandlerArray_setHandler( handler_array,
         (OTF_FunctionPointer*)Handle_DefCreator,
         OTF_DEFCREATOR_RECORD );
      OTF_HandlerArray_setFirstHandlerArg( handler_array,
         &locDefs,
         OTF_DEFCREATOR_RECORD );

      // ... OTF_DEFTIMERRESOLUTION_RECORD
      OTF_HandlerArray_setHandler( handler_array,
         (OTF_FunctionPointer*)Handle_DefTimerResolution,
         OTF_DEFTIMERRESOLUTION_RECORD );
      OTF_HandlerArray_setFirstHandlerArg( handler_array,
         &locDefs,
         OTF_DEFTIMERRESOLUTION_RECORD );

      // ... OTF_DEFTIMERANGE_RECORD
      OTF_HandlerArray_setHandler( handler_array,
         (OTF_FunctionPointer*)Handle_DefTimeRange,
         OTF_DEFTIMERANGE_RECORD );
      OTF_HandlerArray_setFirstHandlerArg( handler_array,
         &locDefs,
         OTF_DEFTIMERANGE_RECORD );

      // ... OTF_DEFPROCESSGROUP_RECORD
      OTF_HandlerArray_setHandler( handler_array,
         (OTF_FunctionPointer*)Handle_DefProcessGroup,
         OTF_DEFPROCESSGROUP_RECORD );
      OTF_HandlerArray_setFirstHandlerArg( handler_array,
         &locDefs,
         OTF_DEFPROCESSGROUP_RECORD );

      // ... OTF_DEFPROCESS_RECORD
      OTF_HandlerArray_setHandler( handler_array,
         (OTF_FunctionPointer*)Handle_DefProcess,
         OTF_DEFPROCESS_RECORD );
      OTF_HandlerArray_setFirstHandlerArg( handler_array,
         &locDefs,
         OTF_DEFPROCESS_RECORD );

      // ... OTF_DEFSCLFILE_RECORD
      OTF_HandlerArray_setHandler( handler_array,
         (OTF_FunctionPointer*)Handle_DefSclFile,
         OTF_DEFSCLFILE_RECORD );
      OTF_HandlerArray_setFirstHandlerArg( handler_array,
         &locDefs,
         OTF_DEFSCLFILE_RECORD );

      // ... OTF_DEFSCL_RECORD
      OTF_HandlerArray_setHandler( handler_array,
         (OTF_FunctionPointer*)Handle_DefScl,
         OTF_DEFSCL_RECORD );
      OTF_HandlerArray_setFirstHandlerArg( handler_array,
         &locDefs,
         OTF_DEFSCL_RECORD );

      // ... OTF_DEFFILEGROUP_RECORD
      OTF_HandlerArray_setHandler( handler_array,
         (OTF_FunctionPointer*)Handle_DefFileGroup,
         OTF_DEFFILEGROUP_RECORD );
      OTF_HandlerArray_setFirstHandlerArg( handler_array,
         &locDefs,
         OTF_DEFFILEGROUP_RECORD );

      // ... OTF_DEFFILE_RECORD
      OTF_HandlerArray_setHandler( handler_array,
         (OTF_FunctionPointer*)Handle_DefFile,
         OTF_DEFFILE_RECORD );
      OTF_HandlerArray_setFirstHandlerArg( handler_array,
         &locDefs,
         OTF_DEFFILE_RECORD );

      // ... OTF_DEFFUNCTIONGROUP_RECORD
      OTF_HandlerArray_setHandler( handler_array,
         (OTF_FunctionPointer*)Handle_DefFunctionGroup,
         OTF_DEFFUNCTIONGROUP_RECORD );
      OTF_HandlerArray_setFirstHandlerArg( handler_array,
         &locDefs,
         OTF_DEFFUNCTIONGROUP_RECORD );

      // ... OTF_DEFFUNCTION_RECORD
      OTF_HandlerArray_setHandler( handler_array,
         (OTF_FunctionPointer*)Handle_DefFunction,
         OTF_DEFFUNCTION_RECORD );
      OTF_HandlerArray_setFirstHandlerArg( handler_array,
         &locDefs,
         OTF_DEFFUNCTION_RECORD );

      // ... OTF_DEFCOLLOP_RECORD
      OTF_HandlerArray_setHandler( handler_array,
         (OTF_FunctionPointer*)Handle_DefCollOp,
         OTF_DEFCOLLOP_RECORD );
      OTF_HandlerArray_setFirstHandlerArg( handler_array,
         &locDefs,
         OTF_DEFCOLLOP_RECORD );

      // ... OTF_DEFCOUNTERGROUP_RECORD
      OTF_HandlerArray_setHandler( handler_array,
         (OTF_FunctionPointer*)Handle_DefCounterGroup,
         OTF_DEFCOUNTERGROUP_RECORD );
      OTF_HandlerArray_setFirstHandlerArg( handler_array,
         &locDefs,
         OTF_DEFCOUNTERGROUP_RECORD );

      // ... OTF_DEFCOUNTER_RECORD
      OTF_HandlerArray_setHandler( handler_array,
         (OTF_FunctionPointer*)Handle_DefCounter,
         OTF_DEFCOUNTER_RECORD );
      OTF_HandlerArray_setFirstHandlerArg( handler_array,
         &locDefs,
         OTF_DEFCOUNTER_RECORD );

      // ... OTF_DEFKEYVALUE_RECORD
      OTF_HandlerArray_setHandler( handler_array,
         (OTF_FunctionPointer*)Handle_DefKeyValue,
         OTF_DEFKEYVALUE_RECORD );
      OTF_HandlerArray_setFirstHandlerArg( handler_array,
         &locDefs,
         OTF_DEFKEYVALUE_RECORD );

      // read local definitions
      //
      if( OTF_RStream_readDefinitions( rstream, handler_array ) ==
          OTF_READ_ERROR )
      {
         std::cerr << ExeName << ": Error: "
                   << "Could not read definitions of OTF stream [namestub "
                   << Params.in_file_prefix << " id "
                   << std::hex << streamId << "]"
                   << std::dec << std::endl;
         error = true;
      }

      // close record handler
      OTF_HandlerArray_close( handler_array );

   } while( false );

   // close reader stream
   OTF_RStream_close( rstream );
   // close file manager for reader stream
   OTF_FileManager_close( manager );

   PVPrint( 3, "  Closed OTF reader stream [namestub %s id %x]\n",
            Params.in_file_prefix.c_str(), streamId );

   return !error;
}

bool
DefinitionsC::processLocal( const std::vector<DefRec_BaseS*> & locDefs )
{
   bool error = false;

   for( uint32_t i = 0; i < locDefs.size() && !error; i++ )
   {
      // handle local definition depending on its type
      //
      switch( locDefs[i]->dtype )
      {
         case DEF_REC_TYPE__DefComment:
         {
            // get reference to local definition entry
            const DefRec_DefCommentS * loc_def_entry =
               static_cast<DefRec_DefCommentS*>( locDefs[i] );

            // process local definition comment
            error = !m_comments->processLocal( *loc_def_entry );

            break;
         }
         case DEF_REC_TYPE__DefCreator:
         {
            // get reference to local definition entry
            const DefRec_DefCreatorS * loc_def_entry =
               static_cast<DefRec_DefCreatorS*>( locDefs[i] );

            // create global creator definition only once
            //
            static bool creator_added = false;
            if( !creator_added )
            {
               m_globDefs.creator = *loc_def_entry;
               creator_added = true;
            }

            break;
         }
         case DEF_REC_TYPE__DefTimerResolution:
         {
            // get reference to local definitions entry
            const DefRec_DefTimerResolutionS * loc_def_entry =
               static_cast<DefRec_DefTimerResolutionS*>( locDefs[i] );

            // create global timer res. definition only once
            //
            static bool timeres_added = false;
            if( !timeres_added )
            {
               m_globDefs.timeres = *loc_def_entry;
               timeres_added = true;
            }

            break;
         }
         case DEF_REC_TYPE__DefTimeRange:
         {
            // get reference to local definitions entry
            const DefRec_DefTimeRangeS * loc_def_entry =
               static_cast<DefRec_DefTimeRangeS*>( locDefs[i] );

            // temporary store local time range
            theTimeSync->setTimeRange( loc_def_entry->loccpuid,
               loc_def_entry->minTime, loc_def_entry->maxTime );

            break;
         }
         case DEF_REC_TYPE__DefProcess:
         {
            // get reference to local definition entry
            const DefRec_DefProcessS * loc_def_entry =
               static_cast<DefRec_DefProcessS*>( locDefs[i] );

            // process definitions don't need to be unified, so add it
            // to global definitions without any change
            m_globDefs.procs.insert( *loc_def_entry );

            break;
         }
         case DEF_REC_TYPE__DefProcessGroup:
         {
            // get reference to local definition entry
            DefRec_DefProcessGroupS * loc_def_entry =
               static_cast<DefRec_DefProcessGroupS*>( locDefs[i] );

            // process local process group definition
            error = !m_procGrps->processLocal( *loc_def_entry );

            break;
         }
         case DEF_REC_TYPE__DefSclFile:
         {
            // get reference to local definition entry
            const DefRec_DefSclFileS * loc_def_entry =
               static_cast<DefRec_DefSclFileS*>( locDefs[i] );

            // get global token factory for DefSclFile
            static TokenFactoryScopeI * tkfac_defsclfile =
               theTokenFactory->getScope( DEF_REC_TYPE__DefSclFile );

            // create global definition for DefSclFile
            tkfac_defsclfile->create( loc_def_entry );

            break;
         }
         case DEF_REC_TYPE__DefScl:
         {
            // get reference to local definition entry
            DefRec_DefSclS * loc_def_entry =
               static_cast<DefRec_DefSclS*>( locDefs[i] );

            // get global token factory for DefSclFile
            static TokenFactoryScopeI * tkfac_defsclfile =
               theTokenFactory->getScope( DEF_REC_TYPE__DefSclFile );

            // get global token factory for DefScl
            static TokenFactoryScopeI * tkfac_defscl =
               theTokenFactory->getScope( DEF_REC_TYPE__DefScl );

            // get global token for DefSclFile
            //
            uint32_t global_sclfile =
               tkfac_defsclfile->translate(
                  loc_def_entry->loccpuid, loc_def_entry->sclfile );
            assert( global_sclfile != 0 );
            loc_def_entry->sclfile = global_sclfile;

            // create global token for DefScl
            tkfac_defscl->create( loc_def_entry );

            break;
         }
         case DEF_REC_TYPE__DefFileGroup:
         {
            // get reference to local definition entry
            const DefRec_DefFileGroupS * loc_def_entry =
               static_cast<DefRec_DefFileGroupS*>( locDefs[i] );

            // get global token factory for DefFileGroup
            static TokenFactoryScopeI * tkfac_deffilegroup =
               theTokenFactory->getScope( DEF_REC_TYPE__DefFileGroup );

            // create global definition for DefFileGroup
            tkfac_deffilegroup->create( loc_def_entry );

            break;
         }
         case DEF_REC_TYPE__DefFile:
         {
            // get reference to local definition entry
            DefRec_DefFileS * loc_def_entry =
               static_cast<DefRec_DefFileS*>( locDefs[i] );

            // get global token factory for DefFileGroup
            static TokenFactoryScopeI * tkfac_deffilegroup =
               theTokenFactory->getScope( DEF_REC_TYPE__DefFileGroup );

            // get global token factory for DefFile
            static TokenFactoryScopeI * tkfac_deffile =
               theTokenFactory->getScope( DEF_REC_TYPE__DefFile );

            // get global token for DefFileGroup
            //
            uint32_t global_filegroup =
               tkfac_deffilegroup->translate(
                  loc_def_entry->loccpuid, loc_def_entry->group );
            assert( global_filegroup != 0 );
            loc_def_entry->group = global_filegroup;

            // create global token for DefFile
            tkfac_deffile->create( loc_def_entry );

            break;
         }
         case DEF_REC_TYPE__DefFunctionGroup:
         {
            // get reference to local definition entry
            const DefRec_DefFunctionGroupS * loc_def_entry =
               static_cast<DefRec_DefFunctionGroupS*>( locDefs[i] );

            // get global token factory for DefFunctionGroup
            static TokenFactoryScopeI * tkfac_deffuncgroup =
               theTokenFactory->getScope( DEF_REC_TYPE__DefFunctionGroup );

            // create global definition for DefFunctionGroup
            tkfac_deffuncgroup->create( loc_def_entry );

            break;
         }
         case DEF_REC_TYPE__DefFunction:
         {
            // get reference to local definition entry
            DefRec_DefFunctionS * loc_def_entry =
               static_cast<DefRec_DefFunctionS*>( locDefs[i] );

            // get global token factory for DefFunctionGroup
            static TokenFactoryScopeI * tkfac_deffuncgroup =
               theTokenFactory->getScope( DEF_REC_TYPE__DefFunctionGroup );

            // get global token factory for DefScl
            static TokenFactoryScopeI * tkfac_defscl =
               theTokenFactory->getScope( DEF_REC_TYPE__DefScl );

            // get global token factory for DefFunction
            static TokenFactoryScopeI * tkfac_deffunc =
               theTokenFactory->getScope( DEF_REC_TYPE__DefFunction );

            // get global token for DefFunctionGroup
            //
            uint32_t global_funcgroup =
               tkfac_deffuncgroup->translate(
                  loc_def_entry->loccpuid, loc_def_entry->group );
            assert( global_funcgroup != 0 );
            loc_def_entry->group = global_funcgroup;

            // get global token for DefScl
            //
            if( loc_def_entry->scltoken != 0 )
            {
               uint32_t global_scl =
                  tkfac_defscl->translate(
                     loc_def_entry->loccpuid, loc_def_entry->scltoken );
               assert( global_scl != 0 );
               loc_def_entry->scltoken = global_scl;
            }

            // create global token for DefFunction
            tkfac_deffunc->create( loc_def_entry );

            break;
         }
         case DEF_REC_TYPE__DefCollOp:
         {
            // get reference to local definition entry
            const DefRec_DefCollOpS * loc_def_entry =
               static_cast<DefRec_DefCollOpS*>( locDefs[i] );

            // get global token factory for DefCollOp
            static TokenFactoryScopeI * tkfac_defcollop =
               theTokenFactory->getScope( DEF_REC_TYPE__DefCollOp );

            // create global definition for DefCollOp
            tkfac_defcollop->create( loc_def_entry );

            break;
         }
         case DEF_REC_TYPE__DefCounterGroup:
         {
            // get reference to local definition entry
            const DefRec_DefCounterGroupS * loc_def_entry =
               static_cast<DefRec_DefCounterGroupS*>( locDefs[i] );

            // get global token factory for DefCounterGroup
            static TokenFactoryScopeI * tkfac_defcntrgroup =
               theTokenFactory->getScope( DEF_REC_TYPE__DefCounterGroup );

            // create global definition for DefCounterGroup
            tkfac_defcntrgroup->create( loc_def_entry );

            break;
         }
         case DEF_REC_TYPE__DefCounter:
         {
            // get reference to local definition entry
            DefRec_DefCounterS * loc_def_entry =
               static_cast<DefRec_DefCounterS*>( locDefs[i] );

            // get global token factory for DefCounterGroup
            static TokenFactoryScopeI * tkfac_defcntrgroup =
               theTokenFactory->getScope( DEF_REC_TYPE__DefCounterGroup );

            // get global token factory for DefCounter
            static TokenFactoryScopeI * tkfac_defcntr =
               theTokenFactory->getScope( DEF_REC_TYPE__DefCounter );

            // get global token for DefCounterGroup
            //
            uint32_t global_cntrgroup =
               tkfac_defcntrgroup->translate(
                  loc_def_entry->loccpuid, loc_def_entry->group );
            assert( global_cntrgroup != 0 );
            loc_def_entry->group = global_cntrgroup;

            // create global token for DefCounter
            tkfac_defcntr->create( loc_def_entry );

            break;
         }
         case DEF_REC_TYPE__DefKeyValue:
         {
            // get reference to local definition entry
            const DefRec_DefKeyValueS * loc_def_entry =
               static_cast<DefRec_DefKeyValueS*>( locDefs[i] );

            // get global token factory for DefKeyValue
            static TokenFactoryScopeI * tkfac_defkeyval =
               theTokenFactory->getScope( DEF_REC_TYPE__DefKeyValue );

            // create global definition for DefKeyValue
            tkfac_defkeyval->create( loc_def_entry );

            break;
         }
         default:
         {
            assert( 0 );
         }
      }
   }

   return !error;
}

bool
DefinitionsC::writeGlobal()
{
   bool error = false;

   VPrint( 2, " Writing global definitions\n" );

   // get temporary output file prefix
   const std::string tmp_out_file_prefix =
      Params.out_file_prefix + TmpFileSuffix;

   // open file manager for writer stream
   //
   OTF_FileManager * manager = OTF_FileManager_open( 1 );
   assert( manager );

   // open stream for writing (stream id = 0)
   //
   OTF_WStream * wstream =
      OTF_WStream_open( tmp_out_file_prefix.c_str(), 0, manager );
   assert( wstream );

   VPrint( 3, "  Opened OTF writer stream [namestub %s id 0]\n",
           tmp_out_file_prefix.c_str() );

   // set file compression
   if( Params.docompress )
      OTF_WStream_setCompression( wstream, OTF_FILECOMPRESSION_COMPRESSED );

   do
   {
      // write OTF version record
      error = ( OTF_WStream_writeOtfVersion( wstream ) == 0 );

      // write global definition records
      //
      for( uint32_t t = 0; t < (uint32_t)DEF_REC_TYPE__Num && !error; t++ )
      {
         DefRecTypeT def_type = static_cast<DefRecTypeT>( t );

         switch( def_type )
         {
            case DEF_REC_TYPE__DefCreator:
            {
               bool do_write = true;

               // get copy of definition record in order that hook(s) can
               // modify it
               DefRec_DefCreatorS record = m_globDefs.creator;

               // trigger write record hook
               theHooks->triggerWriteRecordHook( HooksC::Record_DefCreator, 3,
                  &wstream, &(record.creator), &do_write );

               // write record
               if( do_write )
                  error = ( OTF_WStream_writeDefCreator( wstream,
                               record.creator.c_str() ) == 0 );

               break;
            }
            case DEF_REC_TYPE__DefTimerResolution:
            {
               bool do_write = true;

               // get copy of definition record in order that hook(s) can
               // modify it
               DefRec_DefTimerResolutionS record = m_globDefs.timeres;

               // trigger write record hook
               theHooks->triggerWriteRecordHook(
                  HooksC::Record_DefTimerResolution, 3, &wstream,
                  &(record.ticksPerSecond), &do_write );

               // write record
               if( do_write )
                  error = ( OTF_WStream_writeDefTimerResolution( wstream,
                               record.ticksPerSecond ) == 0 );

               break;
            }
            case DEF_REC_TYPE__DefTimeRange:
            {
               bool do_write = true;

               // get copy of definition record in order that hook(s) can
               // modify it
               DefRec_DefTimeRangeS record = m_globDefs.timerange;

               // trigger write record hook
               theHooks->triggerWriteRecordHook(
                  HooksC::Record_DefTimeRange, 4, &wstream,
                  &(record.minTime), &(record.maxTime), &do_write );

               // write record
               if( do_write )
                  error = ( OTF_WStream_writeDefTimeRange( wstream,
                               record.minTime, record.maxTime, 0 ) == 0 );

               break;
            }
            case DEF_REC_TYPE__DefComment:
            {
               // iterate over all definition comments
               for( std::set<DefRec_DefCommentS>::const_iterator it =
                    m_globDefs.comments.begin();
                    it != m_globDefs.comments.end() && !error; it++ )
               {
                  bool do_write = true;

                  // get copy of definition record in order that hook(s) can
                  // modify it
                  DefRec_DefCommentS record = *it;

                  // trigger write record hook
                  theHooks->triggerWriteRecordHook(
                     HooksC::Record_DefComment, 3, &wstream, &(record.comment),
                     &do_write );

                  // write record
                  if( do_write )
                     error = ( OTF_WStream_writeDefinitionComment(
                                  wstream, record.comment.c_str() ) == 0 );
               }

               break;
            }
            case DEF_REC_TYPE__DefProcess:
            {
               // iterate over all process definitions
               for( std::set<DefRec_DefProcessS>::const_iterator it =
                    m_globDefs.procs.begin();
                    it != m_globDefs.procs.end() && !error; it++ )
               {
                  bool do_write = true;

                  // get copy of definition record in order that hook(s) can
                  // modify it
                  DefRec_DefProcessS record = *it;

                  // trigger write record hook
                  theHooks->triggerWriteRecordHook( HooksC::Record_DefProcess,
                     5, &wstream, &(record.deftoken), &(record.name),
                     &(record.parent), &do_write );

                  // write record
                  if( do_write )
                     error = ( OTF_WStream_writeDefProcess( wstream,
                                  record.deftoken, record.name.c_str(),
                                  record.parent ) == 0 );
               }

               break;
            }
            case DEF_REC_TYPE__DefProcessGroup:
            {
               // iterate over all process group definitions
               for( std::set<DefRec_DefProcessGroupS>::const_iterator it =
                    m_globDefs.procGrps.begin();
                    it != m_globDefs.procGrps.end() && !error; it++ )
               {
                  bool do_write = true;

                  // get copy of definition record in order that hook(s) can
                  // modify it
                  DefRec_DefProcessGroupS record = *it;

                  // inflate group members
                  m_procGrps->inflateMembers( record.members );

                  // trigger write record hook
                  theHooks->triggerWriteRecordHook(
                     HooksC::Record_DefProcessGroup, 5, &wstream,
                     &(record.deftoken), &(record.name), &(record.members),
                     &do_write );

                  if( do_write )
                  {
                     // convert std::vector to C-array
                     //
                     uint32_t n = record.members.size();
                     uint32_t * array = new uint32_t[n];
                     assert( array );
                     for( uint32_t i = 0; i < n; i++ )
                        array[i] = record.members[i];

                     // write record
                     error =
                        ( OTF_WStream_writeDefProcessGroup( wstream,
                             record.deftoken, record.name.c_str(), n,
                             array ) == 0 );

                     delete[] array;
                  }
               }

               break;
            }
            case DEF_REC_TYPE__DefSclFile:
            {
               // iterate over all scl file definitions
               for( std::set<DefRec_DefSclFileS>::const_iterator it =
                    m_globDefs.sclFiles.begin();
                    it != m_globDefs.sclFiles.end() && !error; it++ )
               {
                  bool do_write = true;

                  // get copy of definition record in order that hook(s) can
                  // modify it
                  DefRec_DefSclFileS record = *it;

                  // trigger write record hook
                  theHooks->triggerWriteRecordHook( HooksC::Record_DefSclFile,
                     4, &wstream, &(record.deftoken), &(record.filename),
                     &do_write );

                  // write record
                  if( do_write )
                     error =
                        ( OTF_WStream_writeDefSclFile( wstream,
                             record.deftoken, record.filename.c_str() ) == 0 );
               }

               break;
            }
            case DEF_REC_TYPE__DefScl:
            {
               // iterate over all scl definitions
               for( std::set<DefRec_DefSclS>::const_iterator it =
                    m_globDefs.scls.begin();
                    it != m_globDefs.scls.end() && !error; it++ )
               {
                  bool do_write = true;

                  // get copy of definition record in order that hook(s) can
                  // modify it
                  DefRec_DefSclS record = *it;

                  // trigger write record hook
                  theHooks->triggerWriteRecordHook( HooksC::Record_DefScl, 5,
                     &wstream, &(record.deftoken), &(record.sclfile),
                     &(record.sclline), &do_write );

                  // write record
                  if( do_write )
                     error = ( OTF_WStream_writeDefScl( wstream,
                                  record.deftoken, record.sclfile,
                                  record.sclline ) == 0 );
               }

               break;
            }
            case DEF_REC_TYPE__DefFileGroup:
            {
               // iterate over all file group definitions
               for( std::set<DefRec_DefFileGroupS>::const_iterator it =
                    m_globDefs.fileGrps.begin();
                    it != m_globDefs.fileGrps.end() && !error; it++ )
               {
                  bool do_write = true;

                  // get copy of definition record in order that hook(s) can
                  // modify it
                  DefRec_DefFileGroupS record = *it;

                  // trigger write record hook
                  theHooks->triggerWriteRecordHook( HooksC::Record_DefFileGroup,
                     4, &wstream, &(record.deftoken), &(record.name),
                     &do_write );

                  // write record
                  if( do_write )
                     error = ( OTF_WStream_writeDefFileGroup( wstream,
                                  record.deftoken, record.name.c_str() ) == 0 );
               }

               break;
            }
            case DEF_REC_TYPE__DefFile:
            {
               // iterate over all file definitions
               for( std::set<DefRec_DefFileS>::const_iterator it =
                    m_globDefs.files.begin();
                    it != m_globDefs.files.end() && !error; it++ )
               {
                  bool do_write = true;

                  // get copy of definition record in order that hook(s) can
                  // modify it
                  DefRec_DefFileS record = *it;

                  // trigger write record hook
                  theHooks->triggerWriteRecordHook( HooksC::Record_DefFile, 5,
                     &wstream, &(record.deftoken), &(record.name),
                     &(record.group), &do_write );

                  // write record
                  if( do_write )
                     error = ( OTF_WStream_writeDefFile( wstream,
                                  record.deftoken, record.name.c_str(),
                                  record.group ) == 0 );
               }

               break;
            }
            case DEF_REC_TYPE__DefFunctionGroup:
            {
               // iterate over all function group definitions
               for( std::set<DefRec_DefFunctionGroupS>::const_iterator
                    it = m_globDefs.funcGrps.begin();
                    it != m_globDefs.funcGrps.end() && !error; it++ )
               {
                  bool do_write = true;

                  // get copy of definition record in order that hook(s) can
                  // modify it
                  DefRec_DefFunctionGroupS record = *it;

                  // trigger write record hook
                  theHooks->triggerWriteRecordHook(
                     HooksC::Record_DefFunctionGroup, 4, &wstream,
                     &(record.deftoken), &(record.name), &do_write );

                  // write record
                  if( do_write )
                     error = ( OTF_WStream_writeDefFunctionGroup( wstream,
                                  record.deftoken, record.name.c_str() ) == 0 );
               }

               break;
            }
            case DEF_REC_TYPE__DefFunction:
            {
               // iterate over all function definitions
               for( std::set<DefRec_DefFunctionS>::const_iterator it =
                    m_globDefs.funcs.begin();
                    it != m_globDefs.funcs.end() && !error; it++ )
               {
                  bool do_write = true;

                  // get copy of definition record in order that hook(s) can
                  // modify it
                  DefRec_DefFunctionS record = *it;

                  // trigger write record hook
                  theHooks->triggerWriteRecordHook( HooksC::Record_DefFunction,
                     6, &wstream, &(record.deftoken), &(record.name),
                     &(record.group), &(record.scltoken), &do_write );

                  // write record
                  if( do_write )
                     error = ( OTF_WStream_writeDefFunction( wstream,
                                  record.deftoken, record.name.c_str(),
                                  record.group, record.scltoken ) == 0 );
               }

               break;
            }
            case DEF_REC_TYPE__DefCollOp:
            {
               // iterate over all collop. definitions
               for( std::set<DefRec_DefCollOpS>::const_iterator
                    it = m_globDefs.collops.begin();
                    it != m_globDefs.collops.end() && !error; it++ )
               {
                  bool do_write = true;

                  // get copy of definition record in order that hook(s) can
                  // modify it
                  DefRec_DefCollOpS record = *it;

                  // trigger write record hook
                  theHooks->triggerWriteRecordHook( HooksC::Record_DefCollOp, 5,
                     &wstream, &(record.deftoken), &(record.name),
                     &(record.type), &do_write );

                  // write record
                  if( do_write )
                     error = ( OTF_WStream_writeDefCollectiveOperation( wstream,
                                  record.deftoken, record.name.c_str(),
                                  record.type ) == 0 );
               }

               break;
            }
            case DEF_REC_TYPE__DefCounterGroup:
            {
               // iterate over all counter group definitions
               for( std::set<DefRec_DefCounterGroupS>::const_iterator
                    it = m_globDefs.cntrGrps.begin();
                    it != m_globDefs.cntrGrps.end() && !error; it++ )
               {
                  bool do_write = true;

                  // get copy of definition record in order that hook(s) can
                  // modify it
                  DefRec_DefCounterGroupS record = *it;

                  // trigger write record hook
                  theHooks->triggerWriteRecordHook(
                     HooksC::Record_DefCounterGroup, 4, &wstream,
                     &(record.deftoken), &(record.name), &do_write );

                  // write record
                  if( do_write )
                     error = ( OTF_WStream_writeDefCounterGroup( wstream,
                                  record.deftoken, record.name.c_str() ) == 0 );
               }

               break;
            }
            case DEF_REC_TYPE__DefCounter:
            {
               // iterate over all counter definitions
               for( std::set<DefRec_DefCounterS>::const_iterator it =
                    m_globDefs.cntrs.begin();
                    it != m_globDefs.cntrs.end() && !error; it++ )
               {
                  bool do_write = true;

                  // get copy of definition record in order that hook(s) can
                  // modify it
                  DefRec_DefCounterS record = *it;

                  // trigger write record hook
                  theHooks->triggerWriteRecordHook( HooksC::Record_DefCounter,
                     7, &wstream, &(record.deftoken), &(record.name),
                     &(record.properties), &(record.group), &(record.unit),
                     &do_write );

                  // write record
                  if( do_write )
                     error = ( OTF_WStream_writeDefCounter( wstream,
                                  record.deftoken, record.name.c_str(),
                                  record.properties, record.group,
                                  record.unit.c_str() ) == 0 );
               }

               break;
            }
            case DEF_REC_TYPE__DefKeyValue:
            {
               // iterate over all function definitions
               for( std::set<DefRec_DefKeyValueS>::const_iterator it =
                    m_globDefs.keyVals.begin();
                    it != m_globDefs.keyVals.end() && !error; it++ )
               {
                  bool do_write = true;

                  // get copy of definition record in order that hook(s) can
                  // modify it
                  DefRec_DefKeyValueS record = *it;

                  // trigger write record hook
                  theHooks->triggerWriteRecordHook( HooksC::Record_DefKeyValue,
                     5, &wstream, &(record.deftoken), &(record.type),
                     &(record.name), &do_write );

                  // write record
                  if( do_write )
                     error = ( OTF_WStream_writeDefKeyValue( wstream,
                                  record.deftoken, record.type,
                                  record.name.c_str(),
                                  "" /* description */ ) == 0 );
               }

               break;
            }
            default:
            {
               break;
            }
         }
      }

   } while( false );

   // show an error message, if necessary
   //
   if( error )
   {
      std::cerr << ExeName << ": Error: "
                << "Could not write global definitions to OTF stream [namestub "
                << tmp_out_file_prefix.c_str() << " id 0]" << std::endl;
   }

   // close writer stream
   OTF_WStream_close( wstream );
   // close file manager for writer stream
   OTF_FileManager_close( manager );

   VPrint( 3, "  Closed OTF writer stream [namestub %s id 0]\n",
           tmp_out_file_prefix.c_str() );

   return !error;
}

//////////////////// sub-class DefinitionsC::CommentsC ////////////////////

// private methods
//

DefinitionsC::CommentsC::CommentsC( DefinitionsC & _defs )
   : m_defs( _defs ), m_seqOrderIdx( 0 )
{
   // Empty
}

DefinitionsC::CommentsC::~CommentsC()
{
   // Empty
}

bool
DefinitionsC::CommentsC::processLocal(
   const DefRec_DefCommentS & locComment )
{
   bool error = false;

   switch( locComment.type )
   {
      case DefRec_DefCommentS::TYPE_START_TIME:
      {
         // get minimum start time from comment
         //
         uint64_t starttime;
         sscanf( locComment.comment.c_str(), "%llu",
                 (unsigned long long int*)&starttime );

         // update minimum start time, if necessary
         if( starttime < m_traceTimes.minStartTimeEpoch )
           m_traceTimes.minStartTimeEpoch = starttime;

         break;
      }
      case DefRec_DefCommentS::TYPE_STOP_TIME:
      {
         // get maximum stop time from comment
         //
         uint64_t stoptime;
         sscanf( locComment.comment.c_str(), "%llu",
                 (unsigned long long int*)&stoptime );

         // update maximum stop time, if necessary
         //
         if( stoptime > m_traceTimes.maxStopTimeEpoch )
           m_traceTimes.maxStopTimeEpoch = stoptime;

         break;
      }
      case DefRec_DefCommentS::TYPE_USRCOM_SEND:
      case DefRec_DefCommentS::TYPE_USRCOM_RECV:
      {
         // get peer process id, local communicator token, and tag from comment
         //
         uint32_t peer = locComment.loccpuid;
         uint32_t comm;
         uint32_t tag;
         sscanf( locComment.comment.c_str(), "C%xT%x", &comm, &tag );

         // temporary store user communication id and peer
         m_userCom.comIdsAndPeers.push_back(
            UserComS::ComIdPeerS( UserComC::ComIdS( comm, tag ), peer,
               ( locComment.type == DefRec_DefCommentS::TYPE_USRCOM_SEND ) ) );

         // add process id to certain user communicator
         m_defs.m_procGrps->m_userCom.addCommMember(
            locComment.loccpuid & VT_TRACEID_BITMASK, comm,
            locComment.loccpuid );

         break;
      }
      case DefRec_DefCommentS::TYPE_VT:
      case DefRec_DefCommentS::TYPE_USER:
      {
         // get reference to global definition comments
         std::set<DefRec_DefCommentS> & glob_comments =
            m_defs.m_globDefs.comments;

         // create new comment
         DefRec_DefCommentS new_comment = locComment;

         // user comment?
         if( locComment.type == DefRec_DefCommentS::TYPE_USER )
         {
            static bool first_user_comment = true;

            // create headline for user comments, if it's the first one
            //
            if( first_user_comment )
            {
               glob_comments.insert( DefRec_DefCommentS( 0, m_seqOrderIdx++,
                  DefRec_DefCommentS::TYPE_USER, "User Comments:" ) );
               first_user_comment = false;
            }

            // indent comment
            new_comment.comment = std::string( " " ) + locComment.comment;
         }

         // get order index
         new_comment.deftoken = m_seqOrderIdx++;

         // search for already created global definition comment
         std::set<DefRec_DefCommentS>::const_iterator it =
            std::find( glob_comments.begin(), glob_comments.end(),
                       new_comment );

         // add global definition comment to set, if not found
         if( it == glob_comments.end() )
            glob_comments.insert( new_comment );

         break;
      }
   }

   return !error;
}

bool
DefinitionsC::CommentsC::finish( void )
{
   bool error = false;

   // add time comments to global definitions, if present
   //
   if( m_traceTimes.minStartTimeEpoch != (uint64_t)-1 &&
       m_traceTimes.maxStopTimeEpoch != 0 )
   {
#ifdef VT_UNIFY_HOOKS_TDB
      // trigger HooksTdbC's generic hook to set trace times
      theHooks->triggerGenericHook( VT_UNIFY_HOOKS_TDB_GENID__STARTSTOPTIME_EPOCH,
         2, &m_traceTimes.minStartTimeEpoch, &m_traceTimes.maxStopTimeEpoch );
#endif // VT_UNIFY_HOOKS_TDB

      // get reference to global definition comments
      std::set<DefRec_DefCommentS> & glob_comments = m_defs.m_globDefs.comments;

      // add trace time comments to global definitions
      // (0=headline, 1=start time, 2=stop time, 3=elasped time)
      //
      for( uint32_t i = 0; i < 4; i++ )
      {
         DefRec_DefCommentS new_comment;

         // set comment's type and order index
         //
         new_comment.type = DefRec_DefCommentS::TYPE_START_TIME;
         new_comment.deftoken = m_seqOrderIdx++;

         // compose/set comment's text
         //

         switch( i )
         {
            case 0: // headline
            {
               new_comment.comment = "Trace Times:";
               break;
            }
            case 1: // min. start time
            case 2: // max. stop time
            {
               time_t tt;
               std::ostringstream ss;

               if( i == 1 )
               {
                  tt = (time_t)(m_traceTimes.minStartTimeEpoch / 1e6);
                  ss << " Start: " << asctime(localtime(&tt)) << "("
                     << m_traceTimes.minStartTimeEpoch << ")";
               }
               else // i == 2
               {
                  tt = (time_t)(m_traceTimes.maxStopTimeEpoch / 1e6);
                  ss << " Stop: " << asctime(localtime(&tt)) << "("
                     << m_traceTimes.maxStopTimeEpoch << ")";
               }

               new_comment.comment = ss.str();
               ss.str(""); ss.clear();

               break;
            }
            default: // elapsed time
            {
               time_t tt;
               struct tm elapsed_tm;
               std::ostringstream ss;

               tt =
                  (time_t)((m_traceTimes.maxStopTimeEpoch -
                     m_traceTimes.minStartTimeEpoch) / 1e6);
               gmtime_r(&tt, &elapsed_tm);
               ss << " Elapsed: "
                  << (elapsed_tm.tm_hour < 10 ? "0" : "")
                  << elapsed_tm.tm_hour << ":"
                  << (elapsed_tm.tm_min  < 10 ? "0" : "")
                  << elapsed_tm.tm_min << ":"
                  << (elapsed_tm.tm_sec  < 10 ? "0" : "")
                  << elapsed_tm.tm_sec
                  << " (" << m_traceTimes.maxStopTimeEpoch -
                     m_traceTimes.minStartTimeEpoch << ")";

               new_comment.comment = ss.str();
               ss.str(""); ss.clear();

               break;
            }
         }

         // add comment to global definitions
         glob_comments.insert( new_comment );
      }
   }

   // register user communication ids and peers
   //
   if( !m_userCom.comIdsAndPeers.empty() )
   {
      // get global token factory for DefProcessGroup
      TokenFactoryScopeI * tkfac_defprocgrp =
         theTokenFactory->getScope( DEF_REC_TYPE__DefProcessGroup );

      while( !m_userCom.comIdsAndPeers.empty() )
      {
         // get first list element
         std::list<UserComS::ComIdPeerS>::iterator it =
            m_userCom.comIdsAndPeers.begin();

         // translate local comm. token
         //
         it->comid.comm =
            tkfac_defprocgrp->translate( it->peer, it->comid.comm );
         assert( it->comid.comm != 0 );

         // register communication id and its peer
         //
         if( it->is_sender )
            theUserCom->addSender( it->comid, it->peer );
         else
            theUserCom->addReceiver( it->comid, it->peer );

         // erase first list element
         m_userCom.comIdsAndPeers.pop_front();
      }
   }

   return !error;
}

//////////////////// sub-class DefinitionsC::ProcessGroupsC ////////////////////

// private methods
//

DefinitionsC::ProcessGroupsC::ProcessGroupsC( DefinitionsC & _defs )
   : m_defs( _defs )
{
   // Empty
}

DefinitionsC::ProcessGroupsC::~ProcessGroupsC()
{
   // Empty
}

bool
DefinitionsC::ProcessGroupsC::processLocal(
   DefRec_DefProcessGroupS & locProcGrp )
{
   bool error = false;

   // get global token factory for DefProcessGroup
   static TokenFactoryScopeI * tkfac_defprocgrp =
      theTokenFactory->getScope( DEF_REC_TYPE__DefProcessGroup );

   // handle local process group depending on its type
   //
   switch( locProcGrp.type )
   {
      case DefRec_DefProcessGroupS::TYPE_NODE:
      {
         // add process id to node group members
         m_node.name2Procs[locProcGrp.name].insert( locProcGrp.members[0] );

         break;
      }
      case DefRec_DefProcessGroupS::TYPE_GPU_GROUP:
      {
         // add process ids to GPU group members
         m_gpu.procs.insert( locProcGrp.members.begin(),
                             locProcGrp.members.end() );

         break;
      }
      case DefRec_DefProcessGroupS::TYPE_GPU_COMM:
      {
         // add process ids to comm. group members
         m_gpu.commMembers.insert( locProcGrp.members.begin(),
                                   locProcGrp.members.end() );

         // add local token for translation
         m_gpu.proc2LocCommTk[locProcGrp.loccpuid] = locProcGrp.deftoken;

         break;
      }
      case DefRec_DefProcessGroupS::TYPE_OMP_TEAM:
      {
         static uint32_t omp_thread_team_no = 0;

         // get reference to global process group definitions
         std::set<DefRec_DefProcessGroupS> & glob_proc_grps =
            m_defs.m_globDefs.procGrps;

         // deflate comm. group members
         deflateMembers( locProcGrp.members );

         std::set<DefRec_DefProcessGroupS>::const_iterator it =
            glob_proc_grps.end();

         // not the first OMP thread team comm.?
         if( omp_thread_team_no > 0 )
         {
            // search global definition by comm. group members
            // (content m_globDefs is sorted by type; abort searching
            //  after last TYPE_OMP_TEAM)
            //
            for( it = glob_proc_grps.begin(); it != glob_proc_grps.end(); it++ )
            {
               static bool abort_search = false;
               if( it->type == DefRec_DefProcessGroupS::TYPE_OMP_TEAM )
               {
                  abort_search = true;

                  if( it->members == locProcGrp.members )
                     break;
               }
               else if( abort_search )
               {
                  break;
               }
            }
         }

         // create global definition, if not found
         //
         if( it == glob_proc_grps.end() )
         {
            // compose comm. group name
            //
            std::ostringstream new_name;
            new_name << m_omp.commName() << " " << omp_thread_team_no++;
            locProcGrp.name = new_name.str();

            // create global definition
            tkfac_defprocgrp->create( &locProcGrp );
         }
         // otherwise, set token translation for process
         //
         else
         {
            tkfac_defprocgrp->setTranslation( locProcGrp.loccpuid,
               locProcGrp.deftoken, it->deftoken );
         }

         break;
      }
      case DefRec_DefProcessGroupS::TYPE_MPI_COMM_WORLD:
      {
         // global token of process group definition
         static uint32_t global_token = 0;

         // temporary storage of local tokens to translate
         static std::vector<
            std::pair<uint32_t /* process id */,
                      uint32_t /* local token */> > local_tokens;

         // create global token, if necessary
         //
         if( global_token == 0 )
         {
            // create global process group definition, if members are present
            // (only the first available (not disabled) process contains a
            // filled process group for MPI_COMM_WORLD)
            //
            if( !locProcGrp.members.empty() )
            {
               // deflate comm. group members
               deflateMembers( locProcGrp.members );

               // set group name
               locProcGrp.name = m_mpi.worldCommName();

               // create global definition and get its token
               global_token = tkfac_defprocgrp->create( &locProcGrp );

               // set translations for temporary stored local tokens
               //
               for( uint32_t i = 0; i < local_tokens.size(); i++ )
               {
                  tkfac_defprocgrp->setTranslation( local_tokens[i].first,
                     local_tokens[i].second, global_token );
               }
               local_tokens.clear();
            }
            // otherwise, temporary store local token
            //
            else
            {
               local_tokens.push_back(
                  std::make_pair( locProcGrp.loccpuid, locProcGrp.deftoken ) );
            }
         }
         // otherwise, set token translation for process
         //
         else
         {
            tkfac_defprocgrp->setTranslation( locProcGrp.loccpuid,
               locProcGrp.deftoken, global_token );
         }

         break;
      }
      case DefRec_DefProcessGroupS::TYPE_MPI_COMM_SELF:
      {
         // add communicator
         m_mpi.selfComms.insert(
            MpiS::SelfCommS( locProcGrp.deftoken, locProcGrp.loccpuid ) );

         break;
      }
      case DefRec_DefProcessGroupS::TYPE_MPI_COMM_OTHER:
      {
         // comm. group must have members
         assert( !locProcGrp.members.empty() );

         // deflate comm. group members
         //
         deflateMembers( locProcGrp.members );
         uint32_t membersid = locProcGrp.members[1];

         // search for communicator by its defining process and members
         // (search reversed in order to get an increasing index)
         std::list<MpiS::OtherCommS>::reverse_iterator it =
            std::find( m_mpi.proc2OtherComms[locProcGrp.loccpuid].rbegin(),
               m_mpi.proc2OtherComms[locProcGrp.loccpuid].rend(),
               MpiS::OtherCommS( membersid ) );

         // get new local communicator index
         //
         uint32_t index = 0;
         if( it != m_mpi.proc2OtherComms[locProcGrp.loccpuid].rend() )
            index = it->index + 1;

         // add communicator
         m_mpi.proc2OtherComms[locProcGrp.loccpuid].push_back(
            MpiS::OtherCommS( locProcGrp.deftoken, membersid, index ) );

         break;
      }
      case DefRec_DefProcessGroupS::TYPE_USER_COMM:
      {
         // search for communicator by its name
         std::list<UserComS::CommS>::iterator it =
            std::find( m_userCom.comms.begin(), m_userCom.comms.end(),
               UserComS::CommS( locProcGrp.name ) );

         // add communicator, if not found
         //
         if( it == m_userCom.comms.end() )
         {
            m_userCom.comms.push_back( UserComS::CommS( locProcGrp.name ) );
            it = m_userCom.comms.end(); it--;
         }

         // add local token for translation
         it->proc2LocCommTk[locProcGrp.loccpuid] = locProcGrp.deftoken;

         // add iterator of user comm. for fast access
         m_userCom.proc2LocCommTk2CommIt
            [locProcGrp.loccpuid & VT_TRACEID_BITMASK][locProcGrp.deftoken] =
               it;

         break;
      }
      default:
      {
         // deflate group members
         deflateMembers( locProcGrp.members );

         // create global definition
         tkfac_defprocgrp->create( &locProcGrp );

         break;
      }
   }

   return !error;
}

bool
DefinitionsC::ProcessGroupsC::finish( void )
{
   bool error = false;

   // get global token factory for DefProcessGroup
   TokenFactoryScopeI * tkfac_defprocgrp =
      theTokenFactory->getScope( DEF_REC_TYPE__DefProcessGroup );

   // get reference to global process group definitions
   std::set<DefRec_DefProcessGroupS> & glob_proc_grps =
      m_defs.m_globDefs.procGrps;

   // add node process groups to global definitions
   //
   if( !m_node.name2Procs.empty() )
   {
      // initialize common stuff of new process groups
      //
      DefRec_DefProcessGroupS new_proc_grp;
      new_proc_grp.loccpuid = 0;
      new_proc_grp.type = DefRec_DefProcessGroupS::TYPE_NODE;

      // iterate over all node groups
      for( std::map<std::string, std::set<uint32_t, ProcCmpS> >::iterator
           it = m_node.name2Procs.begin(); it != m_node.name2Procs.end(); it++ )
      {
         // set global token, name, and members
         //
         new_proc_grp.deftoken = tkfac_defprocgrp->getNextToken();
         new_proc_grp.name = it->first;
         new_proc_grp.members.assign( it->second.begin(), it->second.end() );
         it->second.clear();

         // add node process group to global definitions
         glob_proc_grps.insert( new_proc_grp );
      }

      // not needed anymore; free some memory
      m_node.name2Procs.clear();
   }

   // add GPU process group to global definitions
   //
   if( !m_gpu.procs.empty() )
   {
      // initialize new process group
      //
      DefRec_DefProcessGroupS new_proc_grp;
      new_proc_grp.loccpuid = 0;
      new_proc_grp.deftoken = tkfac_defprocgrp->getNextToken();
      new_proc_grp.type = DefRec_DefProcessGroupS::TYPE_GPU_GROUP;
      new_proc_grp.name = m_gpu.groupName();
      new_proc_grp.members.assign( m_gpu.procs.begin(), m_gpu.procs.end() );
      m_gpu.procs.clear();

      // add GPU process group to global definitions
      glob_proc_grps.insert( new_proc_grp );
   }

   // add GPU communicator group to global definitions
   //
   if( !m_gpu.commMembers.empty() && !m_gpu.proc2LocCommTk.empty() )
   {
      // initialize new process group
      //
      DefRec_DefProcessGroupS new_proc_grp;
      new_proc_grp.type = DefRec_DefProcessGroupS::TYPE_GPU_COMM;
      new_proc_grp.name = m_gpu.commName();
      new_proc_grp.members.assign( m_gpu.commMembers.begin(),
                                   m_gpu.commMembers.end() );
      m_gpu.commMembers.clear();

      // iterate over all local tokens
      for( std::map<uint32_t, uint32_t>::const_iterator it =
           m_gpu.proc2LocCommTk.begin(); it != m_gpu.proc2LocCommTk.end();
           it++ )
      {
         static uint32_t global_token = 0;

         // create global definition, if necessary
         //
         if( global_token == 0 )
         {
            new_proc_grp.loccpuid = it->first;
            new_proc_grp.deftoken = it->second;
            global_token = tkfac_defprocgrp->create( &new_proc_grp );
         }
         // otherwise, set token translation for process
         //
         else
         {
            tkfac_defprocgrp->setTranslation( it->first, it->second,
               global_token );
         }
      }

      // not needed anymore; free some memory
      m_gpu.proc2LocCommTk.clear();
   }

   // add MPI_COMM_SELF groups to global definitions
   //
   if( !m_mpi.selfComms.empty() )
   {
      // initialize new process group
      //
      DefRec_DefProcessGroupS new_proc_grp;
      new_proc_grp.type = DefRec_DefProcessGroupS::TYPE_MPI_COMM_SELF;
      new_proc_grp.members.resize( 1 );

      uint32_t i = 0;
      for( std::set<MpiS::SelfCommS>::const_iterator it =
           m_mpi.selfComms.begin(); it != m_mpi.selfComms.end(); it++, i++ )
      {
         // set defining process, members, and local token
         //
         new_proc_grp.loccpuid = new_proc_grp.members[0] = it->member;
         new_proc_grp.deftoken = it->loctk;

         // compose and set name
         //
         std::ostringstream name;
         name << m_mpi.selfCommName() << " " << i;
         new_proc_grp.name = name.str();

         // create global definition
         tkfac_defprocgrp->create( &new_proc_grp );
      }

      // not needed anymore; free some memory
      m_mpi.selfComms.clear();
   }

   // add user created MPI communicator groups to global definitions
   //
   if( !m_mpi.proc2OtherComms.empty() )
   {
      // map comm. members/index <-> global token
      std::map<std::pair<uint32_t, uint32_t>, uint32_t> global_tokens;

      // global comm. index (=name suffix)
      uint32_t global_index = 0;

      for( std::map<uint32_t, std::list<MpiS::OtherCommS> >::const_iterator
           proc_it = m_mpi.proc2OtherComms.begin();
           proc_it != m_mpi.proc2OtherComms.end(); proc_it++ )
      {
         while( !m_mpi.proc2OtherComms[proc_it->first].empty() )
         {
            // get first list element
            std::list<MpiS::OtherCommS>::const_iterator comm_it =
               m_mpi.proc2OtherComms[proc_it->first].begin();

            // get global token of comm. members/index
            uint32_t & global_token =
               global_tokens[std::make_pair(comm_it->membersid,
                                            comm_it->index)];

            // create global definition, if necessary
            //
            if( global_token == 0 )
            {
               // initialize new process group
               //
               DefRec_DefProcessGroupS new_proc_grp;
               new_proc_grp.type = DefRec_DefProcessGroupS::TYPE_MPI_COMM_OTHER;
               new_proc_grp.loccpuid = proc_it->first;
               new_proc_grp.deftoken = comm_it->deftoken;

               // set members
               //
               new_proc_grp.members.resize( 2 );
               new_proc_grp.members[0] = DEFLATED_MEMBERS_TAG;
               new_proc_grp.members[1] = comm_it->membersid;

               // compose and set name
               //
               std::ostringstream name;
               name << m_mpi.otherCommName() << " " << global_index++;
               new_proc_grp.name = name.str();

               // create global definition
               global_token = tkfac_defprocgrp->create( &new_proc_grp );
            }
            // otherwise, set token translation for process
            //
            else
            {
               tkfac_defprocgrp->setTranslation( proc_it->first,
                  comm_it->deftoken, global_token );
            }

            // erase first list element
            m_mpi.proc2OtherComms[proc_it->first].pop_front();
         }
      }
   }

   // add user communicator groups to global definitions
   //
   while( !m_userCom.comms.empty() )
   {
      // get first list element
      std::list<UserComS::CommS>::const_iterator comm_it =
         m_userCom.comms.begin();

      // initialize new process group
      //
      DefRec_DefProcessGroupS new_proc_grp;
      new_proc_grp.type = DefRec_DefProcessGroupS::TYPE_USER_COMM;
      new_proc_grp.name = comm_it->name;
      new_proc_grp.members.assign( comm_it->members.begin(),
         comm_it->members.end() );

      // create global definition
      uint32_t global_token = tkfac_defprocgrp->create( &new_proc_grp );

      // set token translations for processes
      //
      for( std::map<uint32_t, uint32_t>::const_iterator it =
           comm_it->proc2LocCommTk.begin();
           it != comm_it->proc2LocCommTk.end(); it++ )
      {
         tkfac_defprocgrp->setTranslation( it->first, it->second,
            global_token );
      }

      // register global user communicator token
      theUserCom->addUserComm( global_token );

      // erase first list element
      m_userCom.comms.pop_front();
   }

   return !error;
}

void
DefinitionsC::ProcessGroupsC::deflateMembers( std::vector<uint32_t> & members )
{
   // return, if vector is empty or already deflated
   if( members.empty() || members[0] == DEFLATED_MEMBERS_TAG )
      return;

   // search for already known members
   //
   std::map<uint32_t, std::vector<uint32_t> >::const_iterator it;
   for( it = m_id2Members.begin(); it != m_id2Members.end(); it++ )
   {
      if( it->second == members )
         break;
   }

   uint32_t id;

   // get its unique id, if found
   //
   if( it != m_id2Members.end() )
   {
      id = it->first;
   }
   // otherwise, create new unique id and assign members to it
   //
   else
   {
      id = m_id2Members.size();
      m_id2Members[id] = members;
   }

   // do actual deflating
   //
   members.resize( 2 );
   members[0] = DEFLATED_MEMBERS_TAG; // deflated-identifier
   members[1] = id;                   // unique id
}

void
DefinitionsC::ProcessGroupsC::inflateMembers( std::vector<uint32_t> & members )
{
   // return, if vector is empty or not deflated
   if( members.empty() || members[0] != DEFLATED_MEMBERS_TAG )
      return;

   assert( members.size() == 2 );

   // search for members by unique id
   //
   std::map<uint32_t, std::vector<uint32_t> >::const_iterator it =
      m_id2Members.find( members[1] );
   assert( it != m_id2Members.end() );

   // set vector
   members = it->second;
}

