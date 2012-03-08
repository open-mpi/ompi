/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2012, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#include "vt_unify.h"
#include "vt_unify_events_stats.h"
#include "vt_unify_handlers.h"
#include "vt_unify_hooks.h"

#include "otf.h"

#include <iostream>

// instances of class EventsAndStatsC
//
EventsAndStatsC * theEvents = 0;
EventsAndStatsC * theStatistics = 0;

//////////////////// class EventsAndStatsC ////////////////////

// public methods
//

EventsAndStatsC::EventsAndStatsC( const ScopeTypeT & scope )
   : m_scope( scope )
{
   // Empty
}

EventsAndStatsC::~EventsAndStatsC()
{
   // Empty
}

bool
EventsAndStatsC::run()
{
   bool error = false;

#ifdef VT_MPI
   // block until all ranks have reached this point
   if( NumRanks > 1 )
      CALL_MPI( MPI_Barrier( MPI_COMM_WORLD ) );
#endif // VT_MPI

   if( m_scope == SCOPE_EVENTS )
   {
      VPrint( 1, "Unifying events\n" );

      // trigger phase pre hook
      theHooks->triggerPhaseHook( HooksC::Phase_UnifyEvents_pre );
   }
   else // m_scope == SCOPE_STATS
   {
      VPrint( 1, "Unifying statistics\n" );

      // trigger phase pre hook
      theHooks->triggerPhaseHook( HooksC::Phase_UnifyStatistics_pre );
   }

   // rewrite events/statistics
   //
   error = !rewrite();
   SyncError( &error );

   // show an error message, if necessary
   //
   MASTER
   {
      if( error )
      {
         std::cerr << ExeName << ": "
                   << "An error occurred during unifying "
                   << ( m_scope == SCOPE_EVENTS ? "events. " : "statistics. " )
                   << "Aborting." << std::endl;
      }
   }

   // trigger phase post hook, if no error occurred
   //
   if( !error )
   {
      theHooks->triggerPhaseHook( m_scope == SCOPE_EVENTS ?
         HooksC::Phase_UnifyEvents_post : HooksC::Phase_UnifyStatistics_post );
   }

   return !error;
}

bool
EventsAndStatsC::cleanUp()
{
   bool error = false;

   char filename1[STRBUFSIZE];
   char filename2[STRBUFSIZE];

   int streams_num = (int)MyStreamIds.size();
   int i;

   const OTF_FileType common_file_type =
      m_scope == SCOPE_EVENTS ? OTF_FILETYPE_EVENT : OTF_FILETYPE_STATS;

   // remove local event/stat. files, if necessary
   //
   if( Params.doclean )
   {
#if defined(HAVE_OMP) && HAVE_OMP
#     pragma omp parallel for private(i, filename1)
#endif // HAVE_OMP
      for( i = 0; i < streams_num; i++ )
      {
         const uint32_t & streamid = MyStreamIds[i];

         // try to remove file without compression suffix
         OTF_getFilename( Params.in_file_prefix.c_str(), streamid,
            common_file_type, STRBUFSIZE, filename1 );
         if( remove( filename1 ) == 0 )
            PVPrint( 3, " Removed %s\n", filename1 );

         // try to remove file with compression suffix
         OTF_getFilename( Params.in_file_prefix.c_str(), streamid,
            common_file_type | OTF_FILECOMPRESSION_COMPRESSED, STRBUFSIZE,
            filename1 );
         if( remove( filename1 ) == 0 )
            PVPrint( 3, " Removed %s\n", filename1 );
      }
   }

   // rename temporary event/stat. output files
   //

   // get temporary output file prefix
   const std::string tmp_out_file_prefix =
      Params.out_file_prefix + TmpFileSuffix;

   // get output file type
   const OTF_FileType out_file_type = common_file_type |
      ( Params.docompress ? OTF_FILECOMPRESSION_COMPRESSED : 0 );

#if defined(HAVE_OMP) && HAVE_OMP
#  pragma omp parallel for private(i, filename1, filename2)
#endif // HAVE_OMP
   for( i = 0; i < streams_num; i++ )
   {
      const uint32_t & streamid = MyStreamIds[i];

      // get temporary file name
      OTF_getFilename( tmp_out_file_prefix.c_str(), streamid,
                       out_file_type, STRBUFSIZE, filename1 );
      // get new file name
      OTF_getFilename( Params.out_file_prefix.c_str(), streamid,
                       out_file_type, STRBUFSIZE, filename2 );

      // rename file
      if( rename( filename1, filename2 ) == 0 )
         PVPrint( 3, " Renamed %s to %s\n", filename1, filename2 );
   }

   return !error;
}

// private methods
//

bool
EventsAndStatsC::rewrite()
{
   bool error = false;

   // get input file prefix
   const std::string & in_file_prefix = Params.in_file_prefix;

   // get temporary output file prefix
   const std::string tmp_out_file_prefix =
      Params.out_file_prefix + TmpFileSuffix;

   int streams_num = (int)MyStreamIds.size();
   int i;

#if defined(HAVE_OMP) && HAVE_OMP
#  pragma omp parallel for private(i) schedule(static)
#endif // HAVE_OMP
   for( i = 0; i < streams_num; i++ )
   {
      const uint32_t & streamid = MyStreamIds[i];

      // open file manager
      //
      OTF_FileManager * manager = OTF_FileManager_open( 1 );
      assert( manager );

      // open stream for reading
      //
      OTF_RStream * rstream =
      OTF_RStream_open( in_file_prefix.c_str(), streamid, manager );
      assert( rstream );

      PVPrint( 3, " Opened OTF reader stream [namestub %s id %x]\n",
               in_file_prefix.c_str(), streamid );

      do
      {
         // try to get events/statistics buffer
         //
         if( m_scope == SCOPE_EVENTS )
         {
            if( !OTF_RStream_getEventBuffer( rstream ) )
            {
               PVPrint( 3, "  No events found in this OTF reader stream "
                           "- Ignored\n" );
               break;
            }

            // close events buffer
            OTF_RStream_closeEventBuffer( rstream );
         }
         else // m_scope == SCOPE_STATS
         {
            if( !OTF_RStream_getStatsBuffer( rstream ) )
            {
               PVPrint( 3, "  No statistics found in this OTF reader stream "
                           "- Ignored\n" );
               break;
            }

            // close statistics buffer
            OTF_RStream_closeStatsBuffer( rstream );
         }

         // open stream for writing
         //
         OTF_WStream * wstream =
         OTF_WStream_open( tmp_out_file_prefix.c_str(), streamid, manager );
         assert( wstream );

         PVPrint( 3, " Opened OTF writer stream [namestub %s id %x]\n",
                  tmp_out_file_prefix.c_str(), streamid );

#if (defined(VT_UNIFY_HOOKS_AEVENTS) || defined(VT_UNIFY_HOOKS_MARGINS))
         if( m_scope == SCOPE_EVENTS )
         {
            // trigger generic hooks for opened event stream
            theHooks->triggerGenericHook(
               VT_UNIFY_HOOKS_AEVENTS_GENID__EVENT_STREAM_OPEN |
               VT_UNIFY_HOOKS_MARGINS_GENID__EVENT_STREAM_OPEN, 3,
               &wstream, const_cast<uint32_t*>( &streamid ),
               const_cast<std::string*>( &in_file_prefix ) );
         }
#endif // VT_UNIFY_HOOKS_AEVENTS || VT_UNIFY_HOOKS_MARGINS

         // set file compression
         //
         if( Params.docompress )
         {
            OTF_WStream_setCompression( wstream,
               OTF_FILECOMPRESSION_COMPRESSED );
         }

         // create record handler array
         //
         OTF_HandlerArray * handler_array = OTF_HandlerArray_open();
         assert( handler_array );

         if( m_scope == SCOPE_EVENTS )
         {
            // create first handler argument
            FirstHandlerArg_EventsS fha( wstream );

            // set record handler and its first argument for ...
            //

            // ... OTF_EVENTCOMMENT_RECORD
            OTF_HandlerArray_setHandler( handler_array,
               (OTF_FunctionPointer*)HandleEventComment,
               OTF_EVENTCOMMENT_RECORD );
            OTF_HandlerArray_setFirstHandlerArg( handler_array, &fha,
               OTF_EVENTCOMMENT_RECORD );

            // ... OTF_ENTER_RECORD
            OTF_HandlerArray_setHandler( handler_array,
               (OTF_FunctionPointer*)HandleEnter,
               OTF_ENTER_RECORD );
            OTF_HandlerArray_setFirstHandlerArg( handler_array, &fha,
               OTF_ENTER_RECORD );

            // ... OTF_LEAVE_RECORD
            OTF_HandlerArray_setHandler( handler_array,
               (OTF_FunctionPointer*)HandleLeave,
               OTF_LEAVE_RECORD );
            OTF_HandlerArray_setFirstHandlerArg( handler_array, &fha,
               OTF_LEAVE_RECORD );

            // ... OTF_COUNTER_RECORD
            OTF_HandlerArray_setHandler( handler_array,
               (OTF_FunctionPointer*)HandleCounter,
               OTF_COUNTER_RECORD );
            OTF_HandlerArray_setFirstHandlerArg( handler_array, &fha,
               OTF_COUNTER_RECORD );

            // ... OTF_BEGINFILEOPERATION_RECORD
            OTF_HandlerArray_setHandler( handler_array,
               (OTF_FunctionPointer*)HandleBeginFileOp,
               OTF_BEGINFILEOP_RECORD );
            OTF_HandlerArray_setFirstHandlerArg( handler_array, &fha,
               OTF_BEGINFILEOP_RECORD );

            // ... OTF_ENDFILEOPERATION_RECORD
            OTF_HandlerArray_setHandler( handler_array,
               (OTF_FunctionPointer*)HandleEndFileOp,
               OTF_ENDFILEOP_RECORD );
            OTF_HandlerArray_setFirstHandlerArg( handler_array, &fha,
               OTF_ENDFILEOP_RECORD );

            // ... OTF_SEND_RECORD
            OTF_HandlerArray_setHandler( handler_array,
               (OTF_FunctionPointer*)HandleSendMsg,
               OTF_SEND_RECORD );
            OTF_HandlerArray_setFirstHandlerArg( handler_array, &fha,
               OTF_SEND_RECORD );

            // ... OTF_RECEIVE_RECORD
            OTF_HandlerArray_setHandler( handler_array,
               (OTF_FunctionPointer*)HandleRecvMsg,
               OTF_RECEIVE_RECORD );
            OTF_HandlerArray_setFirstHandlerArg( handler_array, &fha,
               OTF_RECEIVE_RECORD );

            // ... OTF_BEGINCOLLOP_RECORD
            OTF_HandlerArray_setHandler( handler_array,
               (OTF_FunctionPointer*)HandleBeginCollOp,
               OTF_BEGINCOLLOP_RECORD );
            OTF_HandlerArray_setFirstHandlerArg( handler_array, &fha,
               OTF_BEGINCOLLOP_RECORD );

            // ... OTF_ENDCOLLOP_RECORD
            OTF_HandlerArray_setHandler( handler_array,
               (OTF_FunctionPointer*)HandleEndCollOp,
               OTF_ENDCOLLOP_RECORD );
            OTF_HandlerArray_setFirstHandlerArg( handler_array, &fha,
               OTF_ENDCOLLOP_RECORD );

            // ... OTF_RMAPUT_RECORD
            OTF_HandlerArray_setHandler( handler_array,
               (OTF_FunctionPointer*)HandleRMAPut,
               OTF_RMAPUT_RECORD );
            OTF_HandlerArray_setFirstHandlerArg( handler_array, &fha,
               OTF_RMAPUT_RECORD );

            // ... OTF_RMAPUTRE_RECORD
            OTF_HandlerArray_setHandler( handler_array,
               (OTF_FunctionPointer*)HandleRMAPutRemoteEnd,
               OTF_RMAPUTRE_RECORD );
            OTF_HandlerArray_setFirstHandlerArg( handler_array, &fha,
               OTF_RMAPUTRE_RECORD );

            // ... OTF_RMAGET_RECORD
            OTF_HandlerArray_setHandler( handler_array,
               (OTF_FunctionPointer*)HandleRMAGet,
               OTF_RMAGET_RECORD );
            OTF_HandlerArray_setFirstHandlerArg( handler_array, &fha,
               OTF_RMAGET_RECORD );

            // ... OTF_RMAEND_RECORD
            OTF_HandlerArray_setHandler( handler_array,
               (OTF_FunctionPointer*)HandleRMAEnd,
               OTF_RMAEND_RECORD );
            OTF_HandlerArray_setFirstHandlerArg( handler_array, &fha,
               OTF_RMAEND_RECORD );

            // rewrite events
            //
            if( OTF_RStream_readEvents( rstream, handler_array ) ==
                OTF_READ_ERROR )
            {
               std::cerr << ExeName << ": Error: "
                         << "Could not read events of OTF stream [namestub "
                         << in_file_prefix << " id "
                         << std::hex << streamid << "]"
                         << std::dec << std::endl;
               error = true;
            }
         }
         else // m_scope == SCOPE_STATS
         {
            // create first handler argument
            FirstHandlerArg_StatsS fha( wstream );

            // set record handler and its first argument for ...
            //

            // ... OTF_FUNCTIONSUMMARY_RECORD
            OTF_HandlerArray_setHandler( handler_array,
               (OTF_FunctionPointer*)HandleFunctionSummary,
               OTF_FUNCTIONSUMMARY_RECORD );
            OTF_HandlerArray_setFirstHandlerArg( handler_array, &fha,
               OTF_FUNCTIONSUMMARY_RECORD );

            // ... OTF_MESSAGESUMMARY_RECORD
            OTF_HandlerArray_setHandler( handler_array,
               (OTF_FunctionPointer*)HandleMessageSummary,
               OTF_MESSAGESUMMARY_RECORD );
            OTF_HandlerArray_setFirstHandlerArg( handler_array, &fha,
               OTF_MESSAGESUMMARY_RECORD );

            // ... OTF_COLLOPSUMMARY_RECORD
            OTF_HandlerArray_setHandler( handler_array,
               (OTF_FunctionPointer*)HandleCollOpSummary,
               OTF_COLLOPSUMMARY_RECORD );
            OTF_HandlerArray_setFirstHandlerArg( handler_array, &fha,
               OTF_COLLOPSUMMARY_RECORD );

            // ... OTF_FILEOPERATIONSUMMARY_RECORD
            OTF_HandlerArray_setHandler( handler_array,
               (OTF_FunctionPointer*)HandleFileOpSummary,
               OTF_FILEOPERATIONSUMMARY_RECORD );
            OTF_HandlerArray_setFirstHandlerArg( handler_array, &fha,
               OTF_FILEOPERATIONSUMMARY_RECORD );

            // rewrite statistics
            //
            if( OTF_RStream_readStatistics( rstream, handler_array ) ==
                OTF_READ_ERROR )
            {
               std::cerr << ExeName << ": Error: "
                         << "Could not read statistics of OTF stream [namestub "
                         << in_file_prefix << " id "
                         << std::hex << streamid << "]"
                         << std::dec << std::endl;
               error = true;
            }
         }

#if (defined(VT_UNIFY_HOOKS_AEVENTS) || defined(VT_UNIFY_HOOKS_MARGINS))
         if( m_scope == SCOPE_EVENTS )
         {
            // trigger generic hooks for closing event stream
            theHooks->triggerGenericHook(
               VT_UNIFY_HOOKS_AEVENTS_GENID__EVENT_STREAM_CLOSE |
               VT_UNIFY_HOOKS_MARGINS_GENID__EVENT_STREAM_CLOSE, 1,
               const_cast<uint32_t*>( &streamid ) );
         }
#endif // VT_UNIFY_HOOKS_AEVENTS || VT_UNIFY_HOOKS_MARGINS

         // close writer stream
         OTF_WStream_close( wstream );

         PVPrint( 3, " Closed OTF writer stream [namestub %s id %x]\n",
               tmp_out_file_prefix.c_str(), streamid );

         // close record handler
         OTF_HandlerArray_close( handler_array );

      } while( false );

      // close reader stream
      OTF_RStream_close( rstream );
      // close file manager
      OTF_FileManager_close( manager );

      PVPrint( 3, " Closed OTF reader stream [namestub %s id %x]\n",
               in_file_prefix.c_str(), streamid );
   }

   return !error;
}
