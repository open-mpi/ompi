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
#include "vt_unify_events.h"
#include "vt_unify_events_hdlr.h"

#include "otf.h"

#include <iostream>
#include <string>

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

Events * theEvents; // instance of class Events

//////////////////// class Events ////////////////////

// public methods
//

Events::Events()
{
   // Empty
}

Events::~Events()
{
   // Empty
}

bool
Events::run()
{
   VPrint( 1, "Unifying events\n" );

   bool error = false;

   int size = (int)g_vecUnifyCtls.size();
   int begin = 0;
   int end = size;
   int i;

   std::string tmp_out_file_prefix =
      Params.out_file_prefix + TmpFileSuffix;

   // prepare parallel unify
   //

#ifdef VT_MPI
   if( g_iMPISize > 1 )
   {
      VTUnify_MPI_Barrier( VTUnify_MPI_COMM_WORLD );

      // calculate lower/upper bound of unify controls for my MPI rank
      //
      int nworker = ((int)g_iMPISize > size) ? size : (int)g_iMPISize;
      int bsize = size / nworker;
      begin = (int)g_iMPIRank * bsize;
      end = begin + bsize;
      if( ((int)g_iMPIRank == nworker - 1) && end != size )
         end = size;
   }
#endif // VT_MPI

   // unify events
   //

#if defined(HAVE_OMP) && HAVE_OMP
   // set number of threads
   if( end-begin < omp_get_max_threads() )
      omp_set_num_threads( end-begin );
   VPrint( 1, " Using %d thread(s)\n", omp_get_max_threads() );
#  pragma omp parallel for private(i)
#endif // HAVE_OMP
   for( i = begin; i < end; i++ )
   {
      if( !error )
      {
	 // open file manager for reader stream
	 OTF_FileManager * p_org_events_manager =
	    OTF_FileManager_open( 1 );
	 assert( p_org_events_manager );

	 // open stream for reading
	 OTF_RStream * p_org_events_rstream =
	    OTF_RStream_open( Params.in_file_prefix.c_str(),
			      g_vecUnifyCtls[i]->streamid,
			      p_org_events_manager );
	 assert( p_org_events_rstream );

         PVPrint( 2, " Opened OTF reader stream [namestub %s id %x]\n",
                  Params.in_file_prefix.c_str(),
                  g_vecUnifyCtls[i]->streamid );

	 if( !OTF_RStream_getEventBuffer( p_org_events_rstream ) )
	 {
            PVPrint( 2, "  No events found in this OTF reader stream "
                        "- Ignored\n" );
	 }
	 else
	 {
	    // close event buffer
	    OTF_RStream_closeEventBuffer( p_org_events_rstream );

	    // open file manager for writer stream
	    OTF_FileManager * p_uni_events_manager =
	       OTF_FileManager_open( 1 );
	    assert( p_uni_events_manager );

	    // open stream for writing
	    OTF_WStream * p_uni_events_wstream =
	       OTF_WStream_open( tmp_out_file_prefix.c_str(),
				 g_vecUnifyCtls[i]->streamid,
				 p_uni_events_manager );
	    assert( p_uni_events_wstream );

            PVPrint( 2, " Opened OTF writer stream [namestub %s id %x]\n",
                     tmp_out_file_prefix.c_str(),
                     g_vecUnifyCtls[i]->streamid );

	    // create record handler
	    OTF_HandlerArray * p_handler_array =
	       OTF_HandlerArray_open();
	    assert( p_handler_array );

	    // set record handler and first handler argument for ...
	    //

	    // ... OTF_ENTER_RECORD
	    OTF_HandlerArray_setHandler( p_handler_array,
               (OTF_FunctionPointer*)Handle_Enter, OTF_ENTER_RECORD );
	    OTF_HandlerArray_setFirstHandlerArg( p_handler_array,
	       p_uni_events_wstream, OTF_ENTER_RECORD );

	    // ... OTF_LEAVE_RECORD
	    OTF_HandlerArray_setHandler( p_handler_array,
               (OTF_FunctionPointer*)Handle_Leave, OTF_LEAVE_RECORD );
	    OTF_HandlerArray_setFirstHandlerArg( p_handler_array,
	       p_uni_events_wstream, OTF_LEAVE_RECORD );

	    // ... OTF_FILEOPERATION_RECORD
	    OTF_HandlerArray_setHandler( p_handler_array,
	       (OTF_FunctionPointer*)Handle_FileOperation,
	          OTF_FILEOPERATION_RECORD );
	    OTF_HandlerArray_setFirstHandlerArg( p_handler_array,
	       p_uni_events_wstream, OTF_FILEOPERATION_RECORD );

	    // ... OTF_BEGINFILEOPERATION_RECORD
	    OTF_HandlerArray_setHandler( p_handler_array,
	       (OTF_FunctionPointer*)Handle_BeginFileOperation,
	          OTF_BEGINFILEOP_RECORD );
	    OTF_HandlerArray_setFirstHandlerArg( p_handler_array,
	       p_uni_events_wstream, OTF_BEGINFILEOP_RECORD );

	    // ... OTF_ENDFILEOPERATION_RECORD
	    OTF_HandlerArray_setHandler( p_handler_array,
	       (OTF_FunctionPointer*)Handle_EndFileOperation,
	          OTF_ENDFILEOP_RECORD );
	    OTF_HandlerArray_setFirstHandlerArg( p_handler_array,
	       p_uni_events_wstream, OTF_ENDFILEOP_RECORD );

	    // ... OTF_SEND_RECORD
	    OTF_HandlerArray_setHandler( p_handler_array,
               (OTF_FunctionPointer*)Handle_SendMsg, OTF_SEND_RECORD );
	    OTF_HandlerArray_setFirstHandlerArg( p_handler_array,
	       p_uni_events_wstream, OTF_SEND_RECORD );

	    // ... OTF_RECEIVE_RECORD
	    OTF_HandlerArray_setHandler( p_handler_array,
               (OTF_FunctionPointer*)Handle_RecvMsg, OTF_RECEIVE_RECORD );
	    OTF_HandlerArray_setFirstHandlerArg( p_handler_array,
	       p_uni_events_wstream, OTF_RECEIVE_RECORD );

	    // ... OTF_COLLOP_RECORD
	    OTF_HandlerArray_setHandler( p_handler_array,
               (OTF_FunctionPointer*)Handle_CollectiveOperation,
                  OTF_COLLOP_RECORD );
	    OTF_HandlerArray_setFirstHandlerArg( p_handler_array,
	       p_uni_events_wstream, OTF_COLLOP_RECORD );

      // ... OTF_RMAPUT_RECORD
      OTF_HandlerArray_setHandler( p_handler_array,
               (OTF_FunctionPointer*)Handle_RMAPut,
                  OTF_RMAPUT_RECORD );
      OTF_HandlerArray_setFirstHandlerArg( p_handler_array,
         p_uni_events_wstream, OTF_RMAPUT_RECORD );

      // ... OTF_RMAPUTRE_RECORD
      OTF_HandlerArray_setHandler( p_handler_array,
               (OTF_FunctionPointer*)Handle_RMAPutRemoteEnd,
                  OTF_RMAPUTRE_RECORD );
      OTF_HandlerArray_setFirstHandlerArg( p_handler_array,
         p_uni_events_wstream, OTF_RMAPUTRE_RECORD );

      // ... OTF_RMAGET_RECORD
      OTF_HandlerArray_setHandler( p_handler_array,
               (OTF_FunctionPointer*)Handle_RMAGet,
                  OTF_RMAGET_RECORD );
      OTF_HandlerArray_setFirstHandlerArg( p_handler_array,
         p_uni_events_wstream, OTF_RMAGET_RECORD );

      // ... OTF_RMAEND_RECORD
      OTF_HandlerArray_setHandler( p_handler_array,
               (OTF_FunctionPointer*)Handle_RMAEnd,
                  OTF_RMAEND_RECORD );
      OTF_HandlerArray_setFirstHandlerArg( p_handler_array,
         p_uni_events_wstream, OTF_RMAEND_RECORD );

	    // ... OTF_COUNTER_RECORD
	    OTF_HandlerArray_setHandler( p_handler_array,
               (OTF_FunctionPointer*)Handle_Counter, OTF_COUNTER_RECORD );
	    OTF_HandlerArray_setFirstHandlerArg( p_handler_array,
	       p_uni_events_wstream, OTF_COUNTER_RECORD );

	    // ... OTF_EVENTCOMMENT_RECORD
	    OTF_HandlerArray_setHandler( p_handler_array,
               (OTF_FunctionPointer*)Handle_EventComment,
	          OTF_EVENTCOMMENT_RECORD );
	    OTF_HandlerArray_setFirstHandlerArg( p_handler_array,
	       p_uni_events_wstream, OTF_EVENTCOMMENT_RECORD );

	    // set file compression
	    if( Params.docompress )
	    {
	       OTF_WStream_setCompression( p_uni_events_wstream,
					   OTF_FILECOMPRESSION_COMPRESSED );
	    }

#ifndef VT_MPI
	    if( Params.showprogress )
	    {
	       // set record limit
	       OTF_RStream_setRecordLimit( p_org_events_rstream, 10000 );

               // initialize progress display
#if defined(HAVE_OMP) && HAVE_OMP
#              pragma omp critical
#endif // HAVE_OMP
               initProgressDisplay( (uint32_t)size );
            }
#endif // VT_MPI

	    // read events
	    //
	    uint64_t records_read = 0;
	    while( ( records_read =
		     OTF_RStream_readEvents( p_org_events_rstream,
					     p_handler_array ) ) == 10000 )
	    {
#ifndef VT_MPI
	       if( Params.showprogress )
	       {
                  uint64_t minimum; uint64_t current; uint64_t maximum;
                  float progress;
#if defined(HAVE_OMP) && HAVE_OMP
#                 pragma omp critical
                  {
#endif // HAVE_OMP
		     // calculate/update progress display
		     //
		     OTF_RStream_eventBytesProgress( p_org_events_rstream, 
						     &minimum, &current,
						     &maximum );
		     progress = ( ((float)(current - minimum) * 100.0)
				  / (float)(maximum - minimum) );
		     updateProgressDisplay( i, progress );
#if defined(HAVE_OMP) && HAVE_OMP
	          }
#endif // HAVE_OMP
	       }
#endif // VT_MPI
            }

	    // check for reading error
	    if( records_read == OTF_READ_ERROR )
	    {
#if defined(HAVE_OMP) && HAVE_OMP
#              pragma omp critical
	       {
#endif // HAVE_OMP
		  std::cerr << ExeName << ": Error: "
			    << "Could not read events of OTF stream [namestub "
			    << tmp_out_file_prefix << " id "
			    << std::hex << g_vecUnifyCtls[i]->streamid << "]"
			    << std::dec << std::endl;
		  error = true;
#if defined(HAVE_OMP) && HAVE_OMP
	       }
#endif // HAVE_OMP
	    }
	    else
	    {
#ifndef VT_MPI
	       if( Params.showprogress )
	       {
#if defined(HAVE_OMP) && HAVE_OMP
#                 pragma omp critical
#endif // HAVE_OMP
		  updateProgressDisplay( i, 100.0 );
	       }
#endif // VT_MPI
	    }

	    // close record handler
	    OTF_HandlerArray_close( p_handler_array );

	    // close writer stream
	    OTF_WStream_close( p_uni_events_wstream );
	    // close file manager for writer stream
	    OTF_FileManager_close( p_uni_events_manager );

            PVPrint( 2, " Closed OTF writer stream [namestub %s id %x]\n",
                     tmp_out_file_prefix.c_str(),
                     g_vecUnifyCtls[i]->streamid );
	 }

	 // close reader stream
	 OTF_RStream_close( p_org_events_rstream );
	 // close file manager for reader stream
	 OTF_FileManager_close( p_org_events_manager );
	 
         PVPrint( 2, " Closed OTF reader stream [namestub %s id %x]\n",
                  Params.in_file_prefix.c_str(),
                  g_vecUnifyCtls[i]->streamid );
      }
   }

   if( !error )
   {
#ifndef VT_MPI
      // finish progress display
      if( Params.showprogress ) finishProgressDisplay();
#endif // VT_MPI
   }
   else
   {
      std::cerr << ExeName << ": "
		<< "An error occurred during unifying events. Aborting"
		<< std::endl;
   }

#ifdef VT_MPI
   if( g_iMPISize > 1 )
      VTUnify_MPI_Barrier( VTUnify_MPI_COMM_WORLD );
#endif // VT_MPI

   return !error;
}

// private methods
//

#ifndef VT_MPI

inline void
Events::initProgressDisplay( const uint32_t nprogress )
{
  // already initialized?
  if( m_vecProgress.size() > 0 ) return;

  m_vecProgress.resize( nprogress );
  for( uint32_t i = 0; i < nprogress; i++ )
    m_vecProgress[i] = 0.0;

  printf( " %7.2f %%\r", 0.0 );
  fflush( stdout );
}

inline void
Events::finishProgressDisplay()
{
   if( m_vecProgress.size() > 0 )
      printf( " %7.2f %%  done\n", 100.0 );
}

inline void
Events::updateProgressDisplay( const uint32_t i, const float progress )
{
   assert( m_vecProgress.size() > 0 );

   m_vecProgress[i] = progress;

   float avg_progress = 0.0;
   for( uint32_t i = 0; i < m_vecProgress.size(); i++ )
      avg_progress += m_vecProgress[i];

   avg_progress /= (float)m_vecProgress.size();

   printf( " %7.2f %%\r", avg_progress );
   fflush( stdout );
}

#endif // VT_MPI
