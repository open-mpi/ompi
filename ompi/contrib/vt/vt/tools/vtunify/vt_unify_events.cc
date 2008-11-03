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

#include "vt_unify_events.h"
#include "vt_unify.h"
#include "vt_unify_events_hdlr.h"

#include "vt_inttypes.h"

#include "otf.h"

#include <iostream>
#include <string>

#include <assert.h>
#include <stdlib.h>

#if (defined (VT_OMP))
#include <omp.h>
#endif

#define UPDATE_PROGESSBAR                                     \
{                                                             \
   uint32_t sum_progress = 0;                                 \
   for( uint32_t j = 0; j < vec_progress.size(); j++ )        \
      sum_progress += vec_progress[j];                        \
	                                                      \
   uint32_t percentage = sum_progress / vec_progress.size();  \
	                                                      \
   if( percentage < 10 )                                      \
      std::cout << "00" << percentage << "%\r" << std::flush; \
   else if( percentage < 100 )                                \
      std::cout << "0" << percentage << "%\r" << std::flush;  \
   else                                                       \
      std::cout << percentage << "%\r" << std::flush;         \
}

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
   if( Params.beverbose )
      std::cout << "Unifying events ..." << std::endl;

   bool error = false;

   int i;
   int size = (int)g_vecUnifyCtls.size();

   std::string tmp_out_file_prefix =
      Params.out_file_prefix + TmpFileSuffix;

   std::vector< uint32_t > vec_progress;
   vec_progress.resize( size );
   for( i = 0; i < size; i++ )
      vec_progress[i] = 0;

#if (defined (VT_OMP))

   uint32_t threads_needed = g_vecUnifyCtls.size();
   uint32_t threads_set;

   uint32_t omp_max_threads = omp_get_max_threads();
   char * env_omp_num_threads = getenv( "OMP_NUM_THREADS" );

   if( env_omp_num_threads )
   {
      if( threads_needed < (uint32_t)atoi( env_omp_num_threads ) )
      {
	 omp_set_num_threads( threads_needed );
	 threads_set = threads_needed;
      }
      else
      {
	 threads_set = atoi( env_omp_num_threads );
      }
   }
   else
   {
      if( threads_needed < omp_max_threads )
      {
	 omp_set_num_threads( threads_needed );
	 threads_set = threads_needed;
      }
      else
      {
	 omp_set_num_threads( omp_max_threads );
	 threads_set = omp_max_threads;
      }
   }

   if( Params.beverbose )
   {
      std::cout << " Using " << threads_set << " worker thread(s)" << std::endl;
   }

#  pragma omp parallel for private(i)
#endif
   for( i = 0; i < size; i++ )
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

	 if( Params.beverbose )
	 {
#if (defined (VT_OMP))
#           pragma omp critical
	    {
	       std::cout << " [" << omp_get_thread_num() << "]:" << std::flush;
#endif
	       std::cout << " Opened OTF reader stream [namestub "
			 << Params.in_file_prefix << " id "
			 << std::hex << g_vecUnifyCtls[i]->streamid << "]" 
			 << std::dec << std::endl;
	       
#if (defined (VT_OMP))
	    }
#endif
	 }

	 if( !OTF_RStream_getEventBuffer( p_org_events_rstream ) )
	 {
	    if( Params.beverbose )
	    {
#if (defined (VT_OMP))
#              pragma omp critical
	       {
		  std::cout << " [" << omp_get_thread_num() << "]:"
			    << std::flush;
#endif
		  std::cout << "  No events found in this OTF reader stream "
			    << "- Ignored" << std::endl;
#if (defined (VT_OMP))
	       }
#endif
	    }
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
	 
	    if( Params.beverbose )
	    {
#if (defined (VT_OMP))
#              pragma omp critical
	       {
		  std::cout << " [" << omp_get_thread_num() << "]:"
			    << std::flush;
#endif
		  std::cout << " Opened OTF writer stream [namestub "
			    << tmp_out_file_prefix << " id "
			    << std::hex << g_vecUnifyCtls[i]->streamid << "]"
			    << std::dec << std::endl;
#if (defined (VT_OMP))
	       }
#endif
	    }

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

	    // set record limit
	    if( Params.beverbose )
	       OTF_RStream_setRecordLimit( p_org_events_rstream, 10000 );

	    // read events
	    //
	    uint64_t records_read = 0;
	    uint64_t minimum; uint64_t current; uint64_t maximum;
	    while( ( records_read =
		     OTF_RStream_readEvents( p_org_events_rstream,
					     p_handler_array ) ) == 10000 )
	    {
	       if( Params.beverbose )
	       {
#if (defined (VT_OMP))
#                 pragma omp critical
		  {
#endif
		     // calculate progress bar
		     //
		     OTF_RStream_eventProgress( p_org_events_rstream, 
						&minimum, &current, &maximum );
		     vec_progress[i] = ( ( current - minimum ) * 100 )
			/ ( maximum - minimum );
		  
		     UPDATE_PROGESSBAR;
		  }
#if (defined (VT_OMP))
	       }
#endif
	    }
      
	    // check for reading error
	    if( records_read == OTF_READ_ERROR )
	    {
#if (defined (VT_OMP))
#              pragma omp critical
	       {
#endif
		  std::cerr << ExeName << ": Error: "
			    << "Could not read events of OTF stream [namestub "
			    << tmp_out_file_prefix << " id "
			    << std::hex << g_vecUnifyCtls[i]->streamid << "]"
			    << std::dec << std::endl;
		  error = true;
#if (defined (VT_OMP))
	       }
#endif
	    }

	    // close record handler
	    OTF_HandlerArray_close( p_handler_array );

	    // close writer stream
	    OTF_WStream_close( p_uni_events_wstream );
	    // close file manager for writer stream
	    OTF_FileManager_close( p_uni_events_manager );

	    if( Params.beverbose )
	    {
#if (defined (VT_OMP))
#              pragma omp critical
	       {
		  std::cout << " [" << omp_get_thread_num() << "]:"
			    << std::flush;
#endif
		  std::cout << " Closed OTF writer stream [namestub "
			    << tmp_out_file_prefix << " id "
			    << std::hex << g_vecUnifyCtls[i]->streamid << "]"
			    << std::dec << std::endl;
#if (defined (VT_OMP))
	       }
#endif
	    }
	 }

	 // close reader stream
	 OTF_RStream_close( p_org_events_rstream );
	 // close file manager for reader stream
	 OTF_FileManager_close( p_org_events_manager );
	 
	 if( Params.beverbose )
	 {
#if (defined (VT_OMP))
#           pragma omp critical
	    {
	       std::cout << " [" << omp_get_thread_num() << "]:" << std::flush;
#endif
	       std::cout << " Closed OTF reader stream [namestub "
			 << Params.in_file_prefix << " id "
			 << std::hex << g_vecUnifyCtls[i]->streamid << "]"
			 << std::dec << std::endl;
	       vec_progress[i] = 100;
	       UPDATE_PROGESSBAR;
#if (defined (VT_OMP))
	    }
#endif
	 }
      }
   }

   if( Params.beverbose )
      std::cout << std::endl;

   if( error )
   {
      std::cerr << ExeName << ": "
		<< "An error occurred during unifying events - Terminating ..."
		<< std::endl;
   }
   
   return !error;
}
