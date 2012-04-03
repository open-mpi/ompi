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

#include "vt_unify_defs_recs.h"
#include "vt_unify_hooks_msgmatch.h"
#include "vt_unify_sync.h"
#include "vt_unify_tkfac.h"
#include "vt_unify_usrcom.h"

#include "otf.h"

#include <iostream>
#include <list>
#include <map>
#include <set>
#include <vector>

//////////////////// class HooksMsgMatchC ////////////////////

// public methods
//

HooksMsgMatchC::HooksMsgMatchC() : HooksBaseC(),
   m_maxThreads( 1 ), m_matchContexts( 0 )
{
   // initialize global key tokens
   for( uint8_t i = 0; i < KEY_NUM; i++ )
      m_keyTokens[i] = 0;
}

HooksMsgMatchC::~HooksMsgMatchC()
{
   // Empty
}

// private methods
//

// event record handlers
//

int
HooksMsgMatchC::HandleEventComment( void * userData,
   uint64_t time, uint32_t proc, const char * comment )
{
#ifdef VT_ETIMESYNC
   // update time sync. parameters, if necessary
   //
   if( theTimeSync->getSyncMethod() == TimeSyncC::METHOD_ENHANCED &&
       strcmp( comment, VT_UNIFY_STRID_ETIMESYNC_COMMENT ) == 0 )
   {
      theTimeSync->updateSyncParam( proc );
   }
#endif // VT_ETIMESYNC

   return OTF_RETURN_OK;
}

int
HooksMsgMatchC::HandleRecvMsg( LargeVectorC<RecvMsgS*> * recvMsgs,
   uint64_t time, uint32_t receiver, uint32_t sender, uint32_t comm,
   uint32_t tag, uint32_t length, uint32_t scl )
{
   // get global token factory for DefProcessGroup
   static const TokenFactoryScopeI * tkfac_defprocgrp =
      theTokenFactory->getScope( DEF_REC_TYPE__DefProcessGroup );

   // get global token factory for DefScl
//   static const TokenFactoryScopeI * tkfac_defscl =
//      theTokenFactory->getScope( DEF_REC_TYPE__DefScl );

   // translate local comm. token
   //
   uint32_t global_comm = tkfac_defprocgrp->translate( receiver, comm );
   assert( global_comm != 0 );

   // translate local scl token, if necessary
   //
//   uint32_t global_scl = scl;
//    if( scl != 0 )
//    {
//       global_scl = tkfac_defscl.translate( receiver, scl );
//       assert( global_scl != 0 );
//    }

   // correct time
   time = theTimeSync->correctTime( receiver, time );

   // get sender process id, if it's an user communication
   //
   if( theUserCom->isUserComm( global_comm ) &&
       ( sender = theUserCom->getSender( global_comm, tag ) ) == 0 )
   {
      // ignore receive message, if no sender process id found
      if( sender == 0 )
         return OTF_RETURN_OK;
   }

   // add receive message to vector, if sender stream is available
   //
   if( StreamId2UnifyCtl[sender]->stream_avail )
   {
      recvMsgs->push_back(
         new RecvMsgS( time, sender, receiver, global_comm, tag
            /*, length, global_scl*/ ) );
      assert( recvMsgs->back() );
   }

   return OTF_RETURN_OK;
}

// vvvvvvvvvvvvvvvvvvvv HOOK METHODS vvvvvvvvvvvvvvvvvvvv

// initialization/finalization hooks
//

void
HooksMsgMatchC::initHook()
{
   // Empty
}

void
HooksMsgMatchC::finalizeHook( const bool & error )
{
   // Empty
}

// phase hooks
//

void
HooksMsgMatchC::phaseHook_UnifyDefinitions_pre()
{
   MASTER
   {
      // get global token factory for DefKeyValue
      TokenFactoryScopeI * tkfac_defkeyval =
         theTokenFactory->getScope( DEF_REC_TYPE__DefKeyValue );

      DefRec_DefKeyValueS new_keyval;

      // create global key definition for matching receive ...
      //

      // ... timestamp
      new_keyval.type = OTFAUX_KEYVALUE_TUD_P2P_RECEIVED_TIME_TYPE;
      new_keyval.name = OTFAUX_KEYVALUE_TUD_P2P_RECEIVED_TIME_NAME;
      m_keyTokens[KEY_TIME] = tkfac_defkeyval->create( &new_keyval );
      // ... length
//      new_keyval.type = OTFAUX_KEYVALUE_TUD_P2P_RECEIVED_SIZE_TYPE;
//      new_keyval.name = OTFAUX_KEYVALUE_TUD_P2P_RECEIVED_SIZE_NAME;
//      m_keyTokens[KEY_LENGTH] = tkfac_defkeyval->create( new_keyval );
      // ... source code location
//      new_keyval.type = OTFAUX_KEYVALUE_TUD_P2P_RECEIVED_SCL_TYPE;
//      new_keyval.name = OTFAUX_KEYVALUE_TUD_P2P_RECEIVED_SCL_NAME;
//      m_keyTokens[KEY_SCL] = tkfac_defkeyval->create( new_keyval );
   }
}

void
HooksMsgMatchC::phaseHook_UnifyEvents_pre()
{
   bool error = false;

   VPrint( 2, " Preparing message matching\n" );

#if defined(HAVE_OMP) && HAVE_OMP
   // update maximum number of threads to use for unifying events
   m_maxThreads = omp_get_max_threads();
#endif // HAVE_OMP

   do
   {
      // vector of receive messages
      LargeVectorC<RecvMsgS*> recv_msgs( 1000000 );

      // read receive messages
      //
      error = !getRecvMsgs( recv_msgs );
      if( SyncError( &error ) )
         break;

      // enqueue receive messages to message matching context(s)
      //
      error = !enqueueRecvMsgs( recv_msgs );
      if( SyncError( &error ) )
         break;

#ifdef VT_MPI
      if( NumRanks > 1 )
      {
         // broadcast global key tokens to all ranks
         CALL_MPI( MPI_Bcast( m_keyTokens, KEY_NUM, MPI_UNSIGNED, 0,
                              MPI_COMM_WORLD ) );
      }
#endif // VT_MPI

   } while( false );

   if( !error )
      VPrint( 2, " Continuing unification of events\n" );

   //return !error;
   assert( !error );
}

void
HooksMsgMatchC::phaseHook_UnifyEvents_post()
{
   // destroy message matching context(s), if necessary
   if( m_matchContexts )
      delete [] m_matchContexts;
}

// record hooks
//

void
HooksMsgMatchC::writeRecHook_SendMsg( HooksC::VaArgsT & args )
{
   // return, if there are no receive messages to match
   if( !m_matchContexts ) return;

#if defined(HAVE_OMP) && HAVE_OMP
   const int threadid = omp_get_thread_num();
#else // HAVE_OMP
   const int threadid = 0;
#endif // HAVE_OMP

   // get hook arguments
   //

   //OTF_WStream **      wstream  = (OTF_WStream**)args[0];
   uint64_t *          time     = (uint64_t*)args[1];
   uint32_t *          sender   = (uint32_t*)args[2];
   uint32_t *          receiver = (uint32_t*)args[3];
   uint32_t *          comm     = (uint32_t*)args[4];
   uint32_t *          tag      = (uint32_t*)args[5];
   uint32_t *          length   = (uint32_t*)args[6];
   uint32_t *          scl      = (uint32_t*)args[7];
   OTF_KeyValueList ** kvs      = (OTF_KeyValueList**)args[8];
   bool *              do_write = (bool*)args[9];

   // return, if this record was dropped by another hook class
   if( !(*do_write) )
      return;

   // try to get timestamp, length, and scl of a matching receive message
   //

   uint64_t recv_time;
   uint32_t recv_length;
   uint32_t recv_scl;

   if( m_matchContexts[threadid].matchSend( *sender, *receiver, *comm, *tag,
          recv_time, recv_length, recv_scl ) )
   {
      // append receive timestamp, length, and scl to key-values, if matched
      //
      OTF_KeyValueList_appendUint64( *kvs, m_keyTokens[KEY_TIME], recv_time );
//      OTF_KeyValueList_appendUint32( *kvs, m_keyTokens[KEY_LENGTH], recv_length );
//      OTF_KeyValueList_appendUint32( *kvs, m_keyTokens[KEY_SCL], recv_scl );
   }
   else
   {
      // otherwise, show a warning message
      PVPrint( 3, "  Warning: No matching message recv. event found "
         "[send msg.: time %llu sender %u receiver %u comm %u tag %u "
         "length %u scl %u]\n",
         (unsigned long long int)*time, *sender, *receiver, *comm, *tag,
         *length, *scl );
   }
}

void
HooksMsgMatchC::writeRecHook_RecvMsg( HooksC::VaArgsT & args )
{
   // get hook arguments
   //

   //OTF_WStream **      wstream  = (OTF_WStream**)args[0];
   //uint64_t *          time     = (uint64_t*)args[1];
   //uint32_t *          receiver = (uint32_t*)args[2];
   //uint32_t *          sender   = (uint32_t*)args[3];
   //uint32_t *          comm     = (uint32_t*)args[4];
   //uint32_t *          tag      = (uint32_t*)args[5];
   //uint32_t *          length   = (uint32_t*)args[6];
   //uint32_t *          scl      = (uint32_t*)args[7];
   //OTF_KeyValueList ** kvs      = (OTF_KeyValueList**)args[8];
   bool *              do_write = (bool*)args[9];

   // drop message receive event, if desired
   if( *do_write && Params.droprecvs )
      *do_write = false;
}

// ^^^^^^^^^^^^^^^^^^^^ HOOK METHODS ^^^^^^^^^^^^^^^^^^^^

bool
HooksMsgMatchC::getRecvMsgs( LargeVectorC<RecvMsgS*> & recvMsgs )
{
   bool error = false;

   VPrint( 2, "  Reading receive messages\n" );

   // get input file prefix
   const std::string & in_file_prefix = Params.in_file_prefix;

   // create receive message vectors for each thread
   //
   LargeVectorC<RecvMsgS*> ** recv_msgs =
      new LargeVectorC<RecvMsgS*>*[m_maxThreads];
   assert( recv_msgs );
   *recv_msgs = &recvMsgs;
   for( int i = 1; i < m_maxThreads; i++ )
   {
      recv_msgs[i] = new LargeVectorC<RecvMsgS*>( recvMsgs.chunkSize() );
      assert( recv_msgs[i] );
   }

   int streams_num = (int)MyStreamIds.size();
   int i;

#if defined(HAVE_OMP) && HAVE_OMP
#  pragma omp parallel for private(i)
#endif // HAVE_OMP
   for( i = 0; i < streams_num; i++ )
   {
#if defined(HAVE_OMP) && HAVE_OMP
#     pragma omp flush(error)
      if( error ) continue;
      const int threadid = omp_get_thread_num();
#else // HAVE_OMP
      if( error ) break;
      const int threadid = 0;
#endif // HAVE_OMP

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

      PVPrint( 3, "   Opened OTF reader stream [namestub %s id %x]\n",
               in_file_prefix.c_str(), streamid );

      do
      {
         // try to get events buffer
         //
         if( !OTF_RStream_getEventBuffer( rstream ) )
         {
            PVPrint( 3, "    No events found in this OTF reader stream "
                        "- Ignored\n" );
            break;
         }

         // close events buffer
         OTF_RStream_closeEventBuffer( rstream );

         // create record handler array
         //
         OTF_HandlerArray * handler_array = OTF_HandlerArray_open();
         assert( handler_array );

         // set record handler and its first argument for ...
         //

         // ... OTF_EVENTCOMMENT_RECORD
         OTF_HandlerArray_setHandler( handler_array,
            (OTF_FunctionPointer*)HooksMsgMatchC::HandleEventComment,
            OTF_EVENTCOMMENT_RECORD );

         // ... OTF_RECEIVE_RECORD
         OTF_HandlerArray_setHandler( handler_array,
            (OTF_FunctionPointer*)HooksMsgMatchC::HandleRecvMsg,
            OTF_RECEIVE_RECORD );
         OTF_HandlerArray_setFirstHandlerArg( handler_array,
            recv_msgs[threadid], OTF_RECEIVE_RECORD );

         // read events
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

         // close record handler
         OTF_HandlerArray_close( handler_array );

      } while( false );

      // close reader stream
      OTF_RStream_close( rstream );
      // close file manager
      OTF_FileManager_close( manager );

      PVPrint( 3, "   Closed OTF reader stream [namestub %s id %x]\n",
               in_file_prefix.c_str(), streamid );

#ifdef VT_ETIMESYNC
      // reset time sync. parameters, if necessary
      if( !error && theTimeSync->getSyncMethod() == TimeSyncC::METHOD_ENHANCED )
         theTimeSync->resetSyncParam( streamid );
#endif // VT_ETIMESYNC
   }

   if( !SyncError( &error ) )
   {
      // merge thread's receive message vectors to one
      //
      for( int i = 1; i < m_maxThreads; i++ )
      {
         for( uint32_t j = 0; j < recv_msgs[i]->size(); j++ )
            recvMsgs.push_back( (*(recv_msgs[i]))[j] );
         recv_msgs[i]->clear();
         delete recv_msgs[i];
      }

#ifdef VT_MPI
      // distribute receive messages to ranks which processes the corresponding
      // sender streams, if necessary
      //
      if( NumRanks > 1 )
      {
         error = !distRecvMsgs( recvMsgs );
//         SyncError( &error );
      }
#endif // VT_MPI
   }

   delete [] recv_msgs;

   return !error;
}

#ifdef VT_MPI

bool
HooksMsgMatchC::distRecvMsgs( LargeVectorC<RecvMsgS*> & recvMsgs )
{
   bool error = false;

   assert( NumRanks > 1 );

   // block until all ranks have reached this point
   CALL_MPI( MPI_Barrier( MPI_COMM_WORLD ) );

   VPrint( 2, "  Distributing receive messages\n" );

   // get ranks which processes the corresponding sender streams
   //

   // map rank (processes the sender stream) <-> receive messages
   std::map<VT_MPI_INT, LargeVectorC<RecvMsgS*> > rank2recv_msgs;

   for( uint32_t i = 0; i < recvMsgs.size(); i++ )
   {
      // search rank which processes the sender stream
      //
      std::map<uint32_t, VT_MPI_INT>::const_iterator it =
         StreamId2Rank.find( recvMsgs[i]->sender );
      assert( it != StreamId2Rank.end() );

      // add receive message to map
      rank2recv_msgs[it->second].push_back( recvMsgs[i] );
   }

   // create MPI datatype for RecvMsgS
   //

   MPI_Datatype recv_msg_type;

   {
      RecvMsgS recv_msg_struct;

      VT_MPI_INT bcounts[2] = { 1, 4/*+1(length) +1(scl)*/ };
      MPI_Aint bdispl[2];
      MPI_Datatype btypes[2] = { MPI_LONG_LONG_INT, MPI_UNSIGNED };

      CALL_MPI( MPI_Address( &(recv_msg_struct.time), &bdispl[0] ) );
      CALL_MPI( MPI_Address( &(recv_msg_struct.sender), &bdispl[1] ) );
      bdispl[1] = bdispl[1] - bdispl[0];
      bdispl[0] = 0;
      CALL_MPI( MPI_Type_struct( 2, bcounts, bdispl, btypes, &recv_msg_type ) );
      CALL_MPI( MPI_Type_commit( &recv_msg_type ) );
   }

   // allocate memory for the send buffer
   //
   RecvMsgS * sendbuf = new RecvMsgS[recvMsgs.size()];
   assert( sendbuf );

   // get sendcounts, senddispls, and fill the send buffer
   //

   VT_MPI_INT * sendcounts = new VT_MPI_INT[NumRanks];
   assert( sendcounts );
   VT_MPI_INT * senddispls = new VT_MPI_INT[NumRanks];
   assert( senddispls );

   for( VT_MPI_INT rank = 0; rank < NumRanks; rank++ )
   {
      // initialize sendcounts
      sendcounts[rank] = 0;

      // compute senddispls
      //
      if( rank == 0 )
         senddispls[rank] = 0;
      else
         senddispls[rank] = senddispls[rank-1] + sendcounts[rank-1];

      // search for receive messages of certain rank
      std::map<VT_MPI_INT, LargeVectorC<RecvMsgS*> >::iterator it =
         rank2recv_msgs.find( rank );

      // found?
      if( it != rank2recv_msgs.end() )
      {
         // update sendcounts
         sendcounts[rank] = it->second.size();

         // add receive messages to send buffer
         //
         for( uint32_t i = 0; i < it->second.size(); i++ )
         {
            (sendbuf+senddispls[rank])[i] = *(it->second[i]);
            delete it->second[i];
         }

         // clear receive message vector
         it->second.clear();
      }
   }
   rank2recv_msgs.clear();
   recvMsgs.clear();

   // get receive counts
   //

   VT_MPI_INT * recvcounts = new VT_MPI_INT[NumRanks];
   assert( recvcounts );

   CALL_MPI( MPI_Alltoall( sendcounts, 1, MPI_INT, recvcounts, 1, MPI_INT,
                           MPI_COMM_WORLD ) );

   // get receive displs
   //

   VT_MPI_INT * recvdispls = new VT_MPI_INT[NumRanks];
   assert( recvdispls );

   recvdispls[0] = 0;
   for( VT_MPI_INT rank = 1; rank < NumRanks; rank++ )
      recvdispls[rank] = recvdispls[rank-1] + recvcounts[rank-1];

   // allocate receive buffer
   //
   RecvMsgS * recvbuf =
      new RecvMsgS[recvdispls[NumRanks-1] + recvcounts[NumRanks-1]];
   assert( recvbuf );

   // distribute receive messages
   CALL_MPI( MPI_Alltoallv( sendbuf, sendcounts, senddispls,
                            recv_msg_type, recvbuf, recvcounts,
                            recvdispls, recv_msg_type, MPI_COMM_WORLD ) );

   // free send buffer, -counts, and -displs
   //
   delete [] sendbuf;
   delete [] sendcounts;
   delete [] senddispls;

   // extract receive messages from receive buffer and add these to vector
   //
   for( VT_MPI_INT rank = 0; rank < NumRanks; rank++ )
   {
      for( VT_MPI_INT i = 0; i < recvcounts[rank]; i++ )
      {
         recvMsgs.push_back( new RecvMsgS( (recvbuf + recvdispls[rank])[i] ) );
         assert( recvMsgs.back() );
      }
   }

   // free receive buffer, -counts, and -displs
   //
   delete [] recvbuf;
   delete [] recvcounts;
   delete [] recvdispls;

   // free MPI datatype
   CALL_MPI( MPI_Type_free( &recv_msg_type ) );

   return !error;
}

#endif // VT_MPI

bool
HooksMsgMatchC::enqueueRecvMsgs( LargeVectorC<RecvMsgS*> & recvMsgs )
{
   bool error = false;

   VPrint( 2, "  Enqueuing receive messages\n" );

   // return, if there are no receive messages to match
   if( recvMsgs.empty() )
      return true;

   // create message matching context for each thread
   //
   m_matchContexts = new MatchContextC[m_maxThreads];
   assert( m_matchContexts );

#if defined(HAVE_OMP) && HAVE_OMP
   if( m_maxThreads > 1 )
   {
      // release unused memory every N enqueued receive message
      const uint32_t release_mem_interval = 100000;

#     pragma omp parallel
      {
         const int threadid = omp_get_thread_num();
         const int streams_num = (int)MyStreamIds.size();
         int i;

         // get streams of interest for my thread
         //
         std::set<uint32_t> streams_of_interest;
#        pragma omp for schedule(static) nowait
         for( i = 0; i < streams_num; i++ )
            streams_of_interest.insert( MyStreamIds[i] );

         // iterate over all receive messages to enqueue
         for( uint32_t j = 0; j < recvMsgs.size(); j++ )
         {
            const RecvMsgS * recv_msg = recvMsgs[j];

            // enqueue receive message, if it's of interest for my thread
            //
            if( streams_of_interest.find( recv_msg->sender ) !=
                streams_of_interest.end() )
            {
               m_matchContexts[threadid].enqueueRecv( recv_msg->time,
                  recv_msg->sender, recv_msg->receiver,
                  recv_msg->comm, recv_msg->tag,
                  0/*recv_msg->length*/, 0/*recv_msg->scl*/ );
            }

            // release memory of already enqueued receive messages
            //
            if( j > 0 && j % release_mem_interval == 0 )
            {
#              pragma omp barrier
#              pragma omp single
               {
                  for( uint32_t k = j - release_mem_interval; k < j; k++ )
                     delete recvMsgs[k];
               }
            }
         }
      }

      // release remaining unused memory
      //
      for( uint32_t j = ( recvMsgs.size() / release_mem_interval )
               * release_mem_interval; j < recvMsgs.size(); j++ )
         delete recvMsgs[j];
      recvMsgs.clear();
   }
   else
#endif // HAVE_OMP
   {
      // enqueue all receive messages to one matching context, if we have
      // no threads
      //
      for( uint32_t i = 0; i < recvMsgs.size(); i++ )
      {
         m_matchContexts->enqueueRecv( recvMsgs[i]->time, recvMsgs[i]->sender,
            recvMsgs[i]->receiver, recvMsgs[i]->comm, recvMsgs[i]->tag,
            0/*recvMsgs[i]->length*/, 0/*recvMsgs[i]->scl*/ );
         delete recvMsgs[i];
      }
      recvMsgs.clear();
   }

   return !error;
}

/////////////////// sub-class HooksMsgMatchC::MatchContextC ///////////////////

// public methods
//

HooksMsgMatchC::MatchContextC::MatchContextC()
   : m_matchCount( 0 )
{
   // create OTFAUX message matching context
   //
   m_context = OTFAUX_MsgMatching_create();
   assert( m_context );
}

HooksMsgMatchC::MatchContextC::~MatchContextC()
{
   assert( m_context );

   // destroy OTFAUX message matching context
   OTFAUX_MsgMatching_destroy( m_context );
}

void
HooksMsgMatchC::MatchContextC::enqueueRecv( const uint64_t & time,
   const uint32_t & sender, const uint32_t & receiver, const uint32_t & comm,
   const uint32_t & tag, const uint32_t & length, const uint32_t & scl )
{
   assert( m_context );

   // enqueue receive message
   OTFAUX_MsgMatching_enqueueRecv( m_context, sender, receiver, tag, comm,
      time, length, scl );
}

bool
HooksMsgMatchC::MatchContextC::matchSend( const uint32_t & sender,
   const uint32_t & receiver, const uint32_t & comm, const uint32_t & tag,
   uint64_t & recv_time, uint32_t & length, uint32_t & scl )
{
   bool match = false;

   assert( m_context );

   // get timestamp of matching receive message
   if( OTFAUX_MsgMatching_matchSend( m_context, sender, receiver, tag, comm,
          &recv_time, &length, &scl ) )
   {
      // increment number of matched messages
      m_matchCount++;

      // release unsed memory, if necessary
      if( m_matchCount % RELEASE_MEM_INTERVAL == 0 )
         OTFAUX_MsgMatching_releaseMemory( m_context );

      match = true;
   }

   return match;
}
