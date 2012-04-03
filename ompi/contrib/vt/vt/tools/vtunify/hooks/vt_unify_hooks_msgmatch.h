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

#ifndef _VT_UNIFY_HOOKS_MSGMATCH_H_
#define _VT_UNIFY_HOOKS_MSGMATCH_H_

#include "vt_unify.h"
#include "vt_unify_hooks_base.h"
#include "vt_unify_lvector.hh"

#include "otfaux.h"

//
// HooksMsgMatchC class
//
class HooksMsgMatchC : public HooksBaseC
{
public:

   // constructor
   HooksMsgMatchC();

   // destructor
   ~HooksMsgMatchC();

   // is this hook enabled?
   static bool isEnabled() { return Params.domsgmatch; }

private:

   //
   // MatchContextC sub-class
   //
   class MatchContextC
   {
   public:

      // constructor
      MatchContextC();

      // destructor
      ~MatchContextC();

      // provide a receive message for matching
      inline void enqueueRecv( const uint64_t & time, const uint32_t & sender,
         const uint32_t & receiver, const uint32_t & comm,
         const uint32_t & tag, const uint32_t & length, const uint32_t & scl );

      // try to match a send with the corresponding receive message
      inline bool matchSend( const uint32_t & sender, const uint32_t & receiver,
         const uint32_t & comm, const uint32_t & tag, uint64_t & recv_time,
         uint32_t & length, uint32_t & scl );

   private:

      // release unused memory every N matched messages
      static const uint64_t RELEASE_MEM_INTERVAL = 1000000;

      // OTFAUX message matching context
      OTFAUX_MsgMatching_Context * m_context;

      // number of matched messages
      uint64_t m_matchCount;

   };

   //
   // receive message structure
   //
   struct RecvMsgS
   {
      // constructors
      //
      RecvMsgS()
         : time( 0 ), sender( 0 ), receiver( 0 ), comm( 0 ), tag( 0 )
           /*, length( 0 ), scl( 0 )*/ {}
      RecvMsgS( const uint64_t & _time, const uint32_t & _sender,
                const uint32_t & _receiver, const uint32_t & _comm,
                const uint32_t & _tag
                /*, const uint32_t & _length, const uint32_t & _scl*/ )
         : time( _time ), sender( _sender ), receiver( _receiver ),
           comm( _comm ), tag( _tag ) /*, length( _length ), scl( _scl )*/ {}

      uint64_t time;     // timestamp on which the recv. message event occurred
      uint32_t sender;   // sender process id
      uint32_t receiver; // receiver process id
      uint32_t comm;     // comm. process group token
      uint32_t tag;      // message tag
      /*uint32_t length;*/   // received bytes
      /*uint32_t scl;*/      // source code location of recv. message event

   };

   // event record handlers
   //

   static int HandleEventComment( void * userData,
                 uint64_t time, uint32_t proc, const char * comment );

   static int HandleRecvMsg( LargeVectorC<RecvMsgS*> * recvMsgs,
                 uint64_t time, uint32_t receiver, uint32_t sender,
                 uint32_t comm, uint32_t tag, uint32_t length, uint32_t scl );

   // vvvvvvvvvvvvvvvvvvvv HOOK METHODS vvvvvvvvvvvvvvvvvvvv

   // initialization/finalization hooks
   //

   void initHook();
   void finalizeHook( const bool & error );

   // phase hooks
   //

   void phaseHook_UnifyDefinitions_pre();
   void phaseHook_UnifyEvents_pre();
   void phaseHook_UnifyEvents_post();

   // record hooks
   //

   void writeRecHook_SendMsg( HooksC::VaArgsT & args );
   void writeRecHook_RecvMsg( HooksC::VaArgsT & args );

   // ^^^^^^^^^^^^^^^^^^^^ HOOK METHODS ^^^^^^^^^^^^^^^^^^^^

   // read receive messages
   bool getRecvMsgs( LargeVectorC<RecvMsgS*> & recvMsgs );

#ifdef VT_MPI

   // distribute receive messages to ranks which processes the corresponding
   // sender streams
   bool distRecvMsgs( LargeVectorC<RecvMsgS*> & recvMsgs );

#endif // VT_MPI

   // enqueue receive messages to message matching context(s)
   bool enqueueRecvMsgs( LargeVectorC<RecvMsgS*> & recvMsgs );

   // global key tokens for matching receive timestamp, length, and
   // source code location
   //
   enum { KEY_TIME /*, KEY_LENGTH, KEY_SCL*/ , KEY_NUM };
   uint32_t m_keyTokens[KEY_NUM];

   // maximum number of threads to use for unifying events
   int m_maxThreads;

   // array of message matching contexts (one for each thread)
   MatchContextC * m_matchContexts;

};

#endif // _VT_UNIFY_HOOKS_MSGMATCH_H_
