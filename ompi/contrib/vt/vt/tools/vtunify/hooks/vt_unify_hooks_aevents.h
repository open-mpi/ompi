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

#ifndef _VT_UNIFY_HOOKS_AEVENTS_H_
#define _VT_UNIFY_HOOKS_AEVENTS_H_

#include "vt_unify.h"
#include "vt_unify_hooks_base.h"

#include "otf.h"

#include <deque>
#include <map>
#include <set>

//
// HooksAsyncEventsC class
//
class HooksAsyncEventsC : public HooksBaseC
{
public:

   // constructor
   HooksAsyncEventsC();

   // destructor
   ~HooksAsyncEventsC();

   // is this hook enabled?
   static bool isEnabled() { return true; }

private:

   //
   // async. event types
   //
   typedef enum
   {
      ASYNC_EVENT_TYPE_COUNTER,
      // TODO: further async. event types
      ASYNC_EVENT_TYPE_UNKNOWN

   } AsyncEventTypeT;

   //
   // async. event base structure
   //
   struct AsyncEventBaseS
   {
      // constructors
      //
      AsyncEventBaseS()
         : type(ASYNC_EVENT_TYPE_UNKNOWN), time(0), kvs(0) {}
      AsyncEventBaseS( const AsyncEventTypeT & _type, const uint64_t & _time,
                       OTF_KeyValueList *& _kvs )
         : type(_type), time(_time), kvs(_kvs) {}

      // destructor
      ~AsyncEventBaseS() { OTF_KeyValueList_close( kvs ); }

      AsyncEventTypeT type;   // async. event type
      uint64_t time;          // actual timestamp
      OTF_KeyValueList * kvs; // key-value list

   };

   //
   // async. counter event structure
   //
   struct AsyncEventCounterS : AsyncEventBaseS
   {
      // constructor
      AsyncEventCounterS( const uint64_t & _time, OTF_KeyValueList *& _kvs,
                          const uint32_t & _counter, const uint64_t & _value )
         : AsyncEventBaseS(ASYNC_EVENT_TYPE_COUNTER, _time, _kvs),
           counter(_counter), value(_value) {}

      uint32_t counter; // global counter token
      uint64_t value;   // counter value

   };

   // TODO: define further async. event structures here

   //
   // async. source manager structure
   //
   struct AsyncSourceManagerS
   {
      //
      // async. source structure
      //
      struct SourceS
      {
         // constructors
         //
         SourceS()
            : key(0), finished_reading(false), file_manager(0), rstream(0),
              handler_array(0) {}
         SourceS( const uint32_t & _key )
            : key(_key), finished_reading(false), file_manager(0), rstream(0),
              handler_array(0) {}

         // maximum number of queued async. events
         static uint32_t MaxQueueSize;

         uint32_t key;                             // async. source key
         bool finished_reading;                    // flag: reading finished?
         OTF_FileManager * file_manager;           // OTF file manager
         OTF_RStream * rstream;                    // OTF reader stream
         OTF_HandlerArray * handler_array;         // OTF handler array
         std::deque<AsyncEventBaseS*> event_queue; // queue of async. events

      };

      // constructor
      AsyncSourceManagerS()
         : stream_id(0), opened(false), hooks_suspended(false), wstream(0) {}

      // maximum number of queued events over all async. sources
      static const uint32_t MAX_QUEUED_EVENTS = 1000000;

      uint32_t stream_id;                  // input stream id
      std::string stream_prefix;           // input stream file prefix
      bool opened;                         // flag: async. sources opened?
      bool hooks_suspended;                // flag: write rec. hooks suspended?
      OTF_WStream * wstream;               // output OTF writer stream
      std::map<uint32_t, SourceS> sources; // map key <-> async. source

   };

   // record handlers for async. events
   //

   // common leading stuff:
   // get actual time of async. event from key-value list
   // return false, if event isn't of interest for given async. source
   static inline bool HandleAsyncEventPre(
                         AsyncSourceManagerS::SourceS & source,
                         const uint32_t & proc, uint64_t & time,
                         OTF_KeyValueList *& kvs );
   // common secondary stuff: enqueue new async. event
   // return false, if failed (time not increasing)
   static inline bool HandleAsyncEventPost(
                         AsyncSourceManagerS::SourceS & source,
                         AsyncEventBaseS *& newAsyncEvent );

   static int HandleAsyncCounter( AsyncSourceManagerS::SourceS * source,
                 uint64_t time, uint32_t proc, uint32_t counter, uint64_t value,
                 OTF_KeyValueList * kvs );

   // TODO: declare further record handlers for async. events here

   // vvvvvvvvvvvvvvvvvvvv HOOK METHODS vvvvvvvvvvvvvvvvvvvv

   // initialization/finalization hooks
   //

   void initHook();
   void finalizeHook( const bool & error );

   // phase hooks
   //

   void phaseHook_UnifyEvents_pre();

   // record hooks
   //

   // definition records

   void writeRecHook_DefKeyValue( HooksC::VaArgsT & args );

   // event records

   // common stuff for write event record hooks
   void writeRecHook_Event( HooksC::VaArgsT & args,
           const uint32_t & timeArgIdx, const uint32_t & streamIdArgIdx,
           const uint32_t & kvsArgIdx, const uint32_t & doWriteArgIdx );

   void writeRecHook_EventComment( HooksC::VaArgsT & args )
   { writeRecHook_Event( args, 1, 2, 4, 5 ); }
   void writeRecHook_Enter( HooksC::VaArgsT & args )
   { writeRecHook_Event( args, 1, 3, 5, 6 ); }
   void writeRecHook_Leave( HooksC::VaArgsT & args )
   { writeRecHook_Event( args, 1, 3, 5, 6 ); }
   void writeRecHook_Counter( HooksC::VaArgsT & args )
   { writeRecHook_Event( args, 1, 2, 5, 6 ); }
   void writeRecHook_BeginFileOp( HooksC::VaArgsT & args )
   { writeRecHook_Event( args, 1, 2, 5, 6 ); }
   void writeRecHook_EndFileOp( HooksC::VaArgsT & args )
   { writeRecHook_Event( args, 1, 2, 9, 10 ); }
   void writeRecHook_SendMsg( HooksC::VaArgsT & args )
   { writeRecHook_Event( args, 1, 2, 8, 9 ); }
   void writeRecHook_RecvMsg( HooksC::VaArgsT & args )
   { writeRecHook_Event( args, 1, 2, 8, 9 ); }
   void writeRecHook_BeginCollOp( HooksC::VaArgsT & args )
   { writeRecHook_Event( args, 1, 2, 10, 11 ); }
   void writeRecHook_EndCollOp( HooksC::VaArgsT & args )
   { writeRecHook_Event( args, 1, 2, 4, 5 ); }
   void writeRecHook_RMAPut( HooksC::VaArgsT & args )
   { writeRecHook_Event( args, 1, 2, 9, 10 ); }
   void writeRecHook_RMAPutRemoteEnd( HooksC::VaArgsT & args )
   { writeRecHook_Event( args, 1, 2, 9, 10 ); }
   void writeRecHook_RMAGet( HooksC::VaArgsT & args )
   { writeRecHook_Event( args, 1, 2, 9, 10 ); }
   void writeRecHook_RMAEnd( HooksC::VaArgsT & args )
   { writeRecHook_Event( args, 1, 2, 7, 8 ); }

   // generic hook
   void genericHook( const uint32_t & id, HooksC::VaArgsT & args );

   // ^^^^^^^^^^^^^^^^^^^^ HOOK METHODS ^^^^^^^^^^^^^^^^^^^^

   // open reader streams of async. sources
   bool openSources( AsyncSourceManagerS & manager,
           const uint32_t & streamId, const std::string & streamPrefix,
           OTF_WStream *& wstream );

   // write remaining queued async. events and close reader streams
   bool closeSources( AsyncSourceManagerS & manager );

   // read events ahead for certain async. source key
   // if sourceKey is 0, read events ahead for all async. sources
   bool readAhead( AsyncSourceManagerS & manager,
           const uint32_t & sourceKey = 0 );

   // write queued async. events until certain timestamp is reached
   // if curTime is (uint64_t)-1, write all queued async. events
   bool writeAsyncEvents( AsyncSourceManagerS & manager,
           const uint64_t & curTime = (uint64_t)-1 );

   // get pointer to async. source manager by stream id
   inline AsyncSourceManagerS *
      getSourceManagerByStreamId( const uint32_t & streamId );

   // check whether an event is asynchronous by searching certain async. source
   // key in key-value list
   // if sourceKey is 0, search for all async. source keys
   inline bool isAsyncEvent( OTF_KeyValueList *& kvs,
                  const uint32_t & sourceKey = 0 ) const;

#ifdef VT_MPI

   // share async. source keys to all ranks
   bool shareSourceKeys();

#endif // VT_MPI

   // class's instance object to allow access from static member methods
   static HooksAsyncEventsC * Obj;

   // map stream id <-> async. source manager
   std::map<uint32_t, AsyncSourceManagerS> m_stream2SourceManager;

   // set of async. source keys
   std::set<uint32_t> m_sourceKeys;

};

#endif // _VT_UNIFY_HOOKS_AEVENTS_H_
