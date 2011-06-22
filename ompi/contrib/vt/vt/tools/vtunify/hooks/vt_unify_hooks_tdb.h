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

#ifndef _VT_UNIFY_HOOKS_TDB_H_
#define _VT_UNIFY_HOOKS_TDB_H_

#include "vt_unify_hooks_base.h"

#include "vt_inttypes.h"

#include <map>
#include <vector>

//
// HooksTdbC class
//
class HooksTdbC : public HooksBaseC
{
public:

   // constructor
   HooksTdbC();

   // destructor
   ~HooksTdbC();

   // is this hook enabled?
   static bool isEnabled();

private:

   //
   // CollOpC sub-class
   //
   class CollOpC
   {
   public:

      CollOpC();
      CollOpC( uint64_t _num, uint64_t _bytes_sent, uint64_t _bytes_recv );

      CollOpC operator+=( CollOpC cs );

      uint64_t num;
      uint64_t bytes_sent;
      uint64_t bytes_recv;

   };

   //
   // IoC sub-class
   //
   class IoC
   {
   public:

      IoC();
//      IoC( uint64_t _num_read, uint64_t _bytes_read, uint64_t _num_written,
//           uint64_t _bytes_written, uint64_t _num_open, uint64_t _num_close,
//           uint64_t _num_seek );

      IoC operator+=( IoC is );

      uint64_t num_read;
      uint64_t bytes_read;
      uint64_t num_written;
      uint64_t bytes_written;
      uint64_t num_open;
      uint64_t num_close;
      uint64_t num_seek;

   };

   //
   // MasterDataC sub-class
   // (is only used on rank 0 for definitions)
   //
   class MasterDataC
   {
   public:

      MasterDataC();

      ~MasterDataC();

      uint64_t calcFilesize();

      int setFilepath( std::string file_prefix );

      int setHostname();

      void setOtfVersion();

      std::string otf_version;
      std::string creator;
      std::string filename;
      std::string filepath;
      std::string hostname;

      uint64_t starttime;
      uint64_t runtime;
      uint64_t filesize;
      uint64_t num_streams;

      uint32_t num_processes;
      uint32_t num_active_processes;
      uint32_t num_threads;

      bool is_compressed;

      bool create_output;


      std::map<std::string, std::string> vt_env;
      std::vector<std::string> counter;

      // stores all function group definitions
      // fgroup_id, fgroup_name
      std::map<uint32_t, std::string> fgroup;

      // stores the fgroup_id for all "flush"-functions,
      // necessary to check later if it was a vt_flush
      // function_id, fgroup_id
      std::map<uint32_t, uint32_t> flush_fgroup_ids;

      // save function_id fpr vt_flush here
      uint32_t vt_flush_id;

      // collop, type
      std::map<uint32_t, uint32_t> collop_def;

      // type, values
      std::map<uint32_t, CollOpC> collop;

   private:

      std::string intToHex( int i );
      std::string safeCwd();

   };

   //
   // ThreadDataC
   //
   class ThreadDataC
   {
   public:

      ThreadDataC();

      ThreadDataC operator+=( ThreadDataC td );

      bool toBuffer( uint64_t * buf );

      ThreadDataC fromBuffer( const uint64_t * buf,
                              const uint64_t * num_bytes );

      uint64_t num_events;

      uint64_t num_enter;
      uint64_t num_leave;
      uint64_t num_sent;
      uint64_t num_recv;
      uint64_t bytes_sent;
      uint64_t bytes_recv;

      uint64_t num_rma;
      uint64_t bytes_rma;

      uint64_t num_marker;
      uint64_t num_stats;
      uint64_t num_snaps;

      uint64_t num_vt_flushes;

      // collop, values
      std::map<uint32_t, CollOpC> collop;

      // ioflag, values
      std::map<uint32_t, IoC> io;

   };

   // vvvvvvvvvvvvvvvvvvvv HOOK METHODS vvvvvvvvvvvvvvvvvvvv

   // initialization/finalization hooks
   //

   void initHook();
   void finalizeHook( const bool & error );

   // phase hooks
   //

   void phaseHook_GetUnifyControls_post();
   void phaseHook_UnifyDefinitions_post();
   void phaseHook_UnifyEvents_pre();

   // record hooks
   //

   void writeRecHook_DefComment( HooksC::VaArgsT & args );
   void writeRecHook_DefCreator( HooksC::VaArgsT & args );
   void writeRecHook_DefProcess( HooksC::VaArgsT & args );
   void writeRecHook_DefFunctionGroup( HooksC::VaArgsT & args );
   void writeRecHook_DefFunction( HooksC::VaArgsT & args );
   void writeRecHook_DefCollOp( HooksC::VaArgsT & args );
   void writeRecHook_DefCounter( HooksC::VaArgsT & args );
   void writeRecHook_FunctionSummary( HooksC::VaArgsT & args );
   void writeRecHook_MessageSummary( HooksC::VaArgsT & args );
   void writeRecHook_CollOpSummary( HooksC::VaArgsT & args );
   void writeRecHook_FileOpSummary( HooksC::VaArgsT & args );
   void writeRecHook_MarkerSpot( HooksC::VaArgsT & args );
   void writeRecHook_Enter( HooksC::VaArgsT & args );
   void writeRecHook_Leave( HooksC::VaArgsT & args );
   void writeRecHook_BeginFileOp( HooksC::VaArgsT & args );
   void writeRecHook_EndFileOp( HooksC::VaArgsT & args );
   void writeRecHook_SendMsg( HooksC::VaArgsT & args );
   void writeRecHook_RecvMsg( HooksC::VaArgsT & args );
   void writeRecHook_BeginCollOp( HooksC::VaArgsT & args );
   void writeRecHook_EndCollOp( HooksC::VaArgsT & args );
   void writeRecHook_RMAPut( HooksC::VaArgsT & args );
   void writeRecHook_RMAPutRemoteEnd( HooksC::VaArgsT & args );
   void writeRecHook_RMAGet( HooksC::VaArgsT & args );
   void writeRecHook_RMAEnd( HooksC::VaArgsT & args );
   void writeRecHook_Counter( HooksC::VaArgsT & args );
   void writeRecHook_EventComment( HooksC::VaArgsT & args );

   // generic hook
   void genericHook( const uint32_t & id, HooksC::VaArgsT & args );

   // ^^^^^^^^^^^^^^^^^^^^ HOOK METHODS ^^^^^^^^^^^^^^^^^^^^

   void handleFileOperation( uint32_t *operation, uint64_t *bytes);

   MasterDataC RootData;
   std::vector<ThreadDataC> ThreadVector;

   uint64_t MinStartTimeEpoch;
   uint64_t MaxStopTimeEpoch;

};

#endif // _VT_UNIFY_HOOKS_TDB_H_
