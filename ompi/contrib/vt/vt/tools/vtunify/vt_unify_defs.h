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

#ifndef _VT_UNIFY_DEFS_H_
#define _VT_UNIFY_DEFS_H_

#include "vt_unify_defs_recs.h"
#include "vt_unify_usrcom.h"

#include "vt_inttypes.h"

#include <algorithm>
#include <list>
#include <map>
#include <set>
#include <string>

//
// DefinitionsC class
//
class DefinitionsC
{
public:

   // constructor
   DefinitionsC();

   // destructor
   ~DefinitionsC();

   // unify definitions
   bool run();

   // rename temporary output files
   bool cleanUp();

private:

   //
   // CommentsC sub-class
   // (pre-processes comments before adding these to global definitions)
   //
   class CommentsC
   {
      friend class DefinitionsC;

   //private:

      // constructor
      CommentsC( DefinitionsC & _defs );

      // destructor
      ~CommentsC();

      // process local definition comment
      bool processLocal( const DefRec_DefCommentS & locComment );

      // finish global definition comments
      // (i.e. add trace time comments to global definitions)
      bool finish();

      //
      // trace time comments
      //
      struct TraceTimeS
      {
         TraceTimeS()
            : minStartTimeEpoch( (uint64_t)-1 ), maxStopTimeEpoch( 0 ) {}

         uint64_t minStartTimeEpoch; // minimum start time since epoch
         uint64_t maxStopTimeEpoch; // maximum stop time since epoch

      } m_traceTimes;

      //
      // user communication ids and peers
      //
      struct UserComS
      {
         //
         // structure for user communication ids and peers
         //
         struct ComIdPeerS
         {
            ComIdPeerS( const UserComC::ComIdS & _comid, const uint32_t _peer,
                        const bool & _is_sender )
               : comid( _comid ), peer( _peer ), is_sender( _is_sender ) {}

            UserComC::ComIdS comid;     // user communication id (comm./tag)
            uint32_t         peer;      // peer process id
            bool             is_sender; // peer type indicator

         };

         // list of user communication ids and peers
         std::list<ComIdPeerS> comIdsAndPeers;

      } m_userCom;

      // reference to parent class instance
      DefinitionsC & m_defs;

      // sequential order index
      uint32_t m_seqOrderIdx;

   };

   //
   // ProcessGroupsC sub-class
   // (pre-processes process groups before adding these to global definitions)
   //
   class ProcessGroupsC
   {
      friend class DefinitionsC;
      friend class DefinitionsC::CommentsC;

   //private:

      // identifier for deflated vector of group members
      // (will be putted at the first vector element)
      static const uint32_t DEFLATED_MEMBERS_TAG = (uint32_t)-1;

      // constructor
      ProcessGroupsC( DefinitionsC & _defs );

      // destructor
      ~ProcessGroupsC();

      // process local process group definition
      bool processLocal( DefRec_DefProcessGroupS & locProcGrp );

      // finish global process group definitions
      // (i.e. add process groups for nodes and MPI-comms. to global defs.)
      bool finish();

      // deflate vector of group members
      // (replaces vector elements by an unique id)
      inline void deflateMembers( std::vector<uint32_t> & members );

      // inflate vector of group members
      // (replaces unique id by the actual vector elements)
      inline void inflateMembers( std::vector<uint32_t> & members );

      //
      // compare structure for sorting process ids
      //
      struct ProcCmpS
      {
         bool operator()( const uint32_t & a, const uint32_t & b ) const
         {
            if( ( a & VT_TRACEID_BITMASK ) == ( b & VT_TRACEID_BITMASK ) )
               return a < b;
            else
               return ( a & VT_TRACEID_BITMASK ) < ( b & VT_TRACEID_BITMASK );
         }

      };

      //
      // Node process groups
      //
      struct NodeS
      {
         // map node name <-> process ids
         std::map<std::string, std::set<uint32_t, ProcCmpS> > name2Procs;

      } m_node;

      //
      // GPU process groups and communicators
      //
      struct GpuS
      {
         // names of final process groups
         //
         static const char * groupName() { return "GPU_GROUP";       }
         static const char * commName()  { return "GPU_COMM_GLOBAL"; }

         // set of GPU process ids
         std::set<uint32_t> procs;

         // set of related process and GPU ids
         std::set<uint32_t, ProcCmpS> commMembers;

         // map process id <-> local GPU comm. token
         // (storage of local tokens to translate afterwards)
         std::map<uint32_t, uint32_t> proc2LocCommTk;

      } m_gpu;

      //
      // OpenMP thread team communicators
      //
      struct OmpS
      {
         // name (prefix) of final process groups
         static const char * commName() { return "OMP Thread Team"; }

      } m_omp;

      //
      // MPI communicators
      //
      struct MpiS
      {
         // names of final process groups
         //
         static const char * worldCommName() { return "MPI_COMM_WORLD";   }
         static const char * selfCommName()  { return "MPI_COMM_SELF";    }
         static const char * otherCommName() { return "MPI Communicator"; }

         //
         // structure for MPI_COMM_SELFs
         //
         struct SelfCommS
         {
            SelfCommS( const uint32_t & _loctk, const uint32_t & _member )
               : loctk( _loctk ), member( _member ) {}

            bool operator<( const SelfCommS & a ) const
            {
               return member < a.member;
            }

            uint32_t loctk;  // local comm. token
            uint32_t member; // comm. member process id

         };

         //
         // structure for user created (other) MPI communicators
         //
         struct OtherCommS
         {
            // constructor for searching a communicator by its members
            OtherCommS( const uint32_t & _membersid )
               : membersid( _membersid ) {}

            // constructor for creating a new communicator entry
            OtherCommS( const uint32_t & _deftoken, const uint32_t & _membersid,
                        const uint32_t & _index )
               : deftoken( _deftoken ), membersid( _membersid ),
                 index( _index ) {}

            // operator for searching
            bool operator==( const OtherCommS & a ) const
            {
               return membersid == a.membersid;
            }

            uint32_t deftoken;  // local token on def. process
            uint32_t membersid; // id of deflated members vector
            uint32_t index;     // local index

         };

         // set of MPI_COMM_SELFs
         std::set<SelfCommS> selfComms;

         // map process <-> list of user created MPI comms.
         std::map<uint32_t, std::list<OtherCommS> > proc2OtherComms;

      } m_mpi;

      //
      // communicators for user communication
      //
      struct UserComS
      {
         //
         // structure for user communicators
         //
         struct CommS
         {
            // constructor for searching a communicator by its name
            CommS( const std::string & _name )
               : name( _name ) {}

            // operator for searching
            bool operator==( const CommS & a ) const
            {
               return name.compare( a.name ) == 0;
            }

            std::string                  name;    // comm. name
            std::set<uint32_t, ProcCmpS> members; // comm. member process ids

            // map process id <-> local comm. token
            // (storage of local tokens to translate afterwards)
            std::map<uint32_t, uint32_t> proc2LocCommTk;

         };

         // add member process id to certain user communicator
         void addCommMember( const uint32_t & proc, const uint32_t & comm,
                 const uint32_t & member )
         {
            std::map<uint32_t,
               std::map<uint32_t, std::list<CommS>::iterator> >::iterator
                  proc_it = proc2LocCommTk2CommIt.find( proc );
            assert( proc_it != proc2LocCommTk2CommIt.end() );

            std::map<uint32_t, std::list<CommS>::iterator>::iterator comm_it =
               proc_it->second.find( comm );
            assert( comm_it != proc_it->second.end() );

            comm_it->second->members.insert( member );
         }

         // list of user communicators
         std::list<CommS> comms;

         // map process <-> local comm. token <-> iterator in comm. list
         std::map<uint32_t, std::map<uint32_t, std::list<CommS>::iterator> >
            proc2LocCommTk2CommIt;

      } m_userCom;

      // reference to parent class instance
      DefinitionsC & m_defs;

      // map id <-> vector of group members
      std::map<uint32_t, std::vector<uint32_t> > m_id2Members;

   };

   // get stream ids to read
   void getStreamIds( std::vector<uint32_t> & streamIds );

   // read local definitions of certain streams
   bool readLocal( const std::vector<uint32_t> & streamIds );

   // read local definitions of certain single stream
   bool readLocal( const uint32_t & streamId,
                   std::vector<DefRec_BaseS*> & locDefs );

   // process local definitions
   // (i.e. create global tokens)
   bool processLocal( const std::vector<DefRec_BaseS*> & locDefs );

   // write global definitions
   bool writeGlobal();

   // instance of class CommentsC
   CommentsC * m_comments;

   // instance of class ProcessGroupsC
   ProcessGroupsC * m_procGrps;

   //
   // container of global definitions
   //
   struct
   {
      DefRec_DefCreatorS                 creator;
      DefRec_DefTimerResolutionS         timeres;
      DefRec_DefTimeRangeS               timerange;
      std::set<DefRec_DefCommentS>       comments;
      std::set<DefRec_DefProcessS>       procs;
      std::set<DefRec_DefProcessGroupS>  procGrps;
      std::set<DefRec_DefSclFileS>       sclFiles;
      std::set<DefRec_DefSclS>           scls;
      std::set<DefRec_DefFileGroupS>     fileGrps;
      std::set<DefRec_DefFileS>          files;
      std::set<DefRec_DefFunctionGroupS> funcGrps;
      std::set<DefRec_DefFunctionS>      funcs;
      std::set<DefRec_DefCollOpS>        collops;
      std::set<DefRec_DefCounterGroupS>  cntrGrps;
      std::set<DefRec_DefCounterS>       cntrs;
      std::set<DefRec_DefKeyValueS>      keyVals;

   } m_globDefs;

};

// instance of class DefinitionsC
extern DefinitionsC * theDefinitions;

#endif // _VT_UNIFY_DEFS_H_
