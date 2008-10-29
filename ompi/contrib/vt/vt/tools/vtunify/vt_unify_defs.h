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

#ifndef _VT_UNIFY_DEFS_H_
#define _VT_UNIFY_DEFS_H_

#include "vt_inttypes.h"

#include <list>
#include <map>
#include <string>
#include <vector>

#include <assert.h>

//
// Definitions class
//
class Definitions
{
public:

   // definition record types
   //
   typedef enum { DEF_REC_TYPE__DefCreator,
		  DEF_REC_TYPE__DefinitionComment,
		  DEF_REC_TYPE__DefTimerResolution,
		  DEF_REC_TYPE__DefProcess,
		  DEF_REC_TYPE__DefProcessGroup,
		  DEF_REC_TYPE__DefSclFile,
		  DEF_REC_TYPE__DefScl,
		  DEF_REC_TYPE__DefFileGroup,
		  DEF_REC_TYPE__DefFile,
		  DEF_REC_TYPE__DefFunctionGroup,
		  DEF_REC_TYPE__DefFunction,
		  DEF_REC_TYPE__DefCollectiveOperation,
		  DEF_REC_TYPE__DefCounterGroup,
		  DEF_REC_TYPE__DefCounter,
                  DEF_REC_TYPE__Unknown } DefRecTypeT;
   
   //
   // DefRec_Base_struct
   //
   struct DefRec_Base_struct
   {
      DefRec_Base_struct()
	 : etype(DEF_REC_TYPE__Unknown), loccpuid(0), deftoken(0) {}
      DefRec_Base_struct(DefRecTypeT _etype)
	 : etype(_etype), loccpuid(0), deftoken(0) {}
      virtual ~DefRec_Base_struct() {}
      
      DefRecTypeT etype;
      uint32_t    loccpuid;
      uint32_t    deftoken;
   };

   //
   // DefRec_DefinitionComment_struct
   //
   struct DefRec_DefinitionComment_struct : DefRec_Base_struct
   {
      DefRec_DefinitionComment_struct() 
	 : DefRec_Base_struct(DEF_REC_TYPE__DefinitionComment),
      orderidx(0) {}
      DefRec_DefinitionComment_struct(uint32_t _orderidx,
				      std::string _comment)
	 : DefRec_Base_struct(DEF_REC_TYPE__DefinitionComment),
      orderidx(_orderidx), comment(_comment) {}
      
      uint32_t    orderidx;
      std::string comment;
   };

   //
   // DefRec_DefCreator_struct
   //
   struct DefRec_DefCreator_struct : DefRec_Base_struct
   {
      DefRec_DefCreator_struct() 
	 : DefRec_Base_struct(DEF_REC_TYPE__DefCreator) {}
      DefRec_DefCreator_struct(std::string _creator)
	 : DefRec_Base_struct(DEF_REC_TYPE__DefCreator),
      creator(_creator) {}
      
      std::string creator;
   };

   //
   // DefRec_DefTimerResolution_struct
   //
   struct DefRec_DefTimerResolution_struct : DefRec_Base_struct
   {
      DefRec_DefTimerResolution_struct() 
	 : DefRec_Base_struct(DEF_REC_TYPE__DefTimerResolution),
      ticksPerSecond(0) {}
      DefRec_DefTimerResolution_struct(uint64_t _ticksPerSecond)
	 : DefRec_Base_struct(DEF_REC_TYPE__DefTimerResolution),
      ticksPerSecond(_ticksPerSecond) {}

      uint64_t ticksPerSecond;
   };

   //
   // DefRec_DefProcess_struct
   //
   struct DefRec_DefProcess_struct : DefRec_Base_struct
   {
      DefRec_DefProcess_struct()
	 : DefRec_Base_struct(DEF_REC_TYPE__DefProcess),
      parent(0) {}
      DefRec_DefProcess_struct(uint32_t _deftoken, std::string _name,
			       uint32_t _parent)
	 : DefRec_Base_struct(DEF_REC_TYPE__DefProcess),
      name(_name), parent(_parent) { deftoken = _deftoken; }
      
      std::string name;
      uint32_t    parent;
   };

   //
   // DefRec_DefProcessGroup_struct
   //
   struct DefRec_DefProcessGroup_struct : DefRec_Base_struct
   {
      typedef enum { TYPE_NODE, TYPE_MPI_COMM_WORLD, TYPE_MPI_COMM_SELF,
		     TYPE_MPI_COMM_USER, TYPE_OMP_TEAM, TYPE_OTHER }
      ProcessGroupTypeT;

      DefRec_DefProcessGroup_struct()
	 : DefRec_Base_struct(DEF_REC_TYPE__DefProcessGroup),
      type(TYPE_OTHER) {}
      DefRec_DefProcessGroup_struct(uint32_t _loccpuid,
				    uint32_t _deftoken,
				    ProcessGroupTypeT _type,
				    std::string _name,
				    uint32_t _nmembers,
				    uint32_t * _members)
	 : DefRec_Base_struct(DEF_REC_TYPE__DefProcessGroup),
      name(_name), type(_type)
	 {
	    loccpuid = _loccpuid;
	    deftoken = _deftoken;
	    for( uint32_t i = 0; i < _nmembers; i++ )
	       members.push_back( _members[i] );
	 }

      DefRec_DefProcessGroup_struct(uint32_t _loccpuid,
				    uint32_t _deftoken,
				    ProcessGroupTypeT _type,
				    std::string _name,
				    std::vector<uint32_t> _members)
	 : DefRec_Base_struct(DEF_REC_TYPE__DefProcessGroup),
      name(_name), type(_type), members(_members)
	 { loccpuid = _loccpuid; deftoken = _deftoken; }

      std::string           name;
      ProcessGroupTypeT     type;
      std::vector<uint32_t> members;
   };

   //
   // DefRec_DefSclFile_struct
   //
   struct DefRec_DefSclFile_struct : DefRec_Base_struct
   {
      DefRec_DefSclFile_struct()
	 : DefRec_Base_struct(DEF_REC_TYPE__DefSclFile) {}
      DefRec_DefSclFile_struct(uint32_t _loccpuid, uint32_t _deftoken,
			       std::string _filename)
	 : DefRec_Base_struct(DEF_REC_TYPE__DefSclFile),
      filename(_filename)
	 { loccpuid = _loccpuid; deftoken = _deftoken; }
      
      std::string filename;
   };

   //
   // DefRec_DefScl_struct
   //
   struct DefRec_DefScl_struct : DefRec_Base_struct
   {
      DefRec_DefScl_struct()
	 : DefRec_Base_struct(DEF_REC_TYPE__DefScl),
      sclfile(0), sclline(0) {}
      DefRec_DefScl_struct(uint32_t _loccpuid, uint32_t _deftoken,
				uint32_t _sclfile, uint32_t _sclline)
	 : DefRec_Base_struct(DEF_REC_TYPE__DefScl),
      sclfile(_sclfile), sclline(_sclline) { loccpuid = _loccpuid; deftoken = _deftoken; }
      ~DefRec_DefScl_struct() {}

      uint32_t sclfile;
      uint32_t sclline;
   };

   //
   // DefRec_DefFileGroup_struct
   //
   struct DefRec_DefFileGroup_struct : DefRec_Base_struct
   {
      DefRec_DefFileGroup_struct()
	 : DefRec_Base_struct(DEF_REC_TYPE__DefFileGroup) {}
      DefRec_DefFileGroup_struct(uint32_t _loccpuid, uint32_t _deftoken,
				 std::string _name)
	 : DefRec_Base_struct(DEF_REC_TYPE__DefFileGroup),
      name(_name)
	 { loccpuid = _loccpuid; deftoken = _deftoken; }
      
      std::string name;
   };

   //
   // DefRec_DefFile_struct
   //
   struct DefRec_DefFile_struct : DefRec_Base_struct
   {
      DefRec_DefFile_struct()
	 : DefRec_Base_struct(DEF_REC_TYPE__DefFile),
      group(0) {}
      DefRec_DefFile_struct(uint32_t _loccpuid, uint32_t _deftoken,
			    std::string _name, uint32_t _group )
	 : DefRec_Base_struct(DEF_REC_TYPE__DefFile),
      name(_name), group(_group)
	 { loccpuid = _loccpuid; deftoken = _deftoken; }
      
      std::string name;
      uint32_t    group;
   };

   //
   // DefRec_DefFunctionGroup_struct
   //
   struct DefRec_DefFunctionGroup_struct : DefRec_Base_struct
   {
      DefRec_DefFunctionGroup_struct()
	 : DefRec_Base_struct(DEF_REC_TYPE__DefFunctionGroup) {}
      DefRec_DefFunctionGroup_struct(uint32_t _loccpuid,
					  uint32_t _deftoken,
					  std::string _name)
	 : DefRec_Base_struct(DEF_REC_TYPE__DefFunctionGroup),
      name(_name)
	 { loccpuid = _loccpuid; deftoken = _deftoken; }
      
      std::string name;
   };
   
   //
   // DefRec_DefFunction_struct
   //
   struct DefRec_DefFunction_struct : DefRec_Base_struct
   {
      DefRec_DefFunction_struct()
	 : DefRec_Base_struct(DEF_REC_TYPE__DefFunction),
      group(0), scltoken(0) {}
      DefRec_DefFunction_struct(uint32_t _loccpuid, uint32_t _deftoken,
				std::string _name, uint32_t _group,
				uint32_t _scltoken)
	 : DefRec_Base_struct(DEF_REC_TYPE__DefFunction),
      name(_name), group(_group), scltoken(_scltoken)
	 { loccpuid = _loccpuid; deftoken = _deftoken; }
      
      std::string name;
      uint32_t    group;
      uint32_t    scltoken;
   };

   //
   // DefRec_DefCollectiveOperation_struct
   //
   struct DefRec_DefCollectiveOperation_struct : DefRec_Base_struct
   {
      DefRec_DefCollectiveOperation_struct()
	 : DefRec_Base_struct(DEF_REC_TYPE__DefCollectiveOperation),
      type(0) {}
      DefRec_DefCollectiveOperation_struct(uint32_t _loccpuid,
					   uint32_t _collOp,
					   std::string _name,
					   uint32_t _type)
	 : DefRec_Base_struct(DEF_REC_TYPE__DefCollectiveOperation),
      name(_name), type(_type) { loccpuid = _loccpuid; deftoken = _collOp; }
      
      std::string name;
      uint32_t    type;
   };
   
   //
   // DefRec_DefCounterGroup_struct
   //
   struct DefRec_DefCounterGroup_struct : DefRec_Base_struct
   {
      DefRec_DefCounterGroup_struct()
	 : DefRec_Base_struct(DEF_REC_TYPE__DefCounterGroup) {}
      DefRec_DefCounterGroup_struct(uint32_t _loccpuid, uint32_t _deftoken,
				    std::string _name)
	 : DefRec_Base_struct(DEF_REC_TYPE__DefCounterGroup),
      name(_name)
	 { loccpuid = _loccpuid; deftoken = _deftoken; }
      
      std::string name;
   };

   //
   // DefRec_DefCounter_struct
   //
   struct DefRec_DefCounter_struct : DefRec_Base_struct
   {
      DefRec_DefCounter_struct()
	 : DefRec_Base_struct(DEF_REC_TYPE__DefCounter),
      properties(0), countergroup(0) {}
      DefRec_DefCounter_struct(uint32_t _loccpuid, uint32_t _deftoken,
			       std::string _name, uint32_t _properties,
			       uint32_t _countergroup, std::string _unit)
	 : DefRec_Base_struct(DEF_REC_TYPE__DefCounter),
      name(_name), properties(_properties), countergroup(_countergroup),
      unit(_unit)
	 { loccpuid = _loccpuid; deftoken = _deftoken; }
      
      std::string name;
      uint32_t    properties;
      uint32_t    countergroup;
      std::string unit;
   };


   // contructor
   Definitions();

   // destructor
   ~Definitions();

   bool run();

private:

   struct NodeProc_struct
   {
      NodeProc_struct(const uint32_t & _procid) : procid(_procid) {}

      uint32_t procid;
      
      bool operator==(const NodeProc_struct & a) const
      {
	 return procid == a.procid;
      }

      bool operator<(const NodeProc_struct & a) const
      {
	 if( procid % 65536 != a.procid % 65536 )
	    return procid % 65536 < a.procid % 65536;
	 else
	    return procid < a.procid;
      }
   };

   struct MPIComm_struct
   {
      MPIComm_struct(const uint32_t & _commid)
	 : commid(_commid), loccpuid(0), deftoken(0), index((uint32_t)-1) {}

      MPIComm_struct(const uint32_t & _commid, const uint32_t & _index)
	 : commid(_commid), loccpuid(0), deftoken(0), index(_index) {}

      MPIComm_struct(const uint32_t & _commid, const uint32_t & _loccpuid,
		     const uint32_t & _deftoken, const uint32_t & _index)
	 : commid(_commid), loccpuid(_loccpuid), deftoken(_deftoken), index(_index)
      {}

      uint32_t commid;
      uint32_t loccpuid;
      uint32_t deftoken;
      uint32_t index;

      bool operator==(const MPIComm_struct & a) const
      {
	 if( a.index == (uint32_t)-1 )
	    return commid == a.commid;
	 else
	    return ( commid == a.commid && index == a.index );
      }
   };

   bool readLocal( std::vector<DefRec_Base_struct*> * p_vecLocDefs );
   bool createGlobal( const std::vector<DefRec_Base_struct*> * p_vecLocDefs,
		      std::vector<DefRec_Base_struct*> * p_vecGlobDefs );
   bool writeGlobal( const std::vector<DefRec_Base_struct*> * p_vecGlobDefs );

   bool addProc2NodeGroup( const std::string & nodeName,
			   const uint32_t & nodeProc );
   bool addNodeGroups2Global( std::vector<DefRec_Base_struct*> *
			      p_vecGlobDefs );

   bool addMPIComm( const uint32_t proc, const uint32_t defToken,
		    const std::vector<uint32_t> & vecMembers );
   bool addMPIComms2Global( std::vector<DefRec_Base_struct*> *
			    p_vecGlobDefs );

   uint32_t getMPICommIdByMembers( const std::vector<uint32_t> & vecMembers );

   std::map<std::string, std::vector<NodeProc_struct> > m_mapNodeProcs;

   std::map<uint32_t, std::vector<uint32_t> > m_mapMPICommId2Members;

   std::map<uint32_t, std::list<MPIComm_struct> > m_mapProcMPIComms;

};

// instance of class Definitions
extern Definitions * theDefinitions;

#endif // _VT_UNIFY_DEFS_H_
