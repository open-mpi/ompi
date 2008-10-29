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

#ifndef _VT_UNIFY_TKFAC_H_
#define _VT_UNIFY_TKFAC_H_

#include "vt_inttypes.h"

#include <algorithm>
#include <map>
#include <string>
#include <vector>

// token factory types
//
enum
{
   TKFAC__DEF_SCL_FILE,
   TKFAC__DEF_SCL,
   TKFAC__DEF_FILE_GROUP,
   TKFAC__DEF_FILE,
   TKFAC__DEF_FUNCTION_GROUP,
   TKFAC__DEF_FUNCTION,
   TKFAC__DEF_COLL_OP,
   TKFAC__DEF_COUNTER_GROUP,
   TKFAC__DEF_COUNTER,
   TKFAC__DEF_PROCESS_GROUP,
   TKFAC_NUM
};

//
// TokenFactory base class
//
class TokenFactory
{
public:

   // contructor
   TokenFactory() : m_SeqToken(1) {}

   // destructor
   virtual ~TokenFactory() {}

   // set local/global token translation
   void setTranslation( uint32_t mCpuId,
			uint32_t localToken, uint32_t globalToken );

   // translate local to global token
   uint32_t translateLocalToken( uint32_t mCpuId, uint32_t localToken );

protected:

   // sequential token
   uint32_t m_SeqToken;

   // map cpu id -> local/global token
   std::map<uint32_t, std::map<uint32_t, uint32_t>* > m_mapLocGlobToken;

};

//
// TokenFactory_DefSclFile
//
class TokenFactory_DefSclFile : public TokenFactory
{
public:

   // contructor
   TokenFactory_DefSclFile() : TokenFactory() {}

   // destructor
   ~TokenFactory_DefSclFile() {}

   // get global token by definition specifications
   uint32_t getGlobalToken( std::string filename );
   
   // create global token by definition specifications
   uint32_t createGlobalToken( uint32_t mCpuId, uint32_t localToken,
			       std::string filename );

private:

   // map definition specifications -> global token
   std::map<std::string, uint32_t> m_mapDefSclFileGlobToken;

};

//
// TokenFactory_DefScl
//
class TokenFactory_DefScl : public TokenFactory
{
public:

   // definition specifications
   //
   struct DefScl_struct
   {
      uint32_t global_token;
      uint32_t sclfile;
      uint32_t sclline;
   };

   // class for compare definition specifications
   //
   class DefScl_eq: public std::unary_function<DefScl_struct, bool>
   {
      uint32_t sclfile;
      uint32_t sclline;
   public:
      explicit DefScl_eq(const uint32_t & _sclfile,
			 const uint32_t & _sclline)
	 : sclfile(_sclfile), sclline(_sclline) {}
      bool operator()(const DefScl_struct & a) const 
      {
	 return ( a.sclfile == sclfile 
		  && a.sclline == sclline );
      } 
   };

   // constructor
   TokenFactory_DefScl() : TokenFactory() {}

   // destructor
   ~TokenFactory_DefScl() {}

   // get global token by definition specifications
   uint32_t getGlobalToken( uint32_t sclfile, uint32_t sclline );

   // create global token by definition specifications
   uint32_t createGlobalToken( uint32_t mCpuId, uint32_t localToken,
			       uint32_t sclfile, uint32_t sclline );

private:

   // vector of definition specifications
   std::vector<DefScl_struct> m_vecDefScl;

};

//
// TokenFactory_DefFileGroup
//
class TokenFactory_DefFileGroup : public TokenFactory
{
public:

   // constructor
   TokenFactory_DefFileGroup() : TokenFactory() {}

   // destructor
   ~TokenFactory_DefFileGroup() {}

   // get global token by definition specifications
   uint32_t getGlobalToken( std::string name );

   // create global token by definition specifications
   uint32_t createGlobalToken( uint32_t mCpuId, uint32_t localToken,
			       std::string name );

private:

   // map definition specifications -> global token
   std::map<std::string, uint32_t> m_mapDefFileGroupGlobToken;

};

//
// TokenFactory_DefFile
//
class TokenFactory_DefFile : public TokenFactory
{
public:

   // definition specifications
   //
   struct DefFile_struct
   {
      uint32_t global_token;
      std::string name;
      uint32_t group;
   };

   // class for compare definition specifications
   //
   class DefFile_eq: public std::unary_function<DefFile_struct, bool>
   {
      std::string name;
      uint32_t group;
   public:
      explicit DefFile_eq(const std::string & _name,
			  const uint32_t & _group)
	 : name(_name), group(_group) {}
      bool operator()(const DefFile_struct & a) const 
      {
	 return ( a.name.compare( name ) == 0
		  && a.group == group );
      } 
   };

   // constructor
   TokenFactory_DefFile() : TokenFactory() {}

   // destructor
   ~TokenFactory_DefFile() {}

   // get global token by definition specifications
   uint32_t getGlobalToken( std::string name, uint32_t group );

   // create global token by definition specifications
   uint32_t createGlobalToken( uint32_t mCpuId, uint32_t localToken,
			       std::string name, uint32_t group );

private:

   // vector of definition specifications
   std::vector<DefFile_struct> m_vecDefFile;

};

//
// TokenFactory_DefFunctionGroup
//
class TokenFactory_DefFunctionGroup : public TokenFactory
{
public:

   // constructor
   TokenFactory_DefFunctionGroup() : TokenFactory() {}

   // destructor
   ~TokenFactory_DefFunctionGroup() {}

   // get global token by definition specifications
   uint32_t getGlobalToken( std::string name );

   // create global token by definition specifications
   uint32_t createGlobalToken( uint32_t mCpuId, uint32_t localToken,
			       std::string name );

private:

   // map definition specifications -> global token
   std::map<std::string, uint32_t> m_mapDefFunctionGroupGlobToken;

};

//
// TokenFactory_DefFunction
//
class TokenFactory_DefFunction : public TokenFactory
{
public:

   // definition specifications
   //
   struct DefFunction_struct
   {
      uint32_t global_token;
      std::string name;
      uint32_t group;
      uint32_t scltoken;
   };

   // class for compare definition specifications
   //
   class DefFunction_eq: public std::unary_function<DefFunction_struct, bool>
   {
      std::string name;
      uint32_t group;
      uint32_t scltoken;
   public:
      explicit DefFunction_eq(const std::string & _name,
			      const uint32_t & _group,
			      const uint32_t & _scltoken)
	 : name(_name), group(_group), scltoken(_scltoken) {}
      bool operator()(const DefFunction_struct & a) const 
      {
	 return ( a.name.compare( name ) == 0
		  && a.group == group
		  && a.scltoken == scltoken );
      } 
   };

   // constructor
   TokenFactory_DefFunction() : TokenFactory() {}

   // destructor
   ~TokenFactory_DefFunction() {}

   // get global token by definition specifications
   uint32_t getGlobalToken( std::string name, uint32_t group, uint32_t scltoken );

   // create global token by definition specifications
   uint32_t createGlobalToken( uint32_t mCpuId, uint32_t localToken,
			       std::string name, uint32_t group, uint32_t scltoken );

private:

   // vector of definition specifications
   std::vector<DefFunction_struct> m_vecDefFunction;

};

//
// TokenFactory_DefCollectiveOperation
//
class TokenFactory_DefCollectiveOperation : public TokenFactory
{
public:

   // definition specifications
   //
   struct DefCollectiveOperation_struct
   {
      uint32_t global_token;
      std::string name;
      uint32_t type;
   };
   
   // class for compare definition specifications
   //
   class DefCollectiveOperation_eq: public std::unary_function<DefCollectiveOperation_struct, bool>
   {
      std::string name;
      uint32_t type;
   public:
      explicit DefCollectiveOperation_eq(const std::string & _name,
					 const uint32_t & _type)
	 : name(_name), type(_type) {}
      bool operator()(const DefCollectiveOperation_struct & a) const 
      {
	 return ( a.name.compare( name ) == 0
		  && a.type == type );
      } 
   };

   // constructor
   TokenFactory_DefCollectiveOperation() : TokenFactory() {}

   // destructor
   ~TokenFactory_DefCollectiveOperation() {}

   // get global token by definition specifications
   uint32_t getGlobalToken( std::string name, uint32_t type );

   // create global token by definition specifications
   uint32_t createGlobalToken( uint32_t mCpuId, uint32_t localToken,
			       std::string name, uint32_t type );

private:

   // vector of definition specifications
   std::vector<DefCollectiveOperation_struct> m_vecDefCollectiveOperation;

};

//
// TokenFactory_DefCounterGroup
//
class TokenFactory_DefCounterGroup : public TokenFactory
{
public:

   // constructor
   TokenFactory_DefCounterGroup() : TokenFactory() {}

   // destructor
   ~TokenFactory_DefCounterGroup() {}

   // get global token by definition specifications
   uint32_t getGlobalToken( std::string name );

   // create global token by definition specifications
   uint32_t createGlobalToken( uint32_t mCpuId, uint32_t localToken,
			       std::string name );

private:

   // map definition specifications -> global token
   std::map<std::string, uint32_t> m_mapDefCounterGroupGlobToken;

};

//
// TokenFactory_DefCounter
//
class TokenFactory_DefCounter : public TokenFactory
{
public:

   // definition specifications
   //
   struct DefCounter_struct
   {
      uint32_t global_token;
      std::string name;
      uint32_t properties;
      uint32_t countergroup;
      std::string unit;
   };

   // class for compare definition specifications
   //
   class DefCounter_eq: public std::unary_function<DefCounter_struct, bool>
   {
      std::string name;
      uint32_t properties;
      uint32_t countergroup;
      std::string unit;
   public:
      explicit DefCounter_eq(const std::string & _name,
			     const uint32_t & _properties,
			     const uint32_t & _countergroup,
			     const std::string & _unit)
	 : name(_name), properties(_properties),
	 countergroup(_countergroup), unit(_unit) {}
      bool operator()(const DefCounter_struct & a) const 
      {
	 return ( a.name.compare( name ) == 0
		  && a.properties == properties
		  && a.countergroup == countergroup
		  && a.unit.compare( unit ) == 0 );
      } 
   };

   // constructor
   TokenFactory_DefCounter() : TokenFactory() {}
   
   // destructor
   ~TokenFactory_DefCounter() {}

   // get global token by definition specifications
   uint32_t getGlobalToken( std::string name, uint32_t properties,
			    uint32_t countergroup, std::string unit );

   // create global token by definition specifications
   uint32_t createGlobalToken( uint32_t mCpuId, uint32_t localToken,
			       std::string name, uint32_t properties,
			       uint32_t countergroup, std::string unit );

private:

   // vector of definition specifications
   std::vector<DefCounter_struct> m_vecDefCounter;

};

//
// TokenFactory_DefProcessGroup
//
class TokenFactory_DefProcessGroup : public TokenFactory
{
public:

   // definition specifications
   //
   struct DefProcessGroup_struct
   {
      uint32_t global_token;
      std::string name;
      std::vector<uint32_t> members;
   };

   // class for compare definition specifications
   //
   class DefProcessGroup_eq: public std::unary_function<DefProcessGroup_struct, bool>
   {
      std::string name;
      std::vector<uint32_t> members;
   public:
      explicit DefProcessGroup_eq(const std::string & _name,
				  const std::vector<uint32_t> & _members)
	 : name(_name), members(_members) {}

      bool operator()(const DefProcessGroup_struct & a) const 
      {
	 return ( a.name.compare( name ) == 0
		  && a.members == members );
      } 
   };

   // constructor
   TokenFactory_DefProcessGroup() : TokenFactory() 
      { m_SeqToken = 1000000000; }

   // destructor
   ~TokenFactory_DefProcessGroup() {}

   // get global token by definition specifications
   uint32_t getGlobalToken( std::string name,
			    std::vector<uint32_t> vecMembers );

   uint32_t createGlobalToken( uint32_t mCpuId, uint32_t localToken,
			       std::string name,
			       std::vector<uint32_t> vecMembers );

private:

   // vector of definition specifications
   std::vector<DefProcessGroup_struct> m_vecDefProcessGroup;

};

// array of token factories
extern TokenFactory * theTokenFactory[TKFAC_NUM];

#endif // _VT_UNIFY_TKFAC_H_
