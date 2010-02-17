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
#include "vt_unify_tkfac.h"

#include <map>
#include <string>
#include <vector>

// array of token factories
TokenFactory * theTokenFactory[TKFAC_NUM];

void
TokenFactory::setTranslation( uint32_t mCpuId,
			      uint32_t localToken, uint32_t globalToken )
{
   // search local/global token mapping by cpu id
   std::map<uint32_t, std::map<uint32_t, uint32_t>* >::iterator it_cpu =
      m_mapLocGlobToken.find( mCpuId );

   // cpu id not found
   //
   if( it_cpu == m_mapLocGlobToken.end() )
   {
      // create local/global token mapping for cpu
      //
      std::map<uint32_t, uint32_t> * p_map_loc_glob_token =
	 new std::map<uint32_t, uint32_t>();

      p_map_loc_glob_token->insert( std::make_pair( localToken, globalToken ) );

      m_mapLocGlobToken.insert( std::make_pair( mCpuId, p_map_loc_glob_token ) );
   }
   // cpu id found
   //
   else
   {
      // search local token
      std::map<uint32_t, uint32_t>::iterator it_local_token =
	 it_cpu->second->find( localToken );

      // add local/global token
      if( it_local_token == it_cpu->second->end() )
	 it_cpu->second->insert( std::make_pair( localToken, globalToken ) );
   }
}

uint32_t
TokenFactory::translateLocalToken( uint32_t mCpuId, uint32_t localToken )
{
   // search local/global token mapping by cpu id
   std::map<uint32_t, std::map<uint32_t, uint32_t>* >::iterator it_cpu =
      m_mapLocGlobToken.find( mCpuId );

   // cpu id found
   //
   if( it_cpu != m_mapLocGlobToken.end() )
   {
      // search local token
      std::map<uint32_t, uint32_t>::iterator it_local_token =
	 it_cpu->second->find( localToken ); 

      // return global token if local token found
      if( it_local_token != it_cpu->second->end() )
	 return it_local_token->second;
   }

   return 0;
}

#ifdef VT_MPI

VT_MPI_INT
TokenFactory::getPackSize()
{
   VT_MPI_INT buffer_size;

   // m_mapLocGlobToken.size()
   VTUnify_MPI_Pack_size( 1, VTUnify_MPI_UNSIGNED,
                          VTUnify_MPI_COMM_WORLD,
                          &buffer_size );

   if( m_mapLocGlobToken.size() > 0 )
   {
      VT_MPI_INT size;

      std::map<uint32_t, std::map<uint32_t, uint32_t>* >::iterator it_cpu;
      for( it_cpu = m_mapLocGlobToken.begin(); it_cpu != m_mapLocGlobToken.end();
         it_cpu++ )
      {
         // m_mapLocGlobToken[].first, m_mapLocGlobToken[].second.size()
         VTUnify_MPI_Pack_size( 2, VTUnify_MPI_UNSIGNED,
                                VTUnify_MPI_COMM_WORLD, &size );
         buffer_size += size;

         if( it_cpu->second && it_cpu->second->size() > 0 )
         {
            // m_mapLocGlobToken[]
            VTUnify_MPI_Pack_size( (VT_MPI_INT)it_cpu->second->size() * 2,
                                   VTUnify_MPI_UNSIGNED,
                                   VTUnify_MPI_COMM_WORLD, &size );
            buffer_size += size;
         }
      }
   }

   return buffer_size;
}

void
TokenFactory::packTranslations( char * buffer, VT_MPI_INT bufferSize,
                                VT_MPI_INT * position )
{
   // m_mapLocGlobToken.size()
   uint32_t cpu_map_size = m_mapLocGlobToken.size();
   VTUnify_MPI_Pack( &cpu_map_size, 1, VTUnify_MPI_UNSIGNED,
                      buffer, bufferSize, position, VTUnify_MPI_COMM_WORLD );

   // m_mapLocGlobToken
   //
   if( cpu_map_size > 0 )
   {
      std::map<uint32_t, std::map<uint32_t, uint32_t>* >::iterator it_cpu;
      for( it_cpu = m_mapLocGlobToken.begin(); it_cpu != m_mapLocGlobToken.end();
           it_cpu++ )
      {
         // m_mapLocGlobToken[].first
         uint32_t cpu_id = it_cpu->first;
         VTUnify_MPI_Pack( &cpu_id, 1, VTUnify_MPI_UNSIGNED,
                           buffer, bufferSize, position,
                           VTUnify_MPI_COMM_WORLD );

         // m_mapLocGlobToken[].second.size()
         uint32_t token_map_size = (it_cpu->second) ? it_cpu->second->size() : 0;
         VTUnify_MPI_Pack( &token_map_size, 1, VTUnify_MPI_UNSIGNED,
                           buffer, bufferSize, position,
                           VTUnify_MPI_COMM_WORLD );

         // m_mapLocGlobToken[].second
         //
         if( token_map_size > 0 )
         {
            uint32_t * token_map_firsts = new uint32_t[token_map_size];
            uint32_t * token_map_seconds = new uint32_t[token_map_size];
            std::map<uint32_t, uint32_t>::iterator it_token;
            uint32_t i;

            for( it_token = it_cpu->second->begin(), i = 0;
                 it_token != it_cpu->second->end(), i < token_map_size;
                 it_token++, i++ )
            {
               token_map_firsts[i] = it_token->first;
               token_map_seconds[i] = it_token->second;
            }

            VTUnify_MPI_Pack( token_map_firsts, (VT_MPI_INT)token_map_size,
                              VTUnify_MPI_UNSIGNED, buffer, bufferSize,
                              position, VTUnify_MPI_COMM_WORLD );
            VTUnify_MPI_Pack( token_map_seconds, (VT_MPI_INT)token_map_size,
                              VTUnify_MPI_UNSIGNED, buffer, bufferSize,
                              position, VTUnify_MPI_COMM_WORLD );

            delete [] token_map_firsts;
            delete [] token_map_seconds;
         }
      }
   }
}

void
TokenFactory::unpackTranslations( char * buffer, VT_MPI_INT bufferSize,
                                  VT_MPI_INT * position )
{
   // m_mapLocGlobToken.size()
   uint32_t cpu_map_size;
   VTUnify_MPI_Unpack( buffer, bufferSize, position, &cpu_map_size, 1,
                       VTUnify_MPI_UNSIGNED, VTUnify_MPI_COMM_WORLD );

   // m_mapLocGlobToken
   //
   if( cpu_map_size > 0 )
   {
      for( uint32_t i = 0; i < cpu_map_size; i++ )
      {
         // m_mapLocGlobToken[].first
         uint32_t cpu_id;
         VTUnify_MPI_Unpack( buffer, bufferSize, position, &cpu_id, 1,
                             VTUnify_MPI_UNSIGNED, VTUnify_MPI_COMM_WORLD );

         // m_mapLocGlobToken[].second.size()
         uint32_t token_map_size;
         VTUnify_MPI_Unpack( buffer, bufferSize, position, &token_map_size, 1,
                             VTUnify_MPI_UNSIGNED, VTUnify_MPI_COMM_WORLD );

         // m_mapLocGlobToken[].second
         //
         if( token_map_size > 0 )
         {
            uint32_t * token_map_firsts = new uint32_t[token_map_size];
            uint32_t * token_map_seconds = new uint32_t[token_map_size];

            VTUnify_MPI_Unpack( buffer, bufferSize, position,
                                token_map_firsts, (VT_MPI_INT)token_map_size,
                                VTUnify_MPI_UNSIGNED, VTUnify_MPI_COMM_WORLD );
            VTUnify_MPI_Unpack( buffer, bufferSize, position,
                                token_map_seconds, (VT_MPI_INT)token_map_size,
                                VTUnify_MPI_UNSIGNED, VTUnify_MPI_COMM_WORLD );

            for( uint32_t j = 0; j < token_map_size; j++ )
               setTranslation( cpu_id, token_map_firsts[j], token_map_seconds[j] );

            delete [] token_map_firsts;
            delete [] token_map_seconds;
         }
      }
   }
}

#endif // VT_MPI

//
// TokenFactory_DefSclFile
//

uint32_t
TokenFactory_DefSclFile::getGlobalToken( std::string filename )
{
   std::map<std::string, uint32_t>::iterator it =
      m_mapDefSclFileGlobToken.find( filename );
   
   if( it != m_mapDefSclFileGlobToken.end() )
      return it->second;
   else
      return 0;
}

uint32_t
TokenFactory_DefSclFile::createGlobalToken( uint32_t mCpuId, uint32_t localToken,
					    std::string filename )
{
   uint32_t global_token = m_SeqToken++;

   m_mapDefSclFileGlobToken.insert( std::make_pair( filename,
						    global_token ) );
  
   setTranslation( mCpuId, localToken, global_token );

   return global_token;
}

//
// TokenFactory_DefScl
//

uint32_t
TokenFactory_DefScl::getGlobalToken( uint32_t sclfile, uint32_t sclline )
{
   std::vector<DefScl_struct>::iterator it =
      std::find_if( m_vecDefScl.begin(), m_vecDefScl.end(),
		    DefScl_eq( sclfile, sclline ) );
	       
   if( it != m_vecDefScl.end() )
      return it->global_token;
   else
      return 0;
}

uint32_t
TokenFactory_DefScl::createGlobalToken( uint32_t mCpuId, uint32_t localToken,
					uint32_t sclfile, uint32_t sclline )
{
   uint32_t global_token = m_SeqToken++;

   DefScl_struct defscl;

   defscl.global_token = global_token;
   defscl.sclfile = sclfile;
   defscl.sclline = sclline;

   m_vecDefScl.push_back( defscl );

   setTranslation( mCpuId, localToken, global_token );

   return global_token;
}

//
// TokenFactory_DefFileGroup
//

uint32_t
TokenFactory_DefFileGroup::getGlobalToken( std::string name )
{
   std::map<std::string, uint32_t>::iterator it =
      m_mapDefFileGroupGlobToken.find( name );
   
   if( it != m_mapDefFileGroupGlobToken.end() )
      return it->second;
   else
      return 0;
}

uint32_t
TokenFactory_DefFileGroup::createGlobalToken( uint32_t mCpuId, uint32_t localToken, std::string name )
{
   uint32_t global_token = m_SeqToken++;

   m_mapDefFileGroupGlobToken.insert( std::make_pair( name,
						      global_token ) );
  
   setTranslation( mCpuId, localToken, global_token );

   return global_token;
}

//
// TokenFactory_DefFile
//

uint32_t
TokenFactory_DefFile::getGlobalToken( std::string name, uint32_t group )
{
   std::vector<DefFile_struct>::iterator it =
      find_if( m_vecDefFile.begin(), m_vecDefFile.end(),
	       DefFile_eq( name, group ) );
	       
   if( it != m_vecDefFile.end() )
      return it->global_token;
   else
      return 0;
}

uint32_t
TokenFactory_DefFile::createGlobalToken( uint32_t mCpuId, uint32_t localToken, std::string name, uint32_t group )
{
   uint32_t global_token = m_SeqToken++;

   DefFile_struct deffile;

   deffile.global_token = global_token;
   deffile.name = name;
   deffile.group = group;

   m_vecDefFile.push_back( deffile );

   setTranslation( mCpuId, localToken, global_token );

   return global_token;
}

//
// TokenFactory_DefFunctionGroup
//

uint32_t
TokenFactory_DefFunctionGroup::getGlobalToken( std::string name )
{
   std::map<std::string, uint32_t>::iterator it =
      m_mapDefFunctionGroupGlobToken.find( name );
   
   if( it != m_mapDefFunctionGroupGlobToken.end() )
      return it->second;
   else
      return 0;
}

uint32_t
TokenFactory_DefFunctionGroup::createGlobalToken( uint32_t mCpuId, uint32_t localToken,
						  std::string name )
{
   uint32_t global_token = m_SeqToken++;

   m_mapDefFunctionGroupGlobToken.insert( std::make_pair( name,
							  global_token ) );
  
   setTranslation( mCpuId, localToken, global_token );

   return global_token;
}

//
// TokenFactory_DefFunction
//

uint32_t
TokenFactory_DefFunction::getGlobalToken( std::string name, uint32_t group, uint32_t scltoken )
{
   std::vector<DefFunction_struct>::iterator it =
      std::find_if( m_vecDefFunction.begin(), m_vecDefFunction.end(),
		    DefFunction_eq( name, group, scltoken ) );
	       
   if( it != m_vecDefFunction.end() )
      return it->global_token;
   else
      return 0;
}

uint32_t
TokenFactory_DefFunction::createGlobalToken( uint32_t mCpuId, uint32_t localToken,
					     std::string name, uint32_t group, uint32_t scltoken )
{
   uint32_t global_token = m_SeqToken++;

   DefFunction_struct deffunction;

   deffunction.global_token = global_token;
   deffunction.name = name;
   deffunction.group = group;
   deffunction.scltoken = scltoken;

   m_vecDefFunction.push_back( deffunction );

   setTranslation( mCpuId, localToken, global_token );

   return global_token;
}

//
// TokenFactory_DefCollectiveOperation
//

uint32_t
TokenFactory_DefCollectiveOperation::getGlobalToken( std::string name, uint32_t type )
{
   std::vector<DefCollectiveOperation_struct>::iterator it =
      std::find_if( m_vecDefCollectiveOperation.begin(), m_vecDefCollectiveOperation.end(),
		    DefCollectiveOperation_eq( name, type ) );
	       
   if( it != m_vecDefCollectiveOperation.end() )
      return it->global_token;
   else
      return 0;
}

uint32_t
TokenFactory_DefCollectiveOperation::createGlobalToken( uint32_t mCpuId, uint32_t localToken,
							std::string name, uint32_t type )
{
   uint32_t global_token = m_SeqToken++;

   DefCollectiveOperation_struct defcollop;

   defcollop.global_token = global_token;
   defcollop.name = name;
   defcollop.type = type;

   m_vecDefCollectiveOperation.push_back( defcollop );

   setTranslation( mCpuId, localToken, global_token );

   return global_token;
}

//
// TokenFactory_DefCounterGroup
//

uint32_t
TokenFactory_DefCounterGroup::getGlobalToken( std::string name )
{
   std::map<std::string, uint32_t>::iterator it =
      m_mapDefCounterGroupGlobToken.find( name );
   
   if( it != m_mapDefCounterGroupGlobToken.end() )
      return it->second;
   else
      return 0;
}

uint32_t
TokenFactory_DefCounterGroup::createGlobalToken( uint32_t mCpuId, uint32_t localToken,
						 std::string name )
{
   uint32_t global_token = m_SeqToken++;

   m_mapDefCounterGroupGlobToken.insert( std::make_pair( name,
							 global_token ) );
  
   setTranslation( mCpuId, localToken, global_token );

   return global_token;
}

//
// TokenFactory_DefCounter
//

uint32_t
TokenFactory_DefCounter::getGlobalToken( std::string name, uint32_t properties,
					 uint32_t countergroup, std::string unit )
{
   std::vector<DefCounter_struct>::iterator it =
      std::find_if( m_vecDefCounter.begin(), m_vecDefCounter.end(),
		    DefCounter_eq( name, properties, countergroup, unit ) );
	       
   if( it != m_vecDefCounter.end() )
      return it->global_token;
   else
      return 0;
}

uint32_t
TokenFactory_DefCounter::createGlobalToken( uint32_t mCpuId, uint32_t localToken,
					    std::string name, uint32_t properties,
					    uint32_t countergroup, std::string unit )
{
   uint32_t global_token = m_SeqToken++;

   DefCounter_struct defcounter;

   defcounter.global_token = global_token;
   defcounter.name = name;
   defcounter.properties = properties;
   defcounter.countergroup = countergroup;
   defcounter.unit = unit;

   m_vecDefCounter.push_back( defcounter );

   setTranslation( mCpuId, localToken, global_token );

   return global_token;
}

//
// TokenFactory_DefProcessGroup
//

uint32_t
TokenFactory_DefProcessGroup::getGlobalToken( std::string name,
					      std::vector<uint32_t> vecMembers )
{
   std::vector<DefProcessGroup_struct>::iterator it =
      std::find_if( m_vecDefProcessGroup.begin(), m_vecDefProcessGroup.end(),
		    DefProcessGroup_eq( name, vecMembers ) );
	       
   if( it != m_vecDefProcessGroup.end() )
      return it->global_token;
   else
      return 0;
}

uint32_t
TokenFactory_DefProcessGroup::createGlobalToken( uint32_t mCpuId, uint32_t localToken,
						 std::string name,
						 std::vector<uint32_t> vecMembers )
{
   uint32_t global_token = m_SeqToken++;

   DefProcessGroup_struct defprocessgroup;

   defprocessgroup.global_token = global_token;
   defprocessgroup.name = name;
   defprocessgroup.members = vecMembers;

   m_vecDefProcessGroup.push_back( defprocessgroup );

   setTranslation( mCpuId, localToken, global_token );

   return global_token;
}

//
// TokenFactory_DefMarker
//

uint32_t
TokenFactory_DefMarker::getGlobalToken( std::string name, uint32_t type )
{
   std::vector<DefMarker_struct>::iterator it =
      std::find_if( m_vecDefMarker.begin(), m_vecDefMarker.end(),
		    DefMarker_eq( name, type ) );
	       
   if( it != m_vecDefMarker.end() )
      return it->global_token;
   else
      return 0;
}

uint32_t
TokenFactory_DefMarker::createGlobalToken( uint32_t mCpuId, uint32_t localToken,
					   std::string name, uint32_t type )
{
   uint32_t global_token = m_SeqToken++;

   DefMarker_struct defmarker;

   defmarker.global_token = global_token;
   defmarker.name = name;
   defmarker.type = type;

   m_vecDefMarker.push_back( defmarker );

   setTranslation( mCpuId, localToken, global_token );

   return global_token;
}
