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

#include "vt_unify.h"
#include "vt_unify_tkfac.h"

#include <assert.h>

TokenFactoryC * theTokenFactory = 0; // instance of class TokenFactoryC

//////////////////// class TokenFactoryC ////////////////////

// public methods
//

TokenFactoryC::TokenFactoryC()
{
   // Empty
}

TokenFactoryC::~TokenFactoryC()
{
   // Empty
}

void
TokenFactoryC::addScope( const DefRecTypeT & type, TokenFactoryScopeI * scope )
{
   assert( type < DEF_REC_TYPE__Num );
   assert( scope );

   // search for already added scope instance
   std::map<DefRecTypeT, TokenFactoryScopeI*>::const_iterator it = m_def2scope.find( type );

   // add scope instance to map, if not found
   if( it == m_def2scope.end() )
      m_def2scope[type] = scope;
}

void
TokenFactoryC::deleteScope( const DefRecTypeT & type )
{
   assert( type < DEF_REC_TYPE__Num );

   // search scope instance
   std::map<DefRecTypeT, TokenFactoryScopeI*>::iterator it = m_def2scope.find( type );

   // erase map entry, if found
   if( it != m_def2scope.end() )
   {
      delete it->second;
      m_def2scope.erase( it );
   }
}

TokenFactoryScopeI *
TokenFactoryC::getScope( const DefRecTypeT & type ) const
{
   assert( type < DEF_REC_TYPE__Num );

   // search scope instance
   std::map<DefRecTypeT, TokenFactoryScopeI*>::const_iterator it_scope =
      m_def2scope.find( type );

   // if found, return scope instance; otherwise, return 0
   return ( it_scope != m_def2scope.end() ) ? it_scope->second : 0;
}

#ifdef VT_MPI

bool
TokenFactoryC::share()
{
   bool error = false;

   assert( NumRanks > 1 );
   assert( !m_def2scope.empty() );

   // block until all ranks have reached this point
   CALL_MPI( MPI_Barrier( MPI_COMM_WORLD ) );

   VPrint( 1, "Sharing token translation tables\n" );

   char * buffer;
   VT_MPI_INT buffer_pos;
   VT_MPI_INT buffer_size;

   MASTER
   {
      // get size needed for the send buffer
      //

      buffer_size = 0;

      for( std::map<DefRecTypeT, TokenFactoryScopeI*>::const_iterator it =
           m_def2scope.begin(); it != m_def2scope.end(); it++ )
      {
         // get scope
         TokenFactoryScopeC<DefRec_BaseS> * scope =
            static_cast<TokenFactoryScopeC<DefRec_BaseS>*>( it->second );

         // get size of token translation map
         buffer_size += scope->getPackSize();
      }
   }

   // broadcast buffer size
   CALL_MPI( MPI_Bcast( &buffer_size, 1, MPI_INT, 0, MPI_COMM_WORLD ) );

   // allocate memory for the send/receive buffer
   //
   buffer = new char[buffer_size];
   assert( buffer );

   MASTER
   {
      // pack send buffer
      //

      buffer_pos = 0;

      for( std::map<DefRecTypeT, TokenFactoryScopeI*>::const_iterator it =
           m_def2scope.begin(); it != m_def2scope.end(); it++ )
      {
         // get scope
         TokenFactoryScopeC<DefRec_BaseS> * scope =
            static_cast<TokenFactoryScopeC<DefRec_BaseS>*>( it->second );

         // pack token translation map to buffer
         scope->pack( buffer, buffer_size, buffer_pos );
      }
   }

   // broadcast buffer
   CALL_MPI( MPI_Bcast( buffer, buffer_size, MPI_PACKED, 0, MPI_COMM_WORLD ) );

   SLAVE
   {
      // unpack receive buffer
      //

      buffer_pos = 0;

      for( std::map<DefRecTypeT, TokenFactoryScopeI*>::const_iterator it =
           m_def2scope.begin(); it != m_def2scope.end(); it++ )
      {
         // get scope
         TokenFactoryScopeC<DefRec_BaseS> * scope =
            static_cast<TokenFactoryScopeC<DefRec_BaseS>*>( it->second );

         // unpack token translation map from buffer
         scope->unpack( buffer, buffer_size, buffer_pos );
      }
   }

   // free memory of send/receive buffer
   delete [] buffer;

//   SyncError( &error );

   return !error;
}

#endif // VT_MPI
