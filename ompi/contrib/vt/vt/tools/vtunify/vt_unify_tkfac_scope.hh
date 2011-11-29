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

#ifndef _VT_UNIFY_TKFAC_SCOPE_HH_
#define _VT_UNIFY_TKFAC_SCOPE_HH_

//////////////////// class TokenFactoryScopeC ////////////////////

// public methods
//

template <class T>
TokenFactoryScopeC<T>::TokenFactoryScopeC( std::set<T> * _globdefs,
   const uint32_t & _tkoffs )
   : m_globDefs( _globdefs ), m_seqToken( _tkoffs )
{
   // Empty
}

template <class T>
TokenFactoryScopeC<T>::~TokenFactoryScopeC()
{
   // Empty
}

template <class T>
uint32_t
TokenFactoryScopeC<T>::create( const void * localDef )
{
   const T & local_def = *static_cast<const T*>(localDef);

   uint32_t global_token;

   // search for already created global definition
   typename std::set<T>::const_iterator it =
      std::find( m_globDefs->begin(), m_globDefs->end(), local_def );

   // get its global token, if found
   //
   if( it != m_globDefs->end() )
   {
      global_token = it->deftoken;
   }
   // otherwise, create global definition
   //
   else
   {
      T global_def = local_def;

      global_def.loccpuid = 0;
      global_def.deftoken = global_token = getNextToken();

      m_globDefs->insert( global_def ).first;
   }

   // set token translation for process, if necessary
   if( local_def.loccpuid != 0 && local_def.deftoken != 0 )
      setTranslation( local_def.loccpuid, local_def.deftoken, global_token );

   return global_token;
}

template <class T>
void
TokenFactoryScopeC<T>::setTranslation( const uint32_t & process,
   const uint32_t & localToken, const uint32_t & globalToken )
{
   // get master process id
   uint32_t mprocess = process & VT_TRACEID_BITMASK;

   // set token translation
   m_mapLocGlobToken[mprocess][localToken] = globalToken;
}

template <class T>
uint32_t
TokenFactoryScopeC<T>::translate( const uint32_t & process,
   const uint32_t & localToken, const bool & showError ) const
{
   uint32_t global_token = 0;

   // get master process id
   uint32_t mprocess = process & VT_TRACEID_BITMASK;

   // search token mappings of process
   std::map<uint32_t, std::map<uint32_t, uint32_t> >::const_iterator
      proc_it = m_mapLocGlobToken.find( mprocess );

   // found?
   if( proc_it != m_mapLocGlobToken.end() )
   {
      // search token mapping by local token
      std::map<uint32_t, uint32_t>::const_iterator map_it =
         proc_it->second.find( localToken );

      // get global token, if found
      if( map_it != proc_it->second.end() )
         global_token = map_it->second;
   }

   // show error message, if no token translation found
   //
   if( global_token == 0 && showError )
   {
      std::cerr << ExeName << ": Error: No translation found for "
                << "local token " << localToken << " on process "
                << process << std::endl;
   }

   return global_token;
}

template <class T>
uint32_t
TokenFactoryScopeC<T>::getNextToken()
{
   return m_seqToken++;
}

#ifdef VT_MPI

template <class T>
VT_MPI_INT
TokenFactoryScopeC<T>::getPackSize()
{
   VT_MPI_INT buffer_size;

   // m_mapLocGlobToken.size()
   CALL_MPI( MPI_Pack_size( 1, MPI_UNSIGNED, MPI_COMM_WORLD, &buffer_size ) );

   // m_mapLocGlobToken
   //
   if( m_mapLocGlobToken.size() > 0 )
   {
      VT_MPI_INT size;

      std::map<uint32_t, std::map<uint32_t, uint32_t> >::const_iterator proc_it;
      for( proc_it = m_mapLocGlobToken.begin();
           proc_it != m_mapLocGlobToken.end(); proc_it++ )
      {
         // m_mapLocGlobToken[].first + m_mapLocGlobToken[].second.size()
         //
         CALL_MPI( MPI_Pack_size( 2, MPI_UNSIGNED, MPI_COMM_WORLD, &size ) );
         buffer_size += size;

         if( proc_it->second.size() > 0 )
         {
            // m_mapLocGlobToken[].second
            //
            CALL_MPI( MPI_Pack_size( (VT_MPI_INT)proc_it->second.size() * 2,
                                     MPI_UNSIGNED, MPI_COMM_WORLD, &size ) );
            buffer_size += size;
         }
      }
   }

   return buffer_size;
}

template <class T>
void
TokenFactoryScopeC<T>::pack( char *& buffer, const VT_MPI_INT & bufferSize,
   VT_MPI_INT & bufferPos )
{
   // m_mapLocGlobToken.size()
   //
   uint32_t proc_map_size = m_mapLocGlobToken.size();
   CALL_MPI( MPI_Pack( &proc_map_size, 1, MPI_UNSIGNED, buffer, bufferSize,
                       &bufferPos, MPI_COMM_WORLD ) );

   // m_mapLocGlobToken
   //
   if( proc_map_size > 0 )
   {
      std::map<uint32_t, std::map<uint32_t, uint32_t> >::const_iterator proc_it;
      for( proc_it = m_mapLocGlobToken.begin();
           proc_it != m_mapLocGlobToken.end(); proc_it++ )
      {
         // m_mapLocGlobToken[].first
         //
         uint32_t proc = proc_it->first;
         CALL_MPI( MPI_Pack( &proc, 1, MPI_UNSIGNED, buffer, bufferSize,
                             &bufferPos, MPI_COMM_WORLD ) );

         // m_mapLocGlobToken[].second.size()
         //
         uint32_t token_map_size = proc_it->second.size();
         CALL_MPI( MPI_Pack( &token_map_size, 1, MPI_UNSIGNED, buffer,
                             bufferSize, &bufferPos, MPI_COMM_WORLD ) );

         // m_mapLocGlobToken[].second
         //
         if( token_map_size > 0 )
         {
            uint32_t * token_map_firsts = new uint32_t[token_map_size];
            uint32_t * token_map_seconds = new uint32_t[token_map_size];

            std::map<uint32_t, uint32_t>::const_iterator tk_it;
            uint32_t i;

            for( tk_it = proc_it->second.begin(), i = 0;
                 tk_it != proc_it->second.end(), i < token_map_size;
                 tk_it++, i++ )
            {
               token_map_firsts[i] = tk_it->first;
               token_map_seconds[i] = tk_it->second;
            }

            CALL_MPI( MPI_Pack( token_map_firsts, (VT_MPI_INT)token_map_size,
                                MPI_UNSIGNED, buffer, bufferSize, &bufferPos,
                                MPI_COMM_WORLD ) );
            CALL_MPI( MPI_Pack( token_map_seconds, (VT_MPI_INT)token_map_size,
                                MPI_UNSIGNED, buffer, bufferSize, &bufferPos,
                                MPI_COMM_WORLD ) );

            delete [] token_map_firsts;
            delete [] token_map_seconds;
         }
      }
   }
}

template <class T>
void
TokenFactoryScopeC<T>::unpack( char *& buffer, const VT_MPI_INT & bufferSize,
   VT_MPI_INT & bufferPos )
{
   // m_mapLocGlobToken.size()
   //
   uint32_t proc_map_size;
   CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, &proc_map_size, 1,
                         MPI_UNSIGNED, MPI_COMM_WORLD ) );

   // m_mapLocGlobToken
   //
   if( proc_map_size > 0 )
   {
      for( uint32_t i = 0; i < proc_map_size; i++ )
      {
         // m_mapLocGlobToken[].first
         //
         uint32_t proc;
         CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, &proc, 1,
                               MPI_UNSIGNED, MPI_COMM_WORLD ) );

         // m_mapLocGlobToken[].second.size()
         //
         uint32_t token_map_size;
         CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos, &token_map_size,
                               1, MPI_UNSIGNED, MPI_COMM_WORLD ) );

         // m_mapLocGlobToken[].second
         //
         if( token_map_size > 0 )
         {
            uint32_t * token_map_firsts = new uint32_t[token_map_size];
            uint32_t * token_map_seconds = new uint32_t[token_map_size];

            CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos,
                                  token_map_firsts, (VT_MPI_INT)token_map_size,
                                  MPI_UNSIGNED, MPI_COMM_WORLD ) );
            CALL_MPI( MPI_Unpack( buffer, bufferSize, &bufferPos,
                                  token_map_seconds, (VT_MPI_INT)token_map_size,
                                  MPI_UNSIGNED, MPI_COMM_WORLD ) );

            // set token translations for process
            //
            for( uint32_t j = 0; j < token_map_size; j++ )
               setTranslation( proc, token_map_firsts[j], token_map_seconds[j] );

            delete [] token_map_firsts;
            delete [] token_map_seconds;
         }
      }
   }
}

#endif // VT_MPI

#endif // _VT_UNIFY_TKFAC_SCOPE_HH_
