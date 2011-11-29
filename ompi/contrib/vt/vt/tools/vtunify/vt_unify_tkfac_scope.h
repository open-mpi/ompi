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

#ifndef _VT_UNIFY_TKFAC_SCOPE_H_
#define _VT_UNIFY_TKFAC_SCOPE_H_

#include "vt_unify.h"

#include <algorithm>
#include <iostream>
#include <map>
#include <set>

//
// TokenFactoryScopeI interface class
//
class TokenFactoryScopeI
{
public:

   // constructor
   TokenFactoryScopeI() {}

   // destructor
   virtual ~TokenFactoryScopeI() {}

   // create global definition
   virtual uint32_t create( const void * localDef ) = 0;

   // set token translation for process
   virtual void setTranslation( const uint32_t & process,
                   const uint32_t & localToken,
                   const uint32_t & globalToken ) = 0;

   // translate local to global token
   virtual uint32_t translate( const uint32_t & process,
                       const uint32_t & localToken,
                       const bool & showError = true ) const = 0;

   // get next unused global token
   virtual uint32_t getNextToken() = 0;

#ifdef VT_MPI

   // get size needed to pack token translation map
   virtual VT_MPI_INT getPackSize() = 0;

   // pack token translations into a buffer
   virtual void pack( char *& buffer, const VT_MPI_INT & bufferSize,
                   VT_MPI_INT & bufferPos ) = 0;

   // unpack token translations from a buffer
   virtual void unpack( char *& buffer, const VT_MPI_INT & bufferSize,
                   VT_MPI_INT & bufferPos ) = 0;

#endif // VT_MPI

};

//
// TokenFactoryScopeC template class
//
template <class T>
class TokenFactoryScopeC : public TokenFactoryScopeI
{
public:

   // constructor
   TokenFactoryScopeC( std::set<T> * _globdefs, const uint32_t & _tkoffs = 1 );

   // destructor
   ~TokenFactoryScopeC();

   // create global definition
   uint32_t create( const void * localDef );

   // set token translation for process
   inline void setTranslation( const uint32_t & process,
                  const uint32_t & localToken, const uint32_t & globalToken );

   // translate local to global token
   inline uint32_t translate( const uint32_t & process,
                      const uint32_t & localToken,
                      const bool & showError = true ) const;

   // get next unused global token
   inline uint32_t getNextToken();

#ifdef VT_MPI

   // get size needed to pack token translation map
   VT_MPI_INT getPackSize();

   // pack token translations into a buffer
   void pack( char *& buffer, const VT_MPI_INT & bufferSize,
              VT_MPI_INT & bufferPos );

   // unpack token translations from a buffer
   void unpack( char *& buffer, const VT_MPI_INT & bufferSize,
                VT_MPI_INT & bufferPos );

#endif // VT_MPI

private:

   // map process id <-> local/global token
   std::map<uint32_t, std::map<uint32_t, uint32_t> > m_mapLocGlobToken;

   // pointer to target global definitions
   std::set<T> * m_globDefs;

   // sequential global token
   uint32_t m_seqToken;

};

#include "vt_unify_tkfac_scope.hh"

#endif // _VT_UNIFY_TKFAC_SCOPE_H_
