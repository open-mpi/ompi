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

#ifndef _VT_WRAPPER_H_
#define _VT_WRAPPER_H_

#include <iostream>
#include <fstream>
#include <sstream>
#include <map>
#include <string>
#include <vector>
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>

#include "vt_inttypes.h"

#include "util/installdirs.h"

// languages types
//
typedef enum { LANG_CC, LANG_CXX, LANG_F77, LANG_F90 } LangTypeT;

// instrumentation types
//
typedef enum { INST_TYPE_COMPINST = 0x1, // auto. instr. by compiler
               INST_TYPE_MANUAL   = 0x2, // manual instr. by VT API
               INST_TYPE_DYNINST  = 0x4, // binary instrumentation by Dyninst
               INST_TYPE_TAUINST  = 0x8  // auto. source code instr. by TAU
} InstTypeT;

//
// Wrapper class
//
class Config;
class Wrapper
{
public:

   // contructor
   Wrapper();

   // destructor
   ~Wrapper();

   bool readDataFile( void );
   bool readEnvironmentVars( void );
   bool parseCommandLine( int argc, char ** argv );
   int  run( void );

private:

   void showVersion( void );
   void showUsageText( void );

   int executeOrShowCommand( std::string & cmd );
   std::vector<std::string> getIncFilesFromTabFile( void );

   Config * m_pConfig;

};

//
// Config class
//
class Config
{
   friend class Wrapper;

public:

   // contructor
   Config();

   // destructor
   ~Config();

   bool setLanguage( const LangTypeT lang );

   void compiler_setCmd( const std::string cmd );
   void compiler_addArg( const std::string arg );
   void compiler_addLib( const std::string lib );
   void compiler_addSrcFile( const std::string file );

   void opari_setRcFile( const std::string file );
   void opari_setTabFile( const std::string file );
   void opari_addArg( const std::string arg );

   void tauinst_addArg( const std::string arg );
   void tauinst_addParseArg( const std::string arg );

   void setUsesMpi( const bool set, const bool ovwrt = false );
   void setUsesThreads( const bool set, const bool ovwrt = false );
   void setUsesOpenMP( const bool set, const bool ovwrt = false );

   void setInstAvail( const InstTypeT type ) {
      m_iInstAvail |= type; }
   bool setInstAvail( const std::string type );
   bool isInstAvail( InstTypeT type ) {
      return (m_iInstAvail & type); }
   bool setInstType( const InstTypeT type );
   bool setInstType( const std::string type );

protected:

   std::string m_sVT_Version;           // VT version

   std::string m_sVT_IncDir;            // VT's include directory
   std::string m_sVT_LibDir;            // VT's library directory

   std::string m_sVT_SeqLib;            // VT-library for sequential programs
   std::string m_sVT_MpiLib;            // VT-library for MPI programs
   std::string m_sVT_MtLib;             // VT-library for multithreading programs
   std::string m_sVT_HybLib;            // VT-library for hybrid (MPI/Threads)
                                        // programs
   std::string m_sVT_PompLib;           // VT's POMP library
   std::string m_sVT_DynAttLib;         // VT's Dyninst attach library

   std::string m_sComp_CmdEnv;          // compiler command env. name
   std::string m_sComp_FlagsEnv;        // compiler flags env. name
   std::string m_sComp_Cmd;             // compiler command
   std::string m_sComp_Args;            // compiler arguments
   std::string m_sComp_Flags;           // compiler flags
   std::string m_sComp_LdFlags;         // linker flags
   std::string m_sComp_FDFlag;          // flag to define preprocessor macro
   std::string m_sComp_InstFlags;       // compiler instrumentation flags
   std::string m_sComp_Libs;            // libraries to link
   std::vector<std::string>
      m_vecComp_SrcFiles;               // compiler input source files
   std::vector<std::string>
      m_vecComp_ObjFiles;               // compiler output object files

   std::string m_sOpari_Cmd;            // OPARI command
   std::string m_sOpari_Args;           // OPARI arguments
   std::string m_sOpari_RcFile;         // OPARI's rc file
   std::pair<std::string, std::string>
      m_sOpari_TabFile;                 // OPARI's table source file 
   std::string m_sOpari_TabCompCmd;     // compiler command for OPARI's table file
   std::string m_sOpari_TabCompFlags;   // compiler flags for OPARI's table file

   std::string m_sCompInst_Flags;       // compiler flags to enable instrumentation
   std::string m_sDynInst_Flags;        // compiler flags to produce debugging information
                                        // (needed for binary instrumentation by Dyninst)

   std::string m_sTauInst_Cmd;          // TAU instrumentor command
   std::string m_sTauInst_Args;         // TAU instrumentor arguments
   std::string m_sTauInst_ParseCmd;     // PDT source code parser command
   std::string m_sTauInst_ParseArgs;    // PDT parser arguments

   LangTypeT   m_eLangType;             // language type
   InstTypeT   m_eInstType;             // instrumentation type
                                        // (e.g. compinst,manual,...)
   int         m_iInstAvail;            // bitmask for available instr.-types
   bool        m_bBeVerbose;            // FLAG: be verbose ?
   bool        m_bCompOnly;             // FLAG: compile only ?
   bool        m_bOutfileGiven;         // FLAG: output file given ?
   bool        m_bUsesMpi;              // FLAG: uses MPI ?
   bool        m_bUsesThreads;          // FLAG: uses Threads ?
   bool        m_bUsesOpenMP;           // FLAG: uses OpenMP ? (use OPARI)
   bool        m_bKeepOpariRcFile;      // FLAG: don't delete OPARI's rc file ?
   bool        m_bShow;                 // FLAG: show compiler/linker flags ?

};

#endif // _VT_WRAPPER_H_
