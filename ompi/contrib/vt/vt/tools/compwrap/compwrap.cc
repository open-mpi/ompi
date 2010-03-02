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

#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>

#include "util/util.h"
#include "util/installdirs.h"

#include "compwrap.h"

static bool ReadDataFile();
static bool ReadEnvironmentVars();
static bool ParseCommandLine( int argc, char ** argv );

#ifdef WRAP_LANG_CC
#define WRAP_LANG_SUFFIX "cc"
static const std::string ExeName = "vtcc";
#elif defined(WRAP_LANG_CXX)
#define WRAP_LANG_SUFFIX "cxx"
static const std::string ExeName = "vtcxx";
#elif defined(WRAP_LANG_F77)
#define WRAP_LANG_SUFFIX "f77"
static const std::string ExeName = "vtf77";
#elif defined(WRAP_LANG_F90)
#define WRAP_LANG_SUFFIX "f90"
static const std::string ExeName = "vtf90";
#else
#define WRAP_LANG_SUFFIX "unknown"
static const std::string ExeName = "";
#error Macro WRAP_LANG_* not defined or invalid
#endif

Wrapper * theWrapper;   // instance of class Wrapper

int
main( int argc, char ** argv )
{
   int rc;

   // create instance of wrapper (initialize)
   theWrapper = new Wrapper();
   assert( theWrapper );

   // set opari's table file
   Properties.opari_tab_file =
      std::make_pair(std::string("opari.tab.c"), std::string("opari.tab.o"));

   // read wrapper's data file
   if( !ReadDataFile() )
      return 1;

   // read environment variables
   if( !ReadEnvironmentVars() )
      return 1;

   // parse command line
   if( !ParseCommandLine( argc, argv ) )
      return 1;

   // start compiling/linking
   rc = theWrapper->run();

   delete theWrapper;
   return rc;
}

static bool
ReadDataFile()
{
   bool error = false;

   const unsigned int keys_num = 25;
   const std::string keys[] = {
      "version", "language", "compiler_env", "compiler_flags_env",
      "compiler", "compiler_flags", "linker_flags", "libs", "includedir",
      "libdir", "opari_bin", "opari_tab_compiler", "opari_tab_compiler_flags",
      "pmpilib", "fmpilib", "dynattlib", "compiler_iflags_gnu",
      "compiler_iflags_intel", "compiler_iflags_pathscale",
      "compiler_iflags_pgi", "compiler_iflags_sun", "compiler_iflags_xl",
      "compiler_iflags_ftrace", "inst_avail", "inst_default"
   };
   
   std::string data_file =
     std::string(vt_installdirs_get(VT_INSTALLDIR_DATADIR)) + "/" +
     ExeName + "-wrapper-data.txt";

   std::ifstream in( data_file.c_str() );
   if( !in )
   {
      std::cerr << ExeName << ": error: could not open "
		<< data_file << std::endl;
      return false;
   }

   char buffer[1024];
   std::string line;
   unsigned int line_no = 0;
   unsigned int key_idx = 0;

   while( key_idx < keys_num 
	  && in.getline( buffer, sizeof(buffer) - 1 ) )
   {
      line_no++;

      if( buffer[0] == '#' || buffer[0] == '\n' || buffer[0] == '\0' )
	 continue;
      if( buffer[strlen(buffer)-1] == '\n' )
	 buffer[strlen(buffer)-1] = '\0';

      line = buffer;

      int valpos = (int)line.find( "=" );
      std::string key;
      std::string value;

      if( valpos < 1 )
      {
	 std::cerr << ExeName << ": "
		   << data_file << ":" << line_no << ": "
		   << "could not be parsed" << std::endl;
	 error = true;
	 break;
      }

      key = line.substr( 0, valpos );
      value = line.substr( valpos+1 );

      if( key.compare( keys[key_idx++] ) != 0 )
      {
	 std::cerr << ExeName << ": "
		   << data_file << ":" << line_no << ": "
		   << "unexpected key '"
		   << key << "'" << std::endl;
	 error = true;
	 break;
      }
            
      if( key.compare( "version" ) == 0 )
      {
	 Properties.version = value;
      }
      else if( key.compare( "language" ) == 0 )
      {
	 Properties.language = value;
      }
      else if( key.compare( "compiler_env" ) == 0 )
      {
	 Properties.comp_cmd_env = value;
      }
      else if( key.compare( "compiler_flags_env" ) == 0 )
      {
	 Properties.comp_flags_env = value;
      }
      else if( key.compare( "compiler" ) == 0 )
      {
	 theWrapper->comp_setCmd( value );
      }
      else if( key.compare( "compiler_flags" ) == 0 )
      {
	 Properties.comp_flags = value;
      }
      else if( key.compare( "linker_flags" ) == 0 )
      {
	 Properties.comp_ldflags = value;
      }
      else if( key.compare( "libs" ) == 0 )
      {
	 Properties.comp_ulibs = value;
      }
      else if( key.compare( "includedir" ) == 0 )
      {
	 if( value.length() > 0 )
	 {
	    char* includedir = vt_installdirs_expand( value.c_str() );
	    if( includedir )
	    {
	       Properties.includedir = "-I" + std::string(includedir);
	       free( includedir );
	    }
	    else
	    {
	       std::cerr << ExeName << ": "
		         << data_file << ":" << line_no << ": "
		         << "could not be parsed" << std::endl;
	       error = true;
	    }
	 }
      }
      else if( key.compare( "libdir" ) == 0 )
      {
	 if( value.length() > 0 )
	 {
	    char* libdir = vt_installdirs_expand( value.c_str() );
	    if( libdir )
	    {
	       Properties.libdir = "-L" + std::string(libdir);
	       free( libdir );
	    }
	    else
	    {
	       std::cerr << ExeName << ": "
		         << data_file << ":" << line_no << ": "
		         << "could not be parsed" << std::endl;
	       error = true;
	    }
	 }
      }
      else if( key.compare( "opari_bin" ) == 0 )
      {
	 Properties.opari_cmd = value;
      }
      else if( key.compare( "opari_tab_compiler" ) == 0 )
      {
	 Properties.opari_tab_comp_cmd = value;
      }
      else if( key.compare( "opari_tab_compiler_flags" ) == 0 )
      {
	 Properties.opari_tab_comp_flags = value;
      }
      else if( key.compare( "pmpilib" ) == 0 )
      {
	 Properties.pmpilib = value;
      }
      else if( key.compare( "fmpilib" ) == 0 )
      {
	 Properties.fmpilib = value;
      }
      else if( key.compare( "dynattlib" ) == 0 )
      {
	 Properties.dynattlib = value;
      }
      else if( key.compare( "compiler_iflags_gnu" ) == 0 )
      {
	 Properties.iflags_gnu = value;
      }
      else if( key.compare( "compiler_iflags_intel" ) == 0 )
      {
	 Properties.iflags_intel = value; 
      }
      else if( key.compare( "compiler_iflags_pathscale" ) == 0 )
      {
         Properties.iflags_pathscale = value;
      }
      else if( key.compare( "compiler_iflags_pgi" ) == 0 )
      {
	 Properties.iflags_pgi = value;
      }
      else if( key.compare( "compiler_iflags_sun" ) == 0 )
      {
	 Properties.iflags_sun = value;
      }
      else if( key.compare( "compiler_iflags_xl" ) == 0 )
      {
	 Properties.iflags_xl = value;
      }
      else if( key.compare( "compiler_iflags_ftrace" ) == 0 )
      {
	 Properties.iflags_ftrace = value;
      }
      else if( key.compare( "inst_avail" ) == 0 )
      {
	 char cvalue[100];
	 strncpy( cvalue, value.c_str(), sizeof(cvalue) - 1 );

	 char * token = strtok( cvalue, " " );
	 if( !token )
	 {
	    std::cerr << ExeName << ": "
		      << data_file << ":" << line_no << ": "
		      << "could not be parsed" << std::endl;
	    error = true;
	    break;   
	 }
	 
	 do
	 {
	    if( strcmp( token, "gnu" ) == 0 )
	       Properties.inst_avail |= INST_TYPE_GNU;
	    else if( strcmp( token, "intel" ) == 0 )
	       Properties.inst_avail |= INST_TYPE_INTEL;
	    else if( strcmp( token, "pathscale" ) == 0 )
	       Properties.inst_avail |= INST_TYPE_PATHSCALE;
	    else if( strcmp( token, "pgi" ) == 0 )
	       Properties.inst_avail |= INST_TYPE_PGI;
	    else if( strcmp( token, "sun" ) == 0 )
	       Properties.inst_avail |= INST_TYPE_SUN;
	    else if( strcmp( token, "xl" ) == 0 )
	       Properties.inst_avail |= INST_TYPE_XL;
	    else if( strcmp( token, "ftrace" ) == 0 )
	       Properties.inst_avail |= INST_TYPE_FTRACE;
	    else if( strcmp( token, "dyninst" ) == 0 )
	       Properties.inst_avail |= INST_TYPE_DYNINST;
	    else if( strcmp( token, "manual" ) == 0 )
	       Properties.inst_avail |= INST_TYPE_MANUAL;
	    else if( strcmp( token, "pomp" ) == 0 )
	       Properties.inst_avail |= INST_TYPE_POMP;
	    else
	    {
	       std::cerr << ExeName << ": "
			 << data_file << ":" << line_no << ": "
			 << "unknown instrumentation type '"
			 << token << "'" << std::endl;
	       error = true;
	       break;  
	    }
	 } while( ( token = strtok( 0, " " ) ) );
	 if( error ) break;
      }
      else if( key.compare( "inst_default" ) == 0 )
      {
	 if( strcmp( value.c_str(), "gnu" ) == 0 )
	    error = !theWrapper->setInstType( INST_TYPE_GNU );
	 else if( strcmp( value.c_str(), "intel" ) == 0 )
	    error = !theWrapper->setInstType( INST_TYPE_INTEL );
	 else if( strcmp( value.c_str(), "pathscale" ) == 0 )
	    error = !theWrapper->setInstType( INST_TYPE_PATHSCALE );
	 else if( strcmp( value.c_str(), "pgi" ) == 0 )
	    error = !theWrapper->setInstType( INST_TYPE_PGI );
	 else if( strcmp( value.c_str(), "sun" ) == 0 )
	    error = !theWrapper->setInstType( INST_TYPE_SUN );
	 else if( strcmp( value.c_str(), "xl" ) == 0 )
	    error = !theWrapper->setInstType( INST_TYPE_XL );
	 else if( strcmp( value.c_str(), "ftrace" ) == 0 )
	    error = !theWrapper->setInstType( INST_TYPE_FTRACE );
	 else if( strcmp( value.c_str(), "dyninst" ) == 0 )
	    error = !theWrapper->setInstType( INST_TYPE_DYNINST );
	 else if( strcmp( value.c_str(), "manual" ) == 0 )
	    error = !theWrapper->setInstType( INST_TYPE_MANUAL );
	 else if( strcmp( value.c_str(), "pomp" ) == 0 )
	    error = !theWrapper->setInstType( INST_TYPE_POMP );
	 else
	 {
	    std::cerr << ExeName << ": "
		      << data_file << ":" << line_no << ": "
		      << "unknown instrumentation type '"
		      << value << "'" << std::endl;
	    error = true;
	    break;   
	 }

	 if( error )
	 {
	    std::cerr << ExeName << ": "
		      << data_file << ":" << line_no << ": "
		      << "instrumentation type '" << value << "' "
		      << "not supported" << std::endl;
	    break;
	 }
      }
      else
      {
	 std::cerr << ExeName << ": "
		   << data_file << ":" << line_no << ": "
		   << "could not be parsed" << std::endl;
	 error = true;
	 break; 
      }
   }
   
   in.close();

   if( !error && key_idx < keys_num )
   {
      std::cerr << ExeName << ": "
		<< data_file << ": "
		<< "unexpected end of file" << std::endl;
      return false;
   }

   return !error;
}

static bool
ReadEnvironmentVars()
{
   char * env;

   // read environment var. for compiler command
   // (VT_<CC|CXX|F77|F90>)
   //
   env = getenv( Properties.comp_cmd_env.c_str() );
   if( env ) theWrapper->comp_setCmd( env );

   // read environment var. for extra compiler flags
   // (VT_<C|CXX|F|FC>FLAGS)
   env = getenv( Properties.comp_flags_env.c_str() );
   if( env ) Properties.comp_flags = env;

   // read environment var. for extra linker flags
   // (VT_LDFLAGS)
   env = getenv( "VT_LDFLAGS" );
   if( env ) Properties.comp_ldflags = env;

   // read environment var. for extra libs
   // (VT_LIBS)
   env = getenv( "VT_LIBS" );
   if( env ) theWrapper->comp_addULib( env );

   // read environment var. for instrumentation type
   // (VT_INST)
   //
   env = getenv( "VT_INST" );
   if( env )
   {
      bool error = false;
      std::string senv = env;
      
      if( senv.compare("gnu") == 0 )
	 error = !theWrapper->setInstType( INST_TYPE_GNU );
      else if( senv.compare("intel") == 0 )
	 error = !theWrapper->setInstType( INST_TYPE_INTEL );
      else if( senv.compare("pathscale") == 0 )
	 error = !theWrapper->setInstType( INST_TYPE_PATHSCALE );
      else if( senv.compare("pgi") == 0 )
	 error = !theWrapper->setInstType( INST_TYPE_PGI );
      else if( senv.compare("sun") == 0 )
	 error = !theWrapper->setInstType( INST_TYPE_SUN );
      else if( senv.compare("xl") == 0 )
	 error = !theWrapper->setInstType( INST_TYPE_XL );
      else if( senv.compare("ftrace") == 0 )
	 error = !theWrapper->setInstType( INST_TYPE_FTRACE );
      else if( senv.compare("manual") == 0 )
	 error = !theWrapper->setInstType( INST_TYPE_MANUAL );
      else if( senv.compare("pomp") == 0 )
	 error = !theWrapper->setInstType( INST_TYPE_POMP );
      else if( senv.compare("dyninst") == 0 )
	 error = !theWrapper->setInstType( INST_TYPE_DYNINST );
      else
      {
	 std::cerr << ExeName << ": error: VT_INST: "
		   << "unknown instrumentation type '"
		   << senv << "'" << std::endl;
	 return false;
      }

      if( error )
      {
	 std::cerr << ExeName << ": error: VT_INST: "
		   << "instrumentation type '" << senv << "' "
		   << "not supported" << std::endl;
	 return false;
      }
   }

   return true;
}

static bool
ParseCommandLine( int argc, char ** argv )
{
   bool addlibs = false;
   int i;
   std::string arg;

   for( i = 1; i < argc; i++ )
   {
      arg = argv[i];

      //
      // we also accept "--vt:" - modify "--vt:" to "-vt:"
      //
      if( arg.compare(0,5,"--vt:") == 0 )
      {
         arg.erase(0,1);
      }

      //
      // -vt:help 
      //
      if( arg.compare("-vt:help") == 0 )
      {
	 theWrapper->showUsageText();
	 exit(0);
      }
      //
      // -vt:version
      //
      if( arg.compare("-vt:version") == 0 )
      {
	 theWrapper->showVersion();
	 exit(0); 
      }
      //
      // -vt:showme
      //
      else if( arg.compare("-vt:showme") == 0 )
      {
	 theWrapper->setShowme( true );
      }
      //
      // -vt:showme_compile
      //
      else if( arg.compare("-vt:showme_compile") == 0 )
      {
	 theWrapper->setShowmeCompile( true );
      }
      //
      // -vt:showme_link
      //
      else if( arg.compare("-vt:showme_link") == 0 )
      {
	 theWrapper->setShowmeLink( true );
      }
      //
      // -vt:inst <type>
      //
      else if( arg.compare("-vt:inst") == 0 )
      {
	 if( i == argc - 1 )
	 {
	    std::cerr << ExeName << ": <type> expected -- -vt:inst"
		      << std::endl;
	    return false;
	 }

	 bool error = false;

	 arg = argv[++i];

	 if( arg.compare("gnu") == 0 )
	    error = !theWrapper->setInstType( INST_TYPE_GNU );
	 else if( arg.compare("intel") == 0 )
	    error = !theWrapper->setInstType( INST_TYPE_INTEL );
	 else if( arg.compare("pathscale") == 0 )
	    error = !theWrapper->setInstType( INST_TYPE_PATHSCALE );
	 else if( arg.compare("pgi") == 0 )
	    error = !theWrapper->setInstType( INST_TYPE_PGI );
	 else if( arg.compare("sun") == 0 )
	    error = !theWrapper->setInstType( INST_TYPE_SUN );
	 else if( arg.compare("xl") == 0 )
	    error = !theWrapper->setInstType( INST_TYPE_XL );
	 else if( arg.compare("ftrace") == 0 )
	    error = !theWrapper->setInstType( INST_TYPE_FTRACE );
	 else if( arg.compare("manual") == 0 )
	    error = !theWrapper->setInstType( INST_TYPE_MANUAL );
	 else if( arg.compare("pomp") == 0 )
	    error = !theWrapper->setInstType( INST_TYPE_POMP );
	 else if( arg.compare("dyninst") == 0 )
	    error = !theWrapper->setInstType( INST_TYPE_DYNINST );
	 else
	 {
	    std::cerr << ExeName << ": unknown instrumentation type '"
		      << arg << "'" << std::endl;
	    return false;
	 }

	 if( error )
	 {
	    std::cerr << ExeName << ": instrumentation type '"
		      << arg << "' not supported" << std::endl;
	    return false;
	 }
      }
      //
      // -vt:seq
      //
      else if( arg.compare("-vt:seq") == 0 )
      {
	 theWrapper->setUsesMPI( false, true, true );
	 theWrapper->setUsesOMP( false, true, true );
      }
      //
      // -vt:mpi
      //
      else if( arg.compare("-vt:mpi") == 0 )
      {
	 theWrapper->setUsesMPI( true, true, true );
	 theWrapper->setUsesOMP( false, true, true );
      }
      //
      // -vt:omp
      //
      else if( arg.compare("-vt:omp") == 0 )
      {
	 theWrapper->setUsesMPI( false, true, true );
	 theWrapper->setUsesOMP( true, true, true );
      }
      //
      // -vt:hyb
      //
      else if( arg.compare("-vt:hyb") == 0 )
      {
	 theWrapper->setUsesMPI( true, true, true );
	 theWrapper->setUsesOMP( true, true, true );
      }
      //
      // openmp flag
      //
      else if( arg.compare( "-openmp" ) == 0
	       || arg.compare( "-xopenmp" ) == 0
	       || arg.compare( "-Popenmp" ) == 0
	       || arg.compare( "-mp" ) == 0
	       || arg.compare( 0, 4, "-mp=" ) == 0
	       || arg.compare( "-qsmp=omp" ) == 0 )
      {
	 theWrapper->setUsesOMP( true );
	 theWrapper->comp_addArg( arg );
      }
   }

   for( i = 1; i < argc; i++ )
   {
      arg = argv[i];

      //
      // we also accept "--vt:" - modify "--vt:" to "-vt:"
      //
      if( arg.compare(0,5,"--vt:") == 0 )
      {
         arg.erase(0,1);
      }
      //
      // escape spaces and double quotes
      //
      size_t found = arg.find_first_of(" \"");
      while(found!=std::string::npos)
      {
         arg.insert(found,"\\");
         found = arg.find_first_of(" \"",found+2);
      }

      //
      // -vt:help, -vt:version, -vt:showme, -vt:showme_compile,
      // -vt:showme_link, -vt:seq, -vt:mpi, -vt:omp, -vt:hyb,
      // openmp flag
      // (processed above; ignore here)
      //
      if( arg.compare("-vt:help") == 0 
	  || arg.compare("-vt:version") == 0
	  || arg.compare("-vt:showme") == 0
	  || arg.compare("-vt:showme_compile") == 0
	  || arg.compare("-vt:showme_link") == 0
	  || arg.compare("-vt:seq") == 0
	  || arg.compare("-vt:mpi") == 0
	  || arg.compare("-vt:omp") == 0
	  || arg.compare("-vt:hyb") == 0
	  || arg.compare( "-openmp" ) == 0
	  || arg.compare( "-xopenmp" ) == 0
	  || arg.compare( "-Popenmp" ) == 0
	  || arg.compare( "-mp" ) == 0
	  || arg.compare( 0, 4, "-mp=" ) == 0
	  || arg.compare( "-qsmp=omp" ) == 0 )
      {
	 // do nothing
      }
      else if( arg.compare("-vt:inst") == 0 )
      {
	 // skip next argument
	 i++;
      }
      //
      // -vt:verbose
      //
      else if( arg.compare("-vt:verbose") == 0 )
      {
	 theWrapper->setBeVerbose( true );
      }
      //
      // -vt:<cc|cxx|f77|f90> <cmd>
      //
      else if( arg.compare("-vt:"WRAP_LANG_SUFFIX) == 0 )
      {
	 if( i == argc - 1 )
	 {
	    std::cerr << ExeName << ": <cmd> expected -- -vt:"WRAP_LANG_SUFFIX
		      << std::endl;
	    return false;
	 }
	 
	 theWrapper->comp_setCmd( argv[++i] );
      }
      //
      // -vt:opari <args>
      //
      else if( arg.compare("-vt:opari") == 0 )
      {
	 if( i == argc - 1 )
	 {
	    std::cerr << ExeName << ": <args> expected -- -vt:opari"
		      << std::endl;
	    return false;
	 }

	 int len = strlen(argv[i+1])+1;
	 char * args = new char[len];
	 strncpy( args, argv[++i], len );
	 char * token = strtok( args, " " );

	 do
	 {
	    if( strcmp( token, "-table" ) == 0 )
	    {
	       token = strtok( 0, " " );
	       if( !token )
	       {
		  std::cerr << ExeName << ": <tabfile> expected -- -table"
			    << std::endl;
		  free( args );
		  return false;
	       }
	       
	       theWrapper->opari_setTabFile( token );
	    }
	    else
	    {
	       theWrapper->opari_addArg( token );
	    }
	 } while( ( token = strtok( 0, " " ) ) );

	 free( args );
      }
      //
      // -vt:*  -> unknown wrapper argument
      //
      else if( arg.compare( 0, 4, "-vt:" ) == 0 )
      {
	 std::cerr << ExeName << ": unknown option -- "
		   << arg << std::endl;
	 return false; 
      }
      //
      // source file
      //
      else if( ( arg.length() >= 2 
		 && arg.compare( arg.length() - 2, 2, ".c" ) == 0 )
	       || ( arg.length() >= 2
		    && arg.compare( arg.length() - 2, 2, ".C" ) == 0 )
	       || ( arg.length() >= 3
		    && arg.compare( arg.length() - 3, 3, ".cc" ) == 0 )
	       || ( arg.length() >= 3
		    && arg.compare( arg.length() - 3, 3, ".CC" ) == 0 )
	       || ( arg.length() >= 4
		    && arg.compare( arg.length() - 4, 4, ".cpp" ) == 0 )
	       || ( arg.length() >= 4
		    && arg.compare( arg.length() - 4, 4, ".CPP" ) == 0 )
	       || ( arg.length() >= 4
		    && arg.compare( arg.length() - 4, 4, ".cxx" ) == 0 )
	       || ( arg.length() >= 4
		    && arg.compare( arg.length() - 4, 4, ".CXX" ) == 0 )
	       || ( arg.length() >= 2
		    && arg.compare( arg.length() - 2, 2, ".f" ) == 0 )
	       || ( arg.length() >= 2
		    && arg.compare( arg.length() - 2, 2, ".F" ) == 0 )
	       || ( arg.length() >= 4
		    && arg.compare( arg.length() - 4, 4, ".f77" ) == 0 )
	       || ( arg.length() >= 4
		    && arg.compare( arg.length() - 4, 4, ".F77" ) == 0 )
	       || ( arg.length() >= 4
		    && arg.compare( arg.length() - 4, 4, ".f90" ) == 0 )
	       || ( arg.length() >= 4
		    && arg.compare( arg.length() - 4, 4, ".F90" ) == 0 )
	       || ( arg.length() >= 4
		    && arg.compare( arg.length() - 4, 4, ".f95" ) == 0 )
	       || ( arg.length() >= 4
		    && arg.compare( arg.length() - 4, 4, ".F95" ) == 0 ) )
      {
	 if( ( !theWrapper->showmeCompile() && !theWrapper->showmeLink() )
	     && ( theWrapper->usesOMP()
		  || theWrapper->getInstType() == INST_TYPE_POMP ) )
	 {
	    theWrapper->opari_addSrcFile( arg );
	 }
	 else
	 {
	    theWrapper->comp_addArg( arg );
	 }
      }
      //
      // -c
      //
      else if( arg.compare("-c") == 0 )
      {
	 theWrapper->setComponly( true );
	 theWrapper->comp_addArg( arg );
      }
      //
      // -l<mpilib>
      //
      else if( arg.compare( 0, 5, "-lmpi" ) == 0
	       || arg.compare( 0, 7, "-lmtmpi" ) == 0
	       || arg.compare( 0, 7, "-lhpmpi" ) == 0
	       || arg.compare( 0, 7, "-lscmpi" ) == 0 )
      {
	 theWrapper->setUsesMPI( true );
	 theWrapper->comp_addULib( arg );
	 addlibs = true;
      }
      //
      // -l*
      //
      else if( arg.compare( 0, 2, "-l" ) == 0 )
      {
	 if( addlibs )
	    theWrapper->comp_addULib( arg );
	 else
	    theWrapper->comp_addArg( arg );
      }
      //
      // unknown argument
      //
      else
      {
	 theWrapper->comp_addArg( arg );
      }
   }

   return true;
}

//////////////////// class Wrapper ////////////////////

// public methods
//

Wrapper::Wrapper()
{
   // empty
}

Wrapper::~Wrapper()
{
   // empty
}

void
Wrapper::showVersion()
{
   std::cout << Properties.version << std::endl;
}

void
Wrapper::showUsageText()
{
   std::cout << std::endl
	     << " " << ExeName << " - " << Properties.language
	     << " compiler wrapper for VampirTrace."
	     << std::endl << std::endl
	     << " Syntax: " << ExeName << " "
	     << "[-vt:"WRAP_LANG_SUFFIX" <cmd>] "
	     << "[-vt:inst <insttype>] [-vt:<seq|mpi|omp|hyb>] "
	     << std::endl << "         "
	     << "[-vt:opari <args>]"
	     << std::endl << "         "
	     << "[-vt:verbose] "
	     << "[-vt:version] "
	     << "[-vt:showme] "
	     << "[-vt:showme_compile] "
	     << "[-vt:showme_link] ..."
	     << std::endl << std::endl;

   std::cout << "   options:"
	     << std::endl
	     << "     -vt:help            Show this help message."
	     << std::endl
	     << "     -vt:"WRAP_LANG_SUFFIX" <cmd>       ";
   if( strlen( WRAP_LANG_SUFFIX ) == 2 ) std::cout << " ";
   std::cout << "Set the underlying compiler command.";
   std::cout <<  std::endl << std::endl;

   std::cout << "     -vt:inst <insttype> Set the instrumentation type."
	     << std::endl << std::endl
	     << "      possible values:"
	     << std::endl << std::endl
	     << "       gnu               fully-automatic by GNU compiler"
	     << std::endl
	     << "       intel             ... Intel (version >= 10.x) ..."
	     << std::endl
	     << "       pathscale         ... Pathscale (version >= 3.1) ..."
	     << std::endl
	     << "       pgi               ... Portland Group (PGI) ..."
	     << std::endl
	     << "       sun               ... SUN Fortran 90 ..."
	     << std::endl
	     << "       xl                ... IBM ..."
	     << std::endl
	     << "       ftrace            ... NEC SX ..."
	     << std::endl
	     << "       manual            manual by using VampirTrace's API"
	     << std::endl
	     << "       pomp              manual by using POMP INST directives"
	     << std::endl
	     << "       dyninst           binary by using Dyninst (www.dyninst.org)"
	     << std::endl << std::endl
	     << "       default: ";
#ifdef DEFAULT_COMPINST
   if( std::string(DEFAULT_COMPINST).compare( "gnu" ) == 0 )
      std::cout << "gnu";
   else if( std::string(DEFAULT_COMPINST).compare( "intel" ) == 0 )
      std::cout << "intel";
   else if( std::string(DEFAULT_COMPINST).compare( "pathscale" ) == 0 )
      std::cout << "pathscale";
   else if( std::string(DEFAULT_COMPINST).compare( "pgi" ) == 0 )
      std::cout << "pgi";
   else if( std::string(DEFAULT_COMPINST).compare( "sun" ) == 0 )
      std::cout << "sun";
   else if( std::string(DEFAULT_COMPINST).compare( "xl" ) == 0 )
      std::cout << "xl";
   else if( std::string(DEFAULT_COMPINST).compare( "ftrace" ) == 0 )
      std::cout << "ftrace";
   else
      std::cout << "manual";
#else
   std::cout << "manual";
#endif

   std::cout << std::endl << std::endl;

   std::cout << "     -vt:opari <args>    Set options for OPARI command."
	     << std::endl
	     << "                         (see "
	     << vt_installdirs_get(VT_INSTALLDIR_DATADIR)
	     << "/doc/opari/Readme.html for more information)"
	     << std::endl << std::endl
	     << "     -vt:<seq|mpi|omp|hyb>"
	     << std::endl
	     << "                         Force application's parallelization type."
	     << std::endl
	     << "                         It's necessary, if this could not determined"
	     << std::endl
	     << "                         by underlying compiler and flags."
	     << std::endl
	     << "                         seq = sequential"
	     << std::endl
	     << "                         mpi = parallel (uses MPI)"
	     << std::endl
	     << "                         omp = parallel (uses OpenMP)"
	     << std::endl
	     << "                         hyb = hybrid parallel (MPI + OpenMP)"
	     << std::endl
	     << "                         (default: automatically determining by"
	     << std::endl
	     << "                          underlying compiler and flags)"
	     << std::endl << std::endl
	     << "     -vt:verbose         Enable verbose mode."
	     << std::endl << std::endl
	     << "     -vt:showme          Do not invoke the underlying compiler."
	     << std::endl
	     << "                         Instead, show the command line that would be"
	     << std::endl
	     << "                         executed to compile and link the program."
	     << std::endl << std::endl
	     << "     -vt:showme_compile  Do not invoke the underlying compiler."
	     << std::endl
	     << "                         Instead, show the compiler flags that would be"
	     << std::endl
	     << "                         supplied to the compiler."
	     << std::endl << std::endl
	     << "     -vt:showme_link     Do not invoke the underlying compiler."
	     << std::endl
	     << "                         Instead, show the linker flags the would be"
	     << std::endl
	     << "                         supplied to the compiler."
	     << std::endl << std::endl
	     << "     See the man page for your underlying compiler for other options that can"
	     << std::endl
	     << "     be passed through 'vt"WRAP_LANG_SUFFIX"'."
	     << std::endl << std::endl
	     << "   environment variables:"
	     << std::endl
#ifdef WRAP_LANG_CC
             << "     VT_CC "
#elif defined(WRAP_LANG_CXX)
             << "     VT_CXX"
#elif defined(WRAP_LANG_F77)
             << "     VT_F77"
#elif defined(WRAP_LANG_F90)
             << "     VT_F90"
#endif
             << "              Equivalent to '-vt:"WRAP_LANG_SUFFIX"'"
             << std::endl
	     << "     VT_INST             Equivalent to '-vt:inst'"
	     << std::endl << std::endl
	     << "     The corresponding command line options overwrites the environment"
	     << std::endl
	     << "     variables setting."
	     << std::endl << std::endl
	     << "   examples:"
	     << std::endl
	     << "     automatically instrumentation by using GNU compiler:"
	     << std::endl << std::endl
#ifdef WRAP_LANG_CC
	     << "        vtcc -vt:cc gcc -vt:inst gnu -c foo.c -o foo.o"
	     << std::endl
	     << "        vtcc -vt:cc gcc -vt:inst gnu -c bar.c -o bar.o"
	     << std::endl
	     << "        vtcc -vt:cc gcc -vt:inst gnu foo.o bar.o -o foo"
	     << std::endl << std::endl
	     << "     manually instrumentation by using VT's API:"
	     << std::endl << std::endl
	     << "        vtcc -vt:inst manual foobar.c -o foobar -DVTRACE"
#elif defined(WRAP_LANG_CXX)
	     << "        vtcxx -vt:cxx g++ -vt:inst gnu -c foo.cpp -o foo.o"
	     << std::endl
	     << "        vtcxx -vt:cxx g++ -vt:inst gnu bar.cpp -o bar.o"
	     << std::endl
	     << "        vtcxx -vt:cxx g++ -vt:inst gnu foo.o bar.o -o foo"
	     << std::endl << std::endl
	     << "     manually instrumentation by using VT's API:"
	     << std::endl << std::endl
	     << "        vtcxx -vt:inst manual foobar.cpp -o foobar -DVTRACE"
#elif defined(WRAP_LANG_F77)
	     << "        vtf77 -vt:f77 g77 -vt:inst gnu -c foo.F -o foo.o"
	     << std::endl
	     << "        vtf77 -vt:f77 g77 -vt:inst gnu bar.F -o bar.o"
	     << std::endl
	     << "        vtf77 -vt:f77 g77 -vt:inst gnu foo.o bar.o -o foo"
	     << std::endl << std::endl
	     << "     manually instrumentation by using VT's API:"
	     << std::endl << std::endl
	     << "        vtf77 -vt:inst manual foobar.F -o foobar -DVTRACE"
#elif defined(WRAP_LANG_F90)
	     << "        vtf90 -vt:f90 gfortran -vt:inst gnu -c foo.F90 -o foo.o"
	     << std::endl
	     << "        vtf90 -vt:f90 gfortran -vt:inst gnu bar.F90 -o bar.o"
	     << std::endl
	     << "        vtf90 -vt:f90 gfortran -vt:inst gnu foo.o bar.o -o foo"
	     << std::endl << std::endl
	     << "     manually instrumentation by using VT's API:"
	     << std::endl << std::endl
	     << "        vtf90 -vt:inst manual foobar.F90 -o foobar -DVTRACE"
#endif

#if defined(WRAP_LANG_F77) || defined(WRAP_LANG_F90)
	     << std::endl << std::endl
	     << "     IMPORTANT: Fortran source files instrumented by using VT's API or POMP"
	     << std::endl
	     << "                directives have to be (CPP) preprocessed."
#endif
	     << std::endl << std::endl;
}

void
Wrapper::showInfo()
{
   // underlying compiler
   //
   std::cout << "Underlying compiler:                       "
	     << Properties.comp_cmd << std::endl;

   // instrumentation type
   //
   InstTypeT inst_type = getInstType();
   std::cout << "Instrumentation type:                      ";

   switch( inst_type )
   {
      case INST_TYPE_GNU:
	 std::cout << "gnu" << std::endl;
	 break;
      case INST_TYPE_INTEL:
	 std::cout << "intel" << std::endl;
	 break;
      case INST_TYPE_PATHSCALE:
	 std::cout << "pathscale" << std::endl;
	 break;
      case INST_TYPE_PGI:
	 std::cout << "pgi" << std::endl;
	 break;
      case INST_TYPE_SUN:
	 std::cout << "sun" << std::endl;
	 break;
      case INST_TYPE_XL:
	 std::cout << "xl" << std::endl;
	 break;
      case INST_TYPE_FTRACE:
	 std::cout << "ftrace" << std::endl;
	 break;
      case INST_TYPE_MANUAL:
	 std::cout << "manual" << std::endl;
	 break;
      case INST_TYPE_POMP:
	 std::cout << "pomp" << std::endl;
	 break;
      case INST_TYPE_DYNINST:
	 std::cout << "dyninst" << std::endl;
	 break;
      default:
	 assert( 0 );
	 break;
   }

   // available instrumentation types
   //
   std::cout << " Available instrumentation types:         ";
   if( isInstAvail( INST_TYPE_GNU ) )
      std::cout << " gnu";
   if( isInstAvail( INST_TYPE_INTEL ) )
      std::cout << " intel";
   if( isInstAvail( INST_TYPE_PATHSCALE ) )
      std::cout << " pathscale";
   if( isInstAvail( INST_TYPE_PGI ) )
      std::cout << " pgi";
   if( isInstAvail( INST_TYPE_SUN ) )
      std::cout << " sun";
   if( isInstAvail( INST_TYPE_XL ) )
      std::cout << " xl";
   if( isInstAvail( INST_TYPE_FTRACE ) )
      std::cout << " ftrace";
   if( isInstAvail( INST_TYPE_MANUAL ) )
      std::cout << " manual";
   if( isInstAvail( INST_TYPE_POMP ) )
      std::cout << " pomp";
   if( isInstAvail( INST_TYPE_DYNINST ) )
      std::cout << " dyninst";
   std::cout << std::endl;
}

void
Wrapper::show()
{
   // show compiler command
   //
   if( showme() )
      std::cout << Properties.comp_cmd << " ";

   // show compiler flags
   //
   if( showmeCompile() )
   {
      std::cout << Properties.comp_flags << " "
		<< Properties.includedir << " "
		<< Properties.comp_iflags << " "
		<< Properties.comp_args << " ";
      if( !showmeLink() ) std::cout << std::endl;
   }
   
   // show linker flags
   //
   if( showmeLink() )
   {
      char vtlib[1024];

      if( usesOMP() )
      {
	 if( usesMPI() )
	 {
	    vt_snprintf( vtlib, sizeof(vtlib) - 1,
			 "%s %s %s %s "VTHYBLIB" %s %s",
			 Properties.comp_ldflags.c_str(),
			 Properties.libdir.c_str(),
			 getInstType() == INST_TYPE_DYNINST ?
			 Properties.dynattlib.c_str() : "",
#if defined(WRAP_LANG_F77) || defined(WRAP_LANG_F90)
			 Properties.fmpilib.c_str(),
#else
			 "",
#endif
			 Properties.pmpilib.c_str(),
			 Properties.comp_ulibs.c_str() );
	 }
	 else
	 {
	    vt_snprintf( vtlib, sizeof(vtlib) - 1, "%s %s %s "VTOMPLIB" %s",
			 Properties.comp_ldflags.c_str(),
			 Properties.libdir.c_str(),
			 getInstType() == INST_TYPE_DYNINST ?
			 Properties.dynattlib.c_str() : "",
			 Properties.comp_ulibs.c_str() );
	 }
      }
      else if( usesMPI() )
      {
	 vt_snprintf( vtlib, sizeof(vtlib) - 1, "%s %s %s %s "VTMPILIB" %s %s",
		      Properties.comp_ldflags.c_str(),
		      Properties.libdir.c_str(),
		      getInstType() == INST_TYPE_DYNINST ?
		      Properties.dynattlib.c_str() : "",
#if defined(WRAP_LANG_F77) || defined(WRAP_LANG_F90)
		      Properties.fmpilib.c_str(),
#else
		      "",
#endif	     
		      Properties.pmpilib.c_str(),
		      Properties.comp_ulibs.c_str() );
      }
      else
      {
	 vt_snprintf( vtlib, sizeof(vtlib) - 1, "%s %s %s "VTSEQLIB" %s",
		      Properties.comp_ldflags.c_str(),
		      Properties.libdir.c_str(),
		      getInstType() == INST_TYPE_DYNINST ?
		      Properties.dynattlib.c_str() : "",
		      Properties.comp_ulibs.c_str() );	     
      }

      std::cout << (showmeCompile() ? "" : Properties.comp_args) << " "
		<< (showmeCompile() ? "" : Properties.comp_iflags) << " "
		<< vtlib << std::endl;
   }
}

int
Wrapper::run()
{
   // show compiler/linker flags ?
   //
   if( showmeCompile() || showmeLink() )
   {
      show();
      return 0;
   }
   
   std::string cmd;
   int rc = 0;

   // call compiler without any parameters, if
   // insufficient arguments given
   //
   if( Properties.comp_args.length() == 0
       && !showmeCompile() && !showmeLink() )
   {
      rc = system( Properties.comp_cmd.c_str() );
      return WEXITSTATUS( rc );
   }

   // run opari on every collected source file
   //
   if( usesOMP() || getInstType() == INST_TYPE_POMP )
   {
      // add opari option '-nodecl' if PGI compiler will be used
      if( getInstType() == INST_TYPE_PGI )
	 opari_addArg( "-nodecl" );

      for( unsigned int i = 0; i < Properties.vec_opari_files.size(); i++ )
      {
	 cmd =
	    Properties.opari_cmd + " "
	    + Properties.opari_args + " "
	    + (usesOMP() ? "" : "-disable omp") + " "
	    + "-table "
	    + Properties.opari_tab_file.first + " "
	    + Properties.vec_opari_files[i];

	 if( beverbose() )
	    std::cout << "+++ " << cmd << std::endl;
	 rc = system( cmd.c_str() );
	 if( WEXITSTATUS( rc ) != 0 )
	    return WEXITSTATUS( rc );
      }
   }

   // executing modified command
   //
   if( componly() )
   {
      cmd =
	 Properties.comp_cmd + " "
	 + Properties.comp_flags + " "
	 + Properties.includedir + " "
	 + Properties.comp_iflags + " "
	 + Properties.comp_args;

      if( beverbose() )
	 std::cout << "+++ " << cmd << std::endl;
      rc = system( cmd.c_str() );
      if( WEXITSTATUS( rc ) != 0 )
	 return WEXITSTATUS( rc );
   }
   else
   {
      char vtlib[1024];

      if( usesOMP() || getInstType() == INST_TYPE_POMP )
      {
	 // compile opari table file
	 //
	 cmd =
	    Properties.opari_tab_comp_cmd + " "
	    + Properties.opari_tab_comp_flags + " "
	    + Properties.includedir + " "
	    + "-c " + Properties.opari_tab_file.first + " "
	    + "-o " + Properties.opari_tab_file.second;

	 if( beverbose() )
	    std::cout << "+++ " << cmd << std::endl;
	 rc = system( cmd.c_str() );
	 if( WEXITSTATUS( rc ) != 0 )
	    return WEXITSTATUS( rc );
      }

      if( usesOMP() )
      {
	 if( usesMPI() )
	 {
	    vt_snprintf( vtlib, sizeof(vtlib) - 1,
			 "%s %s %s %s "VTHYBLIB" %s %s",
			 Properties.comp_ldflags.c_str(),
			 Properties.libdir.c_str(),
			 getInstType() == INST_TYPE_DYNINST ?
			 Properties.dynattlib.c_str() : "",
#if defined(WRAP_LANG_F77) || defined(WRAP_LANG_F90)
			 Properties.fmpilib.c_str(),
#else
			 "",
#endif
			 Properties.pmpilib.c_str(),
			 Properties.comp_ulibs.c_str() );
	 }
	 else
	 {
	    vt_snprintf( vtlib, sizeof(vtlib) - 1, "%s %s %s "VTOMPLIB" %s",
			 Properties.comp_ldflags.c_str(),
			 Properties.libdir.c_str(),
			 getInstType() == INST_TYPE_DYNINST ?
			 Properties.dynattlib.c_str() : "",
			 Properties.comp_ulibs.c_str() );
	 }
      }
      else
      {
	 if( usesMPI() )
	 {
	    vt_snprintf( vtlib, sizeof(vtlib) - 1,
			 "%s %s %s %s "VTMPILIB" %s %s",
			 Properties.comp_ldflags.c_str(),
			 Properties.libdir.c_str(),
			 getInstType() == INST_TYPE_DYNINST ?
			 Properties.dynattlib.c_str() : "",
#if defined(WRAP_LANG_F77) || defined(WRAP_LANG_F90)
			 Properties.fmpilib.c_str(),
#else
			 "",
#endif	     
			 Properties.pmpilib.c_str(),
			 Properties.comp_ulibs.c_str() );
	 }
	 else
	 {
	    vt_snprintf( vtlib, sizeof(vtlib) - 1, "%s %s %s "VTSEQLIB" %s",
			 Properties.comp_ldflags.c_str(),
			 Properties.libdir.c_str(),
			 getInstType() == INST_TYPE_DYNINST ?
			 Properties.dynattlib.c_str() : "",
			 Properties.comp_ulibs.c_str() );	     
	 }
      }

      cmd =
	 Properties.comp_cmd + " "
	 + Properties.comp_flags + " "
	 + Properties.includedir + " "
	 + Properties.comp_iflags + " "
	 + Properties.comp_args + " "
	 + (( usesOMP() || getInstType() == INST_TYPE_POMP ) ? Properties.opari_tab_file.second : "" ) + " "
	 + vtlib;

      if( beverbose() )
	 std::cout << "+++ " << cmd << std::endl;

      rc = system( cmd.c_str() );
      if( WEXITSTATUS( rc ) != 0 )
	 return WEXITSTATUS( rc );

      // cleanup intermediate files (in non-verbose mode)
      //
      if( !beverbose()
	  && (usesOMP() || getInstType() == INST_TYPE_POMP) )
      {
	 std::vector<std::string> vec_incfiles =
	    opari_getIncFilesFromTabFile(
	       Properties.opari_tab_file.first );

	 for( unsigned int i = 0; i < vec_incfiles.size(); i++ )
	    remove( vec_incfiles[i].c_str() );

	 remove( Properties.opari_tab_file.first.c_str() );
	 remove( Properties.opari_tab_file.second.c_str() );
      }
   }

   if( usesOMP() || getInstType() == INST_TYPE_POMP )
   {
      unsigned int i;

      // rename compiler output to original file name
      //
      for( i = 0; i < Properties.vec_opari_mfiles_obj.size(); i++ )
      {
	 if( access( Properties.vec_opari_mfiles_obj[i].c_str(), F_OK ) == 0 )
	 {
	    int modi = Properties.vec_opari_mfiles_obj[i].find( ".mod" );

	    if( modi != -1 )
	    {
	       std::string target = Properties.vec_opari_mfiles_obj[i];
	       target.erase( modi, 4 );
	       
	       if( beverbose() )
		  std::cout << "+++ rename " << Properties.vec_opari_mfiles_obj[i]
			    << " to " << target << std::endl;
	       
	       if( rename( Properties.vec_opari_mfiles_obj[i].c_str(),
			   target.c_str() ) == -1 )
	       {
		  std::cerr << ExeName << ": error: could not rename "
			    << Properties.vec_opari_mfiles_obj[i]
			    << " to " << target << std::endl
			    << strerror(errno) << std::endl;
	       }
	    }
	 }
      }

      // delete intermediate opari output (in non-verbose mode)
      //
      if( !beverbose() )
      {
	 for( i = 0; i < Properties.vec_opari_mfiles_src.size(); i++ )
	    remove( Properties.vec_opari_mfiles_src[i].c_str() );
      }
   }

   return 0;
}

bool
Wrapper::setInstType( const InstTypeT type )
{
   assert( Properties.inst_avail != 0 );
   
   // instrumentation available ?
   if( !isInstAvail( type ) )
      return false;

   Properties.inst_type = type;

   switch( type )
   {
      case INST_TYPE_GNU:
	 Properties.comp_iflags = Properties.iflags_gnu;
	 break;
      case INST_TYPE_INTEL:
	 Properties.comp_iflags = Properties.iflags_intel;
	 break;
      case INST_TYPE_PATHSCALE:
	 Properties.comp_iflags = Properties.iflags_pathscale;
	 break;
      case INST_TYPE_PGI:
	 Properties.comp_iflags = Properties.iflags_pgi;
	 break;
      case INST_TYPE_SUN:
	 Properties.comp_iflags = Properties.iflags_sun;
	 break;
      case INST_TYPE_XL:
	 Properties.comp_iflags = Properties.iflags_xl;
	 break;
      case INST_TYPE_FTRACE:
	 Properties.comp_iflags = Properties.iflags_ftrace;
	 break;
      default:
	 Properties.comp_iflags = "";
	 break;
   }

   return true;
}

void
Wrapper::setUsesMPI( const bool set, const bool lock,
		    const bool ovwrt )
{
   static bool locked = false;
   if( lock ) locked = true;
   if( !locked || ovwrt ) Properties.uses_mpi = set;
}

void
Wrapper::setUsesOMP( const bool set, const bool lock,
		     const bool ovwrt )
{
   static bool locked = false;
   if( lock ) locked = true;
   if( !locked || ovwrt ) Properties.uses_omp = set;
}

void
Wrapper::comp_setCmd( const std::string cmd )
{
   std::string bcomp = cmd;
   int ls = cmd.rfind('/');

   if( ls != -1 ) bcomp = cmd.substr( ls+1 );
   
   if( !usesMPI() &&
       ( bcomp.compare( 0, 2, "mp" ) == 0 ||
	 bcomp.compare( 0, 4, "sxmp" ) == 0 ||
	 bcomp.compare( 0, 4, "scmp" ) == 0 ) )
      setUsesMPI( true );

   Properties.comp_cmd = cmd;
}

void
Wrapper::comp_addArg( const std::string arg )
{
   if( Properties.comp_args.length() > 0 )
      Properties.comp_args += " ";

   Properties.comp_args += arg;
}

void
Wrapper::comp_addULib( const std::string ulib )
{
   if( Properties.comp_ulibs.length() > 0 )
      Properties.comp_ulibs += " ";

   Properties.comp_ulibs += ulib;
}

void
Wrapper::opari_setTabFile( const std::string tabfile )
{
   std::string file_src;
   std::string file_obj;

   if( !( tabfile.length() >= 2
	  && tabfile.compare( tabfile.length()-2, 2, ".c" ) == 0 ) )
   {
      file_src = tabfile + ".c";
      file_obj = tabfile + ".o";
   }
   else
   {
      file_src = file_obj = tabfile;
      file_obj.replace( tabfile.length()-2, 2, ".o" );
   }

   Properties.opari_tab_file = std::make_pair( file_src, file_obj );

}

void
Wrapper::opari_addArg( const std::string arg )
{
   if( Properties.opari_args.length() > 0 )
      Properties.opari_args += " ";

   Properties.opari_args += arg;
}

void
Wrapper::opari_addSrcFile( const std::string srcfile )
{
   std::string base = srcfile.substr(0, srcfile.rfind('.'));
   std::string suf = srcfile.substr(srcfile.rfind('.'));
   std::string newsrcfile;
   std::string newobjfile = base + std::string(".mod.o");

   // erase path of object file
   int si = newobjfile.rfind('/');
   if( si != -1 ) newobjfile.erase( 0, si+1 );

   // replace 'f' by 'F'
   // so the compiler invokes the C-preprocessor
   int fi = suf.rfind( 'f' );
   if( fi != -1 ) suf.replace( fi, 1, "F" );

   newsrcfile = base + std::string(".mod") + suf;

   Properties.vec_opari_files.push_back( srcfile );
   Properties.vec_opari_mfiles_src.push_back( newsrcfile );
   Properties.vec_opari_mfiles_obj.push_back( newobjfile );

   if( !( ( srcfile.length() >= 2
	    && srcfile.compare( srcfile.length() - 2, 2, ".f" ) == 0 )
	  || ( srcfile.length() >= 2
	       && srcfile.compare( srcfile.length() - 2, 2, ".F" ) == 0 )
	  || ( srcfile.length() >= 4
	       && srcfile.compare( srcfile.length() - 4, 4, ".f77" ) == 0 )
	  || ( srcfile.length() >= 4
	       && srcfile.compare( srcfile.length() - 4, 4, ".F77" ) == 0 )
	  || ( srcfile.length() >= 4
	       && srcfile.compare( srcfile.length() - 4, 4, ".f90" ) == 0 )
	  || ( srcfile.length() >= 4
	       && srcfile.compare( srcfile.length() - 4, 4, ".F90" ) == 0 )
	  || ( srcfile.length() >= 4
	       && srcfile.compare( srcfile.length() - 4, 4, ".f95" ) == 0 )
	  || ( srcfile.length() >= 4
	       && srcfile.compare( srcfile.length() - 4, 4, ".F95" ) == 0 ) ) )
   {
      std::string incfile = srcfile + std::string(".opari.inc");
      Properties.vec_opari_mfiles_src.push_back( incfile );
   }

   comp_addArg( newsrcfile );
}

std::vector<std::string>
Wrapper::opari_getIncFilesFromTabFile( const std::string tabfile )
{
   std::vector<std::string> vec_incfiles;

   std::ifstream in( tabfile.c_str() );
   if( in )
   {
      char buffer[1024];
      std::string line;

      while( in.getline( buffer, sizeof(buffer) - 1 ) )
      {
	 line = buffer;

	 if( (int)(line.find( "#include" )) != -1
	     && (int)(line.find( ".opari.inc" )) != -1 )
	 {
	    std::string incfile = line.substr( line.find("#include")+10 );
	    incfile.erase( incfile.length()-1 );

	    vec_incfiles.push_back( incfile );
	 }
      }

      in.close();
   }

   return vec_incfiles;
}
