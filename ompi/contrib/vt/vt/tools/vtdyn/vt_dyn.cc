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
#include <ctype.h>
#include <inttypes.h>
#include <signal.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>

#include "BPatch.h"
#include "BPatch_function.h"
#include "BPatch_image.h"
#include "BPatch_module.h"
#include "BPatch_process.h"
#include "BPatch_snippet.h"
#include "BPatch_statement.h"
#include "BPatch_Vector.h"

#include "vt_dyn.h"

#define USAGETEXT std::endl \
<< " " << ExeName << " - Dyninst Mutator for VampirTrace." << std::endl \
<< std::endl \
<< " Syntax: " << ExeName << " [-v|--verbose] [-s|--shlib <shlib>[,...]] [-b|--blacklist <bfile> [-p|--pid <pid>] <app> [appargs ...]" << std::endl \
<< std::endl \
<< "   options:" << std::endl \
<< "     -h, --help          Show this help message." << std::endl \
<< std::endl \
<< "     -v, --verbose       Enable verbose mode." << std::endl \
<< std::endl \
<< "     -s, --shlib         Comma-separated list of shared libraries" << std::endl \
<< "     <shlib>[,...]       which should also be instrumented." << std::endl \
<< std::endl \
<< "     -b, --blacklist     Set path of blacklist file containing" << std::endl \
<< "     <bfile>             a newline-separated list of functions" << std::endl \
<< "                         which should not be instrumented." << std::endl \
<< std::endl \
<< "     -p, --pid <pid>     application's process id" << std::endl \
<< "                         (attaches the mutator to a running process)" << std::endl \
<< std::endl \
<< "     app                 path of application executable" << std::endl \
<< std::endl \
<< "     appargs             application's arguments" << std::endl

//
// structure that contains the program options
//
struct Params_struct
{
   Params_struct()
      : mutatee(""), mutatee_pid(-1),
	blist_path(""), beverbose(false) {}

   std::string              mutatee;
   std::vector<std::string> mutatee_args;
   int                      mutatee_pid;
   std::vector<std::string> shlibs;
   std::string              blist_path;
   bool                     beverbose;
} Params;

static bool ParseCommandLine( int argc, char ** argv );

static const std::string ExeName = "vtdyn";
static const std::string BLName = "vtdyn_blist";

Mutator * theMutator;            // instance of class Mutator
BPatch    theBpatch;             // instance of class BPatch
int       mutatorPid = getpid(); // mutator's process id

int
main( int argc, char ** argv )
{
   int rc;

   // parse command line
   if( !ParseCommandLine( argc, argv ) )
      return 1;

   // show help text, if command line parameters are incomplete
   //
   if( Params.mutatee.length() == 0 && Params.mutatee_pid == -1 )
   {
      std::cout << USAGETEXT << std::endl;
      return 0;
   }

   if( Params.mutatee_pid == -1 )
   {
      // set/overwrite environment variable VT_UNIFY to zero,
      // so VampirTrace don't unify local traces (DYNINST Bug?)
      putenv( "VT_UNIFY=no" );
   }

   // create instance of mutator
   theMutator = new Mutator();
   assert( theMutator );

   // start mutation
   rc = theMutator->run() ? 0 : 1;

   // cleanup
   //
   delete theMutator;

   return rc;
}

static bool
ParseCommandLine( int argc, char ** argv )
{
   int i, j;

   for( i = 1; i < argc; i++ )
   {
      if( strcmp( argv[i], "-h" ) == 0
	  || strcmp( argv[i], "--help" ) == 0 )
      {
	 Params.mutatee = ""; Params.mutatee_pid = -1;
	 return true;
      }
      else if( strcmp( argv[i], "-v" ) == 0
	       || strcmp( argv[i], "--verbose" ) == 0 )
      {
	 Params.beverbose = true;
      }
      else if( strcmp( argv[i], "-s" ) == 0
	       || strcmp( argv[i], "--shlib" ) == 0 )
      {
	 if( i == argc - 1 )
	 {
	    std::cerr << ExeName << ": [" << mutatorPid << "]: <shlib> expected -- -s" << std::endl;
	    return false;
	 }

	 char shlibs_str[STRBUFSIZE];
	 char * tk;

	 strcpy( shlibs_str, argv[++i] );
	 tk = strtok( shlibs_str, "," );
	 do
	 {
	    Params.shlibs.push_back( std::string(tk) );
	 } while( (tk = strtok( 0, "," )) );
      }
      else if( strcmp( argv[i], "-b" ) == 0
	       || strcmp( argv[i], "--blacklist" ) == 0 )
      {
	 if( i == argc - 1 )
	 {
	    std::cerr << ExeName << ": [" << mutatorPid << "]: <blistpath> expected -- -b" << std::endl;
	    return false;
	 } 

	 Params.blist_path = std::string( argv[++i] );
      }
      else if( strcmp( argv[i], "-p" ) == 0
	       || strcmp( argv[i], "--pid" ) == 0 )
      {
	 if( i == argc - 1 )
	 {
	    std::cerr << ExeName << ": [" << mutatorPid << "]: <pid> expected -- -p" << std::endl;
	    return false;
	 }

	 Params.mutatee_pid = atoi( argv[++i] );
      }
      else
      {
	 if( Params.mutatee.length() == 0 )
	 {
	    Params.mutatee = std::string( argv[i] );
	    Params.mutatee_args.push_back( Params.mutatee );
	 }
	 else
	 {
	    for( j = 1; i < argc; i++, j++ )
	       Params.mutatee_args.push_back( std::string( argv[i] ) );
	    break;
	 }
      }
   }

   return true;
}

//////////////////// class Mutator ////////////////////

// public methods
//

Mutator::Mutator() : m_pAppProcess(0), m_pAppImage(0),
		     m_pVTDynStartFunc(0),  m_pVTDynEndFunc(0)  
{
   // empty
}

Mutator::~Mutator()
{
   // empty
}

bool
Mutator::run()
{
   bool error = false;

   uint32_t i;

   // initialize mutator
   if( !initialize() )
      error = true;

   // get instrumentable functions of image
   //
   std::vector<struct InstFunc*> vec_inst_funcs;
   if( !error && !getFunctions( &vec_inst_funcs ) )
      error = true;

   // instrument functions
   //
   if( !error )
   {
      DGOUT( "Instrumenting functions ..." );

      for( i = 0; i < vec_inst_funcs.size(); i++ )
      {
	 if( !instrumentFunction( vec_inst_funcs[i] ) )
	    break;
      }
      if( i != vec_inst_funcs.size() )
	 error = true;
   }

   for( i = 0; i < vec_inst_funcs.size(); i++ )
      delete vec_inst_funcs[i];
   
   if( !error )
   {
      DGOUT( "Executing application ..." );

      // continue execution of the mutatee
      //
      if( !m_pAppProcess->isStopped() || m_pAppProcess->isTerminated() )
      {
	 std::cerr << ExeName << ": [" << mutatorPid << "]: Error: Could not continue execution of process" << std::endl;
	 error = true;
      }

      if( !error )
      {
	 // send mutatee process a signal to continue execution
	 //
	 if( Params.mutatee_pid != -1 )
	    kill( Params.mutatee_pid, SIGUSR1 );

	 m_pAppProcess->continueExecution();

	 // wait for mutatee to terminate
	 //
	 while( !m_pAppProcess->isTerminated() )
	 {
	    theBpatch.waitForStatusChange();
	    sleep(1);
	 }

	 DGOUT( "End of application" );
      }
   }

   // terminate execution, if an error occurred
   //
   if( error && m_pAppProcess && !m_pAppProcess->isTerminated() )
      m_pAppProcess->terminateExecution();

   return !error;
}

// private methods
//

bool
Mutator::initialize()
{
   // read function blacklist file
   //
   if( Params.blist_path.length() > 0 )
   {
      if( !readFunctionBL() )
	 return false;
   }

   // attach to the program
   //
   if( Params.mutatee_pid == -1 )   // ... by executable
   {
      DGOUT( "Creating process ..." );
      
      char ** mutatee_args = new char*[Params.mutatee_args.size()];

      for( uint32_t i = 0; i < Params.mutatee_args.size(); i++ )
      {
	 mutatee_args[i] = new char[Params.mutatee_args[i].length()+1];
	 strcpy( mutatee_args[i], Params.mutatee_args[i].c_str() );
      }

      m_pAppProcess =
	 theBpatch.processCreate( Params.mutatee.c_str(),
				  (const char**)mutatee_args );
   }
   else                             // ... by pid
   {
      DGOUT( "Attaching myself at pid " << Params.mutatee_pid << " ..." );
      m_pAppProcess =
	 theBpatch.processAttach( Params.mutatee.c_str(), Params.mutatee_pid );
   }

   if( !m_pAppProcess || m_pAppProcess->isTerminated() )
   {
      std::cerr << ExeName << ": [" << mutatorPid << "]: Error: Could not create process object" << std::endl;
      if( Params.mutatee_pid != -1 &&
	  ( Params.mutatee.length() == 0 ) )
      {
	 std::cerr << ExeName << ": [" << mutatorPid << "]: A possible solution to the problem is to set the environment variable VT_APPPATH to the path of your application." << std::endl;
      }
      return false;
   }

   // read the program's image and get an associated image object
   //
   m_pAppImage = m_pAppProcess->getImage();
   if( !m_pAppImage )
   {
      std::cerr << ExeName << ": [" << mutatorPid << "]: Error: Could not get an image object" << std::endl;
      return false;
   }
   
   // search some functions from VampirTrace
   //
   if( !findFunction( "VT_Dyn_start", &m_pVTDynStartFunc ) )
   {
      std::cerr << ExeName << ": [" << mutatorPid << "]: Error: Unable to find function VT_Dyn_start" << std::endl;
      return false;
   }
   if( !findFunction( "VT_Dyn_end", &m_pVTDynEndFunc ) )
   {
      std::cerr << ExeName << ": [" << mutatorPid << "]: Error: Unable to find function VT_Dyn_end" << std::endl;
      return false;
   }

   return true;
}

bool
Mutator::readFunctionBL()
{
   assert( Params.blist_path.length() > 0 );

   // open function blacklist file for reading
   //
   std::ifstream in( Params.blist_path.c_str() );
      
   if( !in )
   {
      std::cerr << ExeName << ": [" << mutatorPid << "]: Error: Cannot open blacklist file " << Params.blist_path << std::endl;
      return false;
   }

   char buffer[STRBUFSIZE];
   
   // read line per line
   //
   while( in.getline( buffer, STRBUFSIZE ) )
   {
      chomp(buffer);
      trim(buffer);

      if( strlen( buffer ) == 0 || buffer[0] == '#' )
	 continue;

      m_vecBlacklist.push_back( buffer );
   }

   // close function blacklist file
   in.close();

   // sort function blacklist for faster searching
   //
   if( m_vecBlacklist.size() > 0 )
   {
      std::sort( m_vecBlacklist.begin(),
		 m_vecBlacklist.end() );
   }

   return true;
}

bool
Mutator::checkFunctionBL( std::string name )
{
   std::vector<std::string>::iterator it = 
      std::find( m_vecBlacklist.begin(),
		 m_vecBlacklist.end(),
		 name );

   return it != m_vecBlacklist.end();
}

bool
Mutator::getFunctions( std::vector<struct InstFunc*> * p_vecInstFuncs )
{
   DGOUT( "Get instrumentable functions ..." );

   // get list of modules from image
   //
   BPatch_Vector<BPatch_module*> * p_vec_modules =
      m_pAppImage->getModules();

   if( !p_vec_modules )
   {
      std::cerr << ExeName << ": [" << mutatorPid << "]: Error: Could not get modules of image" << std::endl;
      return false;
   }

   for( uint32_t i = 0; i < p_vec_modules->size(); i++ )
   {
      std::string module_name;
      char buffer[STRBUFSIZE] = "";

      (*p_vec_modules)[i]->getName( buffer, STRBUFSIZE );
      module_name = buffer;

      if( constraintModule( module_name ) )
      {
	 DGOUT( " Skip module '" << module_name << "'" );
	 continue;
      }

      // get functions of module
      //
      BPatch_Vector<BPatch_function*> * p_vec_functions =
	 (*p_vec_modules)[i]->getProcedures();

      if( !p_vec_functions )
      {
	 std::cerr << ExeName << ": [" << mutatorPid << "]: Error: Could not get functions of module " << module_name << std::endl;
	 return false;
      }

      for( uint32_t j = 0; j < p_vec_functions->size(); j++ )
      {
	 std::string function_name;

	 (*p_vec_functions)[j]->getName( buffer, STRBUFSIZE );
	 function_name = buffer;

	 if( !(*p_vec_functions)[j]->isInstrumentable() )
	 {
	    DGOUT( " Skip function '" << function_name << "' (not instrumentable)" );
	    continue;
	 }

	 if( constraintFunction( function_name ) )
	 {
	    DGOUT( " Skip function '" << function_name << "'" );
	    continue;
	 }

	 std::vector<BPatch_statement> statements;
	 unsigned long addr;
	 const char* file_name = NULL;
	 int line_number = 0;

	 addr = (unsigned long)(*p_vec_functions)[j]->getBaseAddr();
	 (*p_vec_modules)[i]->getSourceLines( addr, statements );

	 if( statements.size() > 0 )
	 {
	    file_name = statements[0].fileName();
	    line_number = statements[0].lineNumber();
	 }

	 if( statements.size() == 0 ||
	     file_name == NULL ||
	     constraintModule( file_name ) )
	 {
	    DGOUT( " Skip function '" << function_name << "' (determ. source code loc. failed)" );
	    continue;
	 }

	 DGOUT( " Add function '" << function_name << "' for instrumenting" );
	 p_vecInstFuncs->push_back(
	    new struct InstFunc( (*p_vec_functions)[j], addr,
				 function_name,
				 file_name,
				 line_number ) );
      }
   }

   return true;
}

bool
Mutator::constraintModule( std::string name )
{
   int len = name.length();

   if( name.compare( "DEFAULT_MODULE" ) == 0 ||
       name.compare( "LIBRARY_MODULE" ) == 0 ||
       ( name.compare( 0, 3, "vt_" ) != 0    &&
	 name.compare( 0, 4, "rfg_" ) != 0   &&
	 name.compare( 0, 4, "OTF_" ) != 0   &&
	 name.compare( 0, 5, "pomp_" ) != 0 )&&
       name.compare( len-2, 2, ".c" ) == 0   ||
       name.compare( len-2, 2, ".C" ) == 0   ||
       name.compare( len-3, 3, ".cc" ) == 0  ||
       name.compare( len-4, 4, ".cpp" ) == 0 ||
       name.compare( len-2, 2, ".f" ) == 0   ||
       name.compare( len-2, 2, ".F" ) == 0   ||
       name.compare( len-4, 4, ".f77" ) == 0 ||
       name.compare( len-4, 4, ".F77" ) == 0 ||
       name.compare( len-4, 4, ".f90" ) == 0 ||
       name.compare( len-4, 4, ".F90" ) == 0 ||
       name.compare( len-4, 4, ".f95" ) == 0 ||
       name.compare( len-4, 4, ".F95" ) == 0 )
   {
      return false;   // ok, module should be instrumented
   }
   else
   {
      // check for user specified constraints on modules
      //
      std::vector<std::string>::iterator it =
	 std::find( Params.shlibs.begin(),
		    Params.shlibs.end(),
		    name );

      return it == Params.shlibs.end();
   }
}

bool
Mutator::constraintFunction( std::string name )
{
   if( isMPI() && name.compare( 0, 4, "MPI_" ) == 0 )
   {
      return true;    // don't instrument MPI functions
                      // (already done by function wrapper)
   }
   else if( checkFunctionBL( name ) ||
	    name.compare( "ataxit" ) == 0 )
   {
      return true;    // don't instrument function
   }
   else
   {
      return false;   // ok, function should be instrumented
   }
}

bool
Mutator::isMPI()
{
   static int is_mpi = -1;

   if( is_mpi == -1 )
   {
      BPatch_function * p_mpi_init = 0;
      if( findFunction( "MPI_Init", &p_mpi_init ) )
	 is_mpi = 1;
      else
	 is_mpi = 0;
   }

   return is_mpi == 1 ? true : false;
}

bool
Mutator::findFunction( std::string name, BPatch_function ** p_function )
{
   BPatch_Vector<BPatch_function*> vec_found_funcs;
 
   m_pAppImage->findFunction( name.c_str(), vec_found_funcs, false );

   if( vec_found_funcs.size() > 0 )
   {
      *p_function = vec_found_funcs[0];
      return true;
   }
   else
   {
      *p_function = 0;
      return false;
   }
}

bool
Mutator::instrumentFunction( struct InstFunc * p_instFunc )
{
   bool error = false;

   BPatch_Vector<BPatch_snippet *> *p_callee_args =
      new BPatch_Vector<BPatch_snippet *>();

   BPatch_constExpr * p_const_expr_faddr =
      new BPatch_constExpr( p_instFunc->addr );
   p_callee_args->push_back( p_const_expr_faddr );
   BPatch_constExpr * p_const_expr_fname =
      new BPatch_constExpr( p_instFunc->name.c_str() );
   p_callee_args->push_back( p_const_expr_fname );
   BPatch_constExpr * p_const_expr_file =
      new BPatch_constExpr( p_instFunc->file.c_str() );
   p_callee_args->push_back( p_const_expr_file );
   BPatch_constExpr * p_const_expr_lno =
      new BPatch_constExpr( p_instFunc->lno );
   p_callee_args->push_back( p_const_expr_lno );

   DGOUT( " Instrumenting-> '" << p_instFunc->name << "' Entry ..." );
   if( !insertFunctionCall( p_instFunc->p_func, BPatch_entry, 
			    m_pVTDynStartFunc, p_callee_args ) )
   { error = true; }

   delete p_const_expr_lno;
   delete p_const_expr_file;
   delete p_const_expr_fname;
   p_callee_args->pop_back();
   p_callee_args->pop_back();
   p_callee_args->pop_back();

   if( !error )
   {
      DGOUT( " Instrumenting-> '" << p_instFunc->name << "' Exit ..." );
      if( !insertFunctionCall( p_instFunc->p_func, BPatch_exit,
			       m_pVTDynEndFunc, p_callee_args ) )
      { error = true; }
   }

   delete p_const_expr_faddr;
   delete p_callee_args;

   return !error;
}

bool
Mutator::insertFunctionCall( BPatch_function * p_function,
			     BPatch_procedureLocation loc,
			     BPatch_function * p_callee,
			     BPatch_Vector<BPatch_snippet *> * p_callee_args )
{
   const BPatch_snippet *p_snippet =
      new BPatch_funcCallExpr( *p_callee, *p_callee_args );

   if( p_snippet == 0 )
   {
      std::cerr << ExeName << ": [" << mutatorPid << "]: Error: Unable to create snippet to call callee" << std::endl;
      return false;
   }

   const BPatch_Vector<BPatch_point *> *p_points =
      p_function->findPoint( loc ); 

   if( p_points == 0 )
   {
      std::cerr << ExeName << ": [" << mutatorPid << "]: Error: Unable to find insert point to call callee" << std::endl;
      delete p_snippet;
      return false;
   }

   if( loc == BPatch_entry )
   {
      m_pAppProcess->insertSnippet( *p_snippet, *p_points,
				    BPatch_callBefore, BPatch_lastSnippet );
   }
   else
   {
      m_pAppProcess->insertSnippet( *p_snippet, *p_points );
   }

   delete p_snippet;

   return true;
}
