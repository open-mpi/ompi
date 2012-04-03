/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2012, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/


#include "vt_dyn.h"

#include <fstream>
#include <ctype.h>
#include <inttypes.h>
#include <signal.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>

#include "BPatch_module.h"
#include "BPatch_process.h"
#include "BPatch_snippet.h"
#include "BPatch_statement.h"

// local functions
//

// parse command line options
static bool parseCommandLine( int argc, char ** argv );

// show usage text
static void showUsage( void );

// print verbose message
static void vPrint( uint8_t level, const char * fmt, ... );

// global variables
//

const std::string ExeName = "vtdyn"; // name of program's (mutator) executable
const int ExePid = getpid();         // mutator's PID
ParamsS Params;                      // mutator's parameters
MutatorC * theMutator;               // instance of class MutatorC

int
main( int argc, char ** argv )
{
   int rc = 0;

   do
   {
      // parse command line options
      //
      if( !parseCommandLine( argc, argv ) )
      {
         rc = 1;
         break;
      }

      // show VT version, if desired
      //
      if( Params.show_version )
      {
         std::cout << PACKAGE_VERSION << std::endl;
         break;
      }

      // show usage text, if necessary/desired
      //
      if( Params.show_usage ||
          ( Params.mutatee.length() == 0 && Params.mutatee_pid == -1 ) )
      {
         showUsage();
         break;
      }

      // create instance of class MutatorC
      //
      theMutator = new MutatorC();
      assert( theMutator );

      // start mutation
      //
      rc = theMutator->run() ? 0 : 1;

      // cleanup
      delete theMutator;

   } while( false );

   return rc;
}

static bool
parseCommandLine( int argc, char ** argv )
{
   bool error = false;

   int i, j;

   for( i = 1; i < argc; i++ )
   {
      // -h, --help
      //
      if( strcmp( argv[i], "-h" ) == 0
          || strcmp( argv[i], "--help" ) == 0 )
      {
         Params.show_usage = true;
         break;
      }
      // -V, --version
      //
      else if( strcmp( argv[i], "-V" ) == 0
               || strcmp( argv[i], "--version" ) == 0 )
      {
         Params.show_version = true;
         break;
      }
      // -v, --version
      //
      else if( strcmp( argv[i], "-v" ) == 0
               || strcmp( argv[i], "--verbose" ) == 0 )
      {
         Params.verbose_level++;
      }
      // -q, --quiet
      //
      else if( strcmp( argv[i], "-q" ) == 0
               || strcmp( argv[i], "--quiet" ) == 0 )
      {
         Params.verbose_level = 0;
      }
      // -o, --output
      //
      else if( strcmp( argv[i], "-o" ) == 0
               || strcmp( argv[i], "--output" ) == 0 )
      {
         if( i == argc - 1 )
         {
            std::cerr << ExeName << ": [" << ExePid << "]: FILE "
                      << "expected -- " << argv[i] << std::endl;
            error = true;
            break;
         }

         Params.outfile = argv[++i];
         Params.mode = MODE_REWRITE;
      }
      // -s, --shlibs
      //
      else if( strcmp( argv[i], "-s" ) == 0
               || strcmp( argv[i], "--shlibs" ) == 0 )
      {
         if( i == argc - 1 )
         {
            std::cerr << ExeName << ": [" << ExePid << "]: SHLIBS expected "
                      << "-- " << argv[i] << std::endl;
            error = true;
            break;
         }

         char shlibs_str[STRBUFSIZE];
         char * tk;

         strcpy( shlibs_str, argv[++i] );
         tk = strtok( shlibs_str, "," );
         do
         {
            Params.shlibs.push_back( tk );
         } while( (tk = strtok( 0, "," )) );
      }
      // --f, --filter
      //
      else if( strcmp( argv[i], "-f" ) == 0
               || strcmp( argv[i], "--filter" ) == 0 )
      {
         if( i == argc - 1 )
         {
            std::cerr << ExeName << ": [" << ExePid << "]: FILE expected "
                      << "-- " << argv[i] << std::endl;
            error = true;
            break;
         }

         Params.filtfile = argv[++i];
      }
      // --ignore-nodbg
      //
      else if( strcmp( argv[i], "--ignore-nodbg" ) == 0 )
      {
         Params.ignore_no_dbg = true;
      }

      // hidden options - only for using within the VampirTrace library
      //

      // -p, --pid
      //
      else if( strcmp( argv[i], "-p" ) == 0
               || strcmp( argv[i], "--pid" ) == 0 )
      {
         if( i == argc - 1 )
         {
            std::cerr << ExeName << ": [" << ExePid << "]: PID expected "
                      << "-- " << argv[i] << std::endl;
            error = true;
            break;
         }

         Params.mutatee_pid = atoi( argv[++i] );
         Params.mode = MODE_ATTACH;
      }
      // --nodetach
      //
      else if( strcmp( argv[i], "--nodetach" ) == 0 )
      {
          Params.detach = false;
      }

      // <executable> [arguments ...]
      //
      else
      {
         if( Params.mutatee.length() == 0 )
         {
            Params.mutatee = argv[i];
            Params.mutatee_args.push_back( Params.mutatee );
         }
         else
         {
            for( j = 1; i < argc; i++, j++ )
               Params.mutatee_args.push_back( argv[i] );
            break;
         }
      }
   }

   return !error;
}

static void
showUsage()
{
   std::cout << std::endl
      << " " << ExeName << " - binary instrumentor (Dyninst mutator) for VampirTrace." << std::endl
      << std::endl
      << " Syntax: " << ExeName << " [options] <executable> [arguments ...]" << std::endl
      << std::endl
      << "   options:" << std::endl
      << "     -h, --help          Show this help message." << std::endl
      << std::endl
      << "     -V, --version       Show VampirTrace version." << std::endl
      << std::endl
      << "     -v, --verbose       Increase output verbosity." << std::endl
      << "                         (can be used more than once)" << std::endl
      << std::endl
      << "     -q, --quiet         Enable quiet mode." << std::endl
      << "                         (only emergency output)" << std::endl
      << std::endl
      << "     -o, --output FILE   Rewrite instrumented executable to specified pathname." << std::endl
      << std::endl
      << "     -s, --shlibs SHLIBS[,...]" << std::endl
      << "                         Comma-separated list of shared libraries which shall" << std::endl
      << "                         also be instrumented." << std::endl
      << std::endl
      << "     -f, --filter FILE   Pathname of input filter file." << std::endl
      << std::endl
      << "     --ignore-nodbg      Don't instrument functions which have no debug" << std::endl
      << "                         information." << std::endl
      << std::endl;
}

static void
vPrint( uint8_t level, const char * fmt, ... )
{
   va_list ap;

   if( Params.verbose_level >= level )
   {
      va_start( ap, fmt );

      char msg[1024] = "";

      snprintf( msg, sizeof( msg ) - 1, "%s: [%d]: ", ExeName.c_str(), ExePid );
      vsnprintf( msg + strlen( msg ), sizeof( msg ) - 1, fmt, ap );

      printf( "%s", msg );

      va_end( ap );
   }
}

//////////////////// class MutatorC ////////////////////

// public methods
//

MutatorC::MutatorC()
   : m_appAddrSpace(0), m_appImage(0), m_vtStartFunc(0), m_vtEndFunc(0),
     m_filter(0)
{
   // Empty
}

MutatorC::~MutatorC()
{
   // Empty
}

bool
MutatorC::run()
{
   bool error = false;

   do
   {
      // create/attach to a process or open binary for rewriting
      if( ( error = !initialize() ) )
         break;

      // get instrumentable functions of image
      //

      std::vector<InstFuncS> inst_funcs;

      if( ( error = !getFunctions( inst_funcs ) ) )
         break;

      // instrument functions
      //

      vPrint( 1, "Instrumenting functions\n" );

      for( uint32_t i = 0; i < inst_funcs.size(); i++ )
      {
         // begin insertion set
         m_appAddrSpace->beginInsertionSet();

         // instrument function entry points
         if( ( error = !instrumentFunctionEntry( inst_funcs[i] ) ) )
            break;
         // instrument function exit points
         if( ( error = !instrumentFunctionExit( inst_funcs[i] ) ) )
            break;

         // finalize insertion set
         //
         if( !m_appAddrSpace->finalizeInsertionSet( true, 0 ) )
         {
            std::cerr << ExeName << ": [" << ExePid << "]: "
                      << "Error: Could not finalize instrumentation set for "
                      << "function '" << inst_funcs[i].name << "'. Aborting."
                      << std::endl;
            error = true;
            break;
         }
      }
      inst_funcs.clear();

   } while( false );

   // continue execution of mutatee or rewrite binary
   error = !finalize( error );

   return !error;
}

// private methods
//

bool
MutatorC::initialize()
{
   bool error = false;

   do
   {
      // set recommended optimizations to reduce runtime overhead
      //

      // turn on inlined trampolines
      m_bpatch.setMergeTramp( true );

      // turn on trampoline recursion because there is no way for the snippets
      // to call themselves
      m_bpatch.setTrampRecursive( true );

      // turn off stack frames in instrumentation
      m_bpatch.setInstrStackFrames( false );

      // turn on floating point saves due to the instrumentation does clobber
      // floating point registers
      //

      m_bpatch.setSaveFPR( true );
#ifdef DYNINST_7_0
      m_bpatch.forceSaveFPR( true );
#endif // DYNINST_7_0

      // read input filter file
      if( ( error = !readFilter() ) )
         break;

      switch( Params.mode )
      {
         case MODE_CREATE:
         {
            vPrint( 1, "Creating process\n" );

            assert( Params.mutatee.length() > 0 );

            char ** mutatee_args;
            uint32_t i;

            // convert vector of mutatee arguments to char array
            //

            mutatee_args = new char*[Params.mutatee_args.size()+1];
            assert( mutatee_args );

            for( i = 0; i < Params.mutatee_args.size(); i++ )
            {
               mutatee_args[i] = new char[Params.mutatee_args[i].length()+1];
               strcpy( mutatee_args[i], Params.mutatee_args[i].c_str() );
            }
            mutatee_args[Params.mutatee_args.size()] = 0;

            // create process
            m_appAddrSpace =
               m_bpatch.processCreate(
                  Params.mutatee.c_str(), (const char**)mutatee_args );

            // catch possible error
            //
            if( !m_appAddrSpace ||
               dynamic_cast<BPatch_process*>(m_appAddrSpace)->isTerminated() )
            {
               std::cerr << ExeName << ": [" << ExePid << "]: "
                         << "Error: Could not create process. Aborting."
                         << std::endl;
               error = true;
            }

            // free array of mutatee arguments
            //
            for( i = 0; i < Params.mutatee_args.size(); i++ )
               delete [] mutatee_args[i];
            delete [] mutatee_args;

            break;
         }
         case MODE_ATTACH:
         {
            assert( Params.mutatee_pid );

            vPrint( 1, "Attaching to PID %d\n", Params.mutatee_pid );

            // attach to running process
            m_appAddrSpace =
               m_bpatch.processAttach(
                  Params.mutatee.c_str(), Params.mutatee_pid );

            // catch possible error
            //
            if( !m_appAddrSpace ||
               dynamic_cast<BPatch_process*>(m_appAddrSpace)->isTerminated() )
            {
               std::cerr << ExeName << ": [" << ExePid << "]: "
                         << "Error: Could not attach to PID "
                         << Params.mutatee_pid << ". Aborting." << std::endl;

               if( Params.mutatee.length() == 0 )
               {
                  std::cerr << ExeName << ": [" << ExePid << "]: "
                            << "A possible solution to the problem is to set "
                            << "the environment variable VT_APPPATH to the "
                            << "path of your application." << std::endl;
               }

               error = true;
            }

            break;
         }
         case MODE_REWRITE:
         {
            assert( Params.mutatee.length() > 0 );

            vPrint( 1, "Opening %s\n", Params.mutatee.c_str() );

            // open binary for rewriting
            m_appAddrSpace =
               m_bpatch.openBinary( Params.mutatee.c_str(), true );

            // catch possible error
            //
            if( !m_appAddrSpace )
            {
               std::cerr << ExeName << ": [" << ExePid << "]: "
                         << "Error: Could not open " << Params.mutatee
                         << ". Aborting." << std::endl;
               error = true;
            }

            break;
         }
      }
      if( error )
         break;

      // load user-specified shared libraries into mutatee
      //
      for( uint32_t i = 0; i < Params.shlibs.size() && !error; i++ )
      {
         if( !m_appAddrSpace->loadLibrary( Params.shlibs[i].c_str() ) )
         {
            std::cerr << ExeName << ": [" << ExePid << "]: "
                      << "Error: Could not load shared library "
                      << Params.shlibs[i] << ". Aborting." << std::endl;
            error = true;
         }
      }
      if( error )
         break;

      // read the mutatee image and get an associated image object
      //
      m_appImage = m_appAddrSpace->getImage();
      if( !m_appImage )
      {
         std::cerr << ExeName << ": [" << ExePid << "]: "
                   << "Error: Could not get an image object. Aborting."
                   << std::endl;
         error = true;
         break;
      }

      // search instrumentation functions to be inserted at entry/exit points
      //

      if( !findFunction( "VT_Dyn_start", m_vtStartFunc ) ||
          !findFunction( "VT_Dyn_end", m_vtEndFunc ) )
      {
         std::cerr << ExeName << ": [" << ExePid << "]: "
                   << "Error: Could not find instrumentation functions. "
                   << "Is VampirTrace library linked? Aborting." << std::endl;
         error = true;
         break;
      }

   } while( false );

   return !error;
}

bool
MutatorC::finalize( bool & error )
{
   switch( Params.mode )
   {
      case MODE_CREATE:
      case MODE_ATTACH:
      {
         BPatch_process * app_process =
            dynamic_cast<BPatch_process*>( m_appAddrSpace );

         // either continue execution of mutatee ...
         //
         if( !error )
         {
            vPrint( 1, "Continuing process execution\n" );

            if( !app_process->isStopped() || app_process->isTerminated() )
            {
               std::cerr << ExeName << ": [" << ExePid << "]: "
                         << "Error: Could not continue process execution. "
                         << "Aborting." << std::endl;
               error = true;
            }
            else
            {
               // send mutatee process a signal to continue execution
               //
               if( Params.mutatee_pid != -1 )
                  kill( Params.mutatee_pid, SIGUSR1 );

               if( Params.mode == MODE_CREATE || !Params.detach )
               {
                  // continue execution of mutatee
                  app_process->continueExecution();

                  // wait until mutatee is terminated
                  //
                  while( !app_process->isTerminated() )
                  {
                     m_bpatch.waitForStatusChange();
                     sleep(1);
                  }

                  vPrint( 1, "End of process\n" );
                  vPrint( 1, "Done\n" );
               }
               else // Params.mode == MODE_ATTACH && Params.detach
               {
                  // continue execution of mutatee and detach from its process
                  app_process->detach( true );
               }
            }
         }
         // ... or terminate execution on error
         //
         else
         {
            if( app_process && !app_process->isTerminated() )
               app_process->terminateExecution();
         }

         break;
      }
      case MODE_REWRITE:
      {
         if( !error )
         {
            vPrint( 1, "Writing %s\n", Params.outfile.c_str() );

            BPatch_binaryEdit * app_editor =
               dynamic_cast<BPatch_binaryEdit*>(m_appAddrSpace);

            // rewrite instrumented binary
            //
            if( !app_editor->writeFile( Params.outfile.c_str() ) )
            {
               std::cerr << ExeName << ": [" << ExePid << "]: "
                         << "Error: Could not write " << Params.outfile
                         << ". Aborting." << std::endl;
               error = true;
            }
            else
            {
               vPrint( 1, "Done\n" );
            }
         }

         break;
      }
   }

   // free RFG filter object, if necessary
   if( m_filter )
      RFG_Filter_free( m_filter );

   return !error;
}

bool
MutatorC::getFunctions( std::vector<InstFuncS> & instFuncs )
{
   bool error = false;

   vPrint( 1, "Get instrumentable functions\n" );

   do
   {
      // get list of modules from image
      //
      const BPatch_Vector<BPatch_module*> * modules = m_appImage->getModules();
      if( !modules )
      {
         std::cerr << ExeName << ": [" << ExePid << "]: "
                   << "Error: Could not get modules of image. Aborting."
                   << std::endl;
         error = true;
         break;
      }

      // iterate over all modules
      for( uint32_t i = 0; i < modules->size() && !error; i++ )
      {
         // get module name
         //

         std::string module_name;
         char buffer[STRBUFSIZE] = "";

         (*modules)[i]->getName( buffer, STRBUFSIZE );
         module_name = buffer;

         // check whether module should be instrumented
         //
         if( constraintModule( module_name ) )
         {
            vPrint( 2, " Skip module '%s'\n", module_name.c_str() );
            continue;
         }

         // get functions of module
         //

         const BPatch_Vector<BPatch_function*> * functions =
            (*modules)[i]->getProcedures();

         if( !functions )
         {
            std::cerr << ExeName << ": [" << ExePid << "]: "
                      << "Error: Could not get functions of module "
                      << module_name << ". Aborting." << std::endl;
            error = true;
            break;
         }

         // iterate over all functions
         for( uint32_t j = 0; j < functions->size(); j++ )
         {
            // get function name
            //

            std::string function_name;

            (*functions)[j]->getName( buffer, STRBUFSIZE );
            function_name = buffer;

            // check whether function is instrumentable
            //
            if( !(*functions)[j]->isInstrumentable() )
            {
               vPrint( 2, " Skip function '%s' (not instrumentable)\n",
                       function_name.c_str() );
               continue;
            }

            // get function entry points
            //
            const BPatch_Vector<BPatch_point*>* entry_points =
               (*functions)[j]->findPoint( BPatch_entry );
            if( !entry_points || entry_points->size() == 0 )
            {
               vPrint( 2, " Skip function '%s' "
                          "(no entry instrumentation points found)\n",
                       function_name.c_str() );
               continue;
            }

            // get function exit points
            //
            const BPatch_Vector<BPatch_point*>* exit_points =
               (*functions)[j]->findPoint( BPatch_exit );
            if( !exit_points || exit_points->size() == 0 )
            {
               vPrint( 2, " Skip function '%s' "
                          "(no exit instrumentation points found)\n",
                       function_name.c_str() );
               continue;
            }

            // check whether function should be instrumented
            //
            if( constraintFunction( function_name ) )
            {
               vPrint( 2, " Skip function '%s'\n", function_name.c_str() );
               continue;
            }

            // get address, source file, and line number of function
            //

            std::vector<BPatch_statement> statements;
            unsigned long addr;
            const char* file_name = 0;
            int line_number = 0;

            addr = (unsigned long)(*functions)[j]->getBaseAddr();
            (*modules)[i]->getSourceLines( addr, statements );

            if( !statements.empty() )
            {
               file_name = statements[0].fileName();
               line_number = statements[0].lineNumber();
            }

            if( file_name )
            {
               if( constraintModule( file_name ) )
               {
                  vPrint( 2, " Skip function '%s'\n", function_name.c_str() );
                  continue;
               }
            }
            else
            {
               if( Params.ignore_no_dbg )
               {
                  vPrint( 2, " Skip function '%s' (no debug information)\n",
                          function_name.c_str() );
                  continue;
               }

               file_name = "";
               line_number = 0;
            }

            vPrint( 2, " Add function '%s' for instrumenting\n",
                    function_name.c_str() );

            // get function index
            //
            uint32_t function_index = instFuncs.size();
            if( function_index + 1 > VT_MAX_DYNINST_REGIONS )
            {
               std::cerr << ExeName << ": [" << ExePid << "]: "
                         << "Error: Too many functions to instrument (max. "
                         << VT_MAX_DYNINST_REGIONS << "). Aborting."
                         << std::endl;
               error = true;
               break;
            }

            // add function for instrumenting
            instFuncs.push_back(
               InstFuncS( function_index, function_name, file_name,
                          line_number, entry_points, exit_points ) );
         }
      }

   } while( false );

   return !error;
}

bool
MutatorC::instrumentFunctionEntry( const InstFuncS & instFunc )
{
   bool error = false;

   vPrint( 2, " Instrumenting-> '%s' Entry\n", instFunc.name.c_str() );

   // set callee arguments
   //

   static BPatch_Vector<BPatch_snippet*> callee_args( 4 );

   // function index
   //
   BPatch_constExpr const_expr_findex( instFunc.index );
   callee_args[0] = &const_expr_findex;

   // function name
   //
   BPatch_constExpr const_expr_fname( instFunc.name.c_str() );
   callee_args[1] = &const_expr_fname;

   // source file name
   //
   BPatch_constExpr const_expr_file( instFunc.file.c_str() );
   callee_args[2] = &const_expr_file;

   // line number
   //
   BPatch_constExpr const_expr_lno( instFunc.lno );
   callee_args[3] = &const_expr_lno;

   // create instrumentation snippet
   BPatch_snippet snippet = BPatch_funcCallExpr( *m_vtStartFunc, callee_args );

   // insert instrumentation snippet
   //
   if( !m_appAddrSpace->insertSnippet( snippet, *(instFunc.entry_points),
          BPatch_callBefore, BPatch_lastSnippet ) )
   {
      std::cerr << ExeName << ": [" << ExePid << "]: "
                << "Error: Could not instrument entry points of "
                << "function '" << instFunc.name << "'. Aborting."
                << std::endl;
      error = true;
   }

   return !error;
}

bool
MutatorC::instrumentFunctionExit( const InstFuncS & instFunc )
{
   bool error = false;

   vPrint( 2, " Instrumenting-> '%s' Exit\n", instFunc.name.c_str() );

   // set callee argument
   //

   static BPatch_Vector<BPatch_snippet*> callee_args( 1 );

   // function index
   //
   BPatch_constExpr const_expr_findex( instFunc.index );
   callee_args[0] = &const_expr_findex;

   // create instrumentation snippet
   BPatch_snippet snippet = BPatch_funcCallExpr( *m_vtEndFunc, callee_args );

   // insert instrumentation snippet
   //
   if( !m_appAddrSpace->insertSnippet( snippet, *(instFunc.exit_points),
          BPatch_callAfter, BPatch_lastSnippet ) )
   {
      std::cerr << ExeName << ": [" << ExePid << "]: "
                << "Error: Could not instrument exit points of "
                << "function '" << instFunc.name << "'. Aborting."
                << std::endl;
      error = true;
   }

   return !error;
}

bool
MutatorC::readFilter()
{
   bool error = false;

   if( Params.filtfile.length() == 0 )
      return true;

   do
   {
      vPrint( 1, "Reading filter file\n" );

      // get RFG filter object
      m_filter = RFG_Filter_init();
      assert( m_filter );

      // set input filter file name
      RFG_Filter_setDefFile( m_filter, Params.filtfile.c_str() );

      // read input filter file to get global filter rules (rank = -1)
      if( ( error = !RFG_Filter_readDefFile( m_filter, -1, 0 ) ) )
         break;

      // NOTE: at this point one can add some default filter rules
      //

      // examples:

      //RFG_Filter_add( m_filter, "foo", 0 );
      //RFG_Filter_add( m_filter, "bar", 0 );

      // instrumenting this function generated by the Intel compiler
      // results in a segmentation fault
      RFG_Filter_add( m_filter, "__intel_cpu_indicator_init", 0 );

   } while( false );

   return !error;
}

bool
MutatorC::constraintModule( const std::string & name ) const
{
   int len = name.length();

   if( name.compare( "DEFAULT_MODULE" ) == 0 ||
       name.compare( "LIBRARY_MODULE" ) == 0 ||
       ( ( name.compare( 0, 3, "vt_" ) != 0     &&
           name.compare( 0, 4, "rfg_" ) != 0    &&
           name.compare( 0, 4, "OTF_" ) != 0    &&
           name.compare( 0, 5, "pomp_" ) != 0 ) &&
         ( ( len >= 2 && name.compare( len-2, 2, ".c" ) == 0 )   ||
           ( len >= 2 && name.compare( len-2, 2, ".C" ) == 0 )   ||
           ( len >= 3 && name.compare( len-3, 3, ".cc" ) == 0 )  ||
           ( len >= 4 && name.compare( len-4, 4, ".cpp" ) == 0 ) ||
           ( len >= 2 && name.compare( len-2, 2, ".f" ) == 0 )   ||
           ( len >= 2 && name.compare( len-2, 2, ".F" ) == 0 )   ||
           ( len >= 4 && name.compare( len-4, 4, ".f77" ) == 0 ) ||
           ( len >= 4 && name.compare( len-4, 4, ".F77" ) == 0 ) ||
           ( len >= 4 && name.compare( len-4, 4, ".f90" ) == 0 ) ||
           ( len >= 4 && name.compare( len-4, 4, ".F90" ) == 0 ) ||
           ( len >= 4 && name.compare( len-4, 4, ".f95" ) == 0 ) ||
           ( len >= 4 && name.compare( len-4, 4, ".F95" ) == 0 ) ) ) )
   {
      return false;   // ok, module should be instrumented
   }
   else
   {
      // check for user specified constraints on modules
      //
      std::vector<std::string>::const_iterator it =
         std::find( Params.shlibs.begin(), Params.shlibs.end(), name );

      return it == Params.shlibs.end();
   }
}

bool
MutatorC::constraintFunction( const std::string & name ) const
{
   if( isMPI() && name.compare( 0, 4, "MPI_" ) == 0 )
   {
      return true;           // don't instrument MPI functions
                             // (already done by function wrapper)
   }
   else if( name.compare( 0, 7, "UNIMCI_" ) == 0 )
   {
      return true;
   }
   else if( m_filter )
   {
      int32_t limit;
      RFG_Filter_get( m_filter, name.c_str(), &limit );

      return ( limit == 0 ); // don't instrument function if call limit is 0
   }
   else
   {
      return false;          // ok, function should be instrumented
   }
}

bool
MutatorC::isMPI() const
{
   static int is_mpi = -1;

   if( is_mpi == -1 )
   {
      BPatch_function * func = 0;
      if( findFunction( "MPI_Init", func ) )
         is_mpi = 1;
      else
         is_mpi = 0;
   }

   return is_mpi == 1 ? true : false;
}

bool
MutatorC::findFunction( const std::string & name,
                        BPatch_function *& func ) const
{
   BPatch_Vector<BPatch_function*> found_funcs;

   m_appImage->findFunction( name.c_str(), found_funcs, false );

   if( !found_funcs.empty() )
   {
      func = found_funcs[0];
      return true;
   }
   else
   {
      func = 0;
      return false;
   }
}
