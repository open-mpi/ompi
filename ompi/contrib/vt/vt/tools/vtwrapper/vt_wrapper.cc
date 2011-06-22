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

#include "vt_wrapper.h"

static char * ExeName;

int
main( int argc, char ** argv )
{
   int rc;

   // get name of executable
   if( ( ExeName = strrchr( argv[0], '/' ) ) )
      ExeName++;
   else
      ExeName = argv[0];

   // create instance of wrapper (initialize)
   Wrapper * p_wrapper = new Wrapper();
   assert( p_wrapper );

   // read wrapper's data file
   if( !p_wrapper->readDataFile() )
      return 1;

   // read environment variables
   if( !p_wrapper->readEnvironmentVars() )
      return 1;

   // parse command line
   if( !p_wrapper->parseCommandLine( argc, argv ) )
      return 1;

   // start compiling/linking
   rc = p_wrapper->run();

   delete p_wrapper;
   return rc;
}


//////////////////// class Wrapper ////////////////////

// public methods
//

Wrapper::Wrapper()
{
   m_pConfig = new Config();
   assert( m_pConfig );
}

Wrapper::~Wrapper()
{
   delete m_pConfig;
}

bool
Wrapper::readDataFile()
{
   bool error = false;

   const std::string data_file =
     std::string(vt_installdirs_get(VT_INSTALLDIR_DATADIR)) + "/" +
     std::string(ExeName) + "-wrapper-data.txt";

   const uint32_t keys_num = 29;
   const std::string keys[] = {
      "version", "language", "compiler_env", "compiler_flags_env",
      "compiler", "compiler_flags", "linker_flags", "libs", "includedir",
      "libdir", "vtlib", "vtmpilib", "vtmtlib", "vthyblib", "vtpomplib",
      "vtdynattlib", "opari_bin", "opari_opts", "opari_tab_compiler",
      "opari_tab_compiler_flags", "compinst_compiler_flags",
      "dyninst_compiler_flags", "tauinst_bin", "tauinst_opts",
      "tauinst_parse_bin", "tauinst_parse_opts",
      "inst_avail", "inst_default", "partype_default"
   };

   std::ifstream in( data_file.c_str() );
   if( !in )
   {
      std::cerr << ExeName << ": error: could not open configuration file "
                << data_file << std::endl;
      return false;
   }

   char buffer[1024];
   std::string line;
   uint32_t line_no = 0;
   uint32_t key_idx = 0;

   while( key_idx < keys_num 
          && in.getline( buffer, sizeof( buffer ) ) )
   {
      line_no++;

      if( buffer[0] == '#' || buffer[0] == '\n' || buffer[0] == '\0' )
         continue;
      if( buffer[strlen(buffer)-1] == '\n' )
         buffer[strlen(buffer)-1] = '\0';

      line = buffer;

      std::string::size_type valpos = line.find( "=" );
      std::string key;
      std::string value;

      if( valpos == std::string::npos || valpos < 1 )
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

      // expand install directories from value except keys 1-4
      //
      if( key_idx > 4 )
      {
         char* tmp = vt_installdirs_expand( value.c_str() );
         if( !tmp )
         {
            std::cerr << ExeName << ": "
                      << data_file << ":" << line_no << ": "
                      << "could not be parsed" << std::endl;
            error = true;
            break;
         }

         value = tmp;
         free( tmp );
      }

      switch( key_idx )
      {
         case 1: // version
         {
            m_pConfig->m_sVT_Version = value;
            break;
         }
         case 2: // language
         {
            if( value.compare( "C" ) == 0 )
               error = !m_pConfig->setLanguage( LANG_CC );
            else if( value.compare( "C++" ) == 0 )
               error = !m_pConfig->setLanguage( LANG_CXX );
            else if( value.compare( "Fortran 77" ) == 0 )
               error = !m_pConfig->setLanguage( LANG_F77 );
            else if( value.compare( "Fortran 90" ) == 0 )
               error = !m_pConfig->setLanguage( LANG_F90 );
            else
            {
               std::cerr << ExeName << ": "
                         << data_file << ":" << line_no << ": "
                         << "unknown language '" << value << "'" << std::endl;
               error = true;
            }
            break;
         }
         case 3: // compiler_env
         {
            m_pConfig->m_sComp_CmdEnv = value;
            break;
         }
         case 4: // compiler_flags_env
         {
            m_pConfig->m_sComp_FlagsEnv = value;
            break;
         }
         case 5: // compiler
         {
            m_pConfig->compiler_setCmd( value );
            break;
         }
         case 6: // compiler_flags
         {
            m_pConfig->m_sComp_Flags = value;
            break;
         }
         case 7: // linker flags
         {
            m_pConfig->m_sComp_LdFlags = value;
            break;
         }
         case 8: // libs
         {
            m_pConfig->m_sComp_Libs = value;
            break;
         }
         case 9: // includedir
         {
            m_pConfig->m_sVT_IncDir = "-I" + value;
            break;
         }
         case 10: // libdir
         {
            m_pConfig->m_sVT_LibDir = "-L" + value;
            break;
         }
         case 11: // vtlib
         {
            m_pConfig->m_sVT_SeqLib = value;
            break;
         }
         case 12: // vtmpilib
         {
            m_pConfig->m_sVT_MpiLib = value;
            break;
         }
         case 13: // vtmtlib
         {
            m_pConfig->m_sVT_MtLib = value;
            break;
         }
         case 14: // vthyblib
         {
            m_pConfig->m_sVT_HybLib = value;
            break;
         }
         case 15: // vtpomplib
         {
            m_pConfig->m_sVT_PompLib = value;
            break;
         }
         case 16: // vtdynattlib
         {
            m_pConfig->m_sVT_DynAttLib = value;
            break;
         }
         case 17: // opari_bin
         {
            m_pConfig->m_sOpari_Cmd = value;
            break;
         }
         case 18: // opari_opts
         {
           m_pConfig->m_sOpari_Args = value;
           break;
         }
         case 19: // opari_tab_compiler
         {
            m_pConfig->m_sOpari_TabCompCmd = value;
            break;
         }
         case 20: // opari_tab_compiler_flags
         {
            m_pConfig->m_sOpari_TabCompFlags = value;
            break;
         }
         case 21: // compinst_compiler_flags
         {
            m_pConfig->m_sCompInst_Flags = value;
            break;
         }
         case 22: // dyninst_compiler_flags
         {
            m_pConfig->m_sDynInst_Flags = value;
            break;
         }
         case 23: // tauinst_bin
         {
            m_pConfig->m_sTauInst_Cmd = value;
            break;
         }
         case 24: // tauinst_opts
         {
            m_pConfig->m_sTauInst_Args = value;
            break;
         }
         case 25: // tauinst_parse_bin
         {
            m_pConfig->m_sTauInst_ParseCmd = value;
            break;
         }
         case 26: // tauinst_parse_opts
         {
            m_pConfig->m_sTauInst_ParseArgs = value;
            break;
         }
         case 27: // inst_avail
         {
            char cvalue[128];
            strncpy( cvalue, value.c_str(), sizeof( cvalue ) - 1 );
            cvalue[sizeof(cvalue) - 1]  = '\0';

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
               if( !m_pConfig->setInstAvail( token ) )
               {
                  std::cerr << ExeName << ": "
                            << data_file << ":" << line_no << ": "
                            << "unknown instrumentation type '"
                            << token << "'" << std::endl;
                  error = true;
                  break;
               }
            } while( ( token = strtok( 0, " " ) ) );

            break;
         }
         case 28: // inst_default
         {
            if( !m_pConfig->setInstType( value ) )
            {
               std::cerr << ExeName << ": "
                         << data_file << ":" << line_no << ": "
                         << "unknown or not supported instrumentation type '"
                         << value << "'" << std::endl;
               error = true;
            }
            break;
         }
         case 29: // partype_default
         {
            if( value.compare( "seq" ) == 0 )
            {
            }
            else if( value.compare( "mpi" ) == 0 )
            {
               m_pConfig->setUsesMpi( true );
            }
            else if( value.compare( "mt" ) == 0 )
            {
               m_pConfig->setUsesThreads( true );
            }
            else if( value.compare( "hyb" ) == 0 )
            {
               m_pConfig->setUsesMpi( true );
               m_pConfig->setUsesThreads( true );
            }
            else
            {
               std::cerr << ExeName << ": "
                         << data_file << ":" << line_no << ": "
                         << "unknown parallelization type '"
                         << value << "'" << std::endl;
               error = true;
            }
            break;
         }
         default:
         {
            std::cerr << ExeName << ": "
                      << data_file << ":" << line_no << ": "
                      << "could not be parsed" << std::endl;
            error = true;
            break; 
         }
      }

      if( error ) break;
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

bool
Wrapper::readEnvironmentVars()
{
   char * env;

   // read environment var. for compiler command
   // (VT_<CC|CXX|F77|F90>)
   //
   env = getenv( m_pConfig->m_sComp_CmdEnv.c_str() );
   if( env ) m_pConfig->compiler_setCmd( env );

   // read environment var. for extra compiler flags
   // (VT_<C|CXX|F|FC>FLAGS)
   env = getenv( m_pConfig->m_sComp_FlagsEnv.c_str() );
   if( env )
   {
      if( m_pConfig->m_sComp_Flags.length() > 0 )
         m_pConfig->m_sComp_Flags += " ";
      m_pConfig->m_sComp_Flags += env;
   }

   // read environment var. for extra linker flags
   // (VT_LDFLAGS)
   env = getenv( "VT_LDFLAGS" );
   if( env ) m_pConfig->m_sComp_LdFlags = env;

   // read environment var. for extra libs
   // (VT_LIBS)
   env = getenv( "VT_LIBS" );
   if( env )
   {
      if( m_pConfig->m_sComp_Libs.length() > 0 )
         m_pConfig->m_sComp_Libs += " ";
      m_pConfig->m_sComp_Libs += env;
   }

   // read environment var. for instrumentation type
   // (VT_INST)
   //
   env = getenv( "VT_INST" );
   if( env )
   {
      if( !m_pConfig->setInstType( env ) )
      {
         std::cerr << ExeName << ": error: VT_INST: "
                   << "unknown or not supported instrumentation type '"
                   << env << "'" << std::endl;
         return false;
      }
   }

   return true;
}

bool
Wrapper::parseCommandLine( int argc, char ** argv )
{
   std::vector<std::string> args;
   std::string arg;
   uint32_t i;

   // pre-process arguments
   //
   for( i = 1; i < (uint32_t)argc; i++ )
   {
      arg = argv[i];

      // we also accept "--vt:" - modify "--vt:" to "-vt:"
      //
      if( arg.compare(0,5,"--vt:") == 0 )
      {
         arg.erase(0,1);
      }

      // merge separated compiler arguments
      // -<I|D|L|l> <dir|lib>
      //             ^
      if( arg.compare( "-I" ) == 0
          || arg.compare( "-D" ) == 0
          || arg.compare( "-L" ) == 0
          || arg.compare( "-l" ) == 0 )
      {
         if( i < (uint32_t)argc - 1 )
            arg += argv[++i];
      }

      args.push_back( arg );
   }

   for( i = 0; i < args.size(); i++ )
   {
      arg = args[i];

      //
      // -vt:help
      //
      if( arg.compare("-vt:help") == 0 )
      {
         showUsageText();
         exit(0);
      }
      //
      // -vt:version
      //
      if( arg.compare("-vt:version") == 0 )
      {
         showVersion();
         exit(0); 
      }
      //
      // -vt:show
      //
      else if( arg.compare("-vt:show") == 0 )
      {
         m_pConfig->m_bShow = true;
      }
      //
      // -vt:verbose
      //
      else if( arg.compare("-vt:verbose") == 0 )
      {
         m_pConfig->m_bBeVerbose = true;
      }
      //
      // -vt:inst <type>
      //
      else if( arg.compare("-vt:inst") == 0 )
      {
         if( i == args.size() - 1 )
         {
            std::cerr << ExeName << ": <type> expected -- -vt:inst"
                      << std::endl;
            return false;
         }

         arg = args[++i];

         if( !m_pConfig->setInstType( arg ) )
         {
            std::cerr << ExeName << ": "
                      << "unknown or not supported instrumentation type '"
                      << arg << "'" << std::endl;
            return false;
         }
      }
      //
      // -vt:opari <args>
      //
      else if( arg.compare("-vt:opari") == 0 )
      {
         if( i == args.size() - 1 )
         {
            std::cerr << ExeName << ": <args> expected -- -vt:opari"
                      << std::endl;
            return false;
         }

         size_t opari_args_len = args[i+1].length()+1;
         char * opari_args = new char[opari_args_len];
         strncpy( opari_args, args[++i].c_str(), opari_args_len - 1 );
         opari_args[opari_args_len - 1] = '\0';

         char * token = strtok( opari_args, " " );
         do
         {
            if( strcmp( token, "-rcfile" ) == 0 )
            {
               token = strtok( 0, " " );
               if( !token )
               {
                  std::cerr << ExeName << ": <rcfile> expected -- -rcfile"
                            << std::endl;
                  delete [] opari_args;
                  return false;
               }
               m_pConfig->opari_setRcFile( token );
            }
            else if( strcmp( token, "-table" ) == 0 )
            {
               token = strtok( 0, " " );
               if( !token )
               {
                  std::cerr << ExeName << ": <tabfile> expected -- -table"
                            << std::endl;
                  delete [] opari_args;
                  return false;
               }

               m_pConfig->opari_setTabFile( token );
            }
            else
            {
               m_pConfig->opari_addArg( token );
            }
         } while( ( token = strtok( 0, " " ) ) );

         delete [] opari_args;
      }
      //
      // -vt:noopari
      //
      else if( arg.compare("-vt:noopari") == 0 )
      {
         m_pConfig->setUsesOpenMP( false, true );
      }
      //
      // -vt:tau <args>
      //
      else if( arg.compare("-vt:tau") == 0 )
      {
         if( i == args.size() - 1 )
         {
            std::cerr << ExeName << ": <args> expected -- -vt:tau"
                      << std::endl;
            return false;
         }

         m_pConfig->tauinst_addArg( args[++i] );
      }
      //
      // -vt:pdt <args>
      //
      else if( arg.compare("-vt:pdt") == 0 )
      {
         if( i == args.size() - 1 )
         {
            std::cerr << ExeName << ": <args> expected -- -vt:pdt"
                      << std::endl;
            return false;
         }

         m_pConfig->tauinst_addParseArg( args[++i] );
      }
      //
      // -vt:seq
      //
      else if( arg.compare("-vt:seq") == 0 )
      {
         m_pConfig->setUsesMpi( false, true );
         m_pConfig->setUsesThreads( false, true );
         m_pConfig->setUsesOpenMP( false, true );
      }
      //
      // -vt:mpi
      //
      else if( arg.compare("-vt:mpi") == 0 )
      {
         m_pConfig->setUsesMpi( true, true );
         m_pConfig->setUsesThreads( false, true );
         m_pConfig->setUsesOpenMP( false, true );
      }
      //
      // -vt:mt
      //
      else if( arg.compare("-vt:mt") == 0 )
      {
         m_pConfig->setUsesMpi( false, true );
         m_pConfig->setUsesThreads( true, true );
      }
      //
      // -vt:hyb
      //
      else if( arg.compare("-vt:hyb") == 0 )
      {
         m_pConfig->setUsesMpi( true, true );
         m_pConfig->setUsesThreads( true, true );
      }
      //
      // pthread flags/libs
      //
      else if( arg.compare( "-Kthread" ) == 0
               || arg.compare( "-kthread" ) == 0
               || arg.compare( "-pthread" ) == 0
               || arg.compare( "-pthreads" ) == 0
               || arg.compare( "-mthreads" ) == 0
               || arg.compare( "--thread-safe" ) == 0
               || arg.compare( "-lpthreads" ) == 0
               || arg.compare( "-llthread" ) == 0
               || arg.compare( "-lpthread" ) == 0 )
      {
         m_pConfig->setUsesThreads( true );
      }
      //
      // openmp flag
      //
      else if( arg.compare( "-openmp" ) == 0
               || arg.compare( "-fopenmp" ) == 0
               || arg.compare( "-Popenmp" ) == 0
               || arg.compare( "-xopenmp" ) == 0
               || arg.compare( "-mp" ) == 0
               || arg.compare( "-qsmp=omp" ) == 0 )
      {
         m_pConfig->setUsesThreads( true );
         m_pConfig->setUsesOpenMP( true );
      }
      //
      // nvcc's pthread/openmp flag
      //
      else if( arg.compare( 0, 10, "-Xcompiler" ) == 0
               || arg.compare( 0, 18, "--compiler-options" ) == 0
               || arg.compare( 0, 8, "-Xlinker" ) == 0
               || arg.compare( 0, 16, "--linker-options" ) == 0 )
      {
         if( arg.find( "-pthread" ) != std::string::npos )
            m_pConfig->setUsesThreads( true );

         if( arg.find( "-fopenmp" ) != std::string::npos )
         {
            m_pConfig->setUsesThreads( true );
            m_pConfig->setUsesOpenMP( true );
         }
      }
   }

   bool addlibs = false;

   for( i = 0; i < args.size(); i++ )
   {
      arg = args[i];

      // escape spaces and double quotes
      //
      size_t found = arg.find_first_of(" \"");
      while(found!=std::string::npos)
      {
         arg.insert(found,"\\");
         found = arg.find_first_of(" \"",found+2);
      }

      //
      // -vt:help, -vt:version, -vt:show, -vt:seq, -vt:mpi, -vt:mt, -vt:hyb,
      // -vt:inst, -vt:opari, -vt:tau, -vt:pdt
      // (processed above; ignore here)
      //
      if( arg.compare("-vt:help") == 0 
          || arg.compare("-vt:version") == 0
          || arg.compare("-vt:show") == 0
          || arg.compare("-vt:verbose") == 0
          || arg.compare("-vt:seq") == 0
          || arg.compare("-vt:mpi") == 0
          || arg.compare("-vt:mt") == 0
          || arg.compare("-vt:hyb") == 0
          || arg.compare("-vt:inst") == 0
          || arg.compare("-vt:opari") == 0
          || arg.compare("-vt:noopari") == 0
          || arg.compare("-vt:tau") == 0
          || arg.compare("-vt:pdt") == 0 )
      {
         // do nothing

         // skip next argument, if necessary
         if( arg.compare("-vt:inst") == 0
             || arg.compare("-vt:opari") == 0
             || arg.compare("-vt:tau") == 0
             || arg.compare("-vt:pdt") == 0 )
           i++;
      }
      //
      // -vt:<cc|cxx|f77|f90> <cmd>
      //
      else if( (m_pConfig->m_eLangType == LANG_CC && arg.compare("-vt:cc") == 0)
               || (m_pConfig->m_eLangType == LANG_CXX && arg.compare("-vt:cxx") == 0)
               || (m_pConfig->m_eLangType == LANG_F77 && arg.compare("-vt:f77") == 0)
               || (m_pConfig->m_eLangType == LANG_F90 && arg.compare("-vt:f90") == 0) )
      {
         if( i == args.size() - 1 )
         {
            std::cerr << ExeName << ": <cmd> expected -- "
                      << arg << std::endl;
            return false;
         }

         m_pConfig->compiler_setCmd( args[++i] );
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
                    && arg.compare( arg.length() - 4, 4, ".F95" ) == 0 )
               || ( arg.length() >= 3
                    && arg.compare( arg.length() - 3, 3, ".cu" ) == 0 ) )
      {
         m_pConfig->compiler_addSrcFile( arg );
      }
      //
      // -<I|D>*
      //
      else if( arg.compare( 0, 2, "-I" ) == 0
               || arg.compare( 0, 2, "-D" ) == 0 )
      {
         if( m_pConfig->m_eInstType == INST_TYPE_TAUINST )
            m_pConfig->tauinst_addParseArg( arg );
         m_pConfig->compiler_addArg( arg );
      }
      //
      // -WF,-D*
      //
      else if( ( m_pConfig->m_eLangType == LANG_F77
                 || m_pConfig->m_eLangType == LANG_F90 )
               && arg.compare( 0, 6, "-WF,-D" ) == 0 )
      {
         if( m_pConfig->m_eInstType == INST_TYPE_TAUINST )
            m_pConfig->tauinst_addParseArg( arg.substr(4) );
         m_pConfig->compiler_addArg( arg );
      }
      //
      // Fortran line length flag
      //
      else if( ( m_pConfig->m_eLangType == LANG_F77
                 || m_pConfig->m_eLangType == LANG_F90 )
               && ( arg.compare( "-ffixed-line-length-132" ) == 0
                    || arg.compare( "-extend_source" ) == 0
                    || arg.compare( "-Mextend" ) == 0
                    || arg.compare( "-e" ) == 0
                    || arg.compare( "-qfixed=132" ) == 0 ) )
      {
         if( m_pConfig->m_eInstType == INST_TYPE_TAUINST )
            m_pConfig->tauinst_addParseArg( "-ffixed-line-length-132" );
         m_pConfig->compiler_addArg( arg );
      }
      //
      // Fortran free format flag
      //
      else if( ( m_pConfig->m_eLangType == LANG_F77
                 || m_pConfig->m_eLangType == LANG_F90 )
               && ( arg.compare( "-ffree-form" ) == 0
                    || arg.compare( "-free" ) == 0
                    || arg.compare( "-Mfree" ) == 0
                    || arg.compare( 0, 6, "-qfree" ) == 0 ) )
      {
         if( m_pConfig->m_eInstType == INST_TYPE_TAUINST
             && m_pConfig->m_sTauInst_ParseCmd.compare( "f95parse" ) == 0 )
            m_pConfig->tauinst_addParseArg( "-R free" );
         m_pConfig->compiler_addArg( arg );
      }
      //
      // Fortran fixed format flag
      //
      else if( ( m_pConfig->m_eLangType == LANG_F77
                 || m_pConfig->m_eLangType == LANG_F90 )
               && ( arg.compare( "-ffixed-form" ) == 0
                    || arg.compare( "-fixed" ) == 0
                    || arg.compare( "-Mfixed" ) == 0
                    || arg.compare( 0, 7, "-qfixed" ) == 0 ) )
      {
         if( m_pConfig->m_eInstType == INST_TYPE_TAUINST
             && m_pConfig->m_sTauInst_ParseCmd.compare( "f95parse" ) == 0 )
            m_pConfig->tauinst_addParseArg( "-R fixed" );
         m_pConfig->compiler_addArg( arg );
      }
      //
      // -c
      //
      else if( arg.compare( "-c" ) == 0 )
      {
         m_pConfig->m_bCompOnly = true;
         m_pConfig->compiler_addArg( arg );
      }
      //
      // -o
      //
      else if( arg.compare( "-o" ) == 0 )
      {
         m_pConfig->m_bOutfileGiven = true;
         m_pConfig->compiler_addArg( arg );
      }
      //
      // -lcuda[rt]
      //
      else if( arg.compare( "-lcuda" ) == 0
               || arg.compare( "-lcudart" ) == 0 )
      {
         m_pConfig->compiler_addLib( arg );
      }
      //
      // -l<mpilib>
      //
      else if( arg.compare( 0, 5, "-lmpi" ) == 0
               || arg.compare( 0, 7, "-lmtmpi" ) == 0
               || arg.compare( 0, 7, "-lhpmpi" ) == 0
               || arg.compare( 0, 7, "-lscmpi" ) == 0 )
      {
         m_pConfig->setUsesMpi( true );
         m_pConfig->compiler_addLib( arg );
         addlibs = true;
      }
      //
      // -<L|l>*
      //
      else if( arg.compare( 0, 2, "-L" ) == 0
               || arg.compare( 0, 2, "-l" ) == 0 )
      {
         if( addlibs )
            m_pConfig->compiler_addLib( arg );
         else
            m_pConfig->compiler_addArg( arg );
      }
      //
      // unknown argument
      //
      else
      {
         m_pConfig->compiler_addArg( arg );
      }
   }

   return true;
}

int
Wrapper::run()
{
   std::string cmd;
   int rc = 0;

   uint32_t i;

   // get frequently used members from config class for easier access
   //
   LangTypeT lang_type = m_pConfig->m_eLangType;
   InstTypeT inst_type = m_pConfig->m_eInstType;
   bool uses_mpi       = m_pConfig->m_bUsesMpi;
   bool uses_threads   = m_pConfig->m_bUsesThreads;
   bool uses_openmp    = m_pConfig->m_bUsesOpenMP;
   bool beverbose      = m_pConfig->m_bBeVerbose;
   bool show           = m_pConfig->m_bShow;

   // vector which holds all intermediately created files
   std::vector<std::string> files_to_remove;

   // map which holds all object files created by the compiler
   // which must be renamed to original file names
   std::map<std::string, std::string> obj_files_to_rename;

   // call compiler without any parameters, if
   // insufficient arguments given
   //
   if( !show
       && m_pConfig->m_sComp_Args.length() == 0
       && m_pConfig->m_vecComp_SrcFiles.size() == 0 )
   {
      rc = system( m_pConfig->m_sComp_Cmd.c_str() );
      return WEXITSTATUS( rc );
   }

   // process collected source files by OPARI and/or TAU
   //
   for( i = 0; i < m_pConfig->m_vecComp_SrcFiles.size(); i++ )
   {
      std::string src_file = m_pConfig->m_vecComp_SrcFiles[i];

      std::string::size_type si;

      // run OPARI command on source file
      //
      if( uses_openmp )
      {
         // build OPARI command
         //
         cmd =
            m_pConfig->m_sOpari_Cmd + " "
            + m_pConfig->m_sOpari_Args + " "
            + "-rcfile "
            + m_pConfig->m_sOpari_RcFile + " "
            + "-table "
            + m_pConfig->m_sOpari_TabFile.first + " "
            + src_file;

         // execute/show OPARI command
         //
         if( ( rc = executeOrShowCommand( cmd ) ) != 0 )
            return rc;

         // create OPARI modified source file name
         //

         std::string mod_file = src_file;
         si = mod_file.rfind( '.' );
         assert( si != std::string::npos );
         mod_file.insert( si, ".mod" );

         // convert Fortran source file suffix to upper case, in order to
         // invoke the C preprocessor before compiling
         //
         if( lang_type == LANG_F77 || lang_type == LANG_F90 )
         {
            si = mod_file.rfind( ".f" );
            if( si != std::string::npos ) mod_file.replace( si, 2, ".F" );
         }

         files_to_remove.push_back( mod_file );

         // create OPARI include file name
         //
         if( lang_type == LANG_CC || lang_type == LANG_CXX )
         {
            std::string inc_file = src_file + ".opari.inc";
            files_to_remove.push_back( inc_file );
         }

         src_file = mod_file;
      }

      // run PDT parser and TAU instrumentor command on source file
      //
      if( inst_type == INST_TYPE_TAUINST )
      {
         // adjust PDT parser options, if source file is instrumented by OPARI
         //
         if( uses_openmp )
         {
            // current directory to find OPARI generated header files
            // (only necessary for C/C++)
            //
            if( lang_type == LANG_CC || lang_type == LANG_CXX )
               m_pConfig->tauinst_addParseArg( "-I." );

            // macro definition '_OPENMP'
            // (the PDT parser has no own option to enable OpenMP)
            m_pConfig->tauinst_addParseArg( "-D_OPENMP" );
         }

         // build PDT parse command
         //
         cmd =
            m_pConfig->m_sTauInst_ParseCmd + " "
            + src_file + " "
            + m_pConfig->m_sVT_IncDir + " "
            + m_pConfig->m_sTauInst_ParseArgs;

         // execute/show PDT parse command
         //
         if( ( rc = executeOrShowCommand( cmd ) ) != 0 )
            return rc;

         // create PDB file name
         //

         std::string pdb_file = src_file;
         si = src_file.rfind( '/' );
         if( si != std::string::npos )
            pdb_file = src_file.substr( si+1 );

         si = pdb_file.rfind( '.' );
         assert( si != std::string::npos );
         pdb_file.replace( si, 4, ".pdb" );

         files_to_remove.push_back( pdb_file );

         // create TAU modified source file name
         //
         std::string tau_file = src_file;
         if( uses_openmp )
         {
            si = tau_file.rfind( ".mod" );
            assert( si != std::string::npos );
            tau_file.replace( si, 4, ".tau" );
         }
         else
         {
            si = tau_file.rfind( '.' );
            assert( si != std::string::npos );
            tau_file.insert( si, ".tau" );

            // convert Fortran source file suffix to upper case, in order to
            // invoke the C preprocessor before compiling
            //
            if( lang_type == LANG_F77 || lang_type == LANG_F90 )
            {
               si = tau_file.rfind( ".f" );
               if( si != std::string::npos ) tau_file.replace( si, 2, ".F" );
            }
         }
         files_to_remove.push_back( tau_file );

         // build TAU instrumentor command
         //
         cmd =
            m_pConfig->m_sTauInst_Cmd + " "
            + pdb_file + " "
            + src_file + " "
            + "-o "
            + tau_file + " "
            + m_pConfig->m_sTauInst_Args;

         // execute/show TAU instrumentor command
         //
         if( ( rc = executeOrShowCommand( cmd ) ) != 0 )
            return rc;

         src_file = tau_file;
      }

      // create and store compiler output file name for renaming, if it's not
      // specified by the command line
      //
      if( m_pConfig->m_bCompOnly && !m_pConfig->m_bOutfileGiven
          && ( uses_openmp || inst_type == INST_TYPE_TAUINST ) )
      {
         std::string obj_file = src_file;

         si = src_file.rfind( '/' );
         if( si != std::string::npos )
            obj_file = src_file.substr( si+1 );

         si = obj_file.rfind( '.' );
         assert( si != std::string::npos );
         obj_file = obj_file.substr( 0, si ) + ".o";

         obj_files_to_rename[obj_file] = m_pConfig->m_vecComp_ObjFiles[i];
      }
   }

   if( uses_openmp )
   {
      // add current working directory to include search path to find OPARI
      // generated header files (only necessary for C/C++)
      //
      if( lang_type == LANG_CC || lang_type == LANG_CXX )
         m_pConfig->compiler_addArg( "-I." );

      // If the source file(s) are instrumented by OPARI *and* TAU, the source
      // code locations determined by TAU are nonsense. Add compiler flag to
      // disable recording of source code locations.
      //
      if( inst_type == INST_TYPE_TAUINST )
      {
         m_pConfig->compiler_addArg(
            m_pConfig->m_sComp_FDFlag + "TAUINST_NOSRC" );
      }
   }

   // compile only ?
   if( m_pConfig->m_bCompOnly )
   {
      // build compiler command
      //
      cmd = m_pConfig->m_sComp_Cmd + " "
         + m_pConfig->m_sVT_IncDir + " "
         + m_pConfig->m_sComp_InstFlags + " "
         + m_pConfig->m_sComp_Flags + " "
         + m_pConfig->m_sComp_Args;

      // execute/show compiler command
      //
      if( ( rc = executeOrShowCommand( cmd ) ) != 0 )
         return rc;

      // rename compiler output files to original file names
      //
      if( !show )
      {
         for( std::map<std::string, std::string>::iterator it =
                 obj_files_to_rename.begin(); it != obj_files_to_rename.end();
                 it++ )
         {
            if( beverbose )
               std::cout << "+++ rename " << it->first
                         << " to " << it->second << std::endl;
            if( rename( it->first.c_str(), it->second.c_str() ) == -1 )
            {
               std::cerr << ExeName << ": could not rename " << it->first
                         << " to " << it->second << std::endl;
               return 1;
            }
         }
      }
   }
   else
   {
      std::string vtlib;

      if( uses_openmp )
      {
         // build command for compiling OPARI table file
         //
         cmd =
            m_pConfig->m_sOpari_TabCompCmd + " "
            + m_pConfig->m_sOpari_TabCompFlags + " "
            + m_pConfig->m_sVT_IncDir + " "
            + ( (lang_type == LANG_F77 || lang_type == LANG_F90) ? "-I. " : "" )
            + "-c " + m_pConfig->m_sOpari_TabFile.first + " "
            + "-o " + m_pConfig->m_sOpari_TabFile.second;

         // execute/show compiler command
         //
         if( ( rc = executeOrShowCommand( cmd ) ) != 0 )
            return rc;

         // add OPARI table object file to to linker arguments
         m_pConfig->compiler_addArg( m_pConfig->m_sOpari_TabFile.second );

         // collect intermediate OPARI output files for removing
         //

         // OPARI include files
         //
         if( lang_type == LANG_F77 || lang_type == LANG_F90 )
         {
            std::vector<std::string> vec_incfiles = getIncFilesFromTabFile();
            for( i = 0; i < vec_incfiles.size(); i++ )
               files_to_remove.push_back( vec_incfiles[i].c_str() );
         }

         // OPARI table source/object file
         //
         files_to_remove.push_back( m_pConfig->m_sOpari_TabFile.first );
         files_to_remove.push_back( m_pConfig->m_sOpari_TabFile.second );

         // OPARI rc file
         //
         if( !m_pConfig->m_bKeepOpariRcFile )
            files_to_remove.push_back( m_pConfig->m_sOpari_RcFile );
      }

      // build compiler command
      //
      vtlib = ( ( inst_type == INST_TYPE_DYNINST ) ?
                m_pConfig->m_sVT_DynAttLib : "" );

      if( uses_threads )
      {
         vtlib += " "
            + ( uses_openmp ?
                m_pConfig->m_sVT_PompLib : "" );

         if( uses_mpi )
            vtlib += " " + m_pConfig->m_sVT_HybLib;
         else
            vtlib += " " + m_pConfig->m_sVT_MtLib;
      }
      else
      {
         if( uses_mpi )
            vtlib += " " + m_pConfig->m_sVT_MpiLib;
         else
            vtlib += " " + m_pConfig->m_sVT_SeqLib;
      }

      cmd =
         m_pConfig->m_sComp_Cmd + " "
         + m_pConfig->m_sVT_IncDir + " "
         + m_pConfig->m_sComp_InstFlags + " "
         + m_pConfig->m_sComp_Flags + " "
         + m_pConfig->m_sComp_Args + " "
         + m_pConfig->m_sComp_LdFlags + " "
         + m_pConfig->m_sVT_LibDir + " "
         + vtlib + " "
         + m_pConfig->m_sComp_Libs;

      // execute/show compiler command
      //
      if( ( rc = executeOrShowCommand( cmd ) ) != 0 )
         return rc;
   }

   // remove intermediate files (in non-verbose mode)
   //
   if( !show && !beverbose )
   {
      for( i = 0; i < files_to_remove.size(); i++ )
         remove( files_to_remove[i].c_str() );
   }

   return 0;
}

// private methods
//

void
Wrapper::showVersion()
{
   std::cout << m_pConfig->m_sVT_Version << std::endl;
}

void
Wrapper::showUsageText()
{
   std::string str_lang;
   std::string str_lang_suffix;

   if( m_pConfig->m_eLangType == LANG_CC )
   {
      str_lang = "C";
      str_lang_suffix = "cc ";
   }
   else if( m_pConfig->m_eLangType == LANG_CXX )
   {
      str_lang = "C++";
      str_lang_suffix = "cxx";
   }
   else if( m_pConfig->m_eLangType == LANG_F77 )
   {
      str_lang = "Fortran 77";
      str_lang_suffix = "f77";
   }
   else if( m_pConfig->m_eLangType == LANG_F90 )
   {
      str_lang = "Fortran 90";
      str_lang_suffix = "f90";
   }

   std::cout << std::endl
             << " " << ExeName << " - " << str_lang
             << " compiler wrapper for VampirTrace."
             << std::endl << std::endl
             << " Syntax: " << ExeName << " "
             << "[-vt:help] "
             << "[-vt:version] "
             << "[-vt:" << str_lang_suffix << " <cmd>] "
             << "[-vt:inst <insttype>] "
             << std::endl << "         "
             << "[-vt:<seq|mpi|mt|hyb>] "
             << "[-vt:opari <args>] [-vt:noopari]" << std::endl
             << "         [-vt:tau <args>] [-vt:pdt <args>] [-vt:verbose] [-vt:show] ..."
             << std::endl << std::endl
             << "   options:"
             << std::endl
             << "     -vt:help            Show this help message."
             << std::endl << std::endl
             << "     -vt:version         Show VampirTrace version."
             << std::endl << std::endl
             << "     -vt:" << str_lang_suffix << " <cmd>       "
             << "Set the underlying compiler command."
             <<  std::endl << std::endl
             << "     -vt:inst <insttype> Set the instrumentation type."
             << std::endl << std::endl
             << "      possible values:"
             << std::endl << std::endl
             << "       compinst          fully-automatic by compiler"
             << std::endl
             << "       manual            manual by using VampirTrace's API"
             << std::endl
             << "       dyninst           binary by using Dyninst"
             << std::endl
             << "       tauinst           automatic source code instrumentation by using PDT/TAU"
             << std::endl << std::endl
             << "     -vt:opari <args>    Set options for the OPARI command."
             << std::endl
             << "                         (see "
             << vt_installdirs_get(VT_INSTALLDIR_DATADIR)
             << "/doc/opari/Readme.html for more information)"
             << std::endl << std::endl
             << "     -vt:noopari         Disable instrumentation of OpenMP contructs by OPARI."
             << std::endl << std::endl


             << "     -vt:tau <args>      Set options for the TAU instrumentor command."
             << std::endl << std::endl
             << "     -vt:pdt <args>      Set options for the PDT parse command."
             << std::endl << std::endl
             << "     -vt:<seq|mpi|mt|hyb>"
             << std::endl
             << "                         Enforce application's parallelization type."
             << std::endl
             << "                         It's only necessary if it could not be determined"
             << std::endl
             << "                         automatically based on underlying compiler and flags."
             << std::endl
             << "                         seq = sequential"
             << std::endl
             << "                         mpi = parallel (uses MPI)"
             << std::endl
             << "                         mt = parallel (uses OpenMP/POSIX threads)"
             << std::endl
             << "                         hyb = hybrid parallel (MPI + Threads)"
             << std::endl
             << "                         (default: automatically)"
             << std::endl << std::endl
             << "     -vt:verbose         Enable verbose mode."
             << std::endl << std::endl
             << "     -vt:show            Do not invoke the underlying compiler."
             << std::endl
             << "                         Instead, show the command line that would be"
             << std::endl
             << "                         executed to compile and link the program."
             << std::endl << std::endl
             << "     See the man page for your underlying compiler for other options that can"
             << std::endl
             << "     be passed through '" << ExeName << "'."
             << std::endl << std::endl
             << "   environment variables:"
             << std::endl
             << "     VT_INST             Instrumentation type (equivalent to '-vt:inst'*)"
             << std::endl;
   if( m_pConfig->m_eLangType == LANG_CC )
   {
      std::cout << "     VT_CC               C compiler command (equivalent to '-vt:"
                << str_lang_suffix << "'*)" << std::endl
                << "     VT_CFLAGS           C compiler flags" << std::endl;
   }
   else if( m_pConfig->m_eLangType == LANG_CXX )
   {
      std::cout << "     VT_CXX              C++ compiler command (equivalent to '-vt:"
                << str_lang_suffix << "'*)" << std::endl
                << "     VT_CXXFLAGS         C++ compiler flags" << std::endl;
   }
   else if( m_pConfig->m_eLangType == LANG_F77 )
   {
      std::cout << "     VT_F77              Fortran 77 compiler command (equivalent to '-vt:"
                << str_lang_suffix << "'*)" << std::endl
                << "     VT_FFLAGS           Fortran 77 compiler flags" << std::endl;
   }
   else if( m_pConfig->m_eLangType == LANG_F90 )
   {
      std::cout << "     VT_F90              Fortran 90 compiler command (equivalent to '-vt:"
                << str_lang_suffix <<"'*)" << std::endl
                << "     VT_FCFLAGS          Fortran 90 compiler flags" << std::endl;
   }
   std::cout << "     VT_LDFLAGS          Linker flags"
             << std::endl
             << "     VT_LIBS             Libraries to pass to the linker"
             << std::endl << std::endl
             << "     * The corresponding command line options overwrites the environment"
             << std::endl
             << "       variables setting."
             << std::endl << std::endl
             << "   examples:"
             << std::endl
             << "     automatically instrumentation by compiler:"
             << std::endl << std::endl;
   if( m_pConfig->m_eLangType == LANG_CC )
   {
      std::cout << "        vtcc -vt:cc gcc -vt:inst compinst -c foo.c -o foo.o"
                << std::endl
                << "        vtcc -vt:cc gcc -vt:inst compinst -c bar.c -o bar.o"
                << std::endl
                << "        vtcc -vt:cc gcc -vt:inst compinst foo.o bar.o -o foo"
                << std::endl << std::endl
                << "     manually instrumentation by using VT's API:"
                << std::endl << std::endl
                << "        vtcc -vt:inst manual foobar.c -o foobar -DVTRACE";
   }
   else if( m_pConfig->m_eLangType == LANG_CXX )
   {
      std::cout << "        vtcxx -vt:cxx g++ -vt:inst compinst -c foo.cpp -o foo.o"
                << std::endl
                << "        vtcxx -vt:cxx g++ -vt:inst compinst bar.cpp -o bar.o"
                << std::endl
                << "        vtcxx -vt:cxx g++ -vt:inst compinst foo.o bar.o -o foo"
                << std::endl << std::endl
                << "     manually instrumentation by using VT's API:"
                << std::endl << std::endl
                << "        vtcxx -vt:inst manual foobar.cpp -o foobar -DVTRACE";
   }
   else if( m_pConfig->m_eLangType == LANG_F77 )
   {
      std::cout << "        vtf77 -vt:f77 g77 -vt:inst compinst -c foo.F -o foo.o"
                << std::endl
                << "        vtf77 -vt:f77 g77 -vt:inst compinst bar.F -o bar.o"
                << std::endl
                << "        vtf77 -vt:f77 g77 -vt:inst compinst foo.o bar.o -o foo"
                << std::endl << std::endl
                << "     manually instrumentation by using VT's API:"
                << std::endl << std::endl
                << "        vtf77 -vt:inst manual foobar.F -o foobar -DVTRACE";
   }
   else if( m_pConfig->m_eLangType == LANG_F90 )
   {
      std::cout << "        vtf90 -vt:f90 gfortran -vt:inst compinst -c foo.F90 -o foo.o"
                << std::endl
                << "        vtf90 -vt:f90 gfortran -vt:inst compinst bar.F90 -o bar.o"
                << std::endl
                << "        vtf90 -vt:f90 gfortran -vt:inst compinst foo.o bar.o -o foo"
                << std::endl << std::endl
                << "     manually instrumentation by using VT's API:"
                << std::endl << std::endl
                << "        vtf90 -vt:inst manual foobar.F90 -o foobar -DVTRACE";
   }

   if( m_pConfig->m_eLangType == LANG_F77 || m_pConfig->m_eLangType == LANG_F90 )
   {
      std::cout << std::endl << std::endl
                << "     IMPORTANT: Fortran source files instrumented by using VT's API"
                << std::endl
                << "                have to be (CPP) preprocessed.";
   }
   std::cout << std::endl << std::endl;
}

int
Wrapper::executeOrShowCommand( std::string & cmd )
{
   int rc = 0;

   // remove double spaces from command
   //
   std::string::size_type i;
   while( ( i = cmd.find( "  " ) ) != std::string::npos )
      cmd.erase( i, 1 );

   // either show
   if( m_pConfig->m_bShow )
   {
     std::cout << cmd << std::endl;
   }
   // or execute command
   else
   {
      if( m_pConfig->m_bBeVerbose )
         std::cout << "+++ " << cmd << std::endl;
      if( ( rc = system( cmd.c_str() ) ) != -1 )
         rc = WEXITSTATUS( rc );
   }

   return rc;
}

std::vector<std::string>
Wrapper::getIncFilesFromTabFile()
{
   std::vector<std::string> vec_incfiles;

   std::ifstream in( m_pConfig->m_sOpari_TabFile.first.c_str() );
   if( in )
   {
      char buffer[1024];
      std::string line;

      while( in.getline( buffer, sizeof( buffer ) ) )
      {
         line = buffer;

         if( line.find( "#include" ) != std::string::npos
            && line.find( ".opari.inc" ) != std::string::npos )
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


//////////////////// class Config ////////////////////

// public methods
//

Config::Config() :
   m_eLangType(LANG_CC), m_eInstType(INST_TYPE_MANUAL), m_iInstAvail(0),

   m_bBeVerbose(false), m_bCompOnly(false), m_bOutfileGiven(false),
   m_bUsesMpi(false), m_bUsesThreads(false),
   m_bUsesOpenMP(false), m_bKeepOpariRcFile(false),
   m_bShow(false)
{
   m_sOpari_RcFile = "opari.rc";
   m_sOpari_TabFile = std::make_pair(std::string("opari.tab.c"),
                                     std::string("opari.tab.o"));
}

Config::~Config()
{
   // empty
}

bool
Config::setLanguage( const LangTypeT lang )
{
#if !(defined(HAVE_F77) && HAVE_F77) || !(defined(HAVE_F90) && HAVE_F90)
   bool error = false;
   std::string str_lang;

   if( lang == LANG_F77 )
   {
#     if !(defined(HAVE_F77) && HAVE_F77)
      str_lang = "Fortran 77";
      error = true;
#     endif // HAVE_F77
   }
   else if( lang == LANG_F90 )
   {
#     if !(defined(HAVE_F90) && HAVE_F90)
      str_lang = "Fortran 90";
      error = true;
#     endif // HAVE_F90
   }

   if( !error )
   {
      m_eLangType = lang;
   }
   else
   {
      std::cerr << "Unfortunately, this installation of VampirTrace "
                << "was not compiled with" << std::endl
                << str_lang << " support.  As such, the " << ExeName
                << " compiler is non-functional." << std::endl;
   }

   return !error;
#else // HAVE_F77 || HAVE_F90
   m_eLangType = lang;
   return true;
#endif // HAVE_F77 || HAVE_F90
}

void
Config::compiler_setCmd( const std::string cmd )
{
   std::string bcomp = cmd;
   std::string::size_type ls = cmd.rfind('/');

   if( ls != std::string::npos ) bcomp = cmd.substr( ls+1 );

   if( !m_bUsesMpi &&
       ( bcomp.compare( 0, 2, "mp" ) == 0
         || bcomp.compare( 0, 4, "sxmp" ) == 0
         || bcomp.compare( 0, 4, "scmp" ) == 0 ) )
      setUsesMpi( true );

   if( bcomp.compare( 0, 3, "xlf" ) == 0
       || bcomp.compare( 0, 9, "blrts_xlf" ) == 0
       || bcomp.compare( 0, 3, "bgf" ) == 0 )
     m_sComp_FDFlag = "-WF,-D";
   else
     m_sComp_FDFlag = "-D";

   m_sComp_Cmd = cmd;
}

void
Config::compiler_addArg( const std::string arg )
{
   if( m_sComp_Args.length() > 0 ) m_sComp_Args += " ";
   m_sComp_Args += arg;
}

void
Config::compiler_addLib( const std::string lib )
{
   if( m_sComp_Libs.length() > 0 ) m_sComp_Libs += " ";
   m_sComp_Libs += lib;
}

void
Config::compiler_addSrcFile( const std::string file )
{
   std::string file_base;
   std::string file_obj;
   std::string::size_type si;

   // get base name of source file
   //
   file_base = file;
   si = file.rfind( '/' );
   if( si != std::string::npos )
      file_base = file.substr( si+1 );

   // create object file name of source file
   //
   si = file_base.rfind( '.' );
   assert( si != std::string::npos );
   file_obj = file_base.substr( 0, si ) + ".o";

   // store source/object file name for later processing by OPARI and/or TAU
   //
   m_vecComp_SrcFiles.push_back( file );
   m_vecComp_ObjFiles.push_back( file_obj );

   // add (modified) source file name to compiler arguments
   //
   if( m_eInstType == INST_TYPE_TAUINST || m_bUsesOpenMP )
   {
      std::string mod_file = file;

      si = mod_file.rfind( '.' );
      assert( si != std::string::npos );

      if( m_eInstType == INST_TYPE_TAUINST )
         mod_file.insert( si, ".tau" );
      else
         mod_file.insert( si, ".mod" );

      // convert Fortran source file suffix to upper case, in order to
      // invoke the C preprocessor before compiling
      //
      if( m_eLangType == LANG_F77 || m_eLangType == LANG_F90 )
      {
         si = mod_file.rfind( ".f" );
         if( si != std::string::npos ) mod_file.replace( si, 2, ".F" );
      }

      compiler_addArg( mod_file );
   }
   else
   {
      compiler_addArg( file );
   }
}

void
Config::opari_setRcFile( const std::string file )
{
   m_sOpari_RcFile = file;
   m_bKeepOpariRcFile = true;
}

void
Config::opari_setTabFile( const std::string file )
{
   std::string file_src;
   std::string file_obj;

   if( !( file.length() >= 2
          && file.compare( file.length()-2, 2, ".c" ) == 0 ) )
   {
      file_src = file + ".c";
      file_obj = file + ".o";
   }
   else
   {
      file_src = file_obj = file;
      file_obj.replace( file.length()-2, 2, ".o" );
   }

   m_sOpari_TabFile = std::make_pair( file_src, file_obj );
}

void
Config::opari_addArg( const std::string arg )
{
   if( m_sOpari_Args.length() > 0 ) m_sOpari_Args += " ";
   m_sOpari_Args += arg;
}

void
Config::tauinst_addArg( const std::string arg )
{
   if( m_sTauInst_Args.length() > 0 ) m_sTauInst_Args += " ";
   m_sTauInst_Args += arg;
}

void
Config::tauinst_addParseArg( const std::string arg )
{
   if( m_sTauInst_ParseArgs.length() > 0 ) m_sTauInst_ParseArgs += " ";
   m_sTauInst_ParseArgs += arg;
}

void
Config::setUsesMpi( const bool set, const bool ovwrt )
{
   static bool first = true;

   if( first || ovwrt )
   {
      if( set )
      {
#if !(defined(HAVE_MPI) && HAVE_MPI)
         std::cerr << "Unfortunately, this installation of VampirTrace was not "
                   << "compiled with" << std::endl
                   << "MPI support." << std::endl;
         exit(1);
#else // HAVE_MPI
         if( m_eLangType == LANG_F77 || m_eLangType == LANG_F90 )
         {
#  if !(defined(HAVE_FMPI) && HAVE_FMPI)
            std::cerr << "Unfortunately, this installation of VampirTrace was "
                      << "not compiled with" << std::endl
                      << "MPI Fortran support." << std::endl;
            exit(1);
#  endif // WRAP_FMPI
         }
#endif // HAVE_MPI
      }

      first = false;
      m_bUsesMpi = set;
   }
}

void
Config::setUsesThreads( const bool set, const bool ovwrt )
{
   static bool first = true;

   if( first || ovwrt )
   {
#if !(defined(HAVE_THREADS) && HAVE_THREADS)
      if( set )
      {
         std::cerr << "Unfortunately, this installation of VampirTrace was not "
                   << "compiled with" << std::endl
                   << "Multithreading support." << std::endl;
         exit(1);
      }
#endif // HAVE_THREADS

      first = false;
      m_bUsesThreads = set;
   }
}

void
Config::setUsesOpenMP( const bool set, const bool ovwrt )
{
   static bool first = true;

   if( first || ovwrt )
   {
#if !(defined(HAVE_OMP) && HAVE_OMP)
      if( set )
      {
         std::cerr << "Unfortunately, this installation of VampirTrace was not "
                   << "compiled with" << std::endl
                   << "OpenMP support." << std::endl;
         exit(1);
      }
#endif // HAVE_OMP

      first = false;
      m_bUsesOpenMP = set;
   }
}

bool
Config::setInstAvail( const std::string type )
{
   if( type.compare( "compinst" ) == 0 )
      setInstAvail( INST_TYPE_COMPINST );
   else if( type.compare( "dyninst" ) == 0 )
      setInstAvail( INST_TYPE_DYNINST );
   else if( type.compare( "tauinst" ) == 0 )
      setInstAvail( INST_TYPE_TAUINST );
   else if( type.compare( "manual" ) == 0 )
      setInstAvail( INST_TYPE_MANUAL );
   else
      return false;

   return true;
}

bool
Config::setInstType( const InstTypeT type )
{
   assert( m_iInstAvail != 0 );

   // instrumentation available ?
   if( !isInstAvail( type ) )
      return false;

   m_eInstType = type;

   if( type == INST_TYPE_COMPINST )
      m_sComp_InstFlags = m_sCompInst_Flags;
   else if( type == INST_TYPE_DYNINST )
      m_sComp_InstFlags = m_sDynInst_Flags;
   else
      m_sComp_InstFlags = "";

   return true;
}

bool
Config::setInstType( const std::string type )
{
   if( type.compare( "compinst" ) == 0 )
      return setInstType( INST_TYPE_COMPINST );
   else if( type.compare( "dyninst" ) == 0 )
      return setInstType( INST_TYPE_DYNINST );
   else if( type.compare( "tauinst" ) == 0 )
      return setInstType( INST_TYPE_TAUINST );
   else if( type.compare( "manual" ) == 0 )
      return setInstType( INST_TYPE_MANUAL );
   else
      return false;
}
