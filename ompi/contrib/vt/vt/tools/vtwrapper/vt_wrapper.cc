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

#include "vt_wrapper.h"

static void remove_dbl_spaces_from_str( std::string & str );

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

static void remove_dbl_spaces_from_str( std::string & str )
{
   std::string::size_type idx;
   while( ( idx = str.find( "  " ) ) != std::string::npos )
      str.erase( idx, 1 );
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

   const uint32_t keys_num = 22;
   const std::string keys[] = {
      "version", "language", "compiler_env", "compiler_flags_env",
      "compiler", "compiler_flags", "linker_flags", "libs", "includedir",
      "libdir", "vtlib", "vtmpilib", "vtmtlib", "vthyblib", "vtpomplib",
      "vtdynattlib", "opari_bin", "opari_tab_compiler",
      "opari_tab_compiler_flags", "inst_compiler_flags",
      "inst_avail", "inst_default"
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
	    if( value.length() > 0 )
	    {
	       char* includedir = vt_installdirs_expand( value.c_str() );
	       if( includedir )
	       {
		  m_pConfig->m_sVT_IncDir = "-I" + std::string(includedir);
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
	    break;
	 }
         case 10: // libdir
	 {
	    if( value.length() > 0 )
	    {
	       char* libdir = vt_installdirs_expand( value.c_str() );
	       if( libdir )
	       {
		  m_pConfig->m_sVT_LibDir = "-L" + std::string(libdir);
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
	    if( value.length() > 0 )
	       m_pConfig->m_sOpari_Cmd = value;
	    break;
	 }
         case 18: // opari_tab_compiler
	 {
	    m_pConfig->m_sOpari_TabCompCmd = value;
	    break;
	 }
         case 19: // opari_tab_compiler_flags
	 {
	    m_pConfig->m_sOpari_TabCompFlags = value;
	    break;
	 }
         case 20: // inst_compiler_flags
	 {
	    m_pConfig->m_sCompInstFlags = value;
	    break;
	 }
         case 21: // inst_avail
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
         case 22: // inst_default
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
	 if( i == argc - 1 )
	 {
	    std::cerr << ExeName << ": <type> expected -- -vt:inst"
		      << std::endl;
	    return false;
	 }

	 arg = argv[++i];

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
	 if( i == argc - 1 )
	 {
	    std::cerr << ExeName << ": <args> expected -- -vt:opari"
		      << std::endl;
	    return false;
	 }

   size_t args_len = strlen(argv[i+1])+1;
   char * args = new char[args_len];
   strncpy( args, argv[++i], args_len - 1 );
   args[args_len - 1] = '\0';

   char * token = strtok( args, " " );
	 do
	 {
	    if( strcmp( token, "-rcfile" ) == 0 )
	    {
	       token = strtok( 0, " " );
	       if( !token )
	       {
		  std::cerr << ExeName << ": <rcfile> expected -- -rcfile"
			    << std::endl;
		  delete [] args;
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
		  delete [] args;
		  return false;
	       }
	       
	       m_pConfig->opari_setTabFile( token );
	    }
	    else
	    {
	       m_pConfig->opari_addArg( token );
	    }
	 } while( ( token = strtok( 0, " " ) ) );

	 delete [] args;
      }
      //
      // -vt:seq
      //
      else if( arg.compare("-vt:seq") == 0 )
      {
	 m_pConfig->setUsesMpi( false, true );
	 m_pConfig->setUsesThreads( false, true );
      }
      //
      // -vt:mpi
      //
      else if( arg.compare("-vt:mpi") == 0 )
      {
	 m_pConfig->setUsesMpi( true, true );
	 m_pConfig->setUsesThreads( false, true );
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
      // pthread flag
      //
      else if( arg.compare( "-Kthread" ) == 0
	       || arg.compare( "-kthread" ) == 0
	       || arg.compare( "-pthread" ) == 0
	       || arg.compare( "-pthreads" ) == 0
	       || arg.compare( "-mthreads" ) == 0
	       || arg.compare( "--thread-safe" ) == 0
	       || arg.compare( "-mt" ) == 0 )
      {
	 m_pConfig->setUsesThreads( true );
	 m_pConfig->compiler_addArg( arg );
      }
      //
      // pthread libs
      //
      else if( arg.compare( "-lpthreads" ) == 0
	       || arg.compare( "-llthread" ) == 0
	       || arg.compare( "-lpthread" ) == 0 )
      {
	 m_pConfig->setUsesThreads( true );
	 m_pConfig->compiler_addLib( arg );
      }
      //
      // openmp flag
      //
      else if( arg.compare( "-openmp" ) == 0
	       || arg.compare( "-fopenmp" ) == 0
	       || arg.compare( "-Popenmp" ) == 0
	       || arg.compare( "-xopenmp" ) == 0
	       || arg.compare( "-mp" ) == 0
	       || arg.compare( 0, 4, "-mp=" ) == 0
	       || arg.compare( "-qsmp=omp" ) == 0 )
      {
	 m_pConfig->setUsesThreads( true );
	 m_pConfig->setUsesOpenMP( true );
	 m_pConfig->compiler_addArg( arg );
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
      // -vt:help, -vt:version, -vt:show, -vt:seq, -vt:mpi, -vt:mt, -vt:hyb,
      // -vt:seq, -vt:mpi, -vt:mt, -vt:hyb, pthread flag,
      // openmp flag
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
	  || arg.compare( "-Kthread" ) == 0
	  || arg.compare( "-kthread" ) == 0
	  || arg.compare( "-pthread" ) == 0
	  || arg.compare( "-pthreads" ) == 0
	  || arg.compare( "-mthreads" ) == 0
	  || arg.compare( "--thread-safe" ) == 0
	  || arg.compare( "-mt" ) == 0
	  || arg.compare( "-lpthreads" ) == 0
	  || arg.compare( "-llthread" ) == 0
	  || arg.compare( "-lpthread" ) == 0
	  || arg.compare( "-openmp" ) == 0
	  || arg.compare( "-fopenmp" ) == 0
	  || arg.compare( "-xopenmp" ) == 0
	  || arg.compare( "-Popenmp" ) == 0
	  || arg.compare( "-mp" ) == 0
	  || arg.compare( 0, 4, "-mp=" ) == 0
	  || arg.compare( "-qsmp=omp" ) == 0 )
      {
	 // do nothing
      }
      else if( arg.compare("-vt:inst") == 0
	       || arg.compare("-vt:opari") == 0 )
      {
	 // do nothing, skip next argument
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
	 if( i == argc - 1 )
	 {
	    std::cerr << ExeName << ": <cmd> expected -- "
		      << arg << std::endl;
	    return false;
	 }
	 
	 m_pConfig->compiler_setCmd( argv[++i] );
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
	 if( m_pConfig->m_bUsesThreads &&
	     m_pConfig->m_bUsesOpenMP )
	 {
	    m_pConfig->opari_addSrcFile( arg );
	 }
	 else
	 {
	    m_pConfig->compiler_addArg( arg );
	 }
      }
      //
      // -c
      //
      else if( arg.compare("-c") == 0 )
      {
	 m_pConfig->m_bCompOnly = true;
	 m_pConfig->compiler_addArg( arg );
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
      }
      //
      // -<L|l>*
      //
      else if( arg.compare( 0, 2, "-L" ) == 0
	       || arg.compare( 0, 2, "-l" ) == 0 )
      {
	 m_pConfig->compiler_addLib( arg );
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

   int  inst_type    = m_pConfig->m_eInstType;
   bool uses_mpi     = m_pConfig->m_bUsesMpi;
   bool uses_threads = m_pConfig->m_bUsesThreads;
   bool uses_openmp  = m_pConfig->m_bUsesOpenMP;
   bool beverbose    = m_pConfig->m_bBeVerbose;
   bool show         = m_pConfig->m_bShow;

   // call compiler without any parameters, if
   // insufficient arguments given
   //
   if( !show && m_pConfig->m_sComp_Args.length() == 0 )
   {
      rc = system( m_pConfig->m_sComp_Cmd.c_str() );
      return WEXITSTATUS( rc );
   }

   // run opari on every collected source file
   //
   if( uses_threads && uses_openmp )
   {
      for( uint32_t i = 0; i < m_pConfig->m_vecOpari_SrcFiles.size(); i++ )
      {
	 // build opari command
	 //
	 cmd =
	    m_pConfig->m_sOpari_Cmd + " "
	    + m_pConfig->m_sOpari_Args + " "
	    + "-rcfile "
	    + m_pConfig->m_sOpari_RcFile + " "
	    + "-table "
	    + m_pConfig->m_sOpari_TabFile.first + " "
	    + m_pConfig->m_vecOpari_SrcFiles[i];

	 // execute/show opari command
	 //
	 remove_dbl_spaces_from_str( cmd );
	 if( show ) std::cout << cmd << std::endl;
	 else
	 {
	    if( beverbose )
	       std::cout << "+++ " << cmd << std::endl;
	    rc = system( cmd.c_str() );
	    if( WEXITSTATUS( rc ) != 0 )
	       return WEXITSTATUS( rc );
	 }
      }
   }

   // compile only ?
   if( m_pConfig->m_bCompOnly )
   {
      // build compiler command
      //
      cmd = m_pConfig->m_sComp_Cmd + " "
	 + m_pConfig->m_sVT_IncDir + " "
	 + ( ( uses_threads && uses_openmp ) ? "-I." : "" ) + " "
	 + m_pConfig->m_sComp_InstFlags + " "
	 + m_pConfig->m_sComp_Flags + " "
	 + m_pConfig->m_sComp_Args;

      // execute/show compiler command
      //
      remove_dbl_spaces_from_str( cmd );
      if( show ) std::cout << cmd << std::endl;
      else
      {
	 if( beverbose )
	    std::cout << "+++ " << cmd << std::endl;
	 rc = system( cmd.c_str() );
	 if( WEXITSTATUS( rc ) != 0 )
	    return WEXITSTATUS( rc );
      }
   }
   else
   {
      std::string vtlib;

      if( uses_threads && uses_openmp )
      {
	 // build command for compiling opari table file
	 //
	 cmd =
	    m_pConfig->m_sOpari_TabCompCmd + " "
	    + m_pConfig->m_sOpari_TabCompFlags + " "
	    + m_pConfig->m_sVT_IncDir + " "
	    + "-c " + m_pConfig->m_sOpari_TabFile.first + " "
	    + "-o " + m_pConfig->m_sOpari_TabFile.second;

	 // execute/show compiler command
	 //
	 remove_dbl_spaces_from_str( cmd );
	 if( show ) std::cout << cmd << std::endl;
	 else
	 {
	    if( beverbose )
	       std::cout << "+++ " << cmd << std::endl;
	    rc = system( cmd.c_str() );
	    if( WEXITSTATUS( rc ) != 0 )
	       return WEXITSTATUS( rc );
	 }
      }
      
      // build compiler command
      //
      vtlib = ( ( inst_type == INST_TYPE_DYNINST ) ?
		m_pConfig->m_sVT_DynAttLib : "" );

      if( uses_threads )
      {
	 vtlib += " "
	    + ( ( m_pConfig->m_bUsesOpenMP ) ?
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
	 + ( ( uses_threads && uses_openmp ) ? "-I." : "" ) + " "
	 + m_pConfig->m_sComp_InstFlags + " "
	 + m_pConfig->m_sComp_Flags + " "
	 + m_pConfig->m_sComp_Args + " "
	 + ( ( uses_threads && uses_openmp ) ?
	     m_pConfig->m_sOpari_TabFile.second : "" ) + " "
	 + m_pConfig->m_sComp_LdFlags + " "
	 + m_pConfig->m_sVT_LibDir + " "
	 + vtlib + " "
	 + m_pConfig->m_sComp_Libs;

      // execute/show compiler command
      //
      remove_dbl_spaces_from_str( cmd );
      if( show ) std::cout << cmd << std::endl;
      else
      {
	 if( beverbose )
	    std::cout << "+++ " << cmd << std::endl;
	 rc = system( cmd.c_str() );
	 if( WEXITSTATUS( rc ) != 0 )
	    return WEXITSTATUS( rc );
      }

      // cleanup intermediate files (in non-verbose mode)
      //
      if( !beverbose && uses_threads && uses_openmp )
      {
         if( m_pConfig->m_eLangType == LANG_F77
             || m_pConfig->m_eLangType == LANG_F90 )
         {
            std::vector<std::string> vec_incfiles = getIncFilesFromTabFile();
            for( uint32_t i = 0; i < vec_incfiles.size(); i++ )
               remove( vec_incfiles[i].c_str() );
         }

         remove( m_pConfig->m_sOpari_TabFile.first.c_str() );
         remove( m_pConfig->m_sOpari_TabFile.second.c_str() );
      }

      // remove OPARI's rc file, if it's not required anymore
      if( !m_pConfig->m_bKeepOpariRcFile )
         remove( m_pConfig->m_sOpari_RcFile.c_str() );
   }

   if( uses_threads && uses_openmp )
   {
      uint32_t i;

      // rename compiler output to original file name
      //
      for( i = 0; i < m_pConfig->m_vecOpari_ModObjFiles.size(); i++ )
      {
         int modi = m_pConfig->m_vecOpari_ModObjFiles[i].find( ".mod" );

         if( modi != -1 )
         {
            std::string target = m_pConfig->m_vecOpari_ModObjFiles[i];
            target.erase( modi, 4 );

            if( beverbose )
               std::cout << "+++ rename " << m_pConfig->m_vecOpari_ModObjFiles[i]
                         << " to " << target << std::endl;

            if( m_pConfig->m_bCompOnly &&
                rename( m_pConfig->m_vecOpari_ModObjFiles[i].c_str(),
                        target.c_str() ) == -1 )
            {
               std::cerr << ExeName << ": could not rename "
                         << m_pConfig->m_vecOpari_ModObjFiles[i] << " to "
                         << target << std::endl;
               return 1;
            }
         }
      }

      // delete intermediate opari output (in non-verbose mode)
      //
      if( !beverbose )
      {
         for( i = 0; i < m_pConfig->m_vecOpari_ModSrcFiles.size(); i++ )
            remove( m_pConfig->m_vecOpari_ModSrcFiles[i].c_str() );
      }
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
	     << "[-vt:" << str_lang_suffix << " <cmd>] "
	     << "[-vt:inst <insttype>] [-vt:<seq|mpi|mt|hyb>] "
	     << std::endl << "         "
	     << "[-vt:opari <args>]"
	     << std::endl << "         "
	     << "[-vt:verbose] "
	     << "[-vt:version] "
	     << "[-vt:show] ..."
	     << std::endl << std::endl
	     << "   options:"
	     << std::endl
	     << "     -vt:help            Show this help message."
	     << std::endl
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
	     << "       dyninst           binary by using Dyninst (www.dyninst.org)"
	     << std::endl << std::endl
	     << "     -vt:opari <args>    Set options for OPARI command."
	     << std::endl
	     << "                         (see "
       << vt_installdirs_get(VT_INSTALLDIR_DATADIR)
       << "/doc/opari/Readme.html for more information)"
	     << std::endl << std::endl
	     << "     -vt:<seq|mpi|mt|hyb>"
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
	     << "                         mt = parallel (uses OpenMP/POSIX threads)"
	     << std::endl
	     << "                         hyb = hybrid parallel (MPI + Threads)"
	     << std::endl
	     << "                         (default: automatically determining by"
	     << std::endl
	     << "                          underlying compiler and flags)"
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


//////////////////// class Config ////////////////////

// public methods
//

Config::Config() :
   m_eLangType(LANG_CC), m_eInstType(INST_TYPE_MANUAL), m_iInstAvail(0),

   m_bBeVerbose(false), m_bCompOnly(false),
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
   int ls = cmd.rfind('/');

   if( ls != -1 ) bcomp = cmd.substr( ls+1 );
   
   if( !m_bUsesMpi &&
       ( bcomp.compare( 0, 2, "mp" ) == 0 ||
	 bcomp.compare( 0, 4, "sxmp" ) == 0 ||
	 bcomp.compare( 0, 4, "scmp" ) == 0 ) )
      setUsesMpi( true );

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
Config::opari_setRcFile( const std::string rcfile )
{
   m_sOpari_RcFile = rcfile;
   m_bKeepOpariRcFile = true;
}

void
Config::opari_setTabFile( const std::string tabfile )
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

   m_sOpari_TabFile = std::make_pair( file_src, file_obj );
}

void
Config::opari_addArg( const std::string arg )
{
   if( m_sOpari_Args.length() > 0 ) m_sOpari_Args += " ";
   m_sOpari_Args += arg;
}

void
Config::opari_addSrcFile( const std::string srcfile )
{
   std::string srcfile_dir  = "";
   std::string srcfile_base = srcfile;
   std::string srcfile_base_pre;
   std::string srcfile_base_suf;

   std::string newsrcfile;
   std::string newobjfile;

   int i;

   // determine srcfile's dirname and basename
   //
   i = srcfile.rfind('/');
   if( i != -1 )
   {
      srcfile_dir = srcfile.substr(0, i + 1);
      srcfile_base = srcfile.substr(i + 1);
   }

   // determine srcfile's prefix and suffix
   //
   srcfile_base_pre = srcfile_base.substr(0, srcfile_base.rfind('.'));
   srcfile_base_suf = srcfile_base.substr(srcfile_base.rfind('.'));

   // generate OPARI include file name
   // C: in directory of srcfile
   // F: in directory of rcfile
   //
   if( m_eLangType == LANG_CC || m_eLangType == LANG_CXX )
   {
      std::string newincfile = srcfile + std::string(".opari.inc");
      m_vecOpari_ModSrcFiles.push_back( newincfile );
   }
   else // m_eLangType == LANG_F77 || m_eLangType == LANG_F90
   {
      i = srcfile_base_suf.rfind( 'f' );
      if( i != -1 ) srcfile_base_suf.replace( i, 1, "F" );
   }

   // generate name of modified source/object file
   //
   newsrcfile = srcfile_dir + srcfile_base_pre + std::string(".mod") 
                + srcfile_base_suf;
   newobjfile = srcfile_base_pre + std::string(".mod.o");

   m_vecOpari_SrcFiles.push_back( srcfile );
   m_vecOpari_ModSrcFiles.push_back( newsrcfile );
   m_vecOpari_ModObjFiles.push_back( newobjfile );

   compiler_addArg( newsrcfile );
}

void
Config::setUsesMpi( const bool set, const bool ovwrt )
{
   static bool first = true;

   if( set )
   {
#if !(defined(HAVE_MPI) && HAVE_MPI)
      std::cerr << "Unfortunately, this installation of VampirTrace was not "
                << "compiled with"
		<< std::endl << "MPI support." << std::endl;
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

   if( first || ovwrt )
   {
      first = false;
      m_bUsesMpi = set;
   }
}

void
Config::setUsesThreads( const bool set, const bool ovwrt )
{
   static bool first = true;

#if !(defined(HAVE_THREADS) && HAVE_THREADS)
   if( set )
   {
      std::cerr << "Unfortunately, this installation of VampirTrace was not "
                << "compiled with" << std::endl
		<< "Multithreading support." << std::endl;
      exit(1);
   }
#endif // HAVE_THREADS

   if( first || ovwrt )
   {
      first = false;
      m_bUsesThreads = set;
   }
}

void
Config::setUsesOpenMP( const bool set, const bool ovwrt )
{
   static bool first = true;

#if !(defined(HAVE_OMP) && HAVE_OMP)
   if( set )
   {
      std::cerr << "Unfortunately, this installation of VampirTrace was not "
                << "compiled with" << std::endl
		<< "OpenMP support." << std::endl;
      exit(1);
   }
#endif // HAVE_OMP

   if( first || ovwrt )
   {
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
      m_sComp_InstFlags = m_sCompInstFlags;
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
   else if( type.compare( "manual" ) == 0 )
      return setInstType( INST_TYPE_MANUAL );
   else
      return false;
}
