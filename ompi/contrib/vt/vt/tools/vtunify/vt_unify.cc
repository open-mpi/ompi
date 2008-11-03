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

#include "vt_unify.h"
#include "vt_unify_defs.h"
#include "vt_unify_events.h"
#include "vt_unify_stats.h"
#include "vt_unify_tkfac.h"

#include "vt_inttypes.h"

#include "otf.h"

#include <fstream>
#include <iostream>
#include <map>
#include <string>
#include <vector>

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#define USAGETEXT std::endl \
<< " " << ExeName << " - local trace unifier for VampirTrace." << std::endl \
<< std::endl \
<< " Syntax: " << ExeName << " <#files> <iprefix> [options...]" << std::endl \
<< std::endl \
<< "   options:" << std::endl \
<< "     -h, --help          Show this help message." << std::endl \
<< std::endl \
<< "     #files              number of local trace files" << std::endl \
<< "                         (equal to # of '*.uctl' files)" << std::endl \
<< std::endl \
<< "     iprefix             prefix of input trace filename" << std::endl \
<< std::endl \
<< "     -o <oprefix>        prefix of output trace filename" << std::endl \
<< std::endl \
<< "     -s <statsofile>     statistics output filename" << std::endl \
<< "                         default=<oprefix>.stats" << std::endl \
<< std::endl \
<< "     -q, --noshowstats   Don't show statistics on stdout." << std::endl \
<< std::endl \
<< "     -c, --nocompress    Don't compress output trace files." << std::endl \
<< std::endl \
<< "     -k, --keeplocal     Don't remove input trace files." << std::endl \
<< std::endl \
<< "     -v, --verbose       Enable verbose mode." << std::endl

static bool parseCommandLine( int argc, char ** argv );
static bool readUnifyControlFiles( void );
static bool writeMasterControl( void );
static bool getMinStartTime( void );
static bool cleanUp( void );

const std::string ExeName = "vtunify";
const std::string TmpFileSuffix = "__ufy.tmp";
const std::string UniFilePrefix = "u_";

// unify parameters
struct Params_struct Params;

// minimal start time
uint64_t g_uMinStartTime = (uint64_t)-1;

// unify control vector
std::vector<UnifyControl_struct*> g_vecUnifyCtls;

// map stream id <-> unify control index
std::map<uint32_t, uint32_t> g_mapStreamIdUnifyCtlIdx;

int
main( int argc, char ** argv )
{
   // parse command line
   if( !parseCommandLine( argc, argv ) )
      return 1;

   // show help text, if command line parameters are incomplete
   //
   if( Params.uctl_files_num == 0
       || Params.in_file_prefix.length() == 0 )
   {
      std::cout << USAGETEXT << std::endl;
      return 0;
   }

   // set namestub of output streams, if necessary
   if( Params.out_file_prefix.length() == 0 )
      Params.out_file_prefix = Params.in_file_prefix;

   // if input files should be kept and output filename
   // is equal to input filename, then prefix output filename
   if( !Params.doclean &&
       Params.out_file_prefix == Params.in_file_prefix )
   {
      int32_t fileidx = Params.out_file_prefix.rfind('/');

      if( fileidx > -1 )
      {
	 Params.out_file_prefix =
	    Params.out_file_prefix.substr( 0, fileidx + 1 ) +
	    UniFilePrefix + Params.out_file_prefix.substr( fileidx + 1 );
      }
      else
      {
	 Params.out_file_prefix = UniFilePrefix + Params.out_file_prefix;
      }
   }

   // set output filename for statistics
   if( Params.stats_out_file.length() == 0 )
      Params.stats_out_file = Params.out_file_prefix + ".stats";

   // read unify control files (*.uctl)
   if( !readUnifyControlFiles() )
      return 1;

   // create instance of classes ...
   // ... definitions
   theDefinitions = new Definitions();
   // ... events
   theEvents = new Events();
   // ... statistics
   theStatistics = new Statistics();
   // ... token factories ...
   // ... DefSclFile
   theTokenFactory[TKFAC__DEF_SCL_FILE] =
      new TokenFactory_DefSclFile();
   assert( theTokenFactory[TKFAC__DEF_SCL_FILE] != 0 );
   // ... DefScl
   theTokenFactory[TKFAC__DEF_SCL] =
      new TokenFactory_DefScl();
   assert( theTokenFactory[TKFAC__DEF_SCL] != 0 );
   // ... DefFileGroup
   theTokenFactory[TKFAC__DEF_FILE_GROUP] =
      new TokenFactory_DefFileGroup();
   assert( theTokenFactory[TKFAC__DEF_FILE_GROUP] != 0 );
   // ... DefFile
   theTokenFactory[TKFAC__DEF_FILE] =
      new TokenFactory_DefFile();
   assert( theTokenFactory[TKFAC__DEF_FILE] != 0 );
   // ... DefFunctionGroup
   theTokenFactory[TKFAC__DEF_FUNCTION_GROUP] =
      new TokenFactory_DefFunctionGroup();
   assert( theTokenFactory[TKFAC__DEF_FUNCTION_GROUP] != 0 );
   // ... DefFunction
   theTokenFactory[TKFAC__DEF_FUNCTION] =
      new TokenFactory_DefFunction();
   assert( theTokenFactory[TKFAC__DEF_FUNCTION] != 0 );
   // ... DefCollectiveOperation
   theTokenFactory[TKFAC__DEF_COLL_OP] =
      new TokenFactory_DefCollectiveOperation();
   assert( theTokenFactory[TKFAC__DEF_COLL_OP] != 0 );
   // ... DefCounterGroup
   theTokenFactory[TKFAC__DEF_COUNTER_GROUP] =
      new TokenFactory_DefCounterGroup();
   assert( theTokenFactory[TKFAC__DEF_COUNTER_GROUP] != 0 );
   // ... DefCounter
   theTokenFactory[TKFAC__DEF_COUNTER] =
      new TokenFactory_DefCounter();
   assert( theTokenFactory[TKFAC__DEF_COUNTER] != 0 );
   // ... DefProcessGroup
   theTokenFactory[TKFAC__DEF_PROCESS_GROUP] =
      new TokenFactory_DefProcessGroup();
   assert( theTokenFactory[TKFAC__DEF_PROCESS_GROUP] != 0 );

   // unify definitions
   if( !theDefinitions->run() )
      return 1;

   // get minimum start time
   if( !getMinStartTime() )
      return 1;

   // unify statistics
   if( !theStatistics->run() )
      return 1;

   // unify events
   if( !theEvents->run() )
      return 1;

   // create OTF master control
   if( !writeMasterControl() )
      return 1;

   // remove local definitions/events streams, unify control files
   // and temporary files
   if( !cleanUp() )
      return 1;

   if( theStatistics->isFuncStatAvail() )
   {
      // write summary function statistics to file

      if( !theStatistics->printFuncStat( Params.stats_out_file,
					 Params.stats_sort_flags ) )
	 return 1;

      // print summary function statistics to stdout
      if( Params.showstats )
      {
	 std::cout << std::endl;
	 theStatistics->printFuncStat( "", Params.stats_sort_flags );
	 std::cout << std::endl
		   << "The complete function summary was written to file '"
		   << Params.stats_out_file << "'." << std::endl;
      }
   }

   for( uint32_t i = 0; i < g_vecUnifyCtls.size(); i++ )
      delete( g_vecUnifyCtls[i] );

   return 0;
}

static bool
parseCommandLine( int argc, char ** argv )
{
   bool error = false;

   for( int i = 1; i < argc; i++ )
   {
      if( strcmp( argv[i], "-h" ) == 0
	  || strcmp( argv[i], "--help" ) == 0 )
      {
	 Params.uctl_files_num = 0;
	 return true;
      }
      else if( i == 1 )
      {
	 Params.uctl_files_num = atoi( argv[1] );
	 if( Params.uctl_files_num == 0 )
	 {
	    std::cerr << "<#files> must be positive integer" << std::endl;
	    error = true;
	 }
      }
      else if( i == 2 )
      {
	 Params.in_file_prefix = argv[2];
	 if( Params.in_file_prefix.compare( 0, 1, "/" ) != 0 &&
	     Params.in_file_prefix.compare( 0, 2, "./" ) != 0 )
	    Params.in_file_prefix = std::string("./") + Params.in_file_prefix;
      }
      else if( strcmp( argv[i], "-o" ) == 0 )
      {
	 if( i == argc - 1 )
	 {
	    std::cerr << ExeName << ": <oprefix> expected -- -o" << std::endl;
	    error = true;
	 }
	 else
	 {
	    Params.out_file_prefix = argv[++i];
	    if( Params.out_file_prefix.compare( 0, 1, "/" ) != 0 &&
		Params.out_file_prefix.compare( 0, 2, "./" ) != 0 )
	       Params.out_file_prefix = std::string("./") + Params.out_file_prefix;
	 }
      }
      else if( strcmp( argv[i], "-s" ) == 0 )
      {
	 if( i == argc - 1 )
	 {
	    std::cerr << ExeName << ": <statsofile> expected -- -s" << std::endl;
	    error = true;
	 }
	 else
	 {
	    Params.stats_out_file = argv[++i];
	    if( Params.stats_out_file.compare( 0, 1, "/" ) != 0 &&
		Params.stats_out_file.compare( 0, 2, "./" ) != 0 )
	       Params.stats_out_file = std::string("./") + Params.stats_out_file; 
	 }
      }
      else if( strcmp( argv[i], "-q" ) == 0
	       || strcmp( argv[i], "--noshowstats" ) == 0 )
      {
	 Params.showstats = false;
      }
      else if( strcmp( argv[i], "-c" ) == 0 
	       || strcmp( argv[i], "--nocompress" ) == 0 )
      {
	 Params.docompress = false;
      }
      else if( strcmp( argv[i], "-k" ) == 0
	       || strcmp( argv[i], "--keeplocal" ) == 0 )
      {
	 Params.doclean = false;
      }
      else if( strcmp( argv[i], "-v" ) == 0 
	       || strcmp( argv[i], "--verbose" ) == 0 )
      {
	 Params.beverbose = true;
      }
      else
      {
	 std::cerr << ExeName << ": invalid option -- " << argv[i] << std::endl;
	 error = true;
      }

      if( error )
	 break;
   }

   return !error;
}

static bool
readUnifyControlFiles()
{
   if( Params.beverbose )
      std::cout << "Reading unify control files ..." << std::endl;

   bool error = false;
   uint32_t i, j;
   
   for( i = 0; i < Params.uctl_files_num; i++ )
   {
      char filename[STRBUFSIZE];

      // create file name
      snprintf( filename, sizeof( filename ) - 1, "%s.%x.uctl",
		Params.in_file_prefix.c_str(), i+1 );

      // open unify control file for reading
      //
      std::ifstream in( filename );
      
      if( !in )
      {
	 std::cerr << ExeName << ": Error: "
		   << "Could not open file " << filename << std::endl;

	 error = true;
	 break;
      }

      if( Params.beverbose )
	 std::cout << " Opened " << filename << " for reading" << std::endl;

      char buffer[STRBUFSIZE];
      uint32_t line_no = 0;

      std::vector<uint32_t> vec_streamids;
      int64_t ltime[2] = { 0, 1 };
      int64_t offset[2] = { 0, 0 };

      // read line per line
      //
      while( in.getline( buffer, STRBUFSIZE ) )
      {
	 line_no++;

	 // line_no = 1: ids of input streams
	 //
	 if( line_no == 1 )
	 {
	    char * p;

	    p = strtok( buffer, ":" );
	    do
	    {
	       vec_streamids.push_back( atoi( p ) );
	    } while( ( p = strtok( 0, ":" ) ) );

	    if( vec_streamids.size() == 0 )
	    {
	       std::cerr << filename << ":" << line_no 
			 << ": Could not be parsed" << std::endl;
	       error = true;
	       break;
	    }
	 }
	 // line_no = 2: read chronological offsets to global time
	 //              and local times
	 //
	 else if( line_no == 2 )
	 {
	    char * p;
	    uint32_t n = 0;

	    p = strtok( buffer, ":" );
	    do
	    {
	       n++;

	       switch( n )
	       {
	          case 1:
		     ltime[0] = ATOL8(p);
		     break;
	          case 2:
		     offset[0] = ATOL8(p);
		     break;
	          case 3:
		     ltime[1] = ATOL8(p);
		     break;
	          case 4:
		     offset[1] = ATOL8(p);
		     break;
	          default:
		     break;
	       }
	    } while( ( p = strtok( 0, ":" ) ) );

	    if( n != 4 )
	    {
	       std::cerr << filename << ":" << line_no 
			 << ": Could not be parsed" << std::endl;
	       
	       error = true;
	       break;
	    }
	 }
	 else
	 {
	    break;
	 }
      }

      // close unify control file
      in.close();

      if( Params.beverbose )
	 std::cout << " Closed " << filename << std::endl;

      if( !error )
      {
	 // add to unify control vector
	 //
	 for( j = 0; j < vec_streamids.size(); j++ )
	 {
	    g_vecUnifyCtls.push_back( 
	       new UnifyControl_struct(vec_streamids[j],
				       ltime,
				       offset) );
	    g_mapStreamIdUnifyCtlIdx.insert(
	       std::make_pair( (uint32_t)vec_streamids[j],
			       (uint32_t)g_vecUnifyCtls.size()-1 ) );
	 }
      }
      else
      {
	 break;
      }
   }

   return !error;
}

static bool
writeMasterControl()
{
   if( Params.beverbose )
      std::cout << "Writing OTF master control ..." << std::endl;

   bool error = false;

   std::string tmp_out_file_prefix =
      Params.out_file_prefix + TmpFileSuffix;

   // open file manager
   OTF_FileManager * p_uni_mastercontrol_manager =
      OTF_FileManager_open( 1 );
   assert( p_uni_mastercontrol_manager );

   // create master control
   OTF_MasterControl * p_uni_mastercontrol =
      OTF_MasterControl_new( p_uni_mastercontrol_manager );
   assert( p_uni_mastercontrol );

   // put stream/process matching to master control
   //
   for( uint32_t i = 0; i < g_vecUnifyCtls.size(); i++ )
   {
      if( OTF_MasterControl_append( p_uni_mastercontrol,
				    g_vecUnifyCtls[i]->streamid,
				    g_vecUnifyCtls[i]->streamid ) == 0 )
      {
	 std::cerr << ExeName << ": Error: "
		   << "Could not append "
		   << g_vecUnifyCtls[i]->streamid << ":"
		   << std::hex << g_vecUnifyCtls[i]->streamid
		   << " to OTF master control"
		   << std::dec << std::endl;
	 error = true;
	 break;
      }
   }

   // write master control
   if( !error )
   {
      OTF_MasterControl_write( p_uni_mastercontrol,
			       tmp_out_file_prefix.c_str() );

   if( Params.beverbose )
      std::cout << " Opened OTF master control [namestub "
		<< tmp_out_file_prefix << "]" << std::endl;
   }

   // close file master control
   OTF_MasterControl_close( p_uni_mastercontrol );
   // close file manager
   OTF_FileManager_close( p_uni_mastercontrol_manager );

   if( !error && Params.beverbose )
      std::cout << " Closed OTF master control [namestub "
		<< tmp_out_file_prefix << "]" << std::endl;

   return !error;
}


static bool
getMinStartTime()
{
   bool error = false;

   for( uint32_t i = 0; i < g_vecUnifyCtls.size(); i++ )
   {
      // open file manager for reader stream
      OTF_FileManager * p_org_events_manager =
	 OTF_FileManager_open( 1 );
      assert( p_org_events_manager );

      // open stream for reading
      OTF_RStream * p_org_events_rstream =
	 OTF_RStream_open( Params.in_file_prefix.c_str(),
			   g_vecUnifyCtls[i]->streamid,
			   p_org_events_manager );
      assert( p_org_events_rstream );

      // create record handler
      OTF_HandlerArray * p_handler_array =
	 OTF_HandlerArray_open();
      assert( p_handler_array );

      // set zero record limit
      OTF_RStream_setRecordLimit( p_org_events_rstream, 0 );

      // start psoudo reading
      OTF_RStream_readEvents( p_org_events_rstream, p_handler_array );
      
      // get minimum timestamp
      uint64_t minimum; uint64_t current; uint64_t maximum;
      OTF_RStream_eventProgress( p_org_events_rstream, 
				 &minimum, &current, &maximum );
      minimum = CorrectTime( g_vecUnifyCtls[i]->streamid, minimum );
      
      if( g_uMinStartTime == (uint64_t)-1 || minimum < g_uMinStartTime )
	g_uMinStartTime = minimum;

      // close record handler
      OTF_HandlerArray_close( p_handler_array );

      // close reader stream
      OTF_RStream_close( p_org_events_rstream );
      // close file manager for reader stream
      OTF_FileManager_close( p_org_events_manager );
   }
   
   return !error;
}

static bool
cleanUp()
{
   if( Params.beverbose )
      std::cout << "Cleaning up ..." << std::endl;

   bool error = false;

   uint32_t i;
   char filename1[STRBUFSIZE];
   char filename2[STRBUFSIZE];

   if( Params.doclean )
   {
      // remove unify control files
      //
      for( i = 0; i < Params.uctl_files_num; i++ )
      {
	 snprintf( filename1, sizeof( filename1 ) - 1, "%s.%x.uctl",
		   Params.in_file_prefix.c_str(), i+1 );
	 
	 if( remove( filename1 ) != 0 )
	 {
	    std::cerr << ExeName << ": Error: Could not remove "
		      << filename1 << std::endl;
	    break;
	 }
	 
	 if( Params.beverbose )
	    std::cout << " Removed " << filename1 << std::endl;
      }

      if( i != Params.uctl_files_num )
	 return false;

      // remove local def./events/stats trace files
      //
      for( i = 0; i < g_vecUnifyCtls.size(); i++ )
      {
	 for( uint32_t j = 0; j < 3; j++ )
	 {
	    OTF_FileType filetype;

	    if( j == 0 ) filetype = OTF_FILETYPE_DEF;
	    else if( j == 1 ) filetype = OTF_FILETYPE_EVENT;
	    else filetype = OTF_FILETYPE_STATS;

	    OTF_getFilename( Params.in_file_prefix.c_str(),
			     g_vecUnifyCtls[i]->streamid,
			     filetype,
			     STRBUFSIZE, filename1 );
	    
	    if( access( filename1, F_OK ) != 0 )
	    {
	       assert( strlen( filename1 ) + 2 + 1 < sizeof( filename1 ) - 1 );

	       // file not found, try '.z' suffix
	       strncat( filename1, ".z", 2 );
	    }

	    if( remove( filename1 ) == 0 )
	    {
	       if( Params.beverbose )
		  std::cout << " Removed " << filename1 << std::endl;
	    }
	 }
      }

      if( i != g_vecUnifyCtls.size() )
	 return false;
   }

   std::string tmp_out_file_prefix =
      Params.out_file_prefix + TmpFileSuffix;

   // rename temporary global definition trace file
   //
   OTF_getFilename( tmp_out_file_prefix.c_str(), 0,
		    OTF_FILETYPE_DEF,
		    STRBUFSIZE, filename1 );
   OTF_getFilename( Params.out_file_prefix.c_str(), 0,
		    OTF_FILETYPE_DEF,
		    STRBUFSIZE, filename2 );
   
   if( access( filename1, F_OK ) != 0 )
   {
      assert( strlen( filename1 ) + 2 + 1 < sizeof( filename1 ) - 1 );
      assert( strlen( filename2 ) + 2 + 1 < sizeof( filename2 ) - 1 );

      // file not found, try '.z' suffix
      strncat( filename1, ".z", 2 );
      strncat( filename2, ".z", 2 );
   }

   if( rename( filename1, filename2 ) != 0 )
   {
      std::cerr << ExeName << ": Error: Could not rename " 
		<< filename1 << " to "
		<< filename2 << std::endl;
      return false;
   }
   
   if( Params.beverbose )
      std::cout << " Renamed " << filename1 << " to " << filename2 << std::endl;

   // rename temporary master control file
   //
   OTF_getFilename( tmp_out_file_prefix.c_str(), 0,
		    OTF_FILETYPE_MASTER,
		    STRBUFSIZE, filename1 );
   OTF_getFilename( Params.out_file_prefix.c_str(), 0,
		    OTF_FILETYPE_MASTER,
		    STRBUFSIZE, filename2 );

   if( rename( filename1, filename2 ) != 0 )
   {
      std::cerr << ExeName << ": Error: Could not rename " 
		<< filename1 << " to "
		<< filename2 << std::endl;
      return false;
   }

   if( Params.beverbose )
      std::cout << " Renamed " << filename1 << " to " << filename2 << std::endl;

   // rename all temporary event/stats trace files
   //
   for( i = 0; i < g_vecUnifyCtls.size(); i++ )
   {
      for( uint32_t j = 0; j < 2; j++ )
      {
	 OTF_FileType filetype;

	 if( j == 0 ) filetype = OTF_FILETYPE_EVENT;
	 else filetype = OTF_FILETYPE_STATS;

	 OTF_getFilename( tmp_out_file_prefix.c_str(),
			  g_vecUnifyCtls[i]->streamid,
			  filetype,
			  STRBUFSIZE, filename1 );
	 OTF_getFilename( Params.out_file_prefix.c_str(),
			  g_vecUnifyCtls[i]->streamid,
			  filetype,
			  STRBUFSIZE, filename2 );

	 if( access( filename1, F_OK ) != 0 )
	 {
	    if( strlen( filename1 ) + 2 + 1 < sizeof( filename1 ) - 1 );
	    if( strlen( filename2 ) + 2 + 1 < sizeof( filename2 ) - 1 );

	    // file not found, try '.z' suffix
	    strncat( filename1, ".z", 2 );
	    strncat( filename2, ".z", 2 );
	 }

	 if( rename( filename1, filename2 ) == 0 )
	 {
	    if( Params.beverbose )
	       std::cout << " Renamed " << filename1 << " to " << filename2 << std::endl;
	 }
      }
   }

   return !error;
}

uint64_t CorrectTime( uint32_t loccpuid, uint64_t time )
{
   assert( loccpuid > 0 );

   std::map<uint32_t, uint32_t>::iterator it = 
      g_mapStreamIdUnifyCtlIdx.find( loccpuid );
   assert( it != g_mapStreamIdUnifyCtlIdx.end() );
   assert( it->second < g_vecUnifyCtls.size() );

   int64_t * ltime = g_vecUnifyCtls[it->second]->ltime;
   int64_t * offset = g_vecUnifyCtls[it->second]->offset;
   
   double d_time = (double)time;
   double d_ltime0 = (double)ltime[0];
   double d_offset0 = (double)offset[0];
   double d_ltime1 = (double)ltime[1];
   double d_offset1 = (double)offset[1];

   return (uint64_t)(d_time + (((d_offset1 - d_offset0) / (d_ltime1 - d_ltime0)) * (d_time - d_ltime0)) + d_offset0);
}
