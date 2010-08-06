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

#include "vt_unify.h"
#include "vt_unify_defs.h"
#include "vt_unify_events.h"
#include "vt_unify_markers.h"
#include "vt_unify_stats.h"
#include "vt_unify_tkfac.h"

#include "otf.h"

#include <fstream>
#include <iostream>
#include <map>
#include <sstream>
#include <string>
#include <vector>

#include <assert.h>
#include <stdarg.h>
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
<< "     #files              Number of local trace files." << std::endl \
<< "                         (equal to # of '*.uctl' files)" << std::endl \
<< std::endl \
<< "     iprefix             Prefix of input trace filename." << std::endl \
<< std::endl \
<< "     -o <oprefix>        Prefix of output trace filename." << std::endl \
<< std::endl \
<< "     -s <statsofile>     Statistics output filename." << std::endl \
<< "                         default=<oprefix>.stats" << std::endl \
<< std::endl \
<< "     -c, --nocompress    Don't compress output trace files." << std::endl \
<< std::endl \
<< "     -k, --keeplocal     Don't remove input trace files." << std::endl \
<< std::endl \
<< "     -p, --progress      Show progress." << std::endl \
<< std::endl \
<< "     -q, --quiet         Enable quiet mode." << std::endl \
<< "                         (only emergency output)" << std::endl \
<< std::endl \
<< "     -v, --verbose       Increase output verbosity." << std::endl \
<< "                         (can be used more than once)" << std::endl

static bool getParams( int argc, char ** argv );
static bool getUnifyControls( void );
static bool parseCommandLine( int argc, char ** argv );
static bool writeMasterControl( void );
static bool getMinStartTime( void );
static bool cleanUp( void );
static void terminate( int status );

#ifdef VT_MPI
   static bool shareParams( void );
   static bool shareUnifyControls( void );
   static bool shareMinStartTime( void );
   static bool shareTokenTranslations( void );

   VT_MPI_INT g_iMPISize;
   VT_MPI_INT g_iMPIRank;
#endif // VT_MPI

#ifdef VT_MPI
const std::string ExeName = "vtunify-mpi";
#else // VT_MPI
const std::string ExeName = "vtunify";
#endif // VT_MPI
const std::string TmpFileSuffix = "__ufy.tmp";
const std::string UniFilePrefix = "u_";

#ifdef VT_ETIMESYNC
   bool HaveETimeSync = false;
#endif // VT_ETIMESYNC

// unify parameters
struct Params_struct Params;

// minimal start time
uint64_t g_uMinStartTime = 0;

// minimal start time (usec since Epoch)
uint64_t g_uMinStartTimeEpoch = (uint64_t)-1;

// maximal stop time (usec since Epoch)
uint64_t g_uMaxStopTimeEpoch = 0;

// unify control vector
std::vector<UnifyControl_struct*> g_vecUnifyCtls;

// map stream id <-> unify control index
std::map<uint32_t, uint32_t> g_mapStreamIdUnifyCtlIdx;

int
main( int argc, char ** argv )
{
#ifdef VT_MPI
   VTUnify_MPI_Init( (VT_MPI_INT*)&argc, &argv );
   VTUnify_MPI_Comm_size( VTUnify_MPI_COMM_WORLD, &g_iMPISize );
   VTUnify_MPI_Comm_rank( VTUnify_MPI_COMM_WORLD, &g_iMPIRank );
#endif // VT_MPI

   // get unify parameters
   if( !getParams( argc, argv ) )
      terminate(1);

   // show usage text, if necessary
   //
   if( Params.showusage )
   {
      MASTER std::cout << USAGETEXT << std::endl;
      terminate(0);
   }

   // read unify control files (*.uctl)
   if( !getUnifyControls() )
      terminate(1);

   // create instance of classes ...
#ifdef VT_ETIMESYNC
   // ... synchronization
   if( HaveETimeSync )
      theSynchronization = new Synchronization();
#endif // VT_ETIMESYNC
   // ... definitions
   theDefinitions = new Definitions();
   // ... statistics
   theStatistics = new Statistics();
   // ... markers
   theMarkers = new Markers();
   // ... events
   theEvents = new Events();
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
   // ... DefMarker
   theTokenFactory[TKFAC__DEF_MARKER] =
      new TokenFactory_DefMarker();
   assert( theTokenFactory[TKFAC__DEF_MARKER] != 0 );

   // get minimum start time
   if( !getMinStartTime() ) terminate(1);

#ifdef VT_ETIMESYNC
   // get synchronization information
   if( HaveETimeSync )
      if( !theSynchronization->run() ) terminate(1);
#endif // VT_ETIMESYNC

   MASTER
   {
      // unify definitions
      if( !theDefinitions->run() ) terminate(1);

      // unify markers
      if( !theMarkers->run() ) terminate(1);

      // unify statistics
      if( !theStatistics->run() ) terminate(1);
   } // MASTER

#ifdef VT_MPI
   // share token translations to all MPI ranks, if necessary
   //
   if( g_iMPISize > 1 )
      if( !shareTokenTranslations() ) terminate(1);

#endif // VT_MPI

   // unify events
   if( !theEvents->run() ) terminate(1);

   MASTER
   {
      // create OTF master control
      if( !writeMasterControl() ) terminate(1);

      // remove local definitions/events streams, unify control files
      // and temporary files
      if( !cleanUp() ) terminate(1);

      if( theStatistics->isFuncStatAvail() )
      {
         // write summary function statistics to file

         if( !theStatistics->printFuncStat( Params.stats_out_file,
                                            Params.stats_sort_flags ) )
            exit(1);

         // print summary function statistics to stdout
         if( !Params.bequiet )
         {
            std::cout << std::endl;
            theStatistics->printFuncStat( "", Params.stats_sort_flags );
            std::cout << std::endl
               << "The complete function summary was written to file '"
               << Params.stats_out_file << "'." << std::endl;
         }
      }
   } // MASTER

   for( uint32_t i = 0; i < g_vecUnifyCtls.size(); i++ )
   {
#ifdef VT_ETIMESYNC
     if( g_vecUnifyCtls[i]->p_vec_sync_phases )
        delete g_vecUnifyCtls[i]->p_vec_sync_phases;
     if( g_vecUnifyCtls[i]->p_vec_sync_times )
        delete g_vecUnifyCtls[i]->p_vec_sync_times;
     if( g_vecUnifyCtls[i]->p_vec_sync_pairs )
        delete g_vecUnifyCtls[i]->p_vec_sync_pairs;
#endif // VT_ETIMESYNC
      delete g_vecUnifyCtls[i];
   }

#ifdef VT_MPI
   VTUnify_MPI_Finalize();
#endif // VT_MPI

   VPrint( 1, "Done\n" );

   return 0;
}

static bool
getParams( int argc, char ** argv )
{
   bool error = false;

   MASTER
   {
      // parse command line parameters
      error = !parseCommandLine( argc, argv );

      if( !error && !Params.showusage )
      {
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
      }

#ifdef VT_MPI
      if( error ) return false;
#endif // VT_MPI
   } // MASTER

#ifdef VT_MPI
   // share unify parameters to all MPI ranks, if necessary
   if( g_iMPISize > 1 )
      error = !shareParams();
#endif // VT_MPI

   return !error;
}

static bool
getUnifyControls()
{
   VPrint( 1, "Reading unify control files\n" );

   bool error = false;

   MASTER
   {
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

         VPrint( 2, " Opened %s for reading\n", filename );

         char buffer[STRBUFSIZE];
         uint32_t line_no = 1;

         std::vector<uint32_t> vec_streamids;
         std::vector<uint32_t> vec_col_no;
         int64_t ltime[2] = { 0, 1 };
         int64_t offset[2] = { 0, 0 };
#ifdef VT_ETIMESYNC
         std::vector<Synchronization::SyncPhase_struct> * p_vec_sync_phases = 0;
         std::vector<Synchronization::SyncTime_struct> * p_vec_sync_times = 0;
         std::vector<std::pair<uint32_t, uint32_t> > * p_vec_sync_pairs = 0;
#endif // VT_ETIMESYNC

         // read file content
         //
         while( !in.getline((char*)buffer, STRBUFSIZE, ':').eof() && !error )
         {
            // increment line number and remove new-line
            //
            if( buffer[0] == '\n' )
            {
               buffer[0] = '0'; // replace new-line by zero
               line_no++;
               vec_col_no.push_back(0);
            }

            switch( line_no )
            {
               // line_no = 1: ids of input streams
               //
               case 1:
               {
                  int stream_id = atoi(buffer);
                  if( stream_id <= 0 ) { error = true; break; }
                  vec_streamids.push_back( (uint32_t)stream_id );
                  break;
               }
               // line_no = 2: read chronological offsets to global time
               //              and local times
               //
               case 2:
               {
                  switch( ++(vec_col_no[line_no-2]) )
                  {
                     case 1:
                     {
                        ltime[0] = ATOL8(buffer);
                        break;
                     }
                     case 2:
                     {
                        offset[0] = ATOL8(buffer);
                        break;
                     }
                     case 3:
                     {
                        ltime[1] = ATOL8(buffer);
                        break;
                     }
                     case 4:
                     {
                        offset[1] = ATOL8(buffer);
                        break;
                     }
                     default:
                     {
                        error = true;
                        break;
                     }
                  }
                  break;
               }
#ifdef VT_ETIMESYNC
               // line_no = 3: read synchronization mapping
               //              information
               case 3:
               {
                  static Synchronization::SyncPhase_struct sync_phase;

                  HaveETimeSync = true;

                  switch( ++(vec_col_no[line_no-2]) )
                  {
                     case 1:
                     {
                        sync_phase.mapid = atoi(buffer);
                        break;
                     }
                     case 2:
                     {
                        sync_phase.time = ATOL8(buffer);
                        break;
                     }
                     case 3:
                     {
                        sync_phase.duration = ATOL8(buffer);

                        if( !p_vec_sync_phases )
                        {
                           p_vec_sync_phases =
                           new std::vector<Synchronization::SyncPhase_struct>();
                        }
                        p_vec_sync_phases->push_back( sync_phase );

                        vec_col_no[line_no-2] = 0;
                        break;
                     }
                     default:
                     {
                        error = true;
                        break;
                     }
                  }
                  break;
               }
               // line_no = 4-?: read synchronization timestamps of each
               //                synchronization phase (each per line)
               default:
               {
                  static std::pair<uint32_t, uint32_t> sync_pair;
                  static Synchronization::SyncTime_struct sync_time;

                  if( 4 <= line_no &&
                      line_no <= 4 + p_vec_sync_phases->size() - 1 )
                  {
                     switch( ++(vec_col_no[line_no-2]) )
                     {
                        case 1:
                        {
                           sync_pair.first = atoi(buffer);
                           break;
                        }
                        case 2:
                        {
                           sync_pair.second = atoi(buffer);
                           break;
                        }
                        case 3:
                        {
                           sync_time.t[0] = ATOL8(buffer);
                           break;
                        }
                        case 4:
                        {
                           sync_time.t[1] = ATOL8(buffer);
                           break;
                        }
                        case 5:
                        {
                           sync_time.t[2] = ATOL8(buffer);
                           break;
                        }
                        case 6:
                        {
                           sync_time.t[3] = ATOL8(buffer);
                           sync_time.phase_idx = line_no - 4;

                           if( !p_vec_sync_times )
                           {
                              p_vec_sync_times =
                                 new std::vector<Synchronization::SyncTime_struct>();
                              p_vec_sync_pairs =
                                 new std::vector<std::pair<uint32_t, uint32_t> >();
                           }
                           p_vec_sync_pairs->push_back( sync_pair );
                           p_vec_sync_times->push_back( sync_time );
                           vec_col_no[line_no-2] = 0;
                           break;
                        }
                        default:
                        {
                           error = true;
                           break;
                        }
                     }
                  }
                  else
                  {
                     error = true;
                  }
                  break;
               }
#else // VT_ETIMESYNC
               // line_no = 3-n: stuff for enhanced time sync.
               //
               default:
               {
                  error = true;
                  break;
               }
#endif // VT_ETIMESYNC
            }
         }

         // time sync. information parsed in the right way? */
         //
         if( !error )
         {
#ifdef VT_ETIMESYNC
            if( HaveETimeSync )
            {
               for( j = 0; j < line_no; j++ )
               {
                  if( (j+1 == 2 && vec_col_no[j-1] != 4) ||
                      ((j+1>=3 && j+1<=4+p_vec_sync_phases->size()-1) &&
                      (vec_col_no.size() >= j) && vec_col_no[j-1] != 0) )
                  {
                     line_no = j+1;
                     error = true;
                  }
               }
            }
            else
#endif // VT_ETIMESYNC
            {
               if( vec_col_no[0] != 4 )
               {
                  line_no = 2;
                  error = true;
               }
            }
         }

         // parse error during read ?
         //
         if( error )
         {
            std::cerr << filename << ":" << line_no 
                      << ": Could not be parsed" << std::endl;
         }

         // close unify control file
         in.close();

         VPrint( 2, " Closed %s\n", filename );

         if( !error )
         {
            // add to unify control vector
            //
            for( j = 0; j < vec_streamids.size(); j++ )
            {
#ifdef VT_ETIMESYNC
               g_vecUnifyCtls.push_back( 
                  new UnifyControl_struct( vec_streamids[j],
                                           ltime, offset,
                                           p_vec_sync_phases,
                                           p_vec_sync_times,
                                           p_vec_sync_pairs ) );
               p_vec_sync_phases = 0;
               p_vec_sync_times = 0;
               p_vec_sync_pairs = 0;
#else // VT_ETIMESYNC
               g_vecUnifyCtls.push_back( 
                  new UnifyControl_struct( vec_streamids[j],
                                           ltime, offset) );
#endif // VT_ETIMESYNC
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

#ifdef VT_MPI
      if( error ) return false;
#endif // VT_MPI
   } // MASTER

#ifdef VT_MPI
   // share unify parameters to all MPI ranks, if necessary
   if( g_iMPISize > 1 )
      error = !shareUnifyControls();
#endif // VT_MPI

   return !error;
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
	 Params.showusage = true;
	 break;
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
      else if( strcmp( argv[i], "-p" ) == 0
	       || strcmp( argv[i], "--progress" ) == 0 )
      {
	 Params.showprogress = true;
      }
      else if( strcmp( argv[i], "-q" ) == 0 
	       || strcmp( argv[i], "--quiet" ) == 0 )
      {
	 Params.bequiet = true;
	 Params.showprogress = false;
	 Params.verbose_level = 0;
      }
      else if( strcmp( argv[i], "-v" ) == 0 
	       || strcmp( argv[i], "--verbose" ) == 0 )
      {
	 Params.verbose_level++;
      }
      else
      {
	 std::cerr << ExeName << ": invalid option -- " << argv[i] << std::endl;
	 error = true;
      }

      if( error )
	 break;
   }

   // set flag to show usage text, if command line parameters are incomplete
   //
   if( !error && !Params.showusage
       && ( Params.uctl_files_num == 0
            || Params.in_file_prefix.length() == 0 ) )
   {
      Params.showusage = true;
   }

   return !error;
}

static bool
writeMasterControl()
{
   VPrint( 1, "Writing OTF master control\n" );

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

   // add stream/process matching to master control
   //
   for( uint32_t i = 0; i < g_vecUnifyCtls.size(); i++ )
   {
      // do only add streams which have datatype
      //
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

      VPrint( 2, " Opened OTF master control [namestub %s]\n",
	      tmp_out_file_prefix.c_str() );
   }

   // close file master control
   OTF_MasterControl_close( p_uni_mastercontrol );
   // close file manager
   OTF_FileManager_close( p_uni_mastercontrol_manager );

   if( !error )
      VPrint( 2, " Closed OTF master control [namestub %s]\n",
	      tmp_out_file_prefix.c_str() );

   return !error;
}

static bool
getMinStartTime()
{
   bool error = false;

   MASTER
   {
      uint64_t min_start_time = (uint64_t)-1;

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

#ifdef VT_ETIMESYNC
         if( HaveETimeSync )
         {
            theSynchronization->setMinStartTimeForStreamId(
               g_vecUnifyCtls[i]->streamid, minimum );
         }
#endif // VT_ETIMESYNC

         // update minimum timestamp
         if( min_start_time == (uint64_t)-1 || minimum < min_start_time )
            min_start_time = minimum;

         // close record handler
         OTF_HandlerArray_close( p_handler_array );

         // close reader stream
         OTF_RStream_close( p_org_events_rstream );
         // close file manager for reader stream
         OTF_FileManager_close( p_org_events_manager );
      }

      // store minimum timestamp
      g_uMinStartTime = min_start_time;
   } // MASTER

#ifdef VT_MPI
   // share minimum start time to all MPI ranks, if necessary
   if( g_iMPISize > 1 )
      error = !shareMinStartTime();
#endif // VT_MPI

   return !error;
}

static bool
cleanUp()
{
   VPrint( 1, "Cleaning up\n" );

   bool error = false;

   uint32_t i, j;
   char filename1[STRBUFSIZE];
   char filename2[STRBUFSIZE];
   OTF_FileType filetype;

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
	 
	 VPrint( 2, " Removed %s\n", filename1 );
      }

      if( i != Params.uctl_files_num )
         return false;

      // remove local def./events/stats/marker trace files
      //
      for( i = 0; i < g_vecUnifyCtls.size(); i++ )
      {
         for( j = 0; j < 4; j++ )
         {
            bool removed = false;

            switch( j )
            {
               case 0:
                  filetype = OTF_FILETYPE_DEF;
                  break;
               case 1:
                  filetype = OTF_FILETYPE_EVENT;
                  break;
               case 2:
                  filetype = OTF_FILETYPE_STATS;
               case 3:
               default:
                  filetype = OTF_FILETYPE_MARKER;
                  break;
            }

            OTF_getFilename( Params.in_file_prefix.c_str(),
                             g_vecUnifyCtls[i]->streamid, filetype,
                             STRBUFSIZE, filename1 );

            if( !( removed = ( remove( filename1 ) == 0 ) ) )
            {
               OTF_getFilename( Params.in_file_prefix.c_str(),
                                g_vecUnifyCtls[i]->streamid,
                                filetype | OTF_FILECOMPRESSION_COMPRESSED,
                                STRBUFSIZE, filename1 );

               removed = ( remove( filename1 ) == 0 );
            }

            if( removed )
               VPrint( 2, " Removed %s\n", filename1 );
         }
      }

      if( i < g_vecUnifyCtls.size() )
         return false;
   }

   std::string tmp_out_file_prefix =
      Params.out_file_prefix + TmpFileSuffix;

   // rename temporary global definition/marker trace file
   //
   for( i = 0; i < 2; i++ )
   {
      bool renamed = false;

      if( i == 0 ) filetype = OTF_FILETYPE_DEF;
      else filetype = OTF_FILETYPE_MARKER;

      OTF_getFilename( tmp_out_file_prefix.c_str(), 0, filetype,
                       STRBUFSIZE, filename1 );
      OTF_getFilename( Params.out_file_prefix.c_str(), 0, filetype,
                       STRBUFSIZE, filename2 );

      if( !( renamed = ( rename( filename1, filename2 ) == 0 ) ) )
      {
         OTF_getFilename( tmp_out_file_prefix.c_str(), 0,
                          filetype | OTF_FILECOMPRESSION_COMPRESSED,
                          STRBUFSIZE, filename1 );
         OTF_getFilename( Params.out_file_prefix.c_str(), 0,
                          filetype | OTF_FILECOMPRESSION_COMPRESSED,
                          STRBUFSIZE, filename2 );

         renamed = ( rename( filename1, filename2 ) == 0 );
      }

      if( renamed )
      {
         VPrint( 2, " Renamed %s to %s\n", filename1, filename2 );
      }
      else if( i == 0 )
      {
         std::cerr << ExeName << ": Error: Could not rename "
                   << filename1 << " to "
                   << filename2 << std::endl;
         break;
      }
   }
   if( i < 2 ) return false;

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

   VPrint( 2, " Renamed %s to %s\n", filename1, filename2 );

   // rename all temporary event/stats trace files
   //
   for( i = 0; i < g_vecUnifyCtls.size(); i++ )
   {
      for( j = 0; j < 2; j++ )
      {
         bool renamed = false;

         if( j == 0 ) filetype = OTF_FILETYPE_EVENT;
         else filetype = OTF_FILETYPE_STATS;

         OTF_getFilename( tmp_out_file_prefix.c_str(),
                          g_vecUnifyCtls[i]->streamid, filetype,
                          STRBUFSIZE, filename1 );
         OTF_getFilename( Params.out_file_prefix.c_str(),
                          g_vecUnifyCtls[i]->streamid, filetype,
                          STRBUFSIZE, filename2 );

         if( !(renamed = ( rename( filename1, filename2 ) == 0 ) ) )
         {
            OTF_getFilename( tmp_out_file_prefix.c_str(),
                             g_vecUnifyCtls[i]->streamid,
                             filetype | OTF_FILECOMPRESSION_COMPRESSED,
                             STRBUFSIZE, filename1 );
            OTF_getFilename( Params.out_file_prefix.c_str(),
                             g_vecUnifyCtls[i]->streamid,
                             filetype | OTF_FILECOMPRESSION_COMPRESSED,
                             STRBUFSIZE, filename2 );

            renamed = ( rename( filename1, filename2 ) == 0 );
         }

         if( renamed )
         {
            VPrint( 2, " Renamed %s to %s\n", filename1, filename2 );
         }
      }
   }

   return !error;
}

static void
terminate( int status )
{
#ifdef VT_MPI
   if( status == 0 )
      VTUnify_MPI_Finalize();
   else
      VTUnify_MPI_Abort( VTUnify_MPI_COMM_WORLD, (VT_MPI_INT)status );
#endif // VT_MPI

   exit(status);
}

#ifdef VT_MPI

static bool
shareParams()
{
   bool error = false;

   VTUnify_MPI_Barrier( VTUnify_MPI_COMM_WORLD );

   // create MPI datatype for Params_struct
   //

   char **filenames;
   char flags[5];
   VT_MPI_INT blockcounts[5] = { 3*1024, 1, 1, 1, 5 };
   VTUnify_MPI_Aint displ[5];
   VTUnify_MPI_Datatype oldtypes[5] =
   { VTUnify_MPI_CHAR, VTUnify_MPI_UNSIGNED, VTUnify_MPI_UNSIGNED_SHORT,
     VTUnify_MPI_INT, VTUnify_MPI_CHAR };
   VTUnify_MPI_Datatype newtype;

   filenames = new char*[3];
   filenames[0] = new char[3*1024];
   filenames[1] = filenames[0] + ( 1024 * sizeof(char) );
   filenames[2] = filenames[1] + ( 1024 * sizeof(char) );

   VTUnify_MPI_Address( filenames[0], &displ[0] );
   VTUnify_MPI_Address( &(Params.uctl_files_num), &displ[1] );
   VTUnify_MPI_Address( &(Params.verbose_level), &displ[2] );
   VTUnify_MPI_Address( &(Params.stats_sort_flags), &displ[3] );
   VTUnify_MPI_Address( &flags, &displ[4] );
   VTUnify_MPI_Type_struct( 5, blockcounts, displ, oldtypes, &newtype );
   VTUnify_MPI_Type_commit( &newtype );

   // fill some elements of new MPI datatype
   //
   if( g_iMPIRank == 0 )
   {
     strncpy( filenames[0], Params.in_file_prefix.c_str(), 1023 );
     filenames[0][1023] = '\0';
     strncpy( filenames[1], Params.out_file_prefix.c_str(), 1023 );
     filenames[1][1023] = '\0';
     strncpy( filenames[2], Params.stats_out_file.c_str(), 1023 );
     filenames[2][1023] = '\0';
     flags[0] = (char)Params.docompress;
     flags[1] = (char)Params.doclean;
     flags[2] = (char)Params.showusage;
     flags[3] = (char)Params.showprogress;
     flags[4] = (char)Params.bequiet;
   }

   // share unify parameters
   VTUnify_MPI_Bcast( VTUnify_MPI_BOTTOM, 1, newtype, 0, VTUnify_MPI_COMM_WORLD );

   // "receive" unify parameters
   //
   if( g_iMPIRank != 0 )
   {
      Params.in_file_prefix = filenames[0];
      Params.out_file_prefix = filenames[1];
      Params.stats_out_file = filenames[2];
      Params.docompress = (flags[0] == 1);
      Params.doclean = (flags[1] == 1);
      Params.showusage = (flags[2] == 1);
      Params.showprogress = (flags[3] == 1);
      Params.bequiet = (flags[4] == 1);
   }

   delete [] filenames[0];
   delete [] filenames;

   // free MPI datatype
   VTUnify_MPI_Type_free( &newtype );

   return !error;
}

static bool
shareUnifyControls()
{
   VPrint( 1, " Sharing unify control data\n" ); 

   bool error = false;

   char * buffer;
   VT_MPI_INT buffer_size;
   VT_MPI_INT position;
   uint32_t unify_ctl_size;
   uint32_t i;

   VTUnify_MPI_Barrier( VTUnify_MPI_COMM_WORLD );

   if( g_iMPIRank == 0 ) unify_ctl_size = g_vecUnifyCtls.size();

#ifdef VT_ETIMESYNC
   // create MPI datatypes for ...

   VT_MPI_INT blockcounts[3];
   VTUnify_MPI_Aint displ[3];
   VTUnify_MPI_Datatype oldtypes[3];

   // ... Synchronization::SyncPhase_struct
   //
   Synchronization::SyncPhase_struct sync_phase_struct;
   VTUnify_MPI_Datatype sync_phase_newtype;

   blockcounts[0] = blockcounts[1] = blockcounts[2] = 1;
   oldtypes[0] = VTUnify_MPI_UNSIGNED;
   oldtypes[1] = oldtypes[2] = VTUnify_MPI_UNSIGNED_LONG_LONG;

   VTUnify_MPI_Address( &(sync_phase_struct.mapid), &displ[0] );
   VTUnify_MPI_Address( &(sync_phase_struct.time), &displ[1] );
   VTUnify_MPI_Address( &(sync_phase_struct.duration), &displ[2] );
   displ[1] -= displ[0]; displ[2] -= displ[0]; displ[0] = 0;

   VTUnify_MPI_Type_struct( 3, blockcounts, displ, oldtypes,
                            &sync_phase_newtype );
   VTUnify_MPI_Type_commit( &sync_phase_newtype );

   // ... Synchronization::SyncTime_struct
   //
   Synchronization::SyncTime_struct sync_time_struct;
   VTUnify_MPI_Datatype sync_time_newtype;

   blockcounts[0] = 4; blockcounts[1] = 1;
   oldtypes[0] = VTUnify_MPI_UNSIGNED_LONG_LONG;
   oldtypes[1] = VTUnify_MPI_UNSIGNED;

   VTUnify_MPI_Address( &(sync_time_struct.t), &displ[0] );
   VTUnify_MPI_Address( &(sync_time_struct.phase_idx), &displ[1] );
   displ[1] -= displ[0]; displ[0] = 0;

   VTUnify_MPI_Type_struct( 2, blockcounts, displ, oldtypes,
                            &sync_time_newtype );
   VTUnify_MPI_Type_commit( &sync_time_newtype );
#endif // VT_ETIMESYNC

   // calculate buffer size
   //
   if( g_iMPIRank == 0 )
   {
      VT_MPI_INT size;

      buffer_size = 0;

      // g_vecUnifyCtls.size()
      VTUnify_MPI_Pack_size( 1, VTUnify_MPI_UNSIGNED,
                             VTUnify_MPI_COMM_WORLD, &size );
      buffer_size += size;

      for( i = 0; i < unify_ctl_size; i++ )
      {
         // streamid
         VTUnify_MPI_Pack_size( 1, VTUnify_MPI_UNSIGNED,
                                VTUnify_MPI_COMM_WORLD, &size );
         buffer_size += size;

         // ltime, offset
         VTUnify_MPI_Pack_size( 4, VTUnify_MPI_LONG_LONG_INT,
                                VTUnify_MPI_COMM_WORLD, &size );
         buffer_size += size;

#ifdef VT_ETIMESYNC
         // sync_offset
         VTUnify_MPI_Pack_size( 1, VTUnify_MPI_UNSIGNED_LONG_LONG,
                                VTUnify_MPI_COMM_WORLD, &size );
         buffer_size += size;

         // sync_drift
         VTUnify_MPI_Pack_size( 1, VTUnify_MPI_DOUBLE,
                                VTUnify_MPI_COMM_WORLD, &size );
         buffer_size += size;

         // p_vec_sync_phases->size(), p_vec_sync_times->size(),
         // p_vec_sync_pairs->size()
         VTUnify_MPI_Pack_size( 3, VTUnify_MPI_UNSIGNED,
                                VTUnify_MPI_COMM_WORLD, &size );
         buffer_size += size;

         // p_vec_sync_phases
         if( g_vecUnifyCtls[i]->p_vec_sync_phases &&
             g_vecUnifyCtls[i]->p_vec_sync_phases->size() > 0 )
         {
            VTUnify_MPI_Pack_size(
               (VT_MPI_INT)g_vecUnifyCtls[i]->p_vec_sync_phases->size(),
               sync_phase_newtype,
               VTUnify_MPI_COMM_WORLD, &size );
            buffer_size += size;
         }

         // p_vec_sync_times
         if( g_vecUnifyCtls[i]->p_vec_sync_times &&
             g_vecUnifyCtls[i]->p_vec_sync_times->size() > 0 )
         {
            VTUnify_MPI_Pack_size(
               (VT_MPI_INT)g_vecUnifyCtls[i]->p_vec_sync_times->size(),
               sync_time_newtype,
               VTUnify_MPI_COMM_WORLD, &size );
            buffer_size += size;
         }

         // p_vec_sync_pairs
         if( g_vecUnifyCtls[i]->p_vec_sync_pairs &&
             g_vecUnifyCtls[i]->p_vec_sync_pairs->size() > 0 )
         {
            VTUnify_MPI_Pack_size(
               (VT_MPI_INT)g_vecUnifyCtls[i]->p_vec_sync_pairs->size() * 2,
               VTUnify_MPI_UNSIGNED, VTUnify_MPI_COMM_WORLD,
               &size );
            buffer_size += size;
         }
#endif // VT_ETIMESYNC
      }

      // g_mapStreamIdUnifyCtlIdx.size()
      VTUnify_MPI_Pack_size( 1, VTUnify_MPI_UNSIGNED,
                             VTUnify_MPI_COMM_WORLD, &size );
      buffer_size += size;

      // g_mapStreamIdUnifyCtlIdx
      VTUnify_MPI_Pack_size(
         (VT_MPI_INT)g_mapStreamIdUnifyCtlIdx.size() * 2,
         VTUnify_MPI_UNSIGNED, VTUnify_MPI_COMM_WORLD, &size );
      buffer_size += size;
   }

   // share buffer size
   VTUnify_MPI_Bcast( &buffer_size, 1, VTUnify_MPI_INT, 0,
                      VTUnify_MPI_COMM_WORLD );

   // allocate buffer
   buffer = new char[buffer_size];

   // pack unify control data
   //
   if( g_iMPIRank == 0 )
   {
      position = 0;

      // g_vecUnifyCtls.size()
      VTUnify_MPI_Pack( &unify_ctl_size, 1,
                        VTUnify_MPI_UNSIGNED, buffer, buffer_size,
                        &position, VTUnify_MPI_COMM_WORLD );

      for( i = 0; i < unify_ctl_size; i++ )
      {
         // streamid
         VTUnify_MPI_Pack( &(g_vecUnifyCtls[i]->streamid), 1,
                           VTUnify_MPI_UNSIGNED, buffer, buffer_size,
                           &position, VTUnify_MPI_COMM_WORLD );

         // ltime
         VTUnify_MPI_Pack( g_vecUnifyCtls[i]->ltime, 2,
                           VTUnify_MPI_LONG_LONG_INT, buffer, buffer_size,
                           &position, VTUnify_MPI_COMM_WORLD );

         // offset
         VTUnify_MPI_Pack( g_vecUnifyCtls[i]->offset, 2,
                           VTUnify_MPI_LONG_LONG_INT, buffer, buffer_size,
                           &position, VTUnify_MPI_COMM_WORLD );

#ifdef VT_ETIMESYNC
         // sync_offset
         VTUnify_MPI_Pack( &(g_vecUnifyCtls[i]->sync_offset), 1,
                           VTUnify_MPI_UNSIGNED_LONG_LONG, buffer, buffer_size,
                           &position, VTUnify_MPI_COMM_WORLD );

         // sync_drift
         VTUnify_MPI_Pack( &(g_vecUnifyCtls[i]->sync_drift), 1,
                           VTUnify_MPI_DOUBLE, buffer, buffer_size,
                           &position, VTUnify_MPI_COMM_WORLD );

         // p_vec_sync_phases->size()
         //
         uint32_t sync_phases_size = g_vecUnifyCtls[i]->p_vec_sync_phases ?
            g_vecUnifyCtls[i]->p_vec_sync_phases->size() : 0;
         VTUnify_MPI_Pack( &sync_phases_size, 1,
                           VTUnify_MPI_UNSIGNED, buffer, buffer_size,
                           &position, VTUnify_MPI_COMM_WORLD );

         // p_vec_sync_phases
         //
         if( sync_phases_size > 0 )
         {
            uint32_t j;

            Synchronization::SyncPhase_struct * sync_phases =
               new Synchronization::SyncPhase_struct[sync_phases_size];
            for( j = 0; j < sync_phases_size; j++ )
               sync_phases[j] = (*(g_vecUnifyCtls[i]->p_vec_sync_phases))[j];

            VTUnify_MPI_Pack( sync_phases, (VT_MPI_INT)sync_phases_size,
                              sync_phase_newtype, buffer, buffer_size,
                              &position, VTUnify_MPI_COMM_WORLD );

            delete [] sync_phases;
         }

         // p_vec_sync_times->size()
         //
         uint32_t sync_times_size = g_vecUnifyCtls[i]->p_vec_sync_times ?
            g_vecUnifyCtls[i]->p_vec_sync_times->size() : 0;
         VTUnify_MPI_Pack( &sync_times_size, 1,
                           VTUnify_MPI_UNSIGNED, buffer, buffer_size,
                           &position, VTUnify_MPI_COMM_WORLD );

         // p_vec_sync_times
         //
         if( sync_times_size > 0 )
         {
            uint32_t j;

            Synchronization::SyncTime_struct * sync_times =
               new Synchronization::SyncTime_struct[sync_times_size];
            for( j = 0; j < sync_times_size; j++ )
               sync_times[j] = (*(g_vecUnifyCtls[i]->p_vec_sync_times))[j];

            VTUnify_MPI_Pack( sync_times, (VT_MPI_INT)sync_times_size,
                              sync_time_newtype, buffer, buffer_size,
                              &position, VTUnify_MPI_COMM_WORLD );

            delete [] sync_times;
         }

         // p_vec_sync_pairs->size()
         //
         uint32_t sync_pairs_size = g_vecUnifyCtls[i]->p_vec_sync_pairs ?
            g_vecUnifyCtls[i]->p_vec_sync_pairs->size() : 0;
         VTUnify_MPI_Pack( &sync_pairs_size, 1,
                           VTUnify_MPI_UNSIGNED, buffer, buffer_size,
                           &position, VTUnify_MPI_COMM_WORLD );

         // p_vec_sync_pairs
         //
         if( sync_pairs_size > 0 )
         {
            uint32_t * sync_pairs_firsts = new uint32_t[sync_pairs_size];
            uint32_t * sync_pairs_seconds = new uint32_t[sync_pairs_size];
            uint32_t j;

            for( j = 0; j < sync_pairs_size; j++ )
            {
               sync_pairs_firsts[j] = (*(g_vecUnifyCtls[i]->p_vec_sync_pairs))[j].first;
               sync_pairs_seconds[j] = (*(g_vecUnifyCtls[i]->p_vec_sync_pairs))[j].second;
            }

            VTUnify_MPI_Pack( sync_pairs_firsts, (VT_MPI_INT)sync_pairs_size,
                              VTUnify_MPI_UNSIGNED, buffer, buffer_size,
                              &position, VTUnify_MPI_COMM_WORLD );
            VTUnify_MPI_Pack( sync_pairs_seconds, (VT_MPI_INT)sync_pairs_size,
                              VTUnify_MPI_UNSIGNED, buffer, buffer_size,
                              &position, VTUnify_MPI_COMM_WORLD );

            delete [] sync_pairs_firsts;
            delete [] sync_pairs_seconds;
         }
#endif // VT_ETIMESYNC
      }

      // g_mapStreamIdUnifyCtlIdx.size()
      uint32_t map_size = g_mapStreamIdUnifyCtlIdx.size();
      VTUnify_MPI_Pack( &map_size, 1,
                        VTUnify_MPI_UNSIGNED, buffer, buffer_size,
                        &position, VTUnify_MPI_COMM_WORLD );

      // g_mapStreamIdUnifyCtlIdx
      //
      if( map_size > 0 )
      {
         uint32_t * map_firsts = new uint32_t[map_size];
         uint32_t * map_seconds = new uint32_t[map_size];
         std::map<uint32_t, uint32_t>::iterator it;
         uint32_t j;

         for( it = g_mapStreamIdUnifyCtlIdx.begin(), j = 0;
              it != g_mapStreamIdUnifyCtlIdx.end() && j < map_size;
              it++, j++ )
         {
            map_firsts[j] = it->first;
            map_seconds[j] = it->second;
         }

         VTUnify_MPI_Pack( map_firsts, (VT_MPI_INT)map_size,
                           VTUnify_MPI_UNSIGNED, buffer, buffer_size,
                           &position, VTUnify_MPI_COMM_WORLD );
         VTUnify_MPI_Pack( map_seconds, (VT_MPI_INT)map_size,
                           VTUnify_MPI_UNSIGNED, buffer, buffer_size,
                           &position, VTUnify_MPI_COMM_WORLD );

         delete [] map_firsts;
         delete [] map_seconds;
      }
   }

   // share packed unify control data
   VTUnify_MPI_Bcast( buffer, buffer_size, VTUnify_MPI_PACKED, 0,
                      VTUnify_MPI_COMM_WORLD );

   // unpack unify control data
   //
   if( g_iMPIRank != 0 )
   {
      position = 0;

      // g_vecUnifyCtls.size()
      //
      VTUnify_MPI_Unpack( buffer, buffer_size, &position,
                          &unify_ctl_size, 1,
                          VTUnify_MPI_UNSIGNED, VTUnify_MPI_COMM_WORLD );
      g_vecUnifyCtls.resize( unify_ctl_size );

      for( i = 0; i < unify_ctl_size; i++ )
      {
         // create new unify control element
         g_vecUnifyCtls[i] = new UnifyControl_struct();

         // streamid
         VTUnify_MPI_Unpack( buffer, buffer_size, &position,
                             &(g_vecUnifyCtls[i]->streamid), 1,
                             VTUnify_MPI_UNSIGNED, VTUnify_MPI_COMM_WORLD );

         // ltime
         VTUnify_MPI_Unpack( buffer, buffer_size, &position,
                             g_vecUnifyCtls[i]->ltime, 2,
                             VTUnify_MPI_LONG_LONG_INT,
                             VTUnify_MPI_COMM_WORLD );
         // offset
         VTUnify_MPI_Unpack( buffer, buffer_size, &position,
                             g_vecUnifyCtls[i]->offset, 2,
                             VTUnify_MPI_LONG_LONG_INT,
                             VTUnify_MPI_COMM_WORLD );

#ifdef VT_ETIMESYNC
         // sync_offset
         VTUnify_MPI_Unpack( buffer, buffer_size, &position,
                             &(g_vecUnifyCtls[i]->sync_offset), 1,
                             VTUnify_MPI_UNSIGNED_LONG_LONG,
                            VTUnify_MPI_COMM_WORLD );

         // sync_drift
         VTUnify_MPI_Unpack( buffer, buffer_size, &position,
                             &(g_vecUnifyCtls[i]->sync_drift), 1,
                             VTUnify_MPI_DOUBLE, VTUnify_MPI_COMM_WORLD );

         // p_vec_sync_phases->size()
         //
         uint32_t sync_phases_size;
         VTUnify_MPI_Unpack( buffer, buffer_size, &position,
                             &sync_phases_size, 1,
                             VTUnify_MPI_UNSIGNED, VTUnify_MPI_COMM_WORLD );

         // p_vec_sync_phases
         //
         if( sync_phases_size > 0 )
         {
            uint32_t j;

            HaveETimeSync = true;

            Synchronization::SyncPhase_struct * sync_phases =
               new Synchronization::SyncPhase_struct[sync_phases_size];

            VTUnify_MPI_Unpack( buffer, buffer_size, &position,
                                sync_phases, (VT_MPI_INT)sync_phases_size,
                                sync_phase_newtype, VTUnify_MPI_COMM_WORLD );

            g_vecUnifyCtls[i]->p_vec_sync_phases =
               new std::vector<Synchronization::SyncPhase_struct>();
            for( j = 0; j < sync_phases_size; j++ )
               g_vecUnifyCtls[i]->p_vec_sync_phases->push_back( sync_phases[j] );

            delete [] sync_phases;
         }

         // p_vec_sync_times->size()
         //
         uint32_t sync_times_size;
         VTUnify_MPI_Unpack( buffer, buffer_size, &position,
                             &sync_times_size, 1,
                             VTUnify_MPI_UNSIGNED, VTUnify_MPI_COMM_WORLD );

         // p_vec_sync_times
         //
         if( sync_times_size > 0 )
         {
            uint32_t j;

            Synchronization::SyncTime_struct * sync_times =
               new Synchronization::SyncTime_struct[sync_times_size];

            VTUnify_MPI_Unpack( buffer, buffer_size, &position,
                                sync_times, (VT_MPI_INT)sync_times_size,
                                sync_time_newtype, VTUnify_MPI_COMM_WORLD );

            g_vecUnifyCtls[i]->p_vec_sync_times =
               new std::vector<Synchronization::SyncTime_struct>();
            for( j = 0; j < sync_times_size; j++ )
               g_vecUnifyCtls[i]->p_vec_sync_times->push_back( sync_times[j] );

            delete [] sync_times;
         }

         // p_vec_sync_pairs->size()
         //
         uint32_t sync_pairs_size;
         VTUnify_MPI_Unpack( buffer, buffer_size, &position,
                             &sync_pairs_size, 1,
                             VTUnify_MPI_UNSIGNED, VTUnify_MPI_COMM_WORLD );

         // p_vec_sync_pairs
         //
         if( sync_pairs_size > 0 )
         {
            uint32_t * sync_pairs_firsts = new uint32_t[sync_pairs_size];
            uint32_t * sync_pairs_seconds = new uint32_t[sync_pairs_size];
            uint32_t j;

            VTUnify_MPI_Unpack( buffer, buffer_size, &position,
                                sync_pairs_firsts, (VT_MPI_INT)sync_pairs_size,
                                VTUnify_MPI_UNSIGNED, VTUnify_MPI_COMM_WORLD );
            VTUnify_MPI_Unpack( buffer, buffer_size, &position,
                                sync_pairs_seconds, (VT_MPI_INT)sync_pairs_size,
                                VTUnify_MPI_UNSIGNED, VTUnify_MPI_COMM_WORLD );

            g_vecUnifyCtls[i]->p_vec_sync_pairs =
               new std::vector<std::pair<uint32_t, uint32_t> >();
            for( j = 0; j < sync_pairs_size; j++ )
            {
               g_vecUnifyCtls[i]->p_vec_sync_pairs->push_back(
                  std::make_pair( sync_pairs_firsts[j], sync_pairs_seconds[j] ) );
            }

            delete [] sync_pairs_firsts;
            delete [] sync_pairs_seconds;
         }
#endif // VT_ETIMESYNC
      }

      // g_mapStreamIdUnifyCtlIdx.size()
      uint32_t map_size;
      VTUnify_MPI_Unpack( buffer, buffer_size, &position,
                          &map_size, 1,
                          VTUnify_MPI_UNSIGNED, VTUnify_MPI_COMM_WORLD );

      // g_mapStreamIdUnifyCtlIdx
      //
      if( map_size > 0 )
      {
         uint32_t * map_firsts = new uint32_t[map_size];
         uint32_t * map_seconds = new uint32_t[map_size];
         uint32_t j;

         VTUnify_MPI_Unpack( buffer, buffer_size, &position,
                             map_firsts, (VT_MPI_INT)map_size,
                             VTUnify_MPI_UNSIGNED, VTUnify_MPI_COMM_WORLD );
         VTUnify_MPI_Unpack( buffer, buffer_size, &position,
                             map_seconds, (VT_MPI_INT)map_size,
                             VTUnify_MPI_UNSIGNED, VTUnify_MPI_COMM_WORLD );

         for( j = 0; j < map_size; j++ )
         {
           g_mapStreamIdUnifyCtlIdx.insert(
              std::make_pair( (uint32_t)map_firsts[j],
                              (uint32_t)map_seconds[j] ) );
         }

         delete [] map_firsts;
         delete [] map_seconds;
      }
   }

   delete [] buffer;

#ifdef VT_ETIMESYNC
   // free MPI datatypes
   VTUnify_MPI_Type_free( &sync_phase_newtype );
   VTUnify_MPI_Type_free( &sync_time_newtype );
#endif // VT_ETIMESYNC

   return !error;
}

static bool
shareMinStartTime()
{
   bool error = false;

   VTUnify_MPI_Barrier( VTUnify_MPI_COMM_WORLD );

   // share minimum start time
   VTUnify_MPI_Bcast( &g_uMinStartTime, 1, VTUnify_MPI_UNSIGNED_LONG_LONG, 0, VTUnify_MPI_COMM_WORLD );

#ifdef VT_ETIMESYNC
   if( HaveETimeSync )
   {
      uint64_t * min_start_times = new uint64_t[g_vecUnifyCtls.size()];
      uint32_t i;

      if( g_iMPIRank == 0 )
      {
         for( i = 0; i < g_vecUnifyCtls.size(); i++ )
         {
            min_start_times[i] =
               theSynchronization->getMinStartTimeForStreamId(
                  g_vecUnifyCtls[i]->streamid );
         }
      }

      // share stream's minimum start times
      VTUnify_MPI_Bcast( min_start_times, (VT_MPI_INT)g_vecUnifyCtls.size(),
                         VTUnify_MPI_UNSIGNED_LONG_LONG, 0,
                         VTUnify_MPI_COMM_WORLD );

      if( g_iMPIRank != 0 )
      {
         for( i = 0; i < g_vecUnifyCtls.size(); i++ )
         {
            theSynchronization->setMinStartTimeForStreamId(
               g_vecUnifyCtls[i]->streamid, min_start_times[i] );
         }
      }

      delete [] min_start_times;
   }
#endif // VT_ETIMESYNC

   return !error;
}

static bool
shareTokenTranslations()
{
   VPrint( 1, "Sharing token translation tables\n" );

   bool error = false;

   char * buffer;
   VT_MPI_INT buffer_size;
   VT_MPI_INT position;
   uint32_t i;

   VTUnify_MPI_Barrier( VTUnify_MPI_COMM_WORLD );

   // calculate buffer size
   //
   if( g_iMPIRank == 0 )
   {
      buffer_size = 0;

      for( i = 0; i < TKFAC_NUM; i++ )
      {
         buffer_size += theTokenFactory[i]->getPackSize();
      }
   }

   // share buffer size
   VTUnify_MPI_Bcast( &buffer_size, 1, VTUnify_MPI_INT, 0,
                      VTUnify_MPI_COMM_WORLD );

   // allocate buffer
   buffer = new char[buffer_size];

   // pack token translation data
   //
   if( g_iMPIRank == 0 )
   {
      position = 0;

      for( i = 0; i < TKFAC_NUM; i++ )
      {
         theTokenFactory[i]->packTranslations( buffer, buffer_size,
                                               &position );
      }
   }

   // share packed token translation data
   VTUnify_MPI_Bcast( buffer, buffer_size, VTUnify_MPI_PACKED, 0,
                      VTUnify_MPI_COMM_WORLD );

   // unpack token translation data
   //
   if( g_iMPIRank != 0 )
   {
      position = 0;

      for( i = 0; i < TKFAC_NUM; i++ )
      {
         theTokenFactory[i]->unpackTranslations( buffer, buffer_size,
                                                 &position );
      }
   }

   delete [] buffer;

   return !error;
}

#endif // VT_MPI

void VPrint( uint8_t level, const char * fmt, ... )
{
   va_list ap;

   if( Params.verbose_level >= level )
   {
      MASTER
      {
         va_start( ap, fmt );
         vprintf( fmt, ap );
         va_end( ap );
      } // MASTER
   }
}

void PVPrint( uint8_t level, const char * fmt, ... )
{
   va_list ap;

   if( Params.verbose_level >= level )
   {
      va_start( ap, fmt );
#if !(defined(VT_MPI) || (defined(HAVE_OMP) && HAVE_OMP))
      vprintf( fmt, ap );
#else // !(VT_MPI || HAVE_OMP)
      char msg[1024] = "";
#  if defined(VT_MPI) && !(defined(HAVE_OMP) && HAVE_OMP)
      snprintf( msg, sizeof(msg)-1, "[%d] ", (int)g_iMPIRank );
#  elif !defined(VT_MPI) && (defined(HAVE_OMP) && HAVE_OMP)
      if( omp_in_parallel() )
         snprintf( msg, sizeof(msg)-1, "[%d] ", omp_get_thread_num() );
#  else // !VT_MPI && HAVE_OMP
      if( omp_in_parallel() )
      {
         snprintf( msg, sizeof(msg)-1, "[%d:%d] ", (int)g_iMPIRank,
                   omp_get_thread_num() );
      }
      else
      {
         snprintf( msg, sizeof(msg)-1, "[%d] ", (int)g_iMPIRank );
      }
#  endif // !VT_MPI && HAVE_OMP
      vsnprintf(msg + strlen(msg), sizeof(msg)-1, fmt, ap);
#  if defined(HAVE_OMP) && HAVE_OMP
#     pragma omp critical
#  endif // HAVE_OMP
      printf( "%s", msg );
#endif // !(VT_MPI || HAVE_OMP)
      va_end( ap );
   }
}

uint64_t CorrectTime( uint32_t loccpuid, uint64_t time )
{
   assert( loccpuid > 0 );

   std::map<uint32_t, uint32_t>::iterator it = 
      g_mapStreamIdUnifyCtlIdx.find( loccpuid );
   assert( it != g_mapStreamIdUnifyCtlIdx.end() );
   assert( it->second < g_vecUnifyCtls.size() );

#ifdef VT_ETIMESYNC
   if( HaveETimeSync )
   {
      // linear synchronization between synchronization points
      int64_t offset = g_vecUnifyCtls[it->second]->sync_offset;
      double drift = g_vecUnifyCtls[it->second]->sync_drift;

      return (uint64_t)( offset + (uint64_t)( drift * (double)time ) );
   }
   else
#endif // VT_ETIMESYNC
   {
      int64_t * ltime = g_vecUnifyCtls[it->second]->ltime;
      int64_t * offset = g_vecUnifyCtls[it->second]->offset;

      double d_time = (double)time;
      double d_ltime0 = (double)ltime[0];
      double d_offset0 = (double)offset[0];
      double d_ltime1 = (double)ltime[1];
      double d_offset1 = (double)offset[1];

      return (uint64_t)((d_time +
			 (((d_offset1 - d_offset0) / (d_ltime1 - d_ltime0))
			  * (d_time - d_ltime0)) + d_offset0) - g_uMinStartTime);
   }
}
