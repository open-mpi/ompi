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

#include "vt_unify.h"
#include "vt_unify_defs.h"
#include "vt_unify_events_stats.h"
#include "vt_unify_hooks.h"
#include "vt_unify_markers.h"
#include "vt_unify_sync.h"
#include "vt_unify_tkfac.h"
#include "vt_unify_usrcom.h"

#ifdef VT_LIB
#  include "vt_unify_lib.h"
#  define VTUNIFY_MAIN VTUnify
#else // VT_LIB
#  define VTUNIFY_MAIN main
#endif // VT_LIB

#include "otf.h"

#include <fstream>
#include <iostream>
#include <map>
#include <sstream>
#include <string>
#include <vector>

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

// local functions
//

// get unify parameters
static bool getParams( int argc, char ** argv );

// get unify controls
static bool getUnifyControls( void );

// parse command line options
static bool parseCommandLine( int argc, char ** argv );

// write OTF master control file
static bool writeMasterControl( void );

// clean up (e.g. rename temporary output files)
static bool cleanUp( void );

// show usage text
static void showUsage( void );

#ifdef VT_MPI
   // share unify parameters to all ranks
   static bool shareParams( void );

   // share unify controls to all ranks
   static bool shareUnifyControls( void );
#endif // VT_MPI

// global variables
//

// name of program's executable
#ifdef VT_MPI
#  ifdef VT_LIB
      const std::string ExeName = "libvt-mpi-unify";
#  else // VT_LIB
      const std::string ExeName = "vtunify-mpi";
#  endif // VT_LIB
#else // VT_MPI
   const std::string ExeName = "vtunify";
#endif // VT_MPI

// temporary output file suffix
const std::string TmpFileSuffix = "__ufy.tmp";

// output file prefix which used if local input files shall be kept
const std::string UniFilePrefix = "u_";

// unify parameters
ParamsS Params;

// vector of unify controls
std::vector<UnifyControlS*> UnifyCtls;

// map stream id <-> unify control
std::map<uint32_t, UnifyControlS*> StreamId2UnifyCtl;

// vector of stream ids to process by my rank
std::vector<uint32_t> MyStreamIds;

#ifdef VT_MPI
   // number of MPI-ranks
   VT_MPI_INT NumRanks;

   // MPI-rank of calling process
   VT_MPI_INT MyRank;

   // map stream id <-> processing MPI-rank
   std::map<uint32_t, VT_MPI_INT> StreamId2Rank;

   // map MPI-rank <-> stream ids
   std::map<VT_MPI_INT, std::set<uint32_t> > Rank2StreamIds;
#endif // VT_MPI

int
VTUNIFY_MAIN( int argc, char ** argv )
{
   bool error = false;

#ifdef VT_MPI
   // initialize MPI
   //
#  ifndef VT_LIB
   CALL_MPI( MPI_Init( (VT_MPI_INT*)&argc, &argv ) );
#  endif // !VT_LIB
   CALL_MPI( MPI_Comm_size( MPI_COMM_WORLD, &NumRanks ) );
   CALL_MPI( MPI_Comm_rank( MPI_COMM_WORLD, &MyRank ) );
#endif // VT_MPI

   // create instance of classes ...
   //

   // ... HooksC
   theHooks = new HooksC();
   assert( theHooks );
   // ... TokenFactoryC
   theTokenFactory = new TokenFactoryC();
   assert( theTokenFactory );
   // ... DefinitionsC
   theDefinitions = new DefinitionsC();
   assert( theDefinitions );
   // ... MarkersC
   theMarkers = new MarkersC();
   assert( theMarkers );
   // ... EventsAndStatsC (for unifying events)
   theEvents = new EventsAndStatsC( EventsAndStatsC::SCOPE_EVENTS );
   assert( theEvents );
   // ... EventsAndStatsC (for unifying statistics)
   theStatistics = new EventsAndStatsC( EventsAndStatsC::SCOPE_STATS );
   assert( theStatistics );
   // ... TimeSyncC
   theTimeSync = new TimeSyncC();
   assert( theTimeSync );
   // ... UserComC
   theUserCom = new UserComC();
   assert( theUserCom );

   do
   {
      // get unify parameters
      if( (error = !getParams( argc, argv )) )
         break;

      // show usage text, if necessary/desired
      //
      if( Params.showusage )
      {
         MASTER showUsage();
         break;
      }

      // show VT version, if desired
      //
      if( Params.showversion )
      {
         MASTER std::cout << PACKAGE_VERSION << std::endl;
         break;
      }

      // register hook classes
      theHooks->registerHooks();

      // trigger initialization hook
      theHooks->triggerInitHook();

      // read unify control files (*.uctl)
      if( (error = !getUnifyControls()) )
         break;

      // unify definitions
      if( (error = !theDefinitions->run()) )
         break;

      // unify markers
      if( (error = !theMarkers->run()) )
         break;

#ifdef VT_MPI
     if( NumRanks > 1 )
     {
        // share user communication ids to all ranks
        if( (error = !theUserCom->share()) )
           break;
     }
#endif // VT_MPI

     // unify events
     if( !Params.onlystats && (error = !theEvents->run()) )
        break;

     // unify statistics
     if( (error = !theStatistics->run()) )
        break;

     // create OTF master control file
     //
     MASTER error = !writeMasterControl();
     if( SyncError( &error ) )
        break;

     // finally, clean up (e.g. rename temporary output files)
     if( (error = !cleanUp()) )
        break;

     // trigger finalization hook
     theHooks->triggerFinalizeHook( error );

     VPrint( 1, "Done\n" );

   } while( false );

   // delete instance of classes ...
   //

   // ... HooksC
   delete theHooks;
   // ... DefinitionsC
   delete theDefinitions;
   // ... MarkersC
   delete theMarkers;
   // ... EventsAndStatsC (for unifying events)
   delete theEvents;
   // ... EventsAndStatsC (for unifying statistics)
   delete theStatistics;
   // ... TokenFactoryC
   delete theTokenFactory;
   // ... TimeSyncC
   delete theTimeSync;
   // ... UserComC
   delete theUserCom;

   // clear vector of unify controls
   //
   for( uint32_t i = 0; i < UnifyCtls.size(); i++ )
      delete UnifyCtls[i];

#if (defined(VT_MPI) && !defined(VT_LIB))
   // either abort on error ...
   //
   if( error )
   {
      CALL_MPI( MPI_Abort( MPI_COMM_WORLD, 1 ) );
   }
   // ... or finalize
   else
   {
      if( NumRanks > 1 )
         CALL_MPI( MPI_Barrier( MPI_COMM_WORLD ) );

      CALL_MPI( MPI_Finalize() );
   }
#endif // VT_MPI && !VT_LIB

   return (error) ? 1 : 0;
}

static bool
getParams( int argc, char ** argv )
{
   bool error = false;

   MASTER
   {
      // parse command line parameters
      error = !parseCommandLine( argc, argv );

      if( !error && !Params.showusage && !Params.showversion )
      {
         // set namestub of output streams, if necessary
         if( Params.out_file_prefix.length() == 0 )
            Params.out_file_prefix = Params.in_file_prefix;

         // if input files shall be kept and output filename
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

#ifdef VT_UNIFY_HOOKS_PROF
         // set profile output filename, if necessary
         if( Params.prof_out_file.length() == 0 )
            Params.prof_out_file = Params.out_file_prefix + ".prof.txt";
#endif // VT_UNIFY_HOOKS_PROF
      }
   } // MASTER

#ifdef VT_MPI
   SyncError( &error );

   // share unify parameters to all ranks, if necessary
   //
   if( !error && NumRanks > 1 )
   {
      error = !shareParams();
      SyncError( &error );
   }
#endif // VT_MPI

   return !error;
}

static bool
getUnifyControls()
{
   bool error = false;

   VPrint( 1, "Reading unify control file\n" );

   // trigger phase pre hook
   theHooks->triggerPhaseHook( HooksC::Phase_GetUnifyControls_pre );

   std::ifstream in;
   char filename[STRBUFSIZE];

   MASTER
   {
      // compose unify control file name
      snprintf( filename, sizeof( filename ) - 1, "%s.uctl",
                Params.in_file_prefix.c_str() );

      // open unify control file for reading
      //
      in.open( filename );
      if( !in )
      {
         std::cerr << ExeName << ": Error: "
                   << "Could not open file " << filename << std::endl;

         error = true;
      }

      VPrint( 3, " Opened %s for reading\n", filename );
   } // MASTER
   if( SyncError( &error ) )
      return false;

   MASTER
   {
      // flag which indicates whether any stream is available
      bool any_stream_avail = false;

      char buffer[STRBUFSIZE];
      uint32_t line_no = 1;
      uint32_t i;

      do
      {
         uint32_t line_no_sec = 1;

         std::vector<uint32_t> streamids;
         std::vector<bool>     streamavs;
         std::vector<uint32_t> col_no;
         int64_t ltime[2] = { 0, 1 };
         int64_t offset[2] = { 0, 0 };
#ifdef VT_ETIMESYNC
         std::vector<ETimeSyncC::SyncPhaseS>         sync_phases;
         std::vector<ETimeSyncC::SyncTimeS>          sync_times;
         std::vector<std::pair<uint32_t, uint32_t> > sync_pairs;
#endif // VT_ETIMESYNC

         // read file content
         //
         while( !in.getline((char*)buffer, STRBUFSIZE, ':').eof() && !error )
         {
            // new unify control section?
            if( std::string(buffer).find( '*' ) != std::string::npos )
            {
              // if that's the first section, continue reading
              if( line_no == 1 )
              {
                continue;
              }
              // otherwise, leave read loop to finalize previous section
              else
              {
                line_no++;
                break;
              }
            }

            // increment line number and remove new-line
            //
            if( buffer[0] == '\n' )
            {
               buffer[0] = '0'; // replace new-line by zero
               line_no_sec++;
               line_no++;
               col_no.push_back(0);
            }

            switch( line_no_sec )
            {
               // line_no_sec = 1: ids of input streams
               //
               case 1:
               {
                  uint32_t stream_id;
                  bool stream_avail = true;
                  std::istringstream iss;

                  // is stream not available?
                  if( buffer[strlen( buffer ) - 1] == '!' )
                  {
                     stream_avail = false;
                     // remove trailing '!'
                     buffer[strlen( buffer ) - 1] = '\0';
                  }
                  else
                  {
                     any_stream_avail = true;
                  }

                  iss.str( buffer );
                  assert( iss );
                  error = !( iss >> std::hex >> stream_id );

                  if( !error && stream_id > 0 )
                  {
                     streamids.push_back( stream_id );
                     streamavs.push_back( stream_avail );
                  }

                  break;
               }
               // line_no_sec = 2: read chronological offsets to global time
               //                  and local times
               //
               case 2:
               {
                  std::istringstream iss( buffer );
                  assert( iss );

                  switch( ++(col_no[line_no_sec-2]) )
                  {
                     case 1:
                     {
                        error = !( iss >> std::hex >> ltime[0] );
                        break;
                     }
                     case 2:
                     {
                        // std::stringstream expects unsigned values after
                        // switching format to std::hex; read unsigned and
                        // convert to signed afterwards
                        uint64_t tmp;
                        if( !( error = !(iss >> std::hex >> tmp ) ) )
                           offset[0] = tmp;
                        break;
                     }
                     case 3:
                     {
                        error = !( iss >> std::hex >> ltime[1] );
                        break;
                     }
                     case 4:
                     {
                        // std::stringstream expects unsigned values after
                        // switching format to std::hex; read unsigned and
                        // convert to signed afterwards
                        uint64_t tmp;
                        if( !( error = !(iss >> std::hex >> tmp ) ) )
                           offset[1] = tmp;
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
               // line_no_sec = 3: read synchronization mapping information
               //
               case 3:
               {
                  static ETimeSyncC::SyncPhaseS sync_phase;
                  std::istringstream iss( buffer );
                  assert( iss );

                  theTimeSync->setSyncMethod( TimeSyncC::METHOD_ENHANCED );

                  switch( ++(col_no[line_no_sec-2]) )
                  {
                     case 1:
                     {
                        error = !( iss >> std::hex >> sync_phase.mapid );
                        break;
                     }
                     case 2:
                     {
                        error = !( iss >> std::hex >> sync_phase.time );
                        break;
                     }
                     case 3:
                     {
                        error = !( iss >> std::hex >> sync_phase.duration );

                        if( !error )
                        {
                           sync_phases.push_back( sync_phase );
                           col_no[line_no_sec-2] = 0;
                        }

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
               // line_no_sec = 4-n: read synchronization timestamps of each
               //                    synchronization phase (each per line)
               //
               default:
               {
                  static std::pair<uint32_t, uint32_t> sync_pair;
                  static ETimeSyncC::SyncTimeS sync_time;
                  std::istringstream iss( buffer );
                  assert( iss );

                  if( 4 <= line_no_sec &&
                      line_no_sec <= 4 + sync_phases.size() - 1 )
                  {
                     switch( ++(col_no[line_no_sec-2]) )
                     {
                        case 1:
                        {
                           error = !( iss >> std::hex >> sync_pair.first );
                           break;
                        }
                        case 2:
                        {
                           error = !( iss >> std::hex >> sync_pair.second );
                           break;
                        }
                        case 3:
                        {
                           error = !( iss >> std::hex >> sync_time.t[0] );
                           break;
                        }
                        case 4:
                        {
                           error = !( iss >> std::hex >> sync_time.t[1] );
                           break;
                        }
                        case 5:
                        {
                           error = !( iss >> std::hex >> sync_time.t[2] );
                           break;
                        }
                        case 6:
                        {
                           error = !( iss >> std::hex >> sync_time.t[3] );

                           if( !error )
                           {
                              sync_time.phase_idx = line_no_sec - 4;

                              sync_pairs.push_back( sync_pair );
                              sync_times.push_back( sync_time );

                              col_no[line_no_sec-2] = 0;
                           }

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
               // line_no_sec = 3-n: stuff for enhanced time sync.
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
            if( theTimeSync->getSyncMethod() == TimeSyncC::METHOD_ENHANCED )
            {
               for( i = 0; i < line_no_sec; i++ )
               {
                  if( (i+1 == 2 && col_no[i-1] != 4) ||
                      ((i+1>=3 && i+1<=4+sync_phases.size()-1) &&
                      (col_no.size() >= i) && col_no[i-1] != 0) )
                  {
                     line_no_sec = i+1;
                     error = true;
                  }
               }
            }
            else
#endif // VT_ETIMESYNC
            {
               if( col_no.empty() || col_no[0] != 4 )
               {
                  line_no_sec = 2;
                  error = true;
               }
            }
         }

         // show error message on parse error
         //
         if( error )
         {
            std::cerr << filename << ":" << line_no 
                      << ": Could not be parsed" << std::endl;
         }
         // otherwise, add unify control to vector
         //
         else
         {
            for( i = 0; i < streamids.size(); i++ )
            {
               uint32_t streamid = streamids[i];
               uint32_t pstreamid = (i == 0) ? 0 : streamids[0];
               bool     streamav = streamavs[i];

#ifdef VT_ETIMESYNC
               UnifyCtls.push_back(
                  new UnifyControlS( streamid, pstreamid, streamav, ltime,
                     offset, sync_phases, sync_times, sync_pairs ) );
#else // VT_ETIMESYNC
               UnifyCtls.push_back(
                  new UnifyControlS( streamid, pstreamid, streamav, ltime,
                     offset) );
#endif // VT_ETIMESYNC
            }

#ifdef VT_ETIMESYNC
            sync_phases.clear();
            sync_times.clear();
            sync_pairs.clear();
#endif // VT_ETIMESYNC
         }

      } while( !error && in.good() );

      // close unify control file
      in.close();

      if( !error )
         VPrint( 3, " Closed %s\n", filename );

      if( !any_stream_avail )
      {
         std::cerr << ExeName << ": Error: "
                   << "No streams are available" << std::endl;
         error = true;
      }

   } // MASTER

#ifdef VT_MPI
   SyncError( &error );

   // share unify parameters to all ranks, if necessary
   //
   if( !error && NumRanks > 1 )
   {
      error = !shareUnifyControls();
      SyncError( &error );
   }
#endif // VT_MPI

   if( !error )
   {
      // get stream context information
      //
      for( uint32_t i = 0; i < UnifyCtls.size(); i++ )
      {
         UnifyControlS * uctl = UnifyCtls[i];

         // set stream id/unify control mapping
         StreamId2UnifyCtl[uctl->streamid] = uctl;

         if( uctl->stream_avail )
         {
#ifdef VT_MPI
            if( NumRanks > 1 )
            {
               // assign stream id to rank, whereas childs will not be
               // separated from its parent stream id
               //

               static VT_MPI_INT rank = 0;

               // assign stream id to rank
               if( rank == MyRank )
                  MyStreamIds.push_back( uctl->streamid );

               // set stream id/rank mapping
               StreamId2Rank[uctl->streamid] = rank;

               // add stream id to processing rank
               Rank2StreamIds[rank].insert( uctl->streamid );

               // get rank for the next stream id
               //
               if( i < UnifyCtls.size() - 1 && UnifyCtls[i+1]->pstreamid == 0 )
               {
                  if( rank + 1 < NumRanks )
                     rank++;
                  else
                     rank = 0;
               }
            }
            else
#endif // VT_MPI
            {
               MyStreamIds.push_back( uctl->streamid );
            }
         }
      }

#if defined(HAVE_OMP) && HAVE_OMP
      // reset number of threads, if necessary
      //
      if( !MyStreamIds.empty() &&
          (int)MyStreamIds.size() < omp_get_max_threads() )
      {
         omp_set_num_threads( (int)MyStreamIds.size() );
         PVPrint( 3, "Reset maximum number of threads to %d\n",
                  omp_get_max_threads() );
      }
#endif // HAVE_OMP

      // trigger phase post hook
      theHooks->triggerPhaseHook( HooksC::Phase_GetUnifyControls_post );
   }

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
      else if( strcmp( argv[i], "-V" ) == 0
               || strcmp( argv[i], "--version" ) == 0 )
      {
         Params.showversion = true;
         break;
      }
      else if( i == 1 )
      {
         Params.in_file_prefix = argv[1];
         if( Params.in_file_prefix.compare( 0, 1, "/" ) != 0 &&
             Params.in_file_prefix.compare( 0, 2, "./" ) != 0 )
             Params.in_file_prefix = std::string("./") + Params.in_file_prefix;
      }
      else if( strcmp( argv[i], "-o" ) == 0 )
      {
         if( i == argc - 1 )
         {
            std::cerr << ExeName << ": PREFIX expected -- -o" << std::endl;
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
#ifdef VT_UNIFY_HOOKS_PROF
      else if( strcmp( argv[i], "-f" ) == 0 )
      {
         if( i == argc - 1 )
         {
            std::cerr << ExeName << ": FILE expected -- -f" << std::endl;
            error = true;
         }
         else
         {
            Params.prof_out_file = argv[++i];
            if( Params.prof_out_file.compare( 0, 1, "/" ) != 0 &&
                Params.prof_out_file.compare( 0, 2, "./" ) != 0 )
               Params.prof_out_file = std::string("./") + Params.prof_out_file;
         }
      }
#endif // VT_UNIFY_HOOKS_PROF
#ifdef VT_UNIFY_HOOKS_MSGMATCH
      else if( strcmp( argv[i], "--nomsgmatch" ) == 0 )
      {
         Params.domsgmatch = false;
      }
      else if( strcmp( argv[i], "--droprecvs" ) == 0 )
      {
         Params.droprecvs = true;
      }
#endif // VT_UNIFY_HOOKS_MSGMATCH
#ifdef VT_UNIFY_HOOKS_THUMB
      else if( strcmp( argv[i], "--nothumb" ) == 0 )
      {
         Params.createthumb = false;
      }
#endif // VT_UNIFY_HOOKS_THUMB
#if defined(HAVE_ZLIB) && HAVE_ZLIB
      else if( strcmp( argv[i], "--nocompress" ) == 0 )
      {
         Params.docompress = false;
      }
#endif // HAVE_ZLIB
      else if( strcmp( argv[i], "-k" ) == 0
               || strcmp( argv[i], "--keeplocal" ) == 0 )
      {
         Params.doclean = false;
      }
      else if( strcmp( argv[i], "-p" ) == 0
               || strcmp( argv[i], "--progress" ) == 0 )
      {
         Params.showprogress = true;
         std::cerr << ExeName << ": Warning: Progress not yet implemented -- "
                   << argv[i] << std::endl;
      }
      else if( strcmp( argv[i], "-q" ) == 0 
               || strcmp( argv[i], "--quiet" ) == 0 )
      {
         Params.bequiet = true;
         Params.showprogress = false;
         Params.verbose_level = 0;
      }
      else if( strcmp( argv[i], "--stats" ) == 0 )
      {
         Params.onlystats = true;
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
   if( !error && !Params.showusage && !Params.showversion &&
       Params.in_file_prefix.length() == 0 )
   {
      Params.showusage = true;
   }

   return !error;
}

static bool
writeMasterControl()
{
   bool error = false;

   VPrint( 1, "Writing OTF master control\n" );

   // trigger phase pre hook
   theHooks->triggerPhaseHook( HooksC::Phase_WriteMasterControl_pre );

   const std::string tmp_out_file_prefix =
      Params.out_file_prefix + TmpFileSuffix;

   // open file manager
   //
   OTF_FileManager * manager = OTF_FileManager_open( 1 );
   assert( manager );

   // create master control
   //
   OTF_MasterControl * mc = OTF_MasterControl_new( manager );
   assert( mc );

   // add stream/process[group] mappings to master control
   //
   for( uint32_t i = 0; i < UnifyCtls.size() && !error; i++ )
   {
      // add only available streams/processes
      //
      if( UnifyCtls[i]->stream_avail )
      {
         const uint32_t & streamid = UnifyCtls[i]->streamid;

         // get additional process group tokens of stream
         const std::set<uint32_t> * procgrps =
            theDefinitions->groupCounters()->getGroupsOfStream( streamid );

         // add mappings
         //
         std::set<uint32_t>::const_iterator procgrp_it;
         if( procgrps ) procgrp_it = procgrps->begin();
         uint32_t proc_or_group = streamid;
         while( proc_or_group != 0 )
         {
            if( OTF_MasterControl_append( mc, streamid, proc_or_group ) == 0 )
            {
               std::cerr << ExeName << ": Error: "
                         << "Could not append mapping " << std::hex
                         << streamid << ":" << proc_or_group << std::dec
                         << " to OTF master control" << std::endl;
               error = true;
               break;
            }

            VPrint( 3, " Added mapping %x:%x to OTF master control\n",
                    streamid, proc_or_group );

            // get next process group token to add
            //
            if( procgrps && procgrp_it != procgrps->end() )
            {
               proc_or_group = *procgrp_it;
               ++procgrp_it;
            }
            else
            {
               proc_or_group = 0;
            }
         }
      }
   }

   // write master control
   //
   if( !error )
   {
      OTF_MasterControl_write( mc, tmp_out_file_prefix.c_str() );

      VPrint( 3, " Opened OTF master control [namestub %s]\n",
              tmp_out_file_prefix.c_str() );
   }

   // close file master control
   OTF_MasterControl_close( mc );
   // close file manager
   OTF_FileManager_close( manager );

   if( !error )
   {
      VPrint( 3, " Closed OTF master control [namestub %s]\n",
              tmp_out_file_prefix.c_str() );

      // trigger phase post hook
      theHooks->triggerPhaseHook( HooksC::Phase_WriteMasterControl_post );
   }

   return !error;
}

static bool
cleanUp()
{
   bool error = false;

#ifdef VT_MPI
   // block until all ranks have reached this point
   if( NumRanks > 1 )
      CALL_MPI( MPI_Barrier( MPI_COMM_WORLD ) );
#endif // VT_MPI

   VPrint( 1, "Cleaning up\n" );

   // trigger phase pre hook
   theHooks->triggerPhaseHook( HooksC::Phase_CleanUp_pre );

   do
   {
      // rename temporary definition output file
      if( (error = !theDefinitions->cleanUp()) )
         break;

      // rename temporary marker output file
      if( (error = !theMarkers->cleanUp()) )
         break;

      // rename temporary event output files
      if( !Params.onlystats && (error = !theEvents->cleanUp()) )
         break;

      // rename temporary statistic output files
      if( (error = !theStatistics->cleanUp()) )
         break;

      // remove unify control file and rename temporary OTF master control file
      //

      if( SyncError( &error ) )
         break;

      MASTER
      {
         char filename1[STRBUFSIZE];
         char filename2[STRBUFSIZE];

         std::string tmp_out_file_prefix =
            Params.out_file_prefix + TmpFileSuffix;

         // remove unify control file, if necessary
         //
         if( Params.doclean )
         {
            snprintf( filename1, sizeof( filename1 ) - 1, "%s.uctl",
                      Params.in_file_prefix.c_str() );

            if( remove( filename1 ) != 0 )
            {
               std::cerr << ExeName << ": Error Could not remove "
                         << filename1 << std::endl;
               error = true;
               break;
            }
            VPrint( 3, " Removed %s\n", filename1 );
         }

         // rename temporary OTF master control file
         //
         OTF_getFilename( tmp_out_file_prefix.c_str(), 0,
                          OTF_FILETYPE_MASTER, STRBUFSIZE, filename1 );
         OTF_getFilename( Params.out_file_prefix.c_str(), 0,
                          OTF_FILETYPE_MASTER, STRBUFSIZE, filename2 );
         if( rename( filename1, filename2 ) != 0 )
         {
            std::cerr << ExeName << ": Error: Could not rename "
                      << filename1 << " to " << filename2 << std::endl;
            error = true;
         }
         else
         {
            VPrint( 3, " Renamed %s to %s\n", filename1, filename2 );
         }
      }
      SyncError( &error );

   } while( false );

   // trigger phase post hook, if no error occurred
   if( !error )
      theHooks->triggerPhaseHook( HooksC::Phase_CleanUp_post );

   return !error;
}

static void
showUsage()
{
   std::cout << std::endl
      << " " << ExeName << " - local trace unifier for VampirTrace."  << std::endl
      << std::endl
      << " Syntax: " << ExeName << " <input trace prefix> [options]" << std::endl
      << std::endl
      << "   options:" << std::endl
      << "     -h, --help          Show this help message." << std::endl
      << std::endl
      << "     -V, --version       Show VampirTrace version." << std::endl
      << std::endl
      << "     -o PREFIX           Prefix of output trace filename." << std::endl
      << std::endl
#ifdef VT_UNIFY_HOOKS_PROF
      << "     -f FILE             Function profile output filename." << std::endl
      << "                         (default: PREFIX.prof.txt)" << std::endl
      << std::endl
#endif // VT_UNIFY_HOOKS_PROF
      << "     -k, --keeplocal     Don't remove input trace files." << std::endl
      << std::endl
      << "     -p, --progress      Show progress." << std::endl
      << std::endl
      << "     -v, --verbose       Increase output verbosity." << std::endl
      << "                         (can be used more than once)" << std::endl
      << std::endl
      << "     -q, --quiet         Enable quiet mode." << std::endl
      << "                         (only emergency output)" << std::endl
      << std::endl
      << "     --stats             Unify only summarized information (*.stats), no events" << std::endl
      << std::endl
#if defined(HAVE_ZLIB) && HAVE_ZLIB
      << "     --nocompress        Don't compress output trace files." << std::endl
      << std::endl
#endif // HAVE_ZLIB
#ifdef VT_UNIFY_HOOKS_MSGMATCH
      << "     --nomsgmatch        Don't match messages." << std::endl
      << std::endl
      << "     --droprecvs         Drop message receive events, if msg. matching" << std::endl
      << "                         is enabled." << std::endl
      << std::endl
#endif // VT_UNIFY_HOOKS_MSGMATCH
#ifdef VT_UNIFY_HOOKS_THUMB
      << "     --nothumb           Don't create Vampir thumbnail." << std::endl
      << std::endl
#endif // VT_UNIFY_HOOKS_THUMB
      ;
}

#ifdef VT_MPI

static bool
shareParams()
{
   bool error = false;

   assert( NumRanks > 1 );

   // block until all ranks have reached this point
   CALL_MPI( MPI_Barrier( MPI_COMM_WORLD ) );

   // create MPI datatype for ParamsS
   //

   char **filenames;
   char flags[9];
   VT_MPI_INT blockcounts[4] = { 3*1024, 1, 1, 9 };
   MPI_Aint displ[4];
   MPI_Datatype oldtypes[4] =
   { MPI_CHAR, MPI_UNSIGNED_SHORT, MPI_INT,
     MPI_CHAR };
   MPI_Datatype newtype;

   filenames = new char*[3];
   filenames[0] = new char[3*1024];
   filenames[1] = filenames[0] + ( 1024 * sizeof(char) );
   filenames[2] = filenames[1] + ( 1024 * sizeof(char) );

   CALL_MPI( MPI_Address( filenames[0], &displ[0] ) );
   CALL_MPI( MPI_Address( &(Params.verbose_level), &displ[1] ) );
   CALL_MPI( MPI_Address( &(Params.prof_sort_flags), &displ[2] ) );
   CALL_MPI( MPI_Address( &flags, &displ[3] ) );
   CALL_MPI( MPI_Type_struct( 4, blockcounts, displ, oldtypes, &newtype ) );
   CALL_MPI( MPI_Type_commit( &newtype ) );

   // fill elements of new MPI datatype
   //
   MASTER
   {
     strncpy( filenames[0], Params.in_file_prefix.c_str(), 1023 );
     filenames[0][1023] = '\0';
     strncpy( filenames[1], Params.out_file_prefix.c_str(), 1023 );
     filenames[1][1023] = '\0';
     strncpy( filenames[2], Params.prof_out_file.c_str(), 1023 );
     filenames[2][1023] = '\0';
     flags[0] = (char)Params.docompress;
     flags[1] = (char)Params.doclean;
     flags[2] = (char)Params.showusage;
     flags[3] = (char)Params.showversion;
     flags[4] = (char)Params.showprogress;
     flags[5] = (char)Params.bequiet;
     flags[6] = (char)Params.onlystats;
     flags[7] = (char)Params.domsgmatch;
     flags[8] = (char)Params.droprecvs;
   }

   // share unify parameters
   CALL_MPI( MPI_Bcast( MPI_BOTTOM, 1, newtype, 0, MPI_COMM_WORLD ) );

   // "receive" unify parameters
   //
   SLAVE
   {
      Params.in_file_prefix = filenames[0];
      Params.out_file_prefix = filenames[1];
      Params.prof_out_file = filenames[2];
      Params.docompress = (flags[0] == 1);
      Params.doclean = (flags[1] == 1);
      Params.showusage = (flags[2] == 1);
      Params.showversion = (flags[3] == 1);
      Params.showprogress = (flags[4] == 1);
      Params.bequiet = (flags[5] == 1);
      Params.onlystats = (flags[6] == 1);
      Params.domsgmatch = (flags[7] == 1);
      Params.droprecvs = (flags[8] == 1);
   }

   delete [] filenames[0];
   delete [] filenames;

   // free MPI datatype
   CALL_MPI( MPI_Type_free( &newtype ) );

   return !error;
}

static bool
shareUnifyControls()
{
   bool error = false;

   assert( NumRanks > 1 );

   // block until all ranks have reached this point
   CALL_MPI( MPI_Barrier( MPI_COMM_WORLD ) );

   VPrint( 2, " Sharing unify control data\n" );

   char * buffer;
   VT_MPI_INT buffer_size;
   VT_MPI_INT position;
   uint32_t unify_ctl_size;
   uint32_t i;

   MASTER unify_ctl_size = UnifyCtls.size();

#ifdef VT_ETIMESYNC
   // create MPI datatypes for ...
   //

   VT_MPI_INT blockcounts[3];
   MPI_Aint displ[3];
   MPI_Datatype oldtypes[3];

   // ... ETimeSyncC::SyncPhaseS
   //
   ETimeSyncC::SyncPhaseS sync_phase_struct;
   MPI_Datatype sync_phase_newtype;

   blockcounts[0] = blockcounts[1] = blockcounts[2] = 1;
   oldtypes[0] = MPI_UNSIGNED;
   oldtypes[1] = oldtypes[2] = MPI_LONG_LONG_INT;

   CALL_MPI( MPI_Address( &(sync_phase_struct.mapid), &displ[0] ) );
   CALL_MPI( MPI_Address( &(sync_phase_struct.time), &displ[1] ) );
   CALL_MPI( MPI_Address( &(sync_phase_struct.duration), &displ[2] ) );
   displ[1] -= displ[0]; displ[2] -= displ[0]; displ[0] = 0;

   CALL_MPI( MPI_Type_struct( 3, blockcounts, displ, oldtypes,
                              &sync_phase_newtype ) );
   CALL_MPI( MPI_Type_commit( &sync_phase_newtype ) );

   // ... ETimeSyncC::SyncTimeS
   //
   ETimeSyncC::SyncTimeS sync_time_struct;
   MPI_Datatype sync_time_newtype;

   blockcounts[0] = 4; blockcounts[1] = 1;
   oldtypes[0] = MPI_LONG_LONG_INT;
   oldtypes[1] = MPI_UNSIGNED;

   CALL_MPI( MPI_Address( &(sync_time_struct.t), &displ[0] ) );
   CALL_MPI( MPI_Address( &(sync_time_struct.phase_idx), &displ[1] ) );
   displ[1] -= displ[0]; displ[0] = 0;

   CALL_MPI( MPI_Type_struct( 2, blockcounts, displ, oldtypes,
                              &sync_time_newtype ) );
   CALL_MPI( MPI_Type_commit( &sync_time_newtype ) );
#endif // VT_ETIMESYNC

   // calculate buffer size
   //
   MASTER
   {
      VT_MPI_INT size;

      buffer_size = 0;

      // UnifyCtls.size()
      CALL_MPI( MPI_Pack_size( 1, MPI_UNSIGNED, MPI_COMM_WORLD, &size ) );
      buffer_size += size;

      for( i = 0; i < unify_ctl_size; i++ )
      {
         // streamid
         CALL_MPI( MPI_Pack_size( 1, MPI_UNSIGNED, MPI_COMM_WORLD, &size ) );
         buffer_size += size;

         // pstreamid
         CALL_MPI( MPI_Pack_size( 1, MPI_UNSIGNED, MPI_COMM_WORLD, &size ) );
         buffer_size += size;

         // stream_avail
         CALL_MPI( MPI_Pack_size( 1, MPI_CHAR, MPI_COMM_WORLD, &size ) );
         buffer_size += size;

         // ltime, offset
         CALL_MPI( MPI_Pack_size( 4, MPI_LONG_LONG_INT, MPI_COMM_WORLD,
                                  &size ) );
         buffer_size += size;

#ifdef VT_ETIMESYNC
         // sync_offset
         CALL_MPI( MPI_Pack_size( 1, MPI_LONG_LONG_INT, MPI_COMM_WORLD,
                                  &size ) );
         buffer_size += size;

         // sync_drift
         CALL_MPI( MPI_Pack_size( 1, MPI_DOUBLE, MPI_COMM_WORLD, &size ) );
         buffer_size += size;

         // sync_phases.size(), sync_times.size(),
         // sync_pairs.size()
         CALL_MPI( MPI_Pack_size( 3, MPI_UNSIGNED, MPI_COMM_WORLD, &size ) );
         buffer_size += size;

         // sync_phases
         if( UnifyCtls[i]->sync_phases.size() > 0 )
         {
            CALL_MPI( MPI_Pack_size(
                         (VT_MPI_INT)UnifyCtls[i]->sync_phases.size(),
                         sync_phase_newtype,
                         MPI_COMM_WORLD, &size ) );
            buffer_size += size;
         }

         // sync_times
         if( UnifyCtls[i]->sync_times.size() > 0 )
         {
            CALL_MPI( MPI_Pack_size(
                         (VT_MPI_INT)UnifyCtls[i]->sync_times.size(),
                         sync_time_newtype,
                         MPI_COMM_WORLD, &size ) );
            buffer_size += size;
         }

         // sync_pairs
         if( UnifyCtls[i]->sync_pairs.size() > 0 )
         {
            CALL_MPI(
               MPI_Pack_size(
                  (VT_MPI_INT)UnifyCtls[i]->sync_pairs.size() * 2,
                  MPI_UNSIGNED, MPI_COMM_WORLD,
                  &size ) );
            buffer_size += size;
         }
#endif // VT_ETIMESYNC
      }
   }

   // share buffer size
   CALL_MPI( MPI_Bcast( &buffer_size, 1, MPI_INT, 0, MPI_COMM_WORLD ) );

   // allocate buffer
   buffer = new char[buffer_size];

   // pack unify control data
   //
   MASTER
   {
      position = 0;

      // UnifyCtls.size()
      CALL_MPI( MPI_Pack( &unify_ctl_size, 1, MPI_UNSIGNED, buffer, buffer_size,
                          &position, MPI_COMM_WORLD ) );

      for( i = 0; i < unify_ctl_size; i++ )
      {
         // streamid
         CALL_MPI( MPI_Pack( &(UnifyCtls[i]->streamid), 1, MPI_UNSIGNED,
                             buffer, buffer_size, &position, MPI_COMM_WORLD ) );

         // pstreamid
         CALL_MPI( MPI_Pack( &(UnifyCtls[i]->pstreamid), 1, MPI_UNSIGNED,
                             buffer, buffer_size, &position, MPI_COMM_WORLD ) );

         // stream_avail
         CALL_MPI( MPI_Pack( &(UnifyCtls[i]->stream_avail), 1, MPI_CHAR,
                             buffer, buffer_size, &position, MPI_COMM_WORLD ) );
         // ltime
         CALL_MPI( MPI_Pack( UnifyCtls[i]->ltime, 2, MPI_LONG_LONG_INT,
                             buffer, buffer_size, &position, MPI_COMM_WORLD ) );

         // offset
         CALL_MPI( MPI_Pack( UnifyCtls[i]->offset, 2, MPI_LONG_LONG_INT,
                             buffer, buffer_size, &position, MPI_COMM_WORLD ) );

#ifdef VT_ETIMESYNC
         // sync_offset
         CALL_MPI( MPI_Pack( &(UnifyCtls[i]->sync_offset), 1,
                             MPI_LONG_LONG_INT, buffer, buffer_size, &position,
                             MPI_COMM_WORLD ) );

         // sync_drift
         CALL_MPI( MPI_Pack( &(UnifyCtls[i]->sync_drift), 1, MPI_DOUBLE,
                             buffer, buffer_size, &position, MPI_COMM_WORLD ) );

         // sync_phases.size()
         //
         uint32_t sync_phases_size =
                     UnifyCtls[i]->sync_phases.size();
         CALL_MPI( MPI_Pack( &sync_phases_size, 1, MPI_UNSIGNED, buffer,
                             buffer_size, &position, MPI_COMM_WORLD ) );

         // sync_phases
         //
         if( sync_phases_size > 0 )
         {
            uint32_t j;

            ETimeSyncC::SyncPhaseS * sync_phases =
               new ETimeSyncC::SyncPhaseS[sync_phases_size];
            for( j = 0; j < sync_phases_size; j++ )
               sync_phases[j] = UnifyCtls[i]->sync_phases[j];

            CALL_MPI( MPI_Pack( sync_phases, (VT_MPI_INT)sync_phases_size,
                                sync_phase_newtype, buffer, buffer_size,
                                &position, MPI_COMM_WORLD ) );

            delete [] sync_phases;
         }

         // sync_times.size()
         //
         uint32_t sync_times_size = UnifyCtls[i]->sync_times.size();
         CALL_MPI( MPI_Pack( &sync_times_size, 1, MPI_UNSIGNED, buffer,
                             buffer_size, &position, MPI_COMM_WORLD ) );

         // sync_times
         //
         if( sync_times_size > 0 )
         {
            uint32_t j;

            ETimeSyncC::SyncTimeS * sync_times =
               new ETimeSyncC::SyncTimeS[sync_times_size];
            for( j = 0; j < sync_times_size; j++ )
               sync_times[j] = UnifyCtls[i]->sync_times[j];

            CALL_MPI( MPI_Pack( sync_times, (VT_MPI_INT)sync_times_size,
                                sync_time_newtype, buffer, buffer_size,
                                &position, MPI_COMM_WORLD ) );

            delete [] sync_times;
         }

         // sync_pairs.size()
         //
         uint32_t sync_pairs_size = UnifyCtls[i]->sync_pairs.size();
         CALL_MPI( MPI_Pack( &sync_pairs_size, 1, MPI_UNSIGNED, buffer,
                             buffer_size, &position, MPI_COMM_WORLD ) );

         // sync_pairs
         //
         if( sync_pairs_size > 0 )
         {
            uint32_t * sync_pairs_firsts = new uint32_t[sync_pairs_size];
            uint32_t * sync_pairs_seconds = new uint32_t[sync_pairs_size];
            uint32_t j;

            for( j = 0; j < sync_pairs_size; j++ )
            {
               sync_pairs_firsts[j] = UnifyCtls[i]->sync_pairs[j].first;
               sync_pairs_seconds[j] = UnifyCtls[i]->sync_pairs[j].second;
            }

            CALL_MPI( MPI_Pack( sync_pairs_firsts, (VT_MPI_INT)sync_pairs_size,
                                MPI_UNSIGNED, buffer, buffer_size, &position,
                                MPI_COMM_WORLD ) );
            CALL_MPI( MPI_Pack( sync_pairs_seconds, (VT_MPI_INT)sync_pairs_size,
                                MPI_UNSIGNED, buffer, buffer_size, &position,
                                MPI_COMM_WORLD ) );

            delete [] sync_pairs_firsts;
            delete [] sync_pairs_seconds;
         }
#endif // VT_ETIMESYNC
      }
   }

   // share packed unify control data
   CALL_MPI( MPI_Bcast( buffer, buffer_size, MPI_PACKED, 0, MPI_COMM_WORLD ) );

   // unpack unify control data
   //
   SLAVE
   {
      position = 0;

      // UnifyCtls.size()
      //
      CALL_MPI( MPI_Unpack( buffer, buffer_size, &position, &unify_ctl_size, 1,
                            MPI_UNSIGNED, MPI_COMM_WORLD ) );
      UnifyCtls.resize( unify_ctl_size );

      for( i = 0; i < unify_ctl_size; i++ )
      {
         // create new unify control element
         UnifyCtls[i] = new UnifyControlS();

         // streamid
         CALL_MPI( MPI_Unpack( buffer, buffer_size, &position,
                               &(UnifyCtls[i]->streamid), 1, MPI_UNSIGNED,
                               MPI_COMM_WORLD ) );

         // pstreamid
         CALL_MPI( MPI_Unpack( buffer, buffer_size, &position,
                               &(UnifyCtls[i]->pstreamid), 1, MPI_UNSIGNED,
                               MPI_COMM_WORLD ) );

         // stream_avail
         CALL_MPI( MPI_Unpack( buffer, buffer_size, &position,
                               &(UnifyCtls[i]->stream_avail), 1, MPI_CHAR,
                               MPI_COMM_WORLD ) );

         // ltime
         CALL_MPI( MPI_Unpack( buffer, buffer_size, &position,
                               UnifyCtls[i]->ltime, 2, MPI_LONG_LONG_INT,
                               MPI_COMM_WORLD ) );

         // offset
         CALL_MPI( MPI_Unpack( buffer, buffer_size, &position,
                               UnifyCtls[i]->offset, 2, MPI_LONG_LONG_INT,
                               MPI_COMM_WORLD ) );

#ifdef VT_ETIMESYNC
         // sync_offset
         CALL_MPI( MPI_Unpack( buffer, buffer_size, &position,
                               &(UnifyCtls[i]->sync_offset), 1,
                               MPI_LONG_LONG_INT, MPI_COMM_WORLD ) );

         // sync_drift
         CALL_MPI( MPI_Unpack( buffer, buffer_size, &position,
                               &(UnifyCtls[i]->sync_drift), 1, MPI_DOUBLE,
                               MPI_COMM_WORLD ) );

         // sync_phases.size()
         //
         uint32_t sync_phases_size;
         CALL_MPI( MPI_Unpack( buffer, buffer_size, &position,
                               &sync_phases_size, 1, MPI_UNSIGNED,
                               MPI_COMM_WORLD ) );

         // sync_phases
         //
         if( sync_phases_size > 0 )
         {
            uint32_t j;

            theTimeSync->setSyncMethod( TimeSyncC::METHOD_ENHANCED );

            ETimeSyncC::SyncPhaseS * sync_phases =
               new ETimeSyncC::SyncPhaseS[sync_phases_size];

            CALL_MPI( MPI_Unpack( buffer, buffer_size, &position, sync_phases,
                                  (VT_MPI_INT)sync_phases_size,
                                  sync_phase_newtype, MPI_COMM_WORLD ) );

            for( j = 0; j < sync_phases_size; j++ )
               UnifyCtls[i]->sync_phases.push_back( sync_phases[j] );

            delete [] sync_phases;
         }

         // sync_times.size()
         //
         uint32_t sync_times_size;
         CALL_MPI( MPI_Unpack( buffer, buffer_size, &position, &sync_times_size,
                               1, MPI_UNSIGNED, MPI_COMM_WORLD ) );

         // sync_times
         //
         if( sync_times_size > 0 )
         {
            uint32_t j;

            ETimeSyncC::SyncTimeS * sync_times =
               new ETimeSyncC::SyncTimeS[sync_times_size];

            CALL_MPI( MPI_Unpack( buffer, buffer_size, &position, sync_times,
                                  (VT_MPI_INT)sync_times_size,
                                  sync_time_newtype, MPI_COMM_WORLD ) );

            for( j = 0; j < sync_times_size; j++ )
               UnifyCtls[i]->sync_times.push_back( sync_times[j] );

            delete [] sync_times;
         }

         // sync_pairs.size()
         //
         uint32_t sync_pairs_size;
         CALL_MPI( MPI_Unpack( buffer, buffer_size, &position, &sync_pairs_size,
                               1, MPI_UNSIGNED, MPI_COMM_WORLD ) );

         // sync_pairs
         //
         if( sync_pairs_size > 0 )
         {
            uint32_t * sync_pairs_firsts = new uint32_t[sync_pairs_size];
            uint32_t * sync_pairs_seconds = new uint32_t[sync_pairs_size];
            uint32_t j;

            CALL_MPI( MPI_Unpack( buffer, buffer_size, &position,
                                  sync_pairs_firsts,
                                  (VT_MPI_INT)sync_pairs_size, MPI_UNSIGNED,
                                  MPI_COMM_WORLD ) );
            CALL_MPI( MPI_Unpack( buffer, buffer_size, &position,
                                  sync_pairs_seconds,
                                  (VT_MPI_INT)sync_pairs_size, MPI_UNSIGNED,
                                  MPI_COMM_WORLD ) );

            for( j = 0; j < sync_pairs_size; j++ )
            {
               UnifyCtls[i]->sync_pairs.push_back(
                  std::make_pair( sync_pairs_firsts[j], sync_pairs_seconds[j] ) );
            }

            delete [] sync_pairs_firsts;
            delete [] sync_pairs_seconds;
         }
#endif // VT_ETIMESYNC
      }
   } // SLAVE

   delete [] buffer;

#ifdef VT_ETIMESYNC
   // free MPI datatypes
   CALL_MPI( MPI_Type_free( &sync_phase_newtype ) );
   CALL_MPI( MPI_Type_free( &sync_time_newtype ) );
#endif // VT_ETIMESYNC

   return !error;
}

#endif // VT_MPI

void
VPrint( uint8_t level, const char * fmt, ... )
{
   va_list ap;

   if( Params.verbose_level >= level )
   {
      MASTER
      {
#ifdef TIME_VERBOSE
         char tstamp[STRBUFSIZE];
         time_t t;
         time( &t );
         ctime_r( &t, tstamp );
         tstamp[strlen(tstamp)-1] = '\0';
         printf( "%s: ", tstamp );
#endif // TIME_VERBOSE

         va_start( ap, fmt );
         vprintf( fmt, ap );
         va_end( ap );
      } // MASTER
   }
}

void
PVPrint( uint8_t level, const char * fmt, ... )
{
   va_list ap;

   if( Params.verbose_level >= level )
   {
#ifdef TIME_VERBOSE
      char tstamp[STRBUFSIZE];
      time_t t;
      time( &t );
      ctime_r( &t, tstamp );
      tstamp[strlen(tstamp)-1] = '\0';
      printf( "%s: ", tstamp );
#endif // TIME_VERBOSE

      va_start( ap, fmt );
#if !(defined(VT_MPI) || (defined(HAVE_OMP) && HAVE_OMP))
      vprintf( fmt, ap );
#else // !(VT_MPI || HAVE_OMP)
      char msg[STRBUFSIZE] = "";
#  if defined(VT_MPI) && !(defined(HAVE_OMP) && HAVE_OMP)
      snprintf( msg, sizeof(msg)-1, "[%d] ", (int)MyRank );
#  elif !defined(VT_MPI) && (defined(HAVE_OMP) && HAVE_OMP)
      if( omp_in_parallel() )
         snprintf( msg, sizeof(msg)-1, "[%d] ", omp_get_thread_num() );
#  else // !VT_MPI && HAVE_OMP
      if( omp_in_parallel() )
      {
         snprintf( msg, sizeof(msg)-1, "[%d:%d] ", (int)MyRank,
                   omp_get_thread_num() );
      }
      else
      {
         snprintf( msg, sizeof(msg)-1, "[%d] ", (int)MyRank );
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

bool
SyncError( bool * error )
{
#if defined(VT_MPI) && defined(SYNC_ERROR)
   if( NumRanks > 1 )
   {
      VT_MPI_INT sendbuf = *error ? 1 : 0;
      VT_MPI_INT recvbuf;
      CALL_MPI( MPI_Allreduce( &sendbuf, &recvbuf, 1, MPI_INT, MPI_MAX,
                               MPI_COMM_WORLD ) );
      *error = ( recvbuf > 0 );
   }
#endif // VT_MPI && SYNC_ERROR

   return *error;
}
