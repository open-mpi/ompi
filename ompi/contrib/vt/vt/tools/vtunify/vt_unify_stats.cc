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

#include "vt_unify_stats.h"
#include "vt_unify.h"
#include "vt_unify_stats_hdlr.h"

#include "vt_inttypes.h"

#include "otf.h"

#include <iostream>
#include <string>
#include <vector>

#include <assert.h>
#include <stdio.h>

Statistics * theStatistics; // instance of class Statistics

//////////////////// class Statistics ////////////////////

// public methods
//

Statistics::Statistics() : m_lTimerRes(1), m_iFuncStatSortFlags(0x22)
{
   // Empty
}

Statistics::~Statistics()
{
   // Empty
}

bool
Statistics::run()
{
   VPrint( 1, "Unifying statistics\n" );

   bool error = false;

   std::string tmp_out_file_prefix =
      Params.out_file_prefix + TmpFileSuffix;

   for( uint32_t i = 0; i < g_vecUnifyCtls.size(); i++ )
   {
      // open file manager for reader stream
      OTF_FileManager * p_org_stats_manager =
	 OTF_FileManager_open( 1 );
      assert( p_org_stats_manager );
      
      // open stream for reading
      OTF_RStream * p_org_stats_rstream =
	 OTF_RStream_open( Params.in_file_prefix.c_str(),
			   g_vecUnifyCtls[i]->streamid,
			   p_org_stats_manager );
      assert( p_org_stats_rstream );

      VPrint( 2, " Opened OTF reader stream [namestub %s id %x]\n",
	      Params.in_file_prefix.c_str(),
	      g_vecUnifyCtls[i]->streamid );

      if( !OTF_RStream_getStatsBuffer( p_org_stats_rstream ) )
      {
	 VPrint( 2, "  No statistics found in this OTF reader stream "
		    "- Ignored\n" );
      }
      else
      {
	 // close statistics buffer
	 OTF_RStream_closeStatsBuffer( p_org_stats_rstream );

	 // open file manager for writer stream
	 OTF_FileManager * p_uni_stats_manager =
	    OTF_FileManager_open( 1 );
	 assert( p_uni_stats_manager );

	 // open stream for writing
	 OTF_WStream * p_uni_stats_wstream =
	    OTF_WStream_open( tmp_out_file_prefix.c_str(),
			      g_vecUnifyCtls[i]->streamid,
			      p_uni_stats_manager );
	 assert( p_uni_stats_wstream );

	 VPrint( 2, " Opened OTF writer stream [namestub %s id %x]\n",
		 tmp_out_file_prefix.c_str(),
		 g_vecUnifyCtls[i]->streamid );

	 // create record handler
	 OTF_HandlerArray * p_handler_array =
	    OTF_HandlerArray_open();
	 assert( p_handler_array );

	 // set record handler and first handler argument for ...
	 //

	 // ... OTF_FUNCTIONSUMMARY_RECORD
	 OTF_HandlerArray_setHandler( p_handler_array,
            (OTF_FunctionPointer*)Handle_FunctionSummary,
				   OTF_FUNCTIONSUMMARY_RECORD );
	 OTF_HandlerArray_setFirstHandlerArg( p_handler_array,
	    p_uni_stats_wstream, OTF_FUNCTIONSUMMARY_RECORD );

	 // ... OTF_MESSAGESUMMARY_RECORD
	 OTF_HandlerArray_setHandler( p_handler_array,
            (OTF_FunctionPointer*)Handle_MessageSummary,
				   OTF_MESSAGESUMMARY_RECORD );
	 OTF_HandlerArray_setFirstHandlerArg( p_handler_array,
	    p_uni_stats_wstream, OTF_MESSAGESUMMARY_RECORD );

	 // ... OTF_COLLOPSUMMARY_RECORD
	 OTF_HandlerArray_setHandler( p_handler_array,
	    (OTF_FunctionPointer*)Handle_CollopSummary,
				   OTF_COLLOPSUMMARY_RECORD );
	 OTF_HandlerArray_setFirstHandlerArg( p_handler_array,
	    p_uni_stats_wstream, OTF_COLLOPSUMMARY_RECORD );

	 // ... OTF_FILEOPERATIONSUMMARY_RECORD
	 OTF_HandlerArray_setHandler( p_handler_array,
            (OTF_FunctionPointer*)Handle_FileOperationSummary,
				   OTF_FILEOPERATIONSUMMARY_RECORD );
	 OTF_HandlerArray_setFirstHandlerArg( p_handler_array,
	    p_uni_stats_wstream, OTF_FILEOPERATIONSUMMARY_RECORD );


	 // set file compression
	 if( Params.docompress )
	 {
	    OTF_WStream_setCompression( p_uni_stats_wstream,
					OTF_FILECOMPRESSION_COMPRESSED );
	 }

	 // read statistics
	 if( OTF_RStream_readStatistics( p_org_stats_rstream, p_handler_array )
	     == OTF_READ_ERROR )
	 {
	    std::cerr << ExeName << ": Error: "
		      << "Could not read statistics of OTF stream [namestub "
		      << Params.in_file_prefix << " id "
		      << std::hex << g_vecUnifyCtls[i]->streamid << "]"
		      << std::dec << std::endl;
	    error = true;
	 }

	 // close record handler
	 OTF_HandlerArray_close( p_handler_array );
	 // close writer stream
	 OTF_WStream_close( p_uni_stats_wstream );
	 // close file manager for writer stream
	 OTF_FileManager_close( p_uni_stats_manager );
	 
	 VPrint( 2, " Closed OTF writer stream [namestub %s id %x]\n",
		 tmp_out_file_prefix.c_str(),
		 g_vecUnifyCtls[i]->streamid );
      }

      // close reader stream
      OTF_RStream_close( p_org_stats_rstream );
      // close file manager for reader stream
      OTF_FileManager_close( p_org_stats_manager );

      VPrint( 2, " Closed OTF reader stream [namestub %s id %x]\n",
	      Params.in_file_prefix.c_str(),
	      g_vecUnifyCtls[i]->streamid );

      if( error ) break;
   }

   if( error )
   {
      std::cerr << ExeName << ": "
		<< "An error occurred during unifying statistics. Aborting"
		<< std::endl;
   }

   return !error;
}

bool
Statistics::addFunc( uint32_t funcId, std::string funcName )
{
   std::map<uint32_t, std::string>::iterator it =
      m_mapFuncIdName.find( funcId );

   if( it == m_mapFuncIdName.end() )
      m_mapFuncIdName.insert( std::make_pair( funcId, funcName ) );

   return true;
}

bool
Statistics::addFuncStat( uint32_t procId, uint32_t funcId, uint64_t count,
			 uint64_t incl, uint64_t excl )
{

   // search function statistics map by process id
   //
   std::map<uint32_t, std::map<uint32_t, struct FuncStat_struct*>*>
      ::iterator proc_it = m_mapProcIdFuncStat.find( procId );
   // found ?
   if( proc_it == m_mapProcIdFuncStat.end() )
   {
      // no -> create function statistics map
      //
      std::map<uint32_t, struct FuncStat_struct*>* p_map_funcid_stat =
	 new std::map<uint32_t, struct FuncStat_struct*>();
      
      m_mapProcIdFuncStat.insert( std::make_pair( procId,
						  p_map_funcid_stat ) );
      proc_it = m_mapProcIdFuncStat.find( procId );
      assert( proc_it != m_mapProcIdFuncStat.end() );
   }

   // search function statistics by function id
   //
   std::map<uint32_t, struct FuncStat_struct*>::iterator func_it =
      proc_it->second->find( funcId );
   // found ?
   if( func_it == proc_it->second->end() )
   {
      // no -> create function statistics
      //
      std::string func_name = getFuncNameById(funcId);
      assert( func_name != "" );
      struct FuncStat_struct* p_func_stat =
	 new struct FuncStat_struct( funcId, func_name );

      proc_it->second->insert( std::make_pair( funcId, p_func_stat ) );
      func_it = proc_it->second->find( funcId );
      assert( func_it != proc_it->second->end() );
   }

   // overwrite function statistics values
   //
   func_it->second->count = (double)count;
   func_it->second->incl = incl;
   func_it->second->excl = excl;

   return true;
}

bool
Statistics::isFuncStatAvail()
{
   // get vector of function statistics
   std::vector<struct FuncStat_struct> vec_func_stat = 
      getFuncStat();

   return vec_func_stat.size() > 0;
}

bool
Statistics::isFuncStatAvail( uint32_t procId )
{
   // get vector of function statistics by process id
   std::vector<struct FuncStat_struct> vec_func_stat = 
      getFuncStat( procId );

   return vec_func_stat.size() > 0;
}

bool
Statistics::printFuncStat( std::string outFile, int sortFlags )
{
   // get vector of function statistics
   std::vector<struct FuncStat_struct> vec_func_stat = 
      getFuncStat();

   return printFuncStat( outFile, vec_func_stat, sortFlags );
}

bool
Statistics::printFuncStat( std::string outFile,
			   uint32_t procId, int sortFlags )
{
   // get vector of function statistics by process id
   std::vector<struct FuncStat_struct> vec_func_stat = 
      getFuncStat( procId );

   return printFuncStat( outFile, vec_func_stat, sortFlags );
}

bool
Statistics::printFuncStat( std::string outFile,
			   std::vector<struct FuncStat_struct> & vecFuncStat,
			   int sortFlags )
{
   const uint32_t max_lines_on_stdout = 10;

   FILE * out;

   // open statistics output file, if given
   //
   if( outFile.length() != 0 )
   {
      if( !( out = fopen( outFile.c_str(), "w" ) ) )
      {
	 std::cerr << ExeName << ": Error: "
		   << "Could not open file " << outFile << std::endl;
	 return false;
      }
   }
   // otherwise, print on stdout
   else
   {
      out = stdout;
   }

   // sort function statistics vector
   //
   if( sortFlags != 0 )
   {   
      int sort_flags_sav = m_iFuncStatSortFlags;
      m_iFuncStatSortFlags = sortFlags;
      std::sort( vecFuncStat.begin(), vecFuncStat.end(), 
		 std::less<struct FuncStat_struct>() );
      m_iFuncStatSortFlags = sort_flags_sav;
   }
   else
   {
      sortFlags = m_iFuncStatSortFlags;
      std::sort( vecFuncStat.begin(), vecFuncStat.end(), 
		 std::less<struct FuncStat_struct>() );
   }

   // print out function statistics
   //
   fprintf( out, "                                   %cexcl. time %cincl. time\n",
	    (sortFlags & STAT_SORT_FLAG_EXCL_CALL) ? '*' : ' ',
	    (sortFlags & STAT_SORT_FLAG_INCL_CALL) ? '*' : ' ' );

   fprintf( out, "%cexcl. time %cincl. time      calls      / call      / call %cname\n",
	    (sortFlags & STAT_SORT_FLAG_EXCL) ? '*' : ' ',
	    (sortFlags & STAT_SORT_FLAG_INCL) ? '*' : ' ',
	    (sortFlags & STAT_SORT_FLAG_FUNCNAME) ? '*' : ' ' );

   // reduce output lines, if necessary
   uint32_t size = vecFuncStat.size();
   if( out == stdout && size > max_lines_on_stdout )
      size = max_lines_on_stdout;
      
   for( uint32_t i = 0; i < size; i++ )
   {
      std::string str_excl = formatTime( vecFuncStat[i].excl );
      std::string str_incl = formatTime( vecFuncStat[i].incl );
      std::string str_excl_call =
	 formatTime( (uint64_t)((double)vecFuncStat[i].excl / vecFuncStat[i].count) );
      std::string str_incl_call =
	 formatTime( (uint64_t)((double)vecFuncStat[i].incl / vecFuncStat[i].count) );
      std::string str_funcname = vecFuncStat[i].funcname;

      if( out == stdout ) str_funcname = shortName( vecFuncStat[i].funcname ); 

      fprintf( out,
	       "%11s %11s %10.*f %11s %11s  %s\n",
	       str_excl.c_str(),
	       str_incl.c_str(),
	       ((double)((uint64_t)vecFuncStat[i].count) ==
		vecFuncStat[i].count) ? 0 : 2,
	       vecFuncStat[i].count,
	       str_excl_call.c_str(),
	       str_incl_call.c_str(),
	       str_funcname.c_str() );
   }

   if( out == stdout && size < vecFuncStat.size() )
   {
      fprintf( out, "Displayed %u from %u functions.\n",
	       size, (uint32_t)vecFuncStat.size() );
   }

   // close statistics output file, if necessary
   if( out != stdout ) fclose( out );

   return true;
}

// private methods
//

std::vector<struct Statistics::FuncStat_struct>
Statistics::getFuncStat()
{
   std::vector<struct FuncStat_struct> vec_sum_func_stat;

   std::map<uint32_t, std::map<uint32_t, struct FuncStat_struct*>*>::iterator
      proc_it;

   for( proc_it = m_mapProcIdFuncStat.begin();
	proc_it != m_mapProcIdFuncStat.end(); proc_it++ )
   {
      std::vector<struct FuncStat_struct> vec_func_stat =
	 getFuncStat( proc_it->first );

      for( uint32_t i = 0; i < vec_func_stat.size(); i++ )
      {
	 std::vector<struct FuncStat_struct>::iterator func_it =
	    std::find_if( vec_sum_func_stat.begin(), vec_sum_func_stat.end(),
			  FuncStat_funcId_eq( vec_func_stat[i].funcid ) );
	 if( func_it == vec_sum_func_stat.end() )
	 {
	    struct FuncStat_struct func_stat(vec_func_stat[i].funcid,
					     vec_func_stat[i].funcname,
					     vec_func_stat[i].count,
					     vec_func_stat[i].incl,
					     vec_func_stat[i].excl);
	    vec_sum_func_stat.push_back( func_stat );
	 }
	 else
	 {
	    func_it->count += vec_func_stat[i].count;
	    func_it->incl += vec_func_stat[i].incl;
	    func_it->excl += vec_func_stat[i].excl;
	 }
      }
   }

   if( m_mapProcIdFuncStat.size() > 1 )
   {
      uint32_t nprocs = m_mapProcIdFuncStat.size();
      for( uint32_t i = 0; i < vec_sum_func_stat.size(); i++ )
      {
	 vec_sum_func_stat[i].count /= (double)nprocs;
	 vec_sum_func_stat[i].incl /= nprocs;
	 vec_sum_func_stat[i].excl /= nprocs;
      }
   }

   return vec_sum_func_stat;
}

std::vector<struct Statistics::FuncStat_struct>
Statistics::getFuncStat( uint32_t procId )
{
   std::vector<struct FuncStat_struct> vec_func_stat;

   // search function statistics map by process id
   //
   std::map<uint32_t, std::map<uint32_t, struct FuncStat_struct*>*>
      ::iterator proc_it = m_mapProcIdFuncStat.find( procId );
   assert( proc_it != m_mapProcIdFuncStat.end() );

   for( std::map<uint32_t, struct FuncStat_struct*>::iterator it =
	   proc_it->second->begin(); it != proc_it->second->end(); it++ )
   {
      vec_func_stat.push_back(*(it->second)); 
   }

   return vec_func_stat;
}

std::string
Statistics::getFuncNameById( uint32_t funcId )
{
   // search function name by function id
   //
   std::map<uint32_t, std::string>::iterator it =
      m_mapFuncIdName.find( funcId );
   if( it != m_mapFuncIdName.end() )
      return it->second;       // return function name, if found
   else
      return std::string("");  // otherwise return ""
}

std::string
Statistics::shortName( const std::string & longName, uint32_t len )
{
   assert( len >= 5 );

   std::string short_name;

   if( longName.length() <= len ) 
   {
      short_name = longName;
   }
   else
   {
      std::string f, b;
	 
      f = longName.substr( 0, (len-3) / 2 ) + "...";
      b = longName.substr( longName.length()-(len-f.length()));
      short_name = f+b;
   }

   return short_name;
}

std::string
Statistics::formatTime( uint64_t time )
{
   char str[20];
   double d_time = (double)time;
   double d_res = (double)m_lTimerRes;
   double sec = d_time / d_res;

   static const char unit[4][3] = { "s ", "ms", "us", "ns" };

   for( uint32_t i = 0; i < 4; i++ )
   {
      if( i == 3 || sec >= 0.1 )
      {
	 snprintf( str, sizeof( str ) - 1, "%.3f%s", sec, unit[i] );
	 break;
      }
      sec *= 1000.0;
   }

   return std::string( str );
}
