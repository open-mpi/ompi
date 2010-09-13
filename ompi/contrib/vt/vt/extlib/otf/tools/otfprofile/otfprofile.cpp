/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2010.
 Authors: Andreas Knuepfer, Denis Huenich, Johannes Spazier
*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef _OPENMP
#	include <omp.h>
#	define MY_THREAD  omp_get_thread_num()
#	define THREAD_NUM omp_get_num_threads()
#else
#	define MY_THREAD  0
#	define THREAD_NUM 1
#endif

#if defined(HAVE_SYS_TIME_H) && HAVE_SYS_TIME_H
#	include <sys/time.h>
#endif
#ifdef _WIN32
#	include <Winsock2.h>
#endif
#include <cassert>

#include "OTF_inttypes.h"
#include "OTF_Platform.h"

#include "Handler.h"
#include "Summary.h"
#include "Prodtex.h"
#include "CSVParse.h"

using namespace std;

#define HELPTEXT "" \
"                                                                  \n" \
" otfprofile -  generate a profile of a trace                      \n" \
"               in Latex or CSV format.                            \n" \
"                                                                  \n" \
"   options:                                                       \n" \
"      -h, --help    show this help message                        \n" \
"      -b <x>        readbuffer size                               \n" \
"      -f <x>        max. number of filehandles to use             \n" \
"      -i <file>     specify an input trace name                   \n" \
"      -csv <file>   specify an input csv-file trace name          \n" \
"                    (as produced by otfprofiler before),          \n" \
"                    don't use -i and -csv together                \n" \
"      -o <path>     specify the path for the output files         \n" \
"      -tex <x>      writes Latex output in different flavours:    \n" \
"                    (all,func,p2p,collop,none)                    \n" \
"      -notex        disable Latex output                          \n" \
"      -nops         disable Postscript output			   \n" \
"      -var          also show statistic variance                  \n" \
"      -top <x>      max. number of functions shown (default 50)   \n" \
"      -progress     show progress information                     \n" \
"      -sum          reads only summarized information, no events  \n" \
"      -omp <x>      specify the number of threads which are used  \n" \
"                    while reading the otf-file parallel	   \n" \
"                    Note: This option overrides the environment   \n" \
"                    variable OMP_NUM_THREADS,    		   \n" \
"                    only useful if compiled with OpenMP support   \n" \
"                                                                  \n" \

int main( int argc, const char** argv ) 
{
	struct timeval tStart,tEnd;
	
	int i;
	OTF_FileManager* manager;
	OTF_Reader* reader;
	OTF_HandlerArray* handlers;
	int buffersize = 1024*1024;
	int nfiles = 300;
	string output_path("./");
	string output_dir;
	char* file = NULL;
	fstream out;
	bool check = false; // if check == true then -i or -csv is set
	bool csv_on = false;
	vector<string> counter_names;
	global_data* gd = new global_data;
	bool sum = false;  // if true -> read summary

	/* show how many events were read */
	uint64_t global_events = 0;
	double global_percent = 0;

	/* what to read? */
	int  tex = TEX_ALL;
	bool status = false;
	bool file_rename = false;
	bool ps = true;
	gd->TOP_FUNC = 50;
	gd->var = false;
	gd->vis = false;
	gd->prog = false;
	gd->prog_start = (uint64_t) - 1;
	gd->prog_end = 0;
	gd->min_time = 0;
	gd->max_time = (uint64_t) - 1;
	gd->ticks = 1;
	gd->clear_temp = false;
	uint64_t ret_read;

	int num_p = 1;
	global_data** data_array;
	uint32_t* cpu2thread;

	char *OMP_NUM_THREADS = getenv("OMP_NUM_THREADS");
	if(OMP_NUM_THREADS != NULL) {	
		num_p = atoi(OMP_NUM_THREADS);
		if(num_p == 0) {
			num_p = 1;
		}
	}
	
	if ( 1 >= argc ) {

		cout << HELPTEXT;
		return 1;
	}

	for(i = 1; i < argc; i++) 
	{
		if ((0 == strcmp("-b", argv[i])) && (i+1 < argc))
		{
			buffersize = atoi(argv[i+1]);
			++i;
		} 
		else if ((0 == strcmp("-tex", argv[i])) && (i+1 < argc))
		{
			if (0 == strcmp("all", argv[i+1]))
				tex = TEX_ALL;
			else if (0 == strcmp("allplot", argv[i+1]))
				tex = TEX_ALLPLOT;
			else if (0 == strcmp("func", argv[i+1]))
				tex = TEX_FUNC;
			else if (0 == strcmp("p2p", argv[i+1]))
				tex = TEX_P2P;
			else if (0 == strcmp("collop", argv[i+1]))
				tex = TEX_COLLOP;
			else if (0 == strcmp("none", argv[i+1]))
				tex = TEX_OFF;
			else
				cerr << "\nWrong argument given after -tex." << endl;
			++i;
		}
		else if ( 0 == strcmp("-notex", argv[i]) ) 
		{
			tex= TEX_OFF;
		}
		else if ((0 == strcmp("-f", argv[i])) && (i+1 < argc)) 
		{
			nfiles = atoi(argv[i+1]);
			++i;
		}
#if 0
		else if (0 == strcmp("-c", argv[i]) && (i+1 < argc)) 
		{
			i++;
			int test_end;
			while((0 != (test_end = strcmp("end", argv[i]))) && (i+1 < argc))
			{
				string s(strdup(argv[i]));
				transform(s.begin(), s.end(), s.begin(), ::toupper);
				counter_names.push_back(s);
				i++;
			}
			if(test_end != 0)
			{
				cerr << "Parameter -c : There was missing an \"end\"" << endl;
				exit(1);
			}
		}
#endif /* 0 */
		else if ((0 == strcmp("-min", argv[i])) && (i+1 < argc)) 
		{
			gd->min_time = atoi(argv[i+1]);
			++i;
		}
		else if ((0 == strcmp("-max", argv[i])) && (i+1 < argc)) 
		{
			gd->max_time = atoi(argv[i+1]);
			++i;
		}
		else if (0 == strcmp("-progress", argv[i])) 
		{
			status = true;
		}
		else if (0 == strcmp("--help", argv[i]) || 0 == strcmp("-h", argv[i])) 
		{
			cout << HELPTEXT;
			return 0;
		} 
		else if ((0 == strcmp("-i", argv[i])) && (i+1 < argc))
		{
			if(!check)
			{
				file = strdup(argv[i+1]);
				check = true;
			}
			++i;
		}
		else if ((0 == strcmp("-csv", argv[i])) && (i+1 < argc))
		{
			if(!check)
			{
				file = strdup(argv[i+1]);
				check = true;
				csv_on = true;
			}
			++i;
		}
		else if ((0 == strcmp("-o", argv[i])) && (i+1 < argc))
		{
			output_path = string(strdup(argv[i+1]));
			++i;
		} 
		else if ((0 == strcmp("-d", argv[i])) && (i+1 < argc))
		{
			output_path = string(strdup(argv[i+1]));
			file_rename = true;
			++i;
		}
		else if (0 == strcmp("-var", argv[i]))
		{
			gd->var = true;
		}
		else if (0 == strcmp("-vis", argv[i]))
		{
			gd->vis = true;
		}
		else if ((0 == strcmp("-top", argv[i])) && (i+1 < argc)) 
		{
			gd->TOP_FUNC = atoi(argv[i+1]);
			++i;
		}
		else if (0 == strcmp("-sum", argv[i]))
		{
			sum = true;
		}
		else if (0 == strcmp("-nops", argv[i]))
		{
			ps = false;
		}
		else if (0 == strcmp("-omp", argv[i]) && (i+1 < argc))
		{
#			ifdef _OPENMP
			num_p = atoi(argv[i+1]);
			++i;
#			else
			cerr << "\nThe option \"-omp\" has no effect because you compiled without OpenMP-Support.\nInstall OpenMP and recompile otfprofile to use this option.\n" << endl;
			++i;
#			endif
		}
		else 
		{
			cerr << "ERROR: Unknown argument: " <<  argv[i] << endl;;
			exit(1);
		}
	}
	
	if (nfiles < 1) 
	{
		cerr << "ERROR: need at least one input, aborting" << endl;
		exit(1);
	}
	
	if(file == NULL)
	{
		cerr << "ERROR: no file given" << endl;
		cerr << "To give a file, use \"-i filename\". " << endl;
		exit(1);
	}

#	ifdef _OPENMP
		uint32_t num_threads = num_p;
#	else
		uint32_t num_threads = 1;
#	endif
	
	gd->filename.assign(OTF_basename(file));
	output_dir = output_path;
	if(!file_rename)
	{
		output_path.append(gd->filename);
	}
	gd->filename_path = output_path;
	
	if(csv_on)
	{
		Glob_Maps glob_maps;
		parse_csv(gd->sum_container, file, glob_maps);
		free(file);
	}
	else
	{	
		manager= OTF_FileManager_open(nfiles);
		assert(NULL != manager);

		handlers = OTF_HandlerArray_open();

		reader = OTF_Reader_open(file, manager);
		assert(NULL != reader);
		
		OTF_MasterControl* master = OTF_Reader_getMasterControl(reader);
		assert(NULL != master);
		uint32_t num_cpus = OTF_MasterControl_getrCount(master);
		uint32_t num_files = OTF_MasterControl_getCount(master);
		gd->num_cpu = num_cpus;

		if( (num_threads > num_files) && (num_threads <= num_cpus) ) {
			cerr << "Warning: Number of working threads greater than number of files. That could reduce the performance." << endl;
		}

		if(num_threads > num_cpus) {
			num_threads = num_cpus;
			cerr << "Remark: It is not possible to have more working threads than processes in the otf-file!" << endl;
		}

		uint32_t current_reader = num_threads;
#		ifdef _OPENMP
			omp_set_num_threads(num_threads);
#		endif

        	/* get the list of processes that should be distributed over the analysis threads */

		uint32_t* processlist;
		uint32_t pos= 0;
		processlist= (uint32_t*) malloc( num_cpus * sizeof(uint32_t) );
		assert( NULL != processlist );

		for( uint32_t k= 0; k < num_files; k++ ) {

		    OTF_MapEntry* entry;
		    entry= OTF_MasterControl_getEntryByIndex( master, k );
		    assert( NULL != entry );

		    for( uint32_t m= 0; m < entry->n ; m++ ) {

			processlist[pos]= entry->values[m];
			++pos;
		    }
		}

		data_array = (global_data**) malloc(num_threads * sizeof(global_data*));
		cpu2thread = (uint32_t*) malloc(num_cpus * sizeof(uint32_t));

		/* Definitons */		
		OTF_Reader_setBufferSizes(reader, buffersize);
		
		OTF_HandlerArray_setHandler(handlers, 
			(OTF_FunctionPointer*) handleDefCreator, OTF_DEFCREATOR_RECORD);
		OTF_HandlerArray_setFirstHandlerArg(handlers, 
        	(void*) gd, OTF_DEFCREATOR_RECORD);

		OTF_HandlerArray_setHandler(handlers, 
			(OTF_FunctionPointer*) handleDefVersion, OTF_DEFVERSION_RECORD);
		OTF_HandlerArray_setFirstHandlerArg(handlers, 
        	(void*) gd, OTF_DEFVERSION_RECORD);

		OTF_HandlerArray_setHandler(handlers, 
			(OTF_FunctionPointer*) handleDefTimerResolution, OTF_DEFTIMERRESOLUTION_RECORD);
		OTF_HandlerArray_setFirstHandlerArg(handlers, 
        	(void*) gd, OTF_DEFTIMERRESOLUTION_RECORD);
        
		OTF_HandlerArray_setHandler(handlers, 
			(OTF_FunctionPointer*) handleDefFunction, OTF_DEFFUNCTION_RECORD);
		OTF_HandlerArray_setFirstHandlerArg(handlers, 
        	(void*) gd, OTF_DEFFUNCTION_RECORD);

		OTF_HandlerArray_setHandler(handlers, 
			(OTF_FunctionPointer*) handleDefFunctionGroup, OTF_DEFFUNCTIONGROUP_RECORD);
		OTF_HandlerArray_setFirstHandlerArg(handlers, 
        	(void*) gd, OTF_DEFFUNCTIONGROUP_RECORD);
		
		OTF_HandlerArray_setHandler(handlers, 
			(OTF_FunctionPointer*) handleDefProcess, OTF_DEFPROCESS_RECORD);
		OTF_HandlerArray_setFirstHandlerArg(handlers, 
        	(void*) gd, OTF_DEFPROCESS_RECORD);

		OTF_HandlerArray_setHandler(handlers, 
			(OTF_FunctionPointer*) handleDefProcessGroup, OTF_DEFPROCESSGROUP_RECORD);
		OTF_HandlerArray_setFirstHandlerArg(handlers, 
        	(void*) gd, OTF_DEFPROCESSGROUP_RECORD);
		
		OTF_HandlerArray_setHandler(handlers, 
			(OTF_FunctionPointer*) handleDefCollectiveOperation, OTF_DEFCOLLOP_RECORD);
		OTF_HandlerArray_setFirstHandlerArg(handlers, 
        	(void*) gd, OTF_DEFCOLLOP_RECORD);
        
   		OTF_HandlerArray_setHandler(handlers, 
			(OTF_FunctionPointer*) handleDefCounter, OTF_DEFCOUNTER_RECORD);
		OTF_HandlerArray_setFirstHandlerArg(handlers, 
      		(void*) gd, OTF_DEFCOUNTER_RECORD);

		OTF_HandlerArray_setHandler(handlers,
			(OTF_FunctionPointer*) handleFunctionSummary, OTF_FUNCTIONSUMMARY_RECORD);
		OTF_HandlerArray_setFirstHandlerArg(handlers,
   	   	(void*) gd, OTF_FUNCTIONSUMMARY_RECORD);

		OTF_HandlerArray_setHandler(handlers,
			(OTF_FunctionPointer*) handleMessageSummary, OTF_MESSAGESUMMARY_RECORD);
		OTF_HandlerArray_setFirstHandlerArg(handlers,
   	   	(void*) gd, OTF_MESSAGESUMMARY_RECORD);

		OTF_HandlerArray_setHandler(handlers,
			(OTF_FunctionPointer*) handleCollopSummary, OTF_COLLOPSUMMARY_RECORD);
		OTF_HandlerArray_setFirstHandlerArg(handlers,
   	   	(void*) gd, OTF_COLLOPSUMMARY_RECORD);

		/* read definitions */
		ret_read = OTF_Reader_readDefinitions(reader, handlers);
		if( ret_read == OTF_READ_ERROR ) {
			cerr << "Cannot read definition file. It seems to be damaged. Abort." << endl;
			return 0;
		}

		gd->sum_container.adddef_Bin(1);

		/* read statistics */
		if(sum) {
			ret_read = OTF_Reader_readStatistics(reader, handlers);
			if( ret_read == OTF_READ_ERROR ) {
				cerr << "Cannot read statistics. The tracefile seems to be damaged. Abort." << endl;
				return 0;
			}
		}

		OTF_Reader_close(reader);
		OTF_HandlerArray_close(handlers);

		OTF_FileManager_close(manager);

	if(!sum) {

		global_data data = *gd;
		
		/* CPU-Verteilung ermitteln */
		cout << "Threads: " << num_threads << endl;
		uint32_t *threads = (uint32_t*) malloc(num_threads * sizeof(uint32_t));
		
		int index = 0;
		for(uint32_t k=0; k<num_threads; k++) {
			threads[k] = ( num_cpus / num_threads ) +
				( (uint32_t)k < (num_cpus % (uint32_t)num_threads) ? 1 : 0 );
			for(uint32_t i=0; i<threads[k]; i++) {
				cpu2thread[index] = k;
				index++;
			}
		}

#		ifdef _OPENMP
#			pragma omp parallel firstprivate(data) private(manager, handlers, reader)
#		endif
		{

		data_array[MY_THREAD] = &data;

		uint64_t events = 0;
		uint64_t read;
		uint64_t min;
		uint64_t cur;
		uint64_t max;
		double percent;
		bool ready = false;
		map<uint32_t, uint32_t> invers_proc_map;

		uint32_t maxfiles = ( nfiles / THREAD_NUM ) +
				( (uint32_t)MY_THREAD < (nfiles % (uint32_t)THREAD_NUM) ? 1 : 0 );

		uint32_t start = 0;
		uint32_t end = 0;
#   ifdef _OPENMP
		for(int k=0; k<MY_THREAD; k++) {
		  	start += threads[k];
		}
#   endif
		end = start + threads[MY_THREAD] - 1;

		for(uint32_t i=0; i<num_cpus; i++) {
			invers_proc_map[processlist[i]] = i;
		}
			
		manager= OTF_FileManager_open(maxfiles);
		assert(NULL != manager);

		handlers = OTF_HandlerArray_open();

		reader = OTF_Reader_open(file, manager);
		assert(NULL != reader);

		OTF_Reader_setProcessStatusAll(reader, 0);
		for(uint32_t k=start; k<=end; k++) {
			OTF_Reader_setProcessStatus(reader, processlist[k], 1);
		}

		OTF_Reader_setBufferSizes(reader, buffersize);
	
		/* set the handler functions */
		OTF_HandlerArray_setHandler(handlers, 
			(OTF_FunctionPointer*) handleEnter, OTF_ENTER_RECORD);
		OTF_HandlerArray_setFirstHandlerArg(handlers, 
      		(void*) &data, OTF_ENTER_RECORD);

		OTF_HandlerArray_setHandler(handlers, 
			(OTF_FunctionPointer*) handleLeave, OTF_LEAVE_RECORD);
		OTF_HandlerArray_setFirstHandlerArg(handlers, 
        	(void*) &data, OTF_LEAVE_RECORD);
        
   		OTF_HandlerArray_setHandler(handlers, 
			(OTF_FunctionPointer*) handleCounter, OTF_COUNTER_RECORD);
		OTF_HandlerArray_setFirstHandlerArg(handlers, 
        	(void*) &data, OTF_COUNTER_RECORD);
		
		OTF_HandlerArray_setHandler(handlers, 
			(OTF_FunctionPointer*) handleRecvMsg, OTF_RECEIVE_RECORD);
		OTF_HandlerArray_setFirstHandlerArg(handlers, 
        	(void*) &data, OTF_RECEIVE_RECORD);

		OTF_HandlerArray_setHandler(handlers, 
			(OTF_FunctionPointer*) handleSendMsg, OTF_SEND_RECORD);
		OTF_HandlerArray_setFirstHandlerArg(handlers, 
      	  	(void*) &data, OTF_SEND_RECORD);
		
		OTF_HandlerArray_setHandler(handlers, 
			(OTF_FunctionPointer*) handleCollectiveOperation, OTF_COLLOP_RECORD);
		OTF_HandlerArray_setFirstHandlerArg(handlers, 
   	   	(void*) &data, OTF_COLLOP_RECORD);

		/* read events */
		cerr.setf(ios::fixed, ios::floatfield);
		cerr.precision(2);
		if(status) {
#		ifdef _OPENMP
#			pragma omp single nowait
#		endif
			{
				gettimeofday(&tStart,0);
				cerr << "\n   starts reading the events" << endl;
				cerr.setf(ios::fixed, ios::floatfield);
				cerr.precision(2);
				cerr << endl;
			}
		}

		OTF_Reader_setRecordLimit( reader, (uint64_t) 1000000 );
		while(current_reader > 0) {
			read = OTF_Reader_readEventsUnsorted(reader, handlers);
			if( read == OTF_READ_ERROR ) {
				cerr << "Cannot read events. The tracefile seems to be damaged. Abort." << endl;
				exit(0);
			}

#			ifdef _OPENMP
			if(status) {
#				pragma omp barrier
			}
#			endif
			if( (read <= 0) && (ready == false) ) {
#				ifdef _OPENMP
#					pragma omp critical(decrement)
#				endif
				{
					current_reader--;
				}
				ready = true;
			}

			if(ready == false) {
	        		for(uint32_t k=start; k<=end; k++) {
					Process *p = &(&data)->p_map[ processlist[k] ];
					p->calc_mbyte_per_sec(processlist[k], invers_proc_map, cpu2thread, data_array);
				}

				if(status) {
					events += read;
					OTF_Reader_eventBytesProgress( reader, &min, &cur, &max );
					percent = 100.0 * ((double) (cur - min)) / ((double) (max - min));
#					ifdef _OPENMP
#						pragma omp critical(progress)
#					endif
					{
						global_events += read;
						global_percent += percent;
					}
				}
			}

			if ( status ) {
#				ifdef _OPENMP
#					pragma omp barrier
#				endif
			}

			if ( status && ready == false ) {
#				ifdef _OPENMP
#					pragma omp single nowait
#				endif
				{
					global_percent += ( num_threads - current_reader ) * 100.0;
					cerr <<  "   " << (global_percent / (double) num_threads) << "% of all events read, events read: " << global_events << "\r";
					global_percent = 0;
				}
			}
		}

		OTF_Reader_setRecordLimit( reader, (uint64_t) OTF_READ_MAXRECORDS );
			
		OTF_Reader_close(reader);
		OTF_HandlerArray_close(handlers);
		OTF_FileManager_close(manager);
		
		set_time_sum_container(&data);

#		ifdef _OPENMP
#			pragma omp barrier
#		endif
		for(uint32_t k=start; k<=end; k++) {
			Process *p = &(&data)->p_map[ processlist[k] ];
			p->calc_mbyte_per_sec(processlist[k], invers_proc_map, cpu2thread, data_array);
		}

#		ifdef _OPENMP
#			pragma omp critical(merge)
#		endif
		{
			gd->sum_container.mergeContainer(data.sum_container);
			mergeProgTime(gd, &data);
		}
	
#		ifdef _OPENMP
#			pragma omp barrier
#		endif

		} /* end parallel */

		if(status)
		{
			cerr << "\n   all events read" << endl;
			
			gettimeofday(&tEnd,0);
	
			cerr << "\n   time used for profiling : " << 
			((double) tEnd.tv_sec + (double) tEnd.tv_usec / 1000000)
		        - ( (double) tStart.tv_sec + (double) tStart.tv_usec / 1000000) << endl;

			cerr << "\n   producing files...";
		}

	} // end !sum
		free(file);

		out.setf(ios::fixed, ios::floatfield);
		out.precision(6);
		out.open((gd->filename_path + "_func.csv").c_str(), ios::out | ios::trunc);
		gd->sum_container.csv_Function(out, 1);
		out.close();
		out.open((gd->filename_path + "_p2p.csv").c_str(), ios::out | ios::trunc);
		gd->sum_container.csv_P2P(out, 1);
		out.close();
		out.open((gd->filename_path + "_collop.csv").c_str(), ios::out | ios::trunc);
		gd->sum_container.csv_CollOp(out, 1);
		out.close();
		out.open((gd->filename_path + "_data.csv").c_str(), ios::out | ios::trunc);
		gd->sum_container.csv_Data(out, 1);
		out.close();
	}

	if(tex != TEX_OFF) {
		prod_tex(tex, gd, counter_names, sum);
		
		if(ps == true) {
			string exe = "cd " + output_dir + "; latex -halt-on-error " + gd->filename + 
											"_result.tex > /dev/null 2>&1";
			int ret = system(exe.c_str());
			if(ret != 0) {
			  //cerr << " Warning: could not create ps-file! latex not installed." << endl;
			} else {
			  exe = "cd " + output_dir + "; dvips " + gd->filename + "_result.dvi > /dev/null 2>&1";
			  ret = system(exe.c_str());
			  if(ret != 0) {
			      //cerr << " Warning: could not create ps-file! dvips not installed." << endl;
			  }
			}
		}
	}

	cerr << "\tfinished." << endl;

	return 0;
}
