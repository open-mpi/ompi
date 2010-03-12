/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2010.
 Authors: Andreas Knuepfer, Denis Huenich, Johannes Spazier
*/

#ifndef SUMMARY_H
#define SUMMARY_H

#include <iostream>
#include <string>
#include <map>
#include <set>
#include <stack>
#include <list>

#include "OTF_inttypes.h"
#include "otf.h"

#include "Definitions.h"
#include "DataStructure.h"

using namespace std;

struct global_data;


/******************************** class Process **************************************/

/* This class summarizes informations about a Process.
	Informations are: -Point2Point: send and receive
						 	-Collective operations: barrier, broadcast, 
						 	 all2one and all2all
						 	-Process identifier and Process name
						 	-A stack which contains all entered functions 
						 	 without an leaving event.This is useful to 
						 	 determine the exclusive time of a function.*/ 

class Process
{
	public:
		Process();
		void set_proc_start(uint64_t time);
		void set_proc_end(uint64_t time);
		void set_exclTime(uint32_t func, uint64_t time);
		void set_mbyte_per_sec(uint32_t sender, uint32_t receiver, uint64_t time, uint32_t tag, bool valid_loc,
				       global_data* gd_ptr);
		int  set_counter(uint32_t counter, uint64_t time, uint64_t value, global_data* gd_ptr);
		void set_data_collective(uint32_t process, uint32_t collop, uint32_t type, bool root, 
		                         uint32_t procGroup, uint32_t sent, uint32_t received,
					 uint64_t duration, global_data* gd_ptr);
		uint64_t get_proc_start();
		uint64_t get_proc_end();
		int  get_exclTime(uint32_t func, uint32_t process, uint64_t time, global_data* gd_ptr);
		void get_mbyte_per_sec(uint32_t sender, uint32_t receiver, uint64_t time, 
                             uint64_t length, uint32_t tag, global_data* gd_ptr);
		bool get_stack_status();
		uint32_t get_stack_top_func_id();
		void calc_mbyte_per_sec(uint32_t sender, map<uint32_t,uint32_t>& proc_map, uint32_t* cpu2thread,
					global_data** data_ptr);
		void clear_recv_map(int i);
		
	private:
		
		bool proc_set;
		uint64_t proc_start;
		uint64_t proc_end;
		
		// attributes to get the exclusive time and count
		struct count_str
		{
			uint64_t start_value;
			uint64_t second_value;
			uint64_t diff_value;
			uint64_t start_time;
			uint64_t second_time;
			bool     valid;
		};
		typedef map<uint32_t,count_str> ProcessCounterMap;
		struct exclt_str
		{
			uint32_t func_id;
			uint64_t time;
			uint64_t diff_time;
			ProcessCounterMap p_counter_map;
		};
		stack<exclt_str> exclt_stack;
		
		// attribute to get MByte per second
		struct receive_str
		{
			uint32_t receiver;
			uint32_t comm_tag;
			uint64_t start_time;
			bool		valid;		//necessary by time interval
		};
		list<receive_str> timelist;

		struct send_str
		{
			uint32_t sender;
			uint32_t comm_tag;
			uint64_t end_time;
			uint32_t length;
		};

		map<uint32_t, list<send_str> > recv_map;
};


/* *** more types *** */

typedef map<uint32_t,Process> ProcessMap;
typedef map<uint32_t,uint32_t> ProcessGroupMap;	// <GroupID,numberOfProcs>

struct global_data 
{
	string            filename;
	string            filename_path;
	string		  creator;
	string		  version;
	bool		  clear_temp;
	bool              prog;
	int               TOP_FUNC;
	bool              vis; //form of visualization
	bool              var; // show variance instaed of average
	uint64_t          prog_start;
	uint64_t          prog_end;
	uint64_t          min_time;
	uint64_t          max_time;
	uint64_t          ticks;
	uint32_t	  num_cpu;
	ProcessMap        p_map;
	ProcessGroupMap	  p_group_map;
	Summary_Container sum_container;
};



/* *** inline methods *** */


inline Process::Process()
{
	proc_start = 0;
	proc_end = 0;
	proc_set = false;
}

inline void Process::set_proc_start(uint64_t ps)
{
	if(!proc_set)
	{
		proc_start = ps;
		proc_set = true;
	}
}

inline void Process::set_proc_end(uint64_t pe)
{
	proc_end = pe;
}

inline bool Process::get_stack_status()
{
	return exclt_stack.empty();
}

inline uint32_t Process::get_stack_top_func_id()
{
	return exclt_stack.top().func_id;
}

inline uint64_t Process::get_proc_start()
{
	return proc_start;
}

inline uint64_t Process::get_proc_end()
{
	return proc_end;
}

inline void Process::clear_recv_map(int i)
{
	recv_map[i].clear();
}

#endif /* SUMMARY_H */
