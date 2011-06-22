/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2011.
 Authors: Andreas Knuepfer, Denis Huenich, Johannes Spazier
*/

#ifndef CSVPARSER_H
#define CSVPARSER_H

#include <iostream>
#include <fstream>
#include <map>
#include <string>

#include "OTF_inttypes.h"

#include "DataStructure.h"


using namespace std;

// to synchronize the data of the different csv files

class Glob_Maps
{
	typedef map<string,uint32_t> GlobalMaps;
	
	public:
		Glob_Maps();
		int      special_synchronize(Summary_Container& sum_container);
		void     set_trace_count(uint32_t t_count);
		uint32_t get_trace_count();
		// the return value is the identifier
		uint32_t set_func(string func_name);
		uint32_t set_funcgroup(string fg_name);
		uint32_t set_counter(string counter_name);
		uint32_t set_collop(string collop_name);
		
		uint32_t get_func(string func_name);
		uint32_t get_funcgroup(string fg_name);
		uint32_t get_counter(string counter_name);
		uint32_t get_collop(string collop_name);
		
	private:
		uint32_t   trace_count;
		uint32_t   func_id;
		uint32_t   funcgroup_id;
		uint32_t   counter_id;
		uint32_t   collop_id;
		GlobalMaps funcmap;
		GlobalMaps funcgroupmap;
		GlobalMaps countermap;
		GlobalMaps collopmap;
};

inline Glob_Maps::Glob_Maps()
{
	trace_count  = 0;
	func_id      = 1;
	funcgroup_id = 1;
	counter_id   = 1;
	collop_id    = 1;
}

int parse_csv(Summary_Container& sum_container, const char* file, Glob_Maps& glob_maps);

#endif /* CSVPARSER_H */
