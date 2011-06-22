/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2011.
 Authors: Andreas Knuepfer, Denis Huenich, Johannes Spazier
*/

#include "OTF_Platform.h"

#include "CSVParse.h"


void Glob_Maps::set_trace_count(uint32_t t_count)
{
	trace_count = t_count;
}

uint32_t Glob_Maps::set_func(string func_name)
{
	uint32_t id = func_id;
	GlobalMaps::iterator it = funcmap.find(func_name);
	if(it == funcmap.end())
	{
		funcmap.insert(pair<string,uint32_t> (func_name, id));
		func_id++;
		return id;
	}
	else
	{
		return it->second;
	}
}

uint32_t Glob_Maps::set_funcgroup(string fg_name)
{
	uint32_t id = funcgroup_id;
	GlobalMaps::iterator it = funcgroupmap.find(fg_name);
	if(it == funcgroupmap.end())
	{
		funcgroupmap.insert(pair<string,uint32_t> (fg_name, id));
		funcgroup_id++;
		return id;
	}
	else
	{
		return it->second;
	}
}

uint32_t Glob_Maps::set_counter(string counter_name)
{
	uint32_t id = counter_id;
	GlobalMaps::iterator it = countermap.find(counter_name);
	if(it == countermap.end())
	{
		countermap.insert(pair<string,uint32_t> (counter_name, id));
		counter_id++;
		return id;
	}
	else
	{
		return it->second;
	}
}

uint32_t Glob_Maps::set_collop(string collop_name)
{
	uint32_t id = collop_id;
	GlobalMaps::iterator it = collopmap.find(collop_name);
	if(it == collopmap.end())
	{
		collopmap.insert(pair<string,uint32_t> (collop_name, id));
		collop_id++;
		return id;
	}
	else
	{
		return it->second;
	}
}

uint32_t Glob_Maps::get_trace_count()
{
	return trace_count;
}

uint32_t Glob_Maps::get_func(string func_name)
{
	GlobalMaps::iterator it = funcmap.find(func_name);
	if(it == funcmap.end())
	{
		return 0;
	}
	else
	{
		return it->second;
	}
}

uint32_t Glob_Maps::get_funcgroup(string fg_name)
{
	GlobalMaps::iterator it = funcgroupmap.find(fg_name);
	if(it == funcgroupmap.end())
	{
		return 0;
	}
	else
	{
		return it->second;
	}
}

uint32_t Glob_Maps::get_counter(string counter_name)
{
	GlobalMaps::iterator it = countermap.find(counter_name);
	if(it == countermap.end())
	{
		return 0;
	}
	else
	{
		return it->second;
	}
}

uint32_t Glob_Maps::get_collop(string collop_name)
{
	GlobalMaps::iterator it = collopmap.find(collop_name);
	if(it == collopmap.end())
	{
		return 0;
	}
	else
	{
		return it->second;
	}
}
	
bool check_value(char* value, const char* place)
{
	if(value == NULL)
	{
		cerr << "Error by reading the csv file." << endl;
		cerr << "An expected value is missing, for the Token " << place << "." << endl;
		return false;
	}
	else
	{
		return true;
	}
}

int parse_csv(Summary_Container& sum_container, const char* file, Glob_Maps& glob_maps)
{
	typedef map<uint32_t,string> LocalMaps;

	LocalMaps localfuncmap;
	LocalMaps localfuncgroupmap;
	LocalMaps localcountermap;
	LocalMaps localcollopmap;
	
	static uint32_t trace_nr = 0;
	trace_nr++;
	
	fstream in;
	in.open(file, ios::in);
	if(!in)
	{
		cerr << "Error, can't find : " << file << endl;
		return 1;
	}
	char buffer[255];
	char* value;
	uint32_t check = 0;
	while(!in.eof())
	{
		in.getline(buffer,255);
		value = strtok(buffer,";");
		if(value == NULL)
		{
			continue;
		}
		
		if(0 == strcmp("ticks", value))
		{
			value = strtok(NULL,";");
			if(!check_value(value, "ticks")) // read ticks
			{
				return 1;
			}
			sum_container.adddef_Ticks(trace_nr, (uint64_t) atol(value));
		}
		if(0 == strcmp("progtime", value))
		{
			value = strtok(NULL,";");
			if(!check_value(value, "progtime")) // read progtime
			{
				return 1;
			}
			sum_container.set_ProgTime(trace_nr, (uint64_t) atol(value));
		}
		else if(0 == strcmp("proctime", value)) // read proctime
		{
			value = strtok(NULL,";");
			if(!check_value(value, "proctime"))
			{
				return 1;
			}
			uint32_t proc_id = (uint32_t) atoi(value);
			value = strtok(NULL,";");
			if(!check_value(value, "proctime"))
			{
				return 1;
			}
			sum_container.addvalues_ProcTime(trace_nr, proc_id, (uint64_t) atol(value));
		}
		else if(0 == strcmp("proc", value)) // read proc
		{
			value = strtok(NULL,";");
			if(!check_value(value, "proc"))
			{
				return 1;
			}
			Process_Def_Key p_def_key(trace_nr, (uint32_t) atoi(value));
			value = strtok(NULL,";");
			if(!check_value(value, "proc"))
			{
				return 1;
			}
			sum_container.adddef_Proc(p_def_key, strdup(value));
		}
		else if(0 == strcmp("fg", value)) // read fg
		{
			value = strtok(NULL,";");
			if(!check_value(value, "fg"))
			{
				return 1;
			}
			uint32_t fg_id = (uint32_t) atoi(value);
			value = strtok(NULL,";");
			if(!check_value(value, "fg"))
			{
				return 1;
			}
			const char* name_char = strdup(value);
			string name;
			name.assign(name_char);
			localfuncgroupmap.insert(make_pair(fg_id, name));
			fg_id = glob_maps.set_funcgroup(name);
			
			FG_Def_Key fg_def_key(trace_nr, fg_id);
			sum_container.adddef_FG(fg_def_key, name_char);

		}
		else if(0 == strcmp("funcdef", value)) // read funcdef
		{
			value = strtok(NULL,";");
			if(!check_value(value, "funcdef"))
			{
				return 1;
			}
			uint32_t func_id = (uint32_t) atoi(value);
			value = strtok(NULL,";");
			if(!check_value(value, "funcdef"))
			{
				return 1;
			}
			const char* name_char = strdup(value);
			string name;
			name.assign(name_char);
			localfuncmap.insert(make_pair(func_id, name));
			func_id = glob_maps.set_func(name);
			Function_Def_Key f_def_key(trace_nr, func_id);
			
			value = strtok(NULL,";");
			if(!check_value(value, "funcdef"))
			{
				return 1;
			}
			LocalMaps::iterator it = localfuncgroupmap.find((uint32_t) atoi(value));
			if(it == localfuncgroupmap.end())
			{
				cerr << "Error by getting values for function. No function group name found" 
					  << ", for the given identifier. A failure in the csv file could be" 
					  << " the reason." << endl;
			}
			else
			{
				uint32_t fg_id = glob_maps.get_funcgroup(it->second);
				if(fg_id == 0)
				{
					cerr << "No function group entry found."
					     << "This could be a failure in the csv file." << endl;
					continue;
				}
				Function_Def f_def(name_char, fg_id);
				sum_container.adddef_Function(f_def_key, f_def);
			}
		}
		else if(0 == strcmp("func", value)) // read func
		{
			value = strtok(NULL,";");
			if(!check_value(value, "func"))
			{
				return 1;
			}
			uint32_t func_id = (uint32_t) atoi(value);
			LocalMaps::iterator it = localfuncmap.find(func_id);
			if(it == localfuncmap.end())
			{
				cerr << "Error by getting values for function. No function name found" 
					  << ", for the given identifier. A failure in the csv file could be" 
					  << " the reason." << endl;
				continue;
			}

			func_id = glob_maps.get_func(it->second);
			
			value = strtok(NULL,";");
			if(!check_value(value, "func"))
			{
				return 1;
			}
			uint32_t proc_id = (uint32_t) atoi(value);
			value = strtok(NULL,";");
			if(!check_value(value, "func"))
			{
				return 1;
			}
			uint64_t invoc = (uint64_t) atol(value);
			value = strtok(NULL,";");
			if(!check_value(value, "func"))
			{
				return 1;
			}
			uint64_t excl_time = (uint64_t) atol(value);
			value = strtok(NULL,";");
			if(!check_value(value, "func"))
			{
				return 1;
			}
			sum_container.addvalues_Function(trace_nr,func_id, proc_id, invoc,
			                                 excl_time, (uint64_t) atol(value));
		}
		else if(0 == strcmp("counterdef", value)) // read counterdef
		{
			value = strtok(NULL,";");
			if(!check_value(value, "counterdef"))
			{
				return 1;
			}
			uint32_t counter_id = (uint32_t) atoi(value);
			value = strtok(NULL,";");
			if(!check_value(value, "counterdef"))
			{
				return 1;
			}
			const char* name_char = strdup(value);
			string name;
			name.assign(name_char);
			localcountermap.insert(pair<uint32_t,string>(counter_id, name));
			counter_id = glob_maps.set_counter(name);

			Counter_Def_Key c_def_key(trace_nr, counter_id);
			value = strtok(NULL,";");
			if(!check_value(value, "counterdef"))
			{
				return 1;
			}
			Counter_Def c_def(name_char, strdup(value));
			sum_container.adddef_Counter(c_def_key,c_def);
		}
		else if(0 == strcmp("counter", value)) // read counter
		{
			value = strtok(NULL,";");
			if(!check_value(value, "counter"))
			{
				return 1;
			}
			uint32_t func_id = (uint32_t) atoi(value);
			LocalMaps::iterator it = localfuncmap.find(func_id);
			if(it == localfuncmap.end())
			{
				cerr << "Error by getting values for counter. No function name found" 
					  << ", for the given identifier. A failure in the csv file could be" 
					  << " the reason." << endl;
				continue;
			}
			else
			{
				func_id = glob_maps.get_func(it->second);
			}
			value = strtok(NULL,";");
			if(!check_value(value, "counter"))
			{
				return 1;
			}
			uint32_t proc_id = (uint32_t) atoi(value);
			value = strtok(NULL,";");
			if(!check_value(value, "counter"))
			{
				return 1;
			}
			uint32_t counter_id = (uint32_t) atoi(value);
			LocalMaps::iterator it2 = localcountermap.find(counter_id);
			if(it2 == localcountermap.end())
			{
				cerr << "Error by getting values for counter. No counter name found" 
					  << ", for the given identifier. A failure in the csv file could be" 
					  << " the reason." << endl;
				continue;
			}
			else
			{
				counter_id = glob_maps.get_counter(it2->second);
			}
			value = strtok(NULL,";");
			if(!check_value(value, "counter"))
			{
				return 1;
			}
			uint64_t excl_value = (uint64_t) atol(value);
			value = strtok(NULL,";");
			if(!check_value(value, "counter"))
			{
				return 1;
			}
			uint64_t incl_value = (uint64_t) atol(value);
			value = strtok(NULL,";");
			if(!check_value(value, "counter"))
			{
				return 1;
			}
			if(0 == strcmp("VALID", value))
			{
				sum_container.addvalues_Counter(trace_nr, func_id, proc_id, counter_id, 
			                                   excl_value, incl_value, VALID);
			}
			else
			{
				sum_container.addvalues_Counter(trace_nr, func_id, proc_id, counter_id, 
			                                   excl_value, incl_value, INVALID);
			}
		}
		else if(0 == strcmp("p2p", value)) // read p2p
		{
			value = strtok(NULL,";");
			if(!check_value(value, "p2p"))
			{
				return 1;
			}
			uint32_t sender = (uint32_t) atoi(value);
			value = strtok(NULL,";");
			if(!check_value(value, "p2p"))
			{
				return 1;
			}
			uint32_t receiver = (uint32_t) atoi(value);
			value = strtok(NULL,";");
			if(!check_value(value, "p2p"))
			{
				return 1;
			}
			uint32_t bin_1 = (uint32_t) atoi(value);
			value = strtok(NULL,";");
			if(!check_value(value, "p2p"))
			{
				return 1;
			}
			uint32_t bin_2 = (uint32_t) atoi(value);
			value = strtok(NULL,";");
			if(!check_value(value, "p2p"))
			{
				return 1;
			}
			uint64_t invoc = (uint64_t) atol(value);
			value = strtok(NULL,";");
			if(!check_value(value, "p2p"))
			{
				return 1;
			}
			uint64_t length = (uint64_t) atol(value);
			value = strtok(NULL,";");
			if(!check_value(value, "p2p"))
			{
				return 1;
			}
			sum_container.addvalues_P2P(trace_nr, sender, receiver, bin_1, bin_2, invoc, 
				                                    length, (uint64_t) atol(value));
		}
		else if(0 == strcmp("collopdef", value)) // read collopdef
		{
			value = strtok(NULL,";");
			if(!check_value(value, "collopdef"))
			{
				return 1;
			}
			uint32_t collop_id = (uint32_t) atoi(value);
			
			value = strtok(NULL,";");
			if(!check_value(value, "collopdef"))
			{
				return 1;
			}
			const char* name_char = strdup(value);
			string name;
			name.assign(name_char);
			localcollopmap.insert(pair<uint32_t,string>(collop_id, name));
			collop_id = glob_maps.set_collop(name);	

			CollOp_Def_Key collop_def_key(trace_nr, collop_id);
			value = strtok(NULL,";");
			if(!check_value(value, "collopdef"))
			{
				return 1;
			}
			uint32_t type;
			if(0 == strcmp("Barrier", value))
			{
				type = OTF_COLLECTIVE_TYPE_BARRIER;
			}
			else if(0 == strcmp("ONE2ALL", value))
			{
				type = OTF_COLLECTIVE_TYPE_ONE2ALL;
			}
			else if(0 == strcmp("ALL2ONE", value))
			{
				type = OTF_COLLECTIVE_TYPE_ALL2ONE;
			}
			else if(0 == strcmp("ALL2ALL", value))
			{
				type = OTF_COLLECTIVE_TYPE_ALL2ALL;
			}
			else
			{
				type = OTF_COLLECTIVE_TYPE_UNKNOWN;
			}
			CollOp_Def collop_def(name_char, type);
			sum_container.adddef_CollOp(collop_def_key, collop_def);
		}
		else if(0 == strcmp("collop", value)) // read collop
		{
			value = strtok(NULL,";");
			if(!check_value(value, "collop"))
			{
				return 1;
			}
			uint32_t proc = (uint32_t) atoi(value);
			value = strtok(NULL,";");
			if(!check_value(value, "collop"))
			{
				return 1;
			}
			uint32_t collop_id = (uint32_t) atoi(value);
			LocalMaps::iterator it = localcollopmap.find(collop_id);
			if(it == localcollopmap.end())
			{
				cerr << "Error by getting values for Collective Operations." 
				     << "No Collective Operation name found" 
					  << ", for the given identifier. A failure in the csv file could be" 
					  << " the reason." << endl;
				continue;
			}
			
			collop_id = glob_maps.get_collop(it->second);

			value = strtok(NULL,";");
			if(!check_value(value, "collop"))
			{
				return 1;
			}
			uint64_t invoc_s = (uint64_t) atol(value);
			value = strtok(NULL,";");
			if(!check_value(value, "collop"))
			{
				return 1;
			}
			uint64_t invoc_r = (uint64_t) atol(value);
			value = strtok(NULL,";");
			if(!check_value(value, "collop"))
			{
				return 1;
			}
			uint64_t sent = (uint64_t) atol(value);
			value = strtok(NULL,";");
			if(!check_value(value, "collop"))
			{
				return 1;
			}
			uint64_t received = (uint64_t) atol(value);
			value = strtok(NULL,";");
			if(!check_value(value, "collop"))
			{
				return 1;
			}
			sum_container.addvalues_CollOp(trace_nr, proc, collop_id, invoc_s, invoc_r, sent, 
			                               received, (uint64_t) atol(value));
		}
		else if(0 == strcmp("bin1", value)) // read bin1
		{
			value = strtok(NULL,";");
			if(!check_value(value, "bin1"))
			{
				return 1;
			}
			uint32_t bin = (uint32_t) atoi(value);
			value = strtok(NULL,";");
			if(!check_value(value, "bin1"))
			{
				return 1;
			}
			uint64_t min = (uint64_t) atol(value);
			value = strtok(NULL,";");
			if(!check_value(value, "bin1"))
			{
				return 1;
			}
			sum_container.setdef_Bin1(trace_nr, bin, min, (uint64_t) atol(value));
		}
		else if(0 == strcmp("bin2", value)) // read bin2
		{
			value = strtok(NULL,";");
			if(!check_value(value, "bin2"))
			{
				return 1;
			}
			uint32_t bin = (uint32_t) atoi(value);
			value = strtok(NULL,";");
			if(!check_value(value, "bin2"))
			{
				return 1;
			}
			uint64_t min = (uint64_t) atol(value);
			value = strtok(NULL,";");
			if(!check_value(value, "bin2"))
			{
				return 1;
			}
			sum_container.setdef_Bin2(trace_nr, bin, min, (uint64_t) atol(value));
		}
		else
		{
			if(check < 20)
			{
				++check;
			}
			else
			{
				cerr << "Error in csv_parse().Too much unknown token in csv file : " 
				     << file << endl;
				return 1;
			}
		}
	}
	in.close();
	sum_container.adddef_Trace(trace_nr, file);
	glob_maps.set_trace_count(trace_nr);
	
	return 0;
}

int Glob_Maps::special_synchronize(Summary_Container& sum_container)
{	
	GlobalMaps::iterator it; 
	it = funcmap.begin();
	uint32_t differences[4] = {0,0,0,0}; // 1 := func_def ; 2:= fg_Def ; ...
	while(it != funcmap.end())
	{
		for(uint32_t i = 1; i <= trace_count; i++)
		{
			Function_Def_Key f_def_key(i, it->second);
			if(!sum_container.find_Function(f_def_key))
			{
				cerr << "Function " << it->first << " couldn\'t found in trace " 
					  << sum_container.get_Trace_name(i) << endl;
				for(uint32_t u = 1; u <= trace_count; u++)
				{
					Function_Def_Key f2_def_key(u, it->second);
					if(sum_container.find_Function(f2_def_key))
					{
						Function_Def f_def = sum_container.get_Function_Def(u, it->second);
						sum_container.adddef_Function(f_def_key, f_def);
						differences[0] += 1;
						cerr << "Problem fixed" << endl;
						break;
					}
				}
			}
		}
		++it;
	}
	it = funcgroupmap.begin();
	while(it != funcgroupmap.end())
	{
		for(uint32_t i = 1; i <= trace_count; i++)
		{
			FG_Def_Key fg_def_key(i, it->second);
			if(!sum_container.find_FG(fg_def_key))
			{
				cerr << "Function Group " << it->first << " couldn\'t found in trace " 
					  << sum_container.get_Trace_name(i) << endl;
				sum_container.adddef_FG(fg_def_key, strdup(it->first.c_str()));
				differences[1] += 1;
				cerr << "Problem fixed" << endl; 
			}
		}
		++it;
	}
	it = countermap.begin();
	while(it != countermap.end())
	{
		for(uint32_t i = 1; i <= trace_count; i++)
		{
			Counter_Def_Key c_def_key(i, it->second);
			if(!sum_container.find_Counter(c_def_key))
			{
				cerr << "Counter " << it->first << " couldn\'t found in trace " 
					  << sum_container.get_Trace_name(i) << endl;
				for(uint32_t u = 1; u <= trace_count; u++)
				{
					Counter_Def_Key c2_def_key(u, it->second);
					if(sum_container.find_Counter(c2_def_key))
					{
						Counter_Def c_def = sum_container.get_Counter_Def(u, it->second);
						sum_container.adddef_Counter(c_def_key, c_def);
						differences[2] += 1;
						cerr << "Problem fixed" << endl; 
						break;
					}
				}
			}
		}
		++it;
	}
	
	it = collopmap.begin();
	while(it != collopmap.end())
	{
		for(uint32_t i = 1; i <= trace_count; i++)
		{
			CollOp_Def_Key co_def_key(i, it->second);
			if(!sum_container.find_CollOp(co_def_key))
			{
				cerr << "Collective Operation " << it->first << " couldn\'t found in trace " 
					  << sum_container.get_Trace_name(i) << endl;
				for(uint32_t u = 1; u <= trace_count; u++)
				{
					CollOp_Def_Key co2_def_key(u, it->second);
					if(sum_container.find_CollOp(co2_def_key))
					{
						CollOp_Def co_def = sum_container.get_CollOp_Def(u, it->second);
						sum_container.adddef_CollOp(co_def_key, co_def);
						differences[3] += 1;
						cerr << "Problem fixed" << endl;
						break;
					}
				}
			}
		}
		++it;
	}
	
	cout << endl;
	cout << "Differences in Func_Def       : " << differences[0] << endl;
	cout << "Differences in Func_Group_Def : " << differences[1] << endl;
	cout << "Differences in Counter_Def    : " << differences[2] << endl;
	cout << "Differences in CollOp_Def     : " << differences[3] << endl;
	
	return 0;
}
