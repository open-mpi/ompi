/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2010.
 Authors: Andreas Knuepfer, Denis Huenich, Johannes Spazier
*/

#include "DataStructure.h"

/************************************** operator< **************************************/

bool operator<(const FG_Def_Key& c1, const FG_Def_Key& c2 )
{
  if(c1.trace != c2.trace)
    return (c1.trace < c2.trace);
  else if(c1.ident != c2.ident)
    return (c1.ident < c2.ident);
  else 
    return false;
}

bool operator<(const Process_Def_Key& c1, const Process_Def_Key& c2)
{
  if(c1.trace != c2.trace)
    return (c1.trace < c2.trace);
  else if(c1.ident != c2.ident)
    return (c1.ident < c2.ident);
  else 
    return false;
}

bool operator<(const Function_Def_Key& c1, const Function_Def_Key& c2)
{
  if(c1.trace != c2.trace)
    return (c1.trace < c2.trace);
  else if(c1.ident != c2.ident)
    return (c1.ident < c2.ident);
  else 
    return false;
}

bool operator<(const Counter_Def_Key& c1, const Counter_Def_Key& c2)
{
  if(c1.trace != c2.trace)
    return (c1.trace < c2.trace);
  else if(c1.ident != c2.ident)
    return (c1.ident < c2.ident);
  else 
    return false;
}

bool operator<(const CollOp_Def_Key& c1, const CollOp_Def_Key& c2)
{
  if(c1.trace != c2.trace)
    return (c1.trace < c2.trace);
  else if(c1.ident != c2.ident)
    return (c1.ident < c2.ident);
  else 
    return false;
}

bool operator<(const Bin_1_Key& c1, const Bin_1_Key& c2)
{
  if(c1.trace != c2.trace)
    return (c1.trace < c2.trace);
  else if(c1.ident != c2.ident)
    return (c1.ident < c2.ident);
  else 
    return false;
}

bool operator<(const Bin_2_Key& c1, const Bin_2_Key& c2)
{
  if(c1.trace != c2.trace)
    return (c1.trace < c2.trace);
  else if(c1.ident != c2.ident)
    return (c1.ident < c2.ident);
  else 
    return false;
}

bool operator<(const Function_Key& f_key1,const Function_Key& f_key2)
{
	if(f_key1.trace != f_key2.trace)
 		return (f_key1.trace < f_key2.trace);
  	else if(f_key1.func != f_key2.func)
  		return (f_key1.func < f_key2.func);
  	else if(f_key1.proc != f_key2.proc)	
		return (f_key1.proc < f_key2.proc);
	else 
		return false;
}

bool operator<(const Counter_Key& c_key1, const Counter_Key& c_key2)
{
	if(c_key1.trace != c_key2.trace)
  		return (c_key1.trace < c_key2.trace);
  	else if(c_key1.func != c_key2.func)
  		return (c_key1.func < c_key2.func);
  	else if(c_key1.proc != c_key2.proc)	
		return (c_key1.proc < c_key2.proc);
	else if(c_key1.counter != c_key2.counter)	
		return (c_key1.counter < c_key2.counter);
	else 
		return false;
}

bool operator<(const P2P_Key& p2p_key1, const P2P_Key& p2p_key2)
{
	if(p2p_key1.trace != p2p_key2.trace)
  		return (p2p_key1.trace < p2p_key2.trace);
  	else if(p2p_key1.sender != p2p_key2.sender)
  		return (p2p_key1.sender < p2p_key2.sender);
  	else if(p2p_key1.receiver != p2p_key2.receiver)	
		return (p2p_key1.receiver < p2p_key2.receiver);
	else if(p2p_key1.bin_1 != p2p_key2.bin_1)	
		return (p2p_key1.bin_1 < p2p_key2.bin_1);
	else if(p2p_key1.bin_2 != p2p_key2.bin_2)	
		return (p2p_key1.bin_2 < p2p_key2.bin_2);
	else 
		return false;
}

bool operator<(const CollOp_Key& coll_key1, const CollOp_Key& coll_key2)
{
	if(coll_key1.trace != coll_key2.trace)
  		return (coll_key1.trace < coll_key2.trace);
  	else if(coll_key1.proc != coll_key2.proc)
  		return (coll_key1.proc < coll_key2.proc);
  	else if(coll_key1.collop != coll_key2.collop)	
		return (coll_key1.collop < coll_key2.collop);
	else 
		return false;
}


/************************************** operator== **************************************/


bool operator==(const FG_Def_Key& c1, const FG_Def_Key& c2 )
{
  if(((0 == c1.trace) || (0 == c2.trace) || (c1.trace == c2.trace)) &&
     ((0 == c1.ident) || (0 == c2.ident) || (c1.ident == c2.ident)))
    return true;
  else
    return false;
}

bool operator==(const Process_Def_Key& c1, const Process_Def_Key& c2)
{
  if(((0 == c1.trace) || (0 == c2.trace) || (c1.trace == c2.trace)) &&
     ((0 == c1.ident) || (0 == c2.ident) || (c1.ident == c2.ident)))
    return true;
  else
    return false;
}

bool operator==(const Function_Def_Key& c1, const Function_Def_Key& c2)
{
  if(((0 == c1.trace) || (0 == c2.trace) || (c1.trace == c2.trace)) &&
     ((0 == c1.ident) || (0 == c2.ident) || (c1.ident == c2.ident)))
    return true;
  else
    return false;
}

bool operator==(const Counter_Def_Key& c1, const Counter_Def_Key& c2)
{
  if(((0 == c1.trace) || (0 == c2.trace) || (c1.trace == c2.trace)) &&
     ((0 == c1.ident) || (0 == c2.ident) || (c1.ident == c2.ident)))
    return true;
  else
    return false;
}

bool operator==(const CollOp_Def_Key& c1, const CollOp_Def_Key& c2)
{
  if(((0 == c1.trace) || (0 == c2.trace) || (c1.trace == c2.trace)) &&
     ((0 == c1.ident) || (0 == c2.ident) || (c1.ident == c2.ident)))
    return true;
  else
    return false;
}

bool operator==(const Bin_1_Key& c1, const Bin_1_Key& c2)
{
  if(((0 == c1.trace) || (0 == c2.trace) || (c1.trace == c2.trace)) &&
     ((0 == c1.ident) || (0 == c2.ident) || (c1.ident == c2.ident)))
    return true;
  else
    return false;
}

bool operator==(const Bin_2_Key& c1, const Bin_2_Key& c2)
{
  if(((0 == c1.trace) || (0 == c2.trace) || (c1.trace == c2.trace)) &&
     ((0 == c1.ident) || (0 == c2.ident) || (c1.ident == c2.ident)))
    return true;
  else
    return false;
}



bool operator==(const Function_Key& f_key1,const Function_Key& f_key2)
{
	if(((0 == f_key1.trace) || (0 == f_key2.trace) || (f_key1.trace == f_key2.trace)) &&
      ((0 == f_key1.func) || (0 == f_key2.func) || (f_key1.func == f_key2.func)) &&
      ((0 == f_key1.proc) || (0 == f_key2.proc) || (f_key1.proc == f_key2.proc)))
		return true;
	else
		return false;
}

bool operator==(const Counter_Key& c_key1,const Counter_Key& c_key2)
{
	if(((0 == c_key1.trace) || (0 == c_key2.trace) || (c_key1.trace == c_key2.trace)) &&
      ((0 == c_key1.func) || (0 == c_key2.func) || (c_key1.func == c_key2.func)) &&
      ((0 == c_key1.proc) || (0 == c_key2.proc) || (c_key1.proc == c_key2.proc)) &&
      ((0 == c_key1.counter) || (0 == c_key2.counter) || (c_key1.counter == c_key2.counter)))
		return true;
	else
		return false;
}

bool operator==(const P2P_Key& p2p_key1,const P2P_Key& p2p_key2)
{
	if(((0 == p2p_key1.trace) || (0 == p2p_key2.trace) || (p2p_key1.trace == p2p_key2.trace)) &&
      ((0 == p2p_key1.sender) || (0 == p2p_key2.sender) || (p2p_key1.sender == p2p_key2.sender)) &&
      ((0 == p2p_key1.receiver) || (0 == p2p_key2.receiver) || (p2p_key1.receiver == p2p_key2.receiver)) &&
      ((0 == p2p_key1.bin_1) || (0 == p2p_key2.bin_1) || (p2p_key1.bin_1 == p2p_key2.bin_1)) &&
      ((0 == p2p_key1.bin_2) || (0 == p2p_key2.bin_2) || (p2p_key1.bin_2 == p2p_key2.bin_2)))
		return true;
	else
		return false;
}

bool operator==(const CollOp_Key& co_key1,const CollOp_Key& co_key2)
{
	if(((0 == co_key1.trace) || (0 == co_key2.trace) || (co_key1.trace == co_key2.trace)) &&
      ((0 == co_key1.proc) || (0 == co_key2.proc) || (co_key1.proc == co_key2.proc)) &&
      ((0 == co_key1.collop) || (0 == co_key2.collop) || (co_key1.collop == co_key2.collop)))
		return true;
	else
		return false;
}


/************************************** operator+= **************************************/

Function_Value& Function_Value::operator+=(const Function_Value& f_value)
{
	invoc += f_value.invoc;
	excl_time += f_value.excl_time;
	incl_time += f_value.incl_time;
	
	return *this;
}

Counter_Value& Counter_Value::operator+=(const Counter_Value& c_value)
{
	if((valid == INVALID) || (c_value.valid == INVALID))
		valid = INVALID;
	excl_value += c_value.excl_value;
	incl_value += c_value.incl_value;
	
	return *this;
}

P2P_Value& P2P_Value::operator+=(const P2P_Value& p2p_value)
{
	invoc += p2p_value.invoc;
	length += p2p_value.length;
	time += p2p_value.time;
	
	return *this;
}

CollOp_Value& CollOp_Value::operator+=(const CollOp_Value& coll_value)
{
	invoc_send += coll_value.invoc_send;
	invoc_receive += coll_value.invoc_receive;
	length_send += coll_value.length_send;
	length_receive += coll_value.length_receive;
	time += coll_value.time;
	
	return *this;
}

/************************************** operator= **************************************/

Function_Value& Function_Value::operator=(const Function_Value& f_value)
{
	invoc = f_value.invoc;
	excl_time = f_value.excl_time;
	incl_time = f_value.incl_time;
	
	return *this;
}

Counter_Value& Counter_Value::operator=(const Counter_Value& c_value)
{
	valid = c_value.valid;
	excl_value = c_value.excl_value;
	incl_value = c_value.incl_value;
	
	return *this;
}

P2P_Value& P2P_Value::operator=(const P2P_Value& p2p_value)
{
	invoc = p2p_value.invoc;
	length = p2p_value.length;
	time = p2p_value.time;
	
	return *this;
}

CollOp_Value& CollOp_Value::operator=(const CollOp_Value& coll_value)
{
	invoc_send = coll_value.invoc_send;
	invoc_receive = coll_value.invoc_receive;
	length_send = coll_value.length_send;
	length_receive = coll_value.length_receive;
	time = coll_value.time;
	
	return *this;
}

/****************************** Summary_Container methodes ******************************/

int Summary_Container::adddef_Function(Function_Def_Key f_def_key, Function_Def f_def)
{
	func_def_map.insert(make_pair(f_def_key, f_def));
	return 0;
}

int Summary_Container::adddef_Counter(Counter_Def_Key c_def_key, Counter_Def c_def)
{
	counter_def_map.insert(make_pair(c_def_key, c_def));
	return 0;
}

int Summary_Container::adddef_FG(FG_Def_Key fg_def_key, const char* name)
{
	if(name == NULL)
		fg_def_map.insert(make_pair(fg_def_key,
					    (const char*)"functiongroup"));
	else
		fg_def_map.insert(make_pair(fg_def_key, name));
	
	return 0;
}

int Summary_Container::adddef_Proc(Process_Def_Key p_def_key, const char* name)
{
	if(name == NULL)
		proc_def_map.insert(make_pair(p_def_key,
					      (const char*)"process"));
	else
		proc_def_map.insert(make_pair(p_def_key, name));
		
	return 0;
}

int Summary_Container::adddef_CollOp(CollOp_Def_Key coll_def_key, CollOp_Def coll_def)
{
	collop_def_map.insert(make_pair(coll_def_key, coll_def));
	return 0;
}

int Summary_Container::adddef_Ticks(uint32_t trace, uint64_t ticks)
{
	ticks_def_map.insert(make_pair(trace, ticks));
	return 0;
}

int Summary_Container::adddef_Trace(uint32_t trace, const char* name)
{
	if(name == NULL)
		trace_map.insert(make_pair(trace,
					   (const char*)"trace"));
	else
		trace_map.insert(make_pair(trace, name));
		
	return 0;
}

int Summary_Container::adddef_Bin(uint32_t trace)
{
	uint32_t i;
	
	/* Bin_1 */
	
	uint32_t N = 24;
	uint64_t border = 1;
	uint64_t border_temp = 0;
	for(i = 1; i < N; i++)
	{
		Bin_1_Key b_1_key(trace, i);
		Bin_1_Value b_1_value(border_temp, border);
		bin_1_map.insert(make_pair(b_1_key, b_1_value));
		border_temp = border + 1;
		border <<= 1;
	}
	Bin_1_Key b_1_key(trace, N);
	Bin_1_Value b_1_value(border_temp, border_temp);
	// min_value == max_value -> for all values bigger than min_value 
	bin_1_map.insert(make_pair(b_1_key, b_1_value));
	
	/* Bin_2 */
	
	N = 16;
	border = 4;
	border_temp = 0;
	for(i = 1; i < N; i++)
	{
		Bin_2_Key b_2_key(trace, i);
		Bin_2_Value b_2_value(border_temp, border);
		bin_2_map.insert(make_pair(b_2_key, b_2_value));
		border_temp = border + 1;
		border <<= 2;
	}
	Bin_2_Key b_2_key(trace, N);
	Bin_2_Value b_2_value(border_temp, border_temp); 
	// min_value == max_value -> for all values bigger than min_value  
	bin_2_map.insert(make_pair(b_2_key, b_2_value));
	
	return 0;
}

int Summary_Container::setdef_Bin1(uint32_t trace, uint32_t bin, uint64_t min, uint64_t max)
{
	if((bin <= 0) || (trace <= 0))
	{
		return 1;
	}
	Bin_1_Key b_1_key(trace, bin);
	Bin_1_Value b_1_value(min, max); 
	bin_1_map.insert(make_pair(b_1_key, b_1_value));
	return 0;
}

int Summary_Container::setdef_Bin2(uint32_t trace, uint32_t bin, uint64_t min, uint64_t max)
{
	if((bin <= 0) || (trace <= 0))
	{
		return 1;
	}
	Bin_2_Key b_2_key(trace, bin);
	Bin_2_Value b_2_value(min, max); 
	bin_2_map.insert(make_pair(b_2_key, b_2_value));
	return 0;
}

int Summary_Container::set_ProgTime(uint32_t trace, uint64_t time)
{
	progtime_map[trace] = time;
	return 0;
}

int Summary_Container::addvalues_Function(uint32_t trace, uint32_t func, uint32_t proc, 
                                          uint64_t invoc, uint64_t excl_time, 
                                          uint64_t incl_time)
{
	if((trace == 0) || (func == 0) || (proc == 0))
	{
		cerr << "\nError in addvalues_Function, one or more key parameter were 0." << endl;
		return 1; 
	}
	Function_Key f_key(trace, func, proc);
	Function_Value f_value(invoc, excl_time, incl_time);
	function_map[f_key] += f_value;

	return 0;
}

int Summary_Container::resetvalues_Function(uint32_t trace, uint32_t func, uint32_t proc, 
                                          uint64_t invoc, uint64_t excl_time, 
                                          uint64_t incl_time)
{
	if((trace == 0) || (func == 0) || (proc == 0))
	{
		cerr << "\nError in resetvalues_Function, one or more key parameter were 0." << endl;
		return 1; 
	}
	Function_Key f_key(trace, func, proc);
	Function_Value f_value(invoc, excl_time, incl_time);
	function_map[f_key] = f_value;
	return 0;
}


int Summary_Container::addvalues_Counter(uint32_t trace, uint32_t func, uint32_t proc, 
                                         uint32_t counter, uint64_t excl_value, 
                                         uint64_t incl_value, bool valid)
{
	if((trace == 0) || (func == 0) || (proc == 0) || (counter == 0))
	{
		cerr << "\nError in addvalues_Counter, one or more key parameter were 0." << endl;
		return 1; 
	}
	Counter_Key c_key(trace, func, proc, counter);
	Counter_Value c_value(valid, excl_value, incl_value);
	counter_map[c_key] += c_value;
	return 0;
}

int Summary_Container::addvalues_P2P(uint32_t trace, uint32_t sender, uint32_t receiver, 
                                     uint32_t bin_1, uint32_t bin_2, uint64_t invoc, 
                                     uint64_t length, uint64_t time)
{
	if((trace == 0) || (sender == 0) || (receiver == 0) || (bin_1 == 0) || (bin_2 == 0))
	{
		cerr << "\nError in addvalues_P2P, one or more key parameter were 0." << endl;
		return 1;
	}
	
	P2P_Value p2p_value(invoc, length, time);

	p2p_map[trace][sender][receiver][bin_1][bin_2] += p2p_value;

	return 0;
}

int Summary_Container::resetvalues_P2P(uint32_t trace, uint32_t sender, uint32_t receiver,
                                     uint32_t bin_1, uint32_t bin_2, uint64_t invoc,
                                     uint64_t length, uint64_t time)
{
	if((trace == 0) || (sender == 0) || (receiver == 0) || (bin_1 == 0) || (bin_2 == 0))
	{
		cerr << "\nError in resetvalues_P2P, one or more key parameter were 0." << endl;
		return 1;
	}
	P2P_Key p2p_key(trace, sender, receiver, bin_1, bin_2);
	P2P_Value p2p_value(invoc, length, time);
	
	map<P2P_Key,uint64_t>::iterator iter;
	iter = p2p_time_map.find(p2p_key);
	if(iter == p2p_time_map.end()) p2p_time_map[p2p_key] = 0;

	if(p2p_time_map[p2p_key] == time) {
		p2p_map[trace][sender][receiver][bin_1][bin_2] += p2p_value;
	} else {
		p2p_map[trace][sender][receiver][bin_1][bin_2] = p2p_value;
		p2p_time_map[p2p_key] = time;
	}
	return 0;
}

int Summary_Container::addvalues_CollOp(uint32_t trace, uint32_t proc, uint32_t collop, 
                     uint64_t invoc_send, uint64_t invoc_receive, uint64_t length_send,    
                     uint64_t length_receive, uint64_t time)
{
	if((trace == 0) || (proc == 0) || (collop == 0))
	{
		cerr << "\nError in addvalues_CollOp, one or more key parameter were 0." << endl;
		return 1;
	}
	CollOp_Key collop_key(trace, proc, collop);
	CollOp_Value collop_value(invoc_send, invoc_receive, length_send, length_receive, time);
	collop_map[collop_key] += collop_value;
	return 0;
}

int Summary_Container::resetvalues_CollOp(uint32_t trace, uint32_t proc, uint32_t collop, 
                     uint64_t invoc_send, uint64_t invoc_receive, uint64_t length_send, uint64_t length_receive, uint64_t time)
{
	if((trace == 0) || (proc == 0) || (collop == 0))
	{
		cerr << "\nError in resetvalues_CollOp, one or more key parameter were 0." << endl;
		return 1;
	}
	CollOp_Key collop_key(trace, proc, collop);
	CollOp_Value collop_value(invoc_send, invoc_receive, length_send, length_receive, time);
	collop_map[collop_key] = collop_value;
	return 0;
}

int Summary_Container::addvalues_ProcTime(uint32_t trace, uint32_t proc, uint64_t time)
{
	if((trace == 0) || (proc == 0))
	{
		cerr << "\nError in addvalues_ProcTime, one or more key parameter were 0." << endl;
		return 1; 
	}
	Process_Def_Key proc_def_key(trace, proc);
	proctime_map[proc_def_key] = time;
	return 0;
}

int Summary_Container::get_Function_Def_Key(uint32_t trace, vector<uint32_t>& f_vector)
{
	FuncDefMap::iterator it = func_def_map.begin();
	while(it != func_def_map.end())
	{
		if((((Function_Def_Key) it->first).get_trace() == trace) || (trace == 0))
		{
			f_vector.push_back(((Function_Def_Key) it->first).get_ident());
		}
		++it;
	}
	return 0;
}

int Summary_Container::get_Counter_Def_Key(uint32_t trace, vector<uint32_t>& c_vector)
{
	CounterDefMap::iterator it = counter_def_map.begin();
	while(it != counter_def_map.end())
	{
		if((((Counter_Def_Key) it->first).get_trace() == trace) || (trace == 0))
		{
			c_vector.push_back(((Counter_Def_Key) it->first).get_ident());
		}
		++it;
	}
	return 0;
}

int Summary_Container::get_Process_Def_Key(uint32_t trace, vector<uint32_t>& p_vector)
{
	ProcDefMap::iterator it = proc_def_map.begin();
	while(it != proc_def_map.end())
	{
		if((((Process_Def_Key) it->first).get_trace() == trace) || (trace == 0))
		{
			p_vector.push_back(((Process_Def_Key) it->first).get_ident());
		}
		++it;
	}
	return 0;
}

int Summary_Container::get_FG_Def_Key(uint32_t trace, vector<uint32_t>& fg_vector)
{
	FGDefMap::iterator it = fg_def_map.begin();
	while(it != fg_def_map.end())
	{
		if((((FG_Def_Key) it->first).get_trace() == trace) || (trace == 0))
		{
			fg_vector.push_back(((FG_Def_Key) it->first).get_ident());
		}
		++it;
	}
	return 0;
}

int Summary_Container::get_CollOp_Def_Key(uint32_t trace, vector<uint32_t>& collop_vector)
{
	CollOpDefMap::iterator it = collop_def_map.begin();
	while(it != collop_def_map.end())
	{
		if((((CollOp_Def_Key) it->first).get_trace() == trace) || (trace == 0))
		{
			collop_vector.push_back(((CollOp_Def_Key) it->first).get_ident());
		}
		++it;
	}
	return 0;
}

int Summary_Container::get_Bin1_Def_Key(uint32_t trace, vector<uint32_t>& bin1_vector)
{
	Bin1Map::iterator it = bin_1_map.begin();
	while(it != bin_1_map.end())
	{
		if((((Bin_1_Key) it->first).get_trace() == trace) || (trace == 0))
		{
			bin1_vector.push_back(((Bin_1_Key) it->first).get_ident());
		}
		++it;
	}
	return 0;
}
   	
int Summary_Container::get_Bin2_Def_Key(uint32_t trace, vector<uint32_t>& bin2_vector)
{
	Bin2Map::iterator it = bin_2_map.begin();
	while(it != bin_2_map.end())
	{
		if((((Bin_2_Key) it->first).get_trace() == trace) || (trace == 0))
		{
			bin2_vector.push_back(((Bin_2_Key) it->first).get_ident());
		}
		++it;
	}
	return 0;
}

int Summary_Container::get_Trace(vector<uint32_t>& trace_vector)
{
	TraceMap::iterator it = trace_map.begin();
	while(it != trace_map.end())
	{
		trace_vector.push_back(it->first);
		++it;
	}
	return 0;
}

Function_Def Summary_Container::get_Function_Def(uint32_t trace, uint32_t func)
{
	Function_Def_Key f_def_key(trace, func);
	FuncDefMap::iterator it = func_def_map.find(f_def_key);
		if(it == func_def_map.end())
		{
			Function_Def f_def(NULL, 0); //No entry in map
			return f_def;
		}
		else
			return it->second;
}

Counter_Def Summary_Container::get_Counter_Def(uint32_t trace, uint32_t counter)
{
	Counter_Def_Key c_def_key(trace, counter);
	CounterDefMap::iterator it = counter_def_map.find(c_def_key);
		if(it == counter_def_map.end())
		{
			Counter_Def c_def(NULL, NULL); //No entry in map
			return c_def;
		}
		else
			return it->second;
}

const char* Summary_Container::get_Process_Def(uint32_t trace, uint32_t proc)
{
	Process_Def_Key p_def_key(trace, proc);
	ProcDefMap::iterator it = proc_def_map.find(p_def_key);
		if(it == proc_def_map.end())
			return NULL;
		else
			return it->second;
}

const char* Summary_Container::get_FG_Def(uint32_t trace, uint32_t fg)
{
	FG_Def_Key fg_def_key(trace, fg);
	FGDefMap::iterator it = fg_def_map.find(fg_def_key);
		if(it == fg_def_map.end())
			return NULL;
		else
			return it->second;
}

const char* Summary_Container::get_Trace_name(uint32_t trace)
{
	TraceMap::iterator it = trace_map.find(trace);
		if(it == trace_map.end())
			return NULL;
		else
			return it->second;
}

CollOp_Def Summary_Container::get_CollOp_Def(uint32_t trace, uint32_t collop)
{
	CollOp_Def_Key collop_def_key(trace, collop);
	CollOpDefMap::iterator it = collop_def_map.find(collop_def_key);
		if(it == collop_def_map.end())
		{
			CollOp_Def collop_def(NULL, 0); //No entry in map
			return collop_def;
		}
		else
			return it->second;
}

uint32_t Summary_Container::get_CollOpType_Def(uint32_t trace, uint32_t collop)
{
	CollOp_Def_Key collop_def_key(trace, collop);
	CollOpDefMap::iterator it = collop_def_map.find(collop_def_key);
	if(it == collop_def_map.end())
		return 0;
	else
		return it->second.get_type();
}

Function_Value Summary_Container::get_Function(uint32_t trace, uint32_t func, uint32_t proc)
{
	Function_Key f_key(trace, func, proc);
	Function_Value f_value;
	FunctionMap::iterator it;
	
	if(trace == 0 || func == 0 || proc == 0)
	{
		it = function_map.begin();
		while(it != function_map.end())
		{
			if(it->first == f_key)
				f_value += it->second;
			++it;
		}
		return f_value;
	}
	else
	{
		it = function_map.find(f_key);
		if(it == function_map.end())
			return f_value;
		else
			return f_value += it->second;
	}
}

Counter_Value Summary_Container::get_Counter(uint32_t trace, uint32_t func, uint32_t proc, 
                                             uint32_t counter)
{
	Counter_Key c_key(trace, func, proc, counter);
	Counter_Value c_value;
	CounterMap::iterator it;
	
	if(trace == 0 || func == 0 || proc == 0 || counter == 0)
	{
		it = counter_map.begin();
		while(it != counter_map.end())
		{
			if(it->first == c_key)
				c_value += it->second;
			++it;
		}
		return c_value;
	}
	else
	{
		it = counter_map.find(c_key);
		if(it == counter_map.end())
			return c_value;
		else
			return c_value += it->second;
	}
}

P2P_Value Summary_Container::get_P2P(uint32_t trace, uint32_t sender, uint32_t receiver, 
                                     uint32_t bin_1, uint32_t bin_2)
{	
	P2P_Value p2p_value;
	map<uint32_t, map<uint32_t, map<uint32_t, map<uint32_t, map<uint32_t, P2P_Value> > > > >::iterator trace_it;
	map<uint32_t, map<uint32_t, map<uint32_t, map<uint32_t, P2P_Value> > > >::iterator send_it;
	map<uint32_t, map<uint32_t, map<uint32_t, P2P_Value> > >::iterator recv_it;
	map<uint32_t, map<uint32_t, P2P_Value> >::iterator bin1_it;
	map<uint32_t, P2P_Value>::iterator bin2_it;
	
	if( trace > 0 && bin_1 == 0 && bin_2 == 0) {
	  if( sender > 0 && receiver > 0) {

	    bin1_it = p2p_map[trace][sender][receiver].begin();
	    while(bin1_it != p2p_map[trace][sender][receiver].end() ) {
		    bin2_it = bin1_it->second.begin();
		    while( bin2_it != bin1_it->second.end() ) {
			    p2p_value += bin2_it->second;
			    ++bin2_it;
		    }
		    ++bin1_it;
	    }

	    return p2p_value;

	  } else if( sender > 0) {

	    recv_it = p2p_map[trace][sender].begin();	
	    while(recv_it != p2p_map[trace][sender].end() ) {
		bin1_it = recv_it->second.begin();
		while(bin1_it != recv_it->second.end() ) {
		    bin2_it = bin1_it->second.begin();
		    while( bin2_it != bin1_it->second.end() ) {
			    p2p_value += bin2_it->second;
			    ++bin2_it;
		    }
		    ++bin1_it;
	    	}
		++recv_it;
	    }

	    return p2p_value;

	  } else if( receiver > 0) {

	    send_it = p2p_map[trace].begin();	
	    while( send_it != p2p_map[trace].end() ) {
		bin1_it = send_it->second[receiver].begin();
		while( bin1_it != send_it->second[receiver].end() ) {
		   bin2_it = bin1_it->second.begin();
		   while( bin2_it != bin1_it->second.end() ) {
			    p2p_value += bin2_it->second;
			    ++bin2_it;
		   }
		   ++bin1_it;
		}
		++send_it;
	    }

	    return p2p_value;

	  }

	} else if( trace > 0 && sender == 0 && receiver == 0 ) {
	  if( bin_1 > 0 && bin_2 > 0 ) {

	    send_it = p2p_map[trace].begin();
	    while(send_it != p2p_map[trace].end() ) {
		recv_it = send_it->second.begin();
		while( recv_it != send_it->second.end() ) {
		   p2p_value += recv_it->second[bin_1][bin_2];
		   ++recv_it;
		}
		++send_it;
	    }

	    return p2p_value;

	  } else if( bin_1 > 0 ) {

	    send_it = p2p_map[trace].begin();
	    while(send_it != p2p_map[trace].end() ) {
		recv_it = send_it->second.begin();
		while( recv_it != send_it->second.end() ) {
		   bin2_it = recv_it->second[bin_1].begin();
		   while( bin2_it != recv_it->second[bin_1].end() ) {
			p2p_value += bin2_it->second;
			++bin2_it;
		   }
		   ++recv_it;
		}
		++send_it;
	    }

	    return p2p_value;

	  }
	
	}

	/* this case should never appear, however following part fits for each case (but it is slow) */
	/*cerr << "Unknown constellation while summarising P2P values." << endl;*/
	
	for( trace_it = p2p_map.begin(); trace_it != p2p_map.end(); ++trace_it) {
	  for( send_it = trace_it->second.begin(); send_it != trace_it->second.end(); ++send_it ) {
	    for( recv_it = send_it->second.begin(); recv_it != send_it->second.end(); ++recv_it ) {
	      for( bin1_it = recv_it->second.begin(); bin1_it != recv_it->second.end(); ++bin1_it ) {
	        for( bin2_it = bin1_it->second.begin(); bin2_it != bin1_it->second.end(); ++bin2_it ) {
		  	
		}
	      }
	    }
	  }
	}

	return p2p_value;
}

CollOp_Value Summary_Container::get_CollOp(uint32_t trace, uint32_t proc, uint32_t collop)
{
	CollOp_Key collop_key(trace, proc, collop);
	CollOp_Value collop_value;
	CollOpMap::iterator it;
	
	if(trace == 0 || proc == 0 || collop == 0)
	{
		it = collop_map.begin();
		while(it != collop_map.end())
		{
			if(it->first == collop_key)
				collop_value += it->second;
			++it;
		}
		return collop_value;
	}
	else
	{
		it = collop_map.find(collop_key);
		if(it == collop_map.end())
			return collop_value;
		else
			return collop_value += it->second;
	}
}

CollOp_Value Summary_Container::get_CollOpType(uint32_t trace, uint32_t proc, uint32_t type)
{
	if(type == 0)
	{
		cerr << "Sorry, type has to be greater then 0, because type has no wildcard option." 
		     << endl;
	}
	
	CollOp_Value collop_value;
	CollOpMap::iterator it;
	
	if(trace == 0 || proc == 0)
	{
		it = collop_map.begin();
		while(it != collop_map.end())
		{
			if(((it->first.trace == trace) || (trace == 0)) && 
			  ((it->first.proc == proc) || (proc == 0)) && 
			   (type == get_CollOpType_Def(it->first.trace,it->first.collop)))
			{
				collop_value += it->second;
			}
			++it;
		}
		return collop_value;
	}
	else
	{
		vector<uint32_t> collop_id;
		get_CollOp_Def_Key(trace, collop_id);
		vector<uint32_t>::iterator it2 = collop_id.begin();
		while(it2 != collop_id.end())
		{
			if(type == get_CollOpType_Def(trace, *it2))
			{
				CollOp_Key collop_key(trace,proc,*it2);
				it = collop_map.find(collop_key);
				if(it != collop_map.end())
				{
					collop_value += it->second;
				}
			}
			++it2;
		}
		return collop_value;
	}
}

uint64_t Summary_Container:: get_ProgTime(uint32_t trace)
{
	ProgTimeMap::iterator it = progtime_map.find(trace);
	if(it == progtime_map.end())
		return 0;
	else 
		return it->second;
}

uint64_t Summary_Container::get_ProcTime(uint32_t trace, uint32_t proc)
{
	Process_Def_Key proc_def_key(trace, proc);
	uint64_t time = 0;
	ProcTimeMap::iterator it;
	
	if((trace == 0) || (proc == 0))
	{
		it = proctime_map.begin();
		while(it != proctime_map.end())
		{
			if(it->first == proc_def_key)
				time += it->second;
			++it;
		}
		return time;
	}
	else
	{
		it = proctime_map.find(proc_def_key);
		if(it == proctime_map.end())
			return time;
		else
			return time += it->second;
	}
}
uint64_t Summary_Container::get_ticks(uint32_t trace)
{
	TicksDefMap::iterator it = ticks_def_map.find(trace);
	if(it == ticks_def_map.end())
		return 0;
	else 
		return it->second;
}

uint32_t Summary_Container::get_bin_1(uint64_t length)
{
	uint32_t N = 24;
	uint32_t i;
	uint64_t border = 1;
	for(i = 1; i < N; i++)
	{
		if(length <= border)
			return i;
		border <<= 1;
	}
	return N; // length is bigger than the last border
}

uint32_t Summary_Container::get_bin_2(double speed)
{
	uint64_t sp = (uint64_t) speed;
	uint32_t N = 16;
	uint32_t i;
	uint64_t border = 4;
	for(i = 1; i < N; i++)
	{
		if(sp <= border)
			return i;
		border <<= 2;
	}
	return N; // speed is bigger than the last border
}


int Summary_Container::get_color_gray(double min, double max, double value, 
                                 float& red, float& green, float& blue)
{
	if((value == min) || (min == max))
	{
		red = 0.9f; green = 0.9f; blue = 0.9f; 
		return 0;
	}
	if(value == max)
	{
		red = 1.0; green = 0.0; blue = 0.0; 
		return 0;
	}
	
	double factor = (max - min) / 5.0;
	uint32_t part = (uint32_t) (((value - min) * 5.0) / (max - min));
	double min_temp = min + (factor * part);
	double max_temp = min + (factor * (part + 1.0));
	double part_temp;
	
	if(value == min_temp)
		part_temp = 0.0;
	else if(value == max_temp)
		part_temp = 1.0; 
	else
		part_temp = (value - min_temp) / (max_temp - min_temp);
	if (part == 0)
		part_temp = part_temp / 2;
	switch(part)
	{
		case 0 :
			red = (float) (0.9 - part_temp);
			green = (float) (0.9 - part_temp);
			blue = (float) (0.9 - part_temp);
			break;
		case 1 :
			red = (float) (0.0);
			green = (float) (part_temp);
			blue = (float) (1.0);
			break;
		case 2 :
			red = (float) (0.0);
			green = (float) (1.0);
			blue = (float) (1.0 - part_temp);
			break;
		case 3 :
			red = (float) (part_temp);
			green = (float) (1.0);
			blue = (float) (0.0);
			break;
		case 4 :
			red = (float) (1.0);
			green = (float) (1.0 - part_temp);
			blue = (float) (0.0);
			break;
		default : cerr << "Error in get_color(). Wrong part calculated." << endl; return 1; 
	}
	
	return 0;
}

int Summary_Container::get_color(double min, double max, double value, 
                                 float& red, float& green, float& blue)
{
	if((value == min) || (min == max))
	{
		red = 0.0; green = 0.0; blue = 1.0; 
		return 0;
	}
	if(value == max)
	{
		red = 1.0; green = 0.0; blue = 0.0; 
		return 0;
	}
	
	double factor = (max - min) / 4.0;
	uint32_t part = (uint32_t) (((value - min) * 4.0) / (max - min));
	double min_temp = min + (factor * part);
	double max_temp = min + (factor * (part + 1.0));
	double part_temp;
	
	if(value == min_temp)
		part_temp = 0.0;
	else if(value == max_temp)
		part_temp = 1.0; 
	else
		part_temp = (value - min_temp) / (max_temp - min_temp);

	switch(part)
	{
		case 0 : red = 0.0f; green = (float) part_temp; blue = 1.0f; break;
		case 1 : red = 0.0f; green = 1.0f; blue = (float) (1.0 - part_temp); break;
		case 2: red = (float) part_temp; green = 1.0f; blue = 0.0f; break;
		case 3 : red = 1.0f; green = (float) (1.0 - part_temp); blue = 0.0f; break;
		default : cerr << "Error in get_color(). Wrong part calculated." << endl; return 1; 
	}
	
	return 0;
}

int Summary_Container::get_gray(double min, double max, double value, 
                                 float& red, float& green, float& blue)
{
	if((value == min) || (min == max))
	{
		red = 1.0; green = 1.0; blue = 1.0; 
		return 0;
	}
	if(value == max)
	{
		red = 0.0; green = 0.0; blue = 0.0; 
		return 0;
	}
	
	double part = (value - min) / (max - min);

	red = (float) (1.0 - part); 
	green = (float) (1.0 - part); 
	blue = (float) (1.0 - part);
	
	return 0;
}

int Summary_Container::get_color_gray(uint64_t min, uint64_t max, uint64_t value, 
                                 float& red, float& green, float& blue)
{
	return get_color((double) min, (double) max, (double) value, red, green, blue);
}

int Summary_Container::get_color(uint64_t min, uint64_t max, uint64_t value, 
                                 float& red, float& green, float& blue)
{
	return get_color((double) min, (double) max, (double) value, red, green, blue);
}

int Summary_Container::get_gray(uint64_t min, uint64_t max, uint64_t value, 
                                 float& red, float& green, float& blue)
{
	return get_color((double) min, (double) max, (double) value, red, green, blue);
}

int Summary_Container::csv_Function(fstream& out, uint32_t trace)
{
	if(trace == 0)
	{
		cerr << "Error in csv_Function. Second parameter was 0" << endl;
		return 1;
	}
	TicksDefMap::iterator it_t = ticks_def_map.find(trace);
	FunctionMap::iterator it = function_map.begin();
	CounterDefMap::iterator it_c = counter_def_map.begin();
	CounterMap::iterator it_c2 = counter_map.begin();
	Counter_Value c_value;

	out << ";;;;;";	
	while(it_c != counter_def_map.end())
	{
		if(((Counter_Def_Key) it_c->first).get_trace() == trace)
		{
			out << ";" << it_c->second.get_name(); 
			out << ";" << it_c->second.get_name();
		}
		++it_c;
	}
	out << endl;
	out << "Function;FunctionGroup;Process;Invocation;Exclusive Time;Inclusive Time";
	
	it_c = counter_def_map.begin();
	while(it_c != counter_def_map.end())
	{
		if(((Counter_Def_Key) it_c->first).get_trace() == trace)
		{
			out << ";Exclusive Value;Inclusive Value"; 
		}
		++it_c;
	}
	out << endl;
	while(it != function_map.end())
	{
		if(it->first.trace == trace)
		{
			out << get_Function_Def(trace, it->first.func).get_name()
		   	<< ";" << get_FG_Def(trace, get_Function_Def(trace, it->first.func).get_funcgroup_id())
		   	<< ";" << get_Process_Def(trace, it->first.proc) 
		   	<< ";" << it->second.invoc
		   	<< ";" << (double) it->second.excl_time / (double) it_t->second
		   	<< ";" << (double) it->second.incl_time / (double) it_t->second;
			it_c = counter_def_map.begin();
			while(it_c != counter_def_map.end())
			{
				Counter_Key c_key(trace,it->first.func,it->first.proc,((Counter_Def_Key) it_c->first).get_ident());
				it_c2 = counter_map.find(c_key);			
				if(it_c2 == counter_map.end())
					out << ";no value";	
				else if(it_c2->second.valid == INVALID)
				{
					out << ";invalid value";
				}
				else
				{		
					out << ";" << it_c2->second.excl_value
		     			<< ";" << it_c2->second.incl_value;
				}
				++it_c;	
			}
			out << endl;
		}
		++it;	
	}
	out << endl;

	return 0;
}      

int Summary_Container::csv_P2P(fstream& out, uint32_t trace)
{
	if(trace == 0)
	{
		cerr << "Error in csv_P2P. Second parameter was 0" << endl;
		return 1;
	}
	uint64_t temp = 1;
	double duration;
	TicksDefMap::iterator it_t = ticks_def_map.find(trace);

	map<uint32_t, map<uint32_t, map<uint32_t, map<uint32_t, P2P_Value> > > >::iterator send_iter;
	map<uint32_t, map<uint32_t, map<uint32_t, P2P_Value> > >::iterator recv_iter;
	map<uint32_t, map<uint32_t, P2P_Value> >::iterator bin1_iter;
	map<uint32_t, P2P_Value>::iterator bin2_iter;
	
	out << "Process to;Process;Msg Length Field;Rate Field;Invocation;Msg Length;Duration;Rate" 	    << endl;
	
	for( send_iter = p2p_map[trace].begin(); send_iter != p2p_map[trace].end(); ++send_iter ) {
	  for( recv_iter = send_iter->second.begin(); recv_iter != send_iter->second.end(); ++recv_iter ) {
	    for( bin1_iter = recv_iter->second.begin(); bin1_iter != recv_iter->second.end(); ++bin1_iter ) {
	      for( bin2_iter = bin1_iter->second.begin(); bin2_iter != bin1_iter->second.end(); ++bin2_iter ) {

		duration = (double) bin2_iter->second.time / (double) it_t->second;
		out << get_Process_Def(trace,send_iter->first) 
		<< ";" << get_Process_Def(trace,recv_iter->first)
		<< ";<" << (temp << bin1_iter->first)
		<< ";<" << (temp << (bin2_iter->first * 2))
		<< ";" << bin2_iter->second.invoc
		<< ";" << bin2_iter->second.length
		<< ";" << duration 
		<< ";" << (double) bin2_iter->second.length / duration << endl;

	      }
	    }
	  }
	}
	
	return 0;
}

int Summary_Container::csv_CollOp(fstream& out, uint32_t trace)
{
	if(trace == 0)
	{
		cerr << "Error in csv_P2P. Second parameter was 0" << endl;
		return 1;
	}
	double duration;
	TicksDefMap::iterator it_t = ticks_def_map.find(trace);
	CollOpMap::iterator it = collop_map.begin();
	out << "Process;Name;Type;Send Invocation;Send Msg Length;Receive Invocation"
		<< ";Receive Msg Length;Duration;Rate" << endl;
	while(it != collop_map.end())
	{
		if(it->first.trace == trace)
		{
			duration = (double) it->second.time / (double) it_t->second;
			out << get_Process_Def(trace,it->first.proc)
			   << ";" << get_CollOp_Def(trace,it->first.collop).get_name();
			switch(get_CollOp_Def(trace,it->first.collop).get_type())
			{
				case OTF_COLLECTIVE_TYPE_BARRIER : out << ";Barrier";break;
				case OTF_COLLECTIVE_TYPE_ONE2ALL : out << ";ONE2ALL";break;
				case OTF_COLLECTIVE_TYPE_ALL2ONE : out << ";ALL2ONE";break;
				case OTF_COLLECTIVE_TYPE_ALL2ALL : out << ";ALL2ALL";break;
				default : out << ";Unknown Type";
			}
			out << ";" << it->second.invoc_send
		   	<< ";" << it->second.invoc_receive
		   	<< ";" << it->second.length_send
		   	<< ";" << it->second.length_receive 
		   	<< ";" << duration 
		   	<< ";" << (double) (it->second.length_send + it->second.length_receive) / duration 
		   	<< endl;
		}	
		++it;	
	}
	
	return 0;
} 

int Summary_Container::csv_Data(fstream& out, uint32_t trace)
{
	if(trace == 0)
	{
		cerr << "Error in csv_P2P. Second parameter was 0" << endl;
		return 1;
	}
	TicksDefMap::iterator it_t = ticks_def_map.find(trace);
	ProgTimeMap::iterator it_pgt = progtime_map.find(trace);
	ProcDefMap::iterator it_pd = proc_def_map.begin();
	FGDefMap::iterator it_fgd = fg_def_map.begin();
	FuncDefMap::iterator it_fd = func_def_map.begin();
	FunctionMap::iterator it_f = function_map.begin();
	CounterDefMap::iterator it_cd = counter_def_map.begin();
	CounterMap::iterator it_c = counter_map.begin();
	CollOpDefMap::iterator it_cod = collop_def_map.begin();
	CollOpMap::iterator it_co = collop_map.begin();
	Bin1Map::iterator it_b1 = bin_1_map.begin();
	Bin2Map::iterator it_b2 = bin_2_map.begin();
	ProcTimeMap::iterator it_pt = proctime_map.begin();
	
	Counter_Value c_value;

	map<uint32_t, map<uint32_t, map<uint32_t, map<uint32_t, P2P_Value> > > >::iterator send_iter;
	map<uint32_t, map<uint32_t, map<uint32_t, P2P_Value> > >::iterator recv_iter;
	map<uint32_t, map<uint32_t, P2P_Value> >::iterator bin1_iter;
	map<uint32_t, P2P_Value>::iterator bin2_iter;


	if(it_t != ticks_def_map.end())
		out << "ticks;" << it_t->second << endl;
	else
		out << "ticks;1" << endl;
	
	if(it_pgt != progtime_map.end())
		out << "progtime;" << it_pgt->second << endl;
	else
		out << "progtime;0" << endl;
	
	while(it_pt != proctime_map.end())
	{
		if(((Process_Def_Key) it_pt->first).get_trace() == trace)
		{
			out << "proctime"
		   	<< ";" << ((Process_Def_Key) it_pt->first).get_ident()
		   	<< ";" << it_pt->second 
		   	<< endl;
		}
		++it_pt;	
	}
	while(it_pd != proc_def_map.end())
	{
		if(((Process_Def_Key) it_pd->first).get_trace() == trace)
		{
			out << "proc"
		   	<< ";" << ((Process_Def_Key) it_pd->first).get_ident()
		   	<< ";" << it_pd->second 
		   	<< endl;
		}
		++it_pd;	
	}
	while(it_fgd != fg_def_map.end())
	{
		if(((FG_Def_Key) it_fgd->first).get_trace() == trace)
		{
			out << "fg"
		   	<< ";" << ((FG_Def_Key) it_fgd->first).get_ident()
		  		<< ";" << it_fgd->second 
		   	<< endl;
		}	
		++it_fgd;	
	}
	while(it_fd != func_def_map.end())
	{
		if(((Function_Def_Key) it_fd->first).get_trace() == trace)
		{
			out << "funcdef"
		   	<< ";" << ((Function_Def_Key) it_fd->first).get_ident()
		   	<< ";" << it_fd->second.get_name()
		   	<< ";" << it_fd->second.get_funcgroup_id() 
		   	<< endl;
		}
		++it_fd;	
	}
	while(it_f != function_map.end())
	{
		if(it_f->first.trace == trace)
		{
			out << "func"
		   	<< ";" << it_f->first.func 
		   	<< ";" << it_f->first.proc
		   	<< ";" << it_f->second.invoc
		   	<< ";" << it_f->second.excl_time
		   	<< ";" << it_f->second.incl_time
		   	<< endl;
		}
		++it_f;	
	}
	while(it_cd != counter_def_map.end())
	{
		if(((Counter_Def_Key) it_cd->first).get_trace() == trace)
		{
			out << "counterdef"
		   	<< ";" << ((Counter_Def_Key) it_cd->first).get_ident()
		   	<< ";" << it_cd->second.get_name()
		   	<< ";" << it_cd->second.get_unit()
		   	<< endl;
		}
		++it_cd;	
	}
	while(it_c != counter_map.end())
	{
		if(it_c->first.trace == trace)
		{
			out << "counter"
		   	<< ";" << it_c->first.func 
		   	<< ";" << it_c->first.proc
		   	<< ";" << it_c->first.counter;
			if(it_c->second.valid == VALID)
			{
				out << ";" << it_c->second.excl_value
		      	<< ";" << it_c->second.incl_value
			   	<< ";VALID";
			}
			else
			{
				out << ";0;0;INVALID";
			}
			out << endl;
		}
		++it_c;	
	}

	for( send_iter = p2p_map[trace].begin(); send_iter != p2p_map[trace].end(); ++send_iter ) {
	  for( recv_iter = send_iter->second.begin(); recv_iter != send_iter->second.end(); ++recv_iter ) {
	    for( bin1_iter = recv_iter->second.begin(); bin1_iter != recv_iter->second.end(); ++bin1_iter ) {
	      for( bin2_iter = bin1_iter->second.begin(); bin2_iter != bin1_iter->second.end(); ++bin2_iter ) {

		out << "p2p"
		<< ";" << send_iter->first 
		<< ";" << recv_iter->first
		<< ";" << bin1_iter->first
		<< ";" << bin2_iter->first
		<< ";" << bin2_iter->second.invoc
		<< ";" << bin2_iter->second.length
		<< ";" << bin2_iter->second.time
		<< endl;

	      }
	    }
	  }
	}

	while(it_cod != collop_def_map.end())
	{
		if(((CollOp_Def_Key) it_cod->first).get_trace() == trace)
		{
			out << "collopdef"
		   	<< ";" << ((CollOp_Def_Key) it_cod->first).get_ident()
		   	<< ";" << it_cod->second.get_name();
			switch(it_cod->second.get_type())
			{
				case OTF_COLLECTIVE_TYPE_BARRIER : out << ";Barrier";break;
				case OTF_COLLECTIVE_TYPE_ONE2ALL : out << ";ONE2ALL";break;
				case OTF_COLLECTIVE_TYPE_ALL2ONE : out << ";ALL2ONE";break;
				case OTF_COLLECTIVE_TYPE_ALL2ALL : out << ";ALL2ALL";break;
				default : out << ";UNKNOWN";
			}
			out << endl;
		}
		++it_cod;	
	}
	while(it_co != collop_map.end())
	{
		if(it_co->first.trace == trace)
		{
			out << "collop"
		      << ";" << it_co->first.proc
		      << ";" << it_co->first.collop
		   	<< ";" << it_co->second.invoc_send
		   	<< ";" << it_co->second.invoc_receive
		   	<< ";" << it_co->second.length_send
		   	<< ";" << it_co->second.length_receive
		   	<< ";" << it_co->second.time
		   	<< endl;
		}
		++it_co;	
	}
	while(it_b1 != bin_1_map.end())
	{
		if(((Bin_1_Key) it_b1->first).get_trace() == trace)
		{
			out << "bin1"
		   	<< ";" << ((Bin_1_Key) it_b1->first).get_ident()
		   	<< ";" << it_b1->second.get_min_value()
		   	<< ";" << it_b1->second.get_max_value()
		   	<< endl;
		}
		++it_b1;	
	}
	while(it_b2 != bin_2_map.end())
	{
		if(((Bin_2_Key) it_b2->first).get_trace() == trace)
		{		
			out << "bin2"
		   	<< ";" << ((Bin_2_Key) it_b2->first).get_ident()
		   	<< ";" << it_b2->second.get_min_value()
		   	<< ";" << it_b2->second.get_max_value()
		   	<< endl;
		}
		++it_b2;	
	}
	return 0;
} 

bool Summary_Container::find_FG(FG_Def_Key fg_def_key)
{
	FGDefMap::iterator it = fg_def_map.find(fg_def_key);
	if(it == fg_def_map.end())
		return false;
	else
		return true; 
}

bool Summary_Container::find_Function(Function_Def_Key f_def_key)
{
	FuncDefMap::iterator it = func_def_map.find(f_def_key);
	if(it == func_def_map.end())
		return false;
	else
		return true; 
}

bool Summary_Container::find_Counter(Counter_Def_Key c_def_key)
{
	CounterDefMap::iterator it = counter_def_map.find(c_def_key);
	if(it == counter_def_map.end())
		return false;
	else
		return true; 
}

bool Summary_Container::find_CollOp(CollOp_Def_Key coll_def_key)
{
	CollOpDefMap::iterator it = collop_def_map.find(coll_def_key);
	if(it == collop_def_map.end())
		return false;
	else
		return true; 
}

int Summary_Container::mergeContainer(Summary_Container& container) {
	FunctionMap fmap = container.function_map;
	FunctionMap::iterator fit;

	for(fit = fmap.begin(); fit!=fmap.end(); fit++) {
		this->function_map[fit->first] += fit->second;
	}

	CounterMap cmap = container.counter_map;
	CounterMap::iterator cit;
	for(cit = cmap.begin(); cit!=cmap.end(); cit++) {
		this->counter_map[cit->first] += cit->second;
	}

	map<uint32_t, map<uint32_t, map<uint32_t, map<uint32_t, map<uint32_t, P2P_Value> > > > >::iterator trace_iter;
	map<uint32_t, map<uint32_t, map<uint32_t, map<uint32_t, P2P_Value> > > >::iterator send_iter;
	map<uint32_t, map<uint32_t, map<uint32_t, P2P_Value> > >::iterator recv_iter;
	map<uint32_t, map<uint32_t, P2P_Value> >::iterator bin1_iter;
	map<uint32_t, P2P_Value>::iterator bin2_iter;

	for( trace_iter = container.p2p_map.begin(); trace_iter != container.p2p_map.end(); ++trace_iter ) {
	  for( send_iter = trace_iter->second.begin(); send_iter != trace_iter->second.end(); ++send_iter ) {
	    for( recv_iter = send_iter->second.begin(); recv_iter != send_iter->second.end(); ++recv_iter ) {
	      for( bin1_iter = recv_iter->second.begin(); bin1_iter != recv_iter->second.end(); ++bin1_iter ) {
	        for( bin2_iter = bin1_iter->second.begin(); bin2_iter != bin1_iter->second.end(); ++bin2_iter ) {

		  this->p2p_map[trace_iter->first][send_iter->first][recv_iter->first][bin1_iter->first][bin2_iter->first] += bin2_iter->second;

	        }
	      }
	    }
	  }
        }

	CollOpMap colmap = container.collop_map;
	CollOpMap::iterator colit;
	for(colit = colmap.begin(); colit!=colmap.end(); colit++) {
		this->collop_map[colit->first] = colit->second;
	}

	ProcTimeMap procmap = container.proctime_map;
	ProcTimeMap::iterator procit;
	for(procit = procmap.begin(); procit!=procmap.end(); procit++) {
		this->proctime_map[procit->first] += procit->second;
	}

	return 0;
}

