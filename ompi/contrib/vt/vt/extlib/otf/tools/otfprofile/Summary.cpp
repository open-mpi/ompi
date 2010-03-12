/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2010.
 Authors: Andreas Knuepfer, Denis Huenich, Johannes Spazier
*/

#include "Summary.h"


using namespace std;

/*********************** methodes of the class Process ***********************************/

/* fills the stack with functions, which have an enter event on this Process */

void Process::set_exclTime(uint32_t func, uint64_t time)
{
	exclt_str e_str;
	e_str.func_id = func;
	e_str.time = time;
	e_str.diff_time = 0;
	exclt_stack.push(e_str);
}

/* returns the time, which has to differ from the leave time */ 

int Process::get_exclTime(uint32_t func, uint32_t process, uint64_t time, global_data* gd_ptr)
{
	if(time < gd_ptr->min_time)
	{
		exclt_stack.pop();
		return 0;
	}
	exclt_str e_str;
	uint64_t incl_time;
   	uint64_t counter_incl_value, counter_excl_value;
	
	if ( exclt_stack.empty() == true) {
		/* time for error output */
		/* more leave than enter records */
		cerr << "Error. There are more leave than enter records in the otf-file." << endl;
		return 1;
	}

	e_str = exclt_stack.top();
	if((e_str.func_id != func) && (func != 0)) 
		cerr << "\nFailure in the otf-file.The leaving function doesn't exist." << endl;
	
	ProcessCounterMap::iterator it_c;
	Function_Def_Key f_def_key(1, e_str.func_id);
	if(!gd_ptr->sum_container.find_Function(f_def_key))
		cerr << "\nError.Wrong function id saved on stack!" << endl;
	
	exclt_stack.pop();
	
	incl_time = time - e_str.time;
	if(!exclt_stack.empty())
	{
		exclt_stack.top().diff_time += incl_time;
		it_c = exclt_stack.top().p_counter_map.begin();
		ProcessCounterMap::iterator it_c_local;
		while(it_c != exclt_stack.top().p_counter_map.end())
		{
			it_c_local = e_str.p_counter_map.find(it_c->first);
			if((it_c->second.valid == INVALID) || (it_c_local->second.valid == INVALID) 
			                                   || (it_c_local == e_str.p_counter_map.end()))
			{
				it_c->second.valid = INVALID;
	  		}
	  		else
	  		{
	  			it_c->second.diff_value += it_c_local->second.second_value 
				                           - it_c_local->second.start_value;
			}
	  		++it_c;
		}
	}
	gd_ptr->sum_container.addvalues_Function(1,e_str.func_id, process, 1,
	                                         (incl_time - e_str.diff_time), incl_time);

	it_c = e_str.p_counter_map.begin();
	while(it_c != e_str.p_counter_map.end())
	{
	  	if(it_c->second.second_value == 0 )
	  		gd_ptr->sum_container.addvalues_Counter(1, e_str.func_id, process, it_c->first,
	  		                                        0, 0, INVALID);
	  	else
	  	{
	  		counter_incl_value = it_c->second.second_value - it_c->second.start_value;
			counter_excl_value = counter_incl_value - it_c->second.diff_value;
			gd_ptr->sum_container.addvalues_Counter(1, e_str.func_id, process, 
			                                        it_c->first,counter_excl_value,
			                                        counter_incl_value, VALID);
		}
	  	++it_c;
	}
	return 0;
}

int Process::set_counter(uint32_t counter, uint64_t time, uint64_t value, global_data* gd_ptr)
{
	if(exclt_stack.empty())
		return 0;
	
	ProcessCounterMap::iterator it_count = exclt_stack.top().p_counter_map.find(counter);	 
	if(it_count == exclt_stack.top().p_counter_map.end())
	{
		count_str c_str;
		if(exclt_stack.top().time != time)
		{
			c_str.start_value = 0;
			c_str.second_value = 0;
			c_str.diff_value = 0;
			c_str.start_time = 0;
			c_str.second_time = 0;
			c_str.valid = INVALID;
		}
		else
		{
			c_str.start_value = value;
			c_str.second_value = 0;
			c_str.diff_value = 0;
			c_str.start_time = time;
			c_str.second_time = 0;
			c_str.valid = VALID;
		}
		exclt_stack.top().p_counter_map.insert(pair<uint32_t,count_str>(counter,c_str));
	}
	else
	{
		if(it_count->second.valid == VALID)
		{
				it_count->second.second_value = value;
				it_count->second.second_time = time;		
		}
	}
	return 0;
}

/* counts the events of the different collective operations */

void Process::set_data_collective(uint32_t process, uint32_t collop, uint32_t type, bool root,
                                  uint32_t procGroup, uint32_t sent, uint32_t received,
				  uint64_t duration, global_data* gd_ptr)
{
	uint32_t root_sent = 0;
	uint32_t root_received = 0;

	if(type == OTF_COLLECTIVE_TYPE_BARRIER)
	{
		gd_ptr->sum_container.addvalues_CollOp(1, process, collop, 1, 0, 0, 0, duration);
	}
	else if(type == OTF_COLLECTIVE_TYPE_ALL2ALL)
	{
		root_sent = sent / gd_ptr->p_group_map[procGroup];
		root_received = received / gd_ptr->p_group_map[procGroup];
		gd_ptr->sum_container.addvalues_CollOp(1, process, collop, 1, 1, root_sent, root_received, duration);
	}
	else if(type == OTF_COLLECTIVE_TYPE_ONE2ALL)
	{
		if(root) {
			root_sent = sent / gd_ptr->p_group_map[procGroup];
			gd_ptr->sum_container.addvalues_CollOp(1, process, collop, 1, 1, root_sent, received, duration);
		}
		else {
			gd_ptr->sum_container.addvalues_CollOp(1, process, collop, 0, 1, sent, received, duration);
		}
	} 
	else if(type == OTF_COLLECTIVE_TYPE_ALL2ONE)
	{
		if(root) {
			gd_ptr->sum_container.addvalues_CollOp(1, process, collop, 1, 1, sent, received, duration);
		} else {
			gd_ptr->sum_container.addvalues_CollOp(1, process, collop, 1, 0, sent, received, duration);
		}
	}
	else
		cerr << "\nError in otf-file, unknowen type in collective event." << endl;	
}

/* collects the necessary information to calculate Mbyte per second by a send event*/

void Process::set_mbyte_per_sec(uint32_t sender, uint32_t receiver, uint64_t time, uint32_t tag, 
                                bool valid_loc, global_data* gd_ptr)
{
		receive_str r_str;
		
	   	r_str.valid = valid_loc;
	    	r_str.receiver = receiver;
	    	r_str.start_time = time;
	    	r_str.comm_tag = tag;
	    	timelist.push_back(r_str);

		list<receive_str>::iterator send_iter;
}

/* collects the necessary information to calculate Mbyte per second by a receive event*/

void Process::get_mbyte_per_sec(uint32_t sender, uint32_t receiver, uint64_t time, 
                                uint64_t length, uint32_t tag, global_data* gd_ptr)
{
	send_str s_str;
	s_str.sender = sender;
	s_str.comm_tag = tag;
	s_str.end_time = time;
	s_str.length = length;

	recv_map[sender].push_back(s_str);

}

void Process::calc_mbyte_per_sec(uint32_t sender, map<uint32_t, uint32_t>& proc_map, uint32_t* cpu2thread ,global_data** data_ptr) {
	list<receive_str>::iterator send_iter;
	list<receive_str>::iterator tmp_iter;
	list<send_str>::iterator recv_iter;

	uint64_t dur;
	uint32_t bin_1, bin_2;

	Process *proc;
	for(send_iter = timelist.begin(); send_iter != timelist.end(); ++send_iter) {
		proc = &data_ptr[cpu2thread[ proc_map[send_iter->receiver]] ]->p_map[send_iter->receiver];
		for(recv_iter = proc->recv_map[sender].begin(); recv_iter != proc->recv_map[sender].end(); ++recv_iter)
		{
		if( (recv_iter->sender == sender) && (recv_iter->comm_tag == send_iter->comm_tag) ){
			if(send_iter->valid) {
				if(recv_iter->end_time <= send_iter->start_time) {
					proc->recv_map[sender].erase(recv_iter);
					--send_iter;
					cerr << "Error, no time entry / no tag found by calculating MByte per second.\n" << endl;
					break;
				}
				dur = recv_iter->end_time - send_iter->start_time;
				bin_1 = data_ptr[0]->sum_container.get_bin_1(recv_iter->length);
				bin_2 = data_ptr[0]->sum_container.get_bin_2((double) recv_iter->length / ((double) dur /
									    (double) data_ptr[0]->ticks));

				data_ptr[cpu2thread[proc_map[sender]]]->sum_container.addvalues_P2P(1, sender, send_iter->receiver,
										      bin_1, bin_2, 1, recv_iter->length, dur);
				proc->recv_map[sender].erase(recv_iter);
			} else {
				proc->recv_map[sender].erase(recv_iter);
			}
			tmp_iter = send_iter;
			--send_iter;
			timelist.erase(tmp_iter);
			break;
		}
		}
	}
}
	
