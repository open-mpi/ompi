/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2010.
 Authors: Andreas Knuepfer, Denis Huenich, Johannes Spazier
*/

#include "OTF_Platform.h"

#include "Handler.h"


int save_temp(global_data* gd_ptr);

/*  SummaryHandler */

int handleFunctionSummary (void *firsthandlerarg, uint64_t time, uint32_t func, uint32_t proc, uint64_t invocations,				   uint64_t exclTime, uint64_t inclTime) {

	global_data* gd_ptr = (global_data*)firsthandlerarg;
	gd_ptr->sum_container.resetvalues_Function(1,func, proc, invocations, exclTime, inclTime);

	return RETURN_HANDLER_OK;
}

int handleMessageSummary (void *firsthandlerarg, uint64_t time, uint32_t process, uint32_t peer, uint32_t comm, uint32_t type, 			  uint64_t sentNumber, uint64_t receivedNumber, uint64_t sentBytes, uint64_t receivedBytes)
{	
	global_data* gd_ptr = (global_data*)firsthandlerarg;
	
	if(peer == 0) peer = (uint32_t) -1;
	//int bin1 = gd_ptr->sum_container.get_bin_1(sentBytes);
	gd_ptr->sum_container.resetvalues_P2P(1, process, peer, 1, 1, sentNumber, sentBytes, time);
	gd_ptr->sum_container.resetvalues_P2P(1, peer, process, 1, 1, receivedNumber, receivedBytes, time);

	return RETURN_HANDLER_OK;
}

int handleCollopSummary (void *firsthandlerarg, uint64_t time, uint32_t process, uint32_t comm,
			uint32_t collective, uint64_t sentNumber, uint64_t receivedNumber,
			uint64_t sentBytes, uint64_t receivedBytes) {

	/* message length in Summary is calculated differently compared to hanldeCollectiveOperation() */

	global_data* gd_ptr = (global_data*)firsthandlerarg;

	gd_ptr->sum_container.resetvalues_CollOp(1, process, collective, sentNumber, receivedNumber, sentBytes, receivedBytes, time);

	return RETURN_HANDLER_OK;
}

/****************/

int handleDefCreator(void *firsthandlerarg, uint32_t stream, const char *creator) {

	global_data* gd_ptr = (global_data*)firsthandlerarg;
	gd_ptr->creator = creator;

	return RETURN_HANDLER_OK;
}

int handleDefVersion(void *firsthandlerarg, uint32_t stream, uint8_t major, uint8_t minor, uint8_t sub, const char *string) {

	global_data* gd_ptr = (global_data*)firsthandlerarg;
	char ver[30];
	
	snprintf(ver,30,"%u.%u.%u %s",major,minor,sub,string);
	gd_ptr->version = ver;

	return RETURN_HANDLER_OK;
}

int handleDefTimerResolution(void* firsthandlerarg, uint32_t streamid, uint64_t ticks_per_sec)
{
	global_data* gd_ptr = (global_data*)firsthandlerarg;
	gd_ptr->sum_container.adddef_Ticks(1, ticks_per_sec);
	gd_ptr->ticks = ticks_per_sec;
	return RETURN_HANDLER_OK;
}

int handleDefFunction(void* firsthandlerarg, uint32_t streamid,
	uint32_t func, const char* name, uint32_t group, uint32_t scltoken) 
{	 
	global_data* gd_ptr = (global_data*)firsthandlerarg;
	Function_Def_Key f_def_key(1, func);
	if(name == NULL)
	{
		Function_Def f_def("Function", group);
		gd_ptr->sum_container.adddef_Function(f_def_key, f_def);
	}
	else
	{
		Function_Def f_def(strdup(name), group);
		gd_ptr->sum_container.adddef_Function(f_def_key, f_def);
	}
	return RETURN_HANDLER_OK;
}

int handleDefFunctionGroup(void* firsthandlerarg, uint32_t streamid,
	uint32_t funcg, const char* name) 
{	 
	global_data* gd_ptr = (global_data*)firsthandlerarg;
	FG_Def_Key fg_def_key(1, funcg);
	if(name == NULL)
	{
		gd_ptr->sum_container.adddef_FG(fg_def_key, "FuncGroup");
	}
	else
	{
		gd_ptr->sum_container.adddef_FG(fg_def_key, strdup(name));
	}
	return RETURN_HANDLER_OK;
}

int handleDefProcess(void* firsthandlerarg, uint32_t streamid,
	uint32_t proc, const char* name, uint32_t parent)
{
	global_data* gd_ptr = (global_data*)firsthandlerarg;
	Process_Def_Key p_def_key(1, proc);
	if(name == NULL)
	{
		gd_ptr->sum_container.adddef_Proc(p_def_key, "Process");
	}
	else
	{

		char* dup= strdup( name );
		char* p= dup;
		while ( '\0' != *p ) {

			if ( '_' == *p ) *p= ' ';
			if ( '\\' == *p ) *p= ' ';

			p++;
		}

		gd_ptr->sum_container.adddef_Proc(p_def_key, dup );
	}
	Process p;

	/* leere recv_map für prozess anlegen */
	for(uint32_t i=1; i<= gd_ptr->num_cpu; i++) {
		p.clear_recv_map(i);
	}

	gd_ptr->p_map.insert(pair<uint32_t,Process>(proc, p));
	return RETURN_HANDLER_OK;
}

int handleDefProcessGroup(void *firsthandlerarg, uint32_t stream, uint32_t procGroup,
	 const char *name, uint32_t numberOfProcs, const uint32_t *procs)
{	
	global_data* gd_ptr= (global_data*)firsthandlerarg;
	gd_ptr->p_group_map[procGroup] = numberOfProcs;

	return RETURN_HANDLER_OK;
}
	
int handleDefCollectiveOperation(void* firsthandlerarg, uint32_t streamid, 
	uint32_t collop, const char* name, uint32_t type)
{
	global_data* gd_ptr= (global_data*)firsthandlerarg;
	CollOp_Def_Key collop_def_key(1, collop);
	if(name == NULL)
	{
		CollOp_Def collop_def("CollOp", type);
		gd_ptr->sum_container.adddef_CollOp(collop_def_key, collop_def);
	}
	else
	{
		CollOp_Def collop_def(strdup(name), type);
		gd_ptr->sum_container.adddef_CollOp(collop_def_key, collop_def);
	}
	return RETURN_HANDLER_OK;
}

int handleDefCounter( void* firsthandlerarg, uint32_t streamid,
	uint32_t counter, const char* name, uint32_t properties, 
	uint32_t countergroup, const char* unit ) 
{	
	if(properties == OTF_COUNTER_TYPE_ACC)
	{
		global_data* gd_ptr = (global_data*)firsthandlerarg;
		Counter_Def_Key c_def_key(1, counter);
		if(name == NULL)
		{
			Counter_Def c_def("Counter", strdup(unit));
			gd_ptr->sum_container.adddef_Counter(c_def_key,c_def);
		}
		else
		{
			Counter_Def c_def(strdup(name), strdup(unit));
			gd_ptr->sum_container.adddef_Counter(c_def_key,c_def);
		}
	}
	return RETURN_HANDLER_OK;
}

int handleEnter(void* firsthandlerarg, uint64_t time, uint32_t func,
	uint32_t proc, uint32_t scltoken)
{
	global_data* gd_ptr = (global_data*)firsthandlerarg;

	if(time > gd_ptr->max_time && gd_ptr->clear_temp) {
		return RETURN_HANDLER_ABORT;
	}
	else if(time > gd_ptr->max_time) 
	{	
		save_temp(gd_ptr);
		gd_ptr->clear_temp = true;
		return RETURN_HANDLER_ABORT;
	}
	
	uint64_t enter_time = time;
	
	if(time < gd_ptr->min_time)
		enter_time = gd_ptr->min_time;

	ProcessMap::iterator it_p = gd_ptr->p_map.find(proc);
	if(it_p == gd_ptr->p_map.end())
	{
		cerr << "\nprocess : " << proc 
		     << "\n   This process wasn't defined. That could cause a failure of the program" 
		     << endl;
		return OTF_RETURN_BREAK;
	}
	it_p->second.set_exclTime(func, enter_time);
	it_p->second.set_proc_start(enter_time);
	
	if(gd_ptr->prog_start > enter_time)
	{
		gd_ptr->prog_start = enter_time;
	}

	return RETURN_HANDLER_OK;
}

int handleLeave(void* firsthandlerarg, uint64_t time, uint32_t func,
	uint32_t proc, uint32_t scltoken)
{
	global_data* gd_ptr = (global_data*)firsthandlerarg;
	pair<uint32_t, uint64_t> data_exclt;

	if(time > gd_ptr->max_time && gd_ptr->clear_temp) {
		return RETURN_HANDLER_ABORT;
	}
	else if(time > gd_ptr->max_time) 
	{	
		save_temp(gd_ptr);
		gd_ptr->clear_temp = true;
		return RETURN_HANDLER_ABORT;
	}
	
	ProcessMap::iterator it_p = gd_ptr->p_map.find(proc);
	
	if(it_p == gd_ptr->p_map.end()) {
		cerr << "\nFailure in the otf-file.The process in the leaving event doesn't exist." 
		     << endl;
	}
	else
	{	
		it_p->second.get_exclTime(func, proc, time, gd_ptr);
		it_p->second.set_proc_end(time);
		if(gd_ptr->prog_end < time) {
			gd_ptr->prog_end = time;
		}
	}

	return RETURN_HANDLER_OK;
}

int handleCounter(void* firsthandlerarg, uint64_t time, uint32_t proc,
	uint32_t counter, uint64_t value)
{
	global_data* gd_ptr = (global_data*)firsthandlerarg;
	Counter_Def_Key c_def_key(1, counter);
	if(gd_ptr->sum_container.find_Counter(c_def_key))
	{
		ProcessMap::iterator it_p = gd_ptr->p_map.find(proc);
		if(it_p == gd_ptr->p_map.end())
			cerr << "\nFailure in the otf-file.The process in the counter event doesn't exist." 
		    	  << endl;
		else
			it_p->second.set_counter(counter, time, value, gd_ptr);
	}

	return RETURN_HANDLER_OK;
}

int handleSendMsg(void* firsthandlerarg, uint64_t time, uint32_t sender,
	uint32_t receiver, uint32_t communicator, uint32_t msgtype, uint32_t msglength,
	uint32_t scltoken)
{
	global_data* gd_ptr = (global_data*)firsthandlerarg;
	if(time > gd_ptr->max_time && gd_ptr->clear_temp) {
		return RETURN_HANDLER_ABORT;
	}
	else if(time > gd_ptr->max_time) 
	{	
		save_temp(gd_ptr);
		gd_ptr->clear_temp = true;
		return RETURN_HANDLER_ABORT;
	}
	
	ProcessMap::iterator it = gd_ptr->p_map.find(sender);
	if(it == gd_ptr->p_map.end())
		cerr << "\nError in SendMsg event. Sender " << sender << " unknown." << endl;

	if(time < gd_ptr->min_time)
		it->second.set_mbyte_per_sec(sender, receiver, time, msgtype, INVALID, gd_ptr);
	
	else
		it->second.set_mbyte_per_sec(sender, receiver, time, msgtype, VALID, gd_ptr);


	return RETURN_HANDLER_OK;

}

int handleRecvMsg(void* firsthandlerarg, uint64_t time, uint32_t receiver,
	uint32_t sender, uint32_t communicator, uint32_t msgtype, uint32_t msglength,
	uint32_t scltoken)
{
	global_data* gd_ptr = (global_data*)firsthandlerarg;
	if(time > gd_ptr->max_time && gd_ptr->clear_temp) {
		return RETURN_HANDLER_ABORT;
	}
	else if(time > gd_ptr->max_time) 
	{	
		save_temp(gd_ptr);
		gd_ptr->clear_temp = true;
		return RETURN_HANDLER_ABORT;
	}	
	
	ProcessMap::iterator it = gd_ptr->p_map.find(receiver);
	if(it == gd_ptr->p_map.end())
		cerr << "\nError in RecvMsg event. Sender " << sender << " unknown." << endl; 

	it->second.get_mbyte_per_sec(sender, receiver, time, msglength, msgtype, gd_ptr);

	return RETURN_HANDLER_OK;
}

int handleCollectiveOperation(void* firsthandlerarg, uint64_t time, 
	uint32_t proc, uint32_t collop, uint32_t procgroup, 
	uint32_t rootprocess, uint32_t sent, uint32_t received, 
	uint64_t duration, uint32_t scltoken)
{
	global_data* gd_ptr = (global_data*)firsthandlerarg;
	uint32_t type;
	if(time > gd_ptr->max_time && gd_ptr->clear_temp)
		return RETURN_HANDLER_ABORT;
	else if(time > gd_ptr->max_time) 
	{	
		save_temp(gd_ptr);
		gd_ptr->clear_temp = true;
		return RETURN_HANDLER_ABORT;
	}
	else if(time < gd_ptr->min_time)
		return RETURN_HANDLER_OK;
	
	ProcessMap::iterator it_p = gd_ptr->p_map.find(proc);
	if((type = gd_ptr->sum_container.get_CollOpType_Def(1, collop)) != 0)
	{
		if(proc == rootprocess)
			it_p->second.set_data_collective(proc, collop, type, true, procgroup, sent, received, duration, 
			                                 gd_ptr);
		else
			it_p->second.set_data_collective(proc, collop, type, false, procgroup, sent, received, duration, 
			                                 gd_ptr);
	}
	else
	{
		cerr << "\nCollOp : " << collop << " started on Process " << proc << " wasn't defined." 
		     << " That's a failure in the otf-file." << endl; 
	}

	return RETURN_HANDLER_OK;

}

int set_time_sum_container(global_data* gd_ptr)
{
	uint64_t time = 0;
	uint64_t prog_time = 0;
	ProcessMap::iterator it_p = gd_ptr->p_map.begin();
	while(it_p != gd_ptr->p_map.end())
	{
		if(it_p->second.get_proc_end() < it_p->second.get_proc_start())
		{
			time = 0;
			cerr << "Error, proc_start is greater than proc_end." << endl;
		}
		else
		{
			time = it_p->second.get_proc_end() - it_p->second.get_proc_start();
		}
		gd_ptr->sum_container.addvalues_ProcTime(1, it_p->first, time);
		++it_p;
	}
	if(gd_ptr->prog_end < gd_ptr->prog_start)
	{
		cerr << "Error, prog_start is greater than prog_end." << endl;
	}
	else
	{
		prog_time = gd_ptr->prog_end - gd_ptr->prog_start;
	}
	gd_ptr->sum_container.set_ProgTime(1, prog_time);

	return 0;
}

int mergeProgTime(global_data* gd, global_data* data) {
	if(gd->prog_start > data->prog_start) {
		gd->prog_start = data->prog_start;
	}
	if(gd->prog_end < data->prog_end) {
		gd->prog_end = data->prog_end;
	}

	uint64_t time = gd->prog_end - gd->prog_start;

	gd->sum_container.set_ProgTime(1, time);

	return 0;
}

/* this function clears the stack for the exclusive time in each object of the class Process */

int save_temp(global_data* gd_ptr)
{
	ProcessMap::iterator it_p = gd_ptr->p_map.begin();
	uint32_t f_id;
	while(it_p != gd_ptr->p_map.end())
	{
		
		if(it_p->second.get_stack_status())
			++it_p;
		else
		{
			while(!it_p->second.get_stack_status())
			{
				f_id = it_p->second.get_stack_top_func_id();
				it_p->second.get_exclTime(f_id, it_p->first, gd_ptr->max_time, gd_ptr);
			}
			++it_p;
		}
	}
	return 0;
}




