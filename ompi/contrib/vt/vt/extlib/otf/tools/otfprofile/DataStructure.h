/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2011.
 Authors: Andreas Knuepfer, Denis Huenich, Johannes Spazier
*/

#ifndef DATASTRUCTURE_H
#define DATASTRUCTURE_H

#include <fstream>
#include <map>
#include <vector>
#include <iostream>

#include "OTF_inttypes.h"
#include "otf.h"

#include "Definitions.h"

/* show wether the result is valid or invalid */
#define INVALID false
#define VALID    true

using namespace std;

/*************************************** FG *****************************************/
/********** Def_Key **********/

class FG_Def_Key
{
	friend bool operator<(const FG_Def_Key& c1, const FG_Def_Key& c2);
	friend bool operator==(const FG_Def_Key& c1, const FG_Def_Key& c2);	

	public:
		FG_Def_Key(uint32_t tr, uint32_t fg_id);
		uint32_t get_trace();
		uint32_t get_ident();
	private:
		uint32_t trace;
		uint32_t ident;
};

inline FG_Def_Key::FG_Def_Key(uint32_t tr, uint32_t fg_id)
	:trace(tr), ident(fg_id)
{
}

inline uint32_t FG_Def_Key::get_trace()
{
		return trace;
}

inline uint32_t FG_Def_Key::get_ident()
{
		return ident;
}

/*************************************** Process *****************************************/
/********** Def_Key **********/

class Process_Def_Key
{
	friend bool operator<(const Process_Def_Key& c1, const Process_Def_Key& c2);
	friend bool operator==(const Process_Def_Key& c1, const Process_Def_Key& c2);
	
	public:
		Process_Def_Key(uint32_t tr, uint32_t p_id);
		uint32_t get_trace();
		uint32_t get_ident();
	private:
		uint32_t trace;
		uint32_t ident;
};

inline Process_Def_Key::Process_Def_Key(uint32_t tr, uint32_t p_id)
	:trace(tr), ident(p_id)
{
}

inline uint32_t Process_Def_Key::get_trace()
{
		return trace;
}

inline uint32_t Process_Def_Key::get_ident()
{
		return ident;
}

/*************************************** Function *****************************************/
/********** Def_Key **********/

class Function_Def_Key
{
	friend bool operator<(const Function_Def_Key& c1, const Function_Def_Key& c2);
	friend bool operator==(const Function_Def_Key& c1, const Function_Def_Key& c2);	
	public:
		Function_Def_Key(uint32_t tr, uint32_t f_id);
		uint32_t get_trace();
		uint32_t get_ident();
	private:
		uint32_t trace;
		uint32_t ident;
};

inline Function_Def_Key::Function_Def_Key(uint32_t tr, uint32_t f_id)
	:trace(tr), ident(f_id)
{
}

inline uint32_t Function_Def_Key::get_trace()
{
		return trace;
}

inline uint32_t Function_Def_Key::get_ident()
{
		return ident;
}

/********** Def **********/

class Function_Def
{
	public:
		Function_Def(const char* n, uint32_t fg);
		uint32_t get_funcgroup_id();
		const char* get_name();
	private:
		const char* name;
		uint32_t funcgroup_id;
};

inline Function_Def::Function_Def(const char* n, uint32_t fg)
	:name(n), funcgroup_id(fg)
{
}

inline const char* Function_Def::get_name()
{
		return name;
}

inline uint32_t Function_Def::get_funcgroup_id()
{
		return funcgroup_id;
}

/********** Key **********/

class Function_Key
{
	friend bool operator<(const Function_Key& f_key1, const Function_Key& f_keyy2);
	friend bool operator==(const Function_Key& f_key1,const Function_Key& f_key2);
	public:
		Function_Key(uint32_t tr, uint32_t f, uint32_t p);
		Function_Key(const Function_Key& f_key);
	//private:       (wenn Tests erfolgreich wieder freigeben)
		uint32_t trace;
		uint32_t func;
		uint32_t proc;
};

inline Function_Key::Function_Key(uint32_t tr, uint32_t f, uint32_t p)
	:trace(tr), func(f), proc(p) 
{
}
inline Function_Key::Function_Key(const Function_Key& f_key)
	:trace(f_key.trace), func(f_key.func), proc(f_key.proc)
{
}

/********** Value **********/

class Function_Value
{
	friend int print_Funtion();
	public:
		Function_Value();
		Function_Value(uint64_t inv, uint64_t excl, uint64_t incl);
		Function_Value(const Function_Value& f_value);
		Function_Value& operator+=(const Function_Value& func_value_add);
		Function_Value& operator=(const Function_Value& f_value);
		uint64_t get_invoc();
		uint64_t get_excl_time();
		uint64_t get_incl_time();
	//private:(wenn Tests erfolgreich wieder freigeben)
		uint64_t invoc;
		uint64_t excl_time;
		uint64_t incl_time;
};

inline Function_Value::Function_Value()
{
	invoc = 0;
	excl_time = 0;
	incl_time = 0;
}

inline Function_Value::Function_Value(uint64_t inv, uint64_t excl, uint64_t incl)
	:invoc(inv), excl_time(excl), incl_time(incl)
{
}

inline Function_Value::Function_Value(const Function_Value& f_value)
	:invoc(f_value.invoc), excl_time(f_value.excl_time), incl_time(f_value.incl_time)
{
}

inline uint64_t Function_Value::get_invoc()
{
	return invoc;
}

inline uint64_t Function_Value::get_excl_time()
{
	return excl_time;
}

inline uint64_t Function_Value::get_incl_time()
{
	return incl_time;
}

/*************************************** Counter ******************************************/
/********** Def_Key **********/

class Counter_Def_Key
{
	friend bool operator<(const Counter_Def_Key& c1, const Counter_Def_Key& c2);
	friend bool operator==(const Counter_Def_Key& c1, const Counter_Def_Key& c2);	
	public:
		Counter_Def_Key(uint32_t tr, uint32_t c_id);
		uint32_t get_trace();
		uint32_t get_ident();
	private:
		uint32_t trace;
		uint32_t ident;
};

inline Counter_Def_Key::Counter_Def_Key(uint32_t tr, uint32_t c_id)
	:trace(tr), ident(c_id)
{
}

inline uint32_t Counter_Def_Key::get_trace()
{
		return trace;
}

inline uint32_t Counter_Def_Key::get_ident()
{
		return ident;
}

/********** Def **********/

class Counter_Def
{
	public:
		Counter_Def(const char* n, const char* u);
		const char* get_name();
		const char* get_unit();
	private:
		const char* name;
		const char* unit;
};

inline Counter_Def::Counter_Def(const char* n, const char* u)
	:name(n), unit(u)
{
}

inline const char* Counter_Def::get_name()
{
		return name;
}

inline const char* Counter_Def::get_unit()
{
		return unit;
}

/********** Key **********/

class Counter_Key
{
	friend bool operator<(const Counter_Key& c_key1, const Counter_Key& c_key2);
	friend bool operator==(const Counter_Key& c_key1,const Counter_Key& c_key2);
	
	public:
		Counter_Key(uint32_t tr, uint32_t f, uint32_t p, uint32_t c);
		Counter_Key(const Counter_Key& c_key);
	//private:(wenn Tests erfolgreich wieder freigeben)
		uint32_t trace;
		uint32_t func;
		uint32_t proc;
		uint32_t counter;
};

inline Counter_Key::Counter_Key(uint32_t tr, uint32_t f, uint32_t p, uint32_t c)
	:trace(tr), func(f), proc(p), counter(c)
{
}

inline Counter_Key::Counter_Key(const Counter_Key& c_key)
	:trace(c_key.trace), func(c_key.func), proc(c_key.proc), counter(c_key.counter)
{
}

/********** Value **********/

class Counter_Value
{
	public:
		Counter_Value();
		Counter_Value(bool val, uint64_t excl, uint64_t incl);
		Counter_Value(const Counter_Value& c_value);
		Counter_Value& operator+=(const Counter_Value& c_value);
		Counter_Value& operator=(const Counter_Value& c_value);
		bool     get_valid();
		uint64_t get_excl_value();
		uint64_t get_incl_value();
	//private:(wenn Tests erfolgreich wieder freigeben)
		bool     valid;
		uint64_t excl_value;
		uint64_t incl_value;
};

inline Counter_Value::Counter_Value()
{
	valid = VALID;
	excl_value = 0;
	incl_value = 0;
}

inline Counter_Value::Counter_Value(bool val, uint64_t excl, uint64_t incl)
	:valid(val), excl_value(excl), incl_value(incl)
{
}

inline Counter_Value::Counter_Value(const Counter_Value& c_value)
	:valid(c_value.valid), excl_value(c_value.excl_value), incl_value(c_value.incl_value)
{
}

inline bool Counter_Value::get_valid()
{
	return valid;
}

inline uint64_t Counter_Value::get_excl_value()
{
	return excl_value;
}

inline uint64_t Counter_Value::get_incl_value()
{
	return incl_value;
}

/****************************************** P2P ********************************************/
/********** Key **********/

class P2P_Key
{
	friend bool operator<(const P2P_Key& p2p_key1, const P2P_Key& p2p_key2);
	friend bool operator==(const P2P_Key& p2p_key1,const P2P_Key& p2p_key2);
	
	public:
		P2P_Key(uint32_t tr, uint32_t send, uint32_t rec, uint32_t b_1, uint32_t b_2);
		P2P_Key(const P2P_Key& p2p_key);
	//private:(wenn Tests erfolgreich wieder freigeben)
		uint32_t trace;
		uint32_t sender;
		uint32_t receiver;
		uint32_t bin_1; 
   	uint32_t bin_2;
};

inline P2P_Key::P2P_Key(uint32_t tr, uint32_t send, uint32_t rec, uint32_t b_1, uint32_t b_2)
	:trace(tr), sender(send), receiver(rec), bin_1(b_1), bin_2(b_2)
{
}

inline P2P_Key::P2P_Key(const P2P_Key& p2p_key)
	:trace(p2p_key.trace), sender(p2p_key.sender), receiver(p2p_key.receiver), 
	 bin_1(p2p_key.bin_1), bin_2(p2p_key.bin_2)
{
}

/********** Value **********/

class P2P_Value
{
	public:
		P2P_Value();
		P2P_Value(uint64_t inv, uint64_t l, uint64_t t);
		P2P_Value(const P2P_Value& p2p_value);
		P2P_Value& operator+=(const P2P_Value& p2p_value);
		P2P_Value& operator=(const P2P_Value& p2p_value);
		uint64_t get_invoc();
		uint64_t get_length();
		uint64_t get_time();
	//private:	(wenn Tests erfolgreich wieder freigeben)
		uint64_t invoc;
		uint64_t length;
		uint64_t time;
};

inline P2P_Value::P2P_Value()
{
	invoc = 0;
	length = 0;
	time = 0;
}

inline P2P_Value::P2P_Value(uint64_t inv, uint64_t l, uint64_t t)
	:invoc(inv), length(l), time(t)
{
}

inline P2P_Value::P2P_Value(const P2P_Value& p2p_value)
	:invoc(p2p_value.invoc), length(p2p_value.length), time(p2p_value.time)
{
}

inline uint64_t P2P_Value::get_invoc()
{
	return invoc;
}

inline uint64_t P2P_Value::get_length()
{
	return length;
}

inline uint64_t P2P_Value::get_time()
{
	return time;
}

/**************************************** CollOp ******************************************/
/********** Def_Key **********/

class CollOp_Def_Key
{
	friend bool operator<(const CollOp_Def_Key& c1, const CollOp_Def_Key& c2);
	friend bool operator==(const CollOp_Def_Key& c1, const CollOp_Def_Key& c2);	
	public:
		CollOp_Def_Key(uint32_t tr, uint32_t coll_id);
		uint32_t get_trace();
		uint32_t get_ident();
	private:
		uint32_t trace;
		uint32_t ident;
};

inline CollOp_Def_Key::CollOp_Def_Key(uint32_t tr, uint32_t coll_id)
	:trace(tr), ident(coll_id)
{
}

inline uint32_t CollOp_Def_Key::get_trace()
{
		return trace;
}

inline uint32_t CollOp_Def_Key::get_ident()
{
		return ident;
}

/********** Def **********/

class CollOp_Def
{
	public:
		CollOp_Def(const char* n, uint32_t t);
		const char* get_name();
		uint32_t    get_type();
	private:
		const char* name;
		uint32_t    type;
};

inline CollOp_Def::CollOp_Def(const char* n, uint32_t t)
	:name(n), type(t)
{
}

inline const char* CollOp_Def::get_name()
{
	return name;
}

inline uint32_t CollOp_Def::get_type()
{
	return type;
}

/********** Key **********/

class CollOp_Key
{
	friend bool operator<(const CollOp_Key& coll_key1, const CollOp_Key& coll_key2);
	friend bool operator==(const CollOp_Key& co_key1,const CollOp_Key& co_key2);
	
	public:
		CollOp_Key(uint32_t tr, uint32_t p, uint32_t co);
		CollOp_Key(const CollOp_Key& coll_key);
	//private:(wenn Tests erfolgreich wieder freigeben)
		uint32_t trace;
		uint32_t proc;
		uint32_t collop;
};

inline CollOp_Key::CollOp_Key(uint32_t tr, uint32_t p, uint32_t co)
	:trace(tr), proc(p), collop(co) 
{
}

inline CollOp_Key::CollOp_Key(const CollOp_Key& coll_key)
	:trace(coll_key.trace), proc(coll_key.proc), collop(coll_key.collop) 
{
}

/********** Value **********/

class CollOp_Value
{
	public:
		CollOp_Value();
		CollOp_Value(uint64_t inv_s, uint64_t inv_r, uint64_t l_s, uint64_t l_r, uint64_t t);
		CollOp_Value(const CollOp_Value& coll_value);
		CollOp_Value& operator+=(const CollOp_Value& coll_value);
		CollOp_Value& operator=(const CollOp_Value& co_value);
		uint64_t get_invoc_send();
		uint64_t get_invoc_receive();
		uint64_t get_length_send();
		uint64_t get_length_receive();
		uint64_t get_time();
	//private:(wenn Tests erfolgreich wieder freigeben)
		uint64_t invoc_send; 
   	uint64_t invoc_receive;
   	uint64_t length_send;
   	uint64_t length_receive;
   	uint64_t time;
};

inline CollOp_Value::CollOp_Value()
{
	invoc_send = 0; 
   invoc_receive = 0;
   length_send = 0;
   length_receive = 0;
   time = 0;
}

inline CollOp_Value::CollOp_Value(uint64_t inv_s, uint64_t inv_r, uint64_t l_s, uint64_t l_r, 
                                  uint64_t t)
	:invoc_send(inv_s), invoc_receive(inv_r), length_send(l_s), length_receive(l_r), time(t)
{
}

inline CollOp_Value::CollOp_Value(const CollOp_Value& coll_value)
	:invoc_send(coll_value.invoc_send), invoc_receive(coll_value.invoc_receive), 
	 length_send(coll_value.length_send), length_receive(coll_value.length_receive), 
	 time(coll_value.time)
{
}

inline uint64_t CollOp_Value::get_invoc_send()
{
	return invoc_send;
}

inline uint64_t CollOp_Value::get_invoc_receive()
{
	return invoc_receive;
}

inline uint64_t CollOp_Value::get_length_send()
{
	return length_send;
}

inline uint64_t CollOp_Value::get_length_receive()
{
	return length_receive;
}

inline uint64_t CollOp_Value::get_time()
{
	return time;
}

/**************************************** Bin_1 ******************************************/
/********** Key **********/

class Bin_1_Key
{
        friend bool operator<(const Bin_1_Key& c1, const Bin_1_Key& c2);
        friend bool operator==(const Bin_1_Key& c1, const Bin_1_Key& c2);	

	public:
		Bin_1_Key(uint32_t tr, uint32_t b);
		uint32_t get_trace();
		uint32_t get_ident();
	private:
		uint32_t trace;
		uint32_t ident;
};

inline Bin_1_Key::Bin_1_Key(uint32_t tr, uint32_t b)
	:trace(tr), ident(b)
{
}

inline uint32_t Bin_1_Key::get_trace()
{
		return trace;
}

inline uint32_t Bin_1_Key::get_ident()
{
		return ident;
}

/********** Value **********/
class Bin_1_Value
{
	public:
		Bin_1_Value(uint64_t min, uint64_t max);
		uint64_t get_min_value();
		uint64_t get_max_value();
	private:
		uint64_t min_value;
		uint64_t max_value;
};

inline Bin_1_Value::Bin_1_Value(uint64_t min, uint64_t max)
	:min_value(min), max_value(max)
{
}

inline uint64_t Bin_1_Value::get_min_value()
{
	return min_value;
}

inline uint64_t Bin_1_Value::get_max_value()
{
	return max_value;
}

/**************************************** Bin_2 ******************************************/
/********** Key **********/

class Bin_2_Key
{
	friend bool operator<(const Bin_2_Key& c1, const Bin_2_Key& c2);
	friend bool operator==(const Bin_2_Key& c1, const Bin_2_Key& c2);
		
	public:
		Bin_2_Key(uint32_t tr, uint32_t b);
		uint32_t get_trace();
		uint32_t get_ident();
	private:
		uint32_t trace;
		uint32_t ident;
};

inline Bin_2_Key::Bin_2_Key(uint32_t tr, uint32_t b)
	:trace(tr), ident(b)
{
}

inline uint32_t Bin_2_Key::get_trace()
{
		return trace;
}

inline uint32_t Bin_2_Key::get_ident()
{
		return ident;
}

/********** Value **********/
class Bin_2_Value
{
	public:
		Bin_2_Value(uint64_t min, uint64_t max);
		uint64_t get_min_value();
		uint64_t get_max_value();
	//private:
		uint64_t min_value;
		uint64_t max_value;
};

inline Bin_2_Value::Bin_2_Value(uint64_t min, uint64_t max)
	:min_value(min), max_value(max)
{
}

inline uint64_t Bin_2_Value::get_min_value()
{
	return min_value;
}

inline uint64_t Bin_2_Value::get_max_value()
{
	return max_value;
}

/********************************** Summary_Container *************************************/

class Summary_Container
{
	/* maps with the values */
	typedef map<Function_Key,Function_Value>   FunctionMap;
	typedef map<Counter_Key,Counter_Value>     CounterMap;
	typedef map<P2P_Key,uint64_t> 		   p2pTimeMap;
	typedef map<CollOp_Key,CollOp_Value>       CollOpMap;
	typedef map<uint32_t,const char*>          TraceMap;
	typedef map<Process_Def_Key,uint64_t>      ProcTimeMap;
	typedef map<uint32_t,uint64_t>             ProgTimeMap; 
	/* maps with the definitions */
	typedef map<Function_Def_Key,Function_Def> FuncDefMap;
	typedef map<FG_Def_Key,const char*>        FGDefMap;
	typedef map<Process_Def_Key,const char*>   ProcDefMap;
	typedef map<CollOp_Def_Key,CollOp_Def>     CollOpDefMap;
	typedef map<Counter_Def_Key,Counter_Def>   CounterDefMap;
	typedef map<uint32_t,uint64_t>             TicksDefMap;
	typedef map<Bin_1_Key,Bin_1_Value>         Bin1Map;
	typedef map<Bin_2_Key,Bin_2_Value>         Bin2Map;

	/*          trace	  sender	receiver      bin1	    bin2      Value		      */
	typedef map<uint32_t, map<uint32_t, map<uint32_t, map<uint32_t, map<uint32_t, P2P_Value> > > > > P2PMap;
		
	public:
		int adddef_Function(Function_Def_Key f_def_key, Function_Def f_def);
		int adddef_Counter(Counter_Def_Key c_def_key, Counter_Def c_def);
		int adddef_FG(FG_Def_Key fg_def_key, const char* name);
		int adddef_Proc(Process_Def_Key p_def_key, const char* name);
		int adddef_CollOp(CollOp_Def_Key coll_def_key, CollOp_Def coll_def);
		int adddef_Ticks(uint32_t trace, uint64_t ticks);
		int adddef_Trace(uint32_t trace, const char* name);
		int adddef_Bin(uint32_t trace);
		int setdef_Bin1(uint32_t trace, uint32_t bin, uint64_t min, uint64_t max);
		int setdef_Bin2(uint32_t trace, uint32_t bin, uint64_t min, uint64_t max);
		int set_ProgTime(uint32_t trace, uint64_t time);
		
		int addvalues_Function(uint32_t trace, uint32_t func, uint32_t proc, uint64_t invoc, 
   		                    uint64_t excl_time, uint64_t incl_time);

		int resetvalues_Function(uint32_t trace, uint32_t func, uint32_t proc,
                                          uint64_t invoc, uint64_t excl_time, 
                                          uint64_t incl_time);
                   
		int addvalues_Counter(uint32_t trace, uint32_t func, uint32_t proc, uint32_t counter, 
   		                   uint64_t excl_value, uint64_t incl_value, bool valid);
                   
		int addvalues_P2P(uint32_t trace, uint32_t sender, uint32_t receiver, uint32_t bin_1, 
   		               uint32_t bin_2, uint64_t invoc, uint64_t length, uint64_t time);

		int resetvalues_P2P(uint32_t trace, uint32_t sender, uint32_t receiver, uint32_t bin_1, 
   		               uint32_t bin_2, uint64_t invoc, uint64_t length, uint64_t time);
                   
		int addvalues_CollOp(uint32_t trace, uint32_t proc, uint32_t type, uint64_t invoc_send, 
   		                  uint64_t invoc_receive, uint64_t length_send, 
   		                  uint64_t length_receive, uint64_t time);

		int resetvalues_CollOp(uint32_t trace, uint32_t proc, uint32_t collop,
                     		  uint64_t invoc_send, uint64_t invoc_receive, uint64_t length_send, uint64_t length_receive, uint64_t time);

        int addvalues_ProcTime(uint32_t trace, uint32_t proc,uint64_t time);                
   		                  
   	int get_Function_Def_Key(uint32_t trace, vector<uint32_t>& f_vector);
   	int get_Counter_Def_Key(uint32_t trace, vector<uint32_t>& c_vector);
   	int get_Process_Def_Key(uint32_t trace, vector<uint32_t>& p_vector);
   	int get_FG_Def_Key(uint32_t trace, vector<uint32_t>& fg_vector);
   	int get_CollOp_Def_Key(uint32_t trace, vector<uint32_t>& collop_vector);
   	int get_Bin1_Def_Key(uint32_t trace, vector<uint32_t>& bin1_vector);
   	int get_Bin2_Def_Key(uint32_t trace, vector<uint32_t>& bin2_vector);
   	int get_Trace(vector<uint32_t>& trace_vector);
   	
   	Function_Def get_Function_Def(uint32_t trace, uint32_t func);
   	Counter_Def  get_Counter_Def(uint32_t trace, uint32_t counter);
   	const char*  get_Process_Def(uint32_t trace, uint32_t proc);
   	const char*  get_FG_Def(uint32_t trace, uint32_t fg);
   	const char*  get_Trace_name(uint32_t trace);
   	CollOp_Def   get_CollOp_Def(uint32_t trace, uint32_t collop);
   	uint32_t     get_CollOpType_Def(uint32_t trace, uint32_t collop);
   	
   	Function_Value get_Function(uint32_t trace, uint32_t func, uint32_t proc);
	   Counter_Value get_Counter(uint32_t trace, uint32_t func, uint32_t proc, 
                                uint32_t counter);
   	P2P_Value get_P2P(uint32_t trace, uint32_t sender, uint32_t receiver, uint32_t bin_1, 
   	                  uint32_t bin_2);
   	CollOp_Value get_CollOp(uint32_t trace, uint32_t proc, uint32_t collop);
   	CollOp_Value get_CollOpType(uint32_t trace, uint32_t proc, uint32_t type);
   	
   	uint64_t get_ProgTime(uint32_t trace);
   	uint64_t get_ProcTime(uint32_t trace, uint32_t proc);
   	uint64_t get_ticks(uint32_t trace);
   	uint32_t get_bin_1(uint64_t length);
		uint32_t get_bin_2(double speed);
		int get_color_gray(double min, double max, double value, 
                    float& red, float& green, float& blue);
		int get_color_gray(uint64_t min, uint64_t max, uint64_t value, 
		              float& red, float& green, float& blue);
		int get_color(double min, double max, double value, 
                    float& red, float& green, float& blue);
		int get_color(uint64_t min, uint64_t max, uint64_t value, 
		              float& red, float& green, float& blue);
		int get_gray(double min, double max, double value, 
                   float& red, float& green, float& blue);
      int get_gray(uint64_t min, uint64_t max, uint64_t value, 
		              float& red, float& green, float& blue);
                     
		bool find_FG(FG_Def_Key fg_def_key);
		bool find_Function(Function_Def_Key f_def_key);
   	bool find_Counter(Counter_Def_Key c_def_key);
   	bool find_CollOp(CollOp_Def_Key coll_def_key);
   	
		int csv_Function(fstream& out, uint32_t trace);
		int csv_P2P(fstream& out, uint32_t trace);
		int csv_CollOp(fstream& out, uint32_t trace);
		int csv_Data(fstream& out, uint32_t trace);
		int mergeContainer(Summary_Container& container);
	
	private:
		FunctionMap   function_map;
		CounterMap    counter_map;
		P2PMap        p2p_map;
		p2pTimeMap    p2p_time_map;
		CollOpMap     collop_map;
		TraceMap      trace_map;
		ProcTimeMap   proctime_map;
		ProgTimeMap   progtime_map;
		
		FuncDefMap    func_def_map;
		FGDefMap      fg_def_map;
		ProcDefMap    proc_def_map;
		CollOpDefMap  collop_def_map;
		CounterDefMap counter_def_map;
		TicksDefMap   ticks_def_map;
		Bin1Map       bin_1_map;
		Bin2Map       bin_2_map;
};

/*
template<class T>
bool operator<(const T& c1, const T& c2)
{
	if(c1.trace != c2.trace)
  		return (c1.trace < c2.trace);
  	else if(c1.ident != c2.ident)
  		return (c1.ident < c2.ident);
	else 
		return false;
}

template<class T>
bool operator==(const T& c1, const T& c2)
{
	if(((0 == c1.trace) || (0 == c2.trace) || (c1.trace == c2.trace)) &&
      ((0 == c1.ident) || (0 == c2.ident) || (c1.ident == c2.ident)))
		return true;
	else
		return false;
}
*/
#endif /* DATASTRUCTURE_H */

