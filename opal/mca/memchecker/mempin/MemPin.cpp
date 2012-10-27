#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include <stdio.h>
#include <assert.h>
#include <iostream>
#include <ostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <map>
#include <set>
#include <string>
#include <assert.h>
#include <stdarg.h>

#include "pin.H"
#include "instlib.H"
#include "portability.H"

#include "MemPin.h"

using namespace INSTLIB;

typedef struct {
    size_t          index;   /* index of the element */
    size_t          addr;    /* the registered mem address */
    size_t          size;    /* size of the memory */
    int             op;      /* memory operation */
    MEMPIN_REG_CB_T cb_func; /* callback function pointer */
    void*           cb_args; /* list of callback function args */
} addr_range_t;


// the key of the maps is start address of each entry
// the second element stores the whole structure
multimap<size_t, addr_range_t> addr_map, result_map;

// declaration of a function
//int search_engine_no_overlap(addr, size);

// global variables
/* ===================================================================== */
/* Commandline Switches */
/* ===================================================================== */
KNOB<string> KnobOutputFile(KNOB_MODE_WRITEONCE, "pintool", "outfile",     "",       "Specify output file name");
KNOB<int>    KnobDebugLevel(KNOB_MODE_WRITEONCE, "pintool", "debug-level", "1",      "Only log debug output lower than level.\n\tlevel 0: standard non-debug output (default);\tlevel 1: output with source location info;\n\tlevel 2: output with application callstack;\n\tlevel 3: all debug info.");
KNOB<string> KnobDebugFile(KNOB_MODE_WRITEONCE,  "pintool", "debug-file",  "stdout", "Specify output file name for debug output");
KNOB<BOOL>   KnobPid(KNOB_MODE_WRITEONCE,        "pintool", "pid",         "0",      "Append PID to output files");

LOCALVAR int it_count = 0;                    // iteration counts, for debug only
LOCALVAR bool main_rtn_instrumented = FALSE;
LOCALVAR BOOL start_trace_callstack = FALSE;  // at which point shall we start tracing callstacks

// define the callstack
LOCALVAR std::vector<ADDRINT> callstack;
LOCALVAR int stack_depth = 0;

LOCALFUN VOID Handler(CONTROL_EVENT ev, VOID *, CONTEXT *, VOID *, THREADID tid)
{
    switch(ev)
    {
      case CONTROL_START:
        start_trace_callstack = TRUE;
        break;

      case CONTROL_STOP:
        start_trace_callstack = FALSE;
        break;

      default:
        ASSERTX(false);
    }
}

LOCALVAR CONTROL control;



// output streams
LOCALVAR ostream *Output;
LOCALVAR ostream *OutputDebug;


///////////////////////// Utility functions ///////////////////////////////////


VOID mempin_log(int debug_level, const char *format, ...)
{
#define MEMPIN_BUFFER_LEN 1024
    va_list arglist;
    char s[MEMPIN_BUFFER_LEN];

    va_start(arglist, format);
    vsnprintf(s, MEMPIN_BUFFER_LEN, format, arglist);
    va_end(arglist);

    if (0 == debug_level) {
        *Output      << "== <MemPin> [" << getpid_portable() << ":" << PIN_ThreadId() << "] == " << s << flush;
    } else if(debug_level <= KnobDebugLevel.Value()) {
        *OutputDebug << "== <MemPin Debug " << debug_level << "> [" << getpid_portable() << ":" << PIN_ThreadId() << "] == " << s << flush;
    }
}


/*
 * Search through the entry map,
 * returns the number of the matched entries.
 * Overlapping or duplicated entries are not allowed.
 */
int search_engine_no_overlap(size_t addr, int size)
{
    if(addr_map.size() == 0) {
        return 0;
    }

    // the first element of the map could be start, end address or entry id
    // the second element stores the whole entry
    multimap<size_t, addr_range_t>::iterator it, it_start, it_end;

    int found=0;

    result_map.clear();

    it_start = addr_map.lower_bound(addr);
    it_end = addr_map.upper_bound(addr+size);
    
    // extend one more element for searching
    if( it_start != addr_map.begin() ) {
        it_start--;
    }
    // don't touch the 'end' element
    if( it_end == addr_map.end() ) {
        it_end--;
    }

    for( ; ( (*it_start).first <= (*it_end).first ) && ( it_start != addr_map.end() ) ; it_start++ ){
        // insert the found items into the new map
        if( addr < (*it_start).second.addr+(*it_start).second.size &&
            addr+size > (*it_start).second.addr )  {
            result_map.insert(pair<int, addr_range_t>( (*it_start) ));
            found++;
        }
        it_count++;
    }

    mempin_log(3, "search_engine_no_overlap: number of iterations: %d\n", it_count);
    return found;
}


/*
 * Return the index of the matched entries.
 * The caller must allocate a size_t array in the applicatioin.
 */
VOID mempin_search_mem_index(ADDRINT ip, size_t addr, int size, size_t *index)
{
    unsigned int i;
    
    /* set the end flag */
    index[result_map.size()] = -1;
    
    if(addr_map.size() == 0) {
        return;
    }

    search_engine_no_overlap(addr, size);

    if(result_map.size() == 0 || index == NULL) {
        return;
    }

    multimap<size_t, addr_range_t>::iterator it = result_map.begin();
    for(i = 0; i < result_map.size(); i++ ) {
        index[i] = (*it).second.index;
        it++;
    }
}


VOID mempin_print_source(int debug_level, ADDRINT ip)
{
    INT32 column=0, line=0;
    string filename, rtn_name;

    PIN_LockClient();
    PIN_GetSourceLocation(ip, &column, &line, &filename);
    PIN_UnlockClient();

    rtn_name.append(RTN_FindNameByAddress(ip));
    mempin_log(debug_level, "<%s> %s:%d,%d\n", rtn_name.c_str(), filename.c_str(), line, column);
}


/*
 * print the current callstack of the application.
 * @params:
 *        ip:  the current instruction address
 *     depth:  the lines should be printed in the callstack
 *             0 means print all.
 */
VOID mempin_print_callstack(int debug_level, ADDRINT ip, int depth)
{
    if(depth == 0) {
        return;
    }

    if( debug_level <= KnobDebugLevel.Value() ) {

        callstack.push_back(ip);

        mempin_log (debug_level, "**** application callstack ****\n");
        vector<ADDRINT>::iterator it;
        it = callstack.end();
        it--;
        do {
            INT32 column=0, line=0;
            string filename, rtn_name;

            PIN_LockClient();
            PIN_GetSourceLocation(*it, &column, &line, &filename);
            PIN_UnlockClient();

            rtn_name.append(RTN_FindNameByAddress(*it));
            mempin_log(debug_level, "<%s> %s:%d,%d\n", rtn_name.c_str(), filename.c_str(), line, column);
            if( !strcmp(rtn_name.c_str(), "main") ) {
                break;
            }
        } while(it-- != callstack.begin() || !depth--);

        callstack.pop_back();
    } else {
        mempin_log (1, "**** application source ****\n");
        mempin_print_source(1, ip);
    }
}


VOID callstack_push(ADDRINT ip, ADDRINT sp)
{
    if(start_trace_callstack &&
       1 < KnobDebugLevel.Value()) {
        callstack.push_back(ip);
        stack_depth++;
    }
}


VOID callstack_pop()
{
    if(start_trace_callstack && callstack.size() > 0 &&
       1 <= KnobDebugLevel.Value()) {
        callstack.pop_back();
        stack_depth--;
    }
}


int mempin_mem_watch_count()
{
    int count = addr_map.size();
    mempin_log (1, "Number of watched memory: %d\n", count);
    return count;
}


/*
 * Register the memory entry.
 */
VOID mempin_reg_mem(ADDRINT ip, VOID* addr, size_t size, int op, MEMPIN_REG_CB_T cb_func, VOID* cb_args)
{
    assert(addr != NULL);
    assert(size > 0);
    assert(op >= 0);
    assert(cb_func != NULL);

    /* make sure no duplicated entries */
    if( search_engine_no_overlap((size_t)addr, size) > 0 ){
        mempin_log(1, "reg dup mem (ignored): %0x, %d, %d, %0x, %0x\n\n",
                   (size_t)addr, size, op, cb_func, cb_args);
        return;
    }

    size_t index = addr_map.size();

    mempin_log(1, "reg mem: %0x, %d, %d, %0x, %0x\n\n",
               (size_t)addr, size, op, cb_func, cb_args);

    mempin_print_callstack(2, ip, 100);

    addr_range_t entry = {index, (size_t)addr, size, op, cb_func, cb_args};
    addr_map.insert(pair<int, addr_range_t>(entry.addr, entry));

    mempin_log(1, "number of watched mem entries: %d\n\n", addr_map.size());
}


/*
 * Unregister the memory entry.
 */
VOID mempin_unreg_mem(ADDRINT ip, VOID* addr, int size)
{
    assert(addr != NULL);
    assert(size > 0);
    
    if(addr_map.size() == 0) {
        return;
    }

    mempin_log(1, "unreg memory: %0x, %d\n", (size_t)addr, size);

    mempin_print_callstack(2, ip, 100);

    if(addr_map.erase((size_t)addr) <= 0){
        mempin_log(1, "map erase error: %0x, %d\n", (size_t)addr, size);
    }

    mempin_log(1, "number of watched mem entries: %d\n\n", addr_map.size());
}


/*
 * Unregister all the memory entry.
 */
VOID mempin_unreg_mem_all(ADDRINT ip)
{
    if(addr_map.size() == 0) {
        return;
    }

    mempin_log(1, "unreg all memory\n");

    mempin_print_callstack(2, ip, 100);
    addr_map.clear();

    mempin_log(1, "number of watched mem entries: %d\n\n", addr_map.size());
}


/*
 * Check whether the address is in the map,
 * if yes, do the callback.
 */
VOID mem_cb(ADDRINT ip, VOID * addr, int size, int op)
{
    //mempin_log(1, "%0x\n",(size_t)addr);
    if( search_engine_no_overlap((size_t)addr, size) > 0 ){

        multimap<size_t, addr_range_t>::iterator it;
        size_t result_size, offset;

        for(it = result_map.begin(); it != result_map.end(); it++){
            // check if it's the operation that we are watching
            if( ((op+1) & ((*it).second.op+1)) > 0 ) {
               // mempin_log(1, "!!!! %d matched entry found from source: %0x,%d (%d) \n",
               //            result_map.size(), addr, size, op);
               // mempin_print_callstack(1, ip, 100);

                offset = (*it).second.addr - (size_t) addr;
                if( (size_t) addr <= (*it).second.addr ){
                    if ( (((size_t)addr+size) < ((*it).second.addr+(*it).second.size)) ){
                        result_size = size - offset; // or addr + size - (*it).second.addr
                    } else {
                        result_size = (*it).second.size;
                    }
                } else {
                    if ( ((size_t)addr+size) > ((*it).second.addr+(*it).second.size) ){
                        result_size = (*it).second.size + offset; // size - ( (a+size) - ((*it).second.addr+(*it).second.size) )
                    } else {
                        result_size = size;
                    }
                }

                // trigger the callback &  check for the alignment of the memory address
                int ret = (*((MEMPIN_REG_CB_T)((*it).second.cb_func)))( addr, result_size, offset, op, (*it).second.cb_args, (void*)ip );

                switch(ret) {
                case MEMPIN_CALLBACK_PRINT_CALLSTACK_1  :
                case MEMPIN_CALLBACK_PRINT_CALLSTACK_2  :
                case MEMPIN_CALLBACK_PRINT_CALLSTACK_3  :
                case MEMPIN_CALLBACK_PRINT_CALLSTACK_4  :
                case MEMPIN_CALLBACK_PRINT_CALLSTACK_5  :
                case MEMPIN_CALLBACK_PRINT_CALLSTACK_ALL:
                    mempin_print_callstack(1, ip, ret);
                    break;
                    /* More options could be added here. */
                default:
                    break;
                }
            }
        }
    }
}



/* tell the application that it's running with pin */
VOID mempin_running_with_pin(ADDRINT ip, int *pin_alive)
{
    mempin_log (3, "mempin_running_with_pin ip:%p pin_alive:%p\n", (void*) ip, pin_alive);
    *pin_alive = 1;
}

static BOOL IsPLT(TRACE trace)
{
    RTN rtn = TRACE_Rtn(trace);

    // All .plt thunks have a valid RTN
    if (!RTN_Valid(rtn))
        return FALSE;

    if (".plt" == SEC_Name(RTN_Sec(rtn)))
        return TRUE;
    return FALSE;
}


/*
 * The main trace routine.
 * Checks all memory reads and writes.
 * Determine where we should get a callstack item.
 */
static void mempin_trace(TRACE trace, void *v)
{
    for(BBL bbl = TRACE_BblHead(trace); BBL_Valid(bbl); bbl = BBL_Next(bbl)) {

        INS tail = BBL_InsTail(bbl);

        // All memory reads/writes
        for( INS ins = BBL_InsHead(bbl); INS_Valid(ins); ins = INS_Next(ins) ) {
            // instrument the instructions
            // Instruments memory accesses using a predicated call, i.e.
            // the instrumentation is called iff the instruction will actually be executed.
            //
            // The IA-64 architecture has explicitly predicated instructions.
            // On the IA-32 and Intel(R) 64 architectures conditional moves and REP
            // prefixed instructions appear as predicated instructions in Pin.
            UINT32 memOperands = INS_MemoryOperandCount(ins);

            // Iterate over each memory operand of the instruction.
            for (UINT32 memOp = 0; memOp < memOperands; memOp++)
            {
                if (INS_MemoryOperandIsRead(ins, memOp))
                {
                    int size = INS_MemoryReadSize(ins);
                    INS_InsertCall(
                                   ins, IPOINT_BEFORE, (AFUNPTR)mem_cb,
                                   IARG_INST_PTR,
                                   IARG_MEMORYOP_EA, memOp,
                                   IARG_UINT32, size,
                                   IARG_UINT32, MEMPIN_WATCH_READ,
                                   IARG_END);
                }
                // Note that in some architectures a single memory operand can be
                // both read and written (for instance incl (%eax) on IA-32)
                // In that case we instrument it once for read and once for write.
                if (INS_MemoryOperandIsWritten(ins, memOp))
                {
                    int size = INS_MemoryReadSize(ins);
                    INS_InsertCall(
                                   ins, IPOINT_BEFORE, (AFUNPTR)mem_cb,
                                   IARG_INST_PTR,
                                   IARG_MEMORYOP_EA, memOp,
                                   IARG_UINT32, size,
                                   IARG_UINT32, MEMPIN_WATCH_WRITE,
                                   IARG_END);
                }
            }
        }

        if (2 < KnobDebugLevel.Value()) {
            // All calls and returns
            if( !INS_IsSyscall(tail) ) {
                if( INS_IsCall(tail) ) {
                    if( INS_IsDirectBranchOrCall(tail) ) {
                        ADDRINT target = INS_DirectBranchOrCallTargetAddress(tail);
                        INS_InsertPredicatedCall(tail, IPOINT_BEFORE,
                                                 (AFUNPTR)callstack_push,
                                                 IARG_INST_PTR,
                                                 IARG_ADDRINT, target,
                                                 IARG_REG_VALUE, REG_STACK_PTR,
                                                 IARG_END);
                    } else if( !IsPLT(trace) ) {
                        INS_InsertPredicatedCall(tail, IPOINT_BEFORE,
                                                 (AFUNPTR)callstack_push,
                                                 IARG_INST_PTR,
                                                 IARG_BRANCH_TARGET_ADDR,
                                                 IARG_REG_VALUE, REG_STACK_PTR,
                                                 IARG_END);
                    }
                }
                if( IsPLT(trace) ) {
                    INS_InsertCall(tail, IPOINT_BEFORE,
                                   (AFUNPTR)callstack_push,
                                   IARG_INST_PTR,
                                   IARG_BRANCH_TARGET_ADDR,
                                   IARG_REG_VALUE, REG_STACK_PTR,
                                   IARG_END);
                }
                if( INS_IsRet(tail) ) {
                    INS_InsertCall(tail, IPOINT_BEFORE,
                                   (AFUNPTR)callstack_pop,
                                   IARG_INST_PTR,
                                   IARG_REG_VALUE, REG_STACK_PTR,
                                   IARG_END);
                }
            }
        }
    }
}


VOID mempin_main(ADDRINT ip, ADDRINT target, ADDRINT sp)
{
    start_trace_callstack = TRUE;
    callstack.clear();
}


static void
mempin_image_load(IMG img, void *v)
{
    RTN rtn;

    mempin_log (3, "mempin_image_load: IMG_Name: %s\n", IMG_Name(img).c_str());

    if( !main_rtn_instrumented && IMG_IsMainExecutable(img)) {
        rtn = RTN_FindByName(img, "main");
        if( rtn == RTN_Invalid() ) {
            rtn = RTN_FindByName(img, "_main");
        } else if( rtn == RTN_Invalid() ) {
            rtn = RTN_FindByName(img, "__libc_start_main@plt");
        } else if( rtn == RTN_Invalid() ) {
            rtn = RTN_FindByName(img, "__libc_start_main");
        } else if ( rtn == RTN_Invalid() ) {
            mempin_log (0, "Pin error in mempin_image_load\n");
            exit (-1);
        }

        mempin_log (3, "mempin_image_load: main found in main executable RTN_name:%s\n",
                    RTN_Name(rtn).c_str());
        main_rtn_instrumented = TRUE;
        RTN_Open(rtn);
        RTN_InsertCall(rtn, IPOINT_BEFORE,
                        (AFUNPTR)mempin_main,
                        IARG_INST_PTR,
                        IARG_ADDRINT, RTN_Address(rtn),
                        IARG_REG_VALUE, REG_STACK_PTR,
                        IARG_END);
        RTN_Close(rtn);
    }

    rtn = RTN_FindByName(img, "MEMPIN_REG_MEM_WATCH");
    if( rtn != RTN_Invalid() ) {
        RTN_Open(rtn);
        mempin_log (3, "mempin_image_load: MEMPIN_REG_MEM_WATCH found\n");

        RTN_InsertCall(rtn, IPOINT_BEFORE, (AFUNPTR)mempin_reg_mem,
                       IARG_INST_PTR,
                       IARG_FUNCARG_ENTRYPOINT_VALUE, 0,
                       IARG_FUNCARG_ENTRYPOINT_VALUE, 1,
                       IARG_FUNCARG_ENTRYPOINT_VALUE, 2,
                       IARG_FUNCARG_ENTRYPOINT_VALUE, 3,
                       IARG_FUNCARG_ENTRYPOINT_VALUE, 4,
                       IARG_END);
        RTN_Close(rtn);
    }

    rtn = RTN_FindByName(img, "MEMPIN_UNREG_MEM_WATCH");
    if( rtn != RTN_Invalid() ) {
        mempin_log (3, "mempin_image_load: MEMPIN_UNREG_MEM_WATCH found\n");
        RTN_Open(rtn);
        RTN_InsertCall(rtn, IPOINT_BEFORE, (AFUNPTR)mempin_unreg_mem,
                       IARG_INST_PTR,
                       IARG_FUNCARG_ENTRYPOINT_VALUE, 0,
                       IARG_FUNCARG_ENTRYPOINT_VALUE, 1,
                       IARG_END);
        RTN_Close(rtn);
    }

    rtn = RTN_FindByName(img, "MEMPIN_UNREG_ALL_MEM_WATCH");
    if( rtn != RTN_Invalid() ) {
        mempin_log (3, "mempin_image_load: MEMPIN_UNREG_ALL_MEM_WATCH found\n");
        RTN_Open(rtn);
        RTN_InsertCall(rtn, IPOINT_BEFORE, (AFUNPTR)mempin_unreg_mem_all,
                       IARG_INST_PTR,
                       IARG_END);
        RTN_Close(rtn);
    }

    rtn = RTN_FindByName(img, "MEMPIN_SEARCH_MEM_INDEX");
    if( rtn != RTN_Invalid() ) {
        mempin_log (3, "mempin_image_load: MEMPIN_SEARCH_MEM_INDEX found\n");
        RTN_Open(rtn);
        RTN_InsertCall(rtn, IPOINT_BEFORE, (AFUNPTR)mempin_search_mem_index,
                       IARG_INST_PTR,
                       IARG_FUNCARG_ENTRYPOINT_VALUE, 0,
                       IARG_FUNCARG_ENTRYPOINT_VALUE, 1,
                       IARG_FUNCARG_ENTRYPOINT_VALUE, 2,
                       IARG_END);
        RTN_Close(rtn);
    }

    rtn = RTN_FindByName(img, "MEMPIN_RUNNING_WITH_PIN");
    if( rtn != RTN_Invalid() ) {
        mempin_log (3, "mempin_image_load: MEMPIN_RUNNING_WITH_PIN found\n");
        RTN_Open(rtn);
        RTN_InsertCall(rtn, IPOINT_AFTER, (AFUNPTR)mempin_running_with_pin,
                       IARG_INST_PTR,
                       IARG_FUNCARG_ENTRYPOINT_VALUE, 0,
                       IARG_END);
        RTN_Close(rtn);
    }

    rtn = RTN_FindByName(img, "MEMPIN_MEM_WATCH_COUNT");
    if( rtn != RTN_Invalid() ) {
        mempin_log (3, "mempin_image_load: MEMPIN_MEM_WACH_COUNT found\n");
        RTN_Open(rtn);
        RTN_InsertCall(rtn, IPOINT_BEFORE, (AFUNPTR)mempin_mem_watch_count,
                       IARG_END);
        RTN_Close(rtn);
    }

    rtn = RTN_FindByName(img, "MEMPIN_PRINT_CALLSTACK");
    if( rtn != RTN_Invalid() ) {
        mempin_log (3, "mempin_image_load: MEMPIN_PRINT_CALLSTACK found\n");
        RTN_Open(rtn);
        RTN_InsertCall(rtn, IPOINT_BEFORE, (AFUNPTR)mempin_print_callstack,
                       IARG_BOOL, FALSE,
                       IARG_INST_PTR,
                       IARG_UINT32, 0,
                       IARG_END);
        RTN_Close(rtn);
    }

    rtn = RTN_FindByName(img, "MEMPIN_DEBUG_LOG");
    if( rtn != RTN_Invalid() ) {
        mempin_log (3, "mempin_image_load: MEMPIN_DEBUG_LOG found\n");
        RTN_Open(rtn);
        RTN_InsertCall(rtn, IPOINT_BEFORE, (AFUNPTR)mempin_log,
                       IARG_BOOL, FALSE,
                       IARG_INST_PTR,
                       IARG_UINT32, 0,
                       IARG_FUNCARG_ENTRYPOINT_VALUE, 1,
                       IARG_END);
        RTN_Close(rtn);
    }

    // more MEMPIN functions needs to be instrumented
    // e.g. MEMPIN_MEM_NOACCESS, MEMPIN_MEM_DEFINED etc.
}


/*
 * Finalize Pin
 */
VOID Fini(INT32 code, VOID *v)
{
    mempin_log(2, "Fini called");
}


/*
 * Print Help Message
 */
INT32 Usage()
{
    cerr <<
        "This pin tool implements a call-back mechanism to discover reads&writes to registered memory.\n"
        "\n";
    cerr << KNOB_BASE::StringKnobSummary();
    cerr << endl;
    return -1;
}


/*
 * Main
 */
int main(int argc, char * argv[])
{
    int ret;
    string filename;

    // Default settings
    Output = &std::cout;
    OutputDebug = Output;

    // Initialize pin
    ret = PIN_Init(argc, argv);
    if (ret) {
        return Usage();
    }
    PIN_InitSymbols();

    // process command line args
    control.CheckKnobs(Handler, 0);

    filename = KnobOutputFile.Value();
    if (0 != filename.length()) {
        if( KnobPid.Value() ) {
            filename += "." + decstr( getpid_portable() );
        }

        Output = new ofstream (filename.c_str());
    }

    if (0 < KnobDebugLevel.Value()) {
        filename = KnobDebugFile.Value();
        if (0 != filename.length()) {
            if (KnobPid.Value()) {
                filename += "." + decstr( getpid_portable() );
            }

            OutputDebug = new ofstream (filename.c_str());
        }
    }

    IMG_AddInstrumentFunction(mempin_image_load, 0);
    TRACE_AddInstrumentFunction(mempin_trace, 0);

    // Register Fini to be called when the application exits
    PIN_AddFiniFunction(Fini, 0);

    // Start the program, never returns
    PIN_StartProgram();

    return 0;
}
