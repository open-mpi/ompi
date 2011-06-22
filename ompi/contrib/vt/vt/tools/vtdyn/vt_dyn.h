/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2011, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#ifndef _VT_DYN_H_
#define _VT_DYN_H_

#include "config.h"

#include "vt_inttypes.h"

#include "rfg_filter.h"

#include "BPatch.h"
#include "BPatch_addressSpace.h"
#include "BPatch_function.h"
#include "BPatch_image.h"

#include <iostream>
#include <set>
#include <string>
#include <vector>

#define STRBUFSIZE 1024

//
// mutation modes
//
typedef enum
{
   // either create/attach to a process, instrument, and execute
   //
   MODE_CREATE,
   MODE_ATTACH,
   // or open, instrument, and rewrite binary
   MODE_REWRITE

} MutationT;

//
// structure that contains the mutator parameters
// (i.e. command line options)
//
struct ParamsS
{
   ParamsS()
      : mode(MODE_CREATE), mutatee_pid(-1), verbose_level(1),
        ignore_no_dbg(false), show_usage(false), show_version(false) {}

   MutationT                mode;          // mutation mode
   std::string              mutatee;       // mutatee executable name
   int                      mutatee_pid;   // mutatee PID
   std::vector<std::string> mutatee_args;  // mutatee arguments
   std::vector<std::string> shlibs;        // shared libs. to be instrumented
   std::string              filtfile;      // pathname of filter file
   std::string              outfile;       // file name of binary to rewrite
   uint32_t                 verbose_level; // verbose level
   bool                     ignore_no_dbg; // flag: ignore funcs. without debug?
   bool                     show_usage;    // flag: show usage text?
   bool                     show_version;  // flag: show VampirTrace version?

};

//
// MutatorC class
//
class MutatorC
{
public:

   // constructor
   MutatorC();

   // destructor
   ~MutatorC();

   // run the mutator
   bool run();

private:

   //
   // structure that contains context information about functions to
   // be instrumented
   //
   struct InstFuncS
   {
      InstFuncS() : func(0), addr(0), lno(0) {}
      InstFuncS(BPatch_function * _func, unsigned long _addr,
                std::string _name, std::string _file, uint32_t _lno )
         : func(_func), addr(_addr), name(_name), file(_file), lno(_lno) {}

      BPatch_function * func;   // BPatch function object
      unsigned long addr;       // function address
      std::string name;         // function name
      std::string file;         // source file name of function definition
      uint32_t lno;             // line number of function definition

   };

   // create/attach to a process or open binary for rewriting
   bool initialize();

   // continue execution of mutatee or rewrite binary
   bool finalize( bool & error );

   // get functions to be instrumented
   bool getFunctions( std::vector<InstFuncS> & instFuncs );

   // instrument a function
   bool instrumentFunction( const InstFuncS & instFunc );

   // read input filter file
   bool readFilter();

   // check whether module is excluded from instrumentation
   inline bool constraintModule( const std::string & name ) const;

   // check whether function is excluded from instrumentation
   inline bool constraintFunction( const std::string & name ) const;

   // check whether mutatee uses MPI
   inline bool isMPI() const;

   // find certain function in mutatee
   inline bool findFunction( const std::string & name,
                             BPatch_function *& func ) const;

   // entire Dyninst library object
   BPatch m_bpatch;

   // mutatee's process or binary edit object
   BPatch_addressSpace * m_appAddrSpace;

   // mutatee's image object
   BPatch_image * m_appImage;

   // instrumentation functions to be inserted at entry/exit points
   //
   BPatch_function * m_vtStartFunc;
   BPatch_function * m_vtEndFunc;

   // RFG filter object to include/exclude functions from instrumenting
   RFG_Filter * m_filter;

};

#endif // _VT_DYN_H_
