/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2010, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#ifndef _VT_DYN_H_
#define _VT_DYN_H_

#define STRBUFSIZE 1024

#define DGOUT(str) \
   if( Params.beverbose ) \
      std::cout << ExeName << ": [" << mutatorPid << "]: " << str << std::endl;

// macro to remove newline character from string
#define chomp(str) { \
  if( str[strlen(str)-1] == '\n' ) \
    str[strlen(str)-1] = '\0'; }

// macro to strip whitespace from string
#define trim(str) { \
  int _trim_start_idx_ = 0; \
  int _trim_stop_idx_ = strlen( str ); \
  int i, j; \
  if( strlen( str ) > 0 ) { \
    for( i = 0; i < (int)strlen( str ) \
         && str[i] == ' '; i++ ) _trim_start_idx_++; \
    for( i = (int)strlen( str ) - 1; i >= 0 \
         && str[i] == ' '; i-- ) _trim_stop_idx_--; \
    for( j = 0, i = _trim_start_idx_; i < _trim_stop_idx_; i++, j++ ) \
      str[j] = str[i]; \
    str[j] = '\0'; } } 

class Mutator
{
   struct InstFunc
   {
      InstFunc() : p_func(0), addr(0), name(""), file(""), lno(0) {}
      InstFunc(BPatch_function * _p_func, unsigned long _addr,
	       std::string _name, std::string _file, uint32_t _lno )
	 : p_func(_p_func), addr(_addr), name(_name), file(_file), lno(_lno) {}

      BPatch_function * p_func;
      unsigned long addr;
      std::string name;
      std::string file;
      uint32_t lno;
   };

public:
   
   Mutator();    // contructor
   ~Mutator();   // destructor

   bool run();

private:

   bool initialize();
   bool readFunctionBL();
   bool checkFunctionBL( std::string name );
   bool getFunctions( std::vector<struct InstFunc*> * p_vecInstFuncs );
   bool constraintModule( std::string name );
   bool constraintFunction( std::string name );
   bool isMPI();
   bool findFunction( std::string name, BPatch_function ** p_function );
   bool instrumentFunction( struct InstFunc * p_instFunc );
   bool insertFunctionCall( BPatch_function * p_function,
			    BPatch_procedureLocation loc,
			    BPatch_function * p_callee,
			    BPatch_Vector<BPatch_snippet *> * p_callee_args );

   BPatch_process * m_pAppProcess;
   BPatch_image * m_pAppImage;

   BPatch_function * m_pVTDynStartFunc;
   BPatch_function * m_pVTDynEndFunc;

   std::vector<std::string> m_vecBlacklist;
};

extern Mutator * theMutator;
extern BPatch theBpatch;
extern int mutatorPid;

#endif // _VT_DYN_H_
