//
// $HEADER$
//	Function:	- wrapper for C++ program compilation
//

#include "lam_config.h"
#include "tools/wrappers/lamwrap.h"


int
main(int argc, char *argv[])
{
  // The four wrapper compilers are extremely similar.  So similar,
  // in fact, that they can be parameterized on what is different.
  // Hence, we call the "wrapper compiler engine" to do all the work,
  // and pass in just a few arguments to customize for the language of
  // this wrapper compiler.

  lam_sv_t str_vec;

  str_vec.clear();
  str_vec.push_back("LAMMPICXX");
  str_vec.push_back("LAMCXX");

  return lam_wrap_engine(argc, argv,
			 str_vec, LAM_CXX,true, false, 
			 WRAPPER_EXTRA_CXXFLAGS);
}
