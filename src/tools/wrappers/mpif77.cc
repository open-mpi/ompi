//
// $HEADER$
//	Function:	- wrapper for fortran program compilation
//

#include "ompi_config.h"
#include "tools/wrappers/ompi_wrap.h"



int
main(int argc, char *argv[])
{
  // The four wrapper compilers are extremely similar.  So similar,
  // in fact, that they can be parameterized on what is different.
  // Hence, we call the "wrapper compiler engine" to do all the work,
  // and pass in just a few arguments to customize for the language of
  // this wrapper compiler.

#if !OMPI_WANT_F77_BINDINGS
#if 0
  show_help("hf77", "no-fortran-support", NULL);
#endif
  return 1;
#else
  ompi_sv_t str_vec;

  str_vec.clear();
  str_vec.push_back("OMPI_MPIF77");
  str_vec.push_back("OMPI_F77");

  return ompi_wrap_engine(argc, argv,
			 str_vec, OMPI_F77, false, true,
			 WRAPPER_EXTRA_FFLAGS);
#endif
}
