//
// $HEADER$
//

#include "ompi_config.h"
#include "util/show_help.h"
#include "tools/wrappers/ompi_wrap.h"

#include <iostream>
using namespace std;


int
main(int argc, char *argv[])
{
    // The four wrapper compilers are extremely similar.  So similar,
    // in fact, that they can be parameterized on what is different.
    // Hence, we call the "wrapper compiler engine" to do all the
    // work, and pass in just a few arguments to customize for the
    // language of this wrapper compiler.

#if !OMPI_WANT_F77_BINDINGS
    ompi_show_help("help-wrapper.txt", "no-fortran-support", true,
		   77, "mpif77");
    return 1;
#else
    ompi_sv_t compiler;
    ompi_sv_t fppflags;
    ompi_sv_t fflags;
    ompi_sv_t ldflags;
    ompi_sv_t libs;

    compiler.clear();
    compiler.push_back("OMPI_MPIF77");

    fppflags.clear();
    fppflags.push_back("OMPI_MPIF77_FPPFLAGS");
    fppflags.push_back("OMPI_FPPFLAGS");

    fflags.clear();
    fflags.push_back("OMPI_MPIF77_FFLAGS");
    fflags.push_back("OMPI_FFLAGS");

    ldflags.clear();
    ldflags.push_back("OMPI_MPIF77_LDFLAGS");
    ldflags.push_back("OMPI_LDFLAGS");

    libs.clear();
    libs.push_back("OMPI_MPIF77_LIBS");
    libs.push_back("OMPI_LIBS");

    return ompi_wrap_engine(argc, argv, compiler, fppflags, fflags,
			    ldflags, libs, OMPI_F77, WRAPPER_EXTRA_FFLAGS,
			    false, true);
#endif
}
