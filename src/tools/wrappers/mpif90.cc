//
// Copyright (c) 2004-2005 The Trustees of Indiana University.
//                         All rights reserved.
// Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
//                         All rights reserved.
// Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
//                         University of Stuttgart.  All rights reserved.
// Copyright (c) 2004-2005 The Regents of the University of California.
//                         All rights reserved.
// $COPYRIGHT$
// 
// Additional copyrights may follow
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

#if !OMPI_WANT_F90_BINDINGS
    ompi_show_help("help-wrapper.txt", "no-fortran-support", true,
		   90, "mpif90");
    return 1;
#else
    ompi_sv_t compiler;
    ompi_sv_t fcppflags;
    ompi_sv_t fcflags;
    ompi_sv_t ldflags;
    ompi_sv_t libs;
    string wrapper_extra_fcflags;

    compiler.clear();
    compiler.push_back("OMPI_MPIF90");

    fcppflags.clear();
    fcppflags.push_back("OMPI_MPIF90_FCPPFLAGS");
    fcppflags.push_back("OMPI_FCPPFLAGS");

    fcflags.clear();
    fcflags.push_back("OMPI_MPIF90_FCFLAGS");
    fcflags.push_back("OMPI_FCFLAGS");

    ldflags.clear();
    ldflags.push_back("OMPI_MPIF90_LDFLAGS");
    ldflags.push_back("OMPI_LDFLAGS");

    libs.clear();
    libs.push_back("OMPI_MPIF90_LIBS");
    libs.push_back("OMPI_LIBS");

    wrapper_extra_fcflags = OMPI_FC_MODULE_FLAG;
    wrapper_extra_fcflags += " ";
    wrapper_extra_fcflags += OMPI_LIBDIR;
    wrapper_extra_fcflags += " ";
    wrapper_extra_fcflags += WRAPPER_EXTRA_FCFLAGS;

    return ompi_wrap_engine(argc, argv, compiler, fcflags, fcflags,
			    ldflags, libs, OMPI_F90, wrapper_extra_fcflags,
			    false, true, true);
#endif
}
