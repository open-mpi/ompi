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
#include "tools/wrappers/ompi_wrap.h"


int
main(int argc, char *argv[])
{
    // The four wrapper compilers are extremely similar.  So similar,
    // in fact, that they can be parameterized on what is different.
    // Hence, we call the "wrapper compiler engine" to do all the
    // work, and pass in just a few arguments to customize for the
    // language of this wrapper compiler.

    ompi_sv_t compiler;
    ompi_sv_t cppflags;
    ompi_sv_t cflags;
    ompi_sv_t ldflags;
    ompi_sv_t libs;

    compiler.clear();
    compiler.push_back("OMPI_MPICC");

    cppflags.clear();
    cppflags.push_back("OMPI_MPICC_CPPFLAGS");
    cppflags.push_back("OMPI_CPPFLAGS");

    cflags.clear();
    cflags.push_back("OMPI_MPICC_CFLAGS");
    cflags.push_back("OMPI_CFLAGS");

    ldflags.clear();
    ldflags.push_back("OMPI_MPICC_LDFLAGS");
    ldflags.push_back("OMPI_LDFLAGS");

    libs.clear();
    libs.push_back("OMPI_MPICC_LIBS");
    libs.push_back("OMPI_LIBS");

    return ompi_wrap_engine(argc, argv, compiler, cppflags, cflags,
			    ldflags, libs, OMPI_CC, WRAPPER_EXTRA_CFLAGS,
			    false, false);
}
