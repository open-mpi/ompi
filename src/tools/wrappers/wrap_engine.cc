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

#include <iostream>
#include "ompi_config.h"
#include "tools/wrappers/ompi_wrap.h"

using namespace std;

///
/// Back-end entry point for the wrapper compiler functionality.  All
/// three wrapper compilers invoke this function.
///
/// \param argc Number of command line arguments
/// \param argv Vector containing command line arguments
/// \param env_vars Environment variables
/// \param default_compiler The default backend compiler
/// \param want_cxx_libs Whether C++ libraries should be linked in
/// \param want_f77_includes Whether F77 include path should be added
/// \param extra_args Extra arguments which need to be added
///
/// At most, we end up with a command line in the following form:
///
/// [compiler(*)] [Xppflags(*)] [Xflags(*)] [user args] [ldflags(*)] [libs(*)]
///
/// Items with (*) can be overridden at run time with environment variables.
///
/// X can be replaced with C, CXX, F, and FC for C, C++, Fortran 77,
/// and Fortran 90, respectively.
///
int
ompi_wrap_engine(int argc, char *argv[],
		 const ompi_sv_t & compiler_env_var,
		 const ompi_sv_t & xppflags_env_var,
		 const ompi_sv_t & xflags_env_var,
		 const ompi_sv_t & ldflags_env_var,
		 const ompi_sv_t & libs_env_var,
		 const std::string & default_compiler,
		 const std::string & default_xflags,
		 bool want_cxx_libs, bool want_f77_includes)
{
    int ret(0);

    bool want_flags;

    ompi_sv_t str_vec;

    ompi_sv_t compiler;
    ompi_sv_t xppflags;
    ompi_sv_t xflags;
    ompi_sv_t user_args;
    ompi_sv_t ldflags;
    ompi_sv_t libs;
    ompi_sv_t extra_flags;

    ompi_sv_t cmd_line;

    // Parse command line

    ompi_wrap_parse_args(argc, argv, want_flags);

    // Get the compiler

    ompi_wrap_get_compiler(compiler_env_var, default_compiler, compiler);

    // Build the XPPFLAGS (-I stuff only)

    ompi_wrap_build_xppflags(xppflags_env_var, want_f77_includes,
			     xppflags);

    // Build any XFLAGS

    ompi_wrap_build_xflags(xflags_env_var, default_xflags, xflags);

    // Build the user arguments

    ompi_wrap_build_user_args(argc, argv, user_args);

    // Build the LDFLAGS (-L stuff and WRAPPER_EXTRA_LDFLAGS)

    ompi_wrap_build_ldflags(ldflags_env_var, ldflags);

    // Build the LIBS (-l stuff and WRAPPER_EXTRA_LIBS)

    ompi_wrap_build_libs(libs_env_var, want_cxx_libs, libs);

    // Now assemble the command line

    cmd_line.clear();
    ompi_wrap_append_sv(compiler, cmd_line);
    if (want_flags) {
	ompi_wrap_append_sv(xppflags, cmd_line);
	ompi_wrap_append_sv(xflags, cmd_line);
    }
    ompi_wrap_append_sv(user_args, cmd_line);
    if (want_flags) {
	ompi_wrap_append_sv(ldflags, cmd_line);
	ompi_wrap_append_sv(libs, cmd_line);
    }
    // Display or execute?

    if (showme_cmd) {
	ompi_wrap_print_sv(compiler);
    }
    if (showme_cmd || showme_compile) {
	ompi_wrap_print_sv(xppflags);
	ompi_wrap_print_sv(xflags);
    }
    if (showme_cmd || showme_compile || showme_link)
	ompi_wrap_print_sv(user_args);
    if (showme_cmd || showme_link) {
	ompi_wrap_print_sv(ldflags);
	ompi_wrap_print_sv(libs);
    }
    if (showme_cmd || showme_compile || showme_link) {
	cout << endl;
    } else {
	ret = ompi_wrap_exec_sv(cmd_line);
    }

    // That's all she wrote

    return ret;
}
