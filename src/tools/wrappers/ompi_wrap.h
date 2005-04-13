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
/// @file
///
/// All of these types, global variables, and functions are internal
/// to the ompi_info program.  There is probably noting here of value
/// outside of ompi_info.
///

#ifndef OMPI_WRAP_H
#define OMPI_WRAP_H 1

#include "ompi_config.h"

#include <string>
#include <vector>

///
/// Commonly used type
///
typedef std::vector <std::string> ompi_sv_t;


//
// Global variables
//
/// Whether we're showing the entire command or not
extern bool showme_cmd;
/// Whether we're showing the compile command or not
extern bool showme_compile;
/// Whether we're showing the link command or not
extern bool showme_link;


///
/// Functions in the helper OMPI wrapper compiler library; 
/// this is the granddaddy that will invoke the rest
///
/// \param argc argc from main()
/// \param argv argv from main()
/// \param compiler_env_var List of environment variables for the compiler
/// \param xppflags_env_var List of environment variables for
/// preprocessor flags
/// \param xflags_env_var List of environment variables for compiler flags
/// \param ldflags_env_var List of environment variables for linker flags
/// \param libs_env_var List of environment variables for linker flags
/// \param default_compiler The compiler chosen by configure
/// \param default_xflags Flags chosen by configure
/// \param want_cxx_libs Do we want to link in the C++ libraries?
/// \param want_f77_inclues Do we want to force the f77 includes?
///
int ompi_wrap_engine(int argc, char *argv[],
                     const ompi_sv_t & compiler_env_var,
                     const ompi_sv_t & xppflags_env_var,
                     const ompi_sv_t & xflags_env_var,
                     const ompi_sv_t & ldflags_env_var,
                     const ompi_sv_t & libs_env_var,
                     const std::string & default_compiler,
                     const std::string & default_xflags,
                     bool want_cxx_libs, 
                     bool want_f90_libs,
                     bool want_f77_includes);

///
/// Parse the command line arguments of the wrapper compiler
///
/// \param argc Number of command line parameters
/// \param argv Vector containing command line parameters
/// \param want_flags True if extra flags are needed
///
void ompi_wrap_parse_args(int argc, char *argv[], bool & want_flags);

///
/// Figure out what the back-end compiler is (even if it's multiple
/// tokens)
///
/// \param env_list List of environment variables passed by the user
/// \param default_comp Defaultl backend compiler
/// \param out Compiler to use (return value)
///
void ompi_wrap_get_compiler(const ompi_sv_t & env_list,
                            const std::string & default_comp, ompi_sv_t & out);

///
/// Build up a list of arguments for XPPFLAGS (CPPFLAGS, CXXCPPFLAGS
/// or FPPFFLAGS, depending on what the front-end wrapper compiler
/// is).
///
/// \param want_f77_includes If F77 includes are wanted
/// \param cflags Vector of strings containing the cflags (returned)
///
void ompi_wrap_build_xppflags(const ompi_sv_t & env_list,
                              bool want_f77_includes, ompi_sv_t & xppflags);

///
/// Build of a list of XFLAGS (CFLAGS, CXXFLAGS, FFLAGS, FCFLAGS) to
/// go to the back-end compiler.  These are typically extra flags that
/// come from the configure script.
///
/// \param extra_string Extra flags to be passed to backend compiler
/// \param extra_flags Vector of strings to be pased to backend 
///        compiler (return value)
///
void ompi_wrap_build_xflags(const ompi_sv_t & env_list,
                            const std::string & default_xflags,
                            ompi_sv_t & xflags);

///
/// Build up a list of user arguments (from argc/argv) that will be
/// plugged into the command line that will invoke the back-end
/// compiler.
///
/// \param argc Number of command line parameters
/// \param argv Vector containing the command line parameters
/// \param user_args List of user arguments that will be passed to 
///        the back-end compiler (return value)
///
void ompi_wrap_build_user_args(int argc, char *argv[], ompi_sv_t & user_args);

///
/// Build up a list of LDFLAGS that will be given to the back-end
/// compiler.
///
/// \param ldflags Vector of strings comtaining the LDFLAGS
///
void ompi_wrap_build_ldflags(const ompi_sv_t & env_list, ompi_sv_t & ldflags);

///
/// Build up a list of LIBS that will be given to the back-end
/// compiler.
///
/// \param want_cxx_libs Are we building mpiCC?
/// \param libs Vector of strings containing the libraries to be
///             linked to the backend compiler
///
void ompi_wrap_build_libs(const ompi_sv_t & env_list,
                          bool want_cxx_libs, bool want_f90_libs,
                          ompi_sv_t & libs);

///
/// Print out a vector of strings
///
/// \param sv Vector of strings to be printed out
///
void ompi_wrap_print_sv(const ompi_sv_t & sv);

///
/// Execute a vector of strings (ultimately results down to a call to
/// some flavor of exec()).
///
/// \param sv Vector of strings to be exec()'ed
///
int ompi_wrap_exec_sv(const ompi_sv_t & sv);

///
/// Remove leading and trailing white space from a given string.
/// Must be sent a null-terminated string.
///
/// \param str String from which leading and trailing spaces should
///            be removed
///
void ompi_wrap_strip_white(std::string & str);

///
/// Split a string into a vector of strings
//
/// \param str String which has to be split into a vector of strings
/// \param c Charecter which demarkates 2 strings
/// \param out Vector of strings (return value)
///
bool ompi_wrap_split(const std::string & str, char c, ompi_sv_t & out);

///
/// Take a string, split it into tokens, and append it to an existing
/// vector of strings
///
/// \param str String which is to be split into tokens
/// \param out Vector of strings to which the tokens are appended
///
void ompi_wrap_split_append_sv(const std::string & str, ompi_sv_t & out);

///
/// Append one vector of strings onto the end of another.
///
/// \param in Vector of strings to be appended
/// \param out Vector of strings to which "in" will be appended
///
void ompi_wrap_append_sv(const ompi_sv_t & in, ompi_sv_t & out);

///
/// Check for the presence of a file
///
/// \param dir Directory in which the file should be present
/// \param file Name of the file
///
bool ompi_wrap_check_file(const std::string & dir, const std::string & file);

#endif // OMPI_WRAP_H
