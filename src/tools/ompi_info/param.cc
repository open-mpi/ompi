//
// Copyright (c) 2004-2005 The Trustees of Indiana University.
//                         All rights reserved.
// Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
//                         All rights reserved.
// Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
//                         University of Stuttgart.  All rights reserved.
// $COPYRIGHT$
// 
// Additional copyrights may follow
// 
// $HEADER$
//

#include "ompi_config.h"

#include <iostream>
#include <string>
#include <map>

#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#if HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif

#include "class/ompi_value_array.h"
#include "util/printf.h"
#include "mca/base/mca_base_param.h"
#include "tools/ompi_info/ompi_info.h"

using namespace std;
using namespace ompi_info;


//
// Public variables
//

string ompi_info::component_all = "all";
string ompi_info::param_all = "all";

string ompi_info::path_prefix = "prefix";
string ompi_info::path_bindir = "bindir";
string ompi_info::path_libdir = "libdir";
string ompi_info::path_incdir = "incdir";
string ompi_info::path_pkglibdir = "pkglibdir";
string ompi_info::path_sysconfdir = "sysconfdir";

//
// External variables
//
// This exists in mca/base/mca_base_param.c.  It's not extern'ed
// in mca_base_param.h so that no one else will use it.
//

extern ompi_value_array_t mca_base_params;


void ompi_info::do_params(bool want_all, bool want_internal)
{
  unsigned int count;
  string type, component;
  bool found;
  ompi_info::type_vector_t::size_type i;

  ompi_info::open_components();

  // See if the special param "all" was givin to --param; that
  // superceeds any individual type

  count = ompi_cmd_line_get_ninsts(cmd_line, "param");
  for (i = 0; i < count; ++i) {
    type = ompi_cmd_line_get_param(cmd_line, "param", i, 0);
    if (type_all == type) {
      want_all = true;
      break;
    }
  }

  // Show the params

  if (want_all) {
    for (i = 0; i < mca_types.size(); ++i) {
      show_mca_params(mca_types[i], component_all, param_all, want_internal);
    }
  } else {
    for (i = 0; i < count; ++i) {
      type = ompi_cmd_line_get_param(cmd_line, "param", i, 0);
      component = ompi_cmd_line_get_param(cmd_line, "param", i, 1);

      for (found = false, i = 0; i < mca_types.size(); ++i) {
        if (mca_types[i] == type) {
          found = true;
          break;
        }
      }

      if (!found) {
#if 0
        show_help("ompi_info", "usage");
#endif
        exit(1);
      }

      show_mca_params(type, component, param_all, want_internal);
    }
  }
}


void ompi_info::show_mca_params(const string& type, const string& component, 
                                const string& param, bool want_internal)
{
    ompi_list_t *info;
    ompi_list_item_t *i;
    mca_base_param_info_t *p;
    char *value_string, empty[] = "\0";
    string message, content;
    int value_int;

    mca_base_param_dump(&info, want_internal);
    for (i = ompi_list_get_first(info); i != ompi_list_get_last(info);
         i = ompi_list_get_next(i)) {
        p = (mca_base_param_info_t*) i;
        
        if (type == p->mbpp_type_name) {
            if (component == component_all || 
                NULL == p->mbpp_component_name ||
                (NULL != p->mbpp_component_name &&
                 component == p->mbpp_component_name)) {
                if (param == param_all || param == p->mbpp_param_name) {

                    // Make a string for the default value.  Invoke a
                    // lookup because it may transform the string
                    // ("~/" -> "<home dir>/") or get the value from
                    // the environment, a file, etc.

                    if (MCA_BASE_PARAM_TYPE_STRING == p->mbpp_type) {
                        mca_base_param_lookup_string(p->mbpp_index,
                                                     &value_string);

                        // Can't let the string be NULL because we
                        // assign it to a std::string, below

                        if (NULL == value_string) {
                            value_string = empty;
                        }
                    } else {
                        mca_base_param_lookup_int(p->mbpp_index, &value_int);
                        asprintf(&value_string, "%d", value_int);
                    }
                    content = value_string;
                    
                    // Build up the strings to output.
                    
                    if (pretty) {
                        message = "MCA ";
                        message += p->mbpp_type_name;
                        
                        // Put in the real, full name (which may be
                        // different than the categorization).
                        
                        content = (p->mbpp_env_var_name != NULL) ?
                            "parameter \"" : "information \"";
                        content += p->mbpp_full_name;
                        content += (p->mbpp_env_var_name != NULL) ?
                            "\" (default: " : "\" (value: ";
                        
                        if (strlen(value_string) == 0)
                            content += "<none>)";
                        else {
                            content += "\"";
                            content += value_string;
                            content += "\")";
                        }
                        
                        out(message, message, content);
                    } else {
                        message = "mca:";
                        message += p->mbpp_type_name;
                        message += ":";
                        
                        if (p->mbpp_component_name != NULL) {
                            message += p->mbpp_component_name;
                        } else {
                            message += "base";
                        }
                        message += (p->mbpp_env_var_name != NULL) ?
                            ":param:" : ":info:";
                        
                        // Put in the real, full name (which may be
                        // different than the categorization).
                        
                        message += p->mbpp_full_name;
                        
                        content = value_string;
                        
                        out(message, message, content);
                    }
                    
                    // If we allocated the string, then free it
                    
                    if (value_string != empty) {
                        free(value_string);
                    }
                }
            }
        }
    }

    /* Release the memory */

    mca_base_param_dump_release(info);
}


void ompi_info::do_path(bool want_all, ompi_cmd_line_t *cmd_line)
{
  int i, count;
  string scope;

  if (want_all) {
    show_path(path_prefix, OMPI_PREFIX);
    show_path(path_bindir, OMPI_BINDIR);
    show_path(path_libdir, OMPI_LIBDIR);
    show_path(path_incdir, OMPI_INCDIR);
    show_path(path_pkglibdir, OMPI_PKGLIBDIR);
    show_path(path_sysconfdir, OMPI_SYSCONFDIR);
  } else {
    count = ompi_cmd_line_get_ninsts(cmd_line, "path");
    for (i = 0; i < count; ++i) {
      scope = ompi_cmd_line_get_param(cmd_line, "path", i, 0);

      if (path_prefix == scope)
        show_path(path_prefix, OMPI_PREFIX);
      else if (path_bindir == scope)
        show_path(path_bindir, OMPI_BINDIR);
      else if (path_libdir == scope)
        show_path(path_libdir, OMPI_LIBDIR);
      else if (path_incdir == scope)
        show_path(path_incdir, OMPI_INCDIR);
      else if (path_pkglibdir == scope)
        show_path(path_pkglibdir, OMPI_PKGLIBDIR);
      else if (path_sysconfdir == scope)
        show_path(path_sysconfdir, OMPI_SYSCONFDIR);
      else {
#if 0
        show_help("ompi_info", "usage");
#endif
        exit(1);
      }
    }
  }
}


void ompi_info::show_path(const string& type, const string& value)
{
  string pretty(type);
  pretty[0] &= toupper(pretty[0]);

  out(pretty, "path:" + type, value);
}


void ompi_info::do_arch(ompi_cmd_line_t *cmd_line)
{
  string prefix;
  char hostname[MAXHOSTNAMELEN];

  if (ompi_cmd_line_is_taken(cmd_line, "hostname")) {
    gethostname(hostname, MAXHOSTNAMELEN);
    prefix = hostname + string(":");
  }
  out("Configured architecture", prefix + "config:arch", OMPI_ARCH);
}


//
// do_config
// Accepts:
//	- want_all: boolean flag; TRUE -> display all options
//				  FALSE -> display selected options
//
// This function displays all the options with which the current
// installation of ompi was configured. There are many options here 
// that are carried forward from OMPI-7 and are not mca parameters 
// in OMPI-10. I have to dig through the invalid options and replace
// them with OMPI-10 options.
//
void ompi_info::do_config(bool want_all)
{
  const string f77(OMPI_WANT_F77_BINDINGS ? string("yes (") +
                   (OMPI_HAVE_WEAK_SYMBOLS ? "all" :
                    (OMPI_F77_CAPS ? "caps" :
                     (OMPI_F77_PLAIN ? "lower case" :
                      (OMPI_F77_SINGLE_UNDERSCORE ? "single underscore" : 
                       "double underscore")))) + string(")"): "no");
  const string f90(OMPI_WANT_F90_BINDINGS ? "yes" : "no");
  const string threads(OMPI_HAVE_SOLARIS_THREADS ? "solaris" :
                       (OMPI_HAVE_POSIX_THREADS ? "posix" : "no"));
  const string memprofile(OMPI_ENABLE_MEM_PROFILE ? "yes" : "no");
  const string memdebug(OMPI_ENABLE_MEM_DEBUG ? "yes" : "no");
  const string debug(OMPI_ENABLE_DEBUG ? "yes" : "no");
  const string cprofiling(OMPI_ENABLE_MPI_PROFILING ? "yes" : "no");
  const string cxxprofiling(OMPI_ENABLE_MPI_PROFILING ? "yes" : "no");
  const string f77profiling((OMPI_ENABLE_MPI_PROFILING && 
                             OMPI_WANT_F77_BINDINGS) ? "yes" : "no");
  const string f90profiling((OMPI_ENABLE_MPI_PROFILING && OMPI_WANT_F90_BINDINGS) ?
                            "yes" : "no");
  const string cxxexceptions(OMPI_HAVE_CXX_EXCEPTION_SUPPORT ? "yes" : "no");
  int ompi_mpi_param_check = 3;
  const string paramcheck(0 == MPI_PARAM_CHECK ? "never" :
                          1 == MPI_PARAM_CHECK ? "always" : "runtime");

  out("Configured by", "config:user", OMPI_CONFIGURE_USER);
  out("Configured on", "config:timestamp", OMPI_CONFIGURE_DATE);
  out("Configure host", "config:host", OMPI_CONFIGURE_HOST);

  out("C bindings", "bindings:c", "yes");
  out("C++ bindings", "bindings:cxx", "yes");
  out("Fortran77 bindings", "bindings:f77", f77);
  out("Fortran90 bindings", "bindings:f90", f90);

  out("C compiler", "compiler:c:command", OMPI_CC);

  if (want_all) {
    out("C char size", "compiler:c:sizeof:char", sizeof(char));
    out("C bool size", "compiler:c:sizeof:bool", sizeof(bool));
    out("C short size", "compiler:c:sizeof:short", sizeof(short));
    out("C int size", "compiler:c:sizeof:int", sizeof(int));
    out("C long size", "compiler:c:sizeof:long", sizeof(long));
    out("C float size", "compiler:c:sizeof:float", sizeof(float));
    out("C double size", "compiler:c:sizeof:double", sizeof(double));
    out("C pointer size", "compiler:c:sizeof:pointer", sizeof(void *));
    out("C char align", "compiler:c:align:char", OMPI_ALIGNMENT_CHAR);
    out("C bool align", "compiler:c:align:bool", OMPI_ALIGNMENT_CXX_BOOL);
    out("C int align", "compiler:c:align:int", OMPI_ALIGNMENT_INT);
    out("C float align", "compiler:c:align:float", OMPI_ALIGNMENT_FLOAT);
    out("C double align", "compiler:c:align:double", OMPI_ALIGNMENT_DOUBLE);
  }

  out("C++ compiler", "compiler:cxx:command", OMPI_CXX);

  out("Fortran77 compiler", "compiler:f77:command", OMPI_F77);
  out("Fortran90 compiler", "compiler:f90:command", OMPI_F90);

  if (want_all) {

    // Will always have the size of Fortran integer

    out("Fort integer size", "compiler:fortran:sizeof:integer", 
        OMPI_SIZEOF_FORTRAN_INT);

    // May or may not have the other Fortran sizes

    if (OMPI_WANT_F77_BINDINGS || OMPI_WANT_F90_BINDINGS) {
      out("Fort real size", "compiler:fortran:sizeof:real", 
          OMPI_SIZEOF_FORTRAN_REAL);
      out("Fort dbl prec size", 
          "compiler:fortran:sizeof:double_precision",
          OMPI_SIZEOF_FORTRAN_REAL);
      out("Fort cplx size", "compiler:fortran:sizeof:complex", 
          OMPI_SIZEOF_FORTRAN_REAL);
      out("Fort dbl cplx size",
          "compiler:fortran:sizeof:double_complex", 
          OMPI_SIZEOF_FORTRAN_REAL);
      
      out("Fort integer align", "compiler:fortran:align:integer", 
          OMPI_ALIGNMENT_FORTRAN_INT);
      out("Fort real align", "compiler:fortran:align:real", 
          OMPI_ALIGNMENT_FORTRAN_REAL);
      out("Fort dbl prec align", 
          "compiler:fortran:align:double_precision",
          OMPI_ALIGNMENT_FORTRAN_REAL);
      out("Fort cplx align", "compiler:fortran:align:complex", 
          OMPI_ALIGNMENT_FORTRAN_REAL);
      out("Fort dbl cplx align",
          "compiler:fortran:align:double_complex", 
          OMPI_ALIGNMENT_FORTRAN_REAL);
    } else {
      out("Fort real size", "compiler:fortran:sizeof:real", "skipped");
      out("Fort dbl prec size",
          "compiler:fortran:sizeof:double_precision", "skipped");
      out("Fort cplx size", "compiler:fortran:sizeof:complex", "skipped");
      out("Fort dbl cplx size",
          "compiler:fortran:sizeof:double_complex", "skipped");
      
      out("Fort integer align", "compiler:fortran:align:integer", "skipped");
      out("Fort real align", "compiler:fortran:align:real", "skipped");
      out("Fort dbl prec align", 
          "compiler:fortran:align:double_precision","skipped");
      out("Fort cplx align", "compiler:fortran:align:complex", "skipped");
      out("Fort dbl cplx align",
          "compiler:fortran:align:double_complex", "skipped");
    }
  }

  out("C profiling", "option:profiling:c", cprofiling);
  out("C++ profiling", "option:profiling:cxx", cxxprofiling);
  out("Fortran77 profiling", "option:profiling:f77", f77profiling);
  out("Fortran90 profiling", "option:profiling:f90", f90profiling);

  out("C++ exceptions", "option:cxx_exceptions", cxxexceptions);
  out("Thread support", "option:threads", threads);

  if (want_all) {
    
    // Don't display the build CPPFLAGS or CXXCPPFLAGS because they're
    // just -I$(top_srcdir)/include, etc.  Hence, they're a) boring,
    // and c) specific for ompi_info.

    out("Build CFLAGS", "option:build:cflags", OMPI_BUILD_CFLAGS);
    out("Build CXXFLAGS", "option:build:cxxflags", OMPI_BUILD_CXXFLAGS);
    out("Build FFLAGS", "option:build:fflags", OMPI_BUILD_FFLAGS);
    out("Build FCFLAGS", "option:build:fcflags", OMPI_BUILD_FCFLAGS);
    out("Build LDFLAGS", "option:build:ldflags", OMPI_BUILD_LDFLAGS);
    out("Build LIBS", "option:build:libs", OMPI_BUILD_LIBS);

    out("Wrapper extra CFLAGS", "option:wrapper:extra_cflags", 
        WRAPPER_EXTRA_CFLAGS);
    out("Wrapper extra CXXFLAGS", "option:wrapper:extra_cxxflags", 
        WRAPPER_EXTRA_CXXFLAGS);
    out("Wrapper extra FFLAGS", "option:wrapper:extra_fflags", 
        WRAPPER_EXTRA_FFLAGS);
    out("Wrapper extra FCFLAGS", "option:wrapper:extra_fcflags", 
        WRAPPER_EXTRA_FCFLAGS);
    out("Wrapper extra LDFLAGS", "option:wrapper:extra_ldflags", 
        WRAPPER_EXTRA_LDFLAGS);
    out("Wrapper extra LIBS", "option:wrapper:extra_libs",
        WRAPPER_EXTRA_LIBS);
  }

  out("Internal debug support", "option:debug", debug);
  out("MPI parameter check", "option:mpi-param-check", paramcheck);
  out("Memory profiling support", "option:mem-profile", memprofile);
  out("Memory debugging support", "option:mem-debug", memdebug);
}
