//
// Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
//                         University Research and Technology
//                         Corporation.  All rights reserved.
// Copyright (c) 2004-2006 The University of Tennessee and The University
//                         of Tennessee Research Foundation.  All rights
//                         reserved.
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

#include <iostream>
#include <string>
#include <map>

#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#if HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#ifdef HAVE_NETDB_H
#include <netdb.h>
#endif

#include "opal/mca/installdirs/installdirs.h"
#include "opal/class/opal_value_array.h"
#include "opal/util/printf.h"
#include "opal/util/show_help.h"
#include "opal/memoryhooks/memory.h"
#include "opal/mca/base/mca_base_param.h"
#include "ompi/tools/ompi_info/ompi_info.h"

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

extern opal_value_array_t mca_base_params;


void ompi_info::do_params(bool want_all, bool want_internal)
{
  unsigned int count;
  string type, component;
  bool found;
  ompi_info::type_vector_t::size_type i;

  ompi_info::open_components();

  // See if the special param "all" was givin to --param; that
  // superceeds any individual type

  count = opal_cmd_line_get_ninsts(cmd_line, "param");
  for (i = 0; i < count; ++i) {
    type = opal_cmd_line_get_param(cmd_line, "param", i, 0);
    if (type_all == type) {
      want_all = true;
      break;
    }
  }

  // Show the params

  if (want_all) {
    for (i = 0; i < mca_types.size(); ++i) {
      show_mca_params(mca_types[i], component_all, want_internal);
    }
  } else {
    for (i = 0; i < count; ++i) {
      type = opal_cmd_line_get_param(cmd_line, "param", i, 0);
      component = opal_cmd_line_get_param(cmd_line, "param", i, 1);

      for (found = false, i = 0; i < mca_types.size(); ++i) {
        if (mca_types[i] == type) {
          found = true;
          break;
        }
      }

      if (!found) {
          char *usage = opal_cmd_line_get_usage_msg(cmd_line);
          opal_show_help("help-ompi_info.txt", "usage", true, usage);
          free(usage);
          exit(1);
      }

      show_mca_params(type, component, want_internal);
    }
  }
}


void ompi_info::show_mca_params(const string& type, const string& component, 
                                bool want_internal)
{
    opal_list_t *info;
    opal_list_item_t *i;
    mca_base_param_info_t *p;
    char *value_string, empty[] = "\0";
    string message, content, tmp;
    int value_int;

    mca_base_param_dump(&info, want_internal);
    for (i = opal_list_get_first(info); i != opal_list_get_last(info);
         i = opal_list_get_next(i)) {
        p = (mca_base_param_info_t*) i;

        if (NULL != p->mbpp_type_name && type == p->mbpp_type_name) {
            if (component == component_all || 
                NULL == p->mbpp_component_name ||
                (NULL != p->mbpp_component_name &&
                 component == p->mbpp_component_name)) {

                // Make a string for the default value.  Invoke a
                // lookup because it may transform the string ("~/" ->
                // "<home dir>/") or get the value from the
                // environment, a file, etc.

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
                    
                    content = p->mbpp_read_only ?
                        "information \"" : "parameter \"";
                    content += p->mbpp_full_name;
                    content += "\" (";
                    content += p->mbpp_read_only ?
                        "value: " : "current value: ";
                    
                    if (strlen(value_string) == 0) {
                        content += "<none>)";
                    } else {
                        content += "\"";
                        content += value_string;
                        content += "\")";
                    }
                    out(message, message, content);

                    // If we have a help message, output it
                    if (NULL != p->mbpp_help_msg) {
                        out("", "", p->mbpp_help_msg);
                    }
                } else {
                    tmp = "mca:";
                    tmp += p->mbpp_type_name;
                    tmp += ":";
                    
                    if (p->mbpp_component_name != NULL) {
                        tmp += p->mbpp_component_name;
                    } else {
                        tmp += "base";
                    }
                    tmp += ":param:";

                    // Put in the real, full name (which may be
                    // different than the categorization).
                    
                    tmp += p->mbpp_full_name;
                    tmp += ":";

                    // Output the value

                    message = tmp;
                    message += "value";
                    content = value_string;
                    out(message, message, content);

                    // Output whether it's read only or writable

                    message = tmp;
                    message += "status";
                    content = p->mbpp_read_only ? "read-only" : "writable";
                    out(message, message, content);

                    // If it has a help message, output that

                    if (NULL != p->mbpp_help_msg) {
                        message = tmp;
                        message += "help";
                        content = p->mbpp_help_msg;
                        out(message, message, content);
                    }
                }
                
                // If we allocated the string, then free it
                
                if (value_string != empty) {
                    free(value_string);
                }
            }
        }
    }

    /* Release the memory */

    mca_base_param_dump_release(info);
}


void ompi_info::do_path(bool want_all, opal_cmd_line_t *cmd_line)
{
  int i, count;
  string scope;

  if (want_all) {
    show_path(path_prefix, opal_install_dirs.prefix);
    show_path(path_bindir, opal_install_dirs.bindir);
    show_path(path_libdir, opal_install_dirs.libdir);
    show_path(path_incdir, opal_install_dirs.includedir);
    show_path(path_pkglibdir, opal_install_dirs.pkglibdir);
    show_path(path_sysconfdir, opal_install_dirs.sysconfdir);
  } else {
    count = opal_cmd_line_get_ninsts(cmd_line, "path");
    for (i = 0; i < count; ++i) {
      scope = opal_cmd_line_get_param(cmd_line, "path", i, 0);

      if (path_prefix == scope)
        show_path(path_prefix, opal_install_dirs.prefix);
      else if (path_bindir == scope)
        show_path(path_bindir, opal_install_dirs.bindir);
      else if (path_libdir == scope)
        show_path(path_libdir, opal_install_dirs.libdir);
      else if (path_incdir == scope)
        show_path(path_incdir, opal_install_dirs.includedir);
      else if (path_pkglibdir == scope)
        show_path(path_pkglibdir, opal_install_dirs.pkglibdir);
      else if (path_sysconfdir == scope)
        show_path(path_sysconfdir, opal_install_dirs.sysconfdir);
      else {
          char *usage = opal_cmd_line_get_usage_msg(cmd_line);
          opal_show_help("help-ompi_info.txt", "usage", true, usage);
          free(usage);
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


void ompi_info::do_arch(opal_cmd_line_t *cmd_line)
{
  string prefix;
  char hostname[MAXHOSTNAMELEN];

  if (opal_cmd_line_is_taken(cmd_line, "hostname")) {
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
  const string cxx(OMPI_WANT_CXX_BINDINGS ? "yes" : "no");
  const string f77(OMPI_WANT_F77_BINDINGS ? string("yes (") +
                   (OMPI_HAVE_WEAK_SYMBOLS ? "all" :
                    (OMPI_F77_CAPS ? "caps" :
                     (OMPI_F77_PLAIN ? "lower case" :
                      (OMPI_F77_SINGLE_UNDERSCORE ? "single underscore" : 
                       "double underscore")))) + string(")"): "no");
  const string f90(OMPI_WANT_F90_BINDINGS ? "yes" : "no");
  const string f90_size(OMPI_F90_BUILD_SIZE);
  const string heterogeneous(OMPI_ENABLE_HETEROGENEOUS_SUPPORT ? "yes" : "no");
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
  string threads;
  const string want_libltdl(OMPI_WANT_LIBLTDL ? "yes" : "no");
  const string mpirun_prefix_by_default(ORTE_WANT_ORTERUN_PREFIX_BY_DEFAULT ?
                                        "yes" : "no");
  
  if (OMPI_HAVE_SOLARIS_THREADS || OMPI_HAVE_POSIX_THREADS) {
      threads = OMPI_HAVE_SOLARIS_THREADS ? "solaris" :
          OMPI_HAVE_POSIX_THREADS ? "posix" : "type unknown";
          threads += " (";
          threads += "mpi: ";
          threads += OMPI_ENABLE_MPI_THREADS ? "yes" : "no";
          threads += ", progress: ";
          threads += OMPI_ENABLE_PROGRESS_THREADS ? "yes" : "no";
          threads += ")";
  } else {
      threads = "no";
  }

  out("Configured by", "config:user", OMPI_CONFIGURE_USER);
  out("Configured on", "config:timestamp", OMPI_CONFIGURE_DATE);
  out("Configure host", "config:host", OMPI_CONFIGURE_HOST);

  out("Built by", "build:user", OMPI_BUILD_USER);
  out("Built on", "build:timestamp", OMPI_BUILD_DATE);
  out("Built host", "build:host", OMPI_BUILD_HOST);

  out("C bindings", "bindings:c", "yes");
  out("C++ bindings", "bindings:cxx", cxx);
  out("Fortran77 bindings", "bindings:f77", f77);
  out("Fortran90 bindings", "bindings:f90", f90);
  out("Fortran90 bindings size", "bindings:f90:size", 
      OMPI_WANT_F90_BINDINGS ? f90_size : "na");

  out("C compiler", "compiler:c:command", OMPI_CC);
  out("C compiler absolute", "compiler:c:absolute", OMPI_CC_ABSOLUTE);

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
  out("C++ compiler absolute", "compiler:cxx:absolute", OMPI_CXX_ABSOLUTE);

  out("Fortran77 compiler", "compiler:f77:command", OMPI_F77);
  out("Fortran77 compiler abs", "compiler:f77:absolute", 
      OMPI_F77_ABSOLUTE);
  out("Fortran90 compiler", "compiler:f90:command", OMPI_F90);
  out("Fortran90 compiler abs", "compiler:f90:absolute", 
      OMPI_F90_ABSOLUTE);

  if (want_all) {

    // Will always have the size of Fortran integer

    out("Fort integer size", "compiler:fortran:sizeof:integer", 
        OMPI_SIZEOF_FORTRAN_INTEGER);

    out("Fort logical size", "compiler:fortran:sizeof:logical", 
        OMPI_SIZEOF_FORTRAN_LOGICAL);
    out("Fort logical value true", "compiler:fortran:value:true",
        OMPI_FORTRAN_VALUE_TRUE);


    // May or may not have the other Fortran sizes

    if (OMPI_WANT_F77_BINDINGS || OMPI_WANT_F90_BINDINGS) {
      out("Fort have integer1", "compiler:fortran:have:integer1", 
          OMPI_HAVE_FORTRAN_INTEGER1 ? "yes" : "no");
      out("Fort have integer2", "compiler:fortran:have:integer2", 
          OMPI_HAVE_FORTRAN_INTEGER2 ? "yes" : "no");
      out("Fort have integer4", "compiler:fortran:have:integer4", 
          OMPI_HAVE_FORTRAN_INTEGER4 ? "yes" : "no");
      out("Fort have integer8", "compiler:fortran:have:integer8", 
          OMPI_HAVE_FORTRAN_INTEGER8 ? "yes" : "no");
      out("Fort have integer16", "compiler:fortran:have:integer16", 
          OMPI_HAVE_FORTRAN_INTEGER16 ? "yes" : "no");

      out("Fort have real4", "compiler:fortran:have:real4", 
          OMPI_HAVE_FORTRAN_REAL4 ? "yes" : "no");
      out("Fort have real8", "compiler:fortran:have:real8", 
          OMPI_HAVE_FORTRAN_REAL8 ? "yes" : "no");
      out("Fort have real16", "compiler:fortran:have:real16", 
          OMPI_HAVE_FORTRAN_REAL16 ? "yes" : "no");

      out("Fort have complex8", "compiler:fortran:have:complex8", 
          OMPI_HAVE_FORTRAN_COMPLEX8 ? "yes" : "no");
      out("Fort have complex16", "compiler:fortran:have:complex16", 
          OMPI_HAVE_FORTRAN_COMPLEX16 ? "yes" : "no");
      out("Fort have complex32", "compiler:fortran:have:complex32", 
          OMPI_HAVE_FORTRAN_COMPLEX32 ? "yes" : "no");

      out("Fort integer1 size", "compiler:fortran:sizeof:integer1", 
          OMPI_HAVE_FORTRAN_INTEGER1 ? OMPI_SIZEOF_FORTRAN_INTEGER1 : -1);
      out("Fort integer2 size", "compiler:fortran:sizeof:integer2", 
          OMPI_HAVE_FORTRAN_INTEGER2 ? OMPI_SIZEOF_FORTRAN_INTEGER2 : -1);
      out("Fort integer4 size", "compiler:fortran:sizeof:integer4", 
          OMPI_HAVE_FORTRAN_INTEGER4 ? OMPI_SIZEOF_FORTRAN_INTEGER4 : -1);
      out("Fort integer8 size", "compiler:fortran:sizeof:integer8", 
          OMPI_HAVE_FORTRAN_INTEGER8 ? OMPI_SIZEOF_FORTRAN_INTEGER8 : -1);
      out("Fort integer16 size", "compiler:fortran:sizeof:integer17", 
          OMPI_HAVE_FORTRAN_INTEGER16 ? OMPI_SIZEOF_FORTRAN_INTEGER16 : -1);

      out("Fort real size", "compiler:fortran:sizeof:real", 
          OMPI_SIZEOF_FORTRAN_REAL);
      out("Fort real4 size", "compiler:fortran:sizeof:real4", 
          OMPI_HAVE_FORTRAN_REAL4 ? OMPI_SIZEOF_FORTRAN_REAL4 : -1);
      out("Fort real8 size", "compiler:fortran:sizeof:real8", 
          OMPI_HAVE_FORTRAN_REAL8 ? OMPI_SIZEOF_FORTRAN_REAL8 : -1);
      out("Fort real16 size", "compiler:fortran:sizeof:real17", 
          OMPI_HAVE_FORTRAN_REAL16 ? OMPI_SIZEOF_FORTRAN_REAL16 : -1);

      out("Fort dbl prec size", 
          "compiler:fortran:sizeof:double_precision",
          OMPI_SIZEOF_FORTRAN_REAL);

      out("Fort cplx size", "compiler:fortran:sizeof:complex", 
          OMPI_SIZEOF_FORTRAN_REAL);
      out("Fort dbl cplx size",
          "compiler:fortran:sizeof:double_complex", 
          OMPI_SIZEOF_FORTRAN_REAL);
      out("Fort cplx8 size", "compiler:fortran:sizeof:complex8", 
          OMPI_HAVE_FORTRAN_COMPLEX8 ? OMPI_SIZEOF_FORTRAN_COMPLEX8 : -1);
      out("Fort cplx16 size", "compiler:fortran:sizeof:complex16", 
          OMPI_HAVE_FORTRAN_COMPLEX16 ? OMPI_SIZEOF_FORTRAN_COMPLEX16 : -1);
      out("Fort cplx32 size", "compiler:fortran:sizeof:complex32", 
          OMPI_HAVE_FORTRAN_COMPLEX32 ? OMPI_SIZEOF_FORTRAN_COMPLEX32 : -1);
      
      out("Fort integer align", "compiler:fortran:align:integer", 
          OMPI_ALIGNMENT_FORTRAN_INTEGER);
      out("Fort integer1 align", "compiler:fortran:align:integer1", 
          OMPI_HAVE_FORTRAN_INTEGER1 ? OMPI_ALIGNMENT_FORTRAN_INTEGER1 : -1);
      out("Fort integer2 align", "compiler:fortran:align:integer2", 
          OMPI_HAVE_FORTRAN_INTEGER2 ? OMPI_ALIGNMENT_FORTRAN_INTEGER2 : -1);
      out("Fort integer4 align", "compiler:fortran:align:integer4", 
          OMPI_HAVE_FORTRAN_INTEGER4 ? OMPI_ALIGNMENT_FORTRAN_INTEGER4 : -1);
      out("Fort integer8 align", "compiler:fortran:align:integer8", 
          OMPI_HAVE_FORTRAN_INTEGER8 ? OMPI_ALIGNMENT_FORTRAN_INTEGER8 : -1);
      out("Fort integer16 align", "compiler:fortran:align:integer16", 
          OMPI_HAVE_FORTRAN_INTEGER16 ? OMPI_ALIGNMENT_FORTRAN_INTEGER16 : -1);

      out("Fort real align", "compiler:fortran:align:real", 
          OMPI_ALIGNMENT_FORTRAN_REAL);
      out("Fort real4 align", "compiler:fortran:align:real4", 
          OMPI_HAVE_FORTRAN_REAL4 ? OMPI_ALIGNMENT_FORTRAN_REAL4 : -1);
      out("Fort real8 align", "compiler:fortran:align:real8", 
          OMPI_HAVE_FORTRAN_REAL8 ? OMPI_ALIGNMENT_FORTRAN_REAL8 : -1);
      out("Fort real16 align", "compiler:fortran:align:real16", 
          OMPI_HAVE_FORTRAN_REAL16 ? OMPI_ALIGNMENT_FORTRAN_REAL16 : -1);

      out("Fort dbl prec align", 
          "compiler:fortran:align:double_precision",
          OMPI_ALIGNMENT_FORTRAN_REAL);

      out("Fort cplx align", "compiler:fortran:align:complex", 
          OMPI_ALIGNMENT_FORTRAN_REAL);
      out("Fort dbl cplx align",
          "compiler:fortran:align:double_complex", 
          OMPI_ALIGNMENT_FORTRAN_REAL);
      out("Fort cplx8 align", "compiler:fortran:align:complex8", 
          OMPI_HAVE_FORTRAN_COMPLEX8 ? OMPI_ALIGNMENT_FORTRAN_COMPLEX8 : -1);
      out("Fort cplx16 align", "compiler:fortran:align:complex16", 
          OMPI_HAVE_FORTRAN_COMPLEX16 ? OMPI_ALIGNMENT_FORTRAN_COMPLEX16 : -1);
      out("Fort cplx32 align", "compiler:fortran:align:complex32", 
          OMPI_HAVE_FORTRAN_COMPLEX32 ? OMPI_ALIGNMENT_FORTRAN_COMPLEX32 : -1);

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
  out("libltdl support", "option:dlopen", want_libltdl);
  out("Heterogeneous support", "options:heterogeneous", heterogeneous);
  out("mpirun default --prefix", "mpirun:prefix_by_default", 
      mpirun_prefix_by_default);
}
