//
// $HEADER$
//

#include "lam_config.h"

#include <iostream>
#include <string>
#include <map>

#include <unistd.h>
#include <sys/param.h>

#include "mca/lam/base/mca_base_param.h"
#include "tools/laminfo/laminfo.h"

using namespace std;
using namespace laminfo;


//
// Public variables
//

string laminfo::module_all = "all";
string laminfo::param_all = "all";

string laminfo::path_prefix = "prefix";
string laminfo::path_bindir = "bindir";
string laminfo::path_libdir = "libdir";
string laminfo::path_incdir = "incdir";
string laminfo::path_pkglibdir = "pkglibdir";
string laminfo::path_sysconfdir = "sysconfdir";

//
// External variables
//
// This exists in mca/lam/base/mca_base_param.c.  It's not extern'ed
// in mca_base_param.h so that no one else will use it.
//

extern lam_array_t mca_base_params;


void laminfo::do_params()
{
  unsigned int count;
  string type, module;
  bool found, want_all;
  laminfo::type_vector_t::size_type i;

  laminfo::open_modules();

  // See if the special param "all" was givin to --param; that
  // superceeds any individual type

  want_all = false;
  count = lam_cmd_line_get_ninsts(cmd_line, "param");
  for (i = 0; i < count; ++i) {
    type = lam_cmd_line_get_param(cmd_line, "param", i, 0);
    if (type_all == type) {
      want_all = true;
      break;
    }
  }

  // Show the params

  if (want_all) {
    for (i = 0; i < mca_types.size(); ++i) {
      show_mca_params(mca_types[i], module_all, param_all);
    }
  } else {
    for (i = 0; i < count; ++i) {
      type = lam_cmd_line_get_param(cmd_line, "param", i, 0);
      module = lam_cmd_line_get_param(cmd_line, "param", i, 1);

      for (found = false, i = 0; i < mca_types.size(); ++i) {
        if (mca_types[i] == type) {
          found = true;
          break;
        }
      }

      if (!found) {
#if 0
        show_help("laminfo", "usage");
#endif
        exit(1);
      }

      show_mca_params(type, module, param_all);
    }
  }
}


void laminfo::show_mca_params(const string& type, const string& module, 
                              const string& param)
{
  size_t i, size;
  char *default_value_string, temp[BUFSIZ];
  string message, content;
  mca_base_param_t *item;

  size = lam_arr_get_size(&mca_base_params);
  if (0 == size) {
    return;
  }

  for (i = 0; i < size; ++i) {
    item = (mca_base_param_t *) lam_arr_get_item(&mca_base_params, i);
    if (type == item->mbp_type_name) {
      if (module == module_all || 
          NULL == item->mbp_module_name ||
          (NULL != item->mbp_module_name &&
           module == item->mbp_module_name)) {
        if (param == param_all || param == item->mbp_param_name) {

          // Make a string for the default value

          temp[0] = '\0';
          if (item->mbp_type == MCA_BASE_PARAM_TYPE_STRING) {
            if (item->mbp_default_value.stringval != NULL)
              default_value_string = item->mbp_default_value.stringval;
            else
              default_value_string = temp;
          } else {
            default_value_string = temp;
            snprintf(default_value_string, BUFSIZ, "%d", 
                     item->mbp_default_value.intval);
          }
          content = default_value_string;

          // Build up the strings to output.

          if (pretty) {
            message = "MCA ";
            message += item->mbp_type_name;

            // Put in the real, full name (which may be different than
            // the categorization).

            content = (item->mbp_env_var_name != NULL) ?
              "parameter \"" : "information \"";
            content += item->mbp_full_name;
            content += (item->mbp_env_var_name != NULL) ?
              "\" (default: " : "\" (value: ";

            if (strlen(default_value_string) == 0)
              content += "<none>)";
            else {
              content += "\"";
              content += default_value_string;
              content += "\")";
            }

            out(message, message, content);
          } else {
            message = "mca:";
            message += item->mbp_type_name;
            message += ":";

            if (item->mbp_module_name != NULL) {
              message += item->mbp_module_name;
            } else {
              message += "base";
            }
            message += (item->mbp_env_var_name != NULL) ?
              ":param:" : ":info:";

            // Put in the real, full name (which may be different than
            // the categorization).

            message += item->mbp_full_name;

            content = default_value_string;

            out(message, message, content);
          }
        }
      }
    }
  }
}


void laminfo::do_path(bool want_all, lam_cmd_line_t *cmd_line)
{
  int i, count;
  string scope;

  if (want_all) {
    show_path(path_prefix, LAM_PREFIX);
    show_path(path_bindir, LAM_BINDIR);
    show_path(path_libdir, LAM_LIBDIR);
    show_path(path_incdir, LAM_INCDIR);
    show_path(path_pkglibdir, LAM_PKGLIBDIR);
    show_path(path_sysconfdir, LAM_SYSCONFDIR);
  } else {
    count = lam_cmd_line_get_ninsts(cmd_line, "path");
    for (i = 0; i < count; ++i) {
      scope = lam_cmd_line_get_param(cmd_line, "path", i, 0);

      if (path_prefix == scope)
        show_path(path_prefix, LAM_PREFIX);
      else if (path_bindir == scope)
        show_path(path_bindir, LAM_BINDIR);
      else if (path_libdir == scope)
        show_path(path_libdir, LAM_LIBDIR);
      else if (path_incdir == scope)
        show_path(path_incdir, LAM_INCDIR);
      else if (path_pkglibdir == scope)
        show_path(path_pkglibdir, LAM_PKGLIBDIR);
      else if (path_sysconfdir == scope)
        show_path(path_sysconfdir, LAM_SYSCONFDIR);
      else {
#if 0
        show_help("laminfo", "usage");
#endif
        exit(1);
      }
    }
  }
}


void laminfo::show_path(const string& type, const string& value)
{
  string pretty(type);
  pretty[0] &= toupper(pretty[0]);

  out(pretty, "path:" + type, value);
}


void laminfo::do_arch(lam_cmd_line_t *cmd_line)
{
  string prefix;
  char hostname[MAXHOSTNAMELEN];

  if (lam_cmd_line_is_taken(cmd_line, "hostname")) {
    gethostname(hostname, MAXHOSTNAMELEN);
    prefix = hostname + string(":");
  }
  out("Architecture", prefix + "arch", LAM_ARCH);
}


//
// do_config
// Accepts:
//	- want_all: boolean flag; TRUE -> display all options
//				  FALSE -> display selected options
//
// This function displays all the options with which the current
// installation of lam was configured. There are many options here 
// that are carried forward from LAM-7 and are not mca parameters 
// in LAM-10. I have to dig through the invalid options and replace
// them with LAM-10 options.
//
void laminfo::do_config(bool want_all)
{
  const string f77(LAM_ENABLE_MPI_F77 ? "yes" : "no");
  const string f90(LAM_ENABLE_MPI_F90 ? "yes" : "no");
  const string sthreads(LAM_HAVE_SOLARIS_THREADS ? "yes" : "no");
  const string pthreads(LAM_HAVE_POSIX_THREADS ? "yes" : "no");
  const string memprofile(LAM_ENABLE_MEM_PROFILE ? "yes" : "no");
  const string memdebug(LAM_ENABLE_MEM_DEBUG ? "yes" : "no");
#if 0
  // Anju: 
  // These are the options that will definately be added along the
  // line. So, I am compiling them out instead of deleting them
  const string romio(LAM_WANT_ROMIO ? "yes" : "no");
  const string impi(LAM_WANT_IMPI ? "yes" : "no");
#endif
  const string debug(LAM_ENABLE_DEBUG ? "yes" : "no");
  const string cprofiling(LAM_WANT_MPI_PROFILING ? "yes" : "no");
  const string cxxprofiling(LAM_WANT_MPI_PROFILING ? "yes" : "no");
  const string f77profiling((LAM_WANT_MPI_PROFILING && LAM_ENABLE_MPI_F77) ?
                          "yes" : "no");
  const string f90profiling((LAM_WANT_MPI_PROFILING && LAM_ENABLE_MPI_F90) ?
                          "yes" : "no");
  const string cxxexceptions(LAM_HAVE_CXX_EXCEPTION_SUPPORT ? "yes" : "no");

  out("Configured by", "config:user", LAM_CONFIGURE_USER);
  out("Configured on", "config:timestamp", LAM_CONFIGURE_DATE);
  out("Configure host", "config:host", LAM_CONFIGURE_HOST);

  out("C bindings", "bindings:c", "yes");
  out("C++ bindings", "bindings:cxx", "yes");
  out("Fortran77 bindings", "bindings:f77", f77);
  out("Fortran90 bindings", "bindings:f90", f90);

  out("C compiler", "compiler:c:command", LAM_CC);

  if (want_all) {
    out("C char size", "compiler:c:sizeof:char", sizeof(char));
    out("C bool size", "compiler:c:sizeof:bool", sizeof(bool));
    out("C short size", "compiler:c:sizeof:short", sizeof(short));
    out("C int size", "compiler:c:sizeof:int", sizeof(int));
    out("C long size", "compiler:c:sizeof:long", sizeof(long));
    out("C float size", "compiler:c:sizeof:float", sizeof(float));
    out("C double size", "compiler:c:sizeof:double", sizeof(double));
    out("C pointer size", "compiler:c:sizeof:pointer", sizeof(void *));
#if 0
    // Anju:
    // Not yet gotten to this part in LAM-10.
    //
    out("C char align", "compiler:c:align:char", LAM_ALIGNMENT_CHAR);
    out("C bool align", "compiler:c:align:bool", LAM_ALIGNMENT_CXX_BOOL);
    out("C int align", "compiler:c:align:int", LAM_ALIGNMENT_INT);
    out("C float align", "compiler:c:align:float", LAM_ALIGNMENT_FLOAT);
    out("C double align", "compiler:c:align:double", 
        LAM_ALIGNMENT_DOUBLE);
#endif
  }

  out("C++ compiler", "compiler:cxx:command", LAM_CXX);

  out("Fortran77 compiler", "compiler:f77:command", LAM_F77);
  out("Fortran90 compiler", "compiler:f90:command", LAM_F90);
  if (want_all) {
#if 0
    // Anju:
    // Not yet gotten to this part in LAM-10.
    //
    out("Fort integer size", "compiler:fortran:sizeof:integer", 
        LAM_SIZEOF_FORTRAN_INT);
    out("Fort real size", "compiler:fortran:sizeof:real", 
        LAM_SIZEOF_FORTRAN_REAL);
    out("Fort dbl prec size", 
        "compiler:fortran:sizeof:double_precision", LAM_SIZEOF_FORTRAN_REAL);
    out("Fort cplx size", "compiler:fortran:sizeof:complex", 
        LAM_SIZEOF_FORTRAN_REAL);
    out("Fort dbl cplx size",
        "compiler:fortran:sizeof:double_complex", 
        LAM_SIZEOF_FORTRAN_REAL);

    out("Fort integer align", "compiler:fortran:align:integer", 
        LAM_ALIGNMENT_FORTRAN_INT);
    out("Fort real align", "compiler:fortran:align:real", 
        LAM_ALIGNMENT_FORTRAN_REAL);
    out("Fort dbl prec align", 
        "compiler:fortran:align:double_precision", LAM_ALIGNMENT_FORTRAN_REAL);
    out("Fort cplx align", "compiler:fortran:align:complex", 
        LAM_ALIGNMENT_FORTRAN_REAL);
    out("Fort dbl cplx align",
        "compiler:fortran:align:double_complex", 
        LAM_ALIGNMENT_FORTRAN_REAL);
#endif
  }

  out("C profiling", "option:profiling:c", cprofiling);
  out("C++ profiling", "option:profiling:cxx", cxxprofiling);
  out("Fortran77 profiling", "option:profiling:f77", f77profiling);
  out("Fortran90 profiling", "option:profiling:f90", f90profiling);

  out("C++ exceptions", "option:cxx_exceptions", cxxexceptions);
  out("POSIX thread support", "option:threads", pthreads);
  out("Solaris thread support", "option:threads", sthreads);

  out("Debug support", "option:debug", debug);
  out("Memory profiling support", "option:mem-profile", memprofile);
  out("Memory debugging support", "option:mem-debug", memdebug);
}
