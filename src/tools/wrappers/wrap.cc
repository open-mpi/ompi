//
// $HEADER$
//	Function:	- helper library for wrapper compilers
//

/** @file **/

#include "ompi_config.h"

#include <iostream>
#include <string>
#include <vector>

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <errno.h>

#include "util/path.h"
#include "tools/wrappers/ompi_wrap.h"
#include "util/argv.h"
#include "util/few.h"
#include "util/path.h"

extern char **environ;

using namespace std;


//
// Global variables
//

bool fl_libs(true);
bool fl_profile(false);
bool fl_cpp(false);
bool fl_want_show_error(false);

bool showme_cmd(false);
bool showme_compile(false);
bool showme_link(false);


//
// Local variables
//

static string cmd_name("<unknown>");

///
/// \internal
///
/// Parse the command line arguments of the wrapper compiler
///
/// \param argc Number of command line parameters
/// \param argv Vector containing command line parameters
/// \param want_flags True if extra flags are needed
///
void
ompi_wrap_parse_args(int argc, char* argv[], bool& want_flags)
{
  string str;
  bool have_arg;
  cmd_name = argv[0];

  // Note: only add all the compiler/linker flags if there is an
  // argv[] that doesn not begin with "-" or some form of "-showme" is
  // specified.

  want_flags = false;
  for (int i = 1; i < argc; ++i) {
    str = argv[i];
    if ("-showme" == str.substr(0, 7) || "--showme" == str.substr(0, 8) ||
        "-show" == str || "--show" == str) {
      want_flags = true;
      fl_want_show_error = true;
      if ("-showme:" == str.substr(0, 8)) {
        str = str.substr(8);
        have_arg = true;
      } else if ("--showme" == str.substr(0, 9)) {
        str = str.substr(9);
        have_arg = true;
      } else {
        showme_cmd = true;
        have_arg = false;
      }

      if (have_arg) {
        if ("compile" == str) {
          showme_compile = true;
        } else if ("link" == str) {
          showme_link = true;
        } else {
          showme_cmd = true;
        }
      }
    } else if ("-c" == str) {
      fl_libs = false;
    } else if ("-E" == str || "-M" == str) {
      fl_libs = false;
      fl_cpp = true;
    } else if ("-S" == str) {
      fl_libs = false;
    } else if ("-lpmpi" == str) {
      fl_profile = true;
    } else if ('-' != str[0]) {
      want_flags = true;
      fl_want_show_error = true;
    }
  }
#if !OMPI_ENABLE_MPI_PROFILING
  // Sanity check
  if (fl_profile) {
#if 0
    show_help("compile", "no-profiling-support", argv[0], NULL);
#endif
    fl_profile = false;
  }
#endif
}


///
/// \internal
///
/// Figure out what the back-end compiler is (even if it's multiple
/// tokens)
///
/// \param env_list List of environment variables passed by the user
/// \param default_comp Defaultl backend compiler
/// \param out Compiler to use (return value)
///
void
ompi_wrap_get_compiler(const ompi_sv_t& env_list, const string& default_comp,
			  ompi_sv_t& out)
{
  int i;
  char *env;
  string comp;
  string temp1, temp2;
  string::size_type pos;
  string compiler0;

  out.clear();

  // Check for environment variable overrides

  for (i = 0; (string::size_type) i < env_list.size(); ++i) {
    env = getenv(env_list[i].c_str());
    if (0 != env) {
      comp = env;
      ompi_wrap_split(comp, ' ', out);
      break;
    }
  }

  // If we didn't find any of the environment variables, use the default

  if (out.empty()) {
    ompi_wrap_split(default_comp, ' ', out);
  }

  // If we're preprocessing, we need to know the basename of argv0
  // (see below).

  if (fl_cpp) {
    pos = out[0].find_last_of('/');
    if (pos != string::npos) {
      // JMS There has to be a better way to do this
      compiler0 = out[0].substr(pos + 1);
    } else {
      compiler0 = out[0];
    }
  }
    
  // Ugh.  If we're acting as the preprocessor, ditch any libtool
  // arguments.

  if (fl_cpp) {
    // If we find "libtool", then advance until we find an argument
    // that does not begin with  "--"; the arguments after that will be
    // the compiler (preprocessor) stuff
    
    if ("libtool" == compiler0) {
      i = 1;
      while ((string::size_type) i < out.size() && 
		 "--" == out[i].substr(0, 2)){
			++i;
      }
      out.erase(out.begin(), out.begin() + i);
    }
  }
}


///
/// \internal
///
/// Build up a list of arguments for CFLAGS (a bit of a misnomer,
/// because it may actually be CXXFLAGS or FFLAGS, depending on what
/// the front-end wrapper compiler is).
///
/// \param want_f77_includes If F77 includes are wanted
/// \param cflags Vector of strings containing the cflags (returned)
///
void
ompi_wrap_build_cflags(bool want_f77_includes, ompi_sv_t& cflags)
{
  string incdir(OMPI_INCDIR);

  cflags.clear();

  // Ensure that we don't -I/usr/include, for the reasons listed
  // above.  EXCEPTION: If this is fortran, then we add -I regardless
  // of what the prefix is, because the fortran compiler will not
  // -I/usr/include automatically.

  if (want_f77_includes || "/usr/include" != incdir) {
    cflags.push_back("-I" + incdir);
  }

#if OMPI_WANT_CXX_BINDINGS
  cflags.push_back("-I" + incdir + "/ompi");
#endif

#if defined(WIN32) && WIN32
#error "Anju needs to fix me.  FIX ME FIX ME FIX ME."
#endif
}


///
/// \internal
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
void
ompi_wrap_build_user_args(int argc, char* argv[], ompi_sv_t& user_args)
{
  string str;

  // Don't copy -showme* or -lpmpi.  -lpmpi will be
  // insertted elsewhere if necessary.

  for (int i = 1; i < argc; ++i) {
    str = argv[i];
    if ("-showme" != str.substr(0, 7) &&
		  "--showme" != str.substr(0, 8) &&
		  "-show" != str &&
		  "--show" != str &&
		  "-lpmpi" != str){
          user_args.push_back(str);
    }
  }
}


///
/// \internal
///
/// Build up a list of LDFLAGS that will be given to the back-end
/// compiler.
///
/// \param ldflags Vector of strings comtaining the LDFLAGS
///
void
ompi_wrap_build_ldflags(ompi_sv_t& ldflags)
{
  string s;
  string libdir(OMPI_LIBDIR);
  ompi_sv_t sv;
  ompi_sv_t::iterator svi;

  ldflags.clear();

  // If we don't want the libs, then we don't want ldflags, either.
  // Hence, return with ldflags empty.

  if (!fl_libs) {
    return;
  }

  // Add in the extra flags passed by configure.  Do the same kinds of
  // checks that we do below -- ensure that we don't add a "-L/usr" to
  // the command line.

  s = WRAPPER_EXTRA_LDFLAGS;
  ompi_wrap_split(s, ' ', sv);

  if (!sv.empty()){
    for (svi = sv.begin(); svi != sv.end(); ++svi) {
      if ("-L/usr" != *svi){
		  ompi_wrap_split_append_sv(*svi, ldflags);
      }
    }
  }

  // Form library directory pathname.
  //
  // Add "-L$prefix/lib".
  //
  // Apply similar logic here as we did with -I -- if the libdir is
  // /usr/lib, there's no need to explicitly add it, because the
  // compiler will already be looking there.

  if ("/usr/lib" != libdir){
    ldflags.push_back("-L" + libdir);
  }
}


///
/// \internal
///
/// Build up a list of LIBS that will be given to the back-end
/// compiler.
///
/// \param want_cxx_libs Are we building mpiCC?
/// \param libs Vector of strings containing the libraries to be
///             linked to the backend compiler
void 
ompi_wrap_build_libs(bool want_cxx_libs, ompi_sv_t& libs)
{
  string libdir(OMPI_LIBDIR);
#if 0
#if OMPI_WANT_ROMIO && HAVE_LIBAIO
  bool want_aio(false);
#endif
#endif
  
  libs.clear();

  // If we don't want the libs, then return with libs empty

  if (!fl_libs){
    return;
  }

  // Now we start adding libraries to libs

  // ROMIO comes first.  Check to ensure that it exists (and that the
  // profiling library was previously found).
#if 0
#if OMPI_WANT_ROMIO
  if (!ompi_wrap_check_file(libdir, "libompi_mpio.a") &&
      !ompi_wrap_check_file(libdir, "libompi_mpio.so")) {
    cerr << "WARNING: " << cmd_name
	 << " expected to find libompi_mpio.* in " << libdir << endl
	 << "WARNING: MPI-2 IO support will be disabled" << endl;
  } else {
    libs.push_back("-lompi_mpio");
#if HAVE_LIBAIO
    want_aio = true;
#endif
  }
#endif
#endif

#if OMPI_WANT_CXX_BINDINGS
  // The C++ bindings come next
  if (want_cxx_libs) {
    if (!ompi_wrap_check_file(libdir, "libmpi_cxx.a") &&
	!ompi_wrap_check_file(libdir, "libmpi_cxx.so") && 
	!ompi_wrap_check_file(libdir, "libmpi_cxx.dylib")) {
      cerr << "WARNING: " << cmd_name
	   << " expected to find libmpi_cxx.* in " << libdir << endl
	   << "WARNING: MPI C++ support will be disabled" << endl;
    } else {
      libs.push_back("-lmpi_cxx");
    }
  }
#endif

  // Next comes the fortran MPI library
#if 0
#if BUILD_MPI_F77
  if (!ompi_wrap_check_file(libdir, "libompif77mpi.a") &&
      !ompi_wrap_check_file(libdir, "libompif77mpi.so")) {
      cerr << "WARNING: " << cmd_name
	   << " expected to find libompif77mpi.* in " << libdir << endl
	   << "WARNING: MPI Fortran support will be disabled" << endl;
  } else {
    libs.push_back("-lompif77mpi");
  }
#endif
#endif

  // Next comes the MPI library

  libs.push_back("-lmpi");

  // Finally, any system libraries
#if 0
#if OMPI_WANT_ROMIO && HAVE_LIBAIO
  if (want_aio) {
    libs.push_back("-laio");
  }
#endif
#endif
  ompi_wrap_split_append_sv(WRAPPER_EXTRA_LIBS, libs);
}


///
/// \internal
///
/// Build of a list of extra flags to go to the back-end compiler.
/// These are typically extra flags that come from the configure
/// script.
///
/// \param extra_string Extra flags to be passed to backend compiler
/// \param extra_flags Vector of strings to be pased to backend 
///        compiler (return value)
///
void
ompi_wrap_build_extra_flags(const string& extra_string, ompi_sv_t& extra_flags)
{
  if (!extra_string.empty()) {
    ompi_wrap_split_append_sv(extra_string, extra_flags);
  }
}


///
/// \internal
///
/// Print out a vector of strings
///
/// \param sv Vector of strings to be printed out
///
void
ompi_wrap_print_sv(const ompi_sv_t& sv)
{
  for (int i = 0; (string::size_type) i < sv.size(); ++i) {
    cout << sv[i] << " ";
  }
}


///
/// \internal
///
/// Execute a vector of strings (ultimately results down to a call to
/// some flavor of exec()).
///
/// \param sv Vector of strings to be exec()'ed
///
int
ompi_wrap_exec_sv(const ompi_sv_t& sv)
{
  int status;
  int ret;
  int i, ac = 0;
  char **av = 0;
  char *tmp = NULL;

  // Build up a C array of the args

  for (i = 0; (string::size_type) i < sv.size(); ++i) {
    ompi_argv_append(&ac, &av, (char*) sv[i].c_str());
  }

  // There is no way to tell whether ompi_few returned non-zero because
  // the called app returned non-zero or if there was a failure in the
  // exec (like the file not being found).  So we look for the
  // compiler first, just to try to eliminate that case.
  tmp = ompi_path_env_findv(av[0], 0, environ, NULL);
  if (NULL == tmp) {
#if 0
    show_help("compile", "no-compiler-found", av[0], NULL);
#endif
    errno = 0;
    status = -1;
  } else {
    free(tmp);
    ret = ompi_few(av, &status);
    status = WIFEXITED(status) ? WEXITSTATUS(status) : 
        (WIFSIGNALED(status) ? WTERMSIG(status) :
         (WIFSTOPPED(status) ? WSTOPSIG(status) : 255));

    if (0 != ret && 0 != errno && fl_want_show_error) {
      perror(cmd_name.c_str());
    }
  }

  // Free the C array
  ompi_argv_free(av);

  return status;
}


///
/// \internal
///
/// Remove leading and trailing white space from a given string.
/// Must be sent a null-terminated string.
///
/// \param str String from which leading and trailing spaces should
///            be removed
///
void 
ompi_wrap_strip_white(string& str)
{
  int start, end, size(str.size());

  /* Remove leading whitespace */

  for (start = 0; start < size; ++start){
    if (!isspace(str[start])){
      break;
    }
  }

  /* Remove trailing whitespace */

  for (end = start; end < size; ++end){
    if (isspace(str[end])) {
      break;
    }
  }

  str = str.substr(start, end);
}


///
/// \internal
///
/// Split a string into a vector of strings
//
/// \param str String which has to be split into a vector of strings
/// \param c Charecter which demarkates 2 strings
/// \param out Vector of strings (return value)
///
bool 
ompi_wrap_split(const string& str, char c, ompi_sv_t& out)
{
  int start, i(0), size(str.size());
  out.clear();

  // Strip off the first bunch of c's

  while (i < size && str[i] == c){
    ++i;
  }

  if (i >= size){
    return false;
  }

  // Now start making a vector of the strings

  while (i < size) {
    start = i;
    while (i < size && str[i] != c){
      ++i;
    }
    out.push_back(str.substr(start, i - start));
    while (i < size && str[i] == c){
      ++i;
    }
  }

  return true;
}


///
/// \internal
///
/// Take a string, split it into tokens, and append it to an existing
/// vector of strings
///
/// \param str String which is to be split into tokens
/// \param out Vector of strings to which the tokens are appended
///
void
ompi_wrap_split_append_sv(const string& str, ompi_sv_t& out)
{
  int i;
  ompi_sv_t temp;

  ompi_wrap_split(str, ' ', temp);
  for (i = 0; (string::size_type) i < temp.size(); ++i){
    out.push_back(temp[i]);
  }
}


///
/// \internal
///
/// Append one vector of strings onto the end of another.
///
/// \param in Vector of strings to be appended
/// \param out Vector of strings to which "in" will be appended
///
void 
ompi_wrap_append_sv(const ompi_sv_t& in, ompi_sv_t& out)
{
  // JMS Is there a better way to do this?
  for (int i = 0; (string::size_type) i < in.size(); ++i){
    out.push_back(in[i]);
  }
}


///
/// \internal
///
/// Check for the presence of a file
///
/// \param dir Directory in which the file should be present
/// \param file Name of the file
///
bool
ompi_wrap_check_file(const string& dir, const string& file)
{
  int ret;
  struct stat buf;
  string name = dir + "/" + file;

  ret = stat(name.c_str(), &buf);

  return (bool) (0 == ret);
}
