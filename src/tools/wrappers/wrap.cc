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
#include <vector>

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <unistd.h>
#include <errno.h>

#include "util/path.h"
#include "util/argv.h"
#include "util/few.h"
#include "util/path.h"
#include "util/show_help.h"
#include "tools/wrappers/ompi_wrap.h"

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

void
ompi_wrap_parse_args(int argc, char *argv[], bool & want_flags)
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
	if ("-showme" == str.substr(0, 7) || "--showme" == str.substr(0, 8)
	    || "-show" == str || "--show" == str) {
	    want_flags = true;
	    fl_want_show_error = true;
	    if ("-showme:" == str.substr(0, 8)) {
		str = str.substr(8);
		have_arg = true;
	    } else if ("--showme:" == str.substr(0, 9)) {
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
	ompi_show_help("help-wrapper.txt", "no-profiling-support", true,
		       argv[0], NULL);
	fl_profile = false;
    }
#endif
}


void
ompi_wrap_get_compiler(const ompi_sv_t & env_list,
		       const string & default_comp, ompi_sv_t & out)
{
    char *env;
    string comp;
    string temp1, temp2;
    string::size_type i, pos;
    string compiler0;

    out.clear();

    // Check for environment variable overrides

    for (i = 0; i < env_list.size(); ++i) {
	env = getenv(env_list[i].c_str());
	if (NULL != env) {
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
	    while (i < out.size() && "--" == out[i].substr(0, 2)) {
		++i;
	    }
	    out.erase(out.begin(), out.begin() + i);
	}
    }
}


void
ompi_wrap_build_xppflags(const ompi_sv_t & env_list,
			 bool want_f77_includes, ompi_sv_t & xppflags)
{
    string incdir(OMPI_INCDIR);
    char *env;
    string temp;

    xppflags.clear();

    // Check for environment variable overrides.  If we find some, use
    // them, regardless of the f77_include setting.

    for (string::size_type i = 0; i < env_list.size(); ++i) {
	env = getenv(env_list[i].c_str());
	if (NULL != env) {
	    temp = env;
	    ompi_wrap_split(temp, ' ', xppflags);
	    return;
	}
    }

    // Ensure that we don't -I/usr/include, for the reasons listed
    // above.  EXCEPTION: If this is fortran, then we add -I regardless
    // of what the prefix is, because the fortran compiler will not
    // -I/usr/include automatically.

    if (want_f77_includes || "/usr/include" != incdir) {
	xppflags.push_back("-I" + incdir);
    }

#if OMPI_WANT_CXX_BINDINGS
    xppflags.push_back("-I" + incdir + "/openmpi");
#endif

#if defined(WIN32) && WIN32
#error "Anju needs to fix me.  FIX ME FIX ME FIX ME."
#endif
}


void
ompi_wrap_build_xflags(const ompi_sv_t & env_list,
		       const string & default_xflags, ompi_sv_t & xflags)
{
    char *env;
    string temp;

    // Check for environment variable overrides.  If we find some, use
    // them, regardless of the f77_include setting.

    for (string::size_type i = 0; i < env_list.size(); ++i) {
	env = getenv(env_list[i].c_str());
	if (NULL != env) {
	    temp = env;
	    ompi_wrap_split(temp, ' ', xflags);
	    return;
	}
    }

    if (!default_xflags.empty()) {
	ompi_wrap_split_append_sv(default_xflags, xflags);
    }
}


void
ompi_wrap_build_user_args(int argc, char *argv[], ompi_sv_t & user_args)
{
    string str;

    // Don't copy -showme* or -lpmpi.  -lpmpi will be
    // insertted elsewhere if necessary.

    for (int i = 1; i < argc; ++i) {
	str = argv[i];
	if ("-showme" != str.substr(0, 7) &&
	    "--showme" != str.substr(0, 8) &&
	    "-show" != str && "--show" != str && "-lpmpi" != str) {
	    user_args.push_back(str);
	}
    }
}


void
ompi_wrap_build_ldflags(const ompi_sv_t & env_list, ompi_sv_t & ldflags)
{
    char *env;
    string s;
    string temp;
    string libdir(OMPI_LIBDIR);
    ompi_sv_t sv;
    ompi_sv_t::iterator svi;

    ldflags.clear();

    // If we don't want the libs, then we don't want ldflags, either.
    // Hence, return with ldflags empty.

    if (!fl_libs) {
	return;
    }
    // Check for environment variable overrides

    for (string::size_type i = 0; i < env_list.size(); ++i) {
	env = getenv(env_list[i].c_str());
	if (NULL != env) {
	    temp = env;
	    ompi_wrap_split(temp, ' ', ldflags);
	    return;
	}
    }

    // Add in the extra flags passed by configure.  Do the same kinds of
    // checks that we do below -- ensure that we don't add a "-L/usr" to
    // the command line.

    s = WRAPPER_EXTRA_LDFLAGS;
    ompi_wrap_split(s, ' ', sv);

    if (!sv.empty()) {
	for (svi = sv.begin(); svi != sv.end(); ++svi) {
	    if ("-L/usr" != *svi) {
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

    if ("/usr/lib" != libdir) {
	ldflags.push_back("-L" + libdir);
    }
}


void
ompi_wrap_build_libs(const ompi_sv_t & env_list,
		     bool want_cxx_libs, ompi_sv_t & libs)
{
    char *env;
    string temp;
    string libdir(OMPI_LIBDIR);

    libs.clear();

    // If we don't want the libs, then return with libs empty

    if (!fl_libs) {
	return;
    }
    // Check for environment variable overrides

    for (string::size_type i = 0; i < env_list.size(); ++i) {
	env = getenv(env_list[i].c_str());
	if (NULL != env) {
	    temp = env;
	    ompi_wrap_split(temp, ' ', libs);
	    return;
	}
    }

    // Now we start adding libraries to libs

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

    // Next comes the MPI library

    libs.push_back("-lmpi");

    ompi_wrap_split_append_sv(WRAPPER_EXTRA_LIBS, libs);
}


void
ompi_wrap_print_sv(const ompi_sv_t & sv)
{
    for (string::size_type i = 0; i < sv.size(); ++i) {
	cout << sv[i] << " ";
    }
}


int
ompi_wrap_exec_sv(const ompi_sv_t & sv)
{
    int status;
    int ret;
    int ac = 0;
    char **av = 0;
    char *tmp = NULL;
    string::size_type i;

    // Build up a C array of the args

    for (i = 0; i < sv.size(); ++i) {
	ompi_argv_append(&ac, &av, (char *) sv[i].c_str());
    }

    // There is no way to tell whether ompi_few returned non-zero because
    // the called app returned non-zero or if there was a failure in the
    // exec (like the file not being found).  So we look for the
    // compiler first, just to try to eliminate that case.
    tmp = ompi_path_findv(av[0], 0, environ, NULL);
    if (NULL == tmp) {
	ompi_show_help("help-wrapper.txt", "no-compiler-found", true,
		       av[0], NULL);
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


void
ompi_wrap_strip_white(string & str)
{
    int start, end, size(str.size());

    /* Remove leading whitespace */

    for (start = 0; start < size; ++start) {
	if (!isspace(str[start])) {
	    break;
	}
    }

    /* Remove trailing whitespace */

    for (end = start; end < size; ++end) {
	if (isspace(str[end])) {
	    break;
	}
    }

    str = str.substr(start, end);
}


bool
ompi_wrap_split(const string & str, char c, ompi_sv_t & out)
{
    int start, i(0), size(str.size());
    out.clear();

    // Strip off the first bunch of c's

    while (i < size && str[i] == c) {
	++i;
    }

    if (i >= size) {
	return false;
    }
    // Now start making a vector of the strings

    while (i < size) {
	start = i;
	while (i < size && str[i] != c) {
	    ++i;
	}
	out.push_back(str.substr(start, i - start));
	while (i < size && str[i] == c) {
	    ++i;
	}
    }

    return true;
}


void
ompi_wrap_split_append_sv(const string & str, ompi_sv_t & out)
{
    ompi_sv_t temp;

    ompi_wrap_split(str, ' ', temp);
    for (string::size_type i = 0; i < temp.size(); ++i) {
	out.push_back(temp[i]);
    }
}


void
ompi_wrap_append_sv(const ompi_sv_t & in, ompi_sv_t & out)
{
    // JMS Is there a better way to do this?
    for (string::size_type i = 0; i < in.size(); ++i) {
	out.push_back(in[i]);
    }
}


bool
ompi_wrap_check_file(const string & dir, const string & file)
{
    int ret;
    struct stat buf;
    string name = dir + "/" + file;

    ret = stat(name.c_str(), &buf);

    return (bool) (0 == ret);
}
