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
int
ompi_wrap_engine(int argc, char* argv[],
		const ompi_sv_t& env_vars, const string& default_compiler,
		bool want_cxx_libs, bool want_f77_includes,
		const string& extra_args)
{
  int ret(0);

  bool want_flags;

  ompi_sv_t str_vec;

  ompi_sv_t compiler;
  ompi_sv_t cflags;
  ompi_sv_t user_args;
  ompi_sv_t ldflags;
  ompi_sv_t libs;
  ompi_sv_t extra_flags;

  ompi_sv_t cmd_line;

  // Parse command line

  ompi_wrap_parse_args(argc, argv, want_flags);

  // Get the compiler

  ompi_wrap_get_compiler(env_vars, default_compiler, compiler);  

  // Build the CFLAGS

  ompi_wrap_build_cflags(want_f77_includes, cflags);

  // Build the user arguments

  ompi_wrap_build_user_args(argc, argv, user_args);

  // Build the LDFLAGS

  ompi_wrap_build_ldflags(ldflags);

  // Build the LIBS

  ompi_wrap_build_libs(want_cxx_libs, libs);

  // Build any extra arguments

  ompi_wrap_build_extra_flags(extra_args, extra_flags);

  // Now assemble the command line

  cmd_line.clear();
  ompi_wrap_append_sv(compiler, cmd_line);
  if (want_flags) {
    ompi_wrap_append_sv(cflags, cmd_line);
    ompi_wrap_append_sv(extra_flags, cmd_line);
  }
  ompi_wrap_append_sv(user_args, cmd_line);
  if (want_flags) {
    ompi_wrap_append_sv(ldflags, cmd_line);
    ompi_wrap_append_sv(libs, cmd_line);
  }

  // Display or execute?

  if (showme_cmd)
    ompi_wrap_print_sv(compiler);
  if (showme_cmd || showme_compile) {
    ompi_wrap_print_sv(cflags);
    ompi_wrap_print_sv(extra_flags);
  }
  if (showme_cmd || showme_compile || showme_link)
    ompi_wrap_print_sv(user_args);
  if (showme_cmd || showme_link) {
    ompi_wrap_print_sv(ldflags);
    ompi_wrap_print_sv(libs);
  }
  if (showme_cmd || showme_compile || showme_link)
    cout << endl;
  else
    ret = ompi_wrap_exec_sv(cmd_line);

  // That's all she wrote

  return ret;
}
