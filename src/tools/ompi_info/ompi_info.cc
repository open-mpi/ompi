//
// $HEADER$
//
/** @file **/

#include "ompi_config.h"

#include <iostream>
#include <string>
#include <utility>
#include <list>

#include <stdio.h>
#include <ctype.h>
#include <unistd.h>
#include <netdb.h>
#include <sys/param.h>
#include <errno.h>

#include "runtime/runtime.h"
#include "util/output.h"
#include "util/cmd_line.h"
#include "communicator/communicator.h"
#include "mca/base/base.h"
#include "tools/ompi_info/ompi_info.h"

using namespace std;
using namespace ompi_info;


//
// Public variables
//

bool ompi_info::pretty = true;
ompi_cmd_line_t *ompi_info::cmd_line = NULL;

const string ompi_info::type_all = "all";
const string ompi_info::type_ompi = "ompi";
const string ompi_info::type_base = "base";
ompi_info::type_vector_t ompi_info::mca_types;

int main(int argc, char *argv[])
{
  int ret = 0;
  bool acted = false;
  bool want_all = false;

  // Start OMPI process

  if (OMPI_SUCCESS != ompi_init(argc, argv)) {
    return -1;
  }

  // Initialize the argv parsing handle

  cmd_line = ompi_cmd_line_create();
  if (NULL == cmd_line) {
    ret = errno;
#if 0
    show_help(NULL, "lib-call-fail", "ompi_cmd_line_create", NULL);
#endif
    exit(ret);
  }
  ompi_cmd_line_make_opt(cmd_line, 'v', "version", 2, 
                        "Show version of Open MPI or a component");
  ompi_cmd_line_make_opt(cmd_line, '\0', "param", 2, 
                        "Show MCA parameters");
  ompi_cmd_line_make_opt(cmd_line, '\0', "path", 1, 
                        "Show paths that Open MPI was configured with");
  ompi_cmd_line_make_opt(cmd_line, '\0', "arch", 0, 
                        "Show architecture Open MPI was compiled on");
  ompi_cmd_line_make_opt(cmd_line, 'c', "config", 0, 
                        "Show configuration options");
  ompi_cmd_line_make_opt(cmd_line, 'h', "help", 0, 
                        "Show this help message");
  ompi_cmd_line_make_opt(cmd_line, '\0', "pretty", 0, 
                        "Display output in 'prettyprint' format (default)");
  ompi_cmd_line_make_opt(cmd_line, '\0', "parsable", 0, 
                        "Display output in parsable format");
  ompi_cmd_line_make_opt(cmd_line, '\0', "hostname", 0, 
                        "Show the hostname that Open MPI was configured "
                        "and built on");
  ompi_cmd_line_make_opt(cmd_line, 'a', "all", 0, 
                        "Show all configuration options and MCA parameters");

  // Call some useless functions in order to guarantee to link in some
  // global variables.  Only check the return value so that the
  // compiler doesn't optimize out the useless function.

  if (OMPI_SUCCESS != ompi_comm_link_function()) {
    // Stop .. or I'll say stop again!
    ++ret;
  } else {
    --ret;
  }

  // Get MCA parameters, if any */
  
  mca_base_open();
  mca_base_cmd_line_setup(cmd_line);

  // Do the parsing

  if (OMPI_SUCCESS != ompi_cmd_line_parse(cmd_line, false, argc, argv) ||
      ompi_cmd_line_is_taken(cmd_line, "help") || 
      ompi_cmd_line_is_taken(cmd_line, "h")) {
#if 1
    printf("...showing ompi_info help message...\n");
#else
    show_help("ompi_info", "usage", NULL);
#endif
    exit(1);
  }

  mca_base_cmd_line_process_args(cmd_line);

  ompi_info::mca_types.push_back("base");

  ompi_info::mca_types.push_back("allocator");
  ompi_info::mca_types.push_back("coll");
  ompi_info::mca_types.push_back("common");
  ompi_info::mca_types.push_back("gpr");
  ompi_info::mca_types.push_back("io");
  ompi_info::mca_types.push_back("mpool");
  ompi_info::mca_types.push_back("ns");
  ompi_info::mca_types.push_back("one");
  ompi_info::mca_types.push_back("oob");
  ompi_info::mca_types.push_back("op");
  ompi_info::mca_types.push_back("pcm");
  ompi_info::mca_types.push_back("pml");
  ompi_info::mca_types.push_back("ptl");
  ompi_info::mca_types.push_back("topo");

  // Execute the desired action(s)

  if (ompi_cmd_line_is_taken(cmd_line, "pretty")) {
    ompi_info::pretty = true;
  } else if (ompi_cmd_line_is_taken(cmd_line, "parsable")) {
    ompi_info::pretty = false;
  }

  want_all = ompi_cmd_line_is_taken(cmd_line, "all");
  if (want_all || ompi_cmd_line_is_taken(cmd_line, "version")) {
    do_version(want_all, cmd_line);
    acted = true;
  }
  if (want_all || ompi_cmd_line_is_taken(cmd_line, "path")) {
    do_path(want_all, cmd_line);
    acted = true;
  }
  if (want_all || ompi_cmd_line_is_taken(cmd_line, "arch")) {
    do_arch(cmd_line);
    acted = true;
  }
  if (want_all || ompi_cmd_line_is_taken(cmd_line, "config")) {
    do_config(true);
    acted = true;
  }
  if (want_all || ompi_cmd_line_is_taken(cmd_line, "param")) {
    do_params();
    acted = true;
  }

  // If no command line args are specified, show default set

  if (!acted) {
    ompi_info::show_ompi_version(ver_full);
    ompi_info::show_path(path_prefix, OMPI_PREFIX);
    ompi_info::do_arch(cmd_line);
    ompi_info::do_config(false);
    ompi_info::open_components();
    ompi_info::show_component_version("base", component_all, ver_full, type_all);
    ompi_info::show_component_version("allocator", component_all, ver_full, type_all);
    ompi_info::show_component_version("coll", component_all, ver_full, type_all);
    ompi_info::show_component_version("common", component_all, ver_full, type_all);
    ompi_info::show_component_version("gpr", component_all, ver_full, type_all);
    ompi_info::show_component_version("io", component_all, ver_full, type_all);
    ompi_info::show_component_version("mpool", component_all, ver_full, type_all);
    ompi_info::show_component_version("ns", component_all, ver_full, type_all);
    ompi_info::show_component_version("one", component_all, ver_full, type_all);
    ompi_info::show_component_version("oob", component_all, ver_full, type_all);
    ompi_info::show_component_version("op", component_all, ver_full, type_all);
    ompi_info::show_component_version("pcm", component_all, ver_full, type_all);
    ompi_info::show_component_version("pml", component_all, ver_full, type_all);
    ompi_info::show_component_version("ptl", component_all, ver_full, type_all);
    ompi_info::show_component_version("topo", component_all, ver_full, type_all);
  }

  // All done

  ompi_info::close_components();
  ompi_cmd_line_free(cmd_line);
  mca_base_close();
  ompi_finalize();
  return 0;
}
