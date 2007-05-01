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
// Copryight (c) 2007      Cisco Systems, Inc.  All rights reserved.
// $COPYRIGHT$
// 
// Additional copyrights may follow
// 
// $HEADER$
//

#include "ompi_config.h"

#include <iostream>
#include <string>
#include <utility>
#include <list>

#include <stdio.h>
#include <ctype.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_NETDB_H
#include <netdb.h>
#endif
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#include <errno.h>

#include "opal/mca/installdirs/installdirs.h"
#include "opal/class/opal_object.h"
#include "opal/runtime/opal.h"
#include "orte/runtime/runtime.h"
#include "opal/util/output.h"
#include "opal/util/cmd_line.h"
#include "opal/util/argv.h"
#include "opal/util/show_help.h"
#include "ompi/communicator/communicator.h"
#include "opal/mca/base/base.h"
#include "ompi/tools/ompi_info/ompi_info.h"
#include "opal/threads/mutex.h"

using namespace std;
using namespace ompi_info;


//
// Public variables
//

bool ompi_info::pretty = true;
opal_cmd_line_t *ompi_info::cmd_line = NULL;

const string ompi_info::type_all = "all";
const string ompi_info::type_ompi = "ompi";
const string ompi_info::type_orte = "orte";
const string ompi_info::type_opal = "opal";
const string ompi_info::type_base = "base";
ompi_info::type_vector_t ompi_info::mca_types;


int main(int argc, char *argv[])
{
  int ret = 0;
  bool want_help = false;
  bool cmd_error = false;
  bool acted = false;
  bool want_all = false;
  char **app_env = NULL, **global_env = NULL;
  int i, len;

  // Initialize the argv parsing handle
  if (OMPI_SUCCESS != opal_init_util()) {
    opal_show_help("help-ompi_info.txt", "lib-call-fail", true, 
                   "opal_init_util", __FILE__, __LINE__, NULL);
    exit(ret);
  }

  cmd_line = OBJ_NEW(opal_cmd_line_t);
  if (NULL == cmd_line) {
    ret = errno;
    opal_show_help("help-ompi_info.txt", "lib-call-fail", true, 
                   "opal_cmd_line_create", __FILE__, __LINE__, NULL);
    exit(ret);
  }

  opal_cmd_line_make_opt(cmd_line, 'v', "version", 2, 
                         "Show version of Open MPI or a component");
  opal_cmd_line_make_opt(cmd_line, '\0', "param", 2, 
                         "Show MCA parameters");
  opal_cmd_line_make_opt(cmd_line, '\0', "internal", 0, 
                         "Show internal MCA parameters (not meant to be modified by users)");
  opal_cmd_line_make_opt(cmd_line, '\0', "path", 1, 
                         "Show paths that Open MPI was configured with");
  opal_cmd_line_make_opt(cmd_line, '\0', "arch", 0, 
                         "Show architecture Open MPI was compiled on");
  opal_cmd_line_make_opt(cmd_line, 'c', "config", 0, 
                         "Show configuration options");
  opal_cmd_line_make_opt(cmd_line, 'h', "help", 0, 
                         "Show this help message");
  opal_cmd_line_make_opt(cmd_line, '\0', "pretty", 0, 
                         "Display output in 'prettyprint' format (default)");
  opal_cmd_line_make_opt(cmd_line, '\0', "parsable", 0, 
                         "Display output in parsable format");
  opal_cmd_line_make_opt(cmd_line, '\0', "parseable", 0, 
                         "Synonym for --parsable");
  opal_cmd_line_make_opt(cmd_line, '\0', "hostname", 0, 
                         "Show the hostname that Open MPI was configured "
                         "and built on");
  opal_cmd_line_make_opt(cmd_line, 'a', "all", 0, 
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

  // set our threading level
  opal_set_using_threads(false);

  // Get MCA parameters, if any */
  
  if( OMPI_SUCCESS != mca_base_open() ) {
      opal_show_help("help-ompi_info.txt", "lib-call-fail", true, "mca_base_open", __FILE__, __LINE__ );
      exit(1);
  }
  mca_base_cmd_line_setup(cmd_line);

  // Do the parsing

  if (OMPI_SUCCESS != opal_cmd_line_parse(cmd_line, false, argc, argv)) {
      cmd_error = true;
  }
  if (!cmd_error && opal_cmd_line_is_taken(cmd_line, "help") || 
      opal_cmd_line_is_taken(cmd_line, "h")) {
      want_help = true;
  }
  if (cmd_error || want_help) {
      char *usage = opal_cmd_line_get_usage_msg(cmd_line);
      opal_show_help("help-ompi_info.txt", "usage", true, usage);
      free(usage);
      exit(cmd_error ? 1 : 0);
  }

  mca_base_cmd_line_process_args(cmd_line, &app_env, &global_env);

  // putenv() all the stuff that we got back from env (in case the
  // user specified some --mca params on the command line).  This
  // creates a memory leak, but that's unfortunately how putenv()
  // works.  :-(

  len = opal_argv_count(app_env);
  for (i = 0; i < len; ++i) {
      putenv(app_env[i]);
  }
  len = opal_argv_count(global_env);
  for (i = 0; i < len; ++i) {
      putenv(global_env[i]);
  }

  ompi_info::mca_types.push_back("mca");
  ompi_info::mca_types.push_back("mpi");
  ompi_info::mca_types.push_back("orte");
  ompi_info::mca_types.push_back("opal");

  ompi_info::mca_types.push_back("backtrace");
  ompi_info::mca_types.push_back("memory");
  ompi_info::mca_types.push_back("paffinity");
  ompi_info::mca_types.push_back("maffinity");
  ompi_info::mca_types.push_back("timer");
  ompi_info::mca_types.push_back("installdirs");

  ompi_info::mca_types.push_back("allocator");
  ompi_info::mca_types.push_back("coll");
  ompi_info::mca_types.push_back("io");
  ompi_info::mca_types.push_back("mpool");
  ompi_info::mca_types.push_back("pml");
  ompi_info::mca_types.push_back("bml");
  ompi_info::mca_types.push_back("rcache");
  ompi_info::mca_types.push_back("btl");
  ompi_info::mca_types.push_back("mtl");
  ompi_info::mca_types.push_back("topo");
  ompi_info::mca_types.push_back("osc");
  ompi_info::mca_types.push_back("common");

  ompi_info::mca_types.push_back("errmgr");
  ompi_info::mca_types.push_back("gpr");
  ompi_info::mca_types.push_back("iof");
  ompi_info::mca_types.push_back("ns");
  ompi_info::mca_types.push_back("oob");
  ompi_info::mca_types.push_back("ras");
  ompi_info::mca_types.push_back("rds");
  ompi_info::mca_types.push_back("rmaps");
  ompi_info::mca_types.push_back("rmgr");
  ompi_info::mca_types.push_back("rml");
  ompi_info::mca_types.push_back("pls");
  ompi_info::mca_types.push_back("sds");
  ompi_info::mca_types.push_back("soh");

  // Execute the desired action(s)

  if (opal_cmd_line_is_taken(cmd_line, "pretty")) {
    ompi_info::pretty = true;
  } else if (opal_cmd_line_is_taken(cmd_line, "parsable") || opal_cmd_line_is_taken(cmd_line, "parseable")) {
    ompi_info::pretty = false;
  }

  want_all = opal_cmd_line_is_taken(cmd_line, "all");
  if (want_all || opal_cmd_line_is_taken(cmd_line, "version")) {
    do_version(want_all, cmd_line);
    acted = true;
  }
  if (want_all || opal_cmd_line_is_taken(cmd_line, "path")) {
    do_path(want_all, cmd_line);
    acted = true;
  }
  if (want_all || opal_cmd_line_is_taken(cmd_line, "arch")) {
    do_arch(cmd_line);
    acted = true;
  }
  if (want_all || opal_cmd_line_is_taken(cmd_line, "config")) {
    do_config(true);
    acted = true;
  }
  if (want_all || opal_cmd_line_is_taken(cmd_line, "param")) {
    do_params(want_all, opal_cmd_line_is_taken(cmd_line, "internal"));
    acted = true;
  }

  // If no command line args are specified, show default set

  if (!acted) {
    ompi_info::show_ompi_version(ver_full);
    ompi_info::show_path(path_prefix, opal_install_dirs.prefix);
    ompi_info::do_arch(cmd_line);
    ompi_info::do_config(false);
    ompi_info::open_components();
    for (ompi_info::type_vector_t::size_type i = 0; 
         i < mca_types.size(); ++i) {
        if ("mpi" != mca_types[i]) {
            ompi_info::show_component_version(mca_types[i], component_all, 
                                              ver_full, type_all);
        }
    }
  }

  // All done

  if (NULL != app_env) {
      opal_argv_free(app_env);
  }
  if (NULL != global_env) {
      opal_argv_free(global_env);
  }
  ompi_info::close_components();
  OBJ_RELEASE(cmd_line);
  mca_base_close();

  opal_finalize_util();

  return 0;
}
