/*
 * $HEADER$
 */
/** @file **/

#include "ompi_config.h"

#include <stdio.h>
#include <ctype.h>
#include <unistd.h>
#include <netdb.h>
#include <sys/param.h>
#include <errno.h>

#include "runtime/runtime.h"
#include "util/output.h"
#include "util/cmd_line.h"
#include "util/proc_info.h"
#include "communicator/communicator.h"
#include "mca/base/base.h"
#include "tools/ompid/ompid.h"

/*
 * Public variables
 */

bool pretty = true;
ompi_cmd_line_t *cmd_line = NULL;

const char *type_all = "all";
const char *type_ompi = "ompi";
const char *type_base = "base";

int main(int argc, char *argv[])
{
  int ret = 0;
  bool acted = false;

  /* Start OMPI process */

  if (OMPI_SUCCESS != ompi_init(argc, argv)) {
    return -1;
  }

  /* Initialize the argv parsing handle */

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
  ompi_cmd_line_make_opt(cmd_line, 'h', "help", 0, 
                        "Show this help message");

  /* Call some useless functions in order to guarantee to link in some
   * global variables.  Only check the return value so that the
   * compiler doesn't optimize out the useless function.
   */

  if (OMPI_SUCCESS != ompi_comm_link_function()) {
    /* Stop .. or I'll say stop again! */
    ++ret;
  } else {
    --ret;
  }

  /* Get MCA parameters, if any */
  
  mca_base_open();
  mca_base_cmd_line_setup(cmd_line);

  /* Do the parsing */

  if (OMPI_SUCCESS != ompi_cmd_line_parse(cmd_line, false, argc, argv) ||
      ompi_cmd_line_is_taken(cmd_line, "help") ){
#if 1
    printf("...showing ompid help message...\n");
#else
    show_help("ompid", "usage", NULL);
#endif
    exit(1);
  }

  mca_base_cmd_line_process_args(cmd_line);

 
  /* Execute the desired action(s) */

  if (ompi_cmd_line_is_taken(cmd_line, "version")) {
    /*do_version(want_all, cmd_line);*/
    acted = true;
  }
 

  /* If no command line args are specified, show default set */

  if (!acted) {
    ;
    /*ompid::show_ompi_version(ver_full);*/
  }

  /* Set proc_info's seed to true.  This is the seed daemon. */
  if ( ompi_proc_info () == MPI_SUCCESS ){
    ompi_process_info.seed = true;
  }
  else {
    /* Should probably git-up and die gracefully.
     * Ask David what I should really do here...
     */
  }

  /* Add in the calls to start up the RTE */
  
  /* Add in the calls to initialize the services */

  /* Add the swection for the event loop... */

  /* All done */

  /* Close services */

 
  /*ompid::close_components(); */

  ompi_cmd_line_free(cmd_line);
  mca_base_close();
  ompi_finalize();
  return 0;
}
