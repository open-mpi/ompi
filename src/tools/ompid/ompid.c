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

const char *type_all  = "all";
const char *type_ompi = "ompi";
const char *type_base = "base";

int main(int argc, char *argv[])
{
  int ret = 0;

  bool multi_thread   = false;
  bool hidden_thread  = false;

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
  ompi_cmd_line_make_opt(cmd_line, 'v', "version", 0, 
                        "Show version of this application");
  ompi_cmd_line_make_opt(cmd_line, 'h', "help", 0, 
                        "Show this help message");
  ompi_cmd_line_make_opt(cmd_line, "seed", "seed", 0, 
                        "Set the daemon seed to true.");

  /* Get MCA parameters, if any */
  
  mca_base_cmd_line_setup(cmd_line);

  /* Do the parsing */

  if (OMPI_SUCCESS != ompi_cmd_line_parse(cmd_line, false, argc, argv) ||
      ompi_cmd_line_is_taken(cmd_line, "help") ){
#if 1
    printf("...showing ompid help message...\n");
    printf("\nThe following optional arguments are available: --v --h --seeded\n");
    printf("\t ompid --h      is used to display this information\n");
    printf("\t ompid --v      is used to display the version of this application\n");
    printf("\t ompid --seeded is used to start a daemon seed.\n");
    printf("\n");
#else
    show_help("ompid", "usage", NULL);
#endif
    exit(1);
  }

  mca_base_cmd_line_process_args(cmd_line);

  if (OMPI_SUCCESS != (ret = mca_base_open())) {
    /* JMS show_help */
    printf("show_help: mca_base_open failed\n");
    return ret;
  }

 
  /* Execute the desired action(s) */

  if (ompi_cmd_line_is_taken(cmd_line, "version")) {
    printf ("ompid (OpenMpi Daemon) version: 0\n");
  }
 
  /* If there is a seed argument, this is the magic
   * seed daemon.
   */
  if ( ompi_cmd_line_is_taken(cmd_line, "seed") )
      ompi_process_info.seed = true;


  /* before calling anything we to call rte init */
  if (OMPI_SUCCESS != ompi_rte_init(&multi_thread, &hidden_thread)) {
        printf("ompid: ompi_rte_init failed\n");
 
	/* Do a partial clean-up.  This needs to be reviewed
	 * at a later date to make certain we are not 
	 * missing soemthing
	 */
	ompi_rte_finalize();
	ompi_cmd_line_free(cmd_line);
	mca_base_close();

        return 1;
  }

  
  /* Add in the calls to initialize the services */

  /* Add the section for the event loop... */

  /* All done */

  /* Close services */

  ompi_cmd_line_free(cmd_line);
  mca_base_close();
  ompi_finalize();
  return 0;
}
