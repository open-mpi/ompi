/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>
#include <string.h>

#include "include/constants.h"
#include "util/cmd_line.h"
#include "util/argv.h"
#include "mca/base/base.h"


/* 
 * Private variables
 */
static int mca_param_argc = 0;
static char **mca_param_argv = NULL;
static int mca_value_argc = 0;
static char **mca_value_argv = NULL;


/*
 * Add -mca to the possible command line options list
 */
int mca_base_cmd_line_setup(ompi_cmd_line_t *cmd)
{
  return ompi_cmd_line_make_opt3(cmd, '\0', "mca", "mca", 2,
                                 "General mechanism to pass MCA parameters");
}


/*
 * Look for and handle any -mca options on the command line
 */
int mca_base_cmd_line_process_args(ompi_cmd_line_t *cmd)
{
  int i, num_insts;
  char *buf, *name;

  /* If no "-mca" parameters were given, just return */

  if (!ompi_cmd_line_is_taken(cmd, "mca")) {
      return OMPI_SUCCESS;
  }

  /* Otherwise, assemble them into an argc/argv */

  num_insts = ompi_cmd_line_get_ninsts(cmd, "mca");
  for (i = 0; i < num_insts; ++i) {
      mca_base_cmd_line_process_arg(ompi_cmd_line_get_param(cmd, "mca", i, 0), 
                                    ompi_cmd_line_get_param(cmd, "mca", i, 1));
  }

  /* Now put that argc/argv in the environment */

  if (NULL == mca_param_argv) {
      return OMPI_SUCCESS;
  }

  /* Loop through all the -mca args that we've gotten and make env
     vars of the form OMPI_MCA_*=value.  This is a memory leak, but
     that's how putenv works.  :-( */

  for (i = 0; NULL != mca_param_argv[i]; ++i) {
      name = mca_base_param_environ_variable(mca_param_argv[i], NULL, NULL);
      asprintf(&buf, "%s=%s", name, mca_value_argv[i]);
      putenv(buf);
      free(name);
  }

  return OMPI_SUCCESS;
}


/*
 * Process a single MCA argument.  Done as a separate function so that
 * top-level applications can directly invoke this to effect MCA
 * command line arguments.  
 */
int mca_base_cmd_line_process_arg(const char *param, const char *value)
{
  int i, len;
  char *new_str;

  /* Look to see if we've already got an -mca argument for the same
     param.  Check against the list of MCA param's that we've already
     saved arguments for. */

  for (i = 0; NULL != mca_param_argv && NULL != mca_param_argv[i]; ++i) {
    if (0 == strcmp(param, mca_param_argv[i])) {
      len = strlen(value) + strlen(mca_param_argv[i]);
      new_str = malloc(len);
      snprintf(new_str, len, "%s,%s", mca_value_argv[i], value);
      free(mca_value_argv[i]);
      mca_value_argv[i] = new_str;

      return OMPI_SUCCESS;
    }
  }

  /* If we didn't already have an value for the same param, save this
     one away */
  
  ompi_argv_append(&mca_param_argc, &mca_param_argv, param);
  ompi_argv_append(&mca_value_argc, &mca_value_argv, value);

  return OMPI_SUCCESS;
}
