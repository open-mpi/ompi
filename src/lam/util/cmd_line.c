/*
 * $HEADER$
 */

/** @file **/

#include "lam_config.h"

#include <stdio.h>
#include <string.h>

#include "lam/lfc/object.h"
#include "lam/lfc/list.h"
#include "lam/util/malloc.h"
#include "lam/util/argv.h"
#include "lam/util/cmd_line.h"
#include "lam/util/strncpy.h"


/*
 * Description of a command line option
 */
typedef struct lam_cmd_line_option {
  lam_list_item_t super;

  char lclo_short_name;
  char *lclo_long_name;
  char *lclo_description;

  int lclo_num_params;
} lam_cmd_line_option_t;

/*
 * An option that was used in the argv that was parsed
 */
typedef struct lam_cmd_line_param {
  lam_list_item_t super;

  lam_cmd_line_option_t *lcl_option;
  int lcl_num_params;
  char **lcl_params;

  int lcll_num_params;
} lam_cmd_line_param_t;


/**
 * Create a LAM command line handle.
 *
 * @return NULL Upon failure.
 * @return cmd A pointer to an empty command line handle upon success.
 *
 * This is the first function invoked to start command line parsing.
 * It creates an independant handle that is used in all other
 * lam_cmd_line*() functions.  Multiple handles can be created and
 * simultaneously processed; each handle is independant from others.
 *
 * The lam_cmd_line_t handles are [simplisticly] thread safe;
 * processing is guaranteed to be mutually exclusive if multiple
 * threads invoke functions on the same handle at the same time --
 * access will be serialized in an unspecified order.
 *
 * It is necessary to call lam_cmd_line_free() with the handle
 * returned from this function in order to free all memory associated
 * with it.
 */
lam_cmd_line_t *lam_cmd_line_create(void)
{
  lam_cmd_line_t *cmd = LAM_MALLOC(sizeof(lam_cmd_line_t));

  if (NULL == cmd)
    return NULL;

  /* Initialize the mutex.  Since we're creating (and therefore the
     only thread that has this instance), there's no need to lock it
     right now. */

  lam_mtx_init(&cmd->lcl_mutex);

  /* Initialize the lists */

  lam_list_init(&cmd->lcl_options);
  lam_list_init(&cmd->lcl_params);

  /* Initialize the argc/argv pairs */

  cmd->lcl_argc = 0;
  cmd->lcl_argv = NULL;
  cmd->lcl_tail_argc = 0;
  cmd->lcl_tail_argv = NULL;

  /* All done */

  return cmd;
}


/**
 * Free a (lam_cmd_line_t*) that was initially allocated via
 * lam_cmd_line_create().
 *
 * @param cmd A pointer to the LAM command line handle to free.
 *
 * @return LAM_SUCCESS Upon success.
 *
 * This function frees a LAM command line handle and all its
 * associated memory.  This function can be called with any non-NULL
 * pointer that was returned from lam_cmd_line_create().  Once called,
 * the handle is no longer valid.
 *
 * Access into this function is not thread safe.  Since, by
 * definition, calling this function will destroy the handle, it does
 * not make sense to try to have thread A invoke a different
 * lam_cmd_line*() function and thread B invoke lam_cmd_line_free() --
 * even if the calls are serialized, there's a race condition where
 * thread A may try to use a now-invalid handle.
 */
int lam_cmd_line_free(lam_cmd_line_t *cmd)
{
#if 0
  /* We don't lock the mutex; there's no point.  Just free it. */

  lam_mtx_free(&cmd->lcl_mutex);
#endif

  /* Free the lists */

  lam_list_destroy(&cmd->lcl_options);
  lam_list_destroy(&cmd->lcl_params);

  /* Free the argv's */

  if (NULL != cmd->lcl_argv)
    lam_argv_free(cmd->lcl_argv);
  if (NULL != cmd->lcl_tail_argv)
    lam_argv_free(cmd->lcl_tail_argv);

  /* All done */

  return LAM_SUCCESS;
}


/* JMS Continue here.... */
int lam_cmd_line_set_opt(lam_cmd_line_t *cmd, const char *short_name, 
                         const char *long_name, int num_params, 
                         const char *desc)
{
  /* All done */

  return LAM_SUCCESS;
}

int lam_cmd_line_set_opt1(lam_cmd_line_t *cmd, const char *short_names,
                          int num_params, const char *desc)
{
  /* All done */

  return LAM_SUCCESS;
}


int lam_cmd_line_parse(lam_cmd_line_t *cmd, int *argc, char **argv)
{
  /* All done */

  return LAM_SUCCESS;
}


char *lam_gm_line_get_usage_msg(lam_cmd_line_t *cmd)
{
  /* All done */

  return NULL;
}


bool lam_cmd_line_is_taken(lam_cmd_line_t *cmd, const char *opt)
{
  /* All done */

  return true;
}


int lam_cmd_line_get_ninsts(lam_cmd_line_t *cmd, const char *opt)
{
  /* All done */

  return LAM_SUCCESS;
}


char *lam_cmd_line_get_param(lam_cmd_line_t *cmd, const char *opt, int inst, 
                             int idx)
{
  /* All done */

  return NULL;
}

