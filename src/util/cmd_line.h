/*
 * $HEADER$
 */

/** @file **/

#ifndef LAM_CMD_LINE_H
#define LAM_CMD_LINE_H

#include "lam_config.h"
#include "include/constants.h"
#include "lfc/lam_list.h"
#include "threads/mutex.h"
#include "util/argv.h"

/*
 * Top-level descriptor
 */
struct lam_cmd_line_t {
  lam_mutex_t lcl_mutex;

  /* List of cmd_line_option_t's, below */

  lam_list_t lcl_options;

  /* Duplicate of the argc / argv passed in to lam_cmd_line_parse() */

  int lcl_argc;
  char **lcl_argv;

  /* Parsed output; list of cmd_line_param_t's, below */

  lam_list_t lcl_params;

  /* List of tail (unprocessed) arguments */

  int lcl_tail_argc;
  char **lcl_tail_argv;
};
typedef struct lam_cmd_line_t lam_cmd_line_t;


#ifdef __cplusplus
extern "C" {
#endif
  /* Create / destroy */

  lam_cmd_line_t *lam_cmd_line_create(void);
  void lam_cmd_line_free(lam_cmd_line_t *cmd);

  /* Writes */

  int lam_cmd_line_make_opt(lam_cmd_line_t *cmd, char short_name, 
                            const char *long_name, int num_params, 
                            const char *desc);

  int lam_cmd_line_parse(lam_cmd_line_t *cmd, bool ignore_unknown,
                         int argc, char **argv);

  /* Reads */

  static inline int lam_cmd_line_get_argc(lam_cmd_line_t *cmd);
  static inline char *lam_cmd_line_get_argv(lam_cmd_line_t *cmd, int index);

  char *lam_cmd_line_get_usage_msg(lam_cmd_line_t *cmd);
  bool lam_cmd_line_is_taken(lam_cmd_line_t *cmd, const char *opt);
  int lam_cmd_line_get_ninsts(lam_cmd_line_t *cmd, const char *opt);
  char *lam_cmd_line_get_param(lam_cmd_line_t *cmd, const char *opt, 
                               int instance_num, int param_num);

  static inline int lam_cmd_line_get_tail(lam_cmd_line_t *cmd, int *tailc, 
                                          char ***tailv);
#ifdef __cplusplus
}
#endif


/**
 * Return the number of arguments parsed on a LAM command line handle.
 *
 * @param cmd A pointer to the LAM command line handle.
 *
 * @retval LAM_ERROR If cmd is NULL.
 * @retval argc Number of arguments previously added to the handle.
 *
 * Arguments are added to the handle via the lam_cmd_line_parse()
 * function.
 */
static inline int lam_cmd_line_get_argc(lam_cmd_line_t *cmd)
{
  return (NULL != cmd) ? cmd->lcl_argc : LAM_ERROR;
}


/**
 * Return a string argument parsed on a LAM command line handle.
 *
 * @param cmd A pointer to the LAM command line handle.
 * @param index The nth argument from the command line (0 is argv[0], etc.).
 *
 * @retval NULL If cmd is NULL or index is invalid
 * @retval argument String of original argv[index]
 *
 * This function returns a single token from the arguments parsed on
 * this handle.  Arguments are added bia the lam_cmd_line_parse()
 * function.
 *
 * What is returned is a pointer to the actual string that is on the
 * handle; it should not be modified or freed.
 */
static inline char *lam_cmd_line_get_argv(lam_cmd_line_t *cmd, int index)
{
  return (NULL == cmd) ? NULL :
    (index >= cmd->lcl_argc || index < 0) ? NULL : cmd->lcl_argv[index];
}


/**
 * Return the entire "tail" of unprocessed argv from a LAM command
 * line handle.
 *
 * @param cmd A pointer to the LAM command line handle.
 * @param tailc Pointer to the output length of the null-terminated
 * tail argv array.
 * @param tailv Pointer to the output null-terminated argv of all
 * unprocessed arguments from the command line.
 *
 * @retval LAM_ERROR If cmd is NULL or otherwise invalid.
 * @retval LAM_SUCCESS Upon success.
 *
 * The "tail" is all the arguments on the command line that were not
 * processed for some reason.  Reasons for not processing arguments
 * include:
 *
 * \sa The argument was not recognized
 * \sa The argument "--" was seen, and therefore all arguments
 * following it were not processed
 *
 * The output tailc parameter will be filled in with the integer
 * length of the null-terminated tailv array (length including the
 * final NULL entry).  The output tailv parameter will be a copy of
 * the tail parameters, and must be freed (likely with a call to
 * lam_argv_free()) by the caller.
 */
static inline int lam_cmd_line_get_tail(lam_cmd_line_t *cmd, int *tailc, 
                                        char ***tailv)
{
  if (NULL != cmd) {
    lam_mutex_lock(&cmd->lcl_mutex);
    *tailc = cmd->lcl_tail_argc;
    *tailv = lam_argv_copy(cmd->lcl_tail_argv);
    lam_mutex_unlock(&cmd->lcl_mutex);
    return LAM_SUCCESS;
  } else {
    return LAM_ERROR;
  }
}

#endif /* LAM_CMD_LINE_H */
