/*
 * $HEADER$
 */

/** @file **/

#ifndef OMPI_CMD_LINE_H
#define OMPI_CMD_LINE_H

#include "ompi_config.h"
#include "include/constants.h"
#include "class/ompi_list.h"
#include "threads/mutex.h"
#include "util/argv.h"

/*
 * Top-level descriptor
 */
struct ompi_cmd_line_t {
  ompi_mutex_t lcl_mutex;

  /* List of cmd_line_option_t's, below */

  ompi_list_t lcl_options;

  /* Duplicate of the argc / argv passed in to ompi_cmd_line_parse() */

  int lcl_argc;
  char **lcl_argv;

  /* Parsed output; list of cmd_line_param_t's, below */

  ompi_list_t lcl_params;

  /* List of tail (unprocessed) arguments */

  int lcl_tail_argc;
  char **lcl_tail_argv;
};
typedef struct ompi_cmd_line_t ompi_cmd_line_t;


#ifdef __cplusplus
extern "C" {
#endif
  /* Create / destroy */

  ompi_cmd_line_t *ompi_cmd_line_create(void);
  void ompi_cmd_line_free(ompi_cmd_line_t *cmd);

  /* Writes */

  int ompi_cmd_line_make_opt(ompi_cmd_line_t *cmd, char short_name, 
                            const char *long_name, int num_params, 
                            const char *desc);

  int ompi_cmd_line_parse(ompi_cmd_line_t *cmd, bool ignore_unknown,
                         int argc, char **argv);

  /* Reads */

  static inline int ompi_cmd_line_get_argc(ompi_cmd_line_t *cmd);
  static inline char *ompi_cmd_line_get_argv(ompi_cmd_line_t *cmd, int index);

  char *ompi_cmd_line_get_usage_msg(ompi_cmd_line_t *cmd);
  bool ompi_cmd_line_is_taken(ompi_cmd_line_t *cmd, const char *opt);
  int ompi_cmd_line_get_ninsts(ompi_cmd_line_t *cmd, const char *opt);
  char *ompi_cmd_line_get_param(ompi_cmd_line_t *cmd, const char *opt, 
                               int instance_num, int param_num);

  static inline int ompi_cmd_line_get_tail(ompi_cmd_line_t *cmd, int *tailc, 
                                          char ***tailv);
#ifdef __cplusplus
}
#endif


/**
 * Return the number of arguments parsed on a OMPI command line handle.
 *
 * @param cmd A pointer to the OMPI command line handle.
 *
 * @retval OMPI_ERROR If cmd is NULL.
 * @retval argc Number of arguments previously added to the handle.
 *
 * Arguments are added to the handle via the ompi_cmd_line_parse()
 * function.
 */
static inline int ompi_cmd_line_get_argc(ompi_cmd_line_t *cmd)
{
  return (NULL != cmd) ? cmd->lcl_argc : OMPI_ERROR;
}


/**
 * Return a string argument parsed on a OMPI command line handle.
 *
 * @param cmd A pointer to the OMPI command line handle.
 * @param index The nth argument from the command line (0 is argv[0], etc.).
 *
 * @retval NULL If cmd is NULL or index is invalid
 * @retval argument String of original argv[index]
 *
 * This function returns a single token from the arguments parsed on
 * this handle.  Arguments are added bia the ompi_cmd_line_parse()
 * function.
 *
 * What is returned is a pointer to the actual string that is on the
 * handle; it should not be modified or freed.
 */
static inline char *ompi_cmd_line_get_argv(ompi_cmd_line_t *cmd, int index)
{
  return (NULL == cmd) ? NULL :
    (index >= cmd->lcl_argc || index < 0) ? NULL : cmd->lcl_argv[index];
}


/**
 * Return the entire "tail" of unprocessed argv from a OMPI command
 * line handle.
 *
 * @param cmd A pointer to the OMPI command line handle.
 * @param tailc Pointer to the output length of the null-terminated
 * tail argv array.
 * @param tailv Pointer to the output null-terminated argv of all
 * unprocessed arguments from the command line.
 *
 * @retval OMPI_ERROR If cmd is NULL or otherwise invalid.
 * @retval OMPI_SUCCESS Upon success.
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
 * ompi_argv_free()) by the caller.
 */
static inline int ompi_cmd_line_get_tail(ompi_cmd_line_t *cmd, int *tailc, 
                                        char ***tailv)
{
  if (NULL != cmd) {
    ompi_mutex_lock(&cmd->lcl_mutex);
    *tailc = cmd->lcl_tail_argc;
    *tailv = ompi_argv_copy(cmd->lcl_tail_argv);
    ompi_mutex_unlock(&cmd->lcl_mutex);
    return OMPI_SUCCESS;
  } else {
    return OMPI_ERROR;
  }
}

#endif /* OMPI_CMD_LINE_H */
