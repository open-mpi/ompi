/*
 * $HEADER$
 */

#ifndef LAM_CMD_LINE_H
#define LAM_CMD_LINE_H

/** @file **/

#include "lam_config.h"
#include "lam/lfc/list.h"
#include "lam/threads/mutex.h"


struct lam_cmd_line_t {
  /* Keep this instance safe from other threads */

  lam_mutex_t lcl_mutex;

  /* The list of possible options */

  lam_list_t lcl_options;

  /* The original argv and argc that were passed */

  int lcl_argc;
  char **lcl_argv;

  /* The resulting list of paramters after parsing the argv */

  lam_list_t lcl_params;

  /* Left over ("tail") argv */

  int lcl_tail_argc;
  char **lcl_tail_argv;
};
typedef struct lam_cmd_line_t lam_cmd_line_t;

#ifdef __cplusplus
extern "C" {
#endif
  /* Create / destroy */

  lam_cmd_line_t *lam_cmd_line_create(void);
  int lam_cmd_line_free(lam_cmd_line_t *cmd);

  /* Writes */

  int lam_cmd_line_set_opt(lam_cmd_line_t *cmd, const char *short_name, 
                           const char *long_name, int num_params, 
                           const char *desc);
  int lam_cmd_line_set_opt1(lam_cmd_line_t *cmd, const char *short_names,
                            int num_params, const char *desc);

  int lam_cmd_line_parse(lam_cmd_line_t *cmd, int *argc, char **argv);

  /* Reads */

  static inline int lam_cmd_line_get_argc(lam_cmd_line_t *cmd);
  static inline char *lam_cmd_line_get_argv(lam_cmd_line_t *cmd, int index);
  char *lam_gm_line_get_usage_msg(lam_cmd_line_t *cmd);

  bool lam_cmd_line_is_taken(lam_cmd_line_t *cmd, const char *opt);
  int lam_cmd_line_get_ninsts(lam_cmd_line_t *cmd, const char *opt);
  char *lam_cmd_line_get_param(lam_cmd_line_t *cmd, const char *opt, int inst, 
                               int idx);

  int lam_cmd_line_get_tail(lam_cmd_line_t *cmd, int *tailc, char ***tailv);
#ifdef __cplusplus
}
#endif


/**
 * Return the number of arguments parsed on a LAM command line handle.
 *
 * @param cmd A pointer to the LAM command line handle.
 *
 * @return LAM_ERROR If cmd is NULL.
 * @return argc Number of arguments previously added to the handle.
 *
 * Arguments are added to the handle via the lam_cmd_line_parse()
 * function.
 */
static inline int lam_cmd_line_get_argc(lam_cmd_line_t *cmd)
{
  return (NULL != cmd) ? cmd->lcl_argc : LAM_ERROR;
}


static char *lam_cmd_line_get_argv(lam_cmd_line_t *cmd, int index)
{
  return (NULL != cmd) ? cmd->lcl_argv : LAM_ERROR;
}

#endif /* LAM_CMD_LINE_H */
