/*
 * $HEADER$
 */

#ifndef LAM_CMD_LINE_H
#define LAM_CMD_LINE_H

typedef struct lam_cmd_line {
  /* JMS Will fill things in here later */
} lam_cmd_line_t;

#ifdef __cplusplus
extern "C" {
#endif

  char *lam_cmd_line_argv0(lam_cmd_line_t *aod);
  char *lam_cmd_line_chosen(lam_cmd_line_t *aod, const char *opt);
  lam_cmd_line_t *lam_cmd_line_init(void);
  int lam_cmd_line_exam_isopt(lam_cmd_line_t *aod, const char *opt);
  int lam_cmd_line_exam_nparams(lam_cmd_line_t *aod, const char *opt, 
                                int *nparams);
  void lam_cmd_line_free(lam_cmd_line_t *aod);
  int lam_cmd_line_intparam(lam_cmd_line_t *aod, const char *opt, int inst, 
                            int idx, int *iparam);
  int lam_cmd_line_ninsts(lam_cmd_line_t *aod, const char *opt);
  int lam_cmd_line_nparams(lam_cmd_line_t *aod, const char *opt, int inst);
  int lam_cmd_line_ntaken(lam_cmd_line_t *aod);
  char *lam_cmd_line_param(lam_cmd_line_t *aod, const char *opt, int inst, 
                           int idx);
  int lam_cmd_line_parse(lam_cmd_line_t *aod, int *argc, char **argv);
  int lam_cmd_line_setflags(lam_cmd_line_t *aod, int flags);
  int lam_cmd_line_setopt(lam_cmd_line_t *aod, const char *opt, 
                          const char *mutex, int np, int flags);
  int lam_cmd_line_setopt1(lam_cmd_line_t *aod, const char *opt, 
                           const char *mutex, int np, int flags);
  int lam_cmd_line_tail(lam_cmd_line_t *aod, int *tailc, char ***tailv);
  int lam_cmd_line_taken(lam_cmd_line_t *aod, const char *opt);
  int lam_cmd_line_unused(lam_cmd_line_t *aod, int *unusedc, char ***unusedv);

#ifdef __cplusplus
}
#endif


#endif /* LAM_CMD_LINE */
