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

  char *lam_cmd_line_argv0(OPT *aod);
  char *lam_cmd_line_chosen(OPT *aod, const char *opt);
  OPT *lam_cmd_line_init(void);
  int lam_cmd_line_exam_isopt(OPT *aod, const char *opt);
  int lam_cmd_line_exam_nparams(OPT *aod, const char *opt, int *nparams);
  void lam_cmd_line_free(OPT *aod);
  int lam_cmd_line_intparam(OPT *aod, const char *opt, int inst, 
			 int idx, int *iparam);
  int lam_cmd_line_ninsts(OPT *aod, const char *opt);
  int lam_cmd_line_nparams(OPT *aod, const char *opt, int inst);
  int lam_cmd_line_ntaken(OPT *aod);
  char *lam_cmd_line_param(OPT *aod, const char *opt, int inst, int idx);
  int lam_cmd_line_parse(OPT *aod, int *argc, char **argv);
  int lam_cmd_line_setflags(OPT *aod, int flags);
  int lam_cmd_line_setopt(OPT *aod, const char *opt, const char *mutex, 
		       int np, int flags);
  int lam_cmd_line_setopt1(OPT *aod, const char *opt, const char *mutex, 
			int np, int flags);
  int lam_cmd_line_tail(OPT *aod, int *tailc, char ***tailv);
  int lam_cmd_line_taken(OPT *aod, const char *opt);
  int lam_cmd_line_unused(OPT *aod, int *unusedc, char ***unusedv);

#ifdef __cplusplus
}
#endif


#endif /* LAM_CMD_LINE */
