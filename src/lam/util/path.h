/*
 * $HEADER$
 */

#ifndef LAM_PATH_H
#define LAM_PATH_H

#ifdef __cplusplus
extern "C" {
#endif

char   *lam_path_find (char *fname, char **pathv, int mode);
char   *lam_path_env_find (char *fname, int mode);
char   *lam_path_findv (char *fname, char **pathv, int mode, char **envv);
char   *lam_path_env_findv (char *fname, int mode, char **envv, char *wrkdir);

#ifdef __cplusplus
}
#endif
#endif /* LAM_PATH_H */
