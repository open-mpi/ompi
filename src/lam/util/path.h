/*
 * $HEADER$
 */

#ifndef LAM_PATH_H
#define LAM_PATH_H

char   *lam_path_find (char *, char **, int);
char   *lam_path_env_find (char *, int);
char   *lam_path_findv (char *, char **, int, char **);
char   *lam_path_env_findv (char *, int, char **, char *);

#endif /* LAM_PATH_H */
