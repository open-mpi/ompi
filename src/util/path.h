/*
 * $HEADER$
 */

#ifndef OMPI_PATH_H
#define OMPI_PATH_H

#ifdef __cplusplus
extern "C" {
#endif

char   *ompi_path_find (char *fname, char **pathv, int mode);
char   *ompi_path_env_find (char *fname, int mode);
char   *ompi_path_findv (char *fname, char **pathv, int mode, char **envv);
char   *ompi_path_env_findv (char *fname, int mode, char **envv, char *wrkdir);

#ifdef __cplusplus
}
#endif
#endif /* OMPI_PATH_H */
