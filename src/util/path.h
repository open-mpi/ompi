/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OMPI_PATH_H
#define OMPI_PATH_H

#include "ompi_config.h"
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

OMPI_DECLSPEC char   *ompi_path_find (char *fname, char **pathv, int mode);
OMPI_DECLSPEC char   *ompi_path_env_find (char *fname, int mode);
OMPI_DECLSPEC char   *ompi_path_findv (char *fname, char **pathv, int mode, char **envv);
OMPI_DECLSPEC char   *ompi_path_env_findv (char *fname, int mode, char **envv, char *wrkdir);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif /* OMPI_PATH_H */
