/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 * @file
 */

#ifndef OPAL_PATH_H
#define OPAL_PATH_H

#include "opal_config.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

    /**
     *  Locates a file with certain permissions
     *  
     *  @param fname File name
     *  @param pathv Array of search directories
     *  @param mode  Permissions which must be satisfied (see access(2))
     *  @param envv  Pointer to string containing environment
     *
     *  @retval Full pathname of located file Success
     *  @retval NULL Failure
     *
     *  Environment variables can appear in the form $variable at the
     *  start of a prefix path and will be replaced by the environment
     *  value if it is defined; otherwise the whole prefix is ignored.
     *  Environment variables must be followed by a path delimiter or
     *  end-of-string.
     *
     * The caller is responsible for freeing the returned string.
     */
    OPAL_DECLSPEC char *opal_path_find(char *fname, char **pathv, int mode,
                                       char **envv);

    /**
     *  Locates a file with certain permissions from a list of search
     *  paths
     *
     *  @param fname File name
     *  @param mode  Target permissions which must be satisfied (see access(2))
     *  @param envv  Pointer to environment list
     *  @param wrkdir Working directory
     *
     *  @retval Full pathname of located file Success
     *  @retval NULL Failure
     *
     *  Locates a file with certain permissions from the list of paths
     *  given by the $PATH environment variable.  Replaces "." in the
     *  path with the working dir.
     *
     * The caller is responsible for freeing the returned string.
     */
    OPAL_DECLSPEC char *opal_path_findv(char *fname, int mode, 
                                        char **envv, char *wrkdir);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif /* OPAL_PATH_H */
