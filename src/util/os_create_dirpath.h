/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/** @file:
 * Creates a directory tree set to the specified permissions.
 *
 * The orte_os_create_dirpath() function creates a directory
 * tree, with each directory that is created in the tree having the specified
 * access permissions. Existing directories within the tree are left
 * untouched - however, if they do not permit the user to create a directory
 * within them, the function will return an error condition.
 * 
 * If the specified full path name already exists, the
 * orte_os_create_dirpath() function will check to ensure that
 * the final directory in the tree has at least the specified access permission. In other
 * words, if the directory has read-write-execute for all, and the user
 * has requested read-write access for just the user, then the function
 * will consider the directory acceptable. If the minimal permissions are
 * not currently provided, the function will attempt to change the
 * access permissions of the directory to add the specified
 * permissions. The function will return ORTE_ERROR if this cannot
 * be done.
 **/

#ifndef ORTE_OS_CREATE_DIRPATH_H
#define ORTE_SHOW_HELP_H

#include "orte_config.h"
#include <stdio.h>
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

/**
 * @param path A pointer to a string that contains the path name to be built.
 * @param mode A mode_t bit mask that specifies the access permissions for the
 * directories being constructed. 
 * @retval ORTE_SUCCESS If the directory tree has been successfully created with
 * the specified access permissions.
 * @retval ORTE_ERROR If the directory tree could not be created with the
 * specified access permissions.
 */

ORTE_DECLSPEC int orte_os_create_dirpath(const char *path, const mode_t mode);

#endif
