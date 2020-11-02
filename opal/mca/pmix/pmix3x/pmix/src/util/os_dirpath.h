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
 * Copyright (c) 2019-2020 Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/** @file:
 * Creates a directory tree set to the specified permissions.
 *
 * The pmix_os_dirpath_create() function creates a directory
 * tree, with each directory that is created in the tree having the specified
 * access permissions. Existing directories within the tree are left
 * untouched - however, if they do not permit the user to create a directory
 * within them, the function will return an error condition.
 *
 * If the specified full path name already exists, the
 * pmix_os_dirpath_create() function will check to ensure that
 * the final directory in the tree has at least the specified access permission. In other
 * words, if the directory has read-write-execute for all, and the user
 * has requested read-write access for just the user, then the function
 * will consider the directory acceptable. If the minimal permissions are
 * not currently provided, the function will attempt to change the
 * access permissions of the directory to add the specified
 * permissions. The function will return PMIX_ERROR if this cannot
 * be done.
 **/

#ifndef PMIX_OS_DIRPATH_CREATE_H
#define PMIX_OS_DIRPATH_CREATE_H

#include "pmix_config.h"
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

BEGIN_C_DECLS

/**
 * @param path A pointer to a string that contains the path name to be built.
 * @param mode A mode_t bit mask that specifies the access permissions for the
 * directories being constructed.
 * @retval PMIX_SUCCESS If the directory tree has been successfully created with
 * the specified access permissions.
 * @retval PMIX_ERROR If the directory tree could not be created with the
 * specified access permissions.
 */

PMIX_EXPORT int pmix_os_dirpath_create(const char *path, const mode_t mode);

/**
 * Check to see if a directory is empty
 *
 * @param path A pointer to a string that contains the path name to be checked.
 *
 * @retval true If the directory is empty
 * @retval false If the directory is not empty
 */
PMIX_EXPORT bool pmix_os_dirpath_is_empty(const char *path);

/**
 * Check access to the directory
 *
 * @param path A pointer to a string that contains the path name to be checked.
 * @param mode A mode_t bit mask that specifies the access permissions for the
 *             directory to be accessed.
 *
 * @retval PMIX_SUCCESS If directory exists, and permissions match
 * @retval PMIX_ERR_NOT_FOUND If directory does not exist
 * @retval PMIX_ERROR   If directory exists, and permissions do not match
 */
PMIX_EXPORT int pmix_os_dirpath_access(const char *path, const mode_t mode );

/**
 * Callback for pmix_os_dirpath_destroy(). Call for every file/directory before
 * taking action to remove/unlink it.
 *
 * @param root A pointer to a string that contains the base path name (e.g., /tmp/foo from /tmp/foo/bar)
 * @param path A pointer to a string that contains the file or directory (e.g., bar from /tmp/foo/bar)
 *
 * @retval true  Allow the program to remove the file/directory
 * @retval false Do not allow the program to remove the file/directory
 */
typedef bool (*pmix_os_dirpath_destroy_callback_fn_t)(const char *root, const char *path);

/**
 * Destroy a directory
 *
 * @param path A pointer to a string that contains the path name to be destroyed
 * @param recursive Recursively descend the directory removing all files and directories.
 *                  if set to 'false' then the directory must be empty to succeed.
 * @param cbfunc A function that will be called before removing a file or directory.
 *               If NULL, then assume all remove.
 *
 * @retval PMIX_SUCCESS If the directory was successfully removed or removed to the
 *                      specification of the user (i.e., obeyed the callback function).
 * @retval PMIX_ERR_NOT_FOUND If directory does not exist.
 * @retval PMIX_ERROR If the directory cannnot be removed, accessed properly, or contains
 *                    directories that could not be removed..
 */
PMIX_EXPORT int pmix_os_dirpath_destroy(const char *path,
                                          bool recursive,
                                          pmix_os_dirpath_destroy_callback_fn_t cbfunc);

END_C_DECLS

#endif
