/*
 * $HEADER$
 */

/** @file:
 * Creates a directory tree set to the specified permissions.
 *
 * The ompi_os_create_dirpath() function creates a directory
 * tree, with each directory that is created in the tree having the specified
 * access permissions. Existing directories within the tree are left
 * untouched - however, if they do not permit the user to create a directory
 * within them, the function will return an error condition.
 * 
 * If the specified full path name already exists, the
 * ompi_os_create_dirpath() function will check to ensure that
 * the final directory in the tree has at least the specified access permission. In other
 * words, if the directory has read-write-execute for all, and the user
 * has requested read-write access for just the user, then the function
 * will consider the directory acceptable. If the minimal permissions are
 * not currently provided, the function will attempt to change the
 * access permissions of the directory to add the specified
 * permissions. The function will return OMPI_ERROR if this cannot
 * be done.
 **/

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>

/**
 * @param path A pointer to a string that contains the path name to be built.
 * @param mode A mode_t bit mask that specifies the access permissions for the
 * directories being constructed. 
 * @retval OMPI_SUCCESS If the directory tree has been successfully created with
 * the specified access permissions.
 * @retval OMPI_ERROR If the directory tree could not be created with the
 * specified access permissions.
 */

int ompi_os_create_dirpath(const char *path, const mode_t mode);
