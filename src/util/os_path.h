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
 * Creates an operating system-acceptable path name.
 *
 * The orte_os_path() function takes a variable number of string arguments and
 * concatenates them into a path name using the path separator character appropriate
 * to the local operating system. NOTE: the string returned by this function has been
 * malloc'd - thus, the user is responsible for free'ing the memory used by
 * the string.
 *
 * CRITICAL NOTE: The input variable list MUST be terminated by a NULL value. Failure
 * to do this will cause the program to suffer a catastrophic failure - usually a
 * segmentation violation or bus error.
 *
 * The function calls orte_sys_info() to ensure that the path separator character
 * has been identified. If that value cannot be identified for some reason,
 * the function will return a NULL value. Likewise, specifying a path name that
 * exceeds the maximum allowable path name length on the local system will result
 * in the return of a NULL value.
 *
 *
 */

#include "orte_config.h"
#include <stdio.h>
#include <stdarg.h>

/** 
 * @param relative A boolean that specifies if the path name is to be constructed
 * relative to the current directory or as an absolute path. If no path
 * elements are included in the function call, then the function returns
 * "." for a relative path name and "<path separator char>" - 
 * the top of the directory tree - for an absolute path name.
 * @param elem1,elem2,... A variable number of (char *)path_elements
 * can be provided to the function, terminated by a NULL value. These
 * elements will be concatenated, each separated by the path separator
 * character, into a path name and returned.
 * @retval path_name A pointer to a fully qualified path name composed of the
 * provided path elements, separated by the path separator character
 * appropriate to the local operating system. The path_name string has been malloc'd
 * and therefore the user is responsible for free'ing the field.
*/
ORTE_DECLSPEC char *orte_os_path(bool relative, ...);
