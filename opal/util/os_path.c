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
 * Copyright (c) 2018      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include <string.h>
#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif /* HAVE_UNISTD_H */
#ifdef HAVE_SYS_PARAM_H
#    include <sys/param.h>
#endif /* HAVE_SYS_PARAM_H */
#include <stdarg.h>
#include <stdlib.h>

#include "opal/util/os_path.h"

static const char *path_sep = OPAL_PATH_SEP;

char *opal_os_path(int relative, ...)
{
    va_list ap;
    char *element, *path;
    size_t num_elements, total_length;

    va_start(ap, relative);

    /* no way to protect ourselves from reading too far, so have to
       trust caller that they ended the list with the NULL */

    num_elements = 0;
    total_length = 0;
    while (NULL != (element = va_arg(ap, char *))) {
        num_elements++;
        total_length = total_length + strlen(element);
        if (path_sep[0] != element[0]) {
            total_length++;
        }
    }
    va_end(ap);

    if (0 == num_elements) { /* must be looking for a simple answer */
        size_t len = 3;
        path = (char *) calloc(len, sizeof(char));
        if (relative) {
            path[0] = '.';
        }
        strncat(path, path_sep, len - 1);
        return (path);
    }

    /* setup path with enough room for the string terminator, the elements, and
       the separator between each of the elements */
    total_length = total_length + num_elements * strlen(path_sep) + 1;
    if (relative) {
        total_length++;
    }

    if (total_length > OPAL_PATH_MAX) { /* path length is too long - reject it */
        return (NULL);
    }

    path = (char *) calloc(total_length, sizeof(char));
    if (NULL == path) {
        return (NULL);
    }

    if (relative) {
        path[0] = '.';
    }

    va_start(ap, relative);
    if (NULL != (element = va_arg(ap, char *))) {
        if (path_sep[0] != element[0]) {
            strncat(path, path_sep, total_length - strlen(path) - 1);
        }
        strncat(path, element, total_length - strlen(path) - 1);
    }
    while (NULL != (element = va_arg(ap, char *))) {
        if (path_sep[0] != element[0]) {
            strncat(path, path_sep, total_length - strlen(path) - 1);
        }
        strncat(path, element, total_length - strlen(path) - 1);
    }

    va_end(ap);
    return opal_make_filename_os_friendly(path);
}
