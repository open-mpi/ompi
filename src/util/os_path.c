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

#include "ompi_config.h"

#include <string.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_LIBGEN_H
#include <libgen.h>
#endif
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#include <stdlib.h>
#include <stdarg.h>

#include "include/constants.h"
#include "util/os_path.h"
#include "util/sys_info.h"

char *ompi_os_path(bool relative, ...)
{
    va_list ap, ap1;
    char *element, *path;
    int num_elements, total_length;

    va_start(ap, relative);
    va_start(ap1, relative);

    /* make sure system info is filled and separator is non-NULL */
    ompi_sys_info();
    if (NULL == ompi_system_info.path_sep) {
	return(NULL);
    }

    /* no way to protect ourselves from reading too far, so have to trust caller
       that they ended the list with the NULL */

    num_elements = 0;
    total_length = 0;
    while (NULL != (element=va_arg(ap, char*))) {
	num_elements++;
	total_length = total_length + strlen(element);
    }

    if (0 == num_elements) { /* must be looking for a simple answer */
	path = (char *)malloc(2);
        path[0] = 0;
	if (relative) {
	    strcpy(path, ".");
        strcat(path, ompi_system_info.path_sep);
	}
	else {
#ifndef WIN32
	    strcpy(path, ompi_system_info.path_sep);
#endif
	}
	return(path);
    }

    /* setup path with enough room for the string terminator, the elements, and
       the separator between each of the elements */
    total_length = total_length + num_elements + 1;
    if (total_length > MAXPATHLEN) {  /* path length is too long - reject it */
	return(NULL);
    }

    path = (char *)malloc(2 + total_length + num_elements-1);
    if (NULL == path) {
	return(NULL);
    }
    path[0] = 0;

    if (relative) {
	strcpy(path, ".");
    }

    /* get the first element here so that we don't have duplicate first
       seperators */
    if (NULL != (element = va_arg(ap1, char*))) {
        strcat(path, element);    
    }

    while (NULL != (element=va_arg(ap1, char*))) {
    	strcat(path, ompi_system_info.path_sep);
        strcat(path, element);
    }

    va_end(ap);
    va_end(ap1);
    return(path);
}
