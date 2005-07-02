/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdlib.h>
#include <string.h>

#include "include/constants.h"
#include "util/basename.h"


char *ompi_basename(const char *filename)
{
    size_t i;
    char *tmp, *ret = NULL;
#ifdef WIN32
    const char sep = '\\';
#else
    const char sep = '/';
#endif

    /* Check for the bozo case */

    if (NULL == filename) {
        return NULL;
    }

    /* On Windows, automatically exclude a drive designator */

#ifdef WIN32
    if (strlen(filename) == 2 &&
        isalpha(filename[0]) && ':' == filename[1]) {
        return strdup(filename);
    } else if (strlen(filename) == 3 &&
        isalpha(filename[0]) && ':' == filename[1] && sep == filename[2]) {
        return strdup(filename);
    }

    if (':' == filename[1] && isalpha(filename[0])) {
        filename += 2;
        if (sep == filename[0]) {
            ++filename;
        }
    }
#endif

    /* Check for the bozo cases */

    if (0 == strlen(filename)) {
        return strdup("");
    }
    if (sep == filename[0] && '\0' == filename[1]) {
        return strdup(filename);
    }

    /* Remove trailing sep's (note that we already know that strlen > 0) */

    tmp = strdup(filename);
    for (i = strlen(tmp) - 1; i > 0; --i) {
        if (sep == tmp[i]) {
            tmp[i] = '\0';
        } else {
            break;
        }
    }
    if (0 == i) {
        tmp[0] = sep;
        return tmp;
    }

    /* Look for the final sep */

    ret = strrchr(tmp, sep);
    if (NULL == ret) {
        return tmp;
    }
    ret = strdup(ret + 1);
    free(tmp);
    return ret;
}
