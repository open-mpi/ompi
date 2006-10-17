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
 */

#include "opal_config.h"

#include <stdlib.h>
#include <string.h>
#if HAVE_DIRNAME
#include <libgen.h>
#endif  /* HAVE_DIRNAME */

#include "opal/util/basename.h"
#include "opal/util/os_path.h"
#include "opal/constants.h"

/**
 * Return a pointer into the original string where the last PATH delimiter
 * was found. It does not modify the original string. Moreover, it does not
 * scan the full string, but only the part allowed by the specified number
 * of characters.
 * If the last character on the string is a path separator, it will be skipped.
 */
static inline char* opal_find_last_path_separator( const char* filename, size_t n )
{
    char* p = (char*)filename + n;

    /* First skip the latest separators */
    for( ; p != filename; p-- ) {
#if defined(__WINDOWS__)
        if( (*p != '\\') && (*p != '/') )
            break;
#else
        if( *p != OPAL_PATH_SEP[0] )
            break;
#endif  /* defined(__WINDOWS__) */
    }

    for( ; p != filename; p-- ) {
#if defined(__WINDOWS__)
        if( (*p == '\\') || (*p == '/') )
            return p;
#else
        if( *p == OPAL_PATH_SEP[0] )
            return p;
#endif  /* defined(__WINDOWS__) */
    }

    return NULL;  /* nothing found inside the filename */
}

char *opal_basename(const char *filename)
{
    size_t i;
    char *tmp, *ret = NULL;
    const char sep = OPAL_PATH_SEP[0];

    /* Check for the bozo case */

    if (NULL == filename) {
        return NULL;
    }

    /* On Windows, automatically exclude a drive designator */

#ifdef __WINDOWS__
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
    return opal_make_filename_os_friendly(ret);
}

char* opal_dirname(const char* filename)
{
#if defined(HAVE_DIRNAME)
    char* safe_tmp = strdup(filename), *result;
    result = strdup(dirname(safe_tmp));
    free(safe_tmp);
    return result;
#else
    const char* p = opal_find_last_path_separator(filename, strlen(filename));

    for( ; p != filename; p-- ) {
        if( (*p == '\\') || (*p == '/') ) {
            /* If there are several delimiters remove them all */
            for( --p; p != filename; p-- ) {
                if( (*p != '\\') && (*p != '/') ) {
                    p++;
                    break;
                }
            }
            if( p != filename ) {
                char* ret = (char*)malloc( p - filename + 1 );
                strncpy_s( ret, (p - filename + 1), filename, p - filename );
                ret[p - filename] = '\0';
                return opal_make_filename_os_friendly(ret);
            }
            break;  /* return the duplicate of "." */
        }
    }
    return _strdup(".");
#endif  /* defined(HAVE_DIRNAME) */
}
