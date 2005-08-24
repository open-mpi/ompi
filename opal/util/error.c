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

#include <string.h>
#include <errno.h>
#include <stdio.h>

#include "opal/util/error.h"
#include "opal/include/constants.h"

#define MAX_CONVERTERS 5
/* all default to NULL */
opal_err2str_fn_t converters[MAX_CONVERTERS];

static const char *
opal_strerror_int(int errnum)
{
    int i;
    const char *ret = NULL;

    for (i = 0 ; i < MAX_CONVERTERS ; ++i) {
        if (NULL != converters[i]) {
            ret = converters[i](errnum);
            if (NULL != ret) break;
        }
    }

    return ret;
}


void
opal_perror(int errnum, const char *msg)
{
    const char* errmsg = opal_strerror_int(errnum);

    if (NULL != msg && errnum != OPAL_ERR_IN_ERRNO) {
        fprintf(stderr, "%s: ", msg);
    }

    if (NULL == errmsg) {
        if (errnum == OPAL_ERR_IN_ERRNO) {
            perror(msg);
        } else {
            fprintf(stderr, "Unknown error: %d\n", errnum);
        }
    } else {
        fprintf(stderr, "%s\n", errmsg);
    }

    fflush(stderr);
}

/* size of "Unknow error: " + 3 digits of errnumber */
static char unknown_retbuf[20];

const char *
opal_strerror(int errnum)
{
    const char* errmsg = opal_strerror_int(errnum);

    if (NULL == errmsg) {
        if (errnum == OPAL_ERR_IN_ERRNO) {
            return strerror(errno);
        } else {
            errno = EINVAL;
            snprintf(unknown_retbuf, 20, "Unknown error: %d", errnum);
            return (const char*) unknown_retbuf;
        }
    } else {
        return errmsg;
    }
}


int
opal_strerror_r(int errnum, char *strerrbuf, size_t buflen)
{
    const char* errmsg = opal_strerror_int(errnum);
    int ret;

    if (NULL == errmsg) {
        if (errnum == OPAL_ERR_IN_ERRNO) {
            char *tmp = strerror(errno);
            strncpy(strerrbuf, tmp, buflen);
            return OPAL_SUCCESS;
        } else {
            errno = EINVAL;
            ret =  snprintf(strerrbuf, buflen, "Unknown error: %d", errnum);
            if (ret > (int) buflen) {
                errno = ERANGE;
                return OPAL_ERR_OUT_OF_RESOURCE;
            } else {
                return OPAL_SUCCESS;
            }
        }
    } else {
        ret =  snprintf(strerrbuf, buflen, "%s", errmsg);
        if (ret > (int) buflen) {
            errno = ERANGE;
            return OPAL_ERR_OUT_OF_RESOURCE;
        } else {
            return OPAL_SUCCESS;
        }
    }
}


int
opal_error_register(opal_err2str_fn_t converter)
{
    int i;

    for (i = 0 ; i < MAX_CONVERTERS ; ++i) {
        if (NULL == converters[i]) {
            converters[i] = converter;
            return OPAL_SUCCESS;
        }
    }

    return OPAL_ERR_OUT_OF_RESOURCE;
}
