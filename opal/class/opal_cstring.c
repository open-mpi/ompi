/*
 * Copyright (c) 2020      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2020      High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_cstring.h"

#include "opal/constants.h"
#include "opal/util/string_copy.h"
#include <ctype.h>
#include <errno.h>
#include <stddef.h>

static void opal_cstring_ctor(opal_cstring_t *obj);

OBJ_CLASS_INSTANCE(opal_cstring_t, opal_object_t, &opal_cstring_ctor, NULL);

/* make sure we have sufficient padding to always null-terminate the string */
#if (__STDC_VERSION__ >= 201112L)
_Static_assert(sizeof(opal_cstring_t) > offsetof(opal_cstring_t, string),
               "Insufficient padding available in opal_cstring_t");
#endif // (__STDC_VERSION__ >= 201112L)

static void opal_cstring_ctor(opal_cstring_t *obj)
{
    *(size_t *) &(obj->length) = 0;
    /* make sure the string is null-terminated */
    ((char *) obj->string)[0] = '\0';
}

static inline size_t opal_cstring_alloc_size(size_t len)
{
    /* the size required for the object and the string, incl. the null-terminator */
    size_t res = sizeof(opal_cstring_t) + len + 1;
    /* adjust for the additional padding that is used for the string anyway */
    res -= (sizeof(opal_cstring_t) - offsetof(opal_cstring_t, string));
    /* make sure we allocate at least sizeof(opal_cstring_t) */
    return (res > sizeof(opal_cstring_t)) ? res : sizeof(opal_cstring_t);
}

opal_cstring_t *opal_cstring_create_l(const char *string, size_t len)
{
    if (NULL == string || 0 == len) {
        return OBJ_NEW(opal_cstring_t);
    }

    /* Allocate space for the object, the characters in \c string and the terminating null */
    opal_cstring_t *res = (opal_cstring_t *) malloc(opal_cstring_alloc_size(len));
    if (NULL == res) {
        return NULL;
    }
    OBJ_CONSTRUCT(res, opal_cstring_t);

    /* cast away const for setting the member values */
    *(size_t *) &(res->length) = len;
    opal_string_copy((char *) res->string, string, len + 1);

    return res;
}

opal_cstring_t *opal_cstring_create(const char *string)
{
    if (NULL == string) {
        return OBJ_NEW(opal_cstring_t);
    }
    size_t len = strlen(string);
    return opal_cstring_create_l(string, len);
}

int opal_cstring_to_int(opal_cstring_t *string, int *interp)
{
    long tmp;
    char *endp;

    if (NULL == string || '\0' == string->string[0]) {
        return OPAL_ERR_BAD_PARAM;
    }

    errno = 0;
    tmp = strtol(string->string, &endp, 10);
    /* we found something not a number */
    if (*endp != '\0') {
        return OPAL_ERR_BAD_PARAM;
    }
    /* base errors */
    if (tmp == 0 && errno == EINVAL) {
        return OPAL_ERR_BAD_PARAM;
    }
    /* overflow/underflow of long int (return of strtol) */
    if (errno == ERANGE && (tmp == LONG_MIN || tmp == LONG_MAX)) {
        return OPAL_ERR_BAD_PARAM;
    }
    /* overflow/underflow of int (for cases where long int is larger) */
    if (tmp < INT_MIN || tmp > INT_MAX) {
        return OPAL_ERR_BAD_PARAM;
    }

    *interp = (int) tmp;

    return OPAL_SUCCESS;
}

static int opal_str_to_bool_impl(const char *string, bool *interp)
{
    const char *ptr = string;

    if (NULL != string) {
        /* Trim leading whitespace */
        while (isspace(*ptr)) {
            ++ptr;
        }

        if ('\0' != *ptr) {
            if (isdigit(*ptr)) {
                *interp = (bool) atoi(ptr);
                return OPAL_SUCCESS;
            } else if (0 == strncasecmp(ptr, "yes", 3) || 0 == strncasecmp(ptr, "true", 4)) {
                *interp = true;
                return OPAL_SUCCESS;
            } else if (0 == strncasecmp(ptr, "no", 2) || 0 == strncasecmp(ptr, "false", 5)) {
                *interp = false;
                return OPAL_SUCCESS;
            }
        }
    }

    *interp = false;
    return OPAL_ERR_BAD_PARAM;
}

int opal_cstring_to_bool(opal_cstring_t *string, bool *interp)
{
    return opal_str_to_bool_impl(string->string, interp);
}

bool opal_str_to_bool(const char *string)
{
    bool res;
    opal_str_to_bool_impl(string, &res);
    return res;
}
