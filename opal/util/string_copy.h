/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
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

#ifndef OPAL_STRING_COPY_H
#define OPAL_STRING_COPY_H

#include "opal_config.h"
#ifdef HAVE_SYS_TYPES_H
#    include <sys/types.h>
#endif

BEGIN_C_DECLS

/**
 * Do a "safe" string copy (i.e., guarantee to \0-terminate the
 * destination string), and assert() fail if the copy length is too
 * large (because we assume it is a programmer error).
 *
 * @param dest Destination string buffer.
 * @param src Source string buffer.
 * @param dest_len Length of the destination string buffer.
 *
 * This function is similar to, but different than, strcpy() and
 * strncpy().
 *
 * It is invalid to pass NULL for either dest or src.
 *
 * If dest_len is larger than
 * OPAL_MAX_SIZE_ALLOWED_BY_OPAL_STRING_COPY, we assume that this is
 * a programmer error (because Open MPI does not generally need to do
 * large string copies), and will assert() fail / abort.
 *
 * There is no return value.
 *
 * This function will essentially do the same thing as strncpy(),
 * except that a) it will guarantee to to terminate the destination
 * string with a \0, and b) it will not \0-pad to the right.
 * Specifically:
 *
 * - If the length of the source string is less than (len), the entire
 *   source string will be copied to the destination, including the
 *   \0.
 * - If the length of the source string is greater than (len), then
 *   (len-1) characters of the source string will be copied to the
 *   destination, and dest[len-1] will be set to '\0'.
 */
OPAL_DECLSPEC void opal_string_copy(char *dest, const char *src, size_t dest_len)
    __opal_attribute_nonnull__(1) __opal_attribute_nonnull__(2);

/**
 * Max dest_size allowed by opal_string_copy().
 *
 * See the description of opal_string_copy() for an explanation.
 */
#define OPAL_MAX_SIZE_ALLOWED_BY_OPAL_STRING_COPY (128 * 1024)

END_C_DECLS

#endif /* OPAL_STRING_COPY_H */
