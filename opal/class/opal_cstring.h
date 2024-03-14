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

/**
 * @file
 *
 * Implementation of a reference-counted immutable string object.
 * The string object is created using either \c opal_cstring_create(string) or
 * \c opal_cstring_create_l(string, len) with the latter accepting the number
 * of characters to take from the input string.
 *
 * The reference counting is done using opal's \c OBJ_RETAIN / \c OBJ_RELEASE mechanism
 * and it is the user's responsibility to ensure that the string is eventually
 * free'd by decrementing the reference counter using OBJ_RETAIN.
 *
 * The structure contains two relevant members:
 *
 * - \c length: the length of the string, i.e., the number of characters in \c string *not*
 *              including the null-terminator.
 * - \c string: the array of characters that make up the string.
 *
 * Both fields are \c const and should not be altered by the user. If the string
 * contained in an \c opal_cstring_t object should be modified it has to be copied
 * to a different buffer (e.g., using strdup).
 *
 * The \c string is always guaranteed to be null-terminated, even if the
 * \c opal_cstring_t object was created using \c OBJ_NEW (which results in an
 * empty string with the \c length field set to zero and the \c string field
 * pointing to the null-terminator).
 *
 */

#ifndef OPAL_STRING_H
#define OPAL_STRING_H

#include "opal/class/opal_object.h"
#include "opal/mca/base/mca_base_var_enum.h"

#include <string.h>

/**
 * Reference-counted immutable string object.
 *
 * The two relevant members are:
 *
 * - \c length: the length of the string, i.e., the number of characters in \c string *not*
 *              including the null-terminator.
 * - \c string: the array of characters that make up the string.
 *
 * The string is eventually free'd by calling \c OBJ_RELEASE on it.
 *
 * If allocated using \c OBJ_NEW the object will contain an empty string.
 * The member field \c _ignored is used to force the existence of padding bytes
 * that can be used to write the null-terminator even if no additional memory
 * was allocated succeeding the object itself and is ignored.
 */
struct opal_cstring_t {
    opal_object_t super;
    const size_t length; //< the number of characters not including the null-terminator
    char _ignored; //< single char forcing additional padding to always ensure null-termination
    const char string[]; //< FMA containing the string, making use of padding bytes
};

typedef struct opal_cstring_t opal_cstring_t;

BEGIN_C_DECLS

/**
 * \internal
 *
 * The class for string objects.
 */
OPAL_DECLSPEC OBJ_CLASS_DECLARATION(opal_cstring_t);

/**
 * Create a new instance of a reference-counted immutable string object
 * (\ref opal_cstring_t) containing the characters of \c string.
 *
 * @param string Value of the new string
 * @return An object representing the null-terminated string with value \c string
 *
 * If \c string is \c NULL then the resulting string will be empty.
 *
 */
OPAL_DECLSPEC
opal_cstring_t *opal_cstring_create(const char *string) __opal_attribute_malloc__;

/**
 * Create a new instance of a reference-counted immutable string object
 * (\ref opal_cstring_t) containing the first \c length characters of \c string.
 *
 * @param string Value of the new string
 * @return An object representing null-terminated string with the first
 *         \c length characters of \c string
 *
 * If \c string is \c NULL or \c length is zero the resulting string will be empty.
 */
OPAL_DECLSPEC
opal_cstring_t *opal_cstring_create_l(const char *string, size_t length) __opal_attribute_malloc__;

/**
 * Convert string to integer
 *
 * Convert \c string into an integer, adhering to the
 * interpretation rules specified in MPI-4 Chapter 10.
 * All others will return \c OPAL_ERR_BAD_PARAM
 *
 * @param string Value string to interpret
 * @param interp returned interpretation of the value key
 *
 * @retval OPAL_SUCCESS string was successfully interpreted
 * @retval OPAL_ERR_BAD_PARAM string could not be interpreted
 *
 */
OPAL_DECLSPEC
int opal_cstring_to_int(opal_cstring_t *string, int *interp);

/**
 * Convert string to boolean
 *
 * Convert \c string into a boolean, adhering to the
 * interpretation rules specified in MPI-4 Chapter 10.
 *
 * @param value Value string to interpret
 * @param interp returned interpretation of the value key
 *
 * @retval OPAL_SUCCESS string was successfully interpreted
 * @retval OPAL_ERR_BAD_PARAM string was not able to be interpreted
 *
 *   The string value will be cast to the boolean output in
 *   the following manner:
 *
 *   - If the string value is digits, the return value is "(bool)
 *     atoi(value)"
 *   - If the string value is (case-insensitive) "yes" or "true", the
 *     result is true
 *   - If the string value is (case-insensitive) "no" or "false", the
 *     result is false
 *   - All other values will lead to a return value of OPAL_ERR_BAD_PARAM and
 *     \c interp will be set to false.
 */
OPAL_DECLSPEC
int opal_cstring_to_bool(opal_cstring_t *string, bool *interp);

OPAL_DECLSPEC
bool opal_str_to_bool(const char *string);

END_C_DECLS

#endif // OPAL_STRING_H
