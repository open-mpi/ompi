/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2009      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2012-2017 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2017-2018 IBM Corporation. All rights reserved.
 * Copyright (c) 2020      Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OPAL_INFO_H
#define OPAL_INFO_H

#include <string.h>

#include "opal/class/opal_cstring.h"
#include "opal/class/opal_list.h"
#include "opal/class/opal_pointer_array.h"
#include "opal/mca/base/mca_base_var_enum.h"
#include "opal/mca/threads/mutex.h"

/**
 * \internal
 * opal_info_t structure. MPI_Info is a pointer to this structure
 */

struct opal_info_t {
    opal_list_t super;
    opal_mutex_t *i_lock;
};

/**
 * \internal
 * Convenience typedef
 */
typedef struct opal_info_t opal_info_t;

/**
 * Table for Fortran <-> C translation table
 */
extern opal_pointer_array_t ompi_info_f_to_c_table;

/**
 * \internal
 *
 * opal_info_entry_t object. Each item in opal_info_list is of this
 * type. It contains (key,value) pairs
 */
struct opal_info_entry_t {
    opal_list_item_t super;   /**< required for opal_list_t type */
    opal_cstring_t *ie_value; /**< value part of the (key, value) pair. */
    opal_cstring_t *ie_key;   /**< "key" part of the (key, value) pair */
    uint32_t ie_referenced;   /**< number of times this entry was internally
                                   referenced */
    bool ie_internal;         /**< internal keys are not handed back to the user */
};

/**
 * \internal
 * Convenience typedef
 */
typedef struct opal_info_entry_t opal_info_entry_t;

BEGIN_C_DECLS

/**
 * \internal
 * Some declarations needed to use OBJ_NEW and OBJ_DESTRUCT macros
 */
OPAL_DECLSPEC OBJ_CLASS_DECLARATION(opal_info_t);

/**
 * \internal
 * Some declarations needed to use OBJ_NEW and OBJ_DESTRUCT macros
 */
OPAL_DECLSPEC OBJ_CLASS_DECLARATION(opal_info_entry_t);

/**
 *   opal_info_dup - Duplicate public keys of an 'MPI_Info' object
 *
 *   @param info source info object (handle)
 *   @param newinfo pointer to the new info object (handle)
 *
 *   @retval OPAL_SUCCESS upon success
 *   @retval OPAL_ERR_OUT_OF_RESOURCE if out of memory
 *
 *   Not only will the (key, value) pairs be duplicated, the order
 *   of keys will be the same in 'newinfo' as it is in 'info'.  When
 *   an info object is no longer being used, it should be freed with
 *   \c opal_info_free.
 */
int opal_info_dup_public(opal_info_t *info, opal_info_t **newinfo);

/**
 *   opal_info_dup - Duplicate all entries of an 'MPI_Info' object
 *
 *   @param info source info object (handle)
 *   @param newinfo pointer to the new info object (handle)
 *
 *   @retval OPAL_SUCCESS upon success
 *   @retval OPAL_ERR_OUT_OF_RESOURCE if out of memory
 *
 *   Not only will the (key, value) pairs be duplicated, the order
 *   of keys will be the same in 'newinfo' as it is in 'info'.  When
 *   an info object is no longer being used, it should be freed with
 *   \c opal_info_free.
 */
int opal_info_dup(opal_info_t *info, opal_info_t **newinfo);

/**
 * Set a new key,value pair on info and mark it as referenced.
 *
 * @param info pointer to opal_info_t object
 * @param key pointer to the new key object
 * @param value pointer to the new value object
 *
 * @retval OPAL_SUCCESS upon success
 * @retval OPAL_ERR_OUT_OF_RESOURCE if out of memory
 */
OPAL_DECLSPEC int opal_info_set(opal_info_t *info, const char *key, const char *value);

/**
 * Set a new key,value pair on info and mark it as internal.
 *
 * @param info pointer to opal_info_t object
 * @param key pointer to the new key object
 * @param value pointer to the new value object
 *
 * @retval OPAL_SUCCESS upon success
 * @retval OPAL_ERR_OUT_OF_RESOURCE if out of memory
 */
OPAL_DECLSPEC int opal_info_set_internal(opal_info_t *info, const char *key, const char *value);

/**
 * Set a new key,value pair on info.
 *
 * @param info pointer to opal_info_t object
 * @param key pointer to the new key string
 * @param value pointer to the new value string object
 *
 * @retval OPAL_SUCCESS upon success
 * @retval OPAL_ERR_OUT_OF_RESOURCE if out of memory
 *
 * The \c value string object will be retained and can be safely released if necessary
 * by the caller.
 */
OPAL_DECLSPEC int opal_info_set_cstring(opal_info_t *info, const char *key, opal_cstring_t *value);

/**
 * Set a new key,value pair from a variable enumerator.
 *
 * @param info pointer to opal_info_t object
 * @param key pointer to the new key object
 * @param value integer value of the info key (must be valid in var_enum)
 * @param var_enum variable enumerator
 *
 * @retval OPAL_SUCCESS upon success
 * @retval OPAL_ERR_OUT_OF_RESOURCE if out of memory
 * @retval OPAL_ERR_VALUE_OUT_OF_BOUNDS if the value is not valid in the enumerator
 */
OPAL_DECLSPEC int opal_info_set_value_enum(opal_info_t *info, const char *key, int value,
                                           mca_base_var_enum_t *var_enum);

/**
 * opal_info_free - Free an 'MPI_Info' object.
 *
 *   @param info pointer to info (opal_info_t *) object to be freed (handle)
 *
 *   @retval OPAL_SUCCESS
 *   @retval OPAL_ERR_BAD_PARAM
 *
 *   Upon successful completion, 'info' will be set to
 *   'MPI_INFO_NULL'.  Free the info handle and all of its keys and
 *   values.
 */
int opal_info_free(opal_info_t **info);

/**
 *   Get a (key, value) pair from an 'MPI_Info' object and assign it
 *   into a boolean output.
 *
 *   This call marks the entry referenced.
 *
 *   @param info Pointer to opal_info_t object
 *   @param key null-terminated character string of the index key
 *   @param value Boolean output value
 *   @param flag true (1) if 'key' defined on 'info', false (0) if not
 *               (logical)
 *
 *   @retval OPAL_SUCCESS
 *
 *   If found, the string value will be cast to the boolean output in
 *   the following manner:
 *
 *   - If the string value is digits, the return value is "(bool)
 *     atoi(value)"
 *   - If the string value is (case-insensitive) "yes" or "true", the
 *     result is true
 *   - If the string value is (case-insensitive) "no" or "false", the
 *     result is false
 *   - All other values are false
 */
OPAL_DECLSPEC int opal_info_get_bool(opal_info_t *info, const char *key, bool *value, int *flag);

/**
 *   Get a (key, value) pair from an 'MPI_Info' object and assign it
 *   into an integer output based on the enumerator value.
 *
 *   @param info Pointer to opal_info_t object
 *   @param key null-terminated character string of the index key
 *   @param value integer output value
 *   @param default_value value to use if the string does not conform to the
 *          values accepted by the enumerator
 *   @param var_enum variable enumerator for the value
 *   @param flag true (1) if 'key' defined on 'info', false (0) if not
 *               (logical)
 *
 *   @retval OPAL_SUCCESS
 */

OPAL_DECLSPEC int opal_info_get_value_enum(opal_info_t *info, const char *key, int *value,
                                           int default_value, mca_base_var_enum_t *var_enum,
                                           int *flag);

/**
 *   Get a (key, value) pair from an 'MPI_Info' object and mark the entry
 *   as referenced.
 *
 *   @param info Pointer to opal_info_t object
 *   @param key null-terminated character string of the index key
 *   @param string null-terminated character string of the value
 *   @param flag true (1) if 'key' defined on 'info', false (0) if not
 *               (logical)
 *
 *   @retval OPAL_SUCCESS
 *
 *   The \c string pointer will only be set if the key is found, i.e., if \c flag
 *   is set to \c true. It is the caller's responsibility to decrement the
 *   reference count of the \c string object by calling \c OBJ_RELEASE on it
 *   once the object is not needed any more.
 */
OPAL_DECLSPEC int opal_info_get(opal_info_t *info, const char *key, opal_cstring_t **string,
                                int *flag);

/**
 * Delete a (key,value) pair from "info"
 *
 * @param info opal_info_t pointer on which we need to operate
 * @param key The key portion of the (key,value) pair that
 *            needs to be deleted
 *
 * @retval OPAL_SUCCESS
 * @retval OPAL_ERR_NOT_FOUND
 */
int opal_info_delete(opal_info_t *info, const char *key);

/**
 *   @param info - opal_info_t pointer object (handle)
 *   @param key - null-terminated character string of the index key
 *   @param valuelen - length of the value associated with 'key' (integer)
 *   @param flag - true (1) if 'key' defined on 'info', false (0) if not
 *   (logical)
 *
 *   @retval OPAL_SUCCESS
 *   @retval OPAL_ERR_BAD_PARAM
 *   @retval MPI_ERR_INFO_KEY
 *
 *   The length returned in C and C++ does not include the end-of-string
 *   character.  If the 'key' is not found on 'info', 'valuelen' is left
 *   alone.
 */
OPAL_DECLSPEC int opal_info_get_valuelen(opal_info_t *info, const char *key, int *valuelen,
                                         int *flag);

/**
 *   opal_info_get_nthkey - Get a key indexed by integer from an info object
 *
 *   @param info Pointer to opal_info_t object
 *   @param n index of key to retrieve (integer)
 *   @param key output opal_cstring_t object, set if the n'th key exists
 *
 *   @retval OPAL_SUCCESS
 *   @retval OPAL_ERR_BAD_PARAM
 *
 *   It is the caller's responsibility to decrement the reference count of the
 *   \c key string by calling \c OBJ_RELEASE on it once the object is not needed
 *   any more.
 */
int opal_info_get_nthkey(opal_info_t *info, int n, opal_cstring_t **key);

/**
 * Get the number of keys defined on on an MPI_Info object
 * @param info Pointer to opal_info_t object.
 * @param nkeys Pointer to nkeys, which needs to be filled up.
 *
 * @retval The number of keys defined on info
 */
static inline int opal_info_get_nkeys(opal_info_t *info, int *nkeys)
{
    *nkeys = (int) opal_list_get_size(&(info->super));
    return OPAL_SUCCESS;
}

END_C_DECLS

#endif /* OPAL_INFO_H */
