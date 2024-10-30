/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2021 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2018 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2009      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2012-2017 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2015-2020 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2016-2018 IBM Corporation. All rights reserved.
 * Copyright (c) 2017-2020 Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <errno.h>
#include <stdlib.h>
#include <string.h>
#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif
#include <ctype.h>
#include <limits.h>
#ifdef HAVE_SYS_UTSNAME_H
#    include <sys/utsname.h>
#endif
#include <assert.h>

#include "opal/util/argv.h"
#include "opal/util/info.h"
#include "opal/util/opal_getcwd.h"
#include "opal/util/output.h"
#include "opal/util/string_copy.h"

/*
 * Local functions
 */
static void info_constructor(opal_info_t *info);
static void info_destructor(opal_info_t *info);
static void info_entry_constructor(opal_info_entry_t *entry);
static void info_entry_destructor(opal_info_entry_t *entry);
static opal_info_entry_t *info_find_key(opal_info_t *info, const char *key);

/*
 * opal_info_t classes
 */
OBJ_CLASS_INSTANCE(opal_info_t, opal_list_t, info_constructor, info_destructor);

/*
 * opal_info_entry_t classes
 */
OBJ_CLASS_INSTANCE(opal_info_entry_t, opal_list_item_t, info_entry_constructor,
                   info_entry_destructor);

/*
 * Duplicate an info into newinfo. If public_info is true we only duplicate
 * key-value pairs that are not internal and that had been referenced,
 * either through opal_info_get or opal_info_set.
 */
static int opal_info_dup_impl(opal_info_t *info, opal_info_t **newinfo, bool public_only)
{
    opal_info_entry_t *iterator;

    OPAL_THREAD_LOCK(info->i_lock);
    OPAL_LIST_FOREACH (iterator, &info->super, opal_info_entry_t) {
        /* skip keys that are internal if we didn't ask for them */
        if (public_only && (iterator->ie_internal || iterator->ie_referenced == 0)) continue;
        /* create a new info entry and retain the string objects */
        opal_info_entry_t *newentry = OBJ_NEW(opal_info_entry_t);
        newentry->ie_key = iterator->ie_key;
        OBJ_RETAIN(iterator->ie_key);
        newentry->ie_value = iterator->ie_value;
        if (public_only) {
            /* for public info-dup we also duplicate the references so that subsequent
             * duplications see the same info keys */
            newentry->ie_referenced = iterator->ie_referenced;
        }
        OBJ_RETAIN(iterator->ie_value);
        opal_list_append (&((*newinfo)->super), (opal_list_item_t *) newentry);
    }
    OPAL_THREAD_UNLOCK(info->i_lock);
    return OPAL_SUCCESS;
}

int opal_info_dup_public(opal_info_t *info, opal_info_t **newinfo)
{
    return opal_info_dup_impl(info, newinfo, true);
}

int opal_info_dup(opal_info_t *info, opal_info_t **newinfo)
{
    return opal_info_dup_impl(info, newinfo, false);
}

static void opal_info_get_nolock(opal_info_t *info, const char *key, opal_cstring_t **value,
                                 int *flag)
{
    opal_info_entry_t *search;

    search = info_find_key(info, key);
    if (NULL == search) {
        *flag = 0;
    } else {
        /*
         * We have found the element, so we can return the value
         * Set the flag and value
         */
        *flag = 1;
        if (NULL != value) {
            OBJ_RETAIN(search->ie_value);
            *value = search->ie_value;
            search->ie_referenced++;
        }
    }
}

static int opal_info_set_cstring_nolock(opal_info_t *info, const char *key, opal_cstring_t *value)
{
    opal_info_entry_t *old_info;

    old_info = info_find_key(info, key);
    if (NULL != old_info) {
        /*
         * key already exists. remove the value associated with it
         */
        OBJ_RELEASE(old_info->ie_value);
        OBJ_RETAIN(value);
        old_info->ie_value = value;
        old_info->ie_referenced++;
    } else {
        opal_info_entry_t *new_info;
        new_info = OBJ_NEW(opal_info_entry_t);
        if (NULL == new_info) {
            return OPAL_ERR_OUT_OF_RESOURCE;
        }
        opal_cstring_t *key_str = opal_cstring_create(key);
        new_info->ie_key = key_str;
        OBJ_RETAIN(value);
        new_info->ie_value = value;
        new_info->ie_referenced++;
        opal_list_append(&(info->super), (opal_list_item_t *) new_info);
    }
    return OPAL_SUCCESS;
}

static int opal_info_set_nolock(opal_info_t *info, const char *key, const char *value, bool internal)
{
    opal_info_entry_t *old_info;

    old_info = info_find_key(info, key);
    if (NULL != old_info) {
        /*
         * key already exists, check whether it is the same
         */
        size_t value_len = strlen(value);
        old_info->ie_referenced++;
        old_info->ie_internal = internal;
        if (old_info->ie_value->length == value_len
            && 0 == strcmp(old_info->ie_value->string, value)) {
            return OPAL_SUCCESS;
        }
        /* value is different so replace it */
        OBJ_RELEASE(old_info->ie_value);
        old_info->ie_value = opal_cstring_create_l(value, value_len);
        if (NULL == old_info->ie_value) {
            OBJ_RELEASE(old_info);
            return OPAL_ERR_OUT_OF_RESOURCE;
        }
    } else {
        opal_info_entry_t *new_info;
        new_info = OBJ_NEW(opal_info_entry_t);
        if (NULL == new_info) {
            return OPAL_ERR_OUT_OF_RESOURCE;
        }
        new_info->ie_key = opal_cstring_create(key);
        new_info->ie_value = opal_cstring_create(value);
        if (NULL == new_info->ie_key || NULL == new_info->ie_value) {
            OBJ_RELEASE(new_info);
            return OPAL_ERR_OUT_OF_RESOURCE;
        }
        new_info->ie_referenced++;
        new_info->ie_internal = internal;
        opal_list_append(&(info->super), (opal_list_item_t *) new_info);
    }
    return OPAL_SUCCESS;
}

/*
 * Set a value on the info
 */
int opal_info_set(opal_info_t *info, const char *key, const char *value)
{
    int ret;

    OPAL_THREAD_LOCK(info->i_lock);
    ret = opal_info_set_nolock(info, key, value, false);
    OPAL_THREAD_UNLOCK(info->i_lock);
    return ret;
}

/*
 * Set a value on the info
 */
int opal_info_set_internal(opal_info_t *info, const char *key, const char *value)
{
    int ret;

    OPAL_THREAD_LOCK(info->i_lock);
    ret = opal_info_set_nolock(info, key, value, true);
    OPAL_THREAD_UNLOCK(info->i_lock);
    return ret;
}

int opal_info_set_cstring(opal_info_t *info, const char *key, opal_cstring_t *value)
{
    int ret;

    OPAL_THREAD_LOCK(info->i_lock);
    ret = opal_info_set_cstring_nolock(info, key, value);
    OPAL_THREAD_UNLOCK(info->i_lock);
    return ret;
}

int opal_info_set_value_enum(opal_info_t *info, const char *key, int value,
                             mca_base_var_enum_t *var_enum)
{
    char *string_value;
    int ret;

    ret = var_enum->string_from_value(var_enum, value, &string_value);
    if (OPAL_SUCCESS != ret) {
        return ret;
    }

    return opal_info_set(info, key, string_value);
}

/*
 * Get a value from an info
 */
int opal_info_get(opal_info_t *info, const char *key, opal_cstring_t **value, int *flag)
{
    OPAL_THREAD_LOCK(info->i_lock);
    opal_info_get_nolock(info, key, value, flag);
    OPAL_THREAD_UNLOCK(info->i_lock);
    return OPAL_SUCCESS;
}

int opal_info_get_value_enum(opal_info_t *info, const char *key, int *value, int default_value,
                             mca_base_var_enum_t *var_enum, int *flag)
{
    int ret;
    opal_cstring_t *str;

    *value = default_value;

    ret = opal_info_get(info, key, &str, flag);

    if (*flag) {
        ret = var_enum->value_from_string(var_enum, str->string, value);
        OBJ_RELEASE(str);
    }

    return ret;
}

/*
 * Similar to opal_info_get(), but cast the result into a boolean
 * using some well-defined rules.
 */
int opal_info_get_bool(opal_info_t *info, const char *key, bool *value, int *flag)
{
    int ret;
    opal_cstring_t *str;

    ret = opal_info_get(info, key, &str, flag);
    if (*flag) {
        ret = opal_cstring_to_bool(str, value);
        OBJ_RELEASE(str);
    }

    return ret;
}

/*
 * Delete a key from an info
 */
int opal_info_delete(opal_info_t *info, const char *key)
{
    opal_info_entry_t *search;

    OPAL_THREAD_LOCK(info->i_lock);
    search = info_find_key(info, key);
    if (NULL == search) {
        OPAL_THREAD_UNLOCK(info->i_lock);
        return OPAL_ERR_NOT_FOUND;
    } else {
        /*
         * An entry with this key value was found. Remove the item
         * and free the memory allocated to it.
         * As this key *must* be available, we do not check for errors.
         */
        opal_list_remove_item(&(info->super), (opal_list_item_t *) search);
        OBJ_RELEASE(search);
    }
    OPAL_THREAD_UNLOCK(info->i_lock);
    return OPAL_SUCCESS;
}

/*
 * Return the length of a value
 */
int opal_info_get_valuelen(opal_info_t *info, const char *key, int *valuelen, int *flag)
{
    opal_info_entry_t *search;

    OPAL_THREAD_LOCK(info->i_lock);
    search = info_find_key(info, key);
    if (NULL == search) {
        *flag = 0;
    } else {
        /*
         * We have found the element, so we can return the value
         * Set the flag, value_length and value
         */
        *flag = 1;
        *valuelen = search->ie_value->length;
    }
    OPAL_THREAD_UNLOCK(info->i_lock);
    return OPAL_SUCCESS;
}

/*
 * Get the nth key
 */
int opal_info_get_nthkey(opal_info_t *info, int n, opal_cstring_t **key)
{
    opal_info_entry_t *iterator;

    /*
     * Iterate over and over till we get to the nth key
     */
    OPAL_THREAD_LOCK(info->i_lock);
    for (iterator = (opal_info_entry_t *) opal_list_get_first(&(info->super)); n > 0; --n) {
        iterator = (opal_info_entry_t *) opal_list_get_next(iterator);
        if (opal_list_get_end(&(info->super)) == (opal_list_item_t *) iterator) {
            OPAL_THREAD_UNLOCK(info->i_lock);
            return OPAL_ERR_BAD_PARAM;
        }
    }
    OBJ_RETAIN(iterator->ie_key);
    *key = iterator->ie_key;
    OPAL_THREAD_UNLOCK(info->i_lock);
    return OPAL_SUCCESS;
}

/*
 * This function is invoked when OBJ_NEW() is called. Here, we add this
 * info pointer to the table and then store its index as the handle
 */
static void info_constructor(opal_info_t *info)
{
    info->i_lock = OBJ_NEW(opal_mutex_t);
}

/*
 * This function is called during OBJ_DESTRUCT of "info". When this
 * done, we need to remove the entry from the opal fortran to C
 * translation table
 */
static void info_destructor(opal_info_t *info)
{
    opal_list_item_t *item;
    opal_info_entry_t *iterator;

    /* Remove every key in the list */

    for (item = opal_list_remove_first(&(info->super)); NULL != item;
         item = opal_list_remove_first(&(info->super))) {
        iterator = (opal_info_entry_t *) item;
        OBJ_RELEASE(iterator);
    }

    /* Release the lock */

    OBJ_RELEASE(info->i_lock);
}

/*
 * opal_info_entry_t interface functions
 */
static void info_entry_constructor(opal_info_entry_t *entry)
{
    entry->ie_key = NULL;
    entry->ie_value = NULL;
    entry->ie_referenced = 0;
    entry->ie_internal = false;
}

static void info_entry_destructor(opal_info_entry_t *entry)
{
    if (NULL != entry->ie_key) {
        OBJ_RELEASE(entry->ie_key);
        entry->ie_key = NULL;
    }

    if (NULL != entry->ie_value) {
        OBJ_RELEASE(entry->ie_value);
        entry->ie_value = NULL;
    }
}

/*
 * Find a key
 *
 * Do NOT thread lock in here -- the calling function is responsible
 * for that.
 */
static opal_info_entry_t *info_find_key(opal_info_t *info, const char *key)
{
    opal_info_entry_t *iterator;

    /* No thread locking in here! */

    /* Iterate over all the entries. If the key is found, then
     * return immediately. Else, the loop will fall of the edge
     * and NULL is returned
     */
    OPAL_LIST_FOREACH (iterator, &info->super, opal_info_entry_t) {
        if (0 == strcmp(key, iterator->ie_key->string)) {
            return iterator;
        }
    }
    return NULL;
}
