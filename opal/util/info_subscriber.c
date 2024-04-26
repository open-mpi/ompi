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
 * Copyright (c) 2015-2018 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2016-2018 IBM Corporation. All rights reserved.
 * Copyright (c) 2017-2018 Intel, Inc. All rights reserved.
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
#include <string.h>

#include "opal/util/argv.h"
#include "opal/util/info_subscriber.h"
#include "opal/util/opal_getcwd.h"
#include "opal/util/output.h"

static const char *opal_infosubscribe_inform_subscribers(opal_infosubscriber_t *object,
                                                         const char *key, const char *new_value,
                                                         int *found_callback);
static void infosubscriber_construct(opal_infosubscriber_t *obj);
static void infosubscriber_destruct(opal_infosubscriber_t *obj);

/*
 * Local structures
 */

typedef struct opal_callback_list_t opal_callback_list_t;

struct opal_callback_list_item_t {
    opal_list_item_t super;
    opal_cstring_t *default_value;
    opal_key_interest_callback_t *callback;
};
typedef struct opal_callback_list_item_t opal_callback_list_item_t;

OPAL_DECLSPEC OBJ_CLASS_DECLARATION(opal_infosubscriber_t);
OBJ_CLASS_INSTANCE(opal_infosubscriber_t, opal_object_t, infosubscriber_construct,
                   infosubscriber_destruct);

OPAL_DECLSPEC OBJ_CLASS_DECLARATION(opal_callback_list_item_t);
static void opal_callback_list_item_destruct(opal_callback_list_item_t *obj);
OBJ_CLASS_INSTANCE(opal_callback_list_item_t, opal_list_item_t, NULL,
                   opal_callback_list_item_destruct);

static void infosubscriber_construct(opal_infosubscriber_t *obj)
{
    OBJ_CONSTRUCT(&obj->s_subscriber_table, opal_hash_table_t);
    opal_hash_table_init(&obj->s_subscriber_table, 10);
}

static void infosubscriber_destruct(opal_infosubscriber_t *obj)
{
    opal_hash_table_t *table = &obj->s_subscriber_table;
    void *node = NULL;
    int err;
    char *next_key;
    size_t key_size;
    opal_list_t *list = NULL;

    err = opal_hash_table_get_first_key_ptr(table, (void **) &next_key, &key_size, (void **) &list,
                                            &node);
    while (list && err == OPAL_SUCCESS) {
        OPAL_LIST_RELEASE(list);

        err = opal_hash_table_get_next_key_ptr(table, (void **) &next_key, &key_size,
                                               (void **) &list, node, &node);
    }

    OBJ_DESTRUCT(&obj->s_subscriber_table);

    if (NULL != obj->s_info) {
        OBJ_RELEASE(obj->s_info);
    }
}

static void opal_callback_list_item_destruct(opal_callback_list_item_t *obj)
{
    if (obj->default_value) {
        OBJ_RELEASE(obj->default_value);
    }
}

static const char *opal_infosubscribe_inform_subscribers(opal_infosubscriber_t *object,
                                                         const char *key, const char *new_value,
                                                         int *found_callback)
{
    opal_hash_table_t *table = &object->s_subscriber_table;
    opal_list_t *list = NULL;
    opal_callback_list_item_t *item;
    const char *updated_value = NULL;

    if (found_callback) {
        *found_callback = 0;
    }
    /*
     * Present the new value to each subscriber.  They can decide to accept it, ignore it, or
     * over-ride it with their own value (like ignore, but they specify what value they want it to
     * have).
     *
     * Since multiple subscribers could set values, only the last setting is kept as the
     * returned value.
     */
    if (table) {
        opal_hash_table_get_value_ptr(table, key, strlen(key), (void **) &list);

        if (list) {
            updated_value = new_value;
            OPAL_LIST_FOREACH (item, list, opal_callback_list_item_t) {
                updated_value = item->callback(object, key, updated_value);
                if (found_callback) {
                    *found_callback = 1;
                }
            }
        }
    }

    return updated_value;
}

/*
 * Testing-only static data, all paths using this code should be
 * inactive in a normal run.  In particular ntesting_callbacks is 0
 * unless testing is in play.
 */
static int ntesting_callbacks = 0;
static opal_key_interest_callback_t *testing_callbacks[5];
static char *testing_keys[5];
static char *testing_initialvals[5];

// User-level call, user adds their own callback function to be subscribed
// to every object:
int opal_infosubscribe_testcallback(opal_key_interest_callback_t *callback, char *key, char *val);

int opal_infosubscribe_testcallback(opal_key_interest_callback_t *callback, char *key, char *val)
{
    int i = ntesting_callbacks;
    if (ntesting_callbacks >= 5) {
        return -1;
    }

    testing_callbacks[i] = callback;
    testing_keys[i] = key;
    testing_initialvals[i] = val;
    ++ntesting_callbacks;
    return 0;
}

int opal_infosubscribe_testregister(opal_infosubscriber_t *object);
int opal_infosubscribe_testregister(opal_infosubscriber_t *object)
{
    opal_hash_table_t *table = &object->s_subscriber_table;
    opal_callback_list_item_t *item;
    opal_list_t *list = NULL;

    // The testing section should only ever be activated if the testing callback
    // above is used.
    if (ntesting_callbacks != 0) {
        int i;
        for (i = 0; i < ntesting_callbacks; ++i) {
            // The testing-code only wants to add test-subscriptions
            // once for an obj.  So before adding a test-subscription, see
            // if it's already there.
            int found = 0;
            opal_hash_table_get_value_ptr(table, testing_keys[i], strlen(testing_keys[i]),
                                          (void **) &list);
            if (list) {
                OPAL_LIST_FOREACH (item, list, opal_callback_list_item_t) {
                    if (0 == strcmp(item->default_value->string, testing_initialvals[i])
                        && item->callback == testing_callbacks[i]) {
                        found = 1;
                    }
                }
            }
            list = NULL;

            if (!found) {
                opal_infosubscribe_subscribe(object, testing_keys[i], testing_initialvals[i],
                                             testing_callbacks[i]);
            }
        }
    }

    // For testing-mode only, while we're here, lets walk the whole list
    // to see if there are any duplicates.
    if (ntesting_callbacks != 0) {
        int err;
        void *node = NULL;
        size_t key_size;
        char *next_key;
        opal_callback_list_item_t *item1, *item2;

        err = opal_hash_table_get_first_key_ptr(table, (void **) &next_key, &key_size,
                                                (void **) &list, &node);
        while (list && err == OPAL_SUCCESS) {
            int counter = 0;
            OPAL_LIST_FOREACH (item1, list, opal_callback_list_item_t) {
                OPAL_LIST_FOREACH (item2, list, opal_callback_list_item_t) {
                    if (0 == strcmp(item1->default_value->string, item2->default_value->string)
                        && item1->callback == item2->callback) {
                        ++counter;
                    }
                }
            }
            if (counter > 1) {
                printf("ERROR: duplicate info key/val subscription found "
                       "in hash table\n");
                exit(-1);
            }

            err = opal_hash_table_get_next_key_ptr(table, (void **) &next_key, &key_size,
                                                   (void **) &list, node, &node);
        }
    }

    return OPAL_SUCCESS;
}

int opal_infosubscribe_change_info(opal_infosubscriber_t *object, opal_info_t *new_info)
{
    opal_info_entry_t *iterator;
    const char *updated_value;

    /* for each key/value in new info, let subscribers know of new value */
    int found_callback;

    if (!object->s_info) {
        object->s_info = OBJ_NEW(opal_info_t);
    }

    if (NULL != new_info) {
        OPAL_LIST_FOREACH (iterator, &new_info->super, opal_info_entry_t) {
            int err = OPAL_SUCCESS;
            opal_cstring_t *value_str, *key_str;
            value_str = iterator->ie_value;
            OBJ_RETAIN(value_str);
            key_str = iterator->ie_key;
            OBJ_RETAIN(key_str);

            updated_value = opal_infosubscribe_inform_subscribers(object, iterator->ie_key->string,
                                                                  iterator->ie_value->string,
                                                                  &found_callback);
            if (NULL != updated_value) {
                err = opal_info_set(object->s_info, key_str->string, updated_value);
            } else {
                err = opal_info_set_internal(object->s_info, key_str->string, value_str->string);
            }
            OBJ_RELEASE(value_str);
            OBJ_RELEASE(key_str);
            if (OPAL_SUCCESS != err) {
                return err;
            }
        }
    }

    return OPAL_SUCCESS;
}

// Callers can provide a callback for processing info k/v pairs.
//
// Currently the callback() is expected to return a static string, and the
// callers of callback() do not try to free the string it returns. for example
// current callbacks do things like
//     return some_condition ? "true" : "false";
// the caller of callback() uses the return value in an opal_info_set() which
// strdups the string. The string returned from callback() is not kept beyond
// that. Currently if the callback() did malloc/strdup/etc for its return value
// the caller of callback() would have no way to know whether it needed freeing
// or not, so that string would be leaked.
//
// For future consideration I'd propose a model where callback() is expected
// to always strdup() its return value, so the value returned by callback()
// would either be NULL or it would be a string that needs free()ed. It seems
// to me this might be required if the strings become more dynamic than the
// simple true/false values seen in the current code. It'll be an easy change,
// callback() is only used two places.
int opal_infosubscribe_subscribe(opal_infosubscriber_t *object, const char *key, const char *value,
                                 opal_key_interest_callback_t *callback)
{
    opal_list_t *list = NULL;
    opal_hash_table_t *table = &object->s_subscriber_table;
    opal_callback_list_item_t *callback_list_item;

    if (table) {
        opal_hash_table_get_value_ptr(table, key, strlen(key), (void **) &list);

        if (!list) {
            list = OBJ_NEW(opal_list_t);
            opal_hash_table_set_value_ptr(table, key, strlen(key), list);
        }
        opal_cstring_t *value_str = opal_cstring_create(value);
        callback_list_item = OBJ_NEW(opal_callback_list_item_t);
        callback_list_item->callback = callback;
        callback_list_item->default_value = value_str;

        opal_list_append(list, (opal_list_item_t *) callback_list_item);

        // Trigger callback() on either the default value or the info that's in the
        // object if there is one. Unfortunately there's some code duplication as
        // this is similar to the job of opal_infosubscribe_change_info().
        //
        // The value we store for key is whatever the callback() returns.
        // We also leave a backup __IN_* key with the previous value.

        //  - is there an info object yet attached to this object
        if (NULL == object->s_info) {
            object->s_info = OBJ_NEW(opal_info_t);
        }
        // - is there a value already associated with key in this obj's info:
        //   to use in the callback()
        opal_cstring_t *val;
        int flag = 0;
        const char *updated_value;
        int err;
        opal_info_get(object->s_info, key, &val, &flag);
        if (!flag) {
            val = value_str; // fall back to default string
            OBJ_RETAIN(val);
        }
        // - callback() and modify the val in info
        updated_value = callback(object, key, val->string);
        if (updated_value) {
            err = opal_info_set(object->s_info, key, updated_value);
        } else {
            err = opal_info_delete(object->s_info, key);
            err = OPAL_SUCCESS; // we don't care if the key was found or not
        }
        OBJ_RELEASE(val);

        if (OPAL_SUCCESS != err) {
            return err;
        }
    } else {
        /*
         * TODO: This should not happen
         */
    }

    return OPAL_SUCCESS;
}
