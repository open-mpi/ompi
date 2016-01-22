/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2015 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2009      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2012-2015 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <string.h>
#include <errno.h>
#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <limits.h>
#include <ctype.h>
#ifdef HAVE_SYS_UTSNAME_H
#include <sys/utsname.h>
#endif
#include <assert.h>

#include "opal/util/argv.h"
#include "opal/util/opal_getcwd.h"
#include "opal/util/output.h"
#include "opal/util/strncpy.h"
#include "opal/util/info_subscriber.h"

static char* opal_infosubscribe_inform_subscribers(opal_infosubscriber_t * object, char *key, char *new_value);
static void infosubscriber_construct(opal_infosubscriber_t *obj);
static void infosubscriber_destruct(opal_infosubscriber_t *obj);

/*
 * Local structures
 */

typedef struct opal_callback_list_t opal_callback_list_t;

struct opal_callback_list_item_t {
    opal_list_item_t super;
    char *default_value;
    opal_key_interest_callback_t *callback;
};
typedef struct opal_callback_list_item_t opal_callback_list_item_t;

OPAL_DECLSPEC OBJ_CLASS_DECLARATION(opal_infosubscriber_t);
OBJ_CLASS_INSTANCE(opal_infosubscriber_t, 
                   opal_object_t, 
                   infosubscriber_construct, 
                   infosubscriber_destruct);

OPAL_DECLSPEC OBJ_CLASS_DECLARATION(opal_callback_list_item_t);
OBJ_CLASS_INSTANCE(opal_callback_list_item_t, 
                   opal_list_item_t, 
                   NULL, 
                   NULL);

static void infosubscriber_construct(opal_infosubscriber_t *obj) {
    OBJ_CONSTRUCT(&obj->s_subscriber_table, opal_hash_table_t);
    opal_hash_table_init(&obj->s_subscriber_table, 10);
}

static void infosubscriber_destruct(opal_infosubscriber_t *obj) {
    OBJ_DESTRUCT(&obj->s_subscriber_table);
}

static char* opal_infosubscribe_inform_subscribers(opal_infosubscriber_t *object, char *key, char *new_value)
{
    opal_hash_table_t *table = &object->s_subscriber_table;
    opal_list_t *list = NULL;
    opal_callback_list_item_t *item;
    char *updated_value = NULL;

/*
 * Present the new value to each subscriber.  They can decide to accept it, ignore it, or 
 * over-ride it with their own value (like ignore, but they specify what value they want it to have).
 *
 * Since multiple subscribers could set values, only the last setting is kept as the 
 * returned value.
 */
    if (table) {
        opal_hash_table_get_value_ptr(table, key, strlen(key), (void**) &list);

        if (list) {
            updated_value = new_value;
            OPAL_LIST_FOREACH(item, list, opal_callback_list_item_t) {
		updated_value = item->callback(object, key, updated_value);
            }
        }
    }

    return updated_value;
}




int
opal_infosubscribe_change_info(opal_infosubscriber_t *object, opal_info_t *new_info)
{
    int err;
    size_t key_size;
    int flag;
    opal_info_entry_t *iterator;
    opal_info_t **old_info = &object->s_info;
    opal_info_t *real_info;
    char *updated_value;
    void *node = NULL;
    char *next_key;
    opal_hash_table_t *table = &object->s_subscriber_table;
    opal_callback_list_item_t *item;
    opal_list_t *list = NULL;

    /* for each key/value in new info, let subscribers know of new value */

    real_info = OBJ_NEW(opal_info_t);
 
    OPAL_LIST_FOREACH(iterator, &new_info->super, opal_info_entry_t) {
      
        if ((updated_value = opal_infosubscribe_inform_subscribers(object, iterator->ie_key, iterator->ie_value))) {
            err = opal_info_set(real_info, iterator->ie_key, updated_value);    
            if (MPI_SUCCESS != err) {
              return err;
            }
        }
    }

/* 
 * Now any values in the old_info that were not included in the new info we should 
 * tell them that they are going away and give a chance to set them in the new info
 * SOLT: TODO: This should be a compare with MPI_INFO_NULL??
 */
   if (NULL != *old_info) {
   
        /* let subscribers know it is going away, they may set a new value for it */

        OPAL_LIST_FOREACH(iterator, &(*old_info)->super, opal_info_entry_t) {

/*
 * See if this is updated in the new_info.  If so, we don't need to tell them about it
 * going away, we already told them about the value change.
 */
            err = opal_info_get (new_info, iterator->ie_key, 0, NULL, &flag);
            if (MPI_SUCCESS != err) {
                return err;
            }
   
            if (!flag && (updated_value = opal_infosubscribe_inform_subscribers(object, iterator->ie_key, NULL))) {
                err = opal_info_set(real_info, iterator->ie_key, updated_value);
                if (MPI_SUCCESS != err) {
                    return err;
                }
            }
        }

       /* Clear old info */
       OBJ_DESTRUCT(old_info);

    } else {
/*
 * If there is no old_info, then this is the first time that we are setting something and we should set all 
 * defaults that were not changed in new_info
 */
        err = opal_hash_table_get_first_key_ptr(table, (void**) &next_key, &key_size, (void**) &list, &node);


        while (list && err == OPAL_SUCCESS) {

            err = opal_info_get (new_info, next_key, 0, NULL, &flag);
            if (MPI_SUCCESS != err) {
                return err;
            }
/*
 * Figure out which subscriber's default value we will take.  (Psst, we are going to 
 * take the first one we see)
 */
            updated_value = NULL;
            OPAL_LIST_FOREACH(item, list, opal_callback_list_item_t) {
                 if (item->default_value) {
	             updated_value = item->default_value;
                     break;
                 }
            }
		
            if (updated_value) {
                err = opal_info_set(real_info, next_key, updated_value);
            }
        }

        err = opal_hash_table_get_next_key_ptr(table, (void**) next_key, &key_size, (void**) &list, node, &node);
    }

   *old_info = real_info;
   
   return OPAL_SUCCESS;
}

int opal_infosubscribe_subscribe(opal_infosubscriber_t *object, char *key, char *value, opal_key_interest_callback_t *callback)
{
    opal_list_t *list = NULL;
    opal_hash_table_t *table = &object->s_subscriber_table;
    opal_callback_list_item_t *callback_list_item;

    if (table) {
        opal_hash_table_get_value_ptr(table, key, strlen(key), (void**) &list);

        if (!list) {
            list = OBJ_NEW(opal_list_t);
            opal_hash_table_set_value_ptr(table, key, strlen(key), list);
        }

	callback_list_item = OBJ_NEW(opal_callback_list_item_t);
        callback_list_item->callback = callback;
	if (value) {
            callback_list_item->default_value = strdup(value);
        } else {
            callback_list_item->default_value = NULL;
        }

        opal_list_append(list, (opal_list_item_t*) callback_list_item); 
    } else {
/*
 * TODO: This should not happen
 */ 
    }

    return OPAL_SUCCESS;
}

/*
    OBJ_DESTRUCT(&opal_comm_info_hashtable);
    OBJ_DESTRUCT(&opal_win_info_hashtable);
    OBJ_DESTRUCT(&opal_file_info_hashtable);
*/
