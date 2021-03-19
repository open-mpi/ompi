#include "opal/mca/threads/tsd.h"

static void _tracked_destructor(void *arg)
{
    opal_tsd_list_item_t *tsd = NULL;
    opal_tsd_tracked_key_t *key = NULL;
    if (NULL == arg) {
        return;
    }

    tsd = (opal_tsd_list_item_t *) arg;
    key = tsd->tracked_key;

    opal_mutex_lock(&key->mutex);
    opal_list_remove_item(&key->tsd_list, &tsd->super);
    opal_mutex_unlock(&key->mutex);

    if (NULL != key->user_destructor) {
        key->user_destructor(tsd->data);
    }
    OBJ_RELEASE(tsd);
}

void opal_tsd_tracked_key_constructor(opal_tsd_tracked_key_t *key)
{
    OBJ_CONSTRUCT(&key->mutex, opal_mutex_t);
    OBJ_CONSTRUCT(&key->tsd_list, opal_list_t);
    key->user_destructor = NULL;
    opal_tsd_key_create(&key->key, _tracked_destructor);
}

void opal_tsd_tracked_key_destructor(opal_tsd_tracked_key_t *key)
{
    opal_tsd_list_item_t *tsd, *next;

    opal_tsd_key_delete(key->key);
    OPAL_LIST_FOREACH_SAFE (tsd, next, &key->tsd_list, opal_tsd_list_item_t) {
        opal_list_remove_item(&key->tsd_list, &tsd->super);
        if (NULL != key->user_destructor) {
            key->user_destructor(tsd->data);
        }
        OBJ_RELEASE(tsd);
    }
    OBJ_DESTRUCT(&key->mutex);
    OBJ_DESTRUCT(&key->tsd_list);
}

int opal_tsd_tracked_key_set(opal_tsd_tracked_key_t *key, void *p)
{
    assert(NULL != key);

    opal_tsd_list_item_t *tsd = NULL;
    opal_tsd_get(key->key, (void **) &tsd);

    if (NULL == tsd) {
        tsd = OBJ_NEW(opal_tsd_list_item_t);
        if (NULL == tsd) {
            return OPAL_ERR_OUT_OF_RESOURCE;
        }

        opal_mutex_lock(&key->mutex);
        opal_list_append(&key->tsd_list, &tsd->super);
        opal_mutex_unlock(&key->mutex);
    }

    tsd->data = p;
    tsd->tracked_key = key;

    return opal_tsd_set(key->key, (void *) tsd);
}

void opal_tsd_tracked_key_set_destructor(opal_tsd_tracked_key_t *key,
                                         opal_tsd_destructor_t destructor)
{
    assert(NULL != key);
    key->user_destructor = destructor;
}

OBJ_CLASS_INSTANCE(opal_tsd_list_item_t, opal_list_item_t, NULL, NULL);
OBJ_CLASS_INSTANCE(opal_tsd_tracked_key_t, opal_object_t, opal_tsd_tracked_key_constructor,
                   opal_tsd_tracked_key_destructor);
