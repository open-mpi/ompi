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

#include "ompi/include/constants.h"
#include "opal/memory/memory.h"
#include "opal/memory/memory_internal.h"
#include "opal/class/opal_list.h"
#include "opal/class/opal_object.h"


/* 
 * local types
 */
struct callback_list_item_t {
    opal_list_item_t super;
    opal_mem_free_unpin_fn_t *cbfunc;
    void *cbdata;
};
typedef struct callback_list_item_t callback_list_item_t;
static OBJ_CLASS_INSTANCE(callback_list_item_t, opal_list_item_t, NULL, NULL);

/*
 * local data
 */
static opal_list_t callback_list;
static opal_atomic_lock_t callback_lock;
static bool have_free_support = false;
static bool run_callbacks = false;


int
opal_mem_free_init(void)
{
    OBJ_CONSTRUCT(&callback_list, opal_list_t);
    opal_atomic_init(&callback_lock, OPAL_ATOMIC_UNLOCKED);

    run_callbacks = true;
    opal_atomic_mb();

    return OMPI_SUCCESS;
}


int
opal_mem_free_finalize(void)
{
    opal_list_item_t *item;
    
    run_callbacks = false;
    opal_atomic_mb();

    /* aquire the lock, just to make sure no one is currently
       twiddling with the list.  We know this won't last long, since
       no new calls will come in twiddle with the list */
    opal_atomic_lock(&callback_lock);

    while (NULL != (item = opal_list_remove_first(&callback_list))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&callback_list);

    opal_atomic_unlock(&callback_lock);

    return OMPI_SUCCESS;
}


/* called from memory manager / memory-manager specific hooks */
void
opal_mem_free_set_free_support(bool support)
{
    printf("someone set mem_free support to %d\n", (int) support);
    have_free_support = support;
}


/* called from the memory manager / memory-manager specific hooks */
void
opal_mem_free_release_hook(void *buf, size_t length)
{
    opal_list_item_t *item;

    if (!run_callbacks) return;

    opal_atomic_lock(&callback_lock);

    for (item = opal_list_get_first(&callback_list) ;
         item != opal_list_get_end(&callback_list) ;
         item = opal_list_get_next(item)) {
        callback_list_item_t *cbitem = (callback_list_item_t*) item;

        cbitem->cbfunc(buf, length, cbitem->cbdata);
    }

    opal_atomic_unlock(&callback_lock);
}


bool
opal_mem_free_is_supported(void)
{
    return have_free_support;
}


int
opal_mem_free_register_handler(opal_mem_free_unpin_fn_t *func, void *cbdata)
{
    opal_list_item_t *item;
    callback_list_item_t *cbitem;
    int ret = OMPI_SUCCESS;

    if (!have_free_support) return OMPI_ERR_NOT_SUPPORTED;

    opal_atomic_lock(&callback_lock);

    /* make sure the callback isn't already in the list */
    for (item = opal_list_get_first(&callback_list) ;
         item != opal_list_get_end(&callback_list) ;
         item = opal_list_get_next(item)) {
        cbitem = (callback_list_item_t*) item;

        if (cbitem->cbfunc == func) {
            ret = OMPI_EXISTS;
            goto done;
        }
    }

    cbitem = OBJ_NEW(callback_list_item_t);
    if (NULL == cbitem) {
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto done;
    }

    cbitem->cbfunc = func;
    cbitem->cbdata = cbdata;

    opal_list_append(&callback_list, (opal_list_item_t*) cbitem);

 done:
    opal_atomic_unlock(&callback_lock);

    return ret;
}


int
opal_mem_free_unregister_handler(opal_mem_free_unpin_fn_t *func)
{
    opal_list_item_t *item;
    callback_list_item_t *cbitem;
    int ret = OMPI_ERR_NOT_FOUND;

    opal_atomic_lock(&callback_lock);

    /* make sure the callback isn't already in the list */
    for (item = opal_list_get_first(&callback_list) ;
         item != opal_list_get_end(&callback_list) ;
         item = opal_list_get_next(item)) {
        cbitem = (callback_list_item_t*) item;

        if (cbitem->cbfunc == func) {
            opal_list_remove_item(&callback_list, item);
            ret = OMPI_SUCCESS;
            break;
        }
    }

    opal_atomic_unlock(&callback_lock);

    return ret;
}
