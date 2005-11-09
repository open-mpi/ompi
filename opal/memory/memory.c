/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
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

#include "ompi_config.h"

#include <sys/types.h>
#include <sys/mman.h>

#include "opal/util/output.h"
#include "opal/memory/memory.h"
#include "opal/memory/memory_internal.h"
#include "opal/class/opal_list.h"
#include "opal/class/opal_object.h"
#include "ompi/include/constants.h"


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
static int have_free_support = false;
static int run_callbacks = false;
static int have_been_called = 0;

int
opal_mem_free_init(void)
{
    void *tmp;
    OBJ_CONSTRUCT(&callback_list, opal_list_t);
    opal_atomic_init(&callback_lock, OPAL_ATOMIC_UNLOCKED);

    /* delay running callbacks until there is something in the
       registration */
    run_callbacks = false;
    opal_atomic_mb();

    /* make sure things actually work - map then unmap a page */
    tmp = mmap(0, 1, PROT_READ|PROT_WRITE, MAP_ANON, -1, 0);
    munmap(tmp, 1);
    if (0 == have_been_called) {
        if (have_free_support) {
            opal_output(0, "WARNING: free() and munmap() hooks inoperative"
                        "disabling memory hooks.  This");
            opal_output(0, "WARNING: may cause performance degredation.");
        } 

        have_free_support = false;
    }

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
       no new calls will come in after we set run_callbacks to false */
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
opal_mem_free_set_free_support(int support)
{
    have_free_support = support;
}


/* called from the memory manager / memory-manager specific hooks */
void
opal_mem_free_release_hook(void *buf, size_t length)
{
    opal_list_item_t *item;

    have_been_called = 1;

    if (!run_callbacks) return;

    /*
     * This is not really thread safe - but we can't hold the lock
     * while calling the callback function as this routine can
     * be called recursively.
     *
     * Instead, we could set a flag if we are already in the callback,
     * and if called recursively queue the new address/length and allow
     * the initial callback to dispatch this
     */

    opal_atomic_lock(&callback_lock);
    item = opal_list_get_first(&callback_list);
    while(item != opal_list_get_end(&callback_list)) {
        opal_list_item_t* next = opal_list_get_next(item);
        callback_list_item_t cbitem = *(callback_list_item_t*) item;
        item = next;

        opal_atomic_unlock(&callback_lock);
        cbitem.cbfunc(buf, length, cbitem.cbdata);
        opal_atomic_lock(&callback_lock);
    }
    opal_atomic_unlock(&callback_lock);
}


bool
opal_mem_free_is_supported(void)
{
    return (bool) have_free_support;
}


int
opal_mem_free_register_handler(opal_mem_free_unpin_fn_t *func, void *cbdata)
{
    opal_list_item_t *item;
    callback_list_item_t *cbitem, *new_cbitem;
    int ret = OMPI_SUCCESS;

    if (!have_free_support) return OMPI_ERR_NOT_SUPPORTED;

    /* we either have or are about to have a registration that needs
       calling back.  Let the system know it needs to run callbacks
       now */
    run_callbacks = true;
    opal_atomic_mb();

    /* pre-allocate a callback item on the assumption it won't be
       found.  We can't call OBJ_NEW inside the lock because it might
       call realloc */
    new_cbitem = OBJ_NEW(callback_list_item_t);
    if (NULL == new_cbitem) {
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto done;
    }

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

    new_cbitem->cbfunc = func;
    new_cbitem->cbdata = cbdata;

    opal_list_append(&callback_list, (opal_list_item_t*) new_cbitem);

 done:
    opal_atomic_unlock(&callback_lock);

    if (OMPI_EXISTS == ret && NULL != new_cbitem) {
        OBJ_RELEASE(new_cbitem);
    }

    return ret;
}


int
opal_mem_free_unregister_handler(opal_mem_free_unpin_fn_t *func)
{
    opal_list_item_t *item;
    opal_list_item_t *found_item = NULL;
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
            found_item = item;
            ret = OMPI_SUCCESS;
            break;
        }
    }

    opal_atomic_unlock(&callback_lock);

    /* OBJ_RELEASE calls free, so we can't release until we get out of
       the lock */
    if (NULL != found_item) {
        OBJ_RELEASE(item);
    }

    return ret;
}
