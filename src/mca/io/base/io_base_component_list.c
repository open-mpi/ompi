/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "class/ompi_list.h"
#include "threads/mutex.h"
#include "mca/base/base.h"
#include "mca/io/io.h"
#include "mca/io/base/base.h"
#include "runtime/ompi_progress.h"

/*
 * Private variables
 */
static bool initialized = false;
static ompi_list_t components_in_use;
static ompi_mutex_t mutex;

struct component_item_t {
    ompi_list_item_t super;
    int refcount;
    mca_io_base_version_t version;
    mca_io_base_components_t component;
};
typedef struct component_item_t component_item_t;
static OBJ_CLASS_INSTANCE(component_item_t, ompi_list_item_t, NULL, NULL);


/*
 * Initialize this interface
 */
int mca_io_base_component_init(void)
{
    OBJ_CONSTRUCT(&components_in_use, ompi_list_t);

    initialized = true;

    ompi_progress_register(mca_io_base_component_run_progress);

    return OMPI_SUCCESS;
}


/*
 * Add a comoponent to the io framework's currently-in-use list, or
 * increase its refcount if it's already in the list.
 */
int mca_io_base_component_add(mca_io_base_components_t *comp)
{
    ompi_list_item_t *item;
    component_item_t *citem;
    mca_base_component_t *c;

    OMPI_THREAD_LOCK(&mutex);

    /* Save the component in ref-counted list of compoonents in use.
       This is used for the progression of non-blocking IO requests.
       If the component is already on the list, just bump up the
       refcount.  Otherwise, add it to the list with a refcount of
       1. */

    for (item = ompi_list_get_first(&components_in_use);
         item != ompi_list_get_end(&components_in_use);
         item = ompi_list_get_next(item)) {
        citem = (component_item_t *) item;

        /* Note the memory / pointer trickery here: we don't care what
           IO version this component is -- all the members of the
           mca_io_base_components union have a base of
           mca_base_component_t.  mca_base_component_compare() will do
           all the Right Things to ensure that the components are the
           same. */

        if (mca_base_component_compare(
                (const mca_base_component_t *) &(citem->component),
                (const mca_base_component_t *) comp) == 0) {
            ++citem->refcount;
            OBJ_RETAIN(citem);
            break;
        }
    }

    /* If we didn't find it, save it */

    if (ompi_list_get_end(&components_in_use) == item) {
        citem = OBJ_NEW(component_item_t);
        citem->refcount = 1;
        citem->component = *comp;

        c = (mca_base_component_t *) (&citem->component);
        if (1 == c->mca_type_major_version &&
            0 == c->mca_type_minor_version &&
            0 == c->mca_type_release_version) {
            citem->version = MCA_IO_BASE_V_1_0_0;
        } else {
            citem->version = MCA_IO_BASE_V_NONE;
        }
        ompi_list_append(&components_in_use, (ompi_list_item_t *) citem);
    }

    OMPI_THREAD_UNLOCK(&mutex);

    /* All done */

    return OMPI_SUCCESS;
}


/*
 * Find a component in the currently-in-use list and decrease its
 * refcount.  If the refcount goes to 0, remove it from the list.
 */
int mca_io_base_component_del(mca_io_base_components_t *comp)
{
    ompi_list_item_t *item;
    component_item_t *citem;

    OMPI_THREAD_LOCK(&mutex);

    /* Find the component in the list */

    for (item = ompi_list_get_first(&components_in_use);
         item != ompi_list_get_end(&components_in_use);
         item = ompi_list_get_next(item)) {
        citem = (component_item_t *) item;

        /* Note the memory / pointer trickery here: we don't care what
           IO version this component is -- all the members of the
           mca_io_base_components union have a base of
           mca_base_component_t. */

        if (mca_base_component_compare(
                (const mca_base_component_t *) &(citem->component),
                (const mca_base_component_t *) comp) == 0) {
            --citem->refcount;
            if (0 == citem->refcount) {
                ompi_list_remove_item(&components_in_use, 
                                      (ompi_list_item_t *) citem);
            }
            OBJ_RELEASE(citem);
            break;
        }
    }

    OMPI_THREAD_UNLOCK(&mutex);

    /* All done */

    return OMPI_SUCCESS;
}


/* in this file so that mutex can be static */
int mca_io_base_component_run_progress(void)
{
    int ret, count = 0;
    ompi_list_item_t *item;
    component_item_t *citem;

    if (! initialized) return 0;

    OMPI_THREAD_LOCK(&mutex);

    /* Go through all the components and call their progress
       function */

    for (item = ompi_list_get_first(&components_in_use);
         item != ompi_list_get_end(&components_in_use);
         item = ompi_list_get_next(item)) {
        citem = (component_item_t *) item;

        switch (citem->version) {
        case MCA_IO_BASE_V_1_0_0:
            ret = citem->component.v1_0_0.io_progress();
            if (ret > 0) {
                count += ret;
            }
            break;

        default:
            break;
        }
    }

    OMPI_THREAD_UNLOCK(&mutex);

    return count;
}


/*
 * Initialize this interface
 */
int mca_io_base_component_finalize(void)
{
    initialized = false;

    ompi_progress_unregister(mca_io_base_component_run_progress);


    OBJ_DESTRUCT(&components_in_use);

    return OMPI_SUCCESS;
}
