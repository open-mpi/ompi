/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "runtime/runtime.h"
#include "util/output.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/pcm/pcm.h"
#include "mca/oob/oob.h"
#include "mca/oob/base/base.h"


OBJ_CLASS_INSTANCE(
    mca_oob_base_module_t,
    ompi_list_item_t,
    NULL,
    NULL
);

ompi_process_name_t mca_oob_base_self;
ompi_process_name_t mca_oob_base_any;                                                                                                                          

/**
 * Function for selecting one module from all those that are
 * available.
 *
 * Call the init function on all available modules.
 */
int mca_oob_base_init(bool *user_threads, bool *hidden_threads)
{
    ompi_list_item_t *item;
    mca_base_module_list_item_t *mli;
    mca_oob_base_module_t * first;
    mca_oob_base_component_t *component;
    mca_oob_t *module;
    extern ompi_list_t mca_oob_base_modules;
    ompi_process_name_t *self;

    /* setup local name */
    self = mca_pcm.pcm_self();
    if(NULL == self) {
        return OMPI_ERROR;
    }
    mca_oob_base_self = *self;

    /* setup wildcard name */
    mca_oob_base_any.cellid = -1;
    mca_oob_base_any.jobid = -1;
    mca_oob_base_any.vpid = -1;

    /* Traverse the list of available modules; call their init functions. */
    for (item = ompi_list_get_first(&mca_oob_base_components);
        item != ompi_list_get_end(&mca_oob_base_components);
        item = ompi_list_get_next(item)) {
        mca_oob_base_module_t *inited;

        mli = (mca_base_module_list_item_t *) item;
        component = (mca_oob_base_component_t *) mli->mli_module;

        if (NULL == component->oob_init) {
            ompi_output_verbose(10, mca_oob_base_output, "mca_oob_base_init: no init function; ignoring component");
        } else {
            module = component->oob_init(user_threads, hidden_threads);
            if (NULL == module) {
                ompi_output_verbose(10, mca_oob_base_output, "mca_oob_base_init: oob_init returned failure");
            }
        }

        inited = OBJ_NEW(mca_oob_base_module_t);
        inited->oob_component = component;
        inited->oob_module = module;
        ompi_list_append(&mca_oob_base_modules, &inited->super);
    }
    /* set the global variable to point to the first initialize module */
    first = (mca_oob_base_module_t *) ompi_list_get_first(&mca_oob_base_modules);
    if(NULL != first)
        mca_oob = *first->oob_module; 
    return OMPI_SUCCESS;
}

