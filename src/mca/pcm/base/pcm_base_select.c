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
#include "mca/pcm/base/base.h"
#include "class/ompi_list.h"


struct avail_module_t {
    ompi_list_item_t super;
    mca_pcm_base_module_t *module;
    int priority;
};
typedef struct avail_module_t avail_module_t;
OBJ_CLASS_INSTANCE(avail_module_t, ompi_list_item_t, NULL, NULL);


/* insert into the list sorted on priority */
static void
insert_module(ompi_list_t *avail_modules, avail_module_t *module)
{
    ompi_list_item_t *item;
    avail_module_t *module_item;

    for (item = ompi_list_get_first(avail_modules) ;
         item != ompi_list_get_end(avail_modules) ;
         item = ompi_list_get_next(item) ) {
        module_item = (avail_module_t*) item;

        if (module->priority >= module_item->priority) break;
    }

    ompi_list_insert_pos(avail_modules, item, (ompi_list_item_t*) module);
}


/**
 * Function for selecting useable modules from all those that are
 * available.
 *
 * Call the init function on all available modules and get their
 * priorities.  If we are runnning in multi-cell, return all available
 * modules.  Otherwise, rerturn module with highest priority.
 */
int
mca_pcm_base_select(bool have_threads,
                    int constraints,
                    mca_pcm_base_module_t ***modules,
                    size_t *modules_len)
{
    int priority;
    avail_module_t *avail_module;
    ompi_list_item_t *item;
    mca_base_component_list_item_t *cli;
    mca_pcm_base_component_t *component;
    mca_pcm_base_module_t *module;
    extern ompi_list_t mca_pcm_base_components_available;
    ompi_list_t avail_module_list, unused_module_list;
    int i;

    ompi_output_verbose(10, mca_pcm_base_output,
                        "pcm: base: select: starting select code");

    OBJ_CONSTRUCT(&avail_module_list, ompi_list_t);
    OBJ_CONSTRUCT(&unused_module_list, ompi_list_t);

    /* traverse the list of available components ; call their init functions */
    
    for (item = ompi_list_get_first(&mca_pcm_base_components_available);
         ompi_list_get_end(&mca_pcm_base_components_available) != item;
         item = ompi_list_get_next(item)) {
        cli = (mca_base_component_list_item_t *) item;
        component = (mca_pcm_base_component_t *) cli->cli_component;

        priority = 0;

        ompi_output_verbose(10, mca_pcm_base_output, 
                            "pcm: base: select: initializing %s component %s",
                            component->pcm_version.mca_type_name,
                            component->pcm_version.mca_component_name);
        if (NULL == component->pcm_init) {
            ompi_output_verbose(10, mca_pcm_base_output,
                                "pcm: base: select: "
                                "no init function; ignoring component");
        } else {
            module = component->pcm_init(&priority, have_threads, constraints);
            if (NULL == module) {
                ompi_output_verbose(10, mca_pcm_base_output,
                                    "pcm: base: select: init returned failure");
            } else {
                ompi_output_verbose(10, mca_pcm_base_output,
                                    "pcm: base: select: init returned priority %d", 
                                    priority);
                avail_module = OBJ_NEW(avail_module_t);
                avail_module->priority = priority;
                avail_module->module = module;

                insert_module(&avail_module_list, avail_module);
            }
        }
    }

    /* Finished querying all components.  Check for the bozo case */
    if (0 == ompi_list_get_size(&avail_module_list)) {
        /* JMS Replace with show_help */
        ompi_abort(1, "No PCM component available.  This shouldn't happen.");
    }

    /* Deal with multicell / make our priority choice */
    if (0 == (constraints & OMPI_RTE_SPAWN_MULTI_CELL)) {
        /* kick out all but the top */
        ompi_list_splice(&unused_module_list, 
                         ompi_list_get_end(&unused_module_list),
                         &avail_module_list,
                         ompi_list_get_next(ompi_list_get_first(&avail_module_list)),
                         ompi_list_get_end(&avail_module_list));
    }
    
    /* Finalize all non-selected components */
    for (item = ompi_list_get_first(&unused_module_list) ;
         item != ompi_list_get_end(&unused_module_list) ;
         item = ompi_list_get_next(item) ) {
    }

    while (NULL != (item = ompi_list_remove_first(&unused_module_list))) {
        avail_module = (avail_module_t*) item;
        avail_module->module->pcm_finalize(avail_module->module);
        OBJ_RELEASE(item);
    }

    /* put the created guys in their place... */
    *modules_len = ompi_list_get_size(&avail_module_list);
    *modules = malloc(sizeof(mca_pcm_base_module_t*) * *modules_len);
    if (*modules == NULL) return OMPI_ERROR;

    i = 0;
    while (NULL != (item = ompi_list_remove_first(&avail_module_list))) {
        avail_module = (avail_module_t*) item;
        (*modules)[i] = avail_module->module;
        ++i;
        OBJ_RELEASE(item);
    }

    /* All done */

    OBJ_DESTRUCT(&unused_module_list);
    OBJ_DESTRUCT(&avail_module_list);

    ompi_output_verbose(10, mca_pcm_base_output,
                        "pcm: base: select: ending select code");

    return OMPI_SUCCESS;
}
