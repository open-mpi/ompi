#include "ompi_config.h"

#include <stdio.h>
#include <stdlib.h>

#include "mpi.h"
#include "include/constants.h"
#include "class/ompi_list.h"
#include "util/output.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/topo/topo.h"
#include "mca/topo/base/base.h"

ompi_list_t mca_topo_base_modules_available;
bool mca_topo_base_modules_available_valid = false;

static int init_query(const mca_base_component_t *m,
                      mca_base_component_priority_list_item_t *entry);
static int init_query_1_0_0(const mca_base_component_t *component,
                            mca_base_component_priority_list_item_t *entry);
    
int mca_topo_base_find_available(bool *allow_multi_user_threads,
                                 bool *have_hidden_threads) 
{
    bool found = false;
    mca_base_component_priority_list_item_t *entry;
    ompi_list_item_t *p;

    /* Initialize the list */

    OBJ_CONSTRUCT(&mca_topo_base_components_available, ompi_list_t);
    mca_topo_base_components_available_valid = true;

    /* The list of components which we should check is already present 
       in mca_topo_base_components_opened, which was established in 
       mca_topo_base_open */

     for (found = false, 
            p = ompi_list_remove_first (&mca_topo_base_components_opened);
          NULL != p;
          p = ompi_list_remove_first (&mca_topo_base_components_opened)) {
         entry = OBJ_NEW(mca_base_component_priority_list_item_t);
         entry->super.cli_component =
           ((mca_base_component_list_item_t *)p)->cli_component;

         /* Now for this entry, we have to determine the thread level. Call 
            a subroutine to do the job for us */

         if (OMPI_SUCCESS == init_query(entry->super.cli_component, entry)) {
             /* Save the results in the list. The priority is not relvant at 
                this point in time. But we save the thread arguments so that
                the initial selection algorithm can negotiate overall thread
                level for this process */
             entry->cpli_priority = 0;
             ompi_list_append (&mca_topo_base_components_available,
                               (ompi_list_item_t *) entry);
             found = true;
         } else {
             /* The component does not want to run, so close it. Its close()
                has already been invoked. Close it out of the DSO repository
                (if it is there in the repository) */
             mca_base_component_repository_release(entry->super.cli_component);
             OBJ_RELEASE(entry);
         }
         /* Free entry from the "opened" list */
         OBJ_RELEASE(p);
     }

     /* The opened list is no longer necessary, so we can free it */
     OBJ_DESTRUCT (&mca_topo_base_components_opened);
     mca_topo_base_components_opened_valid = false;

     /* There should atleast be one topo component which was available */
     if (false == found) {
         /* Need to free all items in the list */
         OBJ_DESTRUCT(&mca_topo_base_components_available);
         mca_topo_base_components_available_valid = false;
         ompi_output_verbose (10, mca_topo_base_output,
                              "topo:find_available: no topo components available!");
         return OMPI_ERROR;
     }

     /* All done */
     return OMPI_SUCCESS;
}
              
       
static int init_query(const mca_base_component_t *m,
                      mca_base_component_priority_list_item_t *entry) {
    int ret;
    
    ompi_output_verbose(10, mca_topo_base_output,
                        "topo:find_available: querying topo component %s",
                        m->mca_component_name);

    /* This component has been successfully opened, now try to query it */
    if (1 == m->mca_type_major_version &&
        0 == m->mca_type_minor_version &&
        0 == m->mca_type_release_version) {
        ret = init_query_1_0_0 (m, entry);
    } else {
        /* unrecognised API version */
        ompi_output_verbose(10, mca_topo_base_output,
                            "topo:find_available:unrecognised topo API version (%d.%d.%d)",
                            m->mca_type_major_version,
                            m->mca_type_minor_version,
                            m->mca_type_release_version);
        return OMPI_ERROR;
    }

    /* Query done -- look at return value to see what happened */
    if (OMPI_SUCCESS != ret) {
        ompi_output_verbose(10, mca_topo_base_output,
                            "topo:find_available topo component %s is not available",
                            m->mca_component_name);
        if (NULL != m->mca_close_component) {
            m->mca_close_component();
        } 
    } else {
        ompi_output_verbose(10, mca_topo_base_output,
                            "topo:find_avalable: topo component %s is available",
                            m->mca_component_name);

    }
    /* All done */
    return ret;
}


static int init_query_1_0_0(const mca_base_component_t *component,
                            mca_base_component_priority_list_item_t *entry) {
    
    mca_topo_base_component_1_0_0_t *topo = (mca_topo_base_component_1_0_0_t *) component;
    
    return topo->topom_init_query(&(entry->cpli_allow_multi_user_threads),
                                  &(entry->cpli_have_hidden_threads));
}
