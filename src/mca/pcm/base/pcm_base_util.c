/* -*- C -*-
 * 
 * $HEADER$
 *
 */

#include "ompi_config.h"

#include <string.h>

#include "include/constants.h"
#include "class/ompi_list.h"
#include "util/argv.h"
#include "runtime/runtime_types.h"
#include "mca/pcm/base/base.h"
#include "mca/llm/base/base.h"
#include "mca/llm/base/base_internal.h"
#include "mca/mca.h"
#include "mca/base/base.h"


char*
mca_pcm_base_get_unique_id(void)
{
    extern ompi_list_t mca_pcm_base_components_available;
    ompi_list_item_t *item;
    mca_base_component_list_item_t *cli;
    mca_pcm_base_component_t *component;
    int priority, top_priority;
    char *id, *top_id;
    int ret;

    top_priority = -1;
    top_id = NULL;
    
    for (item = ompi_list_get_first(&mca_pcm_base_components_available);
         ompi_list_get_end(&mca_pcm_base_components_available) != item;
         item = ompi_list_get_next(item)) {
        cli = (mca_base_component_list_item_t *) item;
        component = (mca_pcm_base_component_t *) cli->cli_component;

        if (NULL == component->pcm_get_unique_id) continue;

        priority = 0;
        id = NULL;

        ret = component->pcm_get_unique_id(&id, &priority);
        if (OMPI_SUCCESS != ret) continue;

        if (priority > top_priority) {
            if (NULL != top_id) free(top_id);
            top_id = id;
            top_priority = priority;
        } else {
            free(id);
        }
    }

    return top_id;
}


int
mca_pcm_base_build_base_env(char **in_env, int *envc, char ***out_envp)
{
    char **env = NULL;
    int i;
    int ret;

    for (i = 0 ; in_env[i] != NULL ; ++i) {
        if (0 == strncmp("OMPI_", in_env[i], strlen("OMPI_"))) {
            ret = ompi_argv_append(envc, &env, in_env[i]);
            if (OMPI_SUCCESS != ret) {
                ompi_argv_free(env);
                return ret;
            }
        }
    }

    *out_envp = env;

    return OMPI_SUCCESS;
}


char *
mca_pcm_base_get_username(mca_llm_base_hostfile_node_t *host)
{
    ompi_list_item_t *item;
    ompi_rte_valuepair_t *valpair;

    for (item = ompi_list_get_first(host->info) ;
         item != ompi_list_get_end(host->info) ;
         item = ompi_list_get_next(item)) {
        valpair = (ompi_rte_valuepair_t*) item;
        if (0 == strcmp("user", valpair->key)) return valpair->value;
    }

    return NULL;
}
