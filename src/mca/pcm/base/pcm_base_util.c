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

char *
mca_pcm_base_no_unique_name(struct mca_pcm_base_module_1_0_0_t* me)
{
    return strdup("0");
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
