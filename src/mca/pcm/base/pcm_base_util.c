/* -*- C -*-
 * 
 * $HEADER$
 *
 */

#include "ompi_config.h"

#include "mca/pcm/base/base.h"
#include "mca/pcm/base/base.h"

#include <string.h>


char *
mca_pcm_base_no_unique_name(void)
{
    return strdup("0");
}


int
mca_pcm_base_build_base_env(char **in_env, char ***out_envp)
{
    char **env = NULL;
    int envc = 0;
    int i;
    int ret;

    for (i = 0 ; in_env[i] != NULL ; ++i) {
        if (0 == strncmp("OMPI_", in_env[i], strlen("OMPI_"))) {
            ret = ompi_argv_append(&envc, &env, in_env[i]);
            if (OMPI_SUCCESS != ret) {
                ompi_argv_free(env);
                return ret;
            }
        }
    }

    *out_envp = env;

    return OMPI_SUCCESS;
}
