/* -*- C -*-
 * 
 * $HEADER$
 *
 */

#include "ompi_config.h"

#include "mca/pcm/base/base.h"
#include "mca/pcm/base/base.h"

#include <string.h>


extern char **environ;

char *
mca_pcm_base_no_unique_name(void)
{
    return strdup("0");
}


int
mca_pcm_base_build_base_env(char ***envp)
{
    char **env = NULL;
    int envc = 0;
    int i, j;

    for (i = 0 ; environ[i] != NULL ; ++i) {
        if (0 != strncmp("OMPI_", environ[i], strlen("OMPI_"))) {
            ++envc;
        }
    }

    env = (char**) malloc(sizeof(char*) * (envc + 1));
    if (NULL == env) return OMPI_ERR_OUT_OF_RESOURCE;

    env[envc] = NULL;
    for (i = 0, j = 0 ; environ[i] != NULL ; ++i) {
        if (0 != strncmp("OMPI_", environ[i], strlen("OMPI_"))) {
            env[j] = strdup(environ[i]);
            ++j;
        }
    }
    
    return OMPI_SUCCESS;
}
