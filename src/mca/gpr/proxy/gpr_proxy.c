/*
 * $HEADER$
 */
/** @file:
 *
 */

#include "ompi_config.h"
#include "mca/mca.h"
#include "mca/gpr/base/base.h"
#include "gpr_proxy.h"

/**
 * globals
 */

/*
 * Implemented registry functions
 */

int gpr_proxy_define_segment(char *segment)
{
    return OMPI_SUCCESS;
}


int gpr_proxy_delete_segment(char *segment)
{
    return OMPI_SUCCESS;
}


int gpr_proxy_put(ompi_registry_mode_t mode, char *segment,
		    char **tokens, ompi_registry_object_t *object,
		    int size)
{
    return OMPI_SUCCESS;
}


int gpr_proxy_delete(ompi_registry_mode_t mode,
		       char *segment, char **tokens)
{
    return OMPI_SUCCESS;
}


ompi_registry_index_t* gpr_proxy_index(char *segment)
{
    return NULL;
}


int gpr_proxy_subscribe(ompi_registry_mode_t mode,
			  ompi_registry_notify_action_t action,
			  char *segment, char **tokens)
{
    return OMPI_SUCCESS;
}


int gpr_proxy_unsubscribe(ompi_registry_mode_t mode,
			    char *segment, char **tokens)
{
    return OMPI_SUCCESS;
}


ompi_registry_value_t* gpr_proxy_get(ompi_registry_mode_t mode, char *segment, char **tokens)
{
    return NULL;
}

