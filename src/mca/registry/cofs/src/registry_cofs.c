/* -*- C -*-
 * 
 * $HEADER$
 *
 */

#include "ompi_config.h"

#include "mca/registry/registry.h"
#include "mca/registry/cofs/src/registry_cofs.h"
#include "types.h"

#include <stdio.h>
#include <sys/types.h>
#include <dirent.h>
#include <string.h>
#include <unistd.h>

int
mca_registry_cofs_publish(char* key, void* data, size_t data_len)
{
  return OMPI_ERR_NOT_IMPLEMENTED;
}


int
mca_registry_cofs_lookup(char* key, void** data, size_t* data_len)
{
  return OMPI_ERR_NOT_IMPLEMENTED;
}


int
mca_registry_cofs_unpublish(char* key)
{
  return OMPI_ERR_NOT_IMPLEMENTED;
}
