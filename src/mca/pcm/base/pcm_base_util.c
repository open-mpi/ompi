/* -*- C -*-
 * 
 * $HEADER$
 *
 */

#include "ompi_config.h"

#include "mca/pcm/pcm.h"

#include <string.h>

char *
mca_pcm_base_no_unique_name(void)
{
    return strdup("0");
}
