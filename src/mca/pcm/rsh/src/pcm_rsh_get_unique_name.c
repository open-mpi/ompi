/* -*- C -*-
 * 
 * $HEADER$
 *
 */

#include "ompi_config.h"

#include "mca/pcm/pcm.h"
#include "mca/pcm/rsh/src/pcm_rsh.h"

#include <string.h>

char *
mca_pcm_rsh_get_unique_name(void)
{
    return strdup("none");
}
