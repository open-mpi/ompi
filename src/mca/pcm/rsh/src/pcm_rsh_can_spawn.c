/* -*- C -*-
 * 
 * $HEADER$
 *
 */

#include "ompi_config.h"

#include "mca/pcm/pcm.h"
#include "mca/pcm/rsh/src/pcm_rsh.h"

bool
mca_pcm_rsh_can_spawn(void)
{
    /* we can always try to rsh some more...  Might not always work as
     * the caller hopes
     */
    return true;
}
