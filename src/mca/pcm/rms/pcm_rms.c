/* -*- C -*-
 * 
 * $HEADER$
 *
 */

#include "ompi_config.h"

#include "include/constants.h"
#include "mca/pcm/pcm.h"
#include "mca/pcm/base/base.h"
#include "mca/pcm/rms/pcm_rms.h"
#include "event/event.h"


ompi_list_t *
mca_pcm_rms_allocate_resources(int jobid,
                               int nodes, int procs)
{
    return NULL;
}


bool
mca_pcm_rms_can_spawn(void)
{
    /* it looks like a prun'd job can call prun again...  let's see
       what happens for now.. */
    return true;
}


int
mca_pcm_rms_spawn_procs(int jobid, ompi_list_t *schedlist)
{

    return OMPI_SUCCESS;
}


int
mca_pcm_rms_kill_proc(ompi_process_name_t *name, int flags)
{
    return OMPI_SUCCESS;
}


int
mca_pcm_rms_kill_job(int jobid, int flags)
{
    return OMPI_SUCCESS;
}


int
mca_pcm_rms_deallocate_resources(int jobid,
                                     ompi_list_t *nodelist)
{
    return OMPI_SUCCESS;
}
