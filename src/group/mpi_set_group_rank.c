/*
 * $HEADER$
 */

#include "group/group.h"
#include "constants.h"

void lam_set_group_rank(lam_group_t *group, lam_proc_t *proc_pointer)
{
    /* local variables */
    int proc;

    /* set the rank to proc_null, just in case this process is not
     *   in this group
     */
    group->grp_my_rank = MPI_PROC_NULL;
    if( NULL != proc_pointer ) {
        /* loop over all procs in the group */
        for ( proc=0 ; proc < group->grp_proc_count ; proc++ ){
            /* check and see if this proc pointer matches proc_pointer
             */
            if( group->grp_proc_pointers[proc] == proc_pointer ) {
                group->grp_my_rank = proc;
            }
        }  /* end proc loop */
    }

    /* return */
    return;
}
