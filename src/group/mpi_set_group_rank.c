/*
 * $HEADER$
 */

#include "group/group.h"
#include "include/constants.h"

/*
 * Set group rank in a group structure.
 */
void ompi_set_group_rank(ompi_group_t *group, ompi_proc_t *proc_pointer)
{
    /* local variables */
    int proc;

    /* set the rank to MPI_UNDEFINED, just in case this process is not
     *   in this group
     */
    group->grp_my_rank = MPI_UNDEFINED;
    if (NULL != proc_pointer) {
        /* loop over all procs in the group */
        for (proc = 0; proc < group->grp_proc_count; proc++) {
            /* check and see if this proc pointer matches proc_pointer
             */
            if (group->grp_proc_pointers[proc] == proc_pointer) {
                group->grp_my_rank = proc;
            }
        }                       /* end proc loop */
    }

    /* return */
    return;
}
