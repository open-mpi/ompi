/* 
 * $HEADER
 */

#include "ompi_config.h"

#include "runtime/runtime.h"
#include "mca/pcm/base/base.h"

#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>


int
main(int argc, char *argv[])
{
    ompi_rte_node_schedule_t *sched;
    ompi_rte_node_allocation_t *nodelist;
    pid_t pid;
    int i;
    int ret;
    int jobid;

    ompi_init(argc, argv);

    /* print our contact information */
    fprintf(stdout, "@BOOTPROXY@\n");

    sched = OBJ_NEW(ompi_rte_node_schedule_t);

    /* recv_schedule wants an already initialized ompi_list_t */
    ret = mca_pcm_base_recv_schedule(stdin, &jobid, sched,
                                     sched->nodelist);
    if (ret != OMPI_SUCCESS) {
        fprintf(stderr, "Failure in receiving schedule information\n");
        exit(1);
    }

    /* sanity check */
    if (ompi_list_get_size(sched->nodelist) > 1) {
        fprintf(stderr, "Received more than one node - ignoring extra info\n");
    }

    /* fill our environment */
    for (i = 0 ; sched->env[i] != NULL ; ++i) {
        putenv(sched->env[i]);
    }

    /* get in the right place */
    if (sched->cwd != NULL) {
        ret = chdir(sched->cwd);
        if (ret != 0) {
            perror("chdir");
            exit(1);
        }
    }

    /* let's go! - if we are the parent, don't stick around... */
    pid = fork();
    if (pid < 0) {
        /* error :( */
        perror("fork");
    } else if (pid == 0) {
        /* child */
        execvp(sched->argv[0], sched->argv);
        perror("exec");
    }

    OBJ_RELEASE(sched);

    ompi_finalize();

    return 0;
} 
