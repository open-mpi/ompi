/* 
 * $HEADER
 */

#include "ompi_config.h"

#include "include/constants.h"
#include "runtime/runtime.h"
#include "mca/pcm/base/base.h"

#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>

static void
show_usage(char *myname)
{
    printf("usage: %s --local_offset [vpid] --global_start_vpid [vpid]\n"
           "         --num_procs [num]\n\n", myname);
}


int
main(int argc, char *argv[])
{
    ompi_rte_node_schedule_t *sched;
    pid_t pid;
    int i;
    int ret;
    mca_ns_base_jobid_t jobid;
    ompi_cmd_line_t *cmd_line = NULL;
    int local_vpid_start, global_vpid_start;
    int cellid = 0;
    int total_num_procs;
    int fork_num_procs;
    char *env_buf;

    ompi_init(argc, argv);

    /* 
     * command line parsing
     */
    cmd_line = OBJ_NEW(ompi_cmd_line_t);
    ompi_cmd_line_make_opt(cmd_line, '\0', "local_offset", 1, 
                           "starting vpid to use when launching");
    ompi_cmd_line_make_opt(cmd_line, '\0', "global_start_vpid", 1, 
                           "starting vpid to use when launching");
    ompi_cmd_line_make_opt(cmd_line, '\0', "num_procs", 1, 
                           "number of procs in job");

    if (OMPI_SUCCESS != ompi_cmd_line_parse(cmd_line, false, argc, argv)) {
        show_usage(argv[0]);
        exit(1);
    }

    if (!ompi_cmd_line_is_taken(cmd_line, "local_offset")) {
        show_usage(argv[0]);
        exit(1);
    }
    local_vpid_start =
        atoi(ompi_cmd_line_get_param(cmd_line, "local_offset", 0, 0));

    if (!ompi_cmd_line_is_taken(cmd_line, "global_start_vpid")) {
        show_usage(argv[0]);
        exit(1);
    }
    global_vpid_start =
        atoi(ompi_cmd_line_get_param(cmd_line, "global_start_vpid", 0, 0));

    if (!ompi_cmd_line_is_taken(cmd_line, "num_procs")) {
        show_usage(argv[0]);
        exit(1);
    }
    total_num_procs = atoi(ompi_cmd_line_get_param(cmd_line, "num_procs", 0, 0));

    /*
     * Receive the startup schedule for here
     */
    sched = OBJ_NEW(ompi_rte_node_schedule_t);
    if (NULL == sched) {
        printf("Error in OBJ_NEW.  aborting\n");
        exit(1);
    }

    ret = mca_pcm_base_recv_schedule(stdin, &jobid, sched,
                                     &fork_num_procs);
    if (ret != OMPI_SUCCESS) {
        fprintf(stderr, "Failure in receiving schedule information\n");
        exit(1);
    }

    /* fill our environment */
    for (i = 0 ; sched->env[i] != NULL ; ++i) {
        putenv(sched->env[i]);
    }
    /* constant pcmclient info */
    asprintf(&env_buf, "OMPI_MCA_pcmclient_env_cellid=%d", cellid);
    putenv(env_buf);
    asprintf(&env_buf, "OMPI_MCA_pcmclient_env_jobid=%d", jobid);
    putenv(env_buf);
    asprintf(&env_buf, "OMPI_MCA_pcmclient_env_num_procs=%d", total_num_procs);
    putenv(env_buf);
    asprintf(&env_buf, "OMPI_MCA_pcmclient_env_vpid_start=%d", 
             global_vpid_start);
    putenv(env_buf);

    /* get in the right place */
    if (sched->cwd != NULL) {
        ret = chdir(sched->cwd);
        if (ret != 0) {
            perror("chdir");
            exit(1);
        }
    }

    /* let's go! - if we are the parent, don't stick around... */
    for (i = 0 ; i < fork_num_procs ; ++i) {
        pid = fork();
        if (pid < 0) {
            /* error :( */
            perror("fork");
        } else if (pid == 0) {
            /* do the putenv here so that we don't look like we have a
               giant memory leak */
            asprintf(&env_buf, "OMPI_MCA_pcmclient_env_procid=%d", 
                     local_vpid_start + i);
            putenv(env_buf);

            /* child */
            execvp(sched->argv[0], sched->argv);
            perror("exec");
        }
    }

    OBJ_RELEASE(sched);

    ompi_finalize();

    return 0;
} 
