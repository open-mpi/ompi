/* 
 * $HEADER
 */

#include "ompi_config.h"

#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/wait.h>
#include <signal.h>
#include <errno.h>

#include "include/constants.h"
#include "runtime/runtime.h"
#include "mca/pcm/base/base.h"
#include "class/ompi_list.h"


struct pid_item_t {
    ompi_list_item_t super;
    pid_t pid;
};
typedef struct pid_item_t pid_item_t;
OBJ_CLASS_INSTANCE(pid_item_t, ompi_list_item_t, NULL, NULL);


static void
show_usage(char *myname)
{
    printf("usage: %s --local_offset [vpid] --global_start_vpid [vpid]\n"
           "         --num_procs [num] [--high-qos]\n\n", myname);
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
    bool high_qos = false;
    int status;
    ompi_list_t pid_list;
    pid_item_t *pid_list_item;
    ompi_list_item_t *list_item;

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
    ompi_cmd_line_make_opt(cmd_line, '\0', "high_qos", 0, 
                           "Do we want High QOS system (keepalive, etc)");

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
    if (ompi_cmd_line_is_taken(cmd_line, "high_qos"))  high_qos = true;

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

    OBJ_CONSTRUCT(&pid_list, ompi_list_t);

    /* launch processes, and do the right cleanup things */
    for (i = 0 ; i < fork_num_procs ; ++i) {
        pid = fork();
        if (pid < 0) {
            /* error :( */
            perror("fork");
        } else if (pid == 0) {
            /* child */

            /* do the putenv here so that we don't look like we have a
               giant memory leak */
            asprintf(&env_buf, "OMPI_MCA_pcmclient_env_procid=%d", 
                     local_vpid_start + i);
            putenv(env_buf);

            if (!high_qos) {
                for (i = 0; i < FD_SETSIZE; i++)
                    close(i);
            }

            execvp(sched->argv[0], sched->argv);
            perror("exec");
        } else {
            /* parent */

            if (high_qos) {
                pid_list_item = OBJ_NEW(pid_item_t);
                pid_list_item->pid = pid;
                ompi_list_append(&pid_list, 
                                 (ompi_list_item_t*) pid_list_item);
            }
        }
    }

    OBJ_RELEASE(sched);

    status = 1;

    /* if we want qos, hang around until the first process exits.  We
       can clean the rest up later if we want */
    if (high_qos) {
        for (i = 0 ; i < fork_num_procs ; ++i) {
            while (1) {
                pid = waitpid(-1, &status, 0);
                if (! (pid == -1 && errno == EINTR)) break;
            }
            if (! (WIFEXITED(status) && WEXITSTATUS(status) == 0)) break;
        }

        while (NULL != (list_item = ompi_list_remove_first(&pid_list))) {
            pid_list_item = (pid_item_t*) list_item;
            if (pid_list_item->pid != pid) {
                kill(pid_list_item->pid, SIGTERM);
            }
            OBJ_RELEASE(list_item);
        }

    } else {
        status = 0;
    }

    OBJ_DESTRUCT(&pid_list);

    ompi_finalize();

    return status;
} 
