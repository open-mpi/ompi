/* 
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <stdlib.h>
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif
#include <signal.h>
#include <errno.h>

#include "include/constants.h"
#include "runtime/runtime.h"
#include "mca/pcm/base/base.h"
#include "class/ompi_list.h"
#include "util/show_help.h"
#include "mca/ns/ns.h"

/*
 * Local data structure
 */
struct app_opts {
    bool high_qos;
    bool debug;
    mca_ns_base_cellid_t cellid;
    mca_ns_base_jobid_t jobid;
    mca_ns_base_vpid_t local_vpid_start;
    mca_ns_base_vpid_t global_vpid_start;
    int num_job_procs;
    int num_fork_procs;
};

/*
 * Local Data
 */
static pid_t *started_pids;
int sigchld_msg[2];
int sigdie_msg[2];
int sigcount = 0;

/*
 * Local functions
 */
static void
sighandler(int sig)
{
    int i = 1;

    if (SIGCHLD == sig) {
        write(sigchld_msg[1], &i, sizeof(int));
    } else {
        write(sigdie_msg[1], &i, sizeof(int));
    }
}




static int
do_args(int argc, char *argv[], struct app_opts *opts)
{
    ompi_cmd_line_t *cmd_line = NULL;

    cmd_line = OBJ_NEW(ompi_cmd_line_t);
    ompi_cmd_line_make_opt(cmd_line, '\0', "local_offset", 1, 
                           "starting vpid to use when launching");
    ompi_cmd_line_make_opt(cmd_line, '\0', "high_qos", 0, 
                           "Do we want High QOS system (keepalive, etc)");
    ompi_cmd_line_make_opt(cmd_line, '\0', "debug", 0, 
                           "Enable debugging support?");

    if (OMPI_SUCCESS != ompi_cmd_line_parse(cmd_line, false, argc, argv)) {
        return OMPI_ERR_BAD_PARAM;
    }

    if (!ompi_cmd_line_is_taken(cmd_line, "local_offset")) {
        return OMPI_ERR_BAD_PARAM;
    }
    opts->local_vpid_start = 
        atoi(ompi_cmd_line_get_param(cmd_line, "local_offset", 0, 0));

    if (ompi_cmd_line_is_taken(cmd_line, "high_qos"))  {
        opts->high_qos = true;
    } else {
        opts->high_qos = false;
    }

    if (ompi_cmd_line_is_taken(cmd_line, "debug")) {
        opts->debug = true;
    } else {
        opts->debug = false;
    }

    OBJ_RELEASE(cmd_line);

    return OMPI_SUCCESS;
}



int
main(int argc, char *argv[])
{
    int ret;
    struct app_opts opts;
    ompi_rte_node_schedule_t *sched;
    char *tmp_buf;
    int i, orig_errno;
    pid_t pid;
    int status;
    fd_set read_fds, ex_fds;
    int num_running_procs = 0;

    /* make ourselves an OMPI process */
    ret = ompi_init(argc, argv);
    if (OMPI_SUCCESS != ret) {
        ompi_show_help("help-bootproxy.txt", "ompi_init", true, ret);
        exit(1);
    }

    /* get all the arguments and all that */
    ret = do_args(argc, argv, &opts);
    if (OMPI_SUCCESS != ret) {
        ompi_show_help("help-bootproxy.txt", "usage", true);
        exit(1);
    }

    /* receive schedule */
    sched = OBJ_NEW(ompi_rte_node_schedule_t);
    if (NULL == sched) {
        ompi_show_help("help-bootproxy.txt", "system-failure", true,
                       "OBJ_NEW", strerror(errno));
        exit(1);
    }

    ret = mca_pcm_base_recv_schedule(stdin, 
                                     &(opts.cellid), 
                                     &(opts.jobid),
                                     &(opts.global_vpid_start),
                                     &(opts.num_job_procs),
                                     sched,
                                     &(opts.num_fork_procs));
    if (ret != OMPI_SUCCESS) {
        ompi_show_help("help-bootproxy.txt", "could-not-receive-schedule",
                       true, ret);
        exit(1);
    }

    /* fill our environment */
    for (i = 0 ; sched->env[i] != NULL ; ++i) {
        putenv(strdup(sched->env[i]));
    }
    /* constant pcmclient info */
    asprintf(&tmp_buf, "OMPI_MCA_pcmclient_env_cellid=%d", opts.cellid);
    putenv(tmp_buf);
    asprintf(&tmp_buf, "OMPI_MCA_pcmclient_env_jobid=%d", opts.jobid);
    putenv(tmp_buf);
    asprintf(&tmp_buf, "OMPI_MCA_pcmclient_env_num_procs=%d",
             opts.num_job_procs);
    putenv(tmp_buf);
    asprintf(&tmp_buf, "OMPI_MCA_pcmclient_env_vpid_start=%d", 
             opts.global_vpid_start);
    putenv(tmp_buf);

    /* get in the right place */
    if (sched->cwd != NULL) {
        ret = chdir(sched->cwd);
        if (ret != 0) {
            ompi_show_help("help-bootproxy.txt", "could-not-chdir",
                           true, sched->cwd, strerror(errno));
            exit(1);
        }
    }

    /* do the pre-fork setup */
    started_pids = malloc(sizeof(pid_t) * opts.num_fork_procs);
    if (NULL == started_pids) {
        ompi_show_help("help-bootproxy.txt", "system-failure", true,
                       "malloc", strerror(errno));
        exit(1);
    }

    ret = pipe(sigchld_msg);
    if (ret < 0) {
        ompi_show_help("help-bootproxy.txt", "system-failure", true,
                       "pipe", strerror(errno));
        exit(1);
    }
    ret = pipe(sigdie_msg);
    if (ret < 0) {
        ompi_show_help("help-bootproxy.txt", "system-failure", true,
                       "pipe", strerror(errno));
        exit(1);
    }

    if (SIG_ERR == signal(SIGCHLD, sighandler)) {
        ompi_show_help("help-bootproxy.txt", "system-failure", true,
                       "signal", strerror(errno));
    }
    if (SIG_ERR == signal(SIGINT, sighandler)) {
        ompi_show_help("help-bootproxy.txt", "system-failure", true,
                       "signal", strerror(errno));
    }
    if (SIG_ERR == signal(SIGQUIT, sighandler)) {
        ompi_show_help("help-bootproxy.txt", "system-failure", true,
                       "signal", strerror(errno));
    }
    if (SIG_ERR == signal(SIGTERM, sighandler)) {
        ompi_show_help("help-bootproxy.txt", "system-failure", true,
                       "signal", strerror(errno));
    }

    /* launch processes, and do the right cleanup things */
    num_running_procs = 0;
    for (i = 0 ; i < opts.num_fork_procs ; ++i) {
        /* BWB - XXX - fix me.  This sleep is here because the
           registry in mpirun can't keep up if you launch a large number
           of processes all at once.  And with this loop, it really is
           basically all at once.  15 seems to be a good balance for a
           hack.  I was able to launch 30 procs on a 2.0Ghz G5 this way,
           so that should cover us for now */
        if (i % 15 == 0) sleep(1);
        pid = fork();
        if (pid < 0) {
            /* error :( */
            ompi_show_help("help-bootproxy.txt", "could-not-fork",
                           true, sched->argv[0], strerror(errno));
            exit(errno);
        } else if (pid == 0) {
            /* child */

            /* do the putenv here so that we don't look like we have a
               giant memory leak */
            asprintf(&tmp_buf, "OMPI_MCA_pcmclient_env_procid=%d", 
                     opts.local_vpid_start + i);
            putenv(tmp_buf);

            if (!opts.high_qos) {
                for (i = 0; i < FD_SETSIZE; i++)
                    close(i);
            }

            execvp(sched->argv[0], sched->argv);
            /* BWB - fix me - do real path search stuff */
            orig_errno = errno;
            asprintf(&tmp_buf, "./%s", sched->argv[0]);
            execvp(tmp_buf, sched->argv);
            if (ENOENT == errno || ELOOP == errno) {
                /* if we could have found something, use our errno.
                   otherwisse, use the non-hack errno */
                orig_errno = errno;
            } 
            ompi_show_help("help-bootproxy.txt", "could-not-exec",
                           true, sched->argv[0], sched->cwd, 
                           strerror(orig_errno));
            exit(errno);
        } else {
            /* parent */

            if (opts.high_qos) {
                started_pids[i] = pid;
                num_running_procs++;
            }
        }
    }

    OBJ_RELEASE(sched);

    status = 0;

    /* if we want qos, hang around until the first process exits.  We
       can clean the rest up later if we want */
    if (opts.high_qos) {
        while (num_running_procs > 0) {
            int max_fd = 0;
            FD_ZERO(&read_fds);
            FD_ZERO(&ex_fds);
            FD_SET(0, &read_fds);
            FD_SET(0, &ex_fds);
            FD_SET(sigchld_msg[0], &read_fds);
            FD_SET(sigchld_msg[0], &ex_fds);
            FD_SET(sigdie_msg[0], &read_fds);
            FD_SET(sigdie_msg[0], &ex_fds);

            max_fd = sigchld_msg[0] > sigdie_msg[0] ?
                sigchld_msg[0] : sigdie_msg[0];

            ret = select(max_fd + 1, &read_fds, NULL, &ex_fds, NULL);
            if (ret < 0) {
                if (EINTR == errno) {
                    continue; 
                } else {
                    ompi_show_help("help-bootproxy.txt",
                                   "system-failure", true,
                                   "select", strerror(errno));
                    exit(1);
                }
            }

            if (FD_ISSET(0, &read_fds) || FD_ISSET(0, &ex_fds)) {
                /* ssh closed - get us out of here */
                break;

            } else if (FD_ISSET(sigdie_msg[0], &read_fds)) {
                /* we got a death signal - get us out of here */
                break;

            } else if (FD_ISSET(sigchld_msg[0], &read_fds)) {
                int buf;
                /* we got a sigchld.  reap the pid.  If abnormal exit,
                   kill and run */
                ret = read(sigchld_msg[0], &buf, sizeof(int));
                pid = 0;
                while (pid >= 0) {
                    pid = waitpid(-1, &status, WNOHANG);
                    if (pid == -1 && errno == EINTR) { 
                        pid = 0;
                        continue; 
                    }
                    if (pid == -1) break;

                    if (pid > 0) {
                        num_running_procs--;
                        if (! (WIFEXITED(status) && 
                               WEXITSTATUS(status) == 0)) {
                            break;
                        }
                    }
                }
            } else if (FD_ISSET(sigchld_msg[0], &ex_fds) ||
                       FD_ISSET(sigdie_msg[0], &ex_fds)) {
                ompi_show_help("help-bootproxy.txt",
                               "signal-exception", true);
                exit(1);
            } else {
                ompi_show_help("help-bootproxy.txt",
                               "internal-accounting-error", true);
                exit(1);
            }
        }

        /* ok, at this point, we shouldn't have any processes running
           if all went well.  Otherwise, clean everyone up */
        if (num_running_procs > 0) {
            for (i = 0 ; i < opts.num_fork_procs ; ++i) {
                kill(started_pids[i], SIGTERM);
            }
        }
    } else {
        status = 0;
    }

    ompi_finalize();

    return status;
} 
