/* -*- C -*-
 * 
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */

#include "ompi_config.h"

#include <errno.h>
#include <sys/types.h>
#include <fcntl.h>
#include <sys/bproc.h>
#include <sys/wait.h>

#include "pcm_bproc.h"
#include "mca/pcm/pcm.h"
#include "mca/pcm/base/base.h"
#include "class/ompi_list.h"
#include "util/argv.h"
#include "mca/pcm/base/base_job_track.h"
#include "mca/pcm/base/base_kill_track.h"
#include "runtime/ompi_rte_wait.h"
#include "mca/ns/ns.h"
#include "mca/ns/base/base.h"
#include "runtime/runtime.h"

extern char **environ;

static int
internal_spawn_procs(mca_pcm_bproc_module_t *me,
                     mca_ns_base_jobid_t jobid, 
                     mca_ns_base_vpid_t base_vpid,
                     const char *cmd, char * const argv[],
                     char ***env, int *envc,
                     ompi_list_t *node_alloc,
                     struct bproc_io_t *io, int iolen, int *offset);


static int
internal_bproc_vexecmove_io(int nnodes, int *nodes, int *pids,
                            struct bproc_io_t *io, int iolen,
                            const char *cmd, char * const argv[],
                            char ***env, int *envc, int offset);


int
mca_pcm_bproc_spawn_procs(struct mca_pcm_base_module_1_0_0_t* me_super,
                          mca_ns_base_jobid_t jobid, 
                          ompi_list_t *schedlist)
{
    mca_pcm_bproc_module_t *me = (mca_pcm_bproc_module_t*) me_super;

    ompi_list_item_t *sched_item, *node_item;
    ompi_rte_node_schedule_t *sched;
    ompi_rte_node_allocation_t *node;
    int ret;
    int num_procs = 0;
    struct bproc_io_t *io = NULL;
    int iolen = 0;
    int offset = 0;
    mca_ns_base_vpid_t base_vpid;
    char *base_proc_name_string, *tmp;
    ompi_process_name_t *base_proc_name;
    char **env;
    int envc;

    /* figure out how many procs we have been allocated */
    for (sched_item = ompi_list_get_first(schedlist) ;
         sched_item != ompi_list_get_end(schedlist) ;
         sched_item = ompi_list_get_next(sched_item)) {
        sched = (ompi_rte_node_schedule_t*) sched_item;

        for (node_item = ompi_list_get_first(sched->nodelist) ;
             node_item != ompi_list_get_end(sched->nodelist) ;
             node_item = ompi_list_get_next(node_item)) {
            node = (ompi_rte_node_allocation_t*) node_item;
            if (node->nodes > 0) {
                num_procs += (node->count * node->nodes);
            } else {
                num_procs += node->count;
            }
        }
    }

    base_vpid = ompi_name_server.reserve_range(jobid, num_procs);
    base_proc_name = 
        ompi_name_server.create_process_name(ompi_name_server.get_cellid(ompi_rte_get_self()),
                                             jobid, base_vpid);
    base_proc_name_string = ompi_name_server.get_proc_name_string(base_proc_name);
    ompi_name_server.free_name(base_proc_name);


    /* setup IO forwarding - kind of */
    iolen = 3;
    io = malloc(sizeof(struct bproc_io_t) * iolen);

    io[0].fd = 0;
    io[0].type = BPROC_IO_FILE;
    io[0].flags = 0;
    io[0].d.file.offset = 0;
    strcpy(io[0].d.file.name, "/dev/null");
    io[0].d.file.flags = O_RDONLY;

    io[1].fd = 1;
    io[1].type = BPROC_IO_FILE;
    io[1].flags = 0;
    io[1].d.file.offset = 0;
    strcpy(io[1].d.file.name, "/tmp/stdout");
    io[1].d.file.flags = O_WRONLY|O_CREAT|O_TRUNC;
    io[1].d.file.mode=0666;

    io[2].fd = 2;
    io[2].type = BPROC_IO_FILE;
    io[2].flags = 0;
    io[2].d.file.offset = 0;
    strcpy(io[2].d.file.name, "/tmp/stderr");
    io[2].d.file.flags = O_WRONLY|O_CREAT|O_TRUNC;
    io[2].d.file.mode=0666;

    /* Each sched_item is a different argv/envp/cwd/etc to set, which
       means it is the largest bproc_vexecmove() that can possibly be
       started */
    for (sched_item = ompi_list_get_first(schedlist) ;
         sched_item != ompi_list_get_end(schedlist) ;
         sched_item = ompi_list_get_next(sched_item)) {
        sched = (ompi_rte_node_schedule_t*) sched_item;


        env = ompi_argv_copy(environ);
        envc = ompi_argv_count(env);
        printf("pcm: bproc: environ size: %d\n", envc);
        ompi_argv_insert(&env, envc, sched->env);
        envc = ompi_argv_count(env);
        printf("pcm: bproc: environ + sched->env size: %d\n", envc);

        /* BWB - this has to go ...  need to figure out env in bproc */
        asprintf(&tmp, "LD_LIBRARY_PATH=%s", getenv("LD_LIBRARY_PATH"));
        ompi_argv_append(&envc, &env, tmp);
        free(tmp);
        /* add the base process name & num procs */
        asprintf(&tmp, "OMPI_MCA_pcmclient_bproc_base_name=%s", base_proc_name_string);
        ompi_argv_append(&envc, &env, tmp);
        free(tmp);
        asprintf(&tmp, "OMPI_MCA_pcmclient_bproc_num_procs=%d", num_procs);
        ompi_argv_append(&envc, &env, tmp);
        free(tmp);

        printf("pcm: bproc: spawning procs: %s %s\n", sched->argv[0], base_proc_name_string);
        ret = internal_spawn_procs(me, jobid, base_vpid,
                                   sched->argv[0], sched->argv,
                                   &env, &envc,
                                   sched->nodelist, io, iolen, &offset);
        printf("pcm: bproc: internal_spawn_procs returned %d\n", ret);

        ompi_argv_free(env);

        if (ret != OMPI_SUCCESS) {
            mca_pcm_bproc_kill_job(me_super, jobid, 0);
            return ret;
        }
    }

    free(base_proc_name_string);

    return OMPI_SUCCESS;
}


/* 
 * convert host_list into a node list useable by bproc_vexec or
 * friends.  host_list should be a list of
 * mca_llm_base_hostfile_node_t.  realloc will be called on *nodelist
 * to extend to the correct length, so values must be sane, even on
 * first call to internal_set_nodelist
 *
 * Host names may either be numerical values (-1, 0, 1, etc.), IP
 * addresses (192.168.0.1, etc.), or hostnames.  Note that only recent
 * versions of BProc have name servers that automatically translate
 * master, self, n0, n-1, etc. properly.
 */
static int
internal_set_nodelist(ompi_list_t *host_list,
                      int **nodelist, int *nodelist_len)
{
    ompi_list_item_t *host_item;
    mca_llm_base_hostfile_node_t *host;
    int growlen = 0;
    int i, j;

    /* yeah, this sucks.  Need to figure out how to balance iteration
       cost vs. malloc costs */
    for (host_item = ompi_list_get_first(host_list) ;
         host_item != ompi_list_get_end(host_list) ;
         host_item = ompi_list_get_next(host_item)) {
        host = (mca_llm_base_hostfile_node_t*) host_item;
        growlen += host->count;
    }

    *nodelist = realloc(*nodelist, (*nodelist_len + growlen) * sizeof(int));
    if (*nodelist == NULL) return -1;


    /* BWB - need to extend this to do the proper name lookups */
    i = *nodelist_len;
    for (host_item = ompi_list_get_first(host_list) ;
         host_item != ompi_list_get_end(host_list) ;
         host_item = ompi_list_get_next(host_item)) {
        host = (mca_llm_base_hostfile_node_t*) host_item;

        for (j = 0 ; j < host->count ; ++j) {
            (*nodelist)[i] = atoi(host->hostname);
            printf("pcm: bproc: %s has node number %d (%d) \n", 
                   host->hostname, (*nodelist)[i], i);
            i++;
        }
    }

    *nodelist_len += growlen;

    return 0;
}


/*
 * spawns a proc on a list of hosts.
 */
static int
internal_spawn_procs(mca_pcm_bproc_module_t *me,
                     mca_ns_base_jobid_t jobid, 
                     mca_ns_base_vpid_t base_vpid,
                     const char *cmd, char * const argv[],
                     char ***env, int *envc,
                     ompi_list_t *node_alloc,
                     struct bproc_io_t *io, int iolen, int *offset)
{
    ompi_list_item_t *node_item;
    ompi_rte_node_allocation_t *node;
    mca_llm_base_hostfile_data_t *data;
    int *nodelist = NULL;
    int nodelist_len = 0;
    int *pids;
    int i, ret;

    printf("pcm: bproc: converting hosts -> bproc ids\n");
    /* convert into bproc node list */
    for (node_item = ompi_list_get_first(node_alloc) ;
         node_item != ompi_list_get_end(node_alloc) ;
         node_item = ompi_list_get_next(node_item) ) {
        node = (ompi_rte_node_allocation_t*) node_item;
        data = (mca_llm_base_hostfile_data_t*) node->data;
        ret = internal_set_nodelist(data->hostlist, &nodelist, &nodelist_len);
        if (ret != 0) {
            /* BWB - want an error message here? */
            printf("pcm: bproc: failure to resolve node at %d\n", ret);
            return OMPI_ERROR;
        }
    }

    printf("pcm: bproc: allocating space for returned pids (%d)\n", nodelist_len);
    /* allocate space for the returned pids */
    pids = (int*) malloc(nodelist_len * sizeof(int));
    if (NULL == pids) return OMPI_ERR_OUT_OF_RESOURCE;

    printf("pcm: bproc: starting %d procs (%s) with offset %d\n", 
           nodelist_len,cmd, *offset);
    ret = internal_bproc_vexecmove_io(nodelist_len, nodelist, pids,
                                      io, iolen, cmd, argv,
                                      env, envc, *offset);
    printf("pcm: bproc: vexecmove returned %d\n", ret);

    /* register the returned pids */
    for (i = 0 ; i < nodelist_len ; ++i) {
        printf("pcm: bproc: registering proc %d, pid %d\n", i, pids[i]);

        if (pids[i] < 0) {
            printf("pcm: bproc: invalid pid\n");
            mca_pcm_bproc_kill_job((mca_pcm_base_module_t*) me, jobid, 0);
            errno = pids[i];
            return OMPI_ERR_IN_ERRNO;
        }

        ret = mca_pcm_base_job_list_add_job_info(me->jobs,
                                                 jobid,
                                                 pids[i],
                                                 base_vpid + *offset + i,
                                                 base_vpid + *offset + i);
        if (OMPI_SUCCESS != ret) {
            mca_pcm_bproc_kill_job((mca_pcm_base_module_t*) me, jobid, 0);
            return ret;
        }

        ret = ompi_rte_wait_cb(pids[i], mca_pcm_bproc_monitor_cb, me);
        if (OMPI_SUCCESS != ret) {
            mca_pcm_bproc_kill_job((mca_pcm_base_module_t*) me, jobid, 0);
            return ret;
        }
    }

    /* cleanup and out */
    *offset += nodelist_len;
    return OMPI_SUCCESS;
}


/*
 * actually do the bproc execs.  env will be modified along the way,
 * but will contain the same elements (in the same order) in the end
 */
static int
internal_bproc_vexecmove_io(int nnodes, int *nodes, int *pids,
                            struct bproc_io_t *io, int iolen,
                            const char *cmd, char * const argv[],
                            char ***env, int *envc, int offset)
{
    char *tmp;
    int loc;
    int ret;

    asprintf(&tmp, "OMPI_MCA_pcmclient_bproc_rank_offset=%d", offset);
    ompi_argv_append(envc, env, tmp);
    free(tmp);
    asprintf(&tmp, "BPROC_RANK=XXXXXXX");
    ompi_argv_append(envc, env, tmp);
    free(tmp);
    ret = bproc_vexecmove_io(nnodes, nodes, pids, io, iolen,
                             cmd, argv, *env);
    loc = ompi_argv_count(*env);
    ompi_argv_delete(envc, env, loc - 2, 2);

    return ret;
}

