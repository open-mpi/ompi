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
#include <sys/bproc.h>

#include "pcm_bproc.h"
#include "mca/pcm/pcm.h"
#include "mca/pcm/base/base.h"
#include "class/ompi_list.h"
#include "util/argv.h"
#include "mca/pcm/base/base_job_track.h"
#include "mca/pcm/base/base_kill_track.h"
#include "runtime/ompi_rte_wait.h"

static int
internal_spawn_procs(mca_pcm_bproc_module_t *me,
                     mca_ns_base_jobid_t jobid, 
                     mca_ns_base_vpid_t base_vpid,
                     char *base_new_procname,
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

    /* Each sched_item is a different argv/envp/cwd/etc to set, which
       means it is the largest bproc_vexecmove() that can possibly be
       started */
    for (sched_item = ompi_list_get_first(schedlist) ;
         sched_item != ompi_list_get_end(schedlist) ;
         sched_item = ompi_list_get_next(sched_item)) {
        sched = (ompi_rte_node_schedule_t*) sched_item;

        ret = internal_spawn_procs(me, jobid, 0, "foobar", 
                                   sched->argv[0], sched->argv,
                                   &(sched->env), &(sched->envc),
                                   sched->nodelist, io, iolen, &offset);

        if (ret != OMPI_SUCCESS) {
            mca_pcm_bproc_kill_job(me_super, jobid, 0);
            return ret;
        }
    }

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

    i = *nodelist_len;
    for (host_item = ompi_list_get_first(host_list) ;
         host_item != ompi_list_get_end(host_list) ;
         host_item = ompi_list_get_next(host_item)) {
        host = (mca_llm_base_hostfile_node_t*) host_item;

        for (j = 0 ; j < host->count ; ++j) {
            (*nodelist)[i] = atoi(host->hostname);
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
                     char *base_new_procname,
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

    /* convert into bproc node list */
    for (node_item = ompi_list_get_first(node_alloc) ;
         node_item != ompi_list_get_end(node_alloc) ;
         node_item = ompi_list_get_next(node_item) ) {
        node = (ompi_rte_node_allocation_t*) node_item;
        data = (mca_llm_base_hostfile_data_t*) node->data;
        ret = internal_set_nodelist(data->hostlist, &nodelist, &nodelist_len);
        if (ret != 0) {
            /* BWB - want an error message here? */
            return OMPI_ERROR;
        }
    }

    /* allocate space for the returned pids */
    pids = (int*) malloc(nodelist_len * sizeof(int));
    if (NULL == pids) return OMPI_ERR_OUT_OF_RESOURCE;
    
    internal_bproc_vexecmove_io(nodelist_len, nodelist, pids,
                                io, iolen, cmd, argv,
                                env, envc, *offset);

    /* register the returned pids */
    for (i = 0 ; i < nodelist_len ; ++i) {
        ret = mca_pcm_base_job_list_add_job_info(me->jobs,
                                                 jobid,
                                                 pids[i],
                                                 base_vpid + *offset + i,
                                                 base_vpid + *offset + i);
        if (OMPI_SUCCESS != ret) {
            mca_pcm_bproc_kill_job((mca_pcm_base_module_t*) me, jobid, 0);
            return ret;
        }

        ret = ompi_rte_wait_cb(pids[i], pcm_bproc_monitor_cb, NULL);
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
    printf("bproc_vexecmove_io called\n");
    ret = bproc_vexecmove_io(nnodes, nodes, pids, io, iolen,
                             cmd, argv, *env);
    loc = ompi_argv_count(*env);
    ompi_argv_shrink(envc, env, loc - 1, 1);

    return ret;
}

