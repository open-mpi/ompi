/* -*- C -*-
 *
 * $HEADER$
 * 
 */

#include "ompi_config.h"
#include <stdio.h>
#include <sys/types.h>
#include <string.h>
#include <unistd.h>

#include "mca/pcm/pcm.h"
#include "include/types.h"
#include "include/constants.h"
#include "util/bufpack.h"
#include "util/output.h"
#include "mca/oob/base/base.h"
#include "mca/oob/oob.h"
#include "pcm_ompid.h"

                                                                                                                             
OBJ_CLASS_INSTANCE(
    mca_pcm_ompid_node_t,
    ompi_rte_node_allocation_data_t,
    NULL,
    NULL);

/**
 * Allocate requested resources
 *
 * Allocate the specified nodes / processes for use in a new job.
 * Requires a jobid from the PCM interface.  The allocation returned
 * may be smaller than requested - it is up to the caller to proceed
 * as appropriate should this occur.  This function should only be
 * called once per jobid.
 *
 * @param jobid (IN) Jobid with which to associate the given resources.
 * @param nodes (IN) Number of nodes to try to allocate. If 0,
 *                   the PCM will try to allocate <code>procs</code>
 *                   processes on as many nodes as are needed.  If non-zero,
 *                   will try to fairly distribute <code>procs</code>
 *                   processes over the nodes.  If <code>procs</code> is 0,
 *                   will attempt to allocate all cpus on
 *                   <code>nodes</code> nodes
 * @param procs (IN) Number of processors to try to allocate.  See the note
 *                   for <code>nodes</code> for usage.
 * @param nodelist (OUT) List of <code>mca_pcm_node_t</code>s describing
 *                   the allocated resources.
 */
                                                                                                                           
ompi_list_t* mca_pcm_ompid_allocate_resources(
    struct mca_pcm_base_module_1_0_0_t* me,
    mca_ns_base_jobid_t jobid,
    int nodes,
    int procs)
{
    ompi_buffer_t request;
    ompi_buffer_t response;
    int32_t _jobid = jobid;
    int32_t _nodes = nodes;
    int32_t _procs = procs;
    int32_t status;
    int32_t i, count;
    ompi_list_t* nodelist = OBJ_NEW(ompi_list_t);
    int rc;
    int tag = MCA_OOB_TAG_SCHED;

    /* build a request to send to the sched service */
    ompi_buffer_init(&request, 32);
    ompi_pack(request, &_jobid, 1, OMPI_INT32);
    ompi_pack(request, &_nodes, 1, OMPI_INT32);
    ompi_pack(request, &_procs, 1, OMPI_INT32);
    rc = mca_oob_send_packed(MCA_OOB_NAME_SEED, request, MCA_OOB_TAG_SCHED, 0);
    if(rc < 0) {
        ompi_output(0, "mca_pcm_ompid_allocate_resources: mca_oob_send_packed failed with error=%d\n", rc);
        return NULL;
    }
    ompi_buffer_free(request);

    /* wait on the response */
    rc = mca_oob_recv_packed(MCA_OOB_NAME_SEED, &response, &tag);
    if(rc < 0) {
        ompi_output(0, "mca_pcm_ompid_allocate_resources: mca_oob_recv_packed failed with error=%d\n", rc);
        return NULL;
    }

    /* unpack the response and build the nodelist */
    ompi_unpack(response, &status, 1, OMPI_INT32);
    if(status != OMPI_SUCCESS) {
        ompi_buffer_free(response);
        return NULL;
    }

    /* iterate through the nodelist */
    ompi_unpack(response, &count, 1, OMPI_INT32);
    for(i=0; i<count; i++) {
        ompi_rte_node_allocation_t* node = OBJ_NEW(ompi_rte_node_allocation_t);
        mca_pcm_ompid_node_t* ompid = OBJ_NEW(mca_pcm_ompid_node_t);
        int32_t args[3];
        ompi_unpack(response, args, 3, OMPI_INT32);
        node->start = args[0];
        node->nodes = args[1];
        node->count = args[2];
        node->data = &ompid->base;
        ompi_unpack(response, &ompid->name, 1, OMPI_NAME);
        ompi_list_append(nodelist, &node->super);
    }
    ompi_buffer_free(response);
    return nodelist;
}
                                                                                                                           
/**
 * This tells you whether the pcm module is capable of spawning new
 * processes or not during a run
 *
 * @return True/False
 */
                                                                                                                           
bool mca_pcm_ompid_can_spawn(struct mca_pcm_base_module_1_0_0_t* me)
{
    return true;
}
                                                                                                                           
/**
 * Spawn a job
 *
 * Start a job with given jobid and starting vpid (should probably be
 * 0 for the forseeable future).  The job is specified using an array
 * of \c mca_pcm_base_schedule_t structures, which give both process
 * and location information.
 */
                                                                                                                           
int mca_pcm_ompid_spawn_procs(
    struct mca_pcm_base_module_1_0_0_t* me,
    mca_ns_base_jobid_t jobid,
    ompi_list_t *schedule_list)
{
    int rc;
    ompi_list_item_t* s_item;
    for(s_item =  ompi_list_get_first(schedule_list);
        s_item != ompi_list_get_end(schedule_list);
        s_item =  ompi_list_get_next(s_item)) {

        ompi_rte_node_schedule_t* schedule = (ompi_rte_node_schedule_t*)s_item;
        ompi_list_item_t* n_item;

        for(n_item =  ompi_list_get_first(schedule->nodelist);
            n_item != ompi_list_get_end(schedule->nodelist);
            n_item =  ompi_list_get_next(n_item)) {

            ompi_rte_node_allocation_t* node = (ompi_rte_node_allocation_t*)n_item;
            mca_pcm_ompid_node_t* ompid = (mca_pcm_ompid_node_t*)node->data;
            ompi_buffer_t request;
            ompi_buffer_t response;
            int32_t i;
            int32_t status;
            int32_t num_procs = node->count;     /* number of processes on this node */
            int32_t num_env = 0;                 /* number of environment variables */
            int32_t num_argc = schedule->argc;   /* number of command line options */
            int32_t base_pid = node->start;
            int tag = MCA_OOB_TAG_EXEC;

            /* pack the request */
            ompi_buffer_init(&request, 256);
            ompi_pack(request, &num_procs, 1, OMPI_INT32);   
            ompi_pack(request, &base_pid, 1, OMPI_INT32);
            /* pack the command line */
            ompi_pack(request, &num_argc, 1, OMPI_INT32);
            for(i=0; i<num_argc; i++)
                ompi_pack_string(request, schedule->argv[i]);
            /* pack the environment */
            for(i=0; schedule->env[i]; i++)
                num_env++;
            ompi_pack(request, &num_env, 1, OMPI_INT32);
            for(i=0; i<num_env; i++)
                ompi_pack_string(request, schedule->env[i]);
            rc = mca_oob_send_packed(&ompid->name, request, tag, 0);
            if(rc != OMPI_SUCCESS) {
                return rc;
            }
            ompi_buffer_free(request);

            /* wait on a response */
            rc = mca_oob_recv_packed(&ompid->name, &response, &tag);
            if(rc != OMPI_SUCCESS) {
                return rc;
            }

            /* unpack the response which contains the array of pids 
             * and overall completion status 
            */
            for(i=0; i<num_procs; i++) {
                int32_t pid;
                ompi_unpack(response, &pid, 1, OMPI_INT32);
            }
            ompi_unpack(response, &status, 1, OMPI_INT32);
            if(status != OMPI_SUCCESS)
                return status;
            ompi_buffer_free(response);
        }
    }
    return OMPI_SUCCESS;
}
                                                                                                                           

