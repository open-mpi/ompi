/*
 *
 * $HEADER$
 *
 */
#include "ompi_config.h"
#include "mca/pcm/pcm.h"
                                                                                                                 
/*
 * Module open / close
 */
int mca_pcm_ompid_open(void);
int mca_pcm_ompid_close(void);
                                                                                                                 
/*
 *  Resource data structures
 */

struct mca_pcm_ompid_node_t {
     ompi_rte_node_allocation_data_t base;
     ompi_process_name_t name;
};
typedef struct mca_pcm_ompid_node_t mca_pcm_ompid_node_t;

OBJ_CLASS_DECLARATION(mca_pcm_ompid_node_t);

/*
 * Startup / Shutdown
 */
struct mca_pcm_base_module_1_0_0_t* mca_pcm_ompid_init(
    int *priority, 
    bool *allow_multi_user_threads, 
    bool *have_hidden_threads,
    int constraints);

int mca_pcm_ompid_finalize(struct mca_pcm_base_module_1_0_0_t* me);
                                                                                                                 

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
    int procs);
                                                                                                         

/**
 * This tells you whether the pcm module is capable of spawning new
 * processes or not during a run
 *
 * @return True/False
 */
                                                                                                         
bool mca_pcm_ompid_can_spawn(struct mca_pcm_base_module_1_0_0_t* me);

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
    ompi_list_t *schedule_list);
                                                                                                         
