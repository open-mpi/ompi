/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 * @file
 *
 * Most of the description of the data layout is in the
 * coll_sm_module.c file.
 */

#include "ompi_config.h"

#include <unistd.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <fcntl.h>

#include "ompi/constants.h"
#include "ompi/mca/coll/base/base.h"
#include "ompi/mca/mpool/base/base.h"
#include "ompi/mca/mpool/mpool.h"
#include "ompi/mca/bcol/base/base.h"
#include "ompi/mca/sbgp/base/base.h"

#include "orte/mca/rml/rml.h"
#include "orte/util/proc_info.h"
#include "orte/util/name_fns.h"
#include "orte/mca/errmgr/errmgr.h"

#include "coll_ml.h"
#include "coll_ml_inlines.h"

#include "ompi/mca/common/netpatterns/common_netpatterns.h"
#include "coll_ml_mca.h"
#include "coll_ml_custom_utils.h"


/*
 * Public string showing the coll ompi_ml V2 component version number
 */
const char *mca_coll_ml_component_version_string =
"Open MPI ml-V2 collective MCA component version " OMPI_VERSION;

/*
 * Local functions
 */

static int ml_open(void);
static int ml_close(void);
static int coll_ml_progress(void);

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

mca_coll_ml_component_t mca_coll_ml_component = {

    /* First, fill in the super */

    {
        /* First, the mca_component_t struct containing meta
           information about the component itself */

        {
            MCA_COLL_BASE_VERSION_2_0_0,

            /* Component name and version */

            "ml",
            OMPI_MAJOR_VERSION,
            OMPI_MINOR_VERSION,
            OMPI_RELEASE_VERSION,

            /* Component open and close functions */

            ml_open,
            ml_close,
        },
        {
            /* The component is not checkpoint ready */
            MCA_BASE_METADATA_PARAM_NONE
        },

        /* Initialization / querying functions */
        mca_coll_ml_init_query,
        mca_coll_ml_comm_query,
    },

    /* ml-component specifc information */

    /* (default) priority */
    0,
    /* Number of levels */
    0,
    /* subgrouping components to use */
    NULL,
    /* basic collectives components to use */
    NULL,
    /* verbose */
    0,
    /* max of communicators */
    0,
    /* min size of comm */
    0,
    /* base_sequence_number */
    0,
};

void mca_coll_ml_abort_ml(char *message)
{
    ML_ERROR(("ML Collective FATAL ERROR: %s", message));
    /* shutdown the MPI */
    ompi_mpi_abort(&ompi_mpi_comm_world.comm, MPI_ERR_INTERN, true);
}
/*
 * progress function
 */

#define INDEX(task) ((task)->my_index_in_coll_schedule)
#define ACTIVE_L    (&mca_coll_ml_component.active_tasks)
#define PENDING_L   (&mca_coll_ml_component.pending_tasks)
#define SEQ_L       (&mca_coll_ml_component.sequential_collectives)

static int coll_ml_progress()
{

    int rc = OMPI_SUCCESS;
    int fn_idx;

    mca_coll_ml_task_status_t *task_status, *task_status_tmp;
    mca_coll_ml_collective_operation_progress_t *seq_coll_op;
    mca_coll_ml_collective_operation_progress_t *seq_coll_op_tmp;

    mca_bcol_base_module_collective_fn_primitives_t progress_fn,
                                                    coll_fn;
    mca_coll_ml_utility_data_t *const_args;
    mca_coll_ml_component_t *cm = &mca_coll_ml_component;

    /* Pasha: Not sure that is it correct way to resolve the problem.
       Iprobe call for progress engine. The progress engine calls for our
       progress and as result the first element on the list is progressed again
       and so we call for Iprobe again.... as result we get HUGE stack.

       One way to prevent it - remove the item from the list, and once you finish
       to process it - put it back.

       Other way - put flag on component, if the progress is running - exit immediate.
     */
    if (cm->progress_is_busy) {
        /* We are already working...*/
        return OMPI_SUCCESS;
    } else {
        cm->progress_is_busy = true;
    }

    /* progress sequential collective operations */
    /* RLG - need to do better here for parallel progress */
    OPAL_THREAD_LOCK(&(cm->sequential_collectives_mutex));
    for (seq_coll_op  = (mca_coll_ml_collective_operation_progress_t *)opal_list_get_first(SEQ_L);
         seq_coll_op != (mca_coll_ml_collective_operation_progress_t *)opal_list_get_end(SEQ_L);
         seq_coll_op  = (mca_coll_ml_collective_operation_progress_t *)opal_list_get_next((opal_list_item_t *)seq_coll_op)){

        fn_idx      = seq_coll_op->sequential_routine.current_active_bcol_fn;
        /* initialize the task */

        if(SEQ_TASK_IN_PROG == seq_coll_op->sequential_routine.current_bcol_status){
            progress_fn = seq_coll_op->coll_schedule->
                component_functions[fn_idx].bcol_function->progress_fn;
        }else{
            /* PPP Pasha - apparently task setup should be called only here. see linr 190 */
            progress_fn = seq_coll_op->coll_schedule->
                            component_functions[fn_idx].bcol_function->coll_fn;
            seq_coll_op->sequential_routine.current_bcol_status =
                SEQ_TASK_IN_PROG;
        }

        const_args  = &seq_coll_op->coll_schedule->component_functions[fn_idx].constant_group_data;
        /* RLG - note need to move to useing coll_ml_utility_data_t as
         * collective argument, rather than  coll_ml_function_t
         */
        rc = progress_fn(&(seq_coll_op->variable_fn_params), (coll_ml_function_t *)const_args);
        if (BCOL_FN_COMPLETE == rc) {
            /* done with this routine */
            seq_coll_op->sequential_routine.current_active_bcol_fn++;
            /* this is totally hardwired for bcast, need a general call-back */
            /*seq_coll_op->variable_fn_params.root_flag = true;*/
            fn_idx      = seq_coll_op->sequential_routine.current_active_bcol_fn;
            if (seq_coll_op->sequential_routine.current_active_bcol_fn  ==
                    seq_coll_op->coll_schedule->n_fns) {
                /* done with this collective - recycle descriptor */

                /* remove from the progress list */
                seq_coll_op_tmp = (mca_coll_ml_collective_operation_progress_t *)
                    opal_list_remove_item(SEQ_L, (opal_list_item_t *)seq_coll_op);

                /* handle fragment completion */
                rc = coll_ml_fragment_completion_processing(seq_coll_op);

                if (OMPI_SUCCESS != rc) {
                    mca_coll_ml_abort_ml("Failed to run coll_ml_fragment_completion_processing");
                }
                /* make sure that for will pick up right one */
                seq_coll_op = seq_coll_op_tmp;
            }else {
                /* task setup */
                /* Pasha - Another call for task setup ? Why ?*/
                rc = seq_coll_op->sequential_routine.seq_task_setup(seq_coll_op);
                /* else, start firing bcol functions */
                while(true) {

                    fn_idx      = seq_coll_op->sequential_routine.current_active_bcol_fn;
                    const_args  = &seq_coll_op->coll_schedule->
                        component_functions[fn_idx].constant_group_data;
                    coll_fn = seq_coll_op->coll_schedule->
                        component_functions[fn_idx].bcol_function->coll_fn;
                    rc = coll_fn(&seq_coll_op->variable_fn_params,
                            (coll_ml_function_t *) const_args);

                    if (BCOL_FN_COMPLETE == rc) {

                        seq_coll_op->sequential_routine.current_active_bcol_fn++;
                        fn_idx  = seq_coll_op->sequential_routine.current_active_bcol_fn;

                        /* done with this routine,
                         * check for collective completion */
                        if (seq_coll_op->sequential_routine.current_active_bcol_fn  ==
                                seq_coll_op->coll_schedule->n_fns) {
                            /* remove from the progress list */
                            seq_coll_op_tmp = (mca_coll_ml_collective_operation_progress_t *)
                                opal_list_remove_item(SEQ_L, (opal_list_item_t *)seq_coll_op);

                            /* handle fragment completion */
                            rc = coll_ml_fragment_completion_processing(seq_coll_op);
                            if (OMPI_SUCCESS != rc) {
                                mca_coll_ml_abort_ml("Failed to run coll_ml_fragment_completion_processing");
                            }

                            /* make sure that for will pick up right one */
                            seq_coll_op = seq_coll_op_tmp;

                            /* break out of while loop */
                            break;
                        }else {
                            /* setup the next task */
                            /* sequential task setup */
                            seq_coll_op->sequential_routine.seq_task_setup(seq_coll_op);
                        }

                    }else if (BCOL_FN_NOT_STARTED == rc) {

                        seq_coll_op->sequential_routine.current_bcol_status = SEQ_TASK_PENDING;

                        break;
                    } else {
                        break;
                    }

                }
            }


        } else if (BCOL_FN_NOT_STARTED == rc ){
            seq_coll_op->sequential_routine.current_bcol_status = SEQ_TASK_PENDING;
        }

    }
    OPAL_THREAD_UNLOCK(&(cm->sequential_collectives_mutex));

    /* general dag's */
    /* see if active tasks can be progressed */
    OPAL_THREAD_LOCK(&(cm->active_tasks_mutex));
    for (task_status  = (mca_coll_ml_task_status_t *)opal_list_get_first(ACTIVE_L);
         task_status != (mca_coll_ml_task_status_t *)opal_list_get_end(ACTIVE_L);
         task_status  = (mca_coll_ml_task_status_t *)opal_list_get_next(task_status)
        )
    {
        /* progress task */
        progress_fn = task_status->bcol_fn->progress_fn;
        const_args = &task_status->ml_coll_operation->coll_schedule->
            component_functions[INDEX(task_status)].constant_group_data;
        rc = progress_fn(&(task_status->ml_coll_operation->variable_fn_params),
                (coll_ml_function_t *)const_args);
        if (BCOL_FN_COMPLETE == rc) {
            ML_VERBOSE(3, ("GOT BCOL_COMPLETED!!!!"));
            rc = mca_coll_ml_task_completion_processing(&task_status, ACTIVE_L);
            if (OMPI_SUCCESS != rc) {
                mca_coll_ml_abort_ml("Failed to run mca_coll_ml_task_completion_processing");
            }
        } else if (BCOL_FN_STARTED == rc) {
            /* nothing to do */
        } else {
            mca_coll_ml_abort_ml("Failed to run mca_coll_ml_task_completion_processing");
        }
    }
    OPAL_THREAD_UNLOCK(&(cm->active_tasks_mutex));

    /* see if new tasks can be initiated */
    OPAL_THREAD_LOCK(&(cm->pending_tasks_mutex));
    for (task_status  = (mca_coll_ml_task_status_t *) opal_list_get_first(PENDING_L);
         task_status != (mca_coll_ml_task_status_t *) opal_list_get_end(PENDING_L);
         task_status  = (mca_coll_ml_task_status_t *) opal_list_get_next(task_status))
    {
        /* check to see if dependencies are satisfied */
        int n_dependencies = task_status->rt_num_dependencies;
        int n_dependencies_satisfied = task_status->n_dep_satisfied;

        if (n_dependencies == n_dependencies_satisfied) {
            /* initiate the task */
            coll_fn = task_status->bcol_fn->coll_fn;
            const_args = &task_status->ml_coll_operation->coll_schedule->
                component_functions[INDEX(task_status)].constant_group_data;
            rc = coll_fn(&(task_status->ml_coll_operation->variable_fn_params),
                    (coll_ml_function_t *)const_args);
            if (BCOL_FN_COMPLETE == rc) {
                ML_VERBOSE(3, ("GOT BCOL_COMPLETED!"));
                rc = mca_coll_ml_task_completion_processing(&task_status, PENDING_L);
                if (OMPI_SUCCESS != rc) {
                    mca_coll_ml_abort_ml("Failed to run mca_coll_ml_task_completion_processing");
                }
            } else if ( BCOL_FN_STARTED == rc ) {
                ML_VERBOSE(3, ("GOT BCOL_STARTED!"));
                task_status_tmp = (mca_coll_ml_task_status_t *)
                    opal_list_remove_item(PENDING_L, (opal_list_item_t *)task_status);
                /* RLG - is there potential for deadlock here ?  Need to
                 * look at this closely
                 */
                OPAL_THREAD_LOCK(&(cm->active_tasks_mutex));
                opal_list_append(ACTIVE_L, (opal_list_item_t *)task_status);
                OPAL_THREAD_UNLOCK(&(cm->active_tasks_mutex));
                task_status = task_status_tmp;
            } else if( BCOL_FN_NOT_STARTED == rc ) {
                /* nothing to do */
                ML_VERBOSE(10, ("GOT BCOL_FN_NOT_STARTED!"));
            } else {
                OPAL_THREAD_UNLOCK(&(cm->pending_tasks_mutex));
                /* error will be returned - RLG : need to reconsider return
                 * types - we have no way to convey error information
                 * the way the code is implemented now */
                ML_VERBOSE(3, ("GOT error !"));
                rc = OMPI_ERROR;
                break;
            }
        }
    }
    OPAL_THREAD_UNLOCK(&(cm->pending_tasks_mutex));

    /* return */
    cm->progress_is_busy = false;

    return rc;
}


static void adjust_coll_config_by_mca_param(void)
{
    assert(false == mca_coll_ml_component.use_static_bcast ||
           false == mca_coll_ml_component.use_sequential_bcast);
	
    /* setting bcast mca params */
    if (mca_coll_ml_component.use_static_bcast) {
        mca_coll_ml_component.coll_config[ML_BCAST][ML_SMALL_MSG].algorithm_id = ML_BCAST_SMALL_DATA_KNOWN;
        mca_coll_ml_component.coll_config[ML_BCAST][ML_LARGE_MSG].algorithm_id = ML_BCAST_LARGE_DATA_KNOWN;
    } else if (mca_coll_ml_component.use_sequential_bcast){
        mca_coll_ml_component.coll_config[ML_BCAST][ML_SMALL_MSG].algorithm_id = ML_BCAST_SMALL_DATA_SEQUENTIAL;
        mca_coll_ml_component.coll_config[ML_BCAST][ML_LARGE_MSG].algorithm_id = ML_BCAST_LARGE_DATA_SEQUENTIAL;
    } else { /* Unknown root */
        mca_coll_ml_component.coll_config[ML_BCAST][ML_SMALL_MSG].algorithm_id = ML_BCAST_SMALL_DATA_UNKNOWN;
        mca_coll_ml_component.coll_config[ML_BCAST][ML_LARGE_MSG].algorithm_id = ML_BCAST_LARGE_DATA_UNKNOWN;
    }
}

/*
 * Open the component
 */
static int ml_open(void)
{
    /* local variables */
    int rc, c_idx, m_idx;
    mca_coll_ml_component_t *cs = &mca_coll_ml_component;


    /* set the starting sequence number */
    cs->base_sequence_number = -1;
    cs->progress_is_busy = false;

    /* load mca parametres */
    rc = mca_coll_ml_register_params();
    if (OMPI_SUCCESS != rc) {
        return OMPI_ERROR;
    }

    /* If the priority is zero (default) disable the component */
    if (mca_coll_ml_component.ml_priority <= 0) {
        return OMPI_ERR_NOT_AVAILABLE;
    }

    /* Init memory structures (no real memory is allocated) */
    OBJ_CONSTRUCT(&cs->memory_manager, mca_coll_ml_lmngr_t);

    if (OMPI_SUCCESS != (rc = mca_sbgp_base_open())) {
        fprintf(stderr," failure in open mca_sbgp_base_open \n");
        return rc;
    }
    if (OMPI_SUCCESS != (rc = mca_bcol_base_open())) {
        fprintf(stderr," failure in open mca_bcol_base_open \n");
        return rc;
    }

    /* Reset collective tunings cache */
    for (c_idx = 0; c_idx < ML_NUM_OF_FUNCTIONS; c_idx++) {
        for (m_idx = 0; m_idx < ML_NUM_MSG; m_idx++) {
            mca_coll_ml_reset_config(&cs->coll_config[c_idx][m_idx]);
        }
    }

    adjust_coll_config_by_mca_param();

    /* Load configuration file and cache the configuration on component */
    rc = mca_coll_ml_config_file_init();
    if (OMPI_SUCCESS != rc) {
        return OMPI_ERROR;
    }

    /* reigster the progress function */
    rc = opal_progress_register(coll_ml_progress);
    if (OMPI_SUCCESS != rc ) {
        fprintf(stderr," failed to register the ml progress function \n");
        fflush(stderr);
        return rc;
    }

    OBJ_CONSTRUCT(&(cs->pending_tasks_mutex), opal_mutex_t);
    OBJ_CONSTRUCT(&(cs->pending_tasks), opal_list_t);
    OBJ_CONSTRUCT(&(cs->active_tasks_mutex), opal_mutex_t);
    OBJ_CONSTRUCT(&(cs->active_tasks), opal_list_t);
    OBJ_CONSTRUCT(&(cs->sequential_collectives_mutex), opal_mutex_t);
    OBJ_CONSTRUCT(&(cs->sequential_collectives), opal_list_t);

    rc = ompi_common_netpatterns_init();
    if (OMPI_SUCCESS != rc) {
        return rc;
    }

    cs->topo_discovery_fn[COLL_ML_HR_FULL] =
        mca_coll_ml_fulltree_hierarchy_discovery;

    cs->topo_discovery_fn[COLL_ML_HR_ALLREDUCE] =
        mca_coll_ml_allreduce_hierarchy_discovery;

    cs->topo_discovery_fn[COLL_ML_HR_NBS] =
        mca_coll_ml_fulltree_exclude_basesmsocket_hierarchy_discovery;

    cs->topo_discovery_fn[COLL_ML_HR_SINGLE_PTP] =
        mca_coll_ml_fulltree_ptp_only_hierarchy_discovery;

    cs->topo_discovery_fn[COLL_ML_HR_SINGLE_IBOFFLOAD] =
        mca_coll_ml_fulltree_iboffload_only_hierarchy_discovery;

    cs->need_allreduce_support = false;

    return OMPI_SUCCESS;
}

/*
 * Close the component
 */
static int ml_close(void)
{
    int ret;

    mca_coll_ml_component_t *cs = &mca_coll_ml_component;

    /* There is not need to release/close resource if the 
     * priority was set to zero */
    if (cs->ml_priority <= 0) {
        return OMPI_SUCCESS;
    }

    OBJ_DESTRUCT(&cs->memory_manager);

    /* deregister progress function */
    ret=opal_progress_unregister(coll_ml_progress);
    if (OMPI_SUCCESS != ret ) {
        fprintf(stderr," failed to un-register the ml progress function \n");
        fflush(stderr);
        return ret;
    }

    return OMPI_SUCCESS;
}

/* query to see if the component is available for use, and can
 * satisfy the thread and progress requirements
 */
int mca_coll_ml_init_query(bool enable_progress_threads,
                           bool enable_mpi_threads)
{
    int ret;

    /* at this stage there is no reason to disaulify this component */
    /* Add here bcol init nand sbgp init */
    ret = mca_sbgp_base_init(enable_progress_threads, enable_mpi_threads);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    ret = mca_bcol_base_init(enable_progress_threads, enable_mpi_threads);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    /* done */
    return OMPI_SUCCESS;
}
