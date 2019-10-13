/**
 * Copyright (c) 2019      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>
#include <string.h>
#ifdef HAVE_SCHED_H
#include <sched.h>
#endif
#include <sys/types.h>
#ifdef HAVE_SYS_MMAN_H
#include <sys/mman.h>
#endif                          /* HAVE_SYS_MMAN_H */
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif                          /* HAVE_UNISTD_H */

#include "mpi.h"
#include "opal_stdint.h"
#include "opal/mca/hwloc/base/base.h"
#include "opal/util/os_path.h"

#include "ompi/communicator/communicator.h"
#include "ompi/group/group.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/base.h"
#include "ompi/mca/rte/rte.h"
#include "ompi/proc/proc.h"
#include "coll_solo.h"
#include "ompi/mca/coll/base/coll_base_functions.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/mca/pml/pml.h"


/**
 * Local functions
 */
static int mca_coll_solo_module_enable(mca_coll_base_module_t * module,
                                       struct ompi_communicator_t *comm);
static int mca_coll_solo_module_disable(mca_coll_base_module_t * module, 
                                        struct ompi_communicator_t *comm);

/* solo module constructor */
static void mca_coll_solo_module_construct(mca_coll_solo_module_t * module)
{
    module->enabled = false;
    module->dynamic_win = NULL;
    module->static_win = NULL;
    module->ctrl_bufs = NULL;
    module->data_bufs = NULL;
    module->barrier_tag = 0;
    module->super.coll_module_disable = mca_coll_solo_module_disable;
}

/* solo module destructor */
static void mca_coll_solo_module_destruct(mca_coll_solo_module_t * module)
{
    return;
}

/* Disable solo module */
static int mca_coll_solo_module_disable(mca_coll_base_module_t * module,
                                        struct ompi_communicator_t *comm)
{
    if (module->base_data != NULL) {
        OBJ_RELEASE(module->base_data);
    }
    mca_coll_solo_module_t *solo_module = (mca_coll_solo_module_t *) module;
    solo_module->enabled = false;

    /* If comm is MPI_COMM_WORLD, windows will be free at ompi_mpi_finalize.c:320 ompi_win_finalize() */
    // if (comm != MPI_COMM_WORLD) {
    //     int rank = ompi_comm_rank(comm);

    //     /* Free the windows */
    //     if (m->dynamic_win != NULL) {
    //         ompi_win_free(m->dynamic_win);
    //     }
    //     if (m->static_win != NULL) {
    //         ompi_win_free(m->static_win);
    //     }
    // }

    if (solo_module->ctrl_bufs != NULL) {
        free(solo_module->ctrl_bufs);
        solo_module->ctrl_bufs = NULL;
    }

    if (solo_module->data_bufs != NULL) {
        free(solo_module->data_bufs);
        solo_module->data_bufs = NULL;
    }

    return OMPI_SUCCESS;
}

OBJ_CLASS_INSTANCE(mca_coll_solo_module_t,
                   mca_coll_base_module_t,
                   mca_coll_solo_module_construct, mca_coll_solo_module_destruct);

/**
 * Initial query function that is invoked during MPI_INIT, allowing this component to disqualify 
 * itself if it doesn't support the required level of thread support.  This function is invoked 
 * exactly once.
 */
int mca_coll_solo_init_query(bool enable_progress_threads, bool enable_mpi_threads)
{
    /* if no session directory was created, then we cannot be used */
    if (NULL == ompi_process_info.job_session_dir) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    /* Don't do much here because we don't really want to allocate any
       shared memory until this component is selected to be used. */
    opal_output_verbose(10, ompi_coll_base_framework.framework_output,
                        "coll:solo:init_query: pick me! pick me!");
    return OMPI_SUCCESS;
}


/**
 * Invoked when there's a new communicator that has been created.
 * Look at the communicator and decide which set of functions and
 * priority we want to return.
 */
mca_coll_base_module_t *mca_coll_solo_comm_query(struct ompi_communicator_t * comm, int *priority)
{
    mca_coll_solo_module_t *solo_module;

    /**
     * If we're intercomm, or if there's only one process in the communicator, or if not all the 
     * processes in the communicator are not on this node, then we don't want to run.
     */
    if (OMPI_COMM_IS_INTER(comm) || 1 == ompi_comm_size(comm)
        || ompi_group_have_remote_peers(comm->c_local_group)) {
        opal_output_verbose(10, ompi_coll_base_framework.framework_output,
                            "coll:solo:comm_query (%d/%s): intercomm, comm is too small, or not all peers local; disqualifying myself",
                            comm->c_contextid, comm->c_name);
        return NULL;
    }

    /* Get the priority level attached to this module. If priority is less
     * than or equal to 0, then the module is unavailable. */
    *priority = mca_coll_solo_component.solo_priority;
    if (0 >= mca_coll_solo_component.solo_priority) {
        opal_output_verbose(10, ompi_coll_base_framework.framework_output,
                            "coll:solo:comm_query (%d/%s): priority too low; disqualifying myself",
                            comm->c_contextid, comm->c_name);
        return NULL;
    }

    solo_module = OBJ_NEW(mca_coll_solo_module_t);
    if (NULL == solo_module) {
        return NULL;
    }

    /* All is good -- return a module */
    solo_module->super.coll_module_enable = mca_coll_solo_module_enable;
    solo_module->super.ft_event = NULL;
    solo_module->super.coll_allgather = NULL;
    solo_module->super.coll_allgatherv = NULL;
    solo_module->super.coll_allreduce = mca_coll_solo_allreduce_intra;
    solo_module->super.coll_alltoall = NULL;
    solo_module->super.coll_alltoallv = NULL;
    solo_module->super.coll_alltoallw = NULL;
    solo_module->super.coll_barrier = mac_coll_solo_barrier_intra;
    solo_module->super.coll_bcast = mca_coll_solo_bcast_intra;
    solo_module->super.coll_exscan = NULL;
    solo_module->super.coll_gather = NULL;
    solo_module->super.coll_gatherv = NULL;
    solo_module->super.coll_reduce = mca_coll_solo_reduce_intra;
    solo_module->super.coll_reduce_scatter = NULL;
    solo_module->super.coll_scan = NULL;
    solo_module->super.coll_scatter = NULL;
    solo_module->super.coll_scatterv = NULL;

    opal_output_verbose(10, ompi_coll_base_framework.framework_output,
                        "coll:solo:comm_query (%d/%s): pick me! pick me!",
                        comm->c_contextid, comm->c_name);
    return &(solo_module->super);
}

/* Init the solo module on the communicator */
static int mca_coll_solo_module_enable(mca_coll_base_module_t * module,
                                         struct ompi_communicator_t *comm)
{
    /* prepare the placeholder for the array of request for invoking base module */
    module->base_data = OBJ_NEW(mca_coll_base_comm_t);
    if (NULL == module->base_data) {
        return OMPI_ERROR;
    }
    return OMPI_SUCCESS;
}

/* Enable the solo module on the communicator lazily */
int mca_coll_solo_lazy_enable(mca_coll_base_module_t * module, struct ompi_communicator_t *comm)
{
    mca_coll_solo_module_t *solo_module = (mca_coll_solo_module_t *) module;

    /**
     * Temporarily use tuned module to prevent the collective operations in this module are invoked
     * before the initialization. 
     */
    int var_id;
    int tmp_priority = 100;
    const int *origin_priority = NULL;
    int tmp_origin = 0;
    mca_base_var_find_by_name("coll_tuned_priority", &var_id);
    mca_base_var_get_value(var_id, &origin_priority, NULL, NULL);
    tmp_origin = *origin_priority;
    mca_base_var_set_flag(var_id, MCA_BASE_VAR_FLAG_SETTABLE, true);
    mca_base_var_set_value(var_id, &tmp_priority, sizeof(int), MCA_BASE_VAR_SOURCE_SET, NULL);
    comm->c_coll->coll_allreduce = ompi_coll_base_allreduce_intra_recursivedoubling;

    /* Create the mpool */
    if (mca_coll_solo_component.solo_mpool == NULL) {
        mca_coll_solo_component.solo_mpool = OBJ_NEW(mca_coll_solo_mpool_t);
    }

    /* Create the dynamic_win */
    ompi_win_create_dynamic((opal_info_t *) (&ompi_mpi_info_null), comm,
                            &solo_module->dynamic_win);

    /* Create the static_win with shared memory allocation */
    mca_coll_solo_setup_static_win(solo_module, comm,
                                     mca_coll_solo_component.static_block_size);

    solo_module->enabled = true;

    /* Set the functions and the priority back */
    comm->c_coll->coll_allreduce = mca_coll_solo_allreduce_intra;
    mca_base_var_set_value(var_id, &tmp_origin, sizeof(int), MCA_BASE_VAR_SOURCE_SET, NULL);
    return OMPI_SUCCESS;
}

/**
 * Attach a memory block to the dynamic_win of a communicator, returns an array contains the 
 * addresses of all the blocks of the processes in the communicator.
 * local_buf == NULL and local_buf_size == 0 means there is no block to be attached on this process.
 */
char **mca_coll_solo_attach_buf(mca_coll_solo_module_t * solo_module,
                                  struct ompi_communicator_t *comm,
                                  char *local_buf, size_t local_buf_size)
{
    int rank = ompi_comm_rank(comm);
    int size = ompi_comm_size(comm);

    char **attached_bufs = (char **) malloc(sizeof(char *) * size);
    attached_bufs[rank] = local_buf;
    ompi_coll_base_allgather_intra_recursivedoubling(MPI_IN_PLACE, 0,
                                                     MPI_DATATYPE_NULL,
                                                     attached_bufs,
                                                     1, MPI_AINT, comm,
                                                     (mca_coll_base_module_t *) solo_module);

    solo_module->dynamic_win->w_osc_module->osc_win_attach(solo_module->dynamic_win, local_buf,
                                                             local_buf_size);

    return attached_bufs;
}

/* Detach a memory block from the dynamic_win of a communicator */
void mca_coll_solo_detach_buf(mca_coll_solo_module_t * solo_module,
                                struct ompi_communicator_t *comm,
                                char *local_buf, char ***attached_bufs)
{
    if (local_buf != NULL) {
        solo_module->dynamic_win->w_osc_module->osc_win_detach(solo_module->dynamic_win, local_buf);
    }

    free(*attached_bufs);
    *attached_bufs = NULL;
    return;
}

/* Setup and initialize the static_win of a communicator */
void mca_coll_solo_setup_static_win(mca_coll_solo_module_t * solo_module,
                                    struct ompi_communicator_t *comm, size_t data_buf_size)
{
    int i;
    int rank = ompi_comm_rank(comm);
    int size = ompi_comm_size(comm);
    int *baseptr;
    /* Create the static win */ 
    ompi_win_allocate_shared(4 * opal_cache_line_size + data_buf_size,
                             sizeof(char),
                             (opal_info_t *) (&ompi_mpi_info_null), comm,
                             &baseptr, &solo_module->static_win);
    size_t static_size[size];
    int static_disp[size];
    solo_module->ctrl_bufs = (char **) malloc(sizeof(char *) * size);
    solo_module->data_bufs = (char **) malloc(sizeof(char *) * size);
    /** 
     * Get the shared memory address created with the static window, 
     * the first 4 * opal_cache_line_size is used for control messages,
     * the rest is used for transfer very small messages.
     */
    for (i = 0; i < size; i++) {
        solo_module->static_win->w_osc_module->osc_win_shared_query(solo_module->static_win, i,
                                                                    &(static_size[i]),
                                                                    &(static_disp[i]),
                                                                    &(solo_module->ctrl_bufs[i]));
        solo_module->data_bufs[i] = (char *) (solo_module->ctrl_bufs[i]) + 4 * opal_cache_line_size;
    }
    /* Init ctrl_bufs with 0s */
    solo_module->static_win->w_osc_module->osc_fence(0, solo_module->static_win);
    for (i = 0; i < 4; i++) {
        char *ptr = solo_module->ctrl_bufs[rank] + i * opal_cache_line_size;
        *((int32_t *) ptr) = 0;
    }
    solo_module->static_win->w_osc_module->osc_fence(0, solo_module->static_win);
}
