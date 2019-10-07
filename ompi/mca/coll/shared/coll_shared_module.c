/*
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
#include "coll_shared.h"
#include "ompi/mca/coll/base/coll_base_functions.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/mca/pml/pml.h"


/*
 * Local functions
 */
static int mca_coll_shared_module_enable(mca_coll_base_module_t * module,
                                         struct ompi_communicator_t *comm);
static int mca_coll_shared_module_disable(mca_coll_base_module_t * module, struct ompi_communicator_t
                                          *comm);

/*
 * Module constructor
 */
static void mca_coll_shared_module_construct(mca_coll_shared_module_t *
                                             module)
{
    module->enabled = false;
    module->dynamic_win = NULL;
    module->static_win = NULL;
    module->ctrl_buf = NULL;
    module->data_buf = NULL;
    module->barrier_tag = 0;
    module->super.coll_module_disable = mca_coll_shared_module_disable;
}

/*
 * Module destructor
 */
static void mca_coll_shared_module_destruct(mca_coll_shared_module_t *
                                            module)
{
    return;
}

/*
 * Module disable
 */
static int mca_coll_shared_module_disable(mca_coll_base_module_t * module,
                                          struct ompi_communicator_t *comm)
{
    if (module->base_data != NULL) {
        OBJ_RELEASE(module->base_data);
    }
    mca_coll_shared_module_t *shared_module = (mca_coll_shared_module_t *) module;
    shared_module->enabled = false;

    // /* If comm is MPI_COMM_WORLD, windows will be free at ompi_mpi_finalize.c:320 ompi_win_finalize() */
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
    
    if (shared_module->ctrl_buf != NULL) {
        free(shared_module->ctrl_buf);
        shared_module->ctrl_buf = NULL;
    }

    if (shared_module->data_buf != NULL) {
        free(shared_module->data_buf);
        shared_module->data_buf = NULL;
    }

    return OMPI_SUCCESS;
}

OBJ_CLASS_INSTANCE(mca_coll_shared_module_t,
                   mca_coll_base_module_t,
                   mca_coll_shared_module_construct,
                   mca_coll_shared_module_destruct);

/*
 * Initial query function that is invoked during MPI_INIT, allowing
 * this component to disqualify itself if it doesn't support the
 * required level of thread support.  This function is invoked exactly
 * once.
 */
int mca_coll_shared_init_query(bool enable_progress_threads,
                               bool enable_mpi_threads)
{
    /* if no session directory was created, then we cannot be used */
    if (NULL == ompi_process_info.job_session_dir) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    /* Don't do much here because we don't really want to allocate any
       shared memory until this component is selected to be used. */
    opal_output_verbose(10, ompi_coll_base_framework.framework_output,
                        "coll:shared:init_query: pick me! pick me!");
    return OMPI_SUCCESS;
}


/*
 * Invoked when there's a new communicator that has been created.
 * Look at the communicator and decide which set of functions and
 * priority we want to return.
 */
mca_coll_base_module_t *mca_coll_shared_comm_query(struct
                                                   ompi_communicator_t *
                                                   comm, int *priority)
{
    mca_coll_shared_module_t *shared_module;

    /* If we're intercomm, or if there's only one process in the
     * communicator, or if not all the processes in the communicator
     * are not on this node, then we don't want to run */
    if (OMPI_COMM_IS_INTER(comm) || 1 == ompi_comm_size(comm)
        || ompi_group_have_remote_peers(comm->c_local_group)) {
        opal_output_verbose(10, ompi_coll_base_framework.framework_output,
                            "coll:shared:comm_query (%d/%s): intercomm, comm is too small, or not all peers local; disqualifying myself",
                            comm->c_contextid, comm->c_name);
        return NULL;
    }

    /* Get the priority level attached to this module. If priority is less
     * than or equal to 0, then the module is unavailable. */
    *priority = mca_coll_shared_component.shared_priority;
    if (0 >= mca_coll_shared_component.shared_priority) {
        opal_output_verbose(10, ompi_coll_base_framework.framework_output,
                            "coll:shared:comm_query (%d/%s): priority too low; disqualifying myself",
                            comm->c_contextid, comm->c_name);
        return NULL;
    }

    shared_module = OBJ_NEW(mca_coll_shared_module_t);
    if (NULL == shared_module) {
        return NULL;
    }

    /* All is good -- return a module */
    shared_module->super.coll_module_enable =
        mca_coll_shared_module_enable;
    shared_module->super.ft_event = NULL;
    shared_module->super.coll_allgather = NULL;
    shared_module->super.coll_allgatherv = NULL;
    shared_module->super.coll_allreduce = mca_coll_shared_allreduce_intra;
    shared_module->super.coll_alltoall = NULL;
    shared_module->super.coll_alltoallv = NULL;
    shared_module->super.coll_alltoallw = NULL;
    shared_module->super.coll_barrier = mac_coll_shared_barrier_intra;
    shared_module->super.coll_bcast = mca_coll_shared_bcast_intra;
    shared_module->super.coll_exscan = NULL;
    shared_module->super.coll_gather = NULL;
    shared_module->super.coll_gatherv = NULL;
    shared_module->super.coll_reduce = mca_coll_shared_reduce_intra;
    shared_module->super.coll_reduce_scatter = NULL;
    shared_module->super.coll_scan = NULL;
    shared_module->super.coll_scatter = NULL;
    shared_module->super.coll_scatterv = NULL;

    opal_output_verbose(10, ompi_coll_base_framework.framework_output,
                        "coll:shared:comm_query (%d/%s): pick me! pick me!",
                        comm->c_contextid, comm->c_name);
    return &(shared_module->super);
}


/*
 * Init module on the communicator
 */
static int mca_coll_shared_module_enable(mca_coll_base_module_t * module,
                                         struct ompi_communicator_t *comm)
{
    /* prepare the placeholder for the array of request for invoking base module */
    module->base_data = OBJ_NEW(mca_coll_base_comm_t);
    if (NULL == module->base_data) {
        return OMPI_ERROR;
    }
    return OMPI_SUCCESS;
}

int mca_coll_shared_lazy_enable(mca_coll_base_module_t * module,
                                struct ompi_communicator_t *comm)
{
    mca_coll_shared_module_t *shared_module =
        (mca_coll_shared_module_t *) module;    
    
    /* Temporarily use tuned module to prevent the collective operations 
       in this module are invoked before the initialization. */
    int var_id;
    int tmp_priority = 100;
    const int *origin_priority = NULL;
    int tmp_origin = 0;
    mca_base_var_find_by_name("coll_tuned_priority", &var_id);
    mca_base_var_get_value(var_id, &origin_priority, NULL, NULL);
    tmp_origin = *origin_priority;
    mca_base_var_set_flag(var_id, MCA_BASE_VAR_FLAG_SETTABLE, true);
    mca_base_var_set_value(var_id, &tmp_priority, sizeof(int),
                           MCA_BASE_VAR_SOURCE_SET, NULL);
    comm->c_coll->coll_allreduce =
        ompi_coll_base_allreduce_intra_recursivedoubling;

    /* Create the mpool */
    if (mca_coll_shared_component.shared_mpool == NULL) {
        mca_coll_shared_component.shared_mpool = OBJ_NEW(mca_coll_shared_mpool_t);
    }

    /* Create a dynamic windows */
    ompi_win_create_dynamic((opal_info_t *) (&ompi_mpi_info_null), comm,
                            &shared_module->dynamic_win);

    /* Create a static window  with shared memory allocation */
    mca_coll_shared_setup_static_win(shared_module, comm, COLL_SHARED_STATIC_BLOCK_SIZE);

    shared_module->enabled = true;

    /* Set the functions and the priority back */
    comm->c_coll->coll_allreduce = mca_coll_shared_allreduce_intra;
    mca_base_var_set_value(var_id, &tmp_origin, sizeof(int),
                           MCA_BASE_VAR_SOURCE_SET, NULL);
    return OMPI_SUCCESS;
}

char **mca_coll_shared_attach_buf(mca_coll_shared_module_t *shared_module,
                                struct ompi_communicator_t *comm,
                                char *local_buf,
                                size_t local_buf_size) 
{
    int rank = ompi_comm_rank(comm);
    int size = ompi_comm_size(comm);

    char **attached_bufs = (char **)malloc(sizeof(char *) * size);
    attached_bufs[rank] = local_buf;
    ompi_coll_base_allgather_intra_recursivedoubling(MPI_IN_PLACE, 0,
                                                     MPI_DATATYPE_NULL,
                                                     attached_bufs,
                                                     1, MPI_AINT, comm,
                                                     (mca_coll_base_module_t
                                                      *) shared_module);

    shared_module->dynamic_win->w_osc_module->osc_win_attach(shared_module->dynamic_win, local_buf,
                                     local_buf_size);
    
    return attached_bufs;
}

void mca_coll_shared_detach_buf(mca_coll_shared_module_t *shared_module,
                                struct ompi_communicator_t *comm,
                                char *local_buf,
                                char ***attached_bufs) 
{
    if (local_buf != NULL) {
        shared_module->dynamic_win->w_osc_module->osc_win_detach(shared_module->dynamic_win, local_buf);
    }
    
    free(*attached_bufs);
    *attached_bufs = NULL;
    return;
}

void mca_coll_shared_setup_static_win(mca_coll_shared_module_t *
                                    shared_module,
                                    struct ompi_communicator_t *comm,
                                    size_t data_buf_size)
{
    int i;
    int rank = ompi_comm_rank(comm);
    int size = ompi_comm_size(comm);
    int *ptr;
    ompi_win_allocate_shared(3 * opal_cache_line_size + data_buf_size, sizeof(char),
                             (opal_info_t *) (&ompi_mpi_info_null), comm,
                             &ptr,
                             &shared_module->static_win);
    size_t static_size[size];
    int static_disp[size];
    shared_module->ctrl_buf = (char **) malloc(sizeof(char *) * size);
    shared_module->data_buf = (char **) malloc(sizeof(char *) * size);
    /* Get shared memory address created with the static window */
    for (i = 0; i < size; i++) {
        shared_module->static_win->
            w_osc_module->osc_win_shared_query(shared_module->static_win,
                                               i, &(static_size[i]),
                                               &(static_disp[i]),
                                               &(shared_module->
                                                 ctrl_buf[i]));
        shared_module->data_buf[i] = (char *)(shared_module->ctrl_buf[i]) + 3 * opal_cache_line_size;
    }
    /* Init ctrl_buf with 0s */
    shared_module->static_win->w_osc_module->osc_fence(0,
                                                        shared_module->
                                                        static_win);
    for (i = 0; i < 3; i++) {
        char *ptr = shared_module->ctrl_buf[rank] + i * opal_cache_line_size;
        *((int *)ptr) = 0;
    }
    shared_module->static_win->w_osc_module->osc_fence(0,
                                                        shared_module->
                                                        static_win);
}