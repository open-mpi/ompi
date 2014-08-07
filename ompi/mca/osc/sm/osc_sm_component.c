/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2012      Sandia National Laboratories.  All rights reserved.
 * Copyright (c) 2014      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mca/osc/osc.h"
#include "ompi/mca/osc/base/base.h"
#include "ompi/mca/osc/base/osc_base_obj_convert.h"
#include "ompi/class/ompi_free_list.h"
#include "opal/util/sys_limits.h"

#include "osc_sm.h"

static int component_open(void);
static int component_init(bool enable_progress_threads, bool enable_mpi_threads);
static int component_finalize(void);
static int component_query(struct ompi_win_t *win, void **base, size_t size, int disp_unit,
                           struct ompi_communicator_t *comm, struct ompi_info_t *info,
                           int flavor);
static int component_select(struct ompi_win_t *win, void **base, size_t size, int disp_unit,
                            struct ompi_communicator_t *comm, struct ompi_info_t *info,
                            int flavor, int *model);


ompi_osc_sm_component_t mca_osc_sm_component = {
    { /* ompi_osc_base_component_t */
        { /* ompi_base_component_t */
            OMPI_OSC_BASE_VERSION_3_0_0,
            "sm",
            OMPI_MAJOR_VERSION,  /* MCA component major version */
            OMPI_MINOR_VERSION,  /* MCA component minor version */
            OMPI_RELEASE_VERSION,  /* MCA component release version */
            component_open,
            NULL
        },
        { /* mca_base_component_data */
            /* The component is not checkpoint ready */
            MCA_BASE_METADATA_PARAM_NONE
        },
        component_init,
        component_query,
        component_select,
        component_finalize
    }
};


ompi_osc_sm_module_t ompi_osc_sm_module_template = {
    {
        ompi_osc_sm_shared_query,

        ompi_osc_sm_attach,
        ompi_osc_sm_detach,
        ompi_osc_sm_free,

        ompi_osc_sm_put,
        ompi_osc_sm_get,
        ompi_osc_sm_accumulate,
        ompi_osc_sm_compare_and_swap,
        ompi_osc_sm_fetch_and_op,
        ompi_osc_sm_get_accumulate,

        ompi_osc_sm_rput,
        ompi_osc_sm_rget,
        ompi_osc_sm_raccumulate,
        ompi_osc_sm_rget_accumulate,

        ompi_osc_sm_fence,

        ompi_osc_sm_start,
        ompi_osc_sm_complete,
        ompi_osc_sm_post,
        ompi_osc_sm_wait,
        ompi_osc_sm_test,

        ompi_osc_sm_lock,
        ompi_osc_sm_unlock,
        ompi_osc_sm_lock_all,
        ompi_osc_sm_unlock_all,

        ompi_osc_sm_sync,
        ompi_osc_sm_flush,
        ompi_osc_sm_flush_all,
        ompi_osc_sm_flush_local,
        ompi_osc_sm_flush_local_all,

        ompi_osc_sm_set_info,
        ompi_osc_sm_get_info
    }
};


static int
component_open(void)
{
    return OMPI_SUCCESS;
}


static int
component_init(bool enable_progress_threads, bool enable_mpi_threads)
{
    return OMPI_SUCCESS;
}


static int 
component_finalize(void)
{
    /* clean up requests free list */

    return OMPI_SUCCESS;
}


static int
check_win_ok(ompi_communicator_t *comm, int flavor)
{
    int i;

    if (! (MPI_WIN_FLAVOR_SHARED == flavor
           || MPI_WIN_FLAVOR_ALLOCATE == flavor) ) {
        return OMPI_ERR_NOT_SUPPORTED;
    }

    for (i = 0 ; i < ompi_comm_size(comm) ; ++i) {
        if (!OPAL_PROC_ON_LOCAL_NODE(ompi_comm_peer_lookup(comm, i)->proc_flags)) {
            return OMPI_ERR_RMA_SHARED;
        }
    }

    return OMPI_SUCCESS;
}


static int
component_query(struct ompi_win_t *win, void **base, size_t size, int disp_unit,
                struct ompi_communicator_t *comm, struct ompi_info_t *info,
                int flavor)
{
    int ret;
    if (OMPI_SUCCESS != (ret = check_win_ok(comm, flavor))) {
        if (OMPI_ERR_NOT_SUPPORTED == ret) {
            return -1;
        }
        return ret;
    }

    return 100;
}


static int
component_select(struct ompi_win_t *win, void **base, size_t size, int disp_unit,
                 struct ompi_communicator_t *comm, struct ompi_info_t *info,
                 int flavor, int *model)
{
    ompi_osc_sm_module_t *module = NULL;
    int ret = OMPI_ERROR;

    if (OMPI_SUCCESS != (ret = check_win_ok(comm, flavor))) {
        return ret;
    }

    /* create module structure */
    module = (ompi_osc_sm_module_t*)
        calloc(1, sizeof(ompi_osc_sm_module_t));
    if (NULL == module) return OMPI_ERR_TEMP_OUT_OF_RESOURCE;

    /* fill in the function pointer part */
    memcpy(module, &ompi_osc_sm_module_template, 
           sizeof(ompi_osc_base_module_t));

    /* need our communicator for collectives in next phase */
    ret = ompi_comm_dup(comm, &module->comm);
    if (OMPI_SUCCESS != ret) goto error;

    module->flavor = flavor;

    /* create the segment */
    if (1 == ompi_comm_size(comm)) {
        module->segment_base = NULL;
        module->sizes = malloc(sizeof(size_t));
        if (NULL == module->sizes) return OMPI_ERR_TEMP_OUT_OF_RESOURCE;
        module->bases = malloc(sizeof(void*));
        if (NULL == module->bases) return OMPI_ERR_TEMP_OUT_OF_RESOURCE;

        module->sizes[0] = size;
        module->bases[0] = malloc(size);
        if (NULL == module->bases[0]) return OMPI_ERR_TEMP_OUT_OF_RESOURCE;

        module->global_state = malloc(sizeof(ompi_osc_sm_global_state_t));
        if (NULL == module->global_state) return OMPI_ERR_TEMP_OUT_OF_RESOURCE;
        module->node_states = malloc(sizeof(ompi_osc_sm_node_state_t));
        if (NULL == module->node_states) return OMPI_ERR_TEMP_OUT_OF_RESOURCE;

    } else {
        unsigned long total, *rbuf;
        char *data_file;
        int i, flag;
        size_t pagesize;
	size_t state_size;

        OPAL_OUTPUT_VERBOSE((1, ompi_osc_base_framework.framework_output,
                             "allocating shared memory region of size %ld\n", (long) size));

        /* get the pagesize */
        pagesize = opal_getpagesize();

        rbuf = malloc(sizeof(unsigned long) * ompi_comm_size(module->comm));
        if (NULL == rbuf) return OMPI_ERR_TEMP_OUT_OF_RESOURCE;

        module->noncontig = false;
        if (OMPI_SUCCESS != ompi_info_get_bool(info, "alloc_shared_noncontig",
                                               &module->noncontig, &flag)) {
            goto error;
        }

        if (module->noncontig) {
            total = ((size - 1) / pagesize + 1) * pagesize;
        } else {
            total = size;
        }
        ret = module->comm->c_coll.coll_allgather(&total, 1, MPI_UNSIGNED_LONG,
                                                  rbuf, 1, MPI_UNSIGNED_LONG,
                                                  module->comm,
                                                  module->comm->c_coll.coll_allgather_module);
        if (OMPI_SUCCESS != ret) return ret;

        total = 0;
        for (i = 0 ; i < ompi_comm_size(module->comm) ; ++i) {
            total += rbuf[i];
        }

        if (asprintf(&data_file, "%s"OPAL_PATH_SEP"shared_window_%d.%s",
                     ompi_process_info.job_session_dir,
                     ompi_comm_get_cid(module->comm),
                     ompi_process_info.nodename) < 0) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

	/* user opal/shmem directly to create a shared memory segment */
	state_size = sizeof(ompi_osc_sm_global_state_t) + sizeof(ompi_osc_sm_node_state_t) * ompi_comm_size(module->comm);
	if (0 == ompi_comm_rank (module->comm)) {
	    ret = opal_shmem_segment_create (&module->seg_ds, data_file, total + pagesize + state_size);
	    if (OPAL_SUCCESS != ret) {
		goto error;
	    }
	}

	ret = module->comm->c_coll.coll_bcast (&module->seg_ds, sizeof (module->seg_ds), MPI_BYTE, 0,
					       module->comm, module->comm->c_coll.coll_bcast_module);
	if (OMPI_SUCCESS != ret) {
	    goto error;
	}

	module->segment_base = opal_shmem_segment_attach (&module->seg_ds);
	if (NULL == module->segment_base) {
	    goto error;
	}

        module->sizes = malloc(sizeof(size_t) * ompi_comm_size(module->comm));
        if (NULL == module->sizes) return OMPI_ERR_TEMP_OUT_OF_RESOURCE;
        module->bases = malloc(sizeof(void*) * ompi_comm_size(module->comm));
        if (NULL == module->bases) return OMPI_ERR_TEMP_OUT_OF_RESOURCE;

        module->global_state = (ompi_osc_sm_global_state_t *) (module->segment_base);
        module->node_states = (ompi_osc_sm_node_state_t *) (module->global_state + 1);

        for (i = 0, total = state_size ; i < ompi_comm_size(module->comm) ; ++i) {
            module->sizes[i] = rbuf[i];
            if (module->sizes[i]) {
                module->bases[i] = ((char *) module->segment_base) + total;
                total += rbuf[i];
            } else {
                module->bases[i] = NULL;
            }
        }

        free(rbuf);
    }

    /* initialize my state shared */
    module->my_node_state = &module->node_states[ompi_comm_rank(module->comm)];
    memset (module->my_node_state, 0, sizeof(*module->my_node_state));

    *base = module->bases[ompi_comm_rank(module->comm)];

    opal_atomic_init(&module->my_node_state->accumulate_lock, OPAL_ATOMIC_UNLOCKED);

    /* share everyone's displacement units. */
    module->disp_units = malloc(sizeof(int) * ompi_comm_size(module->comm));
    ret = module->comm->c_coll.coll_allgather(&disp_unit, 1, MPI_INT,
                                              module->disp_units, 1, MPI_INT,
                                              module->comm,
                                              module->comm->c_coll.coll_allgather_module);
    if (OMPI_SUCCESS != ret) goto error;

    module->start_group = NULL;
    module->post_group = NULL;

    /* initialize synchronization code */
    module->my_sense = 1;

    module->outstanding_locks = calloc(ompi_comm_size(module->comm), sizeof(enum ompi_osc_sm_locktype_t));
    if (NULL == module->outstanding_locks) {
        ret = OMPI_ERR_TEMP_OUT_OF_RESOURCE;
        goto error;
    }

    if (0 == ompi_comm_rank(module->comm)) {
#if HAVE_PTHREAD_CONDATTR_SETPSHARED && HAVE_PTHREAD_MUTEXATTR_SETPSHARED
        pthread_mutexattr_t mattr;
        pthread_condattr_t cattr;
        bool blocking_fence;
        int flag;

        if (OMPI_SUCCESS != ompi_info_get_bool(info, "blocking_fence",
                                               &blocking_fence, &flag)) {
            goto error;
        }

        if (blocking_fence) {
            ret = pthread_mutexattr_init(&mattr);
            ret = pthread_mutexattr_setpshared(&mattr, PTHREAD_PROCESS_SHARED);
            if (ret != 0) {
                module->global_state->use_barrier_for_fence = 1;
            } else {
                ret = pthread_mutex_init(&module->global_state->mtx, &mattr);
                if (ret != 0) {
                    module->global_state->use_barrier_for_fence = 1;
                } else {
                    pthread_condattr_init(&cattr);
                    pthread_condattr_setpshared(&cattr, PTHREAD_PROCESS_SHARED);
                    ret = pthread_cond_init(&module->global_state->cond, &cattr);
                    if (ret != 0) return OMPI_ERROR;
                    pthread_condattr_destroy(&cattr);
                }
            }
            module->global_state->use_barrier_for_fence = 0;
            module->global_state->sense = module->my_sense;
            module->global_state->count = ompi_comm_size(module->comm);
            pthread_mutexattr_destroy(&mattr);
        } else {
            module->global_state->use_barrier_for_fence = 1;
        }
#else
        module->global_state->use_barrier_for_fence = 1;
#endif
    }

    ret = module->comm->c_coll.coll_barrier(module->comm,
                                            module->comm->c_coll.coll_barrier_module);
    if (OMPI_SUCCESS != ret) goto error;

    *model = MPI_WIN_UNIFIED;

    win->w_osc_module = &module->super;

    return OMPI_SUCCESS;

 error:
    if (NULL != module->comm) ompi_comm_free(&module->comm);
    if (NULL != module) free(module);

    return ret;
}


int
ompi_osc_sm_shared_query(struct ompi_win_t *win, int rank, size_t *size, int *disp_unit, void *baseptr)
{
    ompi_osc_sm_module_t *module =
        (ompi_osc_sm_module_t*) win->w_osc_module;

    if (module->flavor != MPI_WIN_FLAVOR_SHARED) {
        return MPI_ERR_WIN;
    }

    if (MPI_PROC_NULL != rank) {
        *size = module->sizes[rank];
        *((void**) baseptr) = module->bases[rank];
        *disp_unit = module->disp_units[rank];
    } else {
        int i = 0;

        *size = 0;
        *((void**) baseptr) = NULL;
        *disp_unit = 0;
        for (i = 0 ; i < ompi_comm_size(module->comm) ; ++i) {
            if (0 != module->sizes[i]) {
                *size = module->sizes[i];
                *((void**) baseptr) = module->bases[i];
                *disp_unit = module->disp_units[i];
                break;
            }
        }
    }

    return OMPI_SUCCESS;
}


int
ompi_osc_sm_attach(struct ompi_win_t *win, void *base, size_t len)
{
    ompi_osc_sm_module_t *module =
        (ompi_osc_sm_module_t*) win->w_osc_module;

    if (module->flavor != MPI_WIN_FLAVOR_DYNAMIC) {
        return MPI_ERR_RMA_ATTACH;
    }
    return OMPI_SUCCESS;
}


int
ompi_osc_sm_detach(struct ompi_win_t *win, void *base)
{
    ompi_osc_sm_module_t *module =
        (ompi_osc_sm_module_t*) win->w_osc_module;

    if (module->flavor != MPI_WIN_FLAVOR_DYNAMIC) {
        return MPI_ERR_RMA_ATTACH;
    }
    return OMPI_SUCCESS;
}


int
ompi_osc_sm_free(struct ompi_win_t *win)
{
    ompi_osc_sm_module_t *module =
        (ompi_osc_sm_module_t*) win->w_osc_module;

    /* free memory */
    if (NULL != module->segment_base) {
        /* synchronize */
        module->comm->c_coll.coll_barrier(module->comm,
                                          module->comm->c_coll.coll_barrier_module);

        if (0 == ompi_comm_rank (module->comm)) {
            opal_shmem_unlink (&module->seg_ds);
        }

	opal_shmem_segment_detach (&module->seg_ds);
    } else {
        free(module->node_states);
        free(module->global_state);
        free(module->bases[0]);
        free(module->bases);
        free(module->sizes);
    }

    /* cleanup */
    ompi_comm_free(&module->comm);
    free(module);

    return OMPI_SUCCESS;
}


int
ompi_osc_sm_set_info(struct ompi_win_t *win, struct ompi_info_t *info)
{
    ompi_osc_sm_module_t *module =
        (ompi_osc_sm_module_t*) win->w_osc_module;

    /* enforce collectiveness... */
    return module->comm->c_coll.coll_barrier(module->comm,
                                             module->comm->c_coll.coll_barrier_module);
}


int
ompi_osc_sm_get_info(struct ompi_win_t *win, struct ompi_info_t **info_used)
{
    ompi_osc_sm_module_t *module =
        (ompi_osc_sm_module_t*) win->w_osc_module;

    ompi_info_t *info = OBJ_NEW(ompi_info_t);
    if (NULL == info) return OMPI_ERR_TEMP_OUT_OF_RESOURCE;

    if (module->flavor == MPI_WIN_FLAVOR_SHARED) {
        ompi_info_set(info, "blocking_fence", 
                      (1 == module->global_state->use_barrier_for_fence) ? "true" : "false");
        ompi_info_set(info, "alloc_shared_noncontig",
                      (module->noncontig) ? "true" : "false");
    }

    *info_used = info;

    return OMPI_SUCCESS;
}
