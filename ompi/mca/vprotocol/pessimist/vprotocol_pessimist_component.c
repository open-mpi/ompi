/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2022      IBM Corporation. All rights reserved
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mca/mca.h"
#include "vprotocol_pessimist.h"

static int mca_vprotocol_pessimist_component_register(void);
static int mca_vprotocol_pessimist_component_open(void);
static int mca_vprotocol_pessimist_component_close(void);

static mca_vprotocol_base_module_t *mca_vprotocol_pessimist_component_init( int* priority, bool, bool);
static int mca_vprotocol_pessimist_component_finalize(void);

static int _priority;
static int _free_list_num;
static int _free_list_max;
static int _free_list_inc;
static int _sender_based_size;
static int _event_buffer_size;
static char *_mmap_file_name;
static int ompi_vprotocol_pessimist_allow_thread_multiple;

mca_vprotocol_base_component_2_0_0_t mca_vprotocol_pessimist_component =
{
    /* First, the mca_base_component_t struct containing meta
     * information about the component itself */
    .pmlm_version = {
        MCA_VPROTOCOL_BASE_VERSION_2_0_0,

        .mca_component_name = "pessimist",
        .mca_component_major_version = OMPI_MAJOR_VERSION,
        .mca_component_minor_version = OMPI_MINOR_VERSION,
        .mca_component_release_version = OMPI_RELEASE_VERSION,
        .mca_open_component = mca_vprotocol_pessimist_component_open,
        .mca_close_component = mca_vprotocol_pessimist_component_close,
        .mca_register_component_params = mca_vprotocol_pessimist_component_register,
    },
    .pmlm_data = {
        /* component is not checkpointable */
        MCA_BASE_METADATA_PARAM_NONE
    },

    .pmlm_init = mca_vprotocol_pessimist_component_init,
    .pmlm_finalize = mca_vprotocol_pessimist_component_finalize,
};

/** MCA level functions
  */
static int mca_vprotocol_pessimist_component_register(void)
{
    _priority = 30;
    (void) mca_base_component_var_register(&mca_vprotocol_pessimist_component.pmlm_version,
                                           "priority", NULL, MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY, &_priority);\
    _free_list_num = 16;
    (void) mca_base_component_var_register(&mca_vprotocol_pessimist_component.pmlm_version,
                                           "free_list_num", NULL, MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY, &_free_list_num);
    _free_list_max = -1;
    (void) mca_base_component_var_register(&mca_vprotocol_pessimist_component.pmlm_version,
                                           "free_list_max", NULL, MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY, &_free_list_max);
    _free_list_inc = 64;
    (void) mca_base_component_var_register(&mca_vprotocol_pessimist_component.pmlm_version,
                                           "free_list_inc", NULL, MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY, &_free_list_inc);
    _sender_based_size = 256 * 1024 * 1024;
    (void) mca_base_component_var_register(&mca_vprotocol_pessimist_component.pmlm_version,
                                           "sender_based_chunk", NULL, MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY, &_sender_based_size);
    _event_buffer_size = 1024;
    (void) mca_base_component_var_register(&mca_vprotocol_pessimist_component.pmlm_version,
                                           "event_buffer_size", NULL, MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY, &_event_buffer_size);
    _mmap_file_name = "vprotocol_pessimist-senderbased";
    (void) mca_base_component_var_register(&mca_vprotocol_pessimist_component.pmlm_version,
                                           "sender_based_file", NULL, MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY, &_mmap_file_name);
    /* Provide an overwrite allowing the PML V to run even if the OMPI library has been
     * initialized with full support for THREAD_MULTIPLE.
     */
    ompi_vprotocol_pessimist_allow_thread_multiple = 0;
    (void) mca_base_component_var_register(&mca_vprotocol_pessimist_component.pmlm_version,
                                           "allow_thread_multiple", "Allow the PML V to work even when the MPI"
                                           " library is initialized with MPI_THREAD_MULTIPLE support. By "
                                           "default the PML V is disabled in such instances, to protect "
                                           "applications that are not send deterministic.",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_READONLY,
                                           &ompi_vprotocol_pessimist_allow_thread_multiple);


    return OMPI_SUCCESS;
}

static int mca_vprotocol_pessimist_component_open(void)
{
    V_OUTPUT_VERBOSE(500, "vprotocol_pessimist: component_open: read priority %d", _priority);
    return OMPI_SUCCESS;
}

static int mca_vprotocol_pessimist_component_close(void)
{
    V_OUTPUT_VERBOSE(500, "vprotocol_pessimist: component_close");
    return OMPI_SUCCESS;
}

/** VPROTOCOL level functions (same as PML one)
  */
static mca_vprotocol_base_module_t*
mca_vprotocol_pessimist_component_init( int* priority,
                                        bool enable_progress_threads,
                                        bool enable_mpi_threads)
{
    V_OUTPUT_VERBOSE(500, "vprotocol_pessimist: component_init");
    *priority = _priority;

    /* sanity check */
    if(enable_mpi_threads && !ompi_vprotocol_pessimist_allow_thread_multiple) {
        /**
         * Prevent the pessimistic protocol from activating if we are in a potentially multithreaded
         * applications. The reason is that without tracking the thread initiator of messages it is
         * extrmely difficult to provide a global message ordering in threaded-scenarios. Thus, the
         * safe approach is to turn off the logger.
         */
        opal_output(0, "vprotocol_pessimist: component_init: threads are enabled, and not supported by vprotocol pessimist fault tolerant layer, will not load");
        return NULL;
    }

    mca_vprotocol_pessimist.clock = 1;
    mca_vprotocol_pessimist.replay = false;
    OBJ_CONSTRUCT(&mca_vprotocol_pessimist.replay_events, opal_list_t);
    OBJ_CONSTRUCT(&mca_vprotocol_pessimist.pending_events, opal_list_t);
    OBJ_CONSTRUCT(&mca_vprotocol_pessimist.events_pool, opal_free_list_t);
    opal_free_list_init (&mca_vprotocol_pessimist.events_pool,
			 sizeof(mca_vprotocol_pessimist_event_t),
                         opal_cache_line_size,
                         OBJ_CLASS(mca_vprotocol_pessimist_event_t),
                         0,opal_cache_line_size,
                         _free_list_num,
                         _free_list_max,
                         _free_list_inc,
                         NULL, 0, NULL, NULL, NULL);
    mca_vprotocol_pessimist.event_buffer_max_length =
                _event_buffer_size / sizeof(vprotocol_pessimist_mem_event_t);
    mca_vprotocol_pessimist.event_buffer_length = 0;
    mca_vprotocol_pessimist.event_buffer =
                (vprotocol_pessimist_mem_event_t *) malloc(_event_buffer_size);
    mca_vprotocol_pessimist.el_comm = MPI_COMM_NULL;

    return &mca_vprotocol_pessimist.super;
}

static int mca_vprotocol_pessimist_component_finalize(void)
{
    V_OUTPUT_VERBOSE(500, "vprotocol_pessimist_finalize");
    free(mca_vprotocol_pessimist.event_buffer);
    OBJ_DESTRUCT(&mca_vprotocol_pessimist.replay_events);
    OBJ_DESTRUCT(&mca_vprotocol_pessimist.pending_events);
    OBJ_DESTRUCT(&mca_vprotocol_pessimist.events_pool);
    return OMPI_SUCCESS;
}

int mca_vprotocol_pessimist_enable(bool enable) {
    if(enable) {
        int ret;
        if((ret = ompi_vprotocol_pessimist_sender_based_init(_mmap_file_name,
                                                 _sender_based_size)) != OMPI_SUCCESS)
            return ret;
    }
    else {
        ompi_vprotocol_pessimist_sender_based_finalize();
        ompi_vprotocol_pessimist_event_logger_disconnect(mca_vprotocol_pessimist.el_comm);
    }
    return OMPI_SUCCESS;
}
