/* -*- Mode: C; c-acoll-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2024 Advanced Micro Devices, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "ompi_config.h"

#include "mpi.h"
#include "ompi/mca/coll/coll.h"
#include "coll_acoll.h"

/*
 * Public string showing the coll ompi_acoll component version number
 */
const char *mca_coll_acoll_component_version_string
    = "Open MPI acoll collective MCA component version " OMPI_VERSION;

/*
 * Global variables
 */
int mca_coll_acoll_priority = 0;
int mca_coll_acoll_max_comms = 10;
int mca_coll_acoll_sg_size = 8;
int mca_coll_acoll_sg_scale = 1;
int mca_coll_acoll_node_size = 128;
int mca_coll_acoll_use_dynamic_rules = 0;
int mca_coll_acoll_mnode_enable = 1;
int mca_coll_acoll_bcast_lin0 = 0;
int mca_coll_acoll_bcast_lin1 = 0;
int mca_coll_acoll_bcast_lin2 = 0;
int mca_coll_acoll_bcast_nonsg = 0;
int mca_coll_acoll_allgather_lin = 0;
int mca_coll_acoll_allgather_ring_1 = 0;
int mca_coll_acoll_reserve_memory_for_algo = 0;
uint64_t mca_coll_acoll_reserve_memory_size_for_algo = 128 * 32768; // 4 MB
uint64_t mca_coll_acoll_xpmem_buffer_size = 128 * 32768;

/* By default utilize xpmem based algorithms applicable when built with xpmem. */
int mca_coll_acoll_without_xpmem = 0;
int mca_coll_acoll_xpmem_use_sr_buf = 1;

/*
 * Local function
 */
static int acoll_register(void);

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

const mca_coll_base_component_3_0_0_t mca_coll_acoll_component = {

    /* First, the mca_component_t struct containing meta information
     * about the component itself */

    .collm_version = {
        MCA_COLL_BASE_VERSION_3_0_0,

        /* Component name and version */
        .mca_component_name = "acoll",
        MCA_BASE_MAKE_VERSION(component, OMPI_MAJOR_VERSION, OMPI_MINOR_VERSION,
                              OMPI_RELEASE_VERSION),

        /* Component open and close functions */
        .mca_register_component_params = acoll_register,
    },
    .collm_data = {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },

    /* Initialization / querying functions */

    .collm_init_query = mca_coll_acoll_init_query,
    .collm_comm_query = mca_coll_acoll_comm_query,
};

static int acoll_register(void)
{
    /* Use a low priority, but allow other components to be lower */
    mca_coll_acoll_priority = 0;
    (void) mca_base_component_var_register(&mca_coll_acoll_component.collm_version, "priority",
                                           "Priority of the acoll coll component",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0, OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY, &mca_coll_acoll_priority);

    /* Defaults on topology */
    (void)
        mca_base_component_var_register(&mca_coll_acoll_component.collm_version, "max_comms",
                                        "Maximum no. of communicators using subgroup based algorithms",
                                        MCA_BASE_VAR_TYPE_INT, NULL, 0, 0, OPAL_INFO_LVL_9,
                                        MCA_BASE_VAR_SCOPE_READONLY, &mca_coll_acoll_max_comms);
    (void)
        mca_base_component_var_register(&mca_coll_acoll_component.collm_version, "sg_size",
                                        "Size of subgroup to be used for subgroup based algorithms",
                                        MCA_BASE_VAR_TYPE_INT, NULL, 0, 0, OPAL_INFO_LVL_9,
                                        MCA_BASE_VAR_SCOPE_READONLY, &mca_coll_acoll_sg_size);

    (void) mca_base_component_var_register(
        &mca_coll_acoll_component.collm_version, "sg_scale",
        "Scale factor for effective subgroup size for subgroup based algorithms",
        MCA_BASE_VAR_TYPE_INT, NULL, 0, 0, OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_READONLY,
        &mca_coll_acoll_sg_scale);
    (void) mca_base_component_var_register(&mca_coll_acoll_component.collm_version, "node_size",
                                           "Size of node for multinode cases",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0, OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY, &mca_coll_acoll_node_size);
    (void)
        mca_base_component_var_register(&mca_coll_acoll_component.collm_version,
                                        "use_dynamic_rules",
                                        "Use dynamic selection of algorithms for multinode cases",
                                        MCA_BASE_VAR_TYPE_INT, NULL, 0, 0, OPAL_INFO_LVL_9,
                                        MCA_BASE_VAR_SCOPE_READONLY,
                                        &mca_coll_acoll_use_dynamic_rules);
    (void) mca_base_component_var_register(&mca_coll_acoll_component.collm_version, "mnode_enable",
                                           "Enable separate algorithm for multinode cases",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0, OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_coll_acoll_mnode_enable);
    (void) mca_base_component_var_register(&mca_coll_acoll_component.collm_version, "bcast_lin0",
                                           "Use lin/log for stage 0 of multinode algorithm",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0, OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY, &mca_coll_acoll_bcast_lin0);
    (void) mca_base_component_var_register(&mca_coll_acoll_component.collm_version, "bcast_lin1",
                                           "Use lin/log for stage 1 of multinode algorithm",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0, OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY, &mca_coll_acoll_bcast_lin1);
    (void) mca_base_component_var_register(&mca_coll_acoll_component.collm_version, "bcast_lin2",
                                           "Use lin/log for stage 2 of multinode algorithm",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0, OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY, &mca_coll_acoll_bcast_lin2);
    (void) mca_base_component_var_register(
        &mca_coll_acoll_component.collm_version, "bcast_nonsg",
        "Flag to turn on/off subgroup based algorithms for multinode", MCA_BASE_VAR_TYPE_INT, NULL,
        0, 0, OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_READONLY, &mca_coll_acoll_bcast_nonsg);
    (void) mca_base_component_var_register(&mca_coll_acoll_component.collm_version, "allgather_lin",
                                           "Flag to indicate use of linear allgather for multinode",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0, OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_coll_acoll_allgather_lin);
    (void)
        mca_base_component_var_register(&mca_coll_acoll_component.collm_version, "allgather_ring_1",
                                        "Flag to indicate use of ring/rd allgather for multinode",
                                        MCA_BASE_VAR_TYPE_INT, NULL, 0, 0, OPAL_INFO_LVL_9,
                                        MCA_BASE_VAR_SCOPE_READONLY,
                                        &mca_coll_acoll_allgather_ring_1);
    (void) mca_base_component_var_register(
        &mca_coll_acoll_component.collm_version, "reserve_memory_for_algo",
        "Flag to inform the acoll component to reserve/pre-allocate memory"
        " for use inside collective algorithms.",
        MCA_BASE_VAR_TYPE_INT, NULL, 0, 0, OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_READONLY,
        &mca_coll_acoll_reserve_memory_for_algo);
    (void) mca_base_component_var_register(
        &mca_coll_acoll_component.collm_version, "reserve_memory_size_for_algo",
        "Size of memory to be allocated by acoll component to use as reserve"
        "memory inside collective algorithms.",
        MCA_BASE_VAR_TYPE_UINT64_T, NULL, 0, 0, OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_READONLY,
        &mca_coll_acoll_reserve_memory_size_for_algo);
    (void) mca_base_component_var_register(
        &mca_coll_acoll_component.collm_version, "without_xpmem",
        "By default, xpmem-based algorithms are used when applicable. "
        "When this flag is set to 1, xpmem-based algorithms are disabled.",
        MCA_BASE_VAR_TYPE_INT, NULL, 0, 0, OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_READONLY,
        &mca_coll_acoll_without_xpmem);
    (void) mca_base_component_var_register(
        &mca_coll_acoll_component.collm_version, "xpmem_buffer_size",
        "Maximum size of memory that can be used for temporary buffers for "
        "xpmem-based algorithms. By default these buffers are not created or "
        "used unless xpmem_use_sr_buf is set to 0.",
        MCA_BASE_VAR_TYPE_UINT64_T, NULL, 0, 0, OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_READONLY,
        &mca_coll_acoll_xpmem_buffer_size);
    (void) mca_base_component_var_register(
        &mca_coll_acoll_component.collm_version, "xpmem_use_sr_buf",
        "Uses application provided send/recv buffers during xpmem registration "
        "when set to 1 instead of temporary buffers. The send/recv buffers are "
        "assumed to persist for the duration of the application.",
        MCA_BASE_VAR_TYPE_INT, NULL, 0, 0, OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_READONLY,
        &mca_coll_acoll_xpmem_use_sr_buf);

    return OMPI_SUCCESS;
}

/*
 * Module constructor
 */
static void mca_coll_acoll_module_construct(mca_coll_acoll_module_t *module)
{

    /* Set number of subcomms to 0 */
    module->num_subc = 0;
    module->subc = NULL;

    /* Reserve memory init. Lazy allocation of memory when needed. */
    (module->reserve_mem_s).reserve_mem = NULL;
    (module->reserve_mem_s).reserve_mem_size = 0;
    (module->reserve_mem_s).reserve_mem_allocate = false;
    (module->reserve_mem_s).reserve_mem_in_use = false;
    if ((0 != mca_coll_acoll_reserve_memory_for_algo)
        && (0 < mca_coll_acoll_reserve_memory_size_for_algo)
        && (false == ompi_mpi_thread_multiple)) {
        (module->reserve_mem_s).reserve_mem_allocate = true;
        (module->reserve_mem_s).reserve_mem_size = mca_coll_acoll_reserve_memory_size_for_algo;
    }
}

/*
 * Module destructor
 */
static void mca_coll_acoll_module_destruct(mca_coll_acoll_module_t *module)
{
    for (int i = 0; i < module->num_subc; i++) {
        coll_acoll_subcomms_t *subc = module->subc[i];
        if (subc->initialized_data) {
            if (subc->initialized_shm_data) {
                if (subc->orig_comm != NULL) {
                    opal_shmem_unlink(
                        &((subc->data)->allshmseg_id[ompi_comm_rank(subc->orig_comm)]));
                    opal_shmem_segment_detach(
                        &((subc->data)->allshmseg_id[ompi_comm_rank(subc->orig_comm)]));
                }
            }
            coll_acoll_data_t *data = subc->data;
            if (NULL != data) {
#ifdef HAVE_XPMEM_H
                for (int j = 0; j < data->comm_size; j++) {
                    xpmem_release(data->all_apid[j]);
                    xpmem_remove(data->allseg_id[j]);
                    mca_rcache_base_module_destroy(data->rcache[j]);
                }

                free(data->allseg_id);
                data->allseg_id = NULL;
                free(data->all_apid);
                data->all_apid = NULL;
                free(data->allshm_sbuf);
                data->allshm_sbuf = NULL;
                free(data->allshm_rbuf);
                data->allshm_rbuf = NULL;
                free(data->xpmem_saddr);
                data->xpmem_saddr = NULL;
                free(data->xpmem_raddr);
                data->xpmem_raddr = NULL;
                free(data->scratch);
                data->scratch = NULL;
                free(data->rcache);
                data->rcache = NULL;
#endif
                free(data->allshmseg_id);
                data->allshmseg_id = NULL;
                free(data->allshmmmap_sbuf);
                data->allshmmmap_sbuf = NULL;
                free(data->l1_gp);
                data->l1_gp = NULL;
                free(data->l2_gp);
                data->l2_gp = NULL;
                free(data);
                data = NULL;
            }
        }

        if (subc->local_comm != NULL) {
            ompi_comm_free(&(subc->local_comm));
            subc->local_comm = NULL;
        }

        if (subc->local_r_comm != NULL) {
            ompi_comm_free(&(subc->local_r_comm));
            subc->local_r_comm = NULL;
        }

        if (subc->leader_comm != NULL) {
            ompi_comm_free(&(subc->leader_comm));
            subc->leader_comm = NULL;
        }

        if (subc->subgrp_comm != NULL) {
            ompi_comm_free(&(subc->subgrp_comm));
            subc->subgrp_comm = NULL;
        }
        if (subc->socket_comm != NULL) {
            ompi_comm_free(&(subc->socket_comm));
            subc->socket_comm = NULL;
        }

        if (subc->socket_ldr_comm != NULL) {
            ompi_comm_free(&(subc->socket_ldr_comm));
            subc->socket_ldr_comm = NULL;
        }
        for (int k = 0; k < MCA_COLL_ACOLL_NUM_BASE_LYRS; k++) {
            for (int j = 0; j < MCA_COLL_ACOLL_NUM_LAYERS; j++) {
                if (subc->base_comm[k][j] != NULL) {
                    ompi_comm_free(&(subc->base_comm[k][j]));
                    subc->base_comm[k][j] = NULL;
                }
            }
        }
        subc->initialized = 0;
        free(subc);
        module->subc[i] = NULL;
    }

    module->num_subc = 0;
    free(module->subc);
    module->subc = NULL;

    if ((true == (module->reserve_mem_s).reserve_mem_allocate)
        && (NULL != (module->reserve_mem_s).reserve_mem)) {
        free((module->reserve_mem_s).reserve_mem);
    }
}

OBJ_CLASS_INSTANCE(mca_coll_acoll_module_t, mca_coll_base_module_t, mca_coll_acoll_module_construct,
                   mca_coll_acoll_module_destruct);
