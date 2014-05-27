/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * Copyright (c) 2014      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/**
 * @file
 *
 */

#include "ompi_config.h"

#include "ompi/constants.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/mpool/base/base.h"
#include "ompi/mca/bcol/bcol.h"
#include "ompi/mca/bcol/base/base.h"
#include "opal/align.h"
#include "bcol_basesmuma.h"

/*
 * Public string showing the coll ompi_sm V2 component version number
 */
const char *mca_bcol_basesmuma_component_version_string =
    "Open MPI bcol - basesmuma collective MCA component version " OMPI_VERSION;

/*
 * Local functions
 */

static int basesmuma_register(void);
static int basesmuma_open(void);
static int basesmuma_close(void);
static int mca_bcol_basesmuma_deregister_ctl_sm(
                                                mca_bcol_basesmuma_component_t *bcol_component);


static inline int mca_bcol_basesmuma_param_register_int(
                                                        const char* param_name, int default_value, int *storage)
{
    *storage = default_value;
    return mca_base_component_var_register(&mca_bcol_basesmuma_component.super.bcol_version, param_name,
                                           NULL, MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY, storage);
}

static inline int mca_bcol_basesmuma_param_register_bool(
                                                         const char* param_name, bool default_value, bool *storage)
{
    *storage = default_value;
    return mca_base_component_var_register(&mca_bcol_basesmuma_component.super.bcol_version, param_name,
                                           NULL, MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY, storage);
}

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

mca_bcol_basesmuma_component_t mca_bcol_basesmuma_component = {

    /* First, fill in the super */

    {
        /* First, the mca_component_t struct containing meta
           information about the component itself */

        {
            MCA_BCOL_BASE_VERSION_2_0_0,

            /* Component name and version */

            "basesmuma",
            OMPI_MAJOR_VERSION,
            OMPI_MINOR_VERSION,
            OMPI_RELEASE_VERSION,

            /* Component open and close functions */

            basesmuma_open,
            basesmuma_close,
            NULL,
            basesmuma_register
        },

        /* Initialization / querying functions */

        mca_bcol_basesmuma_init_query,
        mca_bcol_basesmuma_comm_query,
        NULL,
        NULL,
        false,
        false,
        0, /* (default) priority */
    },
};

/*
 * Register the component
 */
static int basesmuma_register(void)
{
    mca_bcol_basesmuma_component_t *cs = &mca_bcol_basesmuma_component;

    /* set component priority */
    mca_bcol_basesmuma_param_register_int("priority", 90, &cs->super.priority);

    /* Number of memory banks */
    mca_bcol_basesmuma_param_register_int("basesmuma_num_ctl_banks", 2,
                                          &cs->basesmuma_num_mem_banks);

    /* Number of regions per memory bank */
    mca_bcol_basesmuma_param_register_int("basesmuma_num_buffs_per_bank", 16,
                                          &cs->basesmuma_num_regions_per_bank);

    /* number of polling loops to allow pending resources to
     * complete their work
     */
    mca_bcol_basesmuma_param_register_int("n_poll_loops", 4, &cs->n_poll_loops);


    /* Number of groups supported */
    mca_bcol_basesmuma_param_register_int("n_groups_supported", 100,
                                          &cs->n_groups_supported);

    /* order of fanin tree */
    mca_bcol_basesmuma_param_register_int("radix_fanin", 2, &cs->radix_fanin);

    /* order of fanout tree */
    mca_bcol_basesmuma_param_register_int("radix_fanout", 2, &cs->radix_fanout);

    /* order of read tree */
    mca_bcol_basesmuma_param_register_int("radix_read_tree", 3,
                                          &cs->radix_read_tree);

    /* order of reduction fanout tree */
    mca_bcol_basesmuma_param_register_int("order_reduction_tree", 2,
                                          &cs->order_reduction_tree);

    /* k-nomial radix */
    mca_bcol_basesmuma_param_register_int("k_nomial_radix", 3, &cs->k_nomial_radix);

    /* number of polling loops for non-blocking algorithms */
    mca_bcol_basesmuma_param_register_int("num_to_probe", 10, &cs->num_to_probe);

    /* radix of the k-ary scatter tree */
    mca_bcol_basesmuma_param_register_int("scatter_kary_radix", 4,
                                          &cs->scatter_kary_radix);

    /* register parmeters controlling message fragementation */
    mca_bcol_basesmuma_param_register_int("min_frag_size", getpagesize(),
                                          &cs->super.min_frag_size);
    mca_bcol_basesmuma_param_register_int("max_frag_size", FRAG_SIZE_NO_LIMIT,
                                          &cs->super.max_frag_size);

    /* by default use pre-registered shared memory segments */
    /* RLG NOTE: When we have a systematic way to handle single memory
     * copy semantics, we need to update this logic
     */
    mca_bcol_basesmuma_param_register_bool("can_use_user_buffers", false,
                                           &cs->super.can_use_user_buffers);

    mca_bcol_basesmuma_param_register_int("verbose", 0, &cs->verbose);

    return OMPI_SUCCESS;
}

/*
 * Open the component
 */
static int basesmuma_open(void)
{

    /* local variables */
    mca_bcol_basesmuma_component_t *cs = &mca_bcol_basesmuma_component;
    int ret = OMPI_SUCCESS;
    opal_mutex_t *mutex_ptr;
    int dummy;

    /*
     * Make sure that the number of banks is a power of 2
     */
    cs->basesmuma_num_mem_banks=
        roundup_to_power_radix(2,cs->basesmuma_num_mem_banks, &dummy);
    if ( 0 == cs->basesmuma_num_mem_banks ) {
        ret=OMPI_ERROR;
        goto exit_ERROR;
    }

    /*
     * Make sure that the the number of buffers is a power of 2
     */
    cs->basesmuma_num_regions_per_bank=
        roundup_to_power_radix(2,cs->basesmuma_num_regions_per_bank, &dummy);
    if ( 0 == cs->basesmuma_num_regions_per_bank ) {
        ret=OMPI_ERROR;
        goto exit_ERROR;
    }

    /* Portals initialization */
    cs->portals_init = false;
    cs->portals_info = NULL;

    /*
     * initialization
     */
    cs->sm_ctl_structs=NULL;
    OBJ_CONSTRUCT(&(cs->sm_connections_list),opal_list_t);
    OBJ_CONSTRUCT(&(cs->nb_admin_barriers),opal_list_t);
    mutex_ptr= &(cs->nb_admin_barriers_mutex);
    OBJ_CONSTRUCT(mutex_ptr, opal_mutex_t);

    /* Control structures object construct
     */
    OBJ_CONSTRUCT(&(cs->ctl_structures), opal_list_t);

    /* shared memory has not been registered yet */
    cs->mpool_inited = false;

    /* initialize base file names */
    cs->clt_base_fname="sm_ctl_mem_";
    cs->payload_base_fname="sm_payload_mem_";

    /* initialize the size of the shared memory scartch region */
    cs->my_scratch_shared_memory_size=getpagesize();
    cs->my_scratch_shared_memory=NULL;
    cs->scratch_offset_from_base_ctl_file=0;

    /*
     * register the progess function
     */
    ret=opal_progress_register(bcol_basesmuma_progress);
    if (MPI_SUCCESS != ret) {
        opal_output(ompi_bcol_base_framework.framework_output, "failed to register the progress function");
    }

    return ret;

 exit_ERROR:
    return ret;
}

/*
 * release the control structure backing file
 */
static int mca_bcol_basesmuma_deregister_ctl_sm(mca_bcol_basesmuma_component_t *bcol_component)
{
    if (NULL != bcol_component->sm_ctl_structs) {
        OBJ_RELEASE(bcol_component->sm_ctl_structs);
    }

    return OMPI_SUCCESS;
}


/*
 * Close the component
 */
static int basesmuma_close(void)
{
    int ret;
    bcol_basesmuma_registration_data_t *net_ctx;
    bcol_base_network_context_t *net_reg;
    mca_bcol_basesmuma_component_t *cs = &mca_bcol_basesmuma_component;

    /* gvm Leak FIX */
    while(!opal_list_is_empty(&(cs->ctl_structures))) {
        opal_list_item_t *item;
        item = opal_list_remove_first(&(cs->ctl_structures));
        OBJ_DESTRUCT(item);
    }
    OBJ_DESTRUCT(&(cs->ctl_structures));

    /* deregister the progress function */
    ret=opal_progress_unregister(bcol_basesmuma_progress);
    if (MPI_SUCCESS != ret) {
        opal_output(ompi_bcol_base_framework.framework_output, "failed to unregister the progress function");
    }

    /* remove the control structure backing file */
    ret=mca_bcol_basesmuma_deregister_ctl_sm(&mca_bcol_basesmuma_component);
    if (MPI_SUCCESS != ret) {
        opal_output(ompi_bcol_base_framework.framework_output, "failed to remove control structure backing file");
    }

    /* remove the network contexts - only one network context defined for
     * this component.
     */
    /* file_name returne by asprintf, so need to free the resource */
    if(mca_bcol_basesmuma_component.super.network_contexts ) {
        net_reg=(bcol_base_network_context_t *)
            mca_bcol_basesmuma_component.super.network_contexts[0];
        if(net_reg) {
            net_ctx=(bcol_basesmuma_registration_data_t *)net_reg->context_data;
            if( net_ctx) {
                if(net_ctx->file_name) {
                    free(net_ctx->file_name);
                }
                free(net_ctx);
            }
            free(net_reg);
        }
        free(mca_bcol_basesmuma_component.super.network_contexts);
        mca_bcol_basesmuma_component.super.network_contexts=NULL;
    }

    /* normal return */
    return OMPI_SUCCESS;
}

/* query to see if the component is available for use, and can
 * satisfy the thread and progress requirements
 */
int mca_bcol_basesmuma_init_query(bool enable_progress_threads,
                                  bool enable_mpi_threads)
{
    /* done */
    return OMPI_SUCCESS;
}

/* This routine is used to allocate shared memory for the the shared
 * memory control regions.
 */
int mca_bcol_basesmuma_allocate_sm_ctl_memory(mca_bcol_basesmuma_component_t *cs)
{
    /* local variables */
    int name_length, ret;
    size_t ctl_length;
    char *name;
    size_t page_size = getpagesize ();

    /* set the file name */
    name_length=asprintf(&name,
                         "%s"OPAL_PATH_SEP"%s""%0d",
                         ompi_process_info.job_session_dir,
                         cs->clt_base_fname,
                         (int)getpid());
    if( 0 > name_length ) {
        return OMPI_ERROR;
    }
    /* make sure name is not too long */
    if ( OPAL_PATH_MAX < (name_length-1) ) {
        return OMPI_ERROR;
    }

    /* compute segment length */

    ctl_length=(cs->basesmuma_num_mem_banks*
                cs->basesmuma_num_regions_per_bank+cs->basesmuma_num_mem_banks)
        *sizeof(mca_bcol_basesmuma_ctl_struct_t)*cs->n_groups_supported;
    /* need two banks of memory per group - for algorithms that have
     * user payload, and those that don't
     */
    ctl_length*=2;

    /* add space for internal library management purposes */
    ctl_length+=cs->my_scratch_shared_memory_size;

    /* round up to multiple of page size */
    ctl_length = OPAL_ALIGN(ctl_length, page_size, size_t);

    /* allocate the shared file */
    cs->sm_ctl_structs=bcol_basesmuma_smcm_mem_reg (NULL, ctl_length, getpagesize(), name);
    if( !cs->sm_ctl_structs) {
        opal_output (ompi_bcol_base_framework.framework_output,
                     "In mca_bcol_basesmuma_allocate_sm_ctl_memory failed to allocathe backing file %s\n", name);
        ret=OMPI_ERR_OUT_OF_RESOURCE;
        goto Error;
    }

    /* free the memory allocated by asprintf for the file name -
     * in mca_base_smcm_mem_reg this name is copied into a new
     * memory location */
    free(name);

    /* successful return */
    return OMPI_SUCCESS;

 Error:
    if(name) {
        free(name);
    }
    return ret;
}
