/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * Copyright (c) 2013      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2013      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */



#include "ompi_config.h"
#include <stdio.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNIST_H */
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/util/argv.h"

#include "ompi/mca/bcol/bcol.h"
#include "ompi/mca/bcol/base/base.h"
#include "ompi/include/ompi/constants.h"
#include "ompi/mca/mpool/mpool.h"
#include "opal/class/opal_list.h"
/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "ompi/mca/bcol/base/static-components.h"

static int mca_bcol_base_open(mca_base_open_flag_t flags);
static int mca_bcol_base_close (void);
static int mca_bcol_base_register(mca_base_register_flag_t flags);

/*
**  * Global variables
**   */
MCA_BASE_FRAMEWORK_DECLARE(ompi, bcol, NULL, mca_bcol_base_register, mca_bcol_base_open, mca_bcol_base_close,
                           mca_bcol_base_static_components, 0);

OMPI_DECLSPEC opal_list_t mca_bcol_base_components_in_use;
OMPI_DECLSPEC char *ompi_bcol_bcols_string;
OMPI_DECLSPEC int bcol_mpool_compatibility[BCOL_SIZE][BCOL_SIZE];
OMPI_DECLSPEC int bcol_mpool_index[BCOL_SIZE][BCOL_SIZE];

static void bcol_base_module_constructor(mca_bcol_base_module_t *module)
{
    int fnc;

    module->bcol_component = NULL;
    module->network_context = NULL;
    module->context_index = -1;
    module->supported_mode = 0;
    module->init_module = NULL;
    module->sbgp_partner_module = NULL;
    module->squence_number_offset = 0;
    module->n_poll_loops = 0;

    for (fnc = 0; fnc < BCOL_NUM_OF_FUNCTIONS; fnc++) {
        module->bcol_function_table[fnc] = NULL;
        module->small_message_thresholds[fnc] = BCOL_THRESHOLD_UNLIMITED;
    }

    module->set_small_msg_thresholds = NULL;

    module->header_size = 0;
    module->bcol_memory_init = NULL;

    module->next_inorder = NULL;

    mca_bcol_base_fn_table_construct(module);
}

static void bcol_base_module_destructor(mca_bcol_base_module_t *module)
{
    int fnc;

    module->bcol_component = NULL;

    module->context_index = -1;
    module->init_module = NULL;
    module->sbgp_partner_module = NULL;
    module->squence_number_offset = 0;
    module->n_poll_loops = 0;

    for (fnc = 0; fnc < BCOL_NUM_OF_FUNCTIONS; fnc++) {
        module->bcol_function_table[fnc] = NULL;
    }

    module->bcol_memory_init = NULL;
}

OBJ_CLASS_INSTANCE(mca_bcol_base_module_t,
        opal_object_t,
        bcol_base_module_constructor,
        bcol_base_module_destructor);

static void bcol_base_network_context_constructor(bcol_base_network_context_t *nc)
{
    nc->context_id = -1;
    nc->context_data = NULL;
}

static void bcol_base_network_context_destructor(bcol_base_network_context_t *nc)
{
    nc->context_id = -1;
    nc->context_data = NULL;
    nc->register_memory_fn = NULL;
    nc->deregister_memory_fn = NULL;
}

OBJ_CLASS_INSTANCE(bcol_base_network_context_t,
        opal_object_t,
        bcol_base_network_context_constructor,
        bcol_base_network_context_destructor);

/* get list of subgrouping coponents to use */
static int mca_bcol_base_set_components_to_use(opal_list_t *bcol_components_avail,
                opal_list_t *bcol_components_in_use)
{
    /* local variables */
    const mca_base_component_t *b_component;

    mca_base_component_list_item_t *b_cli;
    mca_base_component_list_item_t *b_clj;

    char **bcols_requested;
    const char *b_component_name;

    /* split the requst for the bcol modules */
    bcols_requested = opal_argv_split(ompi_bcol_bcols_string, ',');
    if (NULL == bcols_requested) {
        return OMPI_ERROR;
    }

    /* Initialize list */
    OBJ_CONSTRUCT(bcol_components_in_use, opal_list_t);

    /* figure out basic collective modules to use */
    /* loop over list of components requested */
    for (int i = 0 ; bcols_requested[i] ; ++i) {
        /* loop over discovered components */
        OPAL_LIST_FOREACH(b_cli, bcol_components_avail, mca_base_component_list_item_t) {
            b_component = b_cli->cli_component;
            b_component_name = b_component->mca_component_name;

            if (0 == strcmp (b_component_name, bcols_requested[i])) {
                /* found selected component */
                b_clj = OBJ_NEW(mca_base_component_list_item_t);
                if (NULL == b_clj) {
                    return OPAL_ERR_OUT_OF_RESOURCE;
                }

                b_clj->cli_component = b_component;
                opal_list_append(bcol_components_in_use,
                                (opal_list_item_t *) b_clj);
                break;
             } /* end check for bcol component */
         }
     }

    /* Note: Need to add error checking to make sure all requested functions
    ** were found */

    /*
    ** release resources
    ** */

    opal_argv_free (bcols_requested);

    return OMPI_SUCCESS;
}

static int mca_bcol_base_register(mca_base_register_flag_t flags)
{
    /* figure out which bcol and sbgp components will actually be used */
    /* get list of sub-grouping functions to use */
    ompi_bcol_bcols_string = "basesmuma,basesmuma,iboffload,ptpcoll,ugni";
    (void) mca_base_var_register("ompi", "bcol", "base", "string",
                                 "Default set of basic collective components to use",
                                 MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                 OPAL_INFO_LVL_9,
                                 MCA_BASE_VAR_SCOPE_READONLY,
                                 &ompi_bcol_bcols_string);

    return OMPI_SUCCESS;
}

/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
static int mca_bcol_base_open(mca_base_open_flag_t flags)
{
    int ret;

    /* Open up all available components */
    if (OMPI_SUCCESS !=
        (ret = mca_base_framework_components_open(&ompi_bcol_base_framework, flags))) {
        return ret;
    }

    ret = mca_bcol_base_set_components_to_use(&ompi_bcol_base_framework.framework_components,
                                              &mca_bcol_base_components_in_use);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    /* memory registration compatibilities */
    bcol_mpool_compatibility[BCOL_SHARED_MEMORY_UMA][BCOL_SHARED_MEMORY_UMA]=1;
    bcol_mpool_compatibility[BCOL_SHARED_MEMORY_UMA][BCOL_SHARED_MEMORY_SOCKET]=1;
    bcol_mpool_compatibility[BCOL_SHARED_MEMORY_UMA][BCOL_POINT_TO_POINT]=1;
    bcol_mpool_compatibility[BCOL_SHARED_MEMORY_UMA][BCOL_IB_OFFLOAD]=1;
    bcol_mpool_compatibility[BCOL_SHARED_MEMORY_SOCKET][BCOL_SHARED_MEMORY_UMA]=1;
    bcol_mpool_compatibility[BCOL_POINT_TO_POINT]      [BCOL_SHARED_MEMORY_UMA]=1;
    bcol_mpool_compatibility[BCOL_IB_OFFLOAD]          [BCOL_SHARED_MEMORY_UMA]=1;

    return OMPI_SUCCESS;
}

static int mca_bcol_base_close (void)
{
    opal_list_item_t *item;

    while (NULL != (item = opal_list_remove_first (&mca_bcol_base_components_in_use))) {
        OBJ_RELEASE(item);
    }

    OBJ_DESTRUCT(&mca_bcol_base_components_in_use);

    return mca_base_framework_components_close(&ompi_bcol_base_framework, NULL);
}

/*
 * Prototype implementation of selection logic
 */
int mca_bcol_base_fn_table_construct(struct mca_bcol_base_module_t *bcol_module){

        int bcol_fn;
        /* Call all init functions */

        /* Create a function table */
        for (bcol_fn = 0; bcol_fn < BCOL_NUM_OF_FUNCTIONS; bcol_fn++){
            /* Create a list object for each bcol type list */
            OBJ_CONSTRUCT(&(bcol_module->bcol_fns_table[bcol_fn]), opal_list_t);
        }

    return OMPI_SUCCESS;
}

int mca_bcol_base_fn_table_destroy(struct mca_bcol_base_module_t *bcol_module){

    int bcol_fn;

    for (bcol_fn = 0; bcol_fn < BCOL_NUM_OF_FUNCTIONS; bcol_fn++){
        /* gvm FIX: Go through the list and destroy each item */
        /* Destroy the function table object for each bcol type list */
        OBJ_DESTRUCT(&(bcol_module->bcol_fns_table[bcol_fn]));
    }

    return OMPI_SUCCESS;
}

int mca_bcol_base_set_attributes(struct mca_bcol_base_module_t *bcol_module,
                mca_bcol_base_coll_fn_comm_attributes_t *arg_comm_attribs,
                mca_bcol_base_coll_fn_invoke_attributes_t *arg_inv_attribs,
                mca_bcol_base_module_collective_fn_primitives_t bcol_fn,
                mca_bcol_base_module_collective_fn_primitives_t progress_fn
                )
{
    mca_bcol_base_coll_fn_comm_attributes_t *comm_attribs = NULL;
    mca_bcol_base_coll_fn_invoke_attributes_t *inv_attribs = NULL;
    struct mca_bcol_base_coll_fn_desc_t *fn_filtered = NULL;
    int coll_type;

    comm_attribs = malloc(sizeof(mca_bcol_base_coll_fn_comm_attributes_t));
    inv_attribs = malloc(sizeof(mca_bcol_base_coll_fn_invoke_attributes_t));

    if (!((comm_attribs) && (inv_attribs))) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    coll_type = comm_attribs->bcoll_type = arg_comm_attribs->bcoll_type;
    comm_attribs->comm_size_min = arg_comm_attribs->comm_size_min;
    comm_attribs->comm_size_max = arg_comm_attribs->comm_size_max;
    comm_attribs->data_src = arg_comm_attribs->data_src;
    comm_attribs->waiting_semantics = arg_comm_attribs->waiting_semantics;

    inv_attribs->bcol_msg_min = arg_inv_attribs->bcol_msg_min;
    inv_attribs->bcol_msg_max = arg_inv_attribs->bcol_msg_max ;
    inv_attribs->datatype_bitmap = arg_inv_attribs->datatype_bitmap ;
    inv_attribs->op_types_bitmap = arg_inv_attribs->op_types_bitmap;

    fn_filtered = OBJ_NEW(mca_bcol_base_coll_fn_desc_t);

    fn_filtered->coll_fn = bcol_fn;
    fn_filtered->progress_fn = progress_fn;

    fn_filtered->comm_attr = comm_attribs;
    fn_filtered->inv_attr = inv_attribs;


    opal_list_append(&(bcol_module->bcol_fns_table[coll_type]),(opal_list_item_t*)fn_filtered);

    return OMPI_SUCCESS;
}

int mca_bcol_base_bcol_fns_table_init(struct mca_bcol_base_module_t *bcol_module){

    int ret, bcol_init_fn;

    for (bcol_init_fn =0; bcol_init_fn < BCOL_NUM_OF_FUNCTIONS; bcol_init_fn++) {
        if (NULL != bcol_module->bcol_function_init_table[bcol_init_fn]) {
            ret = (bcol_module->bcol_function_init_table[bcol_init_fn]) (bcol_module);
            if (OMPI_SUCCESS != ret) {
                return OMPI_ERROR;
            }
        }
    }

    return OMPI_SUCCESS;
}

static void mca_bcol_base_coll_fn_desc_destructor(mca_bcol_base_coll_fn_desc_t *fn)
{
    if (fn->comm_attr) {
        free(fn->comm_attr);
    }

    if (fn->inv_attr) {
        free(fn->inv_attr);
    }
}

OBJ_CLASS_INSTANCE(mca_bcol_base_coll_fn_desc_t,
                   opal_list_item_t,
                   NULL,
                   mca_bcol_base_coll_fn_desc_destructor);

static void lmngr_block_constructor(mca_bcol_base_lmngr_block_t *item) 
{
    item->base_addr = NULL;
}

static void lnmgr_block_destructor(mca_bcol_base_lmngr_block_t *item) 
{
    /* I have nothing to do here */
}
OBJ_CLASS_INSTANCE(mca_bcol_base_lmngr_block_t,
        opal_list_item_t,
        lmngr_block_constructor,
        lnmgr_block_destructor);
