/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2009-2013 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * Copyright (c) 2012-2014 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2013-2014 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/* 
 * Code for selecting a collective function. The selection is based on 
 * comm-time attributes and invoke-time attributes. 
 *
 * comm-time attributes: Attributes, which can used to filter available 
 * collective functions at communicator init time. Example attributes include
 * comm size and msg size supported by bcols.
 * 
 * invoke-time attributes: Attributes, which can be used to select function
 * for given collective when a collective is invoked. 
 *
 */

#include "coll_ml_select.h"

static int msg_to_range(size_t msg_len)
{
    int range;    

    if (msg_len < MSG_RANGE_INITIAL) {
        return 1;    
    }

    range = (int) log10((double)((msg_len / MSG_RANGE_INITIAL))); 
    
    if (range > NUM_MSG_RANGES)
        return NUM_MSG_RANGES;

    return range;
}

static int cmp_comm_attribs(struct mca_bcol_base_coll_fn_comm_attributes_t *attrib_var,
                    struct mca_bcol_base_coll_fn_comm_attributes_t *attrib_bcol){


    if (!(attrib_var->comm_size_max <= attrib_bcol->comm_size_max)) {
        return  -1 ;
    }

#if 0 /* Manju: pelase fix it*/
    if (attrib_var->data_src != attrib_bcol->data_src) {
        return -1;
    }
    
    if (attrib_var->waiting_semantics !=
                 attrib_bcol->waiting_semantics) {
        return -1;
    }
#endif

    return 0;
}

/*
 * Table that holds function names 
 */
static int init_invoke_table(mca_coll_ml_module_t *ml_module)
{
    int i=0,j=0,k=0, index_topo;
    int bcoll_type;
    struct mca_bcol_base_module_t *bcol_module = NULL;
    int j_bcol_module=0;
    int i_hier=0;
    mca_coll_ml_topology_t *topo;

    for (index_topo = 0; index_topo < COLL_ML_TOPO_MAX; index_topo++) {
        topo = &ml_module->topo_list[index_topo];
        if (COLL_ML_TOPO_DISABLED == topo->status) {
            /* skip the topology */
            continue;
        }
        for (i_hier = 0; i_hier < topo->n_levels; i_hier++) {

            for (j_bcol_module = 0; 
                    j_bcol_module < topo->component_pairs[i_hier].num_bcol_modules; 
                    ++j_bcol_module) {

                bcol_module = topo->component_pairs[i_hier].bcol_modules[j_bcol_module];

                for (bcoll_type = 0; bcoll_type < BCOL_NUM_OF_FUNCTIONS ; bcoll_type++){
                    for (i=0; i<NUM_MSG_RANGES; i++) {
                        for (j=0; j<OMPI_OP_NUM_OF_TYPES; j++) {
                            for (k=0; k<OMPI_DATATYPE_MAX_PREDEFINED; k++) {
                                bcol_module->filtered_fns_table[DATA_SRC_UNKNOWN][BLOCKING][bcoll_type][i][j][k] 
                                    = NULL;

                                bcol_module->filtered_fns_table[DATA_SRC_KNOWN][BLOCKING][bcoll_type][i][j][k] 
                                    = NULL;

                                bcol_module->filtered_fns_table[DATA_SRC_UNKNOWN][NON_BLOCKING][bcoll_type][i][j][k] 
                                    = NULL;

                                bcol_module->filtered_fns_table[DATA_SRC_KNOWN][NON_BLOCKING][bcoll_type][i][j][k] 
                                    = NULL;

                            }
                        }
                    }
                }
            }

        }
    }

    return 0;
}

static int add_to_invoke_table(mca_bcol_base_module_t *bcol_module, 
                       mca_bcol_base_coll_fn_desc_t *fn_filtered,
                       mca_coll_ml_module_t *ml_module)
{
    struct mca_bcol_base_coll_fn_invoke_attributes_t *inv_attribs = NULL;
    int bcoll_type, data_src_type, waiting_semantic;
    int range_min,range_max;
    int i=0,j=0,k=0,mask=1;


                
    if((NULL == fn_filtered->inv_attr)||(NULL == fn_filtered->comm_attr)) {
        return OMPI_ERROR;
    }

    ML_VERBOSE(10, ("Calling add_to_invoke_table %p",fn_filtered->coll_fn));

    inv_attribs = fn_filtered->inv_attr;
    bcoll_type = fn_filtered->comm_attr->bcoll_type;
    data_src_type = fn_filtered->comm_attr->data_src;
    waiting_semantic = fn_filtered->comm_attr->waiting_semantics;    

    range_min = msg_to_range(inv_attribs->bcol_msg_min);
    range_max = msg_to_range(inv_attribs->bcol_msg_max);

    for (j=0; j<OMPI_OP_NUM_OF_TYPES; j++) {
        for (k=0; k<OMPI_DATATYPE_MAX_PREDEFINED; k++) {
            
            if ((inv_attribs->datatype_bitmap & (mask << k)) && (inv_attribs->op_types_bitmap & (mask << j))){

               for (i=range_min; i<=range_max; i++) {
                    bcol_module->filtered_fns_table[data_src_type][waiting_semantic][bcoll_type][i][j][k] 
                                                                    = fn_filtered; 
                    ML_VERBOSE(21, ("Putting functions %d %d %d %d %p", bcoll_type, i, j, k, fn_filtered));
               }
            }
        }
    }

    return 0;

}

/* 
 * Maps count to msg range that is used for 
 * function table
 * RANGE 0 is for small messages (say small msg =10k)
 * MSG RANGE 1 - 10K - 100K
 * RANGE 2 - 100K -1M
 * RANGE 3 - 1M - 10M
 *
 * This is valid only when MSG_RANGE_INC is 10. 
 * For other values the function should replace log10 to log with 
 * base=MSG_RANGE_INC
 */ 
static int count_to_msg_range(int count,struct ompi_datatype_t *dtype)
{
    size_t msg_len =0,dt_size;    
    int range = 0 ;
    
    ompi_datatype_type_size(dtype, &dt_size);
    msg_len = count*dt_size;    

    if (msg_len < MSG_RANGE_INITIAL) {
        return 1;    
    }

    range = (int) log10((double)((msg_len/MSG_RANGE_INITIAL))); 
    
    if (range > NUM_MSG_RANGES)
        return NUM_MSG_RANGES;

    return range;

}

/* Based on the attributes filled in comm_select_attributes 
      select functions for invoke time filtering */


static int build_algorithms_table(mca_coll_ml_module_t *ml_module,struct
                mca_bcol_base_coll_fn_comm_attributes_t *my_comm_attrib)
{
    int i_hier, j_bcol_module, k_bcol_fn, index_topo;
    struct mca_bcol_base_module_t *bcol_module = NULL;
    opal_list_t *fn_filtered_list; 
    opal_list_item_t *item;
    mca_coll_ml_topology_t *topo;

    /* 
     * Go through each hierarchy and for each 
     * bcol module in the hierarchy, select the alogrithms.
     */
    for (index_topo = 0; index_topo < COLL_ML_TOPO_MAX; index_topo++) {
        topo = &ml_module->topo_list[index_topo];
        for (i_hier = 0; i_hier < topo->n_levels; i_hier++) {
            my_comm_attrib->comm_size_max = 
                topo->component_pairs[i_hier].subgroup_module->group_size;

            for (j_bcol_module = 0; 
                    j_bcol_module < topo->component_pairs[i_hier].num_bcol_modules; 
                    ++j_bcol_module) {

                bcol_module = topo->component_pairs[i_hier].bcol_modules[j_bcol_module];

                /* Go through all bcols and available bcol functions */
                for (k_bcol_fn = 0; k_bcol_fn < BCOL_NUM_OF_FUNCTIONS; k_bcol_fn++) {
                    struct mca_bcol_base_coll_fn_desc_t *fn_filtered = NULL;

                    /* Query the function attributes */
                    fn_filtered_list =
                        &(bcol_module->bcol_fns_table[k_bcol_fn]);


                    if (0 == opal_list_get_size(fn_filtered_list)) {
                        continue;
                    }
                    /* All definitions of a collective type is stored in the list
                     * Each item in the list is checked for compatability in the
                     * attributes and stored in the filtered list */ 
                    for (item = opal_list_get_first(fn_filtered_list);
                            item != opal_list_get_end(fn_filtered_list);
                            item = opal_list_get_next(item)){

                        fn_filtered = (struct mca_bcol_base_coll_fn_desc_t *)item;
                        if (cmp_comm_attribs(my_comm_attrib, fn_filtered->comm_attr) < 0) {
                            /* Criteria not satisfied continue to next bcol function */
                            continue;
                        }

                        /* 
                         * Add bcol function to be available for invoke time selection
                         */
                        add_to_invoke_table(bcol_module, fn_filtered, ml_module);
                    }

                }
            }
        }
    }

    return 0;

}

int mca_coll_ml_build_filtered_fn_table(mca_coll_ml_module_t *ml_module)
{

    struct mca_bcol_base_coll_fn_comm_attributes_t *my_comm_attrib = NULL;


    /* Init table storing all filtered functions */
    init_invoke_table(ml_module);

    my_comm_attrib = malloc(sizeof(struct mca_bcol_base_coll_fn_comm_attributes_t));
    
    if (!my_comm_attrib) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    my_comm_attrib->comm_size_min = 0;

    /*
     * This values should be passed using (maybe) mca parameters
     */
#if 0 /* Manju: pelase fix it*/
    my_comm_attrib->data_src = DATA_SRC_KNOWN;
    my_comm_attrib->waiting_semantics = BLOCKING;
#endif

    if (build_algorithms_table(ml_module,my_comm_attrib)) {
        return OMPI_ERROR;
    }

    free(my_comm_attrib);

    return OMPI_SUCCESS;

}

#if 0
static struct mca_bcol_base_coll_fn_invoke_attributes_t *mca_construct_invoke_attributes(
                struct ompi_datatype_t *dtype, int count,
                struct ompi_op_t op_type)
{
    size_t dt_size, msg_size;
    struct mca_bcol_base_coll_fn_invoke_attributes_t *inv_attribs = NULL;
    
    ompi_datatype_type_size(dtype, &dt_size);
    msg_size = count*dt_size;


    inv_attribs = malloc(sizeof(struct mca_bcol_base_coll_fn_invoke_attributes_t));
    
    /* Fix : We might need to have range for msg size - For now selection will
     * be based on maximum value
     */
    inv_attribs->bcol_msg_min = 0;
    inv_attribs->bcol_msg_max = msg_size;

    return inv_attribs;
}
#endif

int mca_select_bcol_function(mca_bcol_base_module_t *bcol_module,
                int bcoll_type,
                bcol_function_args_t *bcol_fn_arguments,
                mca_bcol_base_function_t *ml_fn_arguments )
{
        
    struct mca_bcol_base_coll_fn_desc_t *fn_filtered = NULL;
    int msg_range=0;
    int ret;
    int data_src_type = DATA_SRC_KNOWN, waiting_type = BLOCKING;

    msg_range =
            count_to_msg_range(bcol_fn_arguments->count,
                            bcol_fn_arguments->dtype); 
    if ((BCOL_ALLREDUCE == bcoll_type) || (BCOL_REDUCE == bcoll_type)) {
        /* needs to be resolved, the op structure has changed, there is no field called "op_type" */
        fn_filtered =
            bcol_module->filtered_fns_table[data_src_type][waiting_type][bcoll_type][msg_range][bcol_fn_arguments->dtype->id][bcol_fn_arguments->op->op_type];
    }
    else {
        fn_filtered =
            bcol_module->filtered_fns_table[data_src_type][waiting_type][bcoll_type][msg_range][bcol_fn_arguments->dtype->id][0];

    }
    
    if (NULL == fn_filtered) {    
        return OMPI_ERROR;
    }

    ret = (fn_filtered->coll_fn)(bcol_fn_arguments,ml_fn_arguments);
    return ret;
}

