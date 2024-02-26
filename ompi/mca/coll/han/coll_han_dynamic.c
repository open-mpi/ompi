/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2020-2022 Bull S.A.S. All rights reserved.
 * Copyright (c) 2021      Triad National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2022      IBM Corporation. All rights reserved
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


/*
 * @file
 * This files contains all functions to dynamically select for each collective 
 * the coll module based on given MCA parameters, configuration file and 
 * messages characteristics
*/

#include "opal/class/opal_list.h"
#include "ompi/mca/coll/han/coll_han.h"
#include "ompi/mca/coll/han/coll_han_dynamic.h"
#include "ompi/mca/coll/han/coll_han_algorithms.h"
#include "ompi/mca/coll/base/coll_base_util.h"

#define MCA_COLL_HAN_ANY_MESSAGE_SIZE 0

/*
 * Tests if a dynamic collective is implemented
 * Useful for file reading warnings and MCA parameter generation
 * When a new dynamic collective is implemented, this function must
 * return true for it
 */
bool mca_coll_han_is_coll_dynamic_implemented(COLLTYPE_T coll_id)
{
    switch (coll_id) {
    case ALLGATHER:
    case ALLGATHERV:
    case ALLREDUCE:
    case BARRIER:
    case BCAST:
    case GATHER:
    case GATHERV:
    case REDUCE:
    case SCATTER:
        return true;
    default:
        return false;
    }
}

COMPONENT_T
mca_coll_han_component_name_to_id(const char* name)
{
    if(NULL == name) {
        return -1;
    }

    for( int i = SELF; i < COMPONENTS_COUNT ; i++ ) {
        if (0 == strcmp(name, ompi_coll_han_available_components[i].component_name)) {
            return i;
        }
    }
    return -1;
}

/*
 * Get all the collective modules initialized on this communicator
 * This function must be called at the start of every selector implementation
 * Note that han module may be not yet enabled
 */
int
mca_coll_han_get_all_coll_modules(struct ompi_communicator_t *comm,
                                  mca_coll_han_module_t *han_module)
{
    mca_coll_base_module_t *han_base_module = (mca_coll_base_module_t *) han_module;
    TOPO_LVL_T topo_lvl = han_module->topologic_level;
    int nb_modules = 0;
    mca_coll_base_avail_coll_t *item;

    /* If the modules are get yet, return success */
    if(han_module->storage_initialized) {
        return OMPI_SUCCESS;
    }
    /* This list is populated at communicator creation */
    OPAL_LIST_FOREACH(item,
                      comm->c_coll->module_list,
                      mca_coll_base_avail_coll_t) {
        mca_coll_base_module_t *module = item->ac_module;
        const char *name = item->ac_component_name;
        int id = mca_coll_han_component_name_to_id(name);

        if(id >= 0 && NULL != module && module != han_base_module) {
            /*
             * The identifier is correct
             * Store the module
             */
            han_module->modules_storage.modules[id].module_handler = module;
            opal_output_verbose(80, mca_coll_han_component.han_output,
                                "coll:han:get_all_coll_modules HAN found module %s with id %d "
                                "for topological level %d (%s) for communicator (%s/%s)\n",
                                name, id, topo_lvl, mca_coll_han_topo_lvl_to_str(topo_lvl),
                                ompi_comm_print_cid(comm), comm->c_name);
            nb_modules++;
        }
    }

    /*
     * Add han_module on global communicator only
     * to prevent any recursive call
     */
    if(GLOBAL_COMMUNICATOR == han_module->topologic_level) {
        han_module->modules_storage.modules[HAN].module_handler = han_base_module;
        nb_modules++;
    }

    opal_output_verbose(60, mca_coll_han_component.han_output,
                        "coll:han:get_all_coll_modules HAN sub-communicator modules storage "
                        "for topological level %d (%s) gets %d modules "
                        "for communicator (%s/%s)\n",
                        topo_lvl, mca_coll_han_topo_lvl_to_str(topo_lvl),
                        nb_modules, ompi_comm_print_cid(comm), comm->c_name);

    assert(0 != nb_modules);

    /* The modules are get */
    han_module->storage_initialized = true;
    return OMPI_SUCCESS;
}

/*
 * Find the correct rule in the dynamic rules
 * Assume rules are sorted by increasing value
 */
static const msg_size_rule_t*
get_dynamic_rule(COLLTYPE_T collective,
                 size_t msg_size,
                 struct ompi_communicator_t *comm,
                 mca_coll_han_module_t *han_module)
{
    /* Indexes of the rule */
    int coll_idx, topo_idx;
    int conf_idx, msg_size_idx;

    /* Aliases */
    const mca_coll_han_dynamic_rules_t *dynamic_rules;
    const collective_rule_t *coll_rule = NULL;
    const topologic_rule_t *topo_rule = NULL;
    const configuration_rule_t *conf_rule = NULL;
    const msg_size_rule_t *msg_size_rule = NULL;

    const TOPO_LVL_T topo_lvl = han_module->topologic_level;
    const int comm_size = ompi_comm_size(comm);

    COMPONENT_T component;

    /* Find the collective rule */
    dynamic_rules = &(mca_coll_han_component.dynamic_rules);
    for(coll_idx = dynamic_rules->nb_collectives-1;
        coll_idx >= 0; coll_idx--) {
        if(dynamic_rules->collective_rules[coll_idx].collective_id == collective) {
            coll_rule = &(dynamic_rules->collective_rules[coll_idx]);
            break;
        }
    }
    if(coll_idx < 0 || NULL == coll_rule) {
        /* No dynamic rules for this collective */
        opal_output_verbose(60, mca_coll_han_component.han_output,
                            "coll:han:get_dynamic_rule HAN searched for collective %d (%s) "
                            "but did not find any rule for this collective\n",
                            collective, mca_coll_base_colltype_to_str(collective));
        return NULL;
    }

    /* Find the topologic level rule */
    for(topo_idx = coll_rule->nb_topologic_levels-1;
        topo_idx >= 0; topo_idx--) {
        if(coll_rule->topologic_rules[topo_idx].topologic_level == topo_lvl) {
            topo_rule = &(coll_rule->topologic_rules[topo_idx]);
            break;
        }
    }
    if(topo_idx < 0 || NULL == topo_rule) {
        /* No topologic level rules for this collective */
        opal_output_verbose(60, mca_coll_han_component.han_output,
                            "coll:han:get_dynamic_rule HAN searched for topologic level %d (%s) rule "
                            "for collective %d (%s) but did not find any rule\n",
                            topo_lvl, mca_coll_han_topo_lvl_to_str(topo_lvl),
                            collective, mca_coll_base_colltype_to_str(collective));
        return NULL;
    }

    /* Find the configuration rule */
    for(conf_idx = topo_rule->nb_rules-1;
        conf_idx >= 0; conf_idx--) {
        if(topo_rule->configuration_rules[conf_idx].configuration_size <= comm_size) {
            conf_rule = &(topo_rule->configuration_rules[conf_idx]);
            break;
        }
    }
    if(conf_idx < 0 || NULL == conf_rule) {
        /* No corresponding configuration. Should not have happen with a correct file */
        opal_output_verbose(60, mca_coll_han_component.han_output,
                            "coll:han:get_dynamic_rule HAN searched a rule for collective %d (%s) "
                            "on topological level %d (%s) for a %d configuration size "
                            "but did not manage to find anything. "
                            "This is the result of an invalid configuration file: "
                            "the first configuration size of each collective must be 1\n",
                            collective, mca_coll_base_colltype_to_str(collective),
                            topo_lvl, mca_coll_han_topo_lvl_to_str(topo_lvl), comm_size);
        return NULL;
    }

    /* Find the message size rule */
    for(msg_size_idx = conf_rule->nb_msg_size-1;
        msg_size_idx >= 0; msg_size_idx--) {
        if(conf_rule->msg_size_rules[msg_size_idx].msg_size <= msg_size) {
            msg_size_rule = &(conf_rule->msg_size_rules[msg_size_idx]);
            break;
        }
    }
    if(msg_size_idx < 0 || NULL == msg_size_rule) {
        /* No corresponding message size. Should not happen with a correct file */
        opal_output_verbose(60, mca_coll_han_component.han_output,
                            "coll:han:get_dynamic_rule HAN searched a rule for collective %d (%s) "
                            "on topological level %d (%s) for a %d configuration size "
                            "for a %" PRIsize_t " sized message but did not manage to find anything. "
                            "This is the result of an invalid configuration file: "
                            "the first message size of each configuration must be 0\n",
                            collective, mca_coll_base_colltype_to_str(collective),
                            topo_lvl, mca_coll_han_topo_lvl_to_str(topo_lvl),
                            comm_size, msg_size);

        return NULL;
    }

    component = msg_size_rule->component;
    /*
     * We have the final rule to use
     * Module correctness is checked outside
     */
    opal_output_verbose(80, mca_coll_han_component.han_output,
                        "coll:han:get_dynamic_rule HAN searched a rule for collective %d (%s) "
                        "on topological level %d (%s) for a %d configuration size "
                        "for a %" PRIsize_t " sized message. Found a rule for collective %d (%s) "
                        "on topological level %d (%s) for a %d configuration size "
                        "for a %" PRIsize_t " sized message : component %d (%s)\n",
                        collective, mca_coll_base_colltype_to_str(collective),
                        topo_lvl, mca_coll_han_topo_lvl_to_str(topo_lvl),
                        comm_size, msg_size, msg_size_rule->collective_id,
                        mca_coll_base_colltype_to_str(msg_size_rule->collective_id),
                        msg_size_rule->topologic_level,
                        mca_coll_han_topo_lvl_to_str(msg_size_rule->topologic_level),
                        msg_size_rule->configuration_size,
                        msg_size_rule->msg_size, component, ompi_coll_han_available_components[component].component_name);

    return msg_size_rule;
}

/*
 * Return the module to use for the collective coll_id
 * for a msg_size sized message on the comm communicator
 * following the dynamic rules
 */
static mca_coll_base_module_t*
get_module(COLLTYPE_T coll_id,
           size_t msg_size,
           struct ompi_communicator_t *comm,
           mca_coll_han_module_t *han_module)
{
    const msg_size_rule_t *dynamic_rule;
    TOPO_LVL_T topo_lvl;
    COMPONENT_T mca_rule_component;

    topo_lvl = han_module->topologic_level;
    mca_rule_component = mca_coll_han_component.mca_sub_components[coll_id][topo_lvl];

    mca_coll_han_get_all_coll_modules(comm, han_module);

    /* Find the correct dynamic rule to check */
    dynamic_rule = get_dynamic_rule(coll_id,
                                    msg_size,
                                    comm,
                                    han_module);
    if(NULL != dynamic_rule) {
        /* Use dynamic rule from file */
        return han_module->modules_storage.modules[dynamic_rule->component].module_handler;
    }
    /*
     * No dynamic rule from file
     * Use rule from mca parameter
     */
    if(mca_rule_component < 0 || mca_rule_component >= COMPONENTS_COUNT) {
        /*
         * Invalid MCA parameter value
         * Warn the user and return NULL
         */
        opal_output_verbose(0, mca_coll_han_component.han_output,
                            "coll:han:get_module Invalid MCA parameter value %d "
                            "for collective %d (%s) on topologic level %d (%s)\n",
                            mca_rule_component, coll_id,
                            mca_coll_base_colltype_to_str(coll_id),
                            topo_lvl, mca_coll_han_topo_lvl_to_str(topo_lvl));
        return NULL;
    }
    return han_module->modules_storage.modules[mca_rule_component].module_handler;
}

/*
 * whether OMPI_MCA_coll_han_use_xxx_algorithm was set by user
 */
static bool
han_algorithm_is_user_provided(COLLTYPE_T coll_id) {
    const int *value = NULL;
    mca_base_var_source_t source;
    mca_base_var_get_value(mca_coll_han_component.use_algorithm_param[coll_id],
                           &value, &source, NULL);
    return (MCA_BASE_VAR_SOURCE_DEFAULT != source);
}

/*
 * Return the algorithm to use for the collective coll_id
 * for a msg_size sized message on the comm communicator
 * following the dynamic rules
 */
static int
get_algorithm(COLLTYPE_T coll_id,
              size_t msg_size,
              struct ompi_communicator_t *comm,
              mca_coll_han_module_t *han_module)
{
    int algorithm_id = -1;
    int rank = ompi_comm_rank(comm);
    algorithm_id = mca_coll_han_component.use_algorithm[coll_id];
    if (!han_algorithm_is_user_provided(coll_id)) {
        /* find the correct dynamic rule to check */
        const msg_size_rule_t *dynamic_rule = get_dynamic_rule(coll_id,
                                                               msg_size,
                                                               comm,
                                                               han_module);
        if(NULL != dynamic_rule && dynamic_rule->algorithm_id >= 0) {
            /* Use dynamic rule from file */
            algorithm_id = dynamic_rule->algorithm_id;
        } else {
            /*
             * No dynamic rule from file
             * Use default behaviour
             */
            algorithm_id = 0;
        }
    }
    if ( 0 == rank ) {
        opal_output_verbose(1, mca_coll_han_component.han_output,
                            "coll:han:get_algorithm %s size:%ld algorithm:%d %s",
                            mca_coll_base_colltype_to_str(coll_id),
                            msg_size,
                            algorithm_id,
                            mca_coll_han_algorithm_id_to_name(coll_id, algorithm_id));
    }
    return algorithm_id;
}


/*
 * Allgather selector:
 * On a sub-communicator, checks the stored rules to find the module to use
 * On the global communicator, calls the han collective implementation, or
 * calls the correct module if fallback mechanism is activated
 */
int
mca_coll_han_allgather_intra_dynamic(const void *sbuf, int scount,
                                     struct ompi_datatype_t *sdtype,
                                     void *rbuf, int rcount,
                                     struct ompi_datatype_t *rdtype,
                                     struct ompi_communicator_t *comm,
                                     mca_coll_base_module_t *module)
{
    mca_coll_han_module_t *han_module = (mca_coll_han_module_t*) module;
    TOPO_LVL_T topo_lvl = han_module->topologic_level;
    mca_coll_base_module_allgather_fn_t allgather;
    mca_coll_base_module_t *sub_module;
    size_t dtype_size;
    int rank, verbosity = 0;

    /* Compute configuration information for dynamic rules */
    if( MPI_IN_PLACE != sbuf ) {
        ompi_datatype_type_size(sdtype, &dtype_size);
        dtype_size = dtype_size * scount;
    } else {
        ompi_datatype_type_size(rdtype, &dtype_size);
        dtype_size = dtype_size * rcount;
    }
    sub_module = get_module(ALLGATHER,
                            dtype_size,
                            comm,
                            han_module);

    /* First errors are always printed by rank 0 */
    rank = ompi_comm_rank(comm);
    if( (0 == rank) && (han_module->dynamic_errors < mca_coll_han_component.max_dynamic_errors) ) {
        verbosity = 30;
    }

    if(NULL == sub_module) {
        /*
         * No valid collective module from dynamic rules
         * nor from mca parameter
         */
        han_module->dynamic_errors++;
        opal_output_verbose(verbosity, mca_coll_han_component.han_output,
                            "coll:han:mca_coll_han_allgather_intra_dynamic "
                            "HAN did not find any valid module for collective %d (%s) "
                            "with topological level %d (%s) on communicator (%s/%s). "
                            "Please check dynamic file/mca parameters\n",
                            ALLGATHER, mca_coll_base_colltype_to_str(ALLGATHER),
                            topo_lvl, mca_coll_han_topo_lvl_to_str(topo_lvl),
                            ompi_comm_print_cid(comm), comm->c_name);
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                             "HAN/ALLGATHER: No module found for the sub-communicator. "
                             "Falling back to another component\n"));
        allgather = han_module->previous_allgather;
        sub_module = han_module->previous_allgather_module;
    } else if (NULL == sub_module->coll_allgather) {
        /*
         * No valid collective from dynamic rules
         * nor from mca parameter
         */
        han_module->dynamic_errors++;
        opal_output_verbose(verbosity, mca_coll_han_component.han_output,
                            "coll:han:mca_coll_han_allgather_intra_dynamic HAN found valid module for collective %d (%s) "
                            "with topological level %d (%s) on communicator (%s/%s) but this module cannot handle this collective. "
                            "Please check dynamic file/mca parameters\n",
                            ALLGATHER, mca_coll_base_colltype_to_str(ALLGATHER),
                            topo_lvl, mca_coll_han_topo_lvl_to_str(topo_lvl),
                            ompi_comm_print_cid(comm), comm->c_name);
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                             "HAN/ALLGATHER: the module found for the sub-communicator"
                             " cannot handle the ALLGATHER operation. Falling back to another component\n"));
        allgather = han_module->previous_allgather;
        sub_module = han_module->previous_allgather_module;
    } else if (GLOBAL_COMMUNICATOR == topo_lvl && sub_module == module) {
        /*
         * No fallback mechanism activated for this configuration
         * sub_module is valid
         * sub_module->coll_allgather is valid and point to this function
         * Call han topological collective algorithm
         */
        int algorithm_id = get_algorithm(ALLGATHER,
                                         dtype_size,
                                         comm,
                                         han_module);
        allgather = (mca_coll_base_module_allgather_fn_t)mca_coll_han_algorithm_id_to_fn(ALLGATHER, algorithm_id);
        if (NULL == allgather) { /* default behaviour */
            if(mca_coll_han_component.use_simple_algorithm[ALLGATHER]) {
                allgather = mca_coll_han_allgather_intra_simple;
            } else {
                allgather = mca_coll_han_allgather_intra;
            }
        }
    } else {
        /*
         * If we get here:
         * sub_module is valid
         * sub_module->coll_allgather is valid
         * They points to the collective to use, according to the dynamic rules
         * Selector's job is done, call the collective
         */
        allgather = sub_module->coll_allgather;
    }
    return allgather(sbuf, scount, sdtype,
                     rbuf, rcount, rdtype,
                     comm,
                     sub_module);
}


/*
 * Allgatherv selector:
 * On a sub-communicator, checks the stored rules to find the module to use
 * On the global communicator, calls the han collective implementation, or
 * calls the correct module if fallback mechanism is activated
 * The allgatherv size is the size of the biggest segment
 */
int
mca_coll_han_allgatherv_intra_dynamic(const void *sbuf, int scount,
                                      struct ompi_datatype_t *sdtype,
                                      void *rbuf, const int *rcounts,
                                      const int *displs,
                                      struct ompi_datatype_t *rdtype,
                                      struct ompi_communicator_t *comm,
                                      mca_coll_base_module_t *module)
{
    mca_coll_han_module_t *han_module = (mca_coll_han_module_t*) module;
    TOPO_LVL_T topo_lvl = han_module->topologic_level;
    mca_coll_base_module_allgatherv_fn_t allgatherv;
    int rank, verbosity = 0, comm_size, i;
    mca_coll_base_module_t *sub_module;
    size_t dtype_size, msg_size = 0;

    /* Compute configuration information for dynamic rules */
    comm_size = ompi_comm_size(comm);
    ompi_datatype_type_size(rdtype, &dtype_size);

    for(i = 0; i < comm_size; i++) {
        if(dtype_size * rcounts[i] > msg_size) {
            msg_size = dtype_size * rcounts[i];
        }
    }

    sub_module = get_module(ALLGATHERV,
                            msg_size,
                            comm,
                            han_module);

    /* First errors are always printed by rank 0 */
    rank = ompi_comm_rank(comm);
    if( (0 == rank) && (han_module->dynamic_errors < mca_coll_han_component.max_dynamic_errors) ) {
        verbosity = 30;
    }

    if(NULL == sub_module) {
        /*
         * No valid collective module from dynamic rules
         * nor from mca parameter
         */
        han_module->dynamic_errors++;
        opal_output_verbose(verbosity, mca_coll_han_component.han_output,
                            "coll:han:mca_coll_han_allgatherv_intra_dynamic "
                            "HAN did not find any valid module for collective %d (%s) "
                            "with topological level %d (%s) on communicator (%s/%s). "
                            "Please check dynamic file/mca parameters\n",
                            ALLGATHERV, mca_coll_base_colltype_to_str(ALLGATHERV),
                            topo_lvl, mca_coll_han_topo_lvl_to_str(topo_lvl),
                            ompi_comm_print_cid(comm), comm->c_name);
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                             "HAN/ALLGATHERV: No module found for the sub-communicator. "
                             "Falling back to another component\n"));
        allgatherv = han_module->previous_allgatherv;
        sub_module = han_module->previous_allgatherv_module;
    } else if (NULL == sub_module->coll_allgatherv) {
        /*
         * No valid collective from dynamic rules
         * nor from mca parameter
         */
        han_module->dynamic_errors++;
        opal_output_verbose(verbosity, mca_coll_han_component.han_output,
                            "coll:han:mca_coll_han_allgatherv_intra_dynamic "
                            "HAN found valid module for collective %d (%s) "
                            "with topological level %d (%s) on communicator (%s/%s) "
                            "but this module cannot handle this collective. "
                            "Please check dynamic file/mca parameters\n",
                            ALLGATHERV, mca_coll_base_colltype_to_str(ALLGATHERV),
                            topo_lvl, mca_coll_han_topo_lvl_to_str(topo_lvl),
                            ompi_comm_print_cid(comm), comm->c_name);
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                             "HAN/ALLGATHERV: the module found for the sub-"
                             "communicator cannot handle the ALLGATHERV operation. "
                             "Falling back to another component\n"));
        allgatherv = han_module->previous_allgatherv;
        sub_module = han_module->previous_allgatherv_module;
    } else if (GLOBAL_COMMUNICATOR == topo_lvl && sub_module == module) {
        /*
         * No fallback mechanism activated for this configuration
         * sub_module is valid
         * sub_module->coll_allgatherv is valid and point to this function
         * Call han topological collective algorithm
         */
        opal_output_verbose(30, mca_coll_han_component.han_output,
                            "coll:han:mca_coll_han_allgatherv_intra_dynamic "
                            "HAN used for collective %d (%s) with topological level %d (%s) "
                            "on communicator (%s/%s) but this module cannot handle "
                            "this collective on this topologic level\n",
                            ALLGATHERV, mca_coll_base_colltype_to_str(ALLGATHERV),
                            topo_lvl, mca_coll_han_topo_lvl_to_str(topo_lvl),
                            ompi_comm_print_cid(comm), comm->c_name);
        allgatherv = han_module->previous_allgatherv;
        sub_module = han_module->previous_allgatherv_module;
    } else {
        /*
         * If we get here:
         * sub_module is valid
         * sub_module->coll_allgatherv is valid
         * They points to the collective to use, according to the dynamic rules
         * Selector's job is done, call the collective
         */
        allgatherv = sub_module->coll_allgatherv;
    }
    return allgatherv(sbuf, scount, sdtype,
                      rbuf, rcounts, displs,
                      rdtype, comm,
                      sub_module);
}


/*
 * Allreduce selector:
 * On a sub-communicator, checks the stored rules to find the module to use
 * On the global communicator, calls the han collective implementation, or
 * calls the correct module if fallback mechanism is activated
 */
int
mca_coll_han_allreduce_intra_dynamic(const void *sbuf,
                                     void *rbuf,
                                     int count,
                                     struct ompi_datatype_t *dtype,
                                     struct ompi_op_t *op,
                                     struct ompi_communicator_t *comm,
                                     mca_coll_base_module_t *module)
{
    mca_coll_han_module_t *han_module = (mca_coll_han_module_t*) module;
    TOPO_LVL_T topo_lvl = han_module->topologic_level;
    mca_coll_base_module_allreduce_fn_t allreduce;
    mca_coll_base_module_t *sub_module;
    size_t dtype_size;
    int rank, verbosity = 0;

    if (!han_module->enabled) {
        return han_module->previous_allreduce(sbuf, rbuf, count, dtype, op, comm,
                                              han_module->previous_allreduce_module);
    }

    /* Compute configuration information for dynamic rules */
    ompi_datatype_type_size(dtype, &dtype_size);
    dtype_size = dtype_size * count;

    sub_module = get_module(ALLREDUCE,
                            dtype_size,
                            comm,
                            han_module);

    /* First errors are always printed by rank 0 */
    rank = ompi_comm_rank(comm);
    if( (0 == rank) && (han_module->dynamic_errors < mca_coll_han_component.max_dynamic_errors) ) {
        verbosity = 30;
    }

    if(NULL == sub_module) {
        /*
         * No valid collective module from dynamic rules
         * nor from mca parameter
         */
        han_module->dynamic_errors++;
        opal_output_verbose(verbosity, mca_coll_han_component.han_output,
                            "coll:han:mca_coll_han_allreduce_intra_dynamic "
                            "HAN did not find any valid module for collective %d (%s) "
                            "with topological level %d (%s) on communicator (%s/%s). "
                            "Please check dynamic file/mca parameters\n",
                            ALLREDUCE, mca_coll_base_colltype_to_str(ALLREDUCE),
                            topo_lvl, mca_coll_han_topo_lvl_to_str(topo_lvl),
                            ompi_comm_print_cid(comm), comm->c_name);
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                             "HAN/ALLREDUCE: No module found for the sub-communicator. "
                             "Falling back to another component\n"));
        allreduce = han_module->previous_allreduce;
        sub_module = han_module->previous_allreduce_module;
    } else if (NULL == sub_module->coll_allreduce) {
        /*
         * No valid collective from dynamic rules
         * nor from mca parameter
         */
        han_module->dynamic_errors++;
        opal_output_verbose(verbosity, mca_coll_han_component.han_output,
                            "coll:han:mca_coll_han_allreduce_intra_dynamic "
                            "HAN found valid module for collective %d (%s) "
                            "with topological level %d (%s) on communicator (%s/%s) "
                            "but this module cannot handle this collective. "
                            "Please check dynamic file/mca parameters\n",
                            ALLREDUCE, mca_coll_base_colltype_to_str(ALLREDUCE),
                            topo_lvl, mca_coll_han_topo_lvl_to_str(topo_lvl),
                            ompi_comm_print_cid(comm), comm->c_name);
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                             "HAN/ALLREDUCE: the module found for the sub-"
                             "communicator cannot handle the ALLREDUCE operation. "
                             "Falling back to another component\n"));
        allreduce = han_module->previous_allreduce;
        sub_module = han_module->previous_allreduce_module;
    } else if (GLOBAL_COMMUNICATOR == topo_lvl && sub_module == module) {
        /* Reproducibility: fallback on reproducible algorithm */
        if (mca_coll_han_component.han_reproducible) {
            allreduce = mca_coll_han_allreduce_reproducible;
        } else {
            /*
             * No fallback mechanism activated for this configuration
             * sub_module is valid
             * sub_module->coll_allreduce is valid and point to this function
             * Call han topological collective algorithm
             */
            int algorithm_id = get_algorithm(ALLREDUCE, dtype_size, comm, han_module);
            allreduce = (mca_coll_base_module_allreduce_fn_t) mca_coll_han_algorithm_id_to_fn(ALLREDUCE, algorithm_id);

            if (NULL == allreduce) { /* default behaviour */
                if(mca_coll_han_component.use_simple_algorithm[ALLREDUCE]) {
                    allreduce = mca_coll_han_allreduce_intra_simple;
                } else {
                    allreduce = mca_coll_han_allreduce_intra;
                }
            }
        }
    } else {
        /*
         * If we get here:
         * sub_module is valid
         * sub_module->coll_allreduce is valid
         * They points to the collective to use, according to the dynamic rules
         * Selector's job is done, call the collective
         */
        allreduce = sub_module->coll_allreduce;
    }
    return allreduce(sbuf, rbuf, count, dtype,
                     op, comm, sub_module);
}


/*
 * Barrier selector:
 * On a sub-communicator, checks the stored rules to find the module to use
 * On the global communicator, calls the han collective implementation, or
 * calls the correct module if fallback mechanism is activated
 */
int
mca_coll_han_barrier_intra_dynamic(struct ompi_communicator_t *comm,
                                   mca_coll_base_module_t *module)
{
    mca_coll_han_module_t *han_module = (mca_coll_han_module_t*) module;
    TOPO_LVL_T topo_lvl = han_module->topologic_level;
    mca_coll_base_module_barrier_fn_t barrier;
    mca_coll_base_module_t *sub_module;
    int rank, verbosity = 0;

    if (!han_module->enabled) {
        return han_module->previous_barrier(comm, han_module->previous_barrier_module);
    }

    /* Compute configuration information for dynamic rules */
    sub_module = get_module(BARRIER,
                            0,
                            comm,
                            han_module);

    /* First errors are always printed by rank 0 */
    rank = ompi_comm_rank(comm);
    if( (0 == rank) && (han_module->dynamic_errors < mca_coll_han_component.max_dynamic_errors) ) {
        verbosity = 30;
    }

    if(NULL == sub_module) {
        /*
         * No valid collective module from dynamic rules
         * nor from mca parameter
         */
        han_module->dynamic_errors++;
        opal_output_verbose(verbosity, mca_coll_han_component.han_output,
                            "coll:han:mca_coll_han_barrier_intra_dynamic "
                            "Han did not find any valid module for collective %d (%s) "
                            "with topological level %d (%s) on communicator (%s/%s). "
                            "Please check dynamic file/mca parameters\n",
                            BARRIER, mca_coll_base_colltype_to_str(BARRIER),
                            topo_lvl, mca_coll_han_topo_lvl_to_str(topo_lvl),
                            ompi_comm_print_cid(comm), comm->c_name);
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                             "HAN/BARRIER: No module found for the sub-communicator. "
                             "Falling back to another component\n"));
        barrier = han_module->previous_barrier;
        sub_module = han_module->previous_barrier_module;
    } else if (NULL == sub_module->coll_barrier) {
        /*
         * No valid collective from dynamic rules
         * nor from mca parameter
         */
        han_module->dynamic_errors++;
        opal_output_verbose(verbosity, mca_coll_han_component.han_output,
                            "coll:han:mca_coll_han_barrier_intra_dynamic "
                            "Han found valid module for collective %d (%s) "
                            "with topological level %d (%s) on communicator (%s/%s) "
                            "but this module cannot handle this collective. "
                            "Please check dynamic file/mca parameters\n",
                            BARRIER, mca_coll_base_colltype_to_str(BARRIER),
                            topo_lvl, mca_coll_han_topo_lvl_to_str(topo_lvl),
                            ompi_comm_print_cid(comm), comm->c_name);
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                             "HAN/BARRIER: the module found for the sub-"
                             "communicator cannot handle the BARRIER operation. "
                             "Falling back to another component\n"));
        barrier = han_module->previous_barrier;
        sub_module = han_module->previous_barrier_module;
    } else if (GLOBAL_COMMUNICATOR == topo_lvl && sub_module == module) {
        /*
         * No fallback mechanism activated for this configuration
         * sub_module is valid
         * sub_module->coll_barrier is valid and point to this function
         * Call han topological collective algorithm
         */
        int algorithm_id = get_algorithm(BARRIER, 0, comm, han_module);
        barrier = (mca_coll_base_module_barrier_fn_t) mca_coll_han_algorithm_id_to_fn(BARRIER, algorithm_id);
        if (NULL == barrier) { /* default behaviour*/
            barrier = mca_coll_han_barrier_intra_simple;
        }
    } else {
        /*
         * If we get here:
         * sub_module is valid
         * sub_module->coll_barrier is valid
         * They points to the collective to use, according to the dynamic rules
         * Selector's job is done, call the collective
         */
        barrier = sub_module->coll_barrier;
    }
    return barrier(comm, sub_module);
}

/*
 * Bcast selector:
 * On a sub-communicator, checks the stored rules to find the module to use
 * On the global communicator, calls the han collective implementation, or
 * calls the correct module if fallback mechanism is activated
 */
int
mca_coll_han_bcast_intra_dynamic(void *buff,
                                 int count,
                                 struct ompi_datatype_t *dtype,
                                 int root,
                                 struct ompi_communicator_t *comm,
                                 mca_coll_base_module_t *module)
{
    mca_coll_han_module_t *han_module = (mca_coll_han_module_t*) module;
    TOPO_LVL_T topo_lvl = han_module->topologic_level;
    mca_coll_base_module_bcast_fn_t bcast;
    mca_coll_base_module_t *sub_module;
    size_t dtype_size;
    int rank, verbosity = 0;

    if (!han_module->enabled) {
        return han_module->previous_bcast(buff, count, dtype, root, comm,
                                          han_module->previous_bcast_module);
    }

    /* Compute configuration information for dynamic rules */
    ompi_datatype_type_size(dtype, &dtype_size);
    dtype_size = dtype_size * count;

    sub_module = get_module(BCAST,
                            dtype_size,
                            comm,
                            han_module);

    /* First errors are always printed by rank 0 */
    rank = ompi_comm_rank(comm);
    if( (0 == rank) && (han_module->dynamic_errors < mca_coll_han_component.max_dynamic_errors) ) {
        verbosity = 30;
    }

    if(NULL == sub_module) {
        /*
         * No valid collective module from dynamic rules
         * nor from mca parameter
         */
        han_module->dynamic_errors++;
        opal_output_verbose(verbosity, mca_coll_han_component.han_output,
                            "coll:han:mca_coll_han_bcast_intra_dynamic "
                            "HAN did not find any valid module for collective %d (%s) "
                            "with topological level %d (%s) on communicator (%s/%s). "
                            "Please check dynamic file/mca parameters\n",
                            BCAST, mca_coll_base_colltype_to_str(BCAST),
                            topo_lvl, mca_coll_han_topo_lvl_to_str(topo_lvl),
                            ompi_comm_print_cid(comm), comm->c_name);
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                             "HAN/BCAST: No module found for the sub-communicator. "
                             "Falling back to another component\n"));
        bcast = han_module->previous_bcast;
        sub_module = han_module->previous_bcast_module;
    } else if (NULL == sub_module->coll_bcast) {
        /*
         * No valid collective from dynamic rules
         * nor from mca parameter
         */
        han_module->dynamic_errors++;
        opal_output_verbose(verbosity, mca_coll_han_component.han_output,
                            "coll:han:mca_coll_han_bcast_intra_dynamic "
                            "HAN found valid module for collective %d (%s) "
                            "with topological level %d (%s) on communicator (%s/%s) "
                            "but this module cannot handle this collective. "
                            "Please check dynamic file/mca parameters\n",
                            BCAST, mca_coll_base_colltype_to_str(BCAST),
                            topo_lvl, mca_coll_han_topo_lvl_to_str(topo_lvl),
                            ompi_comm_print_cid(comm), comm->c_name);
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                             "HAN/BCAST: the module found for the sub-"
                             "communicator cannot handle the BCAST operation. "
                             "Falling back to another component\n"));
        bcast = han_module->previous_bcast;
        sub_module = han_module->previous_bcast_module;
    } else if (GLOBAL_COMMUNICATOR == topo_lvl && sub_module == module) {
        /*
         * No fallback mechanism activated for this configuration
         * sub_module is valid
         * sub_module->coll_bcast is valid and point to this function
         * Call han topological collective algorithm
         */
        int algorithm_id = get_algorithm(BCAST,
                                         dtype_size,
                                         comm,
                                         han_module);
        bcast = (mca_coll_base_module_bcast_fn_t)mca_coll_han_algorithm_id_to_fn(BCAST, algorithm_id);
        if (NULL == bcast) { /* default behaviour */
             if(mca_coll_han_component.use_simple_algorithm[BCAST]) {
                bcast = mca_coll_han_bcast_intra_simple;
            } else {
                bcast = mca_coll_han_bcast_intra;
            }
        }
    } else {
        /*
         * If we get here:
         * sub_module is valid
         * sub_module->coll_bcast is valid
         * They points to the collective to use, according to the dynamic rules
         * Selector's job is done, call the collective
         */
        bcast = sub_module->coll_bcast;
    }
    return bcast(buff, count, dtype,
                 root, comm, sub_module);
}


/*
 * Gather selector:
 * On a sub-communicator, checks the stored rules to find the module to use
 * On the global communicator, calls the han collective implementation, or
 * calls the correct module if fallback mechanism is activated
 */
int
mca_coll_han_gather_intra_dynamic(const void *sbuf, int scount,
                                  struct ompi_datatype_t *sdtype,
                                  void *rbuf, int rcount,
                                  struct ompi_datatype_t *rdtype,
                                  int root,
                                  struct ompi_communicator_t *comm,
                                  mca_coll_base_module_t *module)
{
    mca_coll_han_module_t *han_module = (mca_coll_han_module_t*) module;
    TOPO_LVL_T topo_lvl = han_module->topologic_level;
    mca_coll_base_module_gather_fn_t gather;
    mca_coll_base_module_t *sub_module;
    size_t dtype_size;
    int rank, verbosity = 0;

    if (!han_module->enabled) {
        return han_module->previous_gather(sbuf, scount, sdtype, rbuf, rcount, rdtype, root, comm,
                                           han_module->previous_gather_module);
    }

    /* Compute configuration information for dynamic rules */
    if( MPI_IN_PLACE != sbuf ) {
        ompi_datatype_type_size(sdtype, &dtype_size);
        dtype_size = dtype_size * scount;
    } else {
        ompi_datatype_type_size(rdtype, &dtype_size);
        dtype_size = dtype_size * rcount;
    }

    sub_module = get_module(GATHER,
                            dtype_size,
                            comm,
                            han_module);

    /* First errors are always printed by rank 0 */
    rank = ompi_comm_rank(comm);
    if( (0 == rank) && (han_module->dynamic_errors < mca_coll_han_component.max_dynamic_errors) ) {
        verbosity = 30;
    }

    if(NULL == sub_module) {
        /*
         * No valid collective module from dynamic rules
         * nor from mca parameter
         */
        han_module->dynamic_errors++;
        opal_output_verbose(verbosity, mca_coll_han_component.han_output,
                            "coll:han:mca_coll_han_gather_intra_dynamic "
                            "HAN did not find any valid module for collective %d (%s) "
                            "with topological level %d (%s) on communicator (%s/%s). "
                            "Please check dynamic file/mca parameters\n",
                            GATHER, mca_coll_base_colltype_to_str(GATHER),
                            topo_lvl, mca_coll_han_topo_lvl_to_str(topo_lvl),
                            ompi_comm_print_cid(comm), comm->c_name);
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                             "HAN/GATHER: No module found for the sub-communicator. "
                             "Falling back to another component\n"));
        gather = han_module->previous_gather;
        sub_module = han_module->previous_gather_module;
    } else if (NULL == sub_module->coll_gather) {
        /*
         * No valid collective from dynamic rules
         * nor from mca parameter
         */
        han_module->dynamic_errors++;
        opal_output_verbose(verbosity, mca_coll_han_component.han_output,
                            "coll:han:mca_coll_han_gather_intra_dynamic "
                            "HAN found valid module for collective %d (%s) "
                            "with topological level %d (%s) on communicator (%s/%s) "
                            "but this module cannot handle this collective. "
                            "Please check dynamic file/mca parameters\n",
                            GATHER, mca_coll_base_colltype_to_str(GATHER),
                            topo_lvl, mca_coll_han_topo_lvl_to_str(topo_lvl),
                            ompi_comm_print_cid(comm), comm->c_name);
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                             "HAN/GATHER: the module found for the sub-"
                             "communicator cannot handle the GATHER operation. "
                             "Falling back to another component\n"));
        gather = han_module->previous_gather;
        sub_module = han_module->previous_gather_module;
    } else if (GLOBAL_COMMUNICATOR == topo_lvl && sub_module == module) {
        /*
         * No fallback mechanism activated for this configuration
         * sub_module is valid
         * sub_module->coll_gather is valid and point to this function
         * Call han topological collective algorithm
         */
        int algorithm_id = get_algorithm(GATHER,
                                         dtype_size,
                                         comm,
                                         han_module);
        gather = (mca_coll_base_module_gather_fn_t) mca_coll_han_algorithm_id_to_fn(GATHER, algorithm_id);
        if (NULL == gather) { /* default behaviour */
            if(mca_coll_han_component.use_simple_algorithm[GATHER]) {
                gather = mca_coll_han_gather_intra_simple;
            } else {
                gather = mca_coll_han_gather_intra;
            }
        }
    } else {
        /*
         * If we get here:
         * sub_module is valid
         * sub_module->coll_gather is valid
         * They points to the collective to use, according to the dynamic rules
         * Selector's job is done, call the collective
         */
        gather = sub_module->coll_gather;
    }
    return gather(sbuf, scount, sdtype,
                  rbuf, rcount, rdtype,
                  root, comm,
                  sub_module);
}

/*
 * Gatherv selector:
 * On a sub-communicator, checks the stored rules to find the module to use
 * On the global communicator, calls the han collective implementation, or
 * calls the correct module if fallback mechanism is activated
 */
int mca_coll_han_gatherv_intra_dynamic(const void *sbuf, int scount, struct ompi_datatype_t *sdtype,
                                       void *rbuf, const int *rcounts, const int *displs,
                                       struct ompi_datatype_t *rdtype, int root,
                                       struct ompi_communicator_t *comm,
                                       mca_coll_base_module_t *module)
{
    mca_coll_han_module_t *han_module = (mca_coll_han_module_t*) module;
    TOPO_LVL_T topo_lvl = han_module->topologic_level;
    mca_coll_base_module_gatherv_fn_t gatherv;
    mca_coll_base_module_t *sub_module;
    int rank, verbosity = 0;

    if (!han_module->enabled) {
        return han_module->previous_gatherv(sbuf, scount, sdtype, rbuf, rcounts, displs, rdtype,
                                            root, comm, han_module->previous_gatherv_module);
    }

    /* v collectives do not support message-size based dynamic rules */
    sub_module = get_module(GATHERV, MCA_COLL_HAN_ANY_MESSAGE_SIZE, comm, han_module);

    /* First errors are always printed by rank 0 */
    rank = ompi_comm_rank(comm);
    if( (0 == rank) && (han_module->dynamic_errors < mca_coll_han_component.max_dynamic_errors) ) {
        verbosity = 30;
    }

    if(NULL == sub_module) {
        /*
         * No valid collective module from dynamic rules
         * nor from mca parameter
         */
        han_module->dynamic_errors++;
        opal_output_verbose(verbosity, mca_coll_han_component.han_output,
                            "coll:han:mca_coll_han_gatherv_intra_dynamic "
                            "HAN did not find any valid module for collective %d (%s) "
                            "with topological level %d (%s) on communicator (%s/%s). "
                            "Please check dynamic file/mca parameters\n",
                            GATHERV, mca_coll_base_colltype_to_str(GATHERV),
                            topo_lvl, mca_coll_han_topo_lvl_to_str(topo_lvl),
                            ompi_comm_print_cid(comm), comm->c_name);
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                             "HAN/GATHERV: No module found for the sub-communicator. "
                             "Falling back to another component\n"));
        gatherv = han_module->previous_gatherv;
        sub_module = han_module->previous_gatherv_module;
    } else if (NULL == sub_module->coll_gatherv) {
        /*
         * No valid collective from dynamic rules
         * nor from mca parameter
         */
        han_module->dynamic_errors++;
        opal_output_verbose(verbosity, mca_coll_han_component.han_output,
                            "coll:han:mca_coll_han_gatherv_intra_dynamic "
                            "HAN found valid module for collective %d (%s) "
                            "with topological level %d (%s) on communicator (%s/%s) "
                            "but this module cannot handle this collective. "
                            "Please check dynamic file/mca parameters\n",
                            GATHERV, mca_coll_base_colltype_to_str(GATHERV),
                            topo_lvl, mca_coll_han_topo_lvl_to_str(topo_lvl),
                            ompi_comm_print_cid(comm), comm->c_name);
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                             "HAN/GATHERV: the module found for the sub-"
                             "communicator cannot handle the GATHERV operation. "
                             "Falling back to another component\n"));
        gatherv = han_module->previous_gatherv;
        sub_module = han_module->previous_gatherv_module;
    } else if (GLOBAL_COMMUNICATOR == topo_lvl && sub_module == module) {
        /*
         * No fallback mechanism activated for this configuration
         * sub_module is valid
         * sub_module->coll_gatherv is valid and point to this function
         * Call han topological collective algorithm
         */
        int algorithm_id = get_algorithm(GATHERV, MCA_COLL_HAN_ANY_MESSAGE_SIZE, comm, han_module);
        gatherv = (mca_coll_base_module_gatherv_fn_t) mca_coll_han_algorithm_id_to_fn(GATHERV, algorithm_id);
        if (NULL == gatherv) { /* default behaviour */
            gatherv = mca_coll_han_gatherv_intra;
        }
    } else {
        /*
         * If we get here:
         * sub_module is valid
         * sub_module->coll_gatherv is valid
         * They points to the collective to use, according to the dynamic rules
         * Selector's job is done, call the collective
         */
        gatherv = sub_module->coll_gatherv;
    }
    return gatherv(sbuf, scount, sdtype, rbuf, rcounts, displs, rdtype, root, comm, sub_module);
}


/*
 * Reduce selector:
 * On a sub-communicator, checks the stored rules to find the module to use
 * On the global communicator, calls the han collective implementation, or
 * calls the correct module if fallback mechanism is activated
 */
int
mca_coll_han_reduce_intra_dynamic(const void *sbuf,
                                  void *rbuf,
                                  int count,
                                  struct ompi_datatype_t *dtype,
                                  struct ompi_op_t *op,
                                  int root,
                                  struct ompi_communicator_t *comm,
                                  mca_coll_base_module_t *module)
{
    mca_coll_han_module_t *han_module = (mca_coll_han_module_t*) module;
    TOPO_LVL_T topo_lvl = han_module->topologic_level;
    mca_coll_base_module_reduce_fn_t reduce;
    mca_coll_base_module_t *sub_module;
    size_t dtype_size;
    int rank, verbosity = 0;

    if (!han_module->enabled) {
        return han_module->previous_reduce(sbuf, rbuf, count, dtype, op, root, comm,
                                           han_module->previous_reduce_module);
    }

    /* Compute configuration information for dynamic rules */
    ompi_datatype_type_size(dtype, &dtype_size);
    dtype_size = dtype_size * count;

    sub_module = get_module(REDUCE,
                            dtype_size,
                            comm,
                            han_module);

    /* First errors are always printed by rank 0 */
    rank = ompi_comm_rank(comm);
    if( (0 == rank) && (han_module->dynamic_errors < mca_coll_han_component.max_dynamic_errors) ) {
        verbosity = 30;
    }

    if(NULL == sub_module) {
        /*
         * No valid collective module from dynamic rules
         * nor from mca parameter
         */
        han_module->dynamic_errors++;
        opal_output_verbose(verbosity, mca_coll_han_component.han_output,
                            "coll:han:mca_coll_han_reduce_intra_dynamic "
                            "HAN did not find any valid module for collective %d (%s) "
                            "with topological level %d (%s) on communicator (%s/%s). "
                            "Please check dynamic file/mca parameters\n",
                            REDUCE, mca_coll_base_colltype_to_str(REDUCE),
                            topo_lvl, mca_coll_han_topo_lvl_to_str(topo_lvl),
                            ompi_comm_print_cid(comm), comm->c_name);
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                             "HAN/REDUCE: No module found for the sub-communicator. "
                             "Falling back to another component\n"));
        reduce = han_module->previous_reduce;
        sub_module = han_module->previous_reduce_module;
    } else if (NULL == sub_module->coll_reduce) {
        /*
         * No valid collective from dynamic rules
         * nor from mca parameter
         */
        han_module->dynamic_errors++;
        opal_output_verbose(verbosity, mca_coll_han_component.han_output,
                            "coll:han:mca_coll_han_reduce_intra_dynamic "
                            "HAN found valid module for collective %d (%s) "
                            "with topological level %d (%s) on communicator (%s/%s) "
                            "but this module cannot handle this collective. "
                            "Please check dynamic file/mca parameters\n",
                            REDUCE, mca_coll_base_colltype_to_str(REDUCE),
                            topo_lvl, mca_coll_han_topo_lvl_to_str(topo_lvl),
                            ompi_comm_print_cid(comm), comm->c_name);
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                             "HAN/REDUCE: the module found for the sub-"
                             "communicator cannot handle the REDUCE operation. "
                             "Falling back to another component\n"));
        reduce = han_module->previous_reduce;
        sub_module = han_module->previous_reduce_module;
    } else if (GLOBAL_COMMUNICATOR == topo_lvl && sub_module == module) {
        /* Reproducibility: fallback on reproducible algorithm */
        if (mca_coll_han_component.han_reproducible) {
            reduce = mca_coll_han_reduce_reproducible;
        } else {
            /*
             * No fallback mechanism activated for this configuration
             * sub_module is valid
             * sub_module->coll_reduce is valid and point to this function
             * Call han topological collective algorithm
             */
            int algorithm_id = get_algorithm(REDUCE,
                                             dtype_size,
                                             comm,
                                             han_module);
            reduce = (mca_coll_base_module_reduce_fn_t)mca_coll_han_algorithm_id_to_fn(REDUCE, algorithm_id);
            if (NULL == reduce) { /* default behaviour */
                if(mca_coll_han_component.use_simple_algorithm[REDUCE]) {
                    reduce = mca_coll_han_reduce_intra_simple;
                } else {
                    reduce = mca_coll_han_reduce_intra;
                }
            }
        }
    } else {
        /*
         * If we get here:
         * sub_module is valid
         * sub_module->coll_reduce is valid
         * They points to the collective to use, according to the dynamic rules
         * Selector's job is done, call the collective
         */
        reduce = sub_module->coll_reduce;
    }
    return reduce(sbuf, rbuf, count, dtype,
                  op, root, comm, sub_module);
}


/*
 * Scatter selector:
 * On a sub-communicator, checks the stored rules to find the module to use
 * On the global communicator, calls the han collective implementation, or
 * calls the correct module if fallback mechanism is activated
 */
int
mca_coll_han_scatter_intra_dynamic(const void *sbuf, int scount,
                                   struct ompi_datatype_t *sdtype,
                                   void *rbuf, int rcount,
                                   struct ompi_datatype_t *rdtype,
                                   int root,
                                   struct ompi_communicator_t *comm,
                                   mca_coll_base_module_t *module)
{
    mca_coll_han_module_t *han_module = (mca_coll_han_module_t*) module;
    TOPO_LVL_T topo_lvl = han_module->topologic_level;
    mca_coll_base_module_scatter_fn_t scatter;
    mca_coll_base_module_t *sub_module;
    size_t dtype_size;
    int rank, verbosity = 0;

    if (!han_module->enabled) {
        return han_module->previous_scatter(sbuf, scount, sdtype, rbuf, rcount, rdtype, root, comm,
                                            han_module->previous_scatter_module);
    }

    /* Compute configuration information for dynamic rules */
    if( MPI_IN_PLACE != rbuf ) {
        ompi_datatype_type_size(rdtype, &dtype_size);
        dtype_size = dtype_size * rcount;
    } else {
        ompi_datatype_type_size(sdtype, &dtype_size);
        dtype_size = dtype_size * scount;
    }

    sub_module = get_module(SCATTER,
                            dtype_size,
                            comm,
                            han_module);

    /* First errors are always printed by rank 0 */
    rank = ompi_comm_rank(comm);
    if( (0 == rank) && (han_module->dynamic_errors < mca_coll_han_component.max_dynamic_errors) ) {
        verbosity = 30;
    }

    if(NULL == sub_module) {
        /*
         * No valid collective module from dynamic rules
         * nor from mca parameter
         */
        han_module->dynamic_errors++;
        opal_output_verbose(verbosity, mca_coll_han_component.han_output,
                            "coll:han:mca_coll_han_scatter_intra_dynamic "
                            "HAN did not find any valid module for collective %d (%s) "
                            "with topological level %d (%s) on communicator (%s/%s). "
                            "Please check dynamic file/mca parameters\n",
                            SCATTER, mca_coll_base_colltype_to_str(SCATTER),
                            topo_lvl, mca_coll_han_topo_lvl_to_str(topo_lvl),
                            ompi_comm_print_cid(comm), comm->c_name);
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                             "HAN/SCATTER: No module found for the sub-communicator. "
                             "Falling back to another component\n"));
        scatter = han_module->previous_scatter;
        sub_module = han_module->previous_scatter_module;
    } else if (NULL == sub_module->coll_scatter) {
        /*
         * No valid collective from dynamic rules
         * nor from mca parameter
         */
        han_module->dynamic_errors++;
        opal_output_verbose(verbosity, mca_coll_han_component.han_output,
                            "coll:han:mca_coll_han_scatter_intra_dynamic "
                            "HAN found valid module for collective %d (%s) "
                            "with topological level %d (%s) on communicator (%s/%s) "
                            "but this module cannot handle this collective. "
                            "Please check dynamic file/mca parameters\n",
                            SCATTER, mca_coll_base_colltype_to_str(SCATTER),
                            topo_lvl, mca_coll_han_topo_lvl_to_str(topo_lvl),
                            ompi_comm_print_cid(comm), comm->c_name);
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                             "HAN/SCATTER: the module found for the sub-"
                             "communicator cannot handle the SCATTER operation. "
                             "Falling back to another component\n"));
        scatter = han_module->previous_scatter;
        sub_module = han_module->previous_scatter_module;
    } else if (GLOBAL_COMMUNICATOR == topo_lvl && sub_module == module) {
        /*
         * No fallback mechanism activated for this configuration
         * sub_module is valid
         * sub_module->coll_scatter is valid and point to this function
         * Call han topological collective algorithm
         */
        int algorithm_id = get_algorithm(SCATTER,
                                         dtype_size,
                                         comm,
                                         han_module);
        scatter = (mca_coll_base_module_scatter_fn_t)mca_coll_han_algorithm_id_to_fn(SCATTER, algorithm_id);
        if (NULL == scatter) { /* default behaviour */
            if(mca_coll_han_component.use_simple_algorithm[SCATTER]) {
                scatter = mca_coll_han_scatter_intra_simple;
            } else {
                scatter = mca_coll_han_scatter_intra;
            }
        }
    } else {
        /*
         * If we get here:
         * sub_module is valid
         * sub_module->coll_scatter is valid
         * They points to the collective to use, according to the dynamic rules
         * Selector's job is done, call the collective
         */
        scatter = sub_module->coll_scatter;
    }

    /*
     * If we get here:
     * sub_module is valid
     * sub_module->coll_scatter is valid
     * They points to the collective to use, according to the dynamic rules
     * Selector's job is done, call the collective
     */
    return scatter(sbuf, scount, sdtype,
                   rbuf, rcount, rdtype,
                   root, comm,
                   sub_module);
}
