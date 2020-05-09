/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2020      Bull S.A.S. All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal/class/opal_list.h"
#include "ompi/mca/coll/han/coll_han.h"
#include "ompi/mca/coll/han/coll_han_dynamic.h"
#include "ompi/mca/coll/base/coll_base_util.h"

/*
 * Tests if a dynamic collective is implemented
 * Usefull for file reading warnings and MCA parameter generation
 * When a new dynamic collective is implemented, this function must
 * return true for it
 */
bool mca_coll_han_is_coll_dynamic_implemented(COLLTYPE_T coll_id)
{
    switch (coll_id){
        case ALLGATHER:
        case ALLGATHERV:
        case ALLREDUCE:
        case BCAST:
        case GATHER:
        case REDUCE:
        case SCATTER:
            return true;
        default:
            return false;
    }
}

static COMPONENT_T
component_name_to_id(const char* name)
{
    int i;

    if(NULL == name) {
        return -1;
    }

    for(i=SELF ; i<COMPONENTS_COUNT ; i++) {
        if (0 == strcmp(name, components_name[i])) {
            return i;
        }
    }
    return -1;
}

/*
 * Get all the collective modules initialized on this communicator
 * This function must be call at the start of every selector implementation
 * Note that han module may be not yet enabled
 */
int
mca_coll_han_get_all_coll_modules(struct ompi_communicator_t *comm,
                     mca_coll_han_module_t *han_module)
{
    int nb_modules=0;
    mca_coll_base_avail_coll_t *item;
    TOPO_LVL_T topo_lvl = han_module->topologic_level;
    mca_coll_base_module_t *han_base_module = (mca_coll_base_module_t *) han_module;
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
        int id = component_name_to_id(name);

        if(id >= 0 && NULL != module && module != han_base_module) {
            /*
             * The identifier is correct
             * Store the module
             */
            han_module->modules_storage.modules[id].module_handler = module;
            opal_output_verbose(80, mca_coll_han_component.han_output,
                                "coll:han:get_all_coll_modules "
                                "Han found module %s with id %d "
                                "for topological level %d (%s) "
                                "for communicator (%d/%s)\n",
                                name,
                                id,
                                topo_lvl,
                                mca_coll_han_topo_lvl_to_str(topo_lvl),
                                comm->c_contextid,
                                comm->c_name);
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
                        "coll:han:get_all_coll_modules "
                        "Han sub-communicator modules storage "
                        "for topological level %d (%s) "
                        "gets %d modules "
                        "for communicator (%d/%s)\n",
                        topo_lvl,
                        mca_coll_han_topo_lvl_to_str(topo_lvl),
                        nb_modules,
                        comm->c_contextid,
                        comm->c_name);

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
                int msg_size,
                struct ompi_communicator_t *comm,
                mca_coll_han_module_t *han_module)
{
    /* Indexes of the rule */
    int coll_idx;
    int topo_idx;
    int conf_idx;
    int msg_size_idx;

    /* Aliases */
    const mca_coll_han_dynamic_rules_t *dynamic_rules = NULL;
    const collective_rule_t *coll_rule = NULL;
    const topologic_rule_t *topo_rule = NULL;
    const configuration_rule_t *conf_rule = NULL;
    const msg_size_rule_t *msg_size_rule = NULL;

    const TOPO_LVL_T topo_lvl = han_module->topologic_level;
    const int comm_size = ompi_comm_size(comm);

    COMPONENT_T component;

    /* Find the collective rule */
    dynamic_rules = &(mca_coll_han_component.dynamic_rules);
    for(coll_idx = dynamic_rules->nb_collectives-1 ;
        coll_idx >= 0 ; coll_idx--) {
        if(dynamic_rules->collective_rules[coll_idx].collective_id == collective) {
            coll_rule = &(dynamic_rules->collective_rules[coll_idx]);
            break;
        }
    }
    if(coll_idx < 0) {
        /*
         * No dynamic rules for this collective
         */
        opal_output_verbose(60, mca_coll_han_component.han_output,
                            "coll:han:get_dynamic_rule "
                            "Han searched for collective %d (%s) "
                            "but did not find any rule for this collective\n",
                            collective,
                            mca_coll_han_colltype_to_str(collective));
        return NULL;
    }

    /* Find the topologic level rule */
    for(topo_idx = coll_rule->nb_topologic_levels-1 ;
        topo_idx >= 0 ; topo_idx--) {
        if(coll_rule->topologic_rules[topo_idx].topologic_level == topo_lvl) {
            topo_rule = &(coll_rule->topologic_rules[topo_idx]);
            break;
        }
    }
    if(topo_idx < 0) {
        /*
         * No topologic level rules for this collective
         */
        opal_output_verbose(60, mca_coll_han_component.han_output,
                            "coll:han:get_dynamic_rule "
                            "Han searched for topologic level %d (%s) rule "
                            "for collective %d (%s) but did not find any rule\n",
                            topo_lvl,
                            mca_coll_han_topo_lvl_to_str(topo_lvl),
                            collective,
                            mca_coll_han_colltype_to_str(collective));
        return NULL;
    }

    /* Find the configuration rule */
    for(conf_idx = topo_rule->nb_rules-1 ;
        conf_idx >= 0 ; conf_idx--) {
        if(topo_rule->configuration_rules[conf_idx].configuration_size <= comm_size) {
            conf_rule = &(topo_rule->configuration_rules[conf_idx]);
            break;
        }
    }
    if(conf_idx < 0) {
        /*
         * No corresponding configuration
         * Should not happen with a correct file
         */

        opal_output_verbose(60, mca_coll_han_component.han_output,
                            "coll:han:get_dynamic_rule "
                            "Han searched a rule for collective %d (%s) "
                            "on topological level %d (%s) "
                            "for a %d configuration size "
                            "but did not manage to find anything. "
                            "This is the result of an invalid configuration file: "
                            "the first configuration size of each collective must be 1\n",
                            collective,
                            mca_coll_han_colltype_to_str(collective),
                            topo_lvl,
                            mca_coll_han_topo_lvl_to_str(topo_lvl),
                            comm_size);
        return NULL;
    }

    /* Find the message size rule */
    for(msg_size_idx = conf_rule->nb_msg_size-1 ;
        msg_size_idx >= 0 ; msg_size_idx--) {
        if(conf_rule->msg_size_rules[msg_size_idx].msg_size <= msg_size) {
            msg_size_rule = &(conf_rule->msg_size_rules[msg_size_idx]);
            break;
        }
    }
    if(msg_size_idx < 0) {
        /*
         * No corresponding message size
         * Should not happen with a correct file
         */
        opal_output_verbose(60, mca_coll_han_component.han_output,
                            "coll:han:get_dynamic_rule "
                            "Han searched a rule for collective %d (%s) "
                            "on topological level %d (%s) "
                            "for a %d configuration size "
                            "for a %d sized message "
                            "but did not manage to find anything. "
                            "This is the result of an invalid configuration file: "
                            "the first message size of each configuration must be 0\n",
                            collective,
                            mca_coll_han_colltype_to_str(collective),
                            topo_lvl,
                            mca_coll_han_topo_lvl_to_str(topo_lvl),
                            comm_size,
                            msg_size);

        return NULL;
    }

    component = msg_size_rule->component;
    /*
     * We have the final rule to use
     * Module correctness is checked outside
     */
    opal_output_verbose(80, mca_coll_han_component.han_output,
                        "coll:han:get_dynamic_rule "
                        "Han searched a rule for collective %d (%s) "
                        "on topological level %d (%s) "
                        "for a %d configuration size "
                        "for a %d sized message. "
                        "Found a rule for collective %d (%s) "
                        "on topological level %d (%s) "
                        "for a %d configuration size "
                        "for a %d sized message : component %d (%s)\n",
                        collective,
                        mca_coll_han_colltype_to_str(collective),
                        topo_lvl,
                        mca_coll_han_topo_lvl_to_str(topo_lvl),
                        comm_size,
                        msg_size,
                        msg_size_rule->collective_id,
                        mca_coll_han_colltype_to_str(msg_size_rule->collective_id),
                        msg_size_rule->topologic_level,
                        mca_coll_han_topo_lvl_to_str(msg_size_rule->topologic_level),
                        msg_size_rule->configuration_size,
                        msg_size_rule->msg_size,
                        component,
                        components_name[component]);

    return msg_size_rule;
}

/*
 * Return the module to use for the collective coll_id
 * for a msg_size sized message on the comm communicator
 * following the dynamic rules
 */
mca_coll_base_module_t *
get_module(COLLTYPE_T coll_id,
           int msg_size,
           struct ompi_communicator_t *comm,
           mca_coll_han_module_t *han_module)
{
    const msg_size_rule_t *dynamic_rule;
    mca_coll_base_module_t *sub_module = NULL;
    TOPO_LVL_T topo_lvl;
    COMPONENT_T mca_rule_component;

    topo_lvl = han_module->topologic_level;
    mca_rule_component = mca_coll_han_component.mca_rules[coll_id][topo_lvl];

    mca_coll_han_get_all_coll_modules(comm, han_module);

    /* Find the correct dynamic rule to check */
    dynamic_rule = get_dynamic_rule(coll_id,
                                    msg_size,
                                    comm,
                                    han_module);
    if(NULL != dynamic_rule) {
        /* Use dynamic rule from file */
        sub_module = han_module->modules_storage
                                   .modules[dynamic_rule->component]
                                   .module_handler;
    } else {
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
                                "coll:han:get_module "
                                "Invalid MCA parameter value %d "
                                "for collective %d (%s) "
                                "on topologic level %d (%s)\n",
                                mca_rule_component,
                                coll_id,
                                mca_coll_han_colltype_to_str(coll_id),
                                topo_lvl,
                                mca_coll_han_topo_lvl_to_str(topo_lvl));
            return NULL;
        }
        sub_module = han_module->modules_storage
                                   .modules[mca_rule_component]
                                   .module_handler;
    }

    return sub_module;
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
    size_t dtype_size;
    int msg_size;
    int rank;
    int verbosity;
    mca_coll_han_module_t *han_module = (mca_coll_han_module_t*) module;
    mca_coll_base_module_t *sub_module;
    TOPO_LVL_T topo_lvl;

    topo_lvl = han_module->topologic_level;

    /* Compute configuration information for dynamic rules */
    ompi_datatype_type_size(sdtype, &dtype_size);
    msg_size = dtype_size * scount;

    sub_module = get_module(ALLGATHER,
                            msg_size,
                            comm,
                            han_module);

    /* First errors are always printed by rank 0 */
    rank = ompi_comm_rank(comm);
    if(0 == rank
       && han_module->dynamic_errors
          < mca_coll_han_component.max_dynamic_errors) {
        verbosity = 0;
    } else {
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
                            "Han did not find any valid module for "
                            "collective %d (%s) "
                            "with topological level %d (%s) "
                            "on communicator (%d/%s). "
                            "Please check dynamic file/mca parameters\n",
                            ALLGATHER,
                            mca_coll_han_colltype_to_str(ALLGATHER),
                            topo_lvl,
                            mca_coll_han_topo_lvl_to_str(topo_lvl),
                            comm->c_contextid,
                            comm->c_name);
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                             "HAN/ALLGATHER: No module found for the sub-"
                             "communicator. "
                             "Falling back to another component\n"));
        return han_module->previous_allgather(sbuf, scount, sdtype,
                                                 rbuf, rcount, rdtype,
                                                 comm,
                                                 han_module
                                                 ->previous_allgather_module);
    } else if (NULL == sub_module->coll_allgather) {
        /*
         * No valid collective from dynamic rules
         * nor from mca parameter
         */
        han_module->dynamic_errors++;
        opal_output_verbose(verbosity, mca_coll_han_component.han_output,
                            "coll:han:mca_coll_han_allgather_intra_dynamic "
                            "Han found valid module for "
                            "collective %d (%s) "
                            "with topological level %d (%s) "
                            "on communicator (%d/%s) "
                            "but this module cannot handle "
                            "this collective. "
                            "Please check dynamic file/mca parameters\n",
                            ALLGATHER,
                            mca_coll_han_colltype_to_str(ALLGATHER),
                            topo_lvl,
                            mca_coll_han_topo_lvl_to_str(topo_lvl),
                            comm->c_contextid,
                            comm->c_name);
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                             "HAN/ALLGATHER: the module found for the sub-"
                             "communicator cannot handle the ALLGATHER operation. "
                             "Falling back to another component\n"));
        return han_module->previous_allgather(sbuf, scount, sdtype,
                                                 rbuf, rcount, rdtype,
                                                 comm,
                                                 han_module
                                                 ->previous_allgather_module);
    }

    if (GLOBAL_COMMUNICATOR == topo_lvl && sub_module == module) {
        /*
         * No fallback mechanism activated for this configuration
         * sub_module is valid
         * sub_module->coll_allgather is valid and point to this function
         * Call han topological collective algorithm
         */
        mca_coll_base_module_allgather_fn_t allgather;
        if(mca_coll_han_component.use_simple_algorithm[ALLGATHER]) {
            allgather = mca_coll_han_allgather_intra_simple;
        } else {
            allgather = mca_coll_han_allgather_intra;
        }

        return allgather(sbuf, scount, sdtype,
                         rbuf, rcount, rdtype,
                         comm,
                         sub_module);
    }

    /*
     * If we get here:
     * sub_module is valid
     * sub_module->coll_allgather is valid
     * They points to the collective to use, according to the dynamic rules
     * Selector's job is done, call the collective
     */
    return sub_module->coll_allgather(sbuf, scount, sdtype,
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
    size_t dtype_size, msg_size;
    int rank;
    int verbosity;
    int comm_size;
    int i;
    mca_coll_han_module_t *han_module = (mca_coll_han_module_t*) module;
    mca_coll_base_module_t *sub_module;
    TOPO_LVL_T topo_lvl;

    topo_lvl = han_module->topologic_level;

    /* Compute configuration information for dynamic rules */
    comm_size = ompi_comm_size(comm);
    ompi_datatype_type_size(rdtype, &dtype_size);

    msg_size = 0;
    for(i = 0 ; i < comm_size ; i++) {
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
    if(0 == rank
       && han_module->dynamic_errors
          < mca_coll_han_component.max_dynamic_errors) {
        verbosity = 0;
    } else {
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
                            "Han did not find any valid module for "
                            "collective %d (%s) "
                            "with topological level %d (%s) "
                            "on communicator (%d/%s). "
                            "Please check dynamic file/mca parameters\n",
                            ALLGATHERV,
                            mca_coll_han_colltype_to_str(ALLGATHERV),
                            topo_lvl,
                            mca_coll_han_topo_lvl_to_str(topo_lvl),
                            comm->c_contextid,
                            comm->c_name);
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                             "HAN/ALLGATHERV: No module found for the sub-"
                             "communicator. "
                             "Falling back to another component\n"));
        return han_module->previous_allgatherv(sbuf, scount, sdtype,
                                                 rbuf, rcounts, displs,
                                                 rdtype, comm,
                                                 han_module
                                                 ->previous_allgatherv_module);
    } else if (NULL == sub_module->coll_allgatherv) {
        /*
         * No valid collective from dynamic rules
         * nor from mca parameter
         */
        han_module->dynamic_errors++;
        opal_output_verbose(verbosity, mca_coll_han_component.han_output,
                            "coll:han:mca_coll_han_allgatherv_intra_dynamic "
                            "Han found valid module for "
                            "collective %d (%s) "
                            "with topological level %d (%s) "
                            "on communicator (%d/%s) "
                            "but this module cannot handle "
                            "this collective. "
                            "Please check dynamic file/mca parameters\n",
                            ALLGATHERV,
                            mca_coll_han_colltype_to_str(ALLGATHERV),
                            topo_lvl,
                            mca_coll_han_topo_lvl_to_str(topo_lvl),
                            comm->c_contextid,
                            comm->c_name);
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                             "HAN/ALLGATHERV: the module found for the sub-"
                             "communicator cannot handle the ALLGATHERV operation. "
                             "Falling back to another component\n"));
        return han_module->previous_allgatherv(sbuf, scount, sdtype,
                                                  rbuf, rcounts, displs,
                                                  rdtype, comm,
                                                  han_module
                                                  ->previous_allgatherv_module);
    }

    if (GLOBAL_COMMUNICATOR == topo_lvl && sub_module == module) {
        /*
         * No fallback mechanism activated for this configuration
         * sub_module is valid
         * sub_module->coll_allgatherv is valid and point to this function
         * Call han topological collective algorithm
         */
        opal_output_verbose(30, mca_coll_han_component.han_output,
                            "coll:han:mca_coll_han_allgatherv_intra_dynamic "
                            "Han used for "
                            "collective %d (%s) "
                            "with topological level %d (%s) "
                            "on communicator (%d/%s) "
                            "but this module cannot handle "
                            "this collective on this topologic level\n",
                            ALLGATHERV,
                            mca_coll_han_colltype_to_str(ALLGATHERV),
                            topo_lvl,
                            mca_coll_han_topo_lvl_to_str(topo_lvl),
                            comm->c_contextid,
                            comm->c_name);
        return han_module->previous_allgatherv(sbuf, scount, sdtype,
                                                  rbuf, rcounts, displs,
                                                  rdtype, comm,
                                                  han_module
                                                  ->previous_allgatherv_module);
    }

    /*
     * If we get here:
     * sub_module is valid
     * sub_module->coll_allgatherv is valid
     * They points to the collective to use, according to the dynamic rules
     * Selector's job is done, call the collective
     */
    return sub_module->coll_allgatherv(sbuf, scount, sdtype,
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
    size_t dtype_size;
    int msg_size;
    int rank;
    int verbosity;
    mca_coll_han_module_t *han_module = (mca_coll_han_module_t*) module;
    mca_coll_base_module_t *sub_module;
    TOPO_LVL_T topo_lvl;

    topo_lvl = han_module->topologic_level;

    /* Compute configuration information for dynamic rules */
    ompi_datatype_type_size(dtype, &dtype_size);
    msg_size = dtype_size * count;

    sub_module = get_module(ALLREDUCE,
                            msg_size,
                            comm,
                            han_module);

    /* First errors are always printed by rank 0 */
    rank = ompi_comm_rank(comm);
    if(0 == rank
       && han_module->dynamic_errors
          < mca_coll_han_component.max_dynamic_errors) {
        verbosity = 0;
    } else {
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
                            "Han did not find any valid module for "
                            "collective %d (%s) "
                            "with topological level %d (%s) "
                            "on communicator (%d/%s). "
                            "Please check dynamic file/mca parameters\n",
                            ALLREDUCE,
                            mca_coll_han_colltype_to_str(ALLREDUCE),
                            topo_lvl,
                            mca_coll_han_topo_lvl_to_str(topo_lvl),
                            comm->c_contextid,
                            comm->c_name);
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                             "HAN/ALLREDUCE: No module found for the sub-"
                             "communicator. "
                             "Falling back to another component\n"));
        return han_module->previous_allreduce(sbuf, rbuf, count, dtype,
                                                 op, comm,
                                                 han_module
                                                 ->previous_allreduce_module);
    } else if (NULL == sub_module->coll_allreduce) {
        /*
         * No valid collective from dynamic rules
         * nor from mca parameter
         */
        han_module->dynamic_errors++;
        opal_output_verbose(verbosity, mca_coll_han_component.han_output,
                            "coll:han:mca_coll_han_allreduce_intra_dynamic "
                            "Han found valid module for "
                            "collective %d (%s) "
                            "with topological level %d (%s) "
                            "on communicator (%d/%s) "
                            "but this module cannot handle "
                            "this collective. "
                            "Please check dynamic file/mca parameters\n",
                            ALLREDUCE,
                            mca_coll_han_colltype_to_str(ALLREDUCE),
                            topo_lvl,
                            mca_coll_han_topo_lvl_to_str(topo_lvl),
                            comm->c_contextid,
                            comm->c_name);
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                             "HAN/ALLREDUCE: the module found for the sub-"
                             "communicator cannot handle the ALLREDUCE operation. "
                             "Falling back to another component\n"));
        return han_module->previous_allreduce(sbuf, rbuf, count, dtype,
                                                 op, comm,
                                                 han_module
                                                 ->previous_allreduce_module);
    }

    if (GLOBAL_COMMUNICATOR == topo_lvl && sub_module == module) {
        /* Reproducibility: fallback on reproducible algo */
        if (mca_coll_han_component.han_reproducible) {
            return mca_coll_han_allreduce_reproducible(sbuf, rbuf, count, dtype, op,
                                                       comm, module);
        }
        /*
         * No fallback mechanism activated for this configuration
         * sub_module is valid
         * sub_module->coll_allreduce is valid and point to this function
         * Call han topological collective algorithm
         */
        mca_coll_base_module_allreduce_fn_t allreduce;
        if(mca_coll_han_component.use_simple_algorithm[ALLREDUCE]) {
            allreduce = mca_coll_han_allreduce_intra_simple;
        } else {
            allreduce = mca_coll_han_allreduce_intra;
        }
        return allreduce(sbuf, rbuf, count, dtype,
                         op, comm, module);
    }

    /*
     * If we get here:
     * sub_module is valid
     * sub_module->coll_allreduce is valid
     * They points to the collective to use, according to the dynamic rules
     * Selector's job is done, call the collective
     */
    return sub_module->coll_allreduce(sbuf, rbuf, count, dtype,
                                      op, comm, sub_module);
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
    size_t dtype_size;
    int msg_size;
    int rank;
    int verbosity;
    mca_coll_han_module_t *han_module = (mca_coll_han_module_t*) module;
    mca_coll_base_module_t *sub_module;
    TOPO_LVL_T topo_lvl;

    topo_lvl = han_module->topologic_level;

    /* Compute configuration information for dynamic rules */
    ompi_datatype_type_size(dtype, &dtype_size);
    msg_size = dtype_size * count;

    sub_module = get_module(BCAST,
                            msg_size,
                            comm,
                            han_module);

    /* First errors are always printed by rank 0 */
    rank = ompi_comm_rank(comm);
    if(0 == rank
       && han_module->dynamic_errors
          < mca_coll_han_component.max_dynamic_errors) {
        verbosity = 0;
    } else {
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
                            "Han did not find any valid module for "
                            "collective %d (%s) "
                            "with topological level %d (%s) "
                            "on communicator (%d/%s). "
                            "Please check dynamic file/mca parameters\n",
                            BCAST,
                            mca_coll_han_colltype_to_str(BCAST),
                            topo_lvl,
                            mca_coll_han_topo_lvl_to_str(topo_lvl),
                            comm->c_contextid,
                            comm->c_name);
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                             "HAN/BCAST: No module found for the sub-"
                             "communicator. "
                             "Falling back to another component\n"));
        return han_module->previous_bcast(buff, count, dtype, root, comm,
                                             han_module->previous_bcast_module);
    } else if (NULL == sub_module->coll_bcast) {
        /*
         * No valid collective from dynamic rules
         * nor from mca parameter
         */
        han_module->dynamic_errors++;
        opal_output_verbose(verbosity, mca_coll_han_component.han_output,
                            "coll:han:mca_coll_han_bcast_intra_dynamic "
                            "Han found valid module for "
                            "collective %d (%s) "
                            "with topological level %d (%s) "
                            "on communicator (%d/%s) "
                            "but this module cannot handle "
                            "this collective. "
                            "Please check dynamic file/mca parameters\n",
                            BCAST,
                            mca_coll_han_colltype_to_str(BCAST),
                            topo_lvl,
                            mca_coll_han_topo_lvl_to_str(topo_lvl),
                            comm->c_contextid,
                            comm->c_name);
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                             "HAN/BCAST: the module found for the sub-"
                             "communicator cannot handle the BCAST operation. "
                             "Falling back to another component\n"));
        return han_module->previous_bcast(buff, count, dtype, root, comm,
                                             han_module->previous_bcast_module);
    }

    if (GLOBAL_COMMUNICATOR == topo_lvl && sub_module == module) {
        /*
         * No fallback mechanism activated for this configuration
         * sub_module is valid
         * sub_module->coll_bcast is valid and point to this function
         * Call han topological collective algorithm
         */
        mca_coll_base_module_bcast_fn_t bcast;
        if(mca_coll_han_component.use_simple_algorithm[BCAST]) {
            bcast = mca_coll_han_bcast_intra_simple;
        } else {
            bcast = mca_coll_han_bcast_intra;
        }
        return bcast(buff,
                     count,
                     dtype,
                     root,
                     comm,
                     module);
    }

    /*
     * If we get here:
     * sub_module is valid
     * sub_module->coll_bcast is valid
     * They points to the collective to use, according to the dynamic rules
     * Selector's job is done, call the collective
     */
    return sub_module->coll_bcast(buff,
                                  count,
                                  dtype,
                                  root,
                                  comm,
                                  sub_module);
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
    size_t dtype_size;
    int msg_size;
    int rank;
    int verbosity;
    mca_coll_han_module_t *han_module = (mca_coll_han_module_t*) module;
    mca_coll_base_module_t *sub_module;
    TOPO_LVL_T topo_lvl;

    topo_lvl = han_module->topologic_level;

    /* Compute configuration information for dynamic rules */
    ompi_datatype_type_size(sdtype, &dtype_size);
    msg_size = dtype_size * scount;

    sub_module = get_module(GATHER,
                            msg_size,
                            comm,
                            han_module);

    /* First errors are always printed by rank 0 */
    rank = ompi_comm_rank(comm);
    if(0 == rank
       && han_module->dynamic_errors
          < mca_coll_han_component.max_dynamic_errors) {
        verbosity = 0;
    } else {
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
                            "Han did not find any valid module for "
                            "collective %d (%s) "
                            "with topological level %d (%s) "
                            "on communicator (%d/%s). "
                            "Please check dynamic file/mca parameters\n",
                            GATHER,
                            mca_coll_han_colltype_to_str(GATHER),
                            topo_lvl,
                            mca_coll_han_topo_lvl_to_str(topo_lvl),
                            comm->c_contextid,
                            comm->c_name);
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                             "HAN/GATHER: No module found for the sub-"
                             "communicator. "
                             "Falling back to another component\n"));
        return han_module->previous_gather(sbuf, scount, sdtype,
                                              rbuf, rcount, rdtype,
                                              root, comm,
                                              han_module
                                              ->previous_gather_module);
    } else if (NULL == sub_module->coll_gather) {
        /*
         * No valid collective from dynamic rules
         * nor from mca parameter
         */
        han_module->dynamic_errors++;
        opal_output_verbose(verbosity, mca_coll_han_component.han_output,
                            "coll:han:mca_coll_han_gather_intra_dynamic "
                            "Han found valid module for "
                            "collective %d (%s) "
                            "with topological level %d (%s) "
                            "on communicator (%d/%s) "
                            "but this module cannot handle "
                            "this collective. "
                            "Please check dynamic file/mca parameters\n",
                            GATHER,
                            mca_coll_han_colltype_to_str(GATHER),
                            topo_lvl,
                            mca_coll_han_topo_lvl_to_str(topo_lvl),
                            comm->c_contextid,
                            comm->c_name);
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                             "HAN/GATHER: the module found for the sub-"
                             "communicator cannot handle the GATHER operation. "
                             "Falling back to another component\n"));
        return han_module->previous_gather(sbuf, scount, sdtype,
                                              rbuf, rcount, rdtype,
                                              root, comm,
                                              han_module
                                              ->previous_gather_module);
    }

    if (GLOBAL_COMMUNICATOR == topo_lvl && sub_module == module) {
        /*
         * No fallback mechanism activated for this configuration
         * sub_module is valid
         * sub_module->coll_gather is valid and point to this function
         * Call han topological collective algorithm
         */
        mca_coll_base_module_gather_fn_t gather;
        if(mca_coll_han_component.use_simple_algorithm[GATHER]) {
            gather = mca_coll_han_gather_intra_simple;
        } else {
            gather = mca_coll_han_gather_intra;
        }


        return gather(sbuf, scount, sdtype,
                      rbuf, rcount, rdtype,
                      root, comm,
                      sub_module);
    }

    /*
     * If we get here:
     * sub_module is valid
     * sub_module->coll_gather is valid
     * They points to the collective to use, according to the dynamic rules
     * Selector's job is done, call the collective
     */
    return sub_module->coll_gather(sbuf, scount, sdtype,
                                   rbuf, rcount, rdtype,
                                   root, comm,
                                   sub_module);
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
    size_t dtype_size;
    int msg_size;
    int rank;
    int verbosity;
    mca_coll_han_module_t *han_module = (mca_coll_han_module_t*) module;
    mca_coll_base_module_t *sub_module;
    TOPO_LVL_T topo_lvl;

    topo_lvl = han_module->topologic_level;

    /* Compute configuration information for dynamic rules */
    ompi_datatype_type_size(dtype, &dtype_size);
    msg_size = dtype_size * count;

    sub_module = get_module(REDUCE,
                            msg_size,
                            comm,
                            han_module);

    /* First errors are always printed by rank 0 */
    rank = ompi_comm_rank(comm);
    if(0 == rank
       && han_module->dynamic_errors
          < mca_coll_han_component.max_dynamic_errors) {
        verbosity = 0;
    } else {
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
                            "Han did not find any valid module for "
                            "collective %d (%s) "
                            "with topological level %d (%s) "
                            "on communicator (%d/%s). "
                            "Please check dynamic file/mca parameters\n",
                            REDUCE,
                            mca_coll_han_colltype_to_str(REDUCE),
                            topo_lvl,
                            mca_coll_han_topo_lvl_to_str(topo_lvl),
                            comm->c_contextid,
                            comm->c_name);
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                             "HAN/REDUCE: No module found for the sub-"
                             "communicator. "
                             "Falling back to another component\n"));
        return han_module->previous_reduce(sbuf, rbuf, count, dtype,
                                              op, root, comm,
                                              han_module
                                              ->previous_reduce_module);
    } else if (NULL == sub_module->coll_reduce) {
        /*
         * No valid collective from dynamic rules
         * nor from mca parameter
         */
        han_module->dynamic_errors++;
        opal_output_verbose(verbosity, mca_coll_han_component.han_output,
                            "coll:han:mca_coll_han_reduce_intra_dynamic "
                            "Han found valid module for "
                            "collective %d (%s) "
                            "with topological level %d (%s) "
                            "on communicator (%d/%s) "
                            "but this module cannot handle "
                            "this collective. "
                            "Please check dynamic file/mca parameters\n",
                            REDUCE,
                            mca_coll_han_colltype_to_str(REDUCE),
                            topo_lvl,
                            mca_coll_han_topo_lvl_to_str(topo_lvl),
                            comm->c_contextid,
                            comm->c_name);
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                             "HAN/REDUCE: the module found for the sub-"
                             "communicator cannot handle the REDUCE operation. "
                             "Falling back to another component\n"));
        return han_module->previous_reduce(sbuf, rbuf, count, dtype,
                                              op, root, comm,
                                              han_module
                                              ->previous_reduce_module);
    }

    if (GLOBAL_COMMUNICATOR == topo_lvl && sub_module == module) {
        /* Reproducibility: fallback on reproducible algo */
        if (mca_coll_han_component.han_reproducible) {
            return mca_coll_han_reduce_reproducible(sbuf, rbuf, count, dtype, op,
                                                    root, comm, module);
        }
        /*
         * No fallback mechanism activated for this configuration
         * sub_module is valid
         * sub_module->coll_reduce is valid and point to this function
         * Call han topological collective algorithm
         */
        mca_coll_base_module_reduce_fn_t reduce;
        if(mca_coll_han_component.use_simple_algorithm[REDUCE]) {
            reduce = mca_coll_han_reduce_intra_simple;
        } else {
            reduce = mca_coll_han_reduce_intra;
        }
        return reduce(sbuf, rbuf, count, dtype,
                      op, root, comm, module);
    }

    /*
     * If we get here:
     * sub_module is valid
     * sub_module->coll_reduce is valid
     * They points to the collective to use, according to the dynamic rules
     * Selector's job is done, call the collective
     */
    return sub_module->coll_reduce(sbuf, rbuf, count, dtype,
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
    size_t dtype_size;
    int msg_size;
    int rank;
    int verbosity;
    mca_coll_han_module_t *han_module = (mca_coll_han_module_t*) module;
    mca_coll_base_module_t *sub_module;
    TOPO_LVL_T topo_lvl;

    topo_lvl = han_module->topologic_level;

    /* Compute configuration information for dynamic rules */
    ompi_datatype_type_size(rdtype, &dtype_size);
    msg_size = dtype_size * rcount;

    sub_module = get_module(SCATTER,
                            msg_size,
                            comm,
                            han_module);

    /* First errors are always printed by rank 0 */
    rank = ompi_comm_rank(comm);
    if(0 == rank
       && han_module->dynamic_errors
          < mca_coll_han_component.max_dynamic_errors) {
        verbosity = 0;
    } else {
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
                            "Han did not find any valid module for "
                            "collective %d (%s) "
                            "with topological level %d (%s) "
                            "on communicator (%d/%s). "
                            "Please check dynamic file/mca parameters\n",
                            SCATTER,
                            mca_coll_han_colltype_to_str(SCATTER),
                            topo_lvl,
                            mca_coll_han_topo_lvl_to_str(topo_lvl),
                            comm->c_contextid,
                            comm->c_name);
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                             "HAN/SCATTER: No module found for the sub-"
                             "communicator. "
                             "Falling back to another component\n"));
        return han_module->previous_scatter(sbuf, scount, sdtype,
                                               rbuf, rcount, rdtype,
                                               root, comm,
                                               han_module
                                               ->previous_scatter_module);
    } else if (NULL == sub_module->coll_scatter) {
        /*
         * No valid collective from dynamic rules
         * nor from mca parameter
         */
        han_module->dynamic_errors++;
        opal_output_verbose(verbosity, mca_coll_han_component.han_output,
                            "coll:han:mca_coll_han_scatter_intra_dynamic "
                            "Han found valid module for "
                            "collective %d (%s) "
                            "with topological level %d (%s) "
                            "on communicator (%d/%s) "
                            "but this module cannot handle "
                            "this collective. "
                            "Please check dynamic file/mca parameters\n",
                            SCATTER,
                            mca_coll_han_colltype_to_str(SCATTER),
                            topo_lvl,
                            mca_coll_han_topo_lvl_to_str(topo_lvl),
                            comm->c_contextid,
                            comm->c_name);
        OPAL_OUTPUT_VERBOSE((30, mca_coll_han_component.han_output,
                             "HAN/SCATTER: the module found for the sub-"
                             "communicator cannot handle the SCATTER operation. "
                             "Falling back to another component\n"));
        return han_module->previous_scatter(sbuf, scount, sdtype,
                                               rbuf, rcount, rdtype,
                                               root, comm,
                                               han_module
                                               ->previous_scatter_module);
    }

    if (GLOBAL_COMMUNICATOR == topo_lvl && sub_module == module) {
        /*
         * No fallback mechanism activated for this configuration
         * sub_module is valid
         * sub_module->coll_scatter is valid and point to this function
         * Call han topological collective algorithm
         */
        mca_coll_base_module_scatter_fn_t scatter;
        scatter = mca_coll_han_scatter_intra;
        /*
         * TODO: Uncomment when scatter simple is merged
         * if(mca_coll_han_component.use_simple_algorithm[SCATTER]) {
         * scatter = mca_coll_han_scatter_intra_simple;
         * } else {
         * scatter = mca_coll_han_scatter_intra;
         * }
         */
        return scatter(sbuf, scount, sdtype,
                       rbuf, rcount, rdtype,
                       root, comm,
                       sub_module);
    }

    /*
     * If we get here:
     * sub_module is valid
     * sub_module->coll_scatter is valid
     * They points to the collective to use, according to the dynamic rules
     * Selector's job is done, call the collective
     */
    return sub_module->coll_scatter(sbuf, scount, sdtype,
                                    rbuf, rcount, rdtype,
                                    root, comm,
                                    sub_module);
}


