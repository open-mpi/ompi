/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2018-2022 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2022      IBM Corporation. All rights reserved
 * Copyright (c) 2020-2022 Bull S.A.S. All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/*
 *@file
 * Implementation of configuration file parser to set collective components to use.
 */

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif

#include "ompi_config.h"
#include "coll_han.h"
#include "coll_han_dynamic.h"
#include "coll_han_dynamic_file.h"
#include "coll_han_algorithms.h"

#include "ompi/mca/coll/base/coll_base_util.h"

#define getnext_long(fptr, pval)     ompi_coll_base_file_getnext_long(fptr, &fileline, pval)
#define getnext_string(fptr, pval)   ompi_coll_base_file_getnext_string(fptr, &fileline, pval)
#define getnext_size_t(fptr, pval)   ompi_coll_base_file_getnext_size_t(fptr, &fileline, pval)

static void check_dynamic_rules(void);

/* Current file line for verbose message */
static int fileline = 1;

/*
 * File parsing function. Allocated memory depending on the number of rules.
 * This functions expects a file formatted as described in coll_han_dynamic_file.h.
 * The configuration is then used by coll/han component to determine which module to
 * use at each topological level.
 */
int
mca_coll_han_init_dynamic_rules(void)
{
    /* File management */
    const char *fname;
    FILE *fptr = NULL;
    int nb_entries = 0, rc;

    /* Loop counters */
    int i, j, k, l;

    /* Collective information */
    long nb_coll;
    COLLTYPE_T coll_id;
    int algorithm_id;
    char * coll_name = NULL;
    char * algorithm_name = NULL;
    char * target_comp_name = NULL;
    collective_rule_t *coll_rules;

    /* Topo information */
    long nb_topo, topo_lvl;
    topologic_rule_t *topo_rules;

    /* Configuration information */
    long nb_rules, conf_size;
    configuration_rule_t *conf_rules;

    /* Message size information */
    long nb_msg_size;
    size_t msg_size;
    msg_size_rule_t *msg_size_rules;

    /* Component information */
    long component;

    /* If the dynamic rules are not used, do not even read the file */
    if(!mca_coll_han_component.use_dynamic_file_rules) {
        return OMPI_SUCCESS;
    }

    if( NULL == (fname = mca_coll_han_component.dynamic_rules_filename) ) {
        opal_output_verbose(5, mca_coll_han_component.han_output,
                            "coll:han:mca_coll_han_init_dynamic_rules coll_han_use_dynamic_file_rules is set but "
                            "coll_han_dynamic_rules_filename is not Rules from MCA parameters will be used instead\n");
        mca_coll_han_component.dynamic_rules.nb_collectives = 0;
        return OMPI_SUCCESS;
    }

    fptr = fopen(fname, "r");
    if( NULL == fptr ) {
        opal_output_verbose(5, mca_coll_han_component.han_output,
                            "coll:han:mca_coll_han_init_dynamic_rules cannot open dynamic file provided by "
                            "coll_han_dynamic_rules_filename=%s. Make sure it provides the  full path and "
                            "check file permissions. Rules from MCA parameters will be used instead\n",
                            fname);
        mca_coll_han_component.dynamic_rules.nb_collectives = 0;
        return OMPI_SUCCESS;
    }

    /* The first information of the file is the collective count */
    if( (getnext_long(fptr, &nb_coll) < 0) || (nb_coll <= 0) ) {
        opal_output_verbose(5, mca_coll_han_component.han_output,
                            "coll:han:mca_coll_han_init_dynamic_rules found an error on dynamic rules file %s "
                            "at line %d: an invalid value %ld is given for collective count "
                            "or the reader encountered an unexpected EOF\n",
                            fname, fileline, nb_coll);
        mca_coll_han_component.dynamic_rules.nb_collectives = 0;
        goto file_reading_error;
    }

    mca_coll_han_component.dynamic_rules.nb_collectives = nb_coll;

    /* Allocate collective rules */
    coll_rules = malloc(nb_coll * sizeof(collective_rule_t));
    mca_coll_han_component.dynamic_rules.collective_rules = coll_rules;
    if(NULL == coll_rules) {
        mca_coll_han_component.dynamic_rules.nb_collectives = 0;
        goto cannot_allocate;
    }

    /* Iterates on collective rules */
    for( i = 0 ; i < nb_coll ; i++ ) {
        coll_rules[i].nb_topologic_levels = 0;
        coll_rules[i].topologic_rules = NULL;
        mca_coll_han_component.dynamic_rules.nb_collectives = i+1;

        /* Get the collective identifier */
        free(coll_name);
        if( getnext_string(fptr, &coll_name) < 0 ) {
            opal_output_verbose(5, mca_coll_han_component.han_output,
                                "coll:han:mca_coll_han_init_dynamic_rules invalid collective at line %d."
                                "The rest of the input file will be ignored.\n",
                                fileline);
            goto file_reading_error;
        }
        coll_id = mca_coll_base_name_to_colltype(coll_name);
        if( (coll_id < ALLGATHER) || (coll_id >= COLLCOUNT)) {
            /* maybe the file was in the old format and we read the collective index instead of the name. */
            char* endp;
            coll_id = strtol(coll_name, &endp, 10);
            if( ('\0' != *endp ) || (coll_id < ALLGATHER) || (coll_id >= COLLCOUNT) ) {  /* there is garbage in the input */
                opal_output_verbose(5, mca_coll_han_component.han_output,
                                    "coll:han:mca_coll_han_init_dynamic_rules invalid collective %s "
                                    "at line %d: the collective must be at least %d and less than %d. "
                                    "The rest of the input file will be ignored.\n",
                                    coll_name, fileline, ALLGATHER, COLLCOUNT);
                goto file_reading_error;
            }
            free(coll_name);
            coll_name = strdup(mca_coll_base_colltype_to_str(coll_id));
        }

        if(!mca_coll_han_is_coll_dynamic_implemented(coll_id)) {
            opal_output_verbose(5, mca_coll_han_component.han_output,
                                "coll:han:mca_coll_han_init_dynamic_rules found an error on dynamic rules file %s "
                                "read collective id %d at line %d but this collective is not implemented yet. "
                                "This is not an error but this set of rules will not be used\n",
                                fname, coll_id, fileline);
        }

        /*
         * The first information of a collective rule
         * is the number of topologic rules
         */
        if( (getnext_long(fptr, &nb_topo) < 0) || (nb_topo < 0) ) {
            opal_output_verbose(5, mca_coll_han_component.han_output,
                                "coll:han:mca_coll_han_init_dynamic_rules found an error on dynamic rules file %s "
                                "at line %d: an invalid value %ld is given for topo level count "
                                "or the reader encountered an unexpected EOF\n",
                                fname, fileline, nb_topo);
            goto file_reading_error;
        }

        /* Store the collective rule information */
        coll_rules[i].nb_topologic_levels = nb_topo;
        coll_rules[i].collective_id = (COLLTYPE_T)coll_id;

        if(0 == nb_topo) {
            opal_output_verbose(5, mca_coll_han_component.han_output,
                                "coll:han:mca_coll_han_init_dynamic_rules Warning on dynamic rules file %s "
                                "at line %d: an invalid value %ld is given for topo level count\n",
                                fname, fileline, nb_topo);
            continue;
        }

        /* Allocate topologic rules */
        topo_rules = malloc(nb_topo * sizeof(topologic_rule_t));
        coll_rules[i].topologic_rules = topo_rules;
        if(NULL == topo_rules) {
            coll_rules[i].nb_topologic_levels = 0;
            goto cannot_allocate;
        }

        /* Iterates on topologic rules */
        for( j = 0 ; j < nb_topo ; j++ ) {
            topo_rules[j].nb_rules = 0;
            topo_rules[j].configuration_rules = NULL;
            coll_rules[i].nb_topologic_levels = j+1;

            /* Get the topologic level identifier */
            char *topo_lvl_name = NULL;
            if( getnext_string(fptr, &topo_lvl_name) < 0 ) {
                opal_output_verbose(5, mca_coll_han_component.han_output,
                                    "coll:han:mca_coll_han_init_dynamic_rules found an error on dynamic rules file %s "
                                    "at line %d: cannot read the name/id of a topo level\n",
                                    fname, fileline);
                goto file_reading_error;
            }
            topo_lvl = mca_coll_han_topo_lvl_name_to_id(topo_lvl_name);
            if (topo_lvl < 0) {
                char *endp;
                topo_lvl = (int)strtol(topo_lvl_name, &endp, 10);
                if (('\0' != *endp ) || (topo_lvl < INTRA_NODE) || (topo_lvl >= NB_TOPO_LVL)) {
                    opal_output_verbose(5, mca_coll_han_component.han_output,
                                        "coll:han:mca_coll_han_init_dynamic_rules found an error on dynamic rules file %s "
                                        "at line %d: unknown topo level '%s'\n",
                                        fname, fileline, topo_lvl_name);
                    free(topo_lvl_name);
                    topo_lvl_name = NULL;
                    goto file_reading_error;
                }
            }
            free (topo_lvl_name);

            /*
             * The first information of a topologic rule
             * is the number of configurations
             */
            nb_rules = -1;
            if( (getnext_long(fptr, &nb_rules) < 0) || (nb_rules < 0) ) {
                opal_output_verbose(5, mca_coll_han_component.han_output,
                                    "coll:han:mca_coll_han_init_dynamic_rules found an error on dynamic rules file %s "
                                    "at line %d: an invalid value %ld is given for rules count "
                                    "or the reader encountered an unexpected EOF\n",
                                    fname, fileline, nb_rules);
                goto file_reading_error;
            }

            /* Store the topologic rule information */
            topo_rules[j].collective_id = coll_id;
            topo_rules[j].topologic_level = (TOPO_LVL_T)topo_lvl;
            topo_rules[j].nb_rules = nb_rules;

            if(0 == nb_rules) {
                opal_output_verbose(5, mca_coll_han_component.han_output,
                                    "coll:han:mca_coll_han_init_dynamic_rules Warning on dynamic rules file %s "
                                    "at line %d: an invalid value %ld is given for configuration rules count\n",
                                    fname, fileline, nb_rules);
                continue;
            }

            /* Allocate configuration rules */
            conf_rules = malloc(nb_rules * sizeof(configuration_rule_t));
            topo_rules[j].configuration_rules = conf_rules;
            if(NULL == conf_rules) {
                topo_rules[j].nb_rules = 0;
                goto cannot_allocate;
            }

            /* Iterate on configuration rules */
            for( k = 0; k < nb_rules; k++ ) {
                conf_rules[k].nb_msg_size = 0;
                conf_rules[k].msg_size_rules = NULL;
                topo_rules[j].nb_rules = k+1;

                /* Get the configuration size */
                if( (getnext_long(fptr, &conf_size) < 0) || (conf_size < 1) || (0 == k && conf_size > 1) ) {
                    opal_output_verbose(5, mca_coll_han_component.han_output,
                                        "coll:han:mca_coll_han_init_dynamic_rules invalid configuration size %ld at line %d "
                                        "or the reader encountered an unexpected EOF the configuration size must be at least %d "
                                        "and the first configuration size of a topologic level must be %d\n",
                                        conf_size, fileline, 1, 1);
                    goto file_reading_error;
                }

                /*
                 * The first information of a configuration rule
                 * is the number of message size rules
                 */
                if( (getnext_long(fptr, &nb_msg_size) < 0) || (nb_msg_size < 0) ) {
                    opal_output_verbose(5, mca_coll_han_component.han_output,
                                        "coll:han:mca_coll_han_init_dynamic_rules found an error on dynamic rules file %s "
                                        "at line %d: an invalid value %ld is given for message size rules count "
                                        "or the reader encountered an unexpected EOF\n",
                                        fname, fileline, nb_msg_size);
                    goto file_reading_error;
                }

                /* Store configuration rule information */
                conf_rules[k].collective_id = coll_id;
                conf_rules[k].topologic_level = topo_lvl;
                conf_rules[k].configuration_size = conf_size;
                conf_rules[k].nb_msg_size = nb_msg_size;

                if(0 == nb_msg_size) {
                    opal_output_verbose(5, mca_coll_han_component.han_output,
                                        "coll:han:mca_coll_han_init_dynamic_rules Warning on dynamic rules file %s "
                                        "at line %d: an invalid value %ld is given for message size rules count\n",
                                        fname, fileline, nb_msg_size);
                    continue;
                }

                /* Allocate message size rules */
                msg_size_rules = malloc(nb_msg_size * sizeof(msg_size_rule_t));
                conf_rules[k].msg_size_rules = msg_size_rules;
                if(NULL == msg_size_rules) {
                    conf_rules[k].nb_msg_size = 0;
                    goto cannot_allocate;
                }

                /* Iterate on message size rules */
                for( l = 0; l < nb_msg_size; l++ ) {
                    conf_rules[k].nb_msg_size = l+1;

                    /* Get the message size */
                    rc = getnext_size_t(fptr, &msg_size);
                    if( (rc < 0) ||
                        (0 == l && msg_size > 1)) {
                        opal_output_verbose(5, mca_coll_han_component.han_output,
                                            "coll:han:mca_coll_han_init_dynamic_rules found an error on dynamic rules file %s "
                                            "at line %d: an invalid value %" PRIsize_t " is given for message size "
                                            "or the reader encountered an unexpected EOF. "
                                            "The first message size rule of a configuration must be 0\n",
                                            fname, fileline, msg_size);
                        goto file_reading_error;
                    }

                    /* Get the component identifier for this message size rule */
                    free(target_comp_name);
                    if( getnext_string(fptr, &target_comp_name) < 0 ) {
                        opal_output_verbose(5, mca_coll_han_component.han_output,
                                            "coll:han:mca_coll_han_init_dynamic_rules found an error on dynamic rules file %s "
                                            "at line %d: cannot read the name of a collective component\n",
                                            fname, fileline);
                        goto file_reading_error;
                    }
                    component = mca_coll_han_component_name_to_id(target_comp_name);
                    if( (component < SELF) || (component >= COMPONENTS_COUNT) ) {
                        opal_output_verbose(5, mca_coll_han_component.han_output,
                                            "coll:han:mca_coll_han_init_dynamic_rules found an error on dynamic rules file %s "
                                            "at line %d: an invalid collective component name %s was given or the "
                                            "reader encountered an unexpected EOF. Collective component id must be at "
                                            "least %d and less than %d\n",
                                            fname, fileline, target_comp_name, SELF, COMPONENTS_COUNT);
                        goto file_reading_error;
                    }

                    /* Get the optionnal algorithm for han  */
                    algorithm_id = 0; // default for all collectives
                    if ((component == HAN) && (1 == ompi_coll_base_file_peek_next_char_is(fptr, &fileline, '@')) ) {

                        free(algorithm_name);
                        algorithm_name = NULL;
                        if( getnext_string(fptr, &algorithm_name) < 0 ) {
                            opal_output_verbose(5, mca_coll_han_component.han_output,
                                                "coll:han:mca_coll_han_init_dynamic_rules found an error on dynamic rules file %s "
                                                "at line %d: cannot read the name/id of an algorithm\n",
                                                fname, fileline);
                            goto file_reading_error;
                        }
                        algorithm_id = mca_coll_han_algorithm_name_to_id(coll_id, algorithm_name);
                        if (algorithm_id < 0) {
                            char *endp;
                            algorithm_id = (int)strtol(algorithm_name, &endp, 10);
                            char endc = *endp;
                            if (('\0' != endc ) || !mca_coll_han_algorithm_id_is_valid(coll_id, algorithm_id)) {
                                opal_output_verbose(5, mca_coll_han_component.han_output,
                                                    "coll:han:mca_coll_han_init_dynamic_rules found an error on dynamic rules file %s "
                                                    "at line %d: unknown algorithm '%s' for %s\n",
                                                    fname, fileline, algorithm_name, coll_name);
                                goto file_reading_error;
                            }
                        }
                        opal_output_verbose(5, mca_coll_han_component.han_output,
                                            "coll:han:mca_coll_han_init_dynamic_rules found for coll=%s msg_size=%ld : algorithm '%s' %d\n",
                                            coll_name, msg_size, algorithm_name, algorithm_id);
                    }


                    /* Store message size rule information */
                    msg_size_rules[l].collective_id = coll_id;
                    msg_size_rules[l].topologic_level = topo_lvl;
                    msg_size_rules[l].configuration_size = conf_size;
                    msg_size_rules[l].msg_size = msg_size;
                    msg_size_rules[l].component = (COMPONENT_T)component;
                    msg_size_rules[l].algorithm_id = algorithm_id;

                    nb_entries++;
                    /* do we have the optional segment length */
                    if( 1 == ompi_coll_base_file_peek_next_char_is(fptr, &fileline, '[') ) {
                        opal_output_verbose(5, mca_coll_han_component.han_output,
                                            "coll:han:mca_coll_han_init_dynamic_rules found optional pipelining segment lengths\n");
                        long seglength;
                        if( 0 != topo_lvl ) {
                            opal_output_verbose(5, mca_coll_han_component.han_output,
                                                "coll:han:mca_coll_han_init_dynamic_rules "
                                                "file %s line %d found segment lengths for topological collective at level != 0 "
                                                "for collective %s component %s. These values will be ignored.\n",
                                                fname, fileline, coll_name, target_comp_name);
                        }
                        while( 0 == ompi_coll_base_file_peek_next_char_is(fptr, &fileline, ']') ) {
                            if( getnext_long(fptr, &seglength) ) {
                                opal_output_verbose(5, mca_coll_han_component.han_output,
                                                    "coll:han:mca_coll_han_init_dynamic_rules "
                                                    "file %s line %d found end of file while reading the optional list "
                                                    "of segment lengths for collective %s component %s\n",
                                                    fname, fileline, coll_name, target_comp_name);
                                goto file_reading_error;
                            }
                        }
                    }
                }
            }
        }
    }

    if( getnext_long(fptr, &nb_coll) > 0 ) {
        opal_output_verbose(5, mca_coll_han_component.han_output,
                            "coll:han:mca_coll_han_init_dynamic_rules. Warning on file %s at line %d: "
                            "rule reading is over but reader does not seem to have reached the end of the file\n",
                            fname, fileline);
    }

    opal_output_verbose(5, mca_coll_han_component.han_output,
                        "coll:han:mca_coll_han_init_dynamic_rules read %d rules from %s\n",
                        nb_entries, fname);

    if(mca_coll_han_component.dump_dynamic_rules) {
        mca_coll_han_dump_dynamic_rules();
    }

    fclose(fptr);

    check_dynamic_rules();
    free(coll_name);
    free(algorithm_name);
    free(target_comp_name);
    return OMPI_SUCCESS;

cannot_allocate:
    /* The dynamic rules allocation failed
     * Free the already allocated rules and return a failure
     */
    opal_output_verbose(0, mca_coll_han_component.han_output,
                        "coll:han:mca_coll_han_init_dynamic_rules "
                        "cannot allocate dynamic rules\n");
    free(coll_name);
    free(algorithm_name);
    free(target_comp_name);
    fclose (fptr);
    /* We disable the module, we don't need to keep the rules */
    mca_coll_han_free_dynamic_rules();
    return OMPI_ERROR;

file_reading_error:
    opal_output_verbose(0, mca_coll_han_component.han_output,
                        "coll:han:mca_coll_han_init_dynamic_rules "
                        "could not fully read dynamic rules file. "
                        "Will use mca parameters defined rules. "
                        "To see error detail, please set "
                        "collective verbosity level over 5\n");
    free(coll_name);
    free(algorithm_name);
    free(target_comp_name);
    fclose (fptr);
    /* We disable the module, we don't need to keep the rules */
    mca_coll_han_free_dynamic_rules();
    return OMPI_SUCCESS;
}

/*
 * Memory free all the rules parsed in the file
 */
void
mca_coll_han_free_dynamic_rules(void)
{
    /* Loop counters */
    int i, j, k;

    /* Loop ranges */
    int nb_coll, nb_conf;

    /* Aliases */
    collective_rule_t *coll_rules;
    topologic_rule_t *topo_rules;
    configuration_rule_t *conf_rules;

    nb_coll = mca_coll_han_component.dynamic_rules.nb_collectives;
    coll_rules = mca_coll_han_component.dynamic_rules.collective_rules;

    for(i=0 ; i<nb_coll ; i++) {
        int nb_topo = coll_rules[i].nb_topologic_levels;
        topo_rules = coll_rules[i].topologic_rules;

        for(j=0 ; j<nb_topo ; j++) {
            nb_conf = topo_rules[j].nb_rules;
            conf_rules = topo_rules[j].configuration_rules;

            for(k=0 ; k<nb_conf ; k++) {
                if(conf_rules[k].nb_msg_size > 0) {
                    free(conf_rules[k].msg_size_rules);
                }
            }

            if(nb_conf > 0) {
                free(conf_rules);
            }
        }

        if(nb_topo > 0) {
            free(topo_rules);
        }
    }

    if(nb_coll > 0) {
        free(coll_rules);
    }

    mca_coll_han_component.dynamic_rules.nb_collectives = 0;
}

/*
 * Try to find any logical issue in dynamic rules
 */
static void check_dynamic_rules(void)
{
    /* Loop counters */
    int i, j, k, l;

    /* Collective information */
    int nb_coll;
    COLLTYPE_T coll_id;
    collective_rule_t *coll_rules;

    /* Topo information */
    TOPO_LVL_T topo_lvl;
    topologic_rule_t *topo_rules;

    /* Configuration information */
    int nb_rules, conf_size;
    configuration_rule_t *conf_rules;

    /* Message size information */
    int nb_msg_size;
    size_t msg_size;
    msg_size_rule_t *msg_size_rules;

    /* Component information */
    COMPONENT_T component;

    nb_coll = mca_coll_han_component.dynamic_rules.nb_collectives;
    coll_rules = mca_coll_han_component.dynamic_rules.collective_rules;

    for( i = 0; i < nb_coll; i++ ) {
        coll_id = coll_rules[i].collective_id;
        int nb_topo = coll_rules[i].nb_topologic_levels;
        topo_rules = coll_rules[i].topologic_rules;

        for( j = 0; j < nb_topo; j++ ) {
            topo_lvl = topo_rules[j].topologic_level;
            nb_rules = topo_rules[j].nb_rules;
            conf_rules = topo_rules[j].configuration_rules;

            for( k = 0; k < nb_rules; k++ ) {
                conf_size = conf_rules[k].configuration_size;
                nb_msg_size = conf_rules[k].nb_msg_size;
                msg_size_rules = conf_rules[k].msg_size_rules;

                if( k >= 1 && conf_rules[k-1].configuration_size > conf_size) {
                    opal_output_verbose(5, mca_coll_han_component.han_output,
                                        "coll:han:check_dynamic_rules HAN found an issue on dynamic rules "
                                        "for collective %d on topological level %d: "
                                        "configuration sizes %d and %d are not sorted by increasing value\n",
                                        coll_id, topo_lvl, conf_rules[k-1].configuration_size, conf_size);
                }

                for( l = 0; l < nb_msg_size; l++ ) {
                    msg_size = msg_size_rules[l].msg_size;
                    component = msg_size_rules[l].component;

                    if( l >= 1 && msg_size_rules[l-1].msg_size > msg_size) {
                        opal_output_verbose(5, mca_coll_han_component.han_output,
                                            "coll:han:check_dynamic_rules HAN found an issue on dynamic rules "
                                            "for collective %d on topological level %d with configuration size %d: "
                                            "message sizes %" PRIsize_t " and %" PRIsize_t " are "
                                            "not sorted by increasing value\n",
                                            coll_id, topo_lvl, conf_size, msg_size_rules[l-1].msg_size, msg_size);
                    }

                    if( (HAN == component) && (GLOBAL_COMMUNICATOR != topo_lvl) ) {
                        opal_output_verbose(5, mca_coll_han_component.han_output,
                                            "coll:han:check_dynamic_rules HAN found an issue on dynamic rules "
                                            "for collective %d on topological level %d with configuration size %d "
                                            "for message size %" PRIsize_t ": han collective component %d "
                                            "can only be activated for topology level %d\n",
                                            coll_id, topo_lvl, conf_size, msg_size, HAN, GLOBAL_COMMUNICATOR);
                    }
                }
            }
        }
    }
}

/* Print configurations parsed from the file */
void mca_coll_han_dump_dynamic_rules(void)
{
    int nb_entries = 0;

    /* Collective information */
    int nb_coll;
    COLLTYPE_T coll_id;
    collective_rule_t *coll_rules;

    /* Topo information */
    TOPO_LVL_T topo_lvl;
    topologic_rule_t *topo_rules;

    /* Configuration information */
    int nb_rules, conf_size;
    configuration_rule_t *conf_rules;

    /* Message size information */
    int nb_msg_size, msg_size;
    msg_size_rule_t *msg_size_rules;

    /* Component information */
    COMPONENT_T component;

    nb_coll = mca_coll_han_component.dynamic_rules.nb_collectives;
    coll_rules = mca_coll_han_component.dynamic_rules.collective_rules;

    for(int i = 0; i < nb_coll; i++ ) {
        coll_id = coll_rules[i].collective_id;
        int nb_topo = coll_rules[i].nb_topologic_levels;
        topo_rules = coll_rules[i].topologic_rules;

        for(int j = 0; j < nb_topo; j++ ) {
            topo_lvl = topo_rules[j].topologic_level;
            nb_rules = topo_rules[j].nb_rules;
            conf_rules = topo_rules[j].configuration_rules;

            for(int k = 0; k < nb_rules; k++ ) {
                conf_size = conf_rules[k].configuration_size;
                nb_msg_size = conf_rules[k].nb_msg_size;
                msg_size_rules = conf_rules[k].msg_size_rules;

                for(int l = 0; l < nb_msg_size; l++ ) {
                    msg_size = msg_size_rules[l].msg_size;
                    component = msg_size_rules[l].component;

                    opal_output(mca_coll_han_component.han_output,
                                "coll:han:dump_dynamic_rules %d collective %d (%s) "
                                "topology level %d (%s) configuration size %d "
                                "message size %d -> collective component %d (%s)\n",
                                nb_entries, coll_id, mca_coll_base_colltype_to_str(coll_id),
                                topo_lvl, mca_coll_han_topo_lvl_to_str(topo_lvl), conf_size,
                                msg_size, component, ompi_coll_han_available_components[component].component_name);

                    nb_entries++;
                }
            }
        }
    }
}
