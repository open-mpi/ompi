/*
 * Copyright (c) 2011      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2013 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 * Processing for command line interface options
 *
 */
#include "rmaps_lama.h"

#include "opal/util/argv.h"

#include "orte/mca/rmaps/base/rmaps_private.h"
#include "orte/mca/rmaps/base/base.h"
#include "orte/util/show_help.h"

#include <ctype.h>

/*********************************
 * Local Functions
 *********************************/
/*
 * QSort: Integer comparison
 */
static int lama_parse_int_sort(const void *a, const void *b);

/*
 * Convert the '-ppr' syntax from the 'ppr' component to the 'lama' '-mppr' syntax.
 */
static char * rmaps_lama_covert_ppr(char * given_ppr);

/*********************************
 * Parsing Functions
 *********************************/
int rmaps_lama_process_alias_params(orte_job_t *jdata)
{
    int exit_status = ORTE_SUCCESS;

    /*
     * Mapping options
     * Note: L1, L2, L3 are not exposed in orterun to the user, so
     *       there is no need to specify them here.
     */
    if( NULL == rmaps_lama_cmd_map ) {
        /* orte_rmaps_base.mapping */
        switch( ORTE_GET_MAPPING_POLICY(jdata->map->mapping) ) {
        case ORTE_MAPPING_BYNODE:
            /* rmaps_lama_cmd_map = strdup("nbNsL3L2L1ch"); */
            rmaps_lama_cmd_map = strdup("nbsch");
            break;
        case ORTE_MAPPING_BYBOARD:
            /* rmaps_lama_cmd_map = strdup("bnNsL3L2L1ch"); */
            orte_show_help("help-orte-rmaps-lama.txt",
                           "invalid mapping option",
                           true,
                           "by board", "mapping by board not supported by LAMA");
            exit_status = ORTE_ERR_NOT_SUPPORTED;
            goto cleanup;
            break;
        case ORTE_MAPPING_BYNUMA:
            /* rmaps_lama_cmd_map = strdup("NbnsL3L2L1ch"); */
            rmaps_lama_cmd_map = strdup("Nbnsch");
            break;
        case ORTE_MAPPING_BYSOCKET:
            /* rmaps_lama_cmd_map = strdup("sNbnL3L2L1ch"); */
            rmaps_lama_cmd_map = strdup("sbnch");
            break;
        case ORTE_MAPPING_BYL3CACHE:
            rmaps_lama_cmd_map = strdup("L3sNbnL2L1ch");
            break;
        case ORTE_MAPPING_BYL2CACHE:
            rmaps_lama_cmd_map = strdup("L2sNbnL1ch");
            break;
        case ORTE_MAPPING_BYL1CACHE:
            rmaps_lama_cmd_map = strdup("L1sNbnch");
            break;
        case ORTE_MAPPING_BYCORE:
        case ORTE_MAPPING_BYSLOT:
            /* rmaps_lama_cmd_map = strdup("cL1L2L3sNbnh"); */
            rmaps_lama_cmd_map = strdup("csbnh");
            break;
        case ORTE_MAPPING_BYHWTHREAD:
            /* rmaps_lama_cmd_map = strdup("hcL1L2L3sNbn"); */
            rmaps_lama_cmd_map = strdup("hcsbn");
            break;
        case ORTE_MAPPING_RR:
            orte_show_help("help-orte-rmaps-lama.txt",
                           "invalid mapping option",
                           true,
                           "round robin", "mapping by round robin not supported by LAMA");
            exit_status = ORTE_ERR_NOT_SUPPORTED;
            goto cleanup;
        case ORTE_MAPPING_SEQ:
            orte_show_help("help-orte-rmaps-lama.txt",
                           "invalid mapping option",
                           true,
                           "sequential", "mapping by sequential not supported by LAMA");
            exit_status = ORTE_ERR_NOT_SUPPORTED;
            goto cleanup;
        case ORTE_MAPPING_BYUSER:
            orte_show_help("help-orte-rmaps-lama.txt",
                           "invalid mapping option",
                           true,
                           "by user", "mapping by user not supported by LAMA");
            exit_status = ORTE_ERR_NOT_SUPPORTED;
            goto cleanup;
        default:
            /*
             * Default is map-by core
             */
            rmaps_lama_cmd_map = strdup("cL1L2L3sNbnh");
            break;
        }
    }

    /*
     * Binding Options
     */
    if( NULL == rmaps_lama_cmd_bind ) {
        /*
         * No binding specified, use default
         */
        if( !OPAL_BINDING_POLICY_IS_SET(jdata->map->binding) ||
            !OPAL_BINDING_REQUIRED(opal_hwloc_binding_policy) ||
            OPAL_BIND_TO_NONE == OPAL_GET_BINDING_POLICY(jdata->map->binding) ) {
            rmaps_lama_cmd_bind = NULL;
        }

        switch( OPAL_GET_BINDING_POLICY(jdata->map->binding) ) {
        case OPAL_BIND_TO_BOARD:
            /* rmaps_lama_cmd_bind = strdup("1b"); */
            orte_show_help("help-orte-rmaps-lama.txt",
                           "invalid binding option",
                           true,
                           "by board", "binding to board not supported by LAMA");
            exit_status = ORTE_ERR_NOT_SUPPORTED;
            goto cleanup;
            break;
        case OPAL_BIND_TO_NUMA:
            rmaps_lama_cmd_bind = strdup("1N");
            break;
        case OPAL_BIND_TO_SOCKET:
            rmaps_lama_cmd_bind = strdup("1s");
            break;
        case OPAL_BIND_TO_L3CACHE:
            rmaps_lama_cmd_bind = strdup("1L3");
            break;
        case OPAL_BIND_TO_L2CACHE:
            rmaps_lama_cmd_bind = strdup("1L2");
            break;
        case OPAL_BIND_TO_L1CACHE:
            rmaps_lama_cmd_bind = strdup("1L1");
            break;
        case OPAL_BIND_TO_CORE:
            rmaps_lama_cmd_bind = strdup("1c");
            break;
        case OPAL_BIND_TO_HWTHREAD:
            rmaps_lama_cmd_bind = strdup("1h");
            break;
        case OPAL_BIND_TO_CPUSET:
            orte_show_help("help-orte-rmaps-lama.txt",
                           "invalid binding option",
                           true,
                           "by CPU set", "binding to CPU set not supported by LAMA");
            exit_status = ORTE_ERR_NOT_SUPPORTED;
            goto cleanup;
            break;
        default:
            rmaps_lama_cmd_bind = NULL;
            break;
        }
    }

    /*
     * Ordering (a.k.a. Ranking) Options
     */
    if( NULL == rmaps_lama_cmd_ordering ) {
        /* orte_rmaps_base.ranking */
        switch( ORTE_GET_RANKING_POLICY(jdata->map->ranking) ) {
        case ORTE_RANK_BY_SLOT:
            rmaps_lama_cmd_ordering = strdup("s");
            break;
        case ORTE_RANK_BY_NODE:
        case ORTE_RANK_BY_NUMA:
        case ORTE_RANK_BY_SOCKET:
        case ORTE_RANK_BY_L3CACHE:
        case ORTE_RANK_BY_L2CACHE:
        case ORTE_RANK_BY_L1CACHE:
        case ORTE_RANK_BY_CORE:
        case ORTE_RANK_BY_HWTHREAD:
            rmaps_lama_cmd_ordering = strdup("n");
            break;
        case ORTE_RANK_BY_BOARD:
            /* rmaps_lama_cmd_ordering = strdup("n"); */
            orte_show_help("help-orte-rmaps-lama.txt",
                           "invalid ordering option",
                           true,
                           "by board", "ordering by board not supported by LAMA");
            exit_status = ORTE_ERR_NOT_SUPPORTED;
            goto cleanup;
            break;
        default:
            rmaps_lama_cmd_ordering = strdup("n");
            break;
        }
    }

    /*
     * MPPR
     */
    if( NULL == rmaps_lama_cmd_mppr ) {
        /*
         * The ppr is given in the map
         */
        if( NULL != jdata->map->ppr) {
            rmaps_lama_cmd_mppr = rmaps_lama_covert_ppr(jdata->map->ppr);
        }
    }

    /*
     * Oversubscription
     */
    if( ORTE_MAPPING_NO_OVERSUBSCRIBE & ORTE_GET_MAPPING_DIRECTIVE(jdata->map->mapping) ) {
        rmaps_lama_can_oversubscribe = false;
    }
    else {
        rmaps_lama_can_oversubscribe = true;
    }

    /*
     * Display revised values
     */
    opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                        "mca:rmaps:lama: Revised Parameters -----");
    opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                        "mca:rmaps:lama: Map   : %s",
                        rmaps_lama_cmd_map);
    opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                        "mca:rmaps:lama: Bind  : %s",
                        rmaps_lama_cmd_bind);
    opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                        "mca:rmaps:lama: MPPR  : %s",
                        rmaps_lama_cmd_mppr);
    opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                        "mca:rmaps:lama: Order : %s",
                        rmaps_lama_cmd_ordering);

 cleanup:
    return exit_status;
}

static char * rmaps_lama_covert_ppr(char * given_ppr)
{
    return strdup(given_ppr);
}

int rmaps_lama_parse_mapping(char *layout,
                             rmaps_lama_level_type_t **layout_types,
                             rmaps_lama_level_type_t **layout_types_sorted,
                             int *num_types)
{
    int exit_status = ORTE_SUCCESS;
    char param[3];
    int i, j, len;
    bool found_req_param_n = false;
    bool found_req_param_h = false;
    bool found_req_param_bind = false;

    /*
     * Sanity Check:
     * There is no default layout, so if we get here and nothing is specified
     * then this is an error.
     */
    if( NULL == layout ) {
        orte_show_help("help-orte-rmaps-lama.txt",
                       "internal error",
                       true,
                       "rmaps_lama_parse_mapping",
                       "internal error 1");
        return ORTE_ERROR;
    }

    *num_types = 0;

    /*
     * Extract and convert all the keys
     */
    len = strlen(layout);
    for(i = 0; i < len; ++i) {
        /*
         * L1 : L1 Cache
         * L2 : L2 Cache
         * L3 : L3 Cache
         */
        if( layout[i] == 'L' ) {
            param[0] = layout[i];
            ++i;
            /*
             * Check for 2 characters
             */
            if( i >= len ) {
                orte_show_help("help-orte-rmaps-lama.txt",
                               "invalid mapping option",
                               true,
                               layout, "cache level missing number");
                exit_status = ORTE_ERROR;
                goto cleanup;
            }
            param[1] = layout[i];
            param[2] = '\0';
        }
        /*
         * n : Machine
         * b : Board
         * s : Socket
         * c : Core
         * h : Hardware Thread
         * N : NUMA Node
         */
        else {
            param[0] = layout[i];
            param[1] = '\0';
        }

        /*
         * Append level
         */
        *num_types += 1;
        *layout_types = (rmaps_lama_level_type_t*)realloc(*layout_types, sizeof(rmaps_lama_level_type_t) * (*num_types));
        (*layout_types)[(*num_types)-1] = lama_type_str_to_enum(param);
    }

    /*
     * Check for duplicates and unknowns
     * Copy to sorted list
     */
    *layout_types_sorted = (rmaps_lama_level_type_t*)malloc(sizeof(rmaps_lama_level_type_t) * (*num_types));
    for( i = 0; i < *num_types; ++i ) {
        /*
         * Copy for later sorting
         */
        (*layout_types_sorted)[i] = (*layout_types)[i];

        /*
         * Look for unknown and unsupported options
         */
        if( LAMA_LEVEL_UNKNOWN <= (*layout_types)[i] ) {
            char *msg;
            asprintf(&msg, "unknown mapping level at position %d", i + 1);
            orte_show_help("help-orte-rmaps-lama.txt",
                           "invalid mapping option",
                           true,
                           layout, msg);
            free(msg);
            exit_status = ORTE_ERROR;
            goto cleanup;
        }

        if( LAMA_LEVEL_MACHINE == (*layout_types)[i] ) {
            found_req_param_n = true;
        }

        if( LAMA_LEVEL_PU == (*layout_types)[i] ) {
            found_req_param_h = true;
        }

        if( lama_binding_level == (*layout_types)[i] ) {
            found_req_param_bind = true;
        }

        /*
         * Look for duplicates
         */
        for( j = i+1; j < *num_types; ++j ) {
            if( (*layout_types)[i] == (*layout_types)[j] ) {
                char *msg;
                asprintf(&msg, "duplicate mapping levels at position %d and %d",
                         i + 1, j + 1);
                orte_show_help("help-orte-rmaps-lama.txt",
                               "invalid mapping option",
                               true,
                               layout, msg);
                free(msg);
                exit_status = ORTE_ERROR;
                goto cleanup;
            }
        }
    }

    /*
     * The user is required to specify at least the:
     * - machine
     * - hardware thread (needed for lower bound binding) JJH: We should be able to lift this...
     * - binding layer (need it to stride the mapping)
     * Only print the error message once, for brevity.
     */
    if( !found_req_param_n ) {
        char *msg;
        asprintf(&msg, "missing required 'n' mapping token");
        orte_show_help("help-orte-rmaps-lama.txt",
                       "invalid mapping option",
                       true,
                       layout, msg);
        free(msg);
        exit_status = ORTE_ERROR;
        goto cleanup;
    }
    else if(!found_req_param_h) {
        char *msg;
        asprintf(&msg, "missing required 'h' mapping token");
        orte_show_help("help-orte-rmaps-lama.txt",
                       "invalid mapping option",
                       true,
                       layout, msg);
        free(msg);
        exit_status = ORTE_ERROR;
        goto cleanup;
    } else if (!found_req_param_bind) {
        char *msg;
        asprintf(&msg, "missing required mapping token for the current binding level");
        orte_show_help("help-orte-rmaps-lama.txt",
                       "invalid mapping option",
                       true,
                       layout, msg);
        free(msg);
        exit_status = ORTE_ERROR;
        goto cleanup;
    }

    /*
     * Sort the items
     */
    qsort((*layout_types_sorted ), (*num_types), sizeof(int), lama_parse_int_sort);

 cleanup:
    return exit_status;
}

int rmaps_lama_parse_binding(char *layout, rmaps_lama_level_type_t *binding_level, int *num_types)
{
    int exit_status = ORTE_SUCCESS;
    char param[3];
    char num[MAX_BIND_DIGIT_LEN];
    int i, n, p, len;

    /*
     * Default: If nothing specified
     * - Bind to machine
     */
    if( NULL == layout ) {
        *binding_level = LAMA_LEVEL_MACHINE;
        *num_types = 1;
        return ORTE_SUCCESS;
    }

    *num_types = 0;

    /*
     * Extract and convert all the keys
     */
    len = strlen(layout);
    n = 0;
    p = 0;
    for(i = 0; i < len; ++i) {
        /*
         * Must start with a digit
         */
        if( isdigit(layout[i]) ) {
            /*
             * Check: Digits must come first
             */
            if( p != 0 ) {
                orte_show_help("help-orte-rmaps-lama.txt",
                               "invalid binding option",
                               true,
                               layout, "missing digit(s) before binding level token");
                exit_status = ORTE_ERROR;
                goto cleanup;
            }

            num[n] = layout[i];
            ++n;
            /*
             * Check: Exceed bound of number of digits
             */
            if( n >= MAX_BIND_DIGIT_LEN ) {
                orte_show_help("help-orte-rmaps-lama.txt",
                               "invalid binding option",
                               true,
                               layout, "too many digits");
                exit_status = ORTE_ERROR;
                goto cleanup;
            }
        }
        /*
         * Extract the level
         */
        else {
            /*
             * Check: Digits must come first
             */
            if( n == 0 ) {
                orte_show_help("help-orte-rmaps-lama.txt",
                               "invalid binding option",
                               true,
                               layout, "missing digit(s) before binding level token");
                exit_status = ORTE_ERROR;
                goto cleanup;
            }
            /*
             * Check: Only one level allowed
             */
            if( p != 0 ) {
                orte_show_help("help-orte-rmaps-lama.txt",
                               "invalid binding option",
                               true,
                               layout, "only one binding level may be specified");
                exit_status = ORTE_ERROR;
                goto cleanup;
            }

            /*
             * L1 : L1 Cache
             * L2 : L2 Cache
             * L3 : L3 Cache
             */
            if( layout[i] == 'L' ) {
                param[0] = layout[i];
                ++i;
                /*
                 * Check for 2 characters
                 */
                if( i >= len ) {
                    orte_show_help("help-orte-rmaps-lama.txt",
                                   "invalid binding option",
                                   true,
                                   layout, "only one binding level may be specified");
                    exit_status = ORTE_ERROR;
                    goto cleanup;
                }
                param[1] = layout[i];
                p = 2;
            }
            /*
             * n : Machine
             * b : Board
             * s : Socket
             * c : Core
             * h : Hardware Thread
             * N : NUMA Node
             */
            else {
                param[0] = layout[i];
                p = 1;
            }
            param[p] = '\0';
        }
    }
    /*
     * Check that the level was specified
     */
    if( p == 0 ) {
        orte_show_help("help-orte-rmaps-lama.txt",
                       "invalid binding option",
                       true,
                       layout, "binding specification is empty");
        exit_status = ORTE_ERROR;
        goto cleanup;
    }
    num[n] = '\0';

    *binding_level = lama_type_str_to_enum(param);
    *num_types = atoi(num);

    /*
     * Check for unknown level
     */
    if( LAMA_LEVEL_UNKNOWN <= *binding_level ) {
        orte_show_help("help-orte-rmaps-lama.txt",
                       "invalid binding option",
                       true,
                       layout, "unknown binding level");
        exit_status = ORTE_ERROR;
        goto cleanup;
    }

 cleanup:
    return exit_status;
}

int rmaps_lama_parse_mppr(char *layout, rmaps_lama_level_info_t **mppr_levels, int *num_types)
{
    int exit_status = ORTE_SUCCESS;
    char param[3];
    char num[MAX_BIND_DIGIT_LEN];
    char **argv = NULL;
    int argc = 0;
    int i, j, len;
    int p, n;

    /*
     * Default: Unrestricted allocation
     * 'oversubscribe' flag accounted for elsewhere
     */
    if( NULL == layout ) {
        *mppr_levels = NULL;
        *num_types = 0;
        return ORTE_SUCCESS;
    }

    *num_types = 0;

    /*
     * Split by ','
     * <#:level>,<#:level>,...
     */
    argv = opal_argv_split(layout, ',');
    argc = opal_argv_count(argv);
    for(j = 0; j < argc; ++j) {
        /*
         * Parse <#:level>
         */
        len = strlen(argv[j]);
        n = 0;
        p = 0;
        for(i = 0; i < len; ++i) {
            /*
             * Skip the ':' separator and whitespace
             */
            if( argv[j][i] == ':' || isblank(argv[j][i])) {
                continue;
            }
            /*
             * Must start with a digit
             */
            else if( isdigit(argv[j][i]) ) {
                /*
                 * Check: Digits must come first
                 */
                if( p != 0 ) {
                    orte_show_help("help-orte-rmaps-lama.txt",
                                   "invalid mppr option",
                                   true,
                                   layout, "missing digit(s) before resource specification");
                    exit_status = ORTE_ERROR;
                    goto cleanup;
                }

                num[n] = argv[j][i];
                ++n;
                /*
                 * Check: Exceed bound of number of digits
                 */
                if( n >= MAX_BIND_DIGIT_LEN ) {
                    orte_show_help("help-orte-rmaps-lama.txt",
                                   "invalid mppr option",
                                   true,
                                   layout, "too many digits");
                    exit_status = ORTE_ERROR;
                    goto cleanup;
                }
            }
            /*
             * Extract the level
             */
            else {
                /*
                 * Check: Digits must come first
                 */
                if( n == 0 ) {
                    orte_show_help("help-orte-rmaps-lama.txt",
                                   "invalid mppr option",
                                   true,
                                   layout, "missing digit(s) before resource specification");
                    exit_status = ORTE_ERROR;
                    goto cleanup;
                }
                /*
                 * Check: Only one level allowed
                 */
                if( p != 0 ) {
                    orte_show_help("help-orte-rmaps-lama.txt",
                                   "invalid mppr option",
                                   true,
                                   layout, "only one resource type may be listed per specification");
                    exit_status = ORTE_ERROR;
                    goto cleanup;
                }

                /*
                 * L1 : L1 Cache
                 * L2 : L2 Cache
                 * L3 : L3 Cache
                 */
                if( argv[j][i] == 'L' ) {
                    param[0] = argv[j][i];
                    ++i;
                    /*
                     * Check for 2 characters
                     */
                    if( i >= len ) {
                        orte_show_help("help-orte-rmaps-lama.txt",
                                       "invalid mppr option",
                                       true,
                                       layout, "cache level missing number");
                        exit_status = ORTE_ERROR;
                        goto cleanup;
                    }
                    param[1] = argv[j][i];
                    p = 2;
                }
                /*
                 * n : Machine
                 * b : Board
                 * s : Socket
                 * c : Core
                 * h : Hardware Thread
                 * N : NUMA Node
                 */
                else {
                    param[0] = argv[j][i];
                    p = 1;
                }
                param[p] = '\0';
            }
        }

        /*
         * Whitespace, just skip
         */
        if( n == 0 && p == 0 ) {
            continue;
        }

        /*
         * Check that the level was specified
         */
        if( p == 0 ) {
            orte_show_help("help-orte-rmaps-lama.txt",
                           "invalid mppr option",
                           true,
                           layout, "resource type not specified");
            exit_status = ORTE_ERROR;
            goto cleanup;
        }
        num[n] = '\0';

        /*
         * Append level
         */
        *num_types += 1;
        *mppr_levels = (rmaps_lama_level_info_t*)realloc(*mppr_levels, sizeof(rmaps_lama_level_info_t) * (*num_types));
        (*mppr_levels)[(*num_types)-1].type          = lama_type_str_to_enum(param);
        (*mppr_levels)[(*num_types)-1].max_resources = atoi(num);

    }

    /*
     * Check for duplicates and unknowns
     */
    for( i = 0; i < *num_types; ++i ) {
        /*
         * Look for unknown and unsupported options
         */
        if( LAMA_LEVEL_UNKNOWN <= (*mppr_levels)[i].type ) {
            char *msg;
            asprintf(&msg, "unknown resource type at position %d", i + 1);
            orte_show_help("help-orte-rmaps-lama.txt",
                           "invalid mppr option",
                           true,
                           layout, msg);
            free(msg);
            exit_status = ORTE_ERROR;
            goto cleanup;
        }

        /*
         * Look for duplicates
         */
        for( j = i+1; j < *num_types; ++j ) {
            if( (*mppr_levels)[i].type == (*mppr_levels)[j].type ) {
                char *msg;
                asprintf(&msg, "duplicate resource tpyes at position %d and %d",
                         i + 1, j + 1);
                orte_show_help("help-orte-rmaps-lama.txt",
                               "invalid mppr option",
                               true,
                               layout, msg);
                free(msg);
                exit_status = ORTE_ERROR;
                goto cleanup;
            }
        }
    }

 cleanup:
    if( NULL != argv ) {
        opal_argv_free(argv);
        argv = NULL;
    }

    return exit_status;
}

int rmaps_lama_parse_ordering(char *layout,
                              rmaps_lama_order_type_t *order)
{
    /*
     * Default: Natural ordering
     */
    if( NULL == layout ) {
        *order = LAMA_ORDER_NATURAL;
        return ORTE_SUCCESS;
    }

    /*
     * Sequential Ordering
     */
    if( 0 == strncmp(layout, "s", strlen("s")) ||
        0 == strncmp(layout, "S", strlen("S"))  ) {
        *order = LAMA_ORDER_SEQ;
    }
    /*
     * Natural Ordering
     */
    else if( 0 == strncmp(layout, "n", strlen("n")) ||
             0 == strncmp(layout, "N", strlen("N"))  ) {
        *order = LAMA_ORDER_NATURAL;
    }
    /*
     * Check for unknown options
     */
    else {
        orte_show_help("help-orte-rmaps-lama.txt",
                       "invalid ordering option",
                       true,
                       "unsupported ordering option", layout);
        return ORTE_ERROR;
    }

    return ORTE_SUCCESS;
}

bool rmaps_lama_ok_to_prune_level(rmaps_lama_level_type_t level)
{
    int i;

    for( i = 0; i < lama_mapping_num_layouts; ++i ) {
        if( level == lama_mapping_layout[i] ) {
            return false;
        }
    }

    return true;
}

/*********************************
 * Support Functions
 *********************************/
static int lama_parse_int_sort(const void *a, const void *b) {
    int left  = *((int*)a);
    int right = *((int*)b);

    if( left < right ) {
        return -1;
    }
    else if( left > right ) {
        return 1;
    }
    else {
        return 0;
    }
}
