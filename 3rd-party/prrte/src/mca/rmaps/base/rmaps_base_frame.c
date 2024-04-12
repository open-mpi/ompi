/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2021 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "prte_config.h"
#include "constants.h"

#include <string.h>

#include "src/mca/base/pmix_base.h"
#include "src/mca/mca.h"
#include "src/prted/pmix/pmix_server_internal.h"
#include "src/util/pmix_argv.h"
#include "src/util/pmix_output.h"
#include "src/util/pmix_printf.h"
#include "src/util/prte_cmd_line.h"

#include "src/mca/errmgr/errmgr.h"
#include "src/runtime/prte_globals.h"
#include "src/util/pmix_show_help.h"

#include "src/mca/rmaps/base/base.h"
#include "src/mca/rmaps/base/rmaps_private.h"
/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public pmix_mca_base_component_t struct.
 */

#include "src/mca/rmaps/base/static-components.h"

/*
 * Global variables
 */
prte_rmaps_base_t prte_rmaps_base = {
    .selected_modules = PMIX_LIST_STATIC_INIT,
    .mapping = 0,
    .ranking = 0,
    .device = NULL,
    .inherit = false,
    .hwthread_cpus = false,
    .file = NULL,
    .available = NULL,
    .baseset = NULL,
    .default_mapping_policy = NULL
};

/*
 * Local variables
 */
static char *rmaps_base_ranking_policy = NULL;
static bool rmaps_base_inherit = false;

static int prte_rmaps_base_register(pmix_mca_base_register_flag_t flags)
{
    PRTE_HIDE_UNUSED_PARAMS(flags);

    /* define default mapping policy */
    prte_rmaps_base.default_mapping_policy = NULL;
    (void) pmix_mca_base_var_register("prte", "rmaps", "default", "mapping_policy",
                                      "Default mapping Policy [slot | hwthread | core | l1cache | "
                                      "l2cache | l3cache | numa | package | node | seq | dist | ppr | "
                                      "rankfile | pe-list=a,b (comma-delimited ranges of cpus to use for this job)],"
                                      " with supported colon-delimited modifiers: PE=y (for multiple cpus/proc), "
                                      "SPAN, OVERSUBSCRIBE, NOOVERSUBSCRIBE, NOLOCAL, HWTCPUS, CORECPUS, "
                                      "DEVICE=dev (for dist policy), INHERIT, NOINHERIT, ORDERED, FILE=%s (path to file containing sequential "
                                      "or rankfile entries)",
                                      PMIX_MCA_BASE_VAR_TYPE_STRING,
                                      &prte_rmaps_base.default_mapping_policy);

    /* define default ranking policy */
    rmaps_base_ranking_policy = NULL;
    (void) pmix_mca_base_var_register("prte", "rmaps", "default", "ranking_policy",
                                      "Default ranking Policy [slot | node | span | fill]",
                                      PMIX_MCA_BASE_VAR_TYPE_STRING,
                                      &rmaps_base_ranking_policy);

    rmaps_base_inherit = false;
    (void) pmix_mca_base_var_register("prte", "rmaps", "default", "inherit",
                                      "Whether child jobs shall inherit mapping/ranking/binding "
                                      "directives from their parent by default",
                                      PMIX_MCA_BASE_VAR_TYPE_BOOL,
                                      &rmaps_base_inherit);

    return PRTE_SUCCESS;
}

static int prte_rmaps_base_close(void)
{
    pmix_list_item_t *item;

    /* cleanup globals */
    while (NULL != (item = pmix_list_remove_first(&prte_rmaps_base.selected_modules))) {
        PMIX_RELEASE(item);
    }
    PMIX_DESTRUCT(&prte_rmaps_base.selected_modules);
    hwloc_bitmap_free(prte_rmaps_base.available);
    hwloc_bitmap_free(prte_rmaps_base.baseset);

    return pmix_mca_base_framework_components_close(&prte_rmaps_base_framework, NULL);
}

/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
static int prte_rmaps_base_open(pmix_mca_base_open_flag_t flags)
{
    int rc;

    /* init the globals */
    PMIX_CONSTRUCT(&prte_rmaps_base.selected_modules, pmix_list_t);
    prte_rmaps_base.mapping = 0;
    prte_rmaps_base.ranking = 0;
    prte_rmaps_base.inherit = rmaps_base_inherit;
    prte_rmaps_base.hwthread_cpus = false;
    prte_rmaps_base.require_hwtcpus = false;
    prte_rmaps_base.available = hwloc_bitmap_alloc();
    prte_rmaps_base.baseset = hwloc_bitmap_alloc();

    /* set the default mapping and ranking policies */
    if (NULL != prte_rmaps_base.default_mapping_policy) {
        rc = prte_rmaps_base_set_mapping_policy(NULL, prte_rmaps_base.default_mapping_policy);
        if (PRTE_SUCCESS != rc) {
            return rc;
        }
    }

    if (NULL != rmaps_base_ranking_policy) {
        rc = prte_rmaps_base_set_ranking_policy(NULL, rmaps_base_ranking_policy);
        if (PRTE_SUCCESS != rc) {
            return rc;
        }
    }

    /* Open up all available components */
    return pmix_mca_base_framework_components_open(&prte_rmaps_base_framework, flags);
}

PMIX_MCA_BASE_FRAMEWORK_DECLARE(prte, rmaps, "PRTE Mapping Subsystem", prte_rmaps_base_register,
                                prte_rmaps_base_open, prte_rmaps_base_close,
                                prte_rmaps_base_static_components,
                                PMIX_MCA_BASE_FRAMEWORK_FLAG_DEFAULT);

PMIX_CLASS_INSTANCE(prte_rmaps_base_selected_module_t, pmix_list_item_t, NULL, NULL);

static int check_modifiers(char *ck, prte_job_t *jdata, prte_mapping_policy_t *tmp)
{
    char **ck2, *ptr;
    int i;
    uint16_t u16;
    bool inherit_given = false;
    bool noinherit_given = false;
    bool hwthread_cpus_given = false;
    bool core_cpus_given = false;
    bool oversubscribe_given = false;
    bool nooversubscribe_given = false;

    pmix_output_verbose(5, prte_rmaps_base_framework.framework_output,
                        "%s rmaps:base check modifiers with %s",
                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                        (NULL == ck) ? "NULL" : ck);

    if (NULL == ck) {
        return PRTE_SUCCESS;
    }

    ck2 = PMIX_ARGV_SPLIT_COMPAT(ck, ':');
    for (i = 0; NULL != ck2[i]; i++) {
        if (PMIX_CHECK_CLI_OPTION(ck2[i], PRTE_CLI_SPAN)) {
            PRTE_SET_MAPPING_DIRECTIVE(*tmp, PRTE_MAPPING_SPAN);
            PRTE_SET_MAPPING_DIRECTIVE(*tmp, PRTE_MAPPING_GIVEN);

        } else if (PMIX_CHECK_CLI_OPTION(ck2[i], PRTE_CLI_OVERSUB)) {
            if (nooversubscribe_given) {
                /* conflicting directives */
                pmix_show_help("help-prte-rmaps-base.txt", "conflicting-directives", true,
                               "OVERSUBSCRIBE", "NOOVERSUBSCRIBE");
                PMIX_ARGV_FREE_COMPAT(ck2);
                return PRTE_ERR_SILENT;
            }
            PRTE_UNSET_MAPPING_DIRECTIVE(*tmp, PRTE_MAPPING_NO_OVERSUBSCRIBE);
            PRTE_SET_MAPPING_DIRECTIVE(*tmp, PRTE_MAPPING_SUBSCRIBE_GIVEN);
            oversubscribe_given = true;

        } else if (PMIX_CHECK_CLI_OPTION(ck2[i], PRTE_CLI_NOOVER)) {
            if (oversubscribe_given) {
                /* conflicting directives */
                pmix_show_help("help-prte-rmaps-base.txt", "conflicting-directives", true,
                               "OVERSUBSCRIBE", "NOOVERSUBSCRIBE");
                PMIX_ARGV_FREE_COMPAT(ck2);
                return PRTE_ERR_SILENT;
            }
            PRTE_SET_MAPPING_DIRECTIVE(*tmp, PRTE_MAPPING_NO_OVERSUBSCRIBE);
            PRTE_SET_MAPPING_DIRECTIVE(*tmp, PRTE_MAPPING_SUBSCRIBE_GIVEN);
            nooversubscribe_given = true;

        } else if (PMIX_CHECK_CLI_OPTION(ck2[i], PRTE_CLI_NOLOCAL)) {
            PRTE_SET_MAPPING_DIRECTIVE(*tmp, PRTE_MAPPING_NO_USE_LOCAL);

        } else if (PMIX_CHECK_CLI_OPTION(ck2[i], PRTE_CLI_ORDERED)) {
            if (NULL == jdata) {
                pmix_show_help("help-prte-rmaps-base.txt", "unsupported-default-modifier", true,
                               "mapping policy", ck2[i]);
                return PRTE_ERR_SILENT;
            }
            PRTE_SET_MAPPING_DIRECTIVE(*tmp, PRTE_MAPPING_ORDERED);

        } else if (PMIX_CHECK_CLI_OPTION(ck2[i], PRTE_CLI_PE)) {
            if (NULL == jdata) {
                pmix_show_help("help-prte-rmaps-base.txt", "unsupported-default-modifier", true,
                               "mapping policy", ck2[i]);
                return PRTE_ERR_SILENT;
            }
            /* Numeric value must immediately follow '=' (PE=2) */
            u16 = strtol(&ck2[i][3], &ptr, 10);
            if ('\0' != *ptr) {
                /* missing the value or value is invalid */
                pmix_show_help("help-prte-rmaps-base.txt", "invalid-value", true, "mapping policy",
                               "PE", ck2[i]);
                PMIX_ARGV_FREE_COMPAT(ck2);
                return PRTE_ERR_SILENT;
            }
            prte_set_attribute(&jdata->attributes, PRTE_JOB_PES_PER_PROC, PRTE_ATTR_GLOBAL,
                               &u16, PMIX_UINT16);

        } else if (PMIX_CHECK_CLI_OPTION(ck2[i], PRTE_CLI_INHERIT)) {
            if (noinherit_given) {
                /* conflicting directives */
                pmix_show_help("help-prte-rmaps-base.txt", "conflicting-directives", true,
                               "INHERIT", "NOINHERIT");
                PMIX_ARGV_FREE_COMPAT(ck2);
                return PRTE_ERR_SILENT;
            }
            if (NULL == jdata) {
                prte_rmaps_base.inherit = true;
            } else {
                prte_set_attribute(&jdata->attributes, PRTE_JOB_INHERIT, PRTE_ATTR_GLOBAL,
                                   NULL, PMIX_BOOL);
            }
            inherit_given = true;

        } else if (PMIX_CHECK_CLI_OPTION(ck2[i], PRTE_CLI_NOINHERIT)) {
            if (inherit_given) {
                /* conflicting directives */
                pmix_show_help("help-prte-rmaps-base.txt", "conflicting-directives", true,
                               "INHERIT", "NOINHERIT");
                PMIX_ARGV_FREE_COMPAT(ck2);
                return PRTE_ERR_SILENT;
            }
            if (NULL == jdata) {
                prte_rmaps_base.inherit = false;
            } else {
                prte_set_attribute(&jdata->attributes, PRTE_JOB_NOINHERIT, PRTE_ATTR_GLOBAL,
                                   NULL, PMIX_BOOL);
            }
            noinherit_given = true;

        } else if (PMIX_CHECK_CLI_OPTION(ck2[i], PRTE_CLI_HWTCPUS)) {
            if (core_cpus_given) {
                pmix_show_help("help-prte-rmaps-base.txt", "conflicting-directives", true,
                               "HWTCPUS", "CORECPUS");
                PMIX_ARGV_FREE_COMPAT(ck2);
                return PRTE_ERR_SILENT;
            }
            if (NULL == jdata) {
                prte_rmaps_base.hwthread_cpus = true;
            } else {
                prte_set_attribute(&jdata->attributes, PRTE_JOB_HWT_CPUS, PRTE_ATTR_GLOBAL,
                                   NULL, PMIX_BOOL);
            }
            hwthread_cpus_given = true;

        } else if (PMIX_CHECK_CLI_OPTION(ck2[i], PRTE_CLI_CORECPUS)) {
            if (hwthread_cpus_given) {
                pmix_show_help("help-prte-rmaps-base.txt", "conflicting-directives", true,
                               "HWTCPUS", "CORECPUS");
                PMIX_ARGV_FREE_COMPAT(ck2);
                return PRTE_ERR_SILENT;
            }
            if (prte_rmaps_base.require_hwtcpus) {
                if (NULL == jdata) {
                    prte_rmaps_base.hwthread_cpus = true;
                } else {
                    prte_set_attribute(&jdata->attributes, PRTE_JOB_HWT_CPUS, PRTE_ATTR_GLOBAL,
                                       NULL, PMIX_BOOL);
                }
            } else {
                if (NULL == jdata) {
                    prte_rmaps_base.hwthread_cpus = false;
                } else {
                    prte_set_attribute(&jdata->attributes, PRTE_JOB_CORE_CPUS,
                                       PRTE_ATTR_GLOBAL, NULL, PMIX_BOOL);
                }
            }
            core_cpus_given = true;

        } else if (PMIX_CHECK_CLI_OPTION(ck2[i], PRTE_CLI_QFILE)) {
            if ('\0' == ck2[i][5]) {
                /* missing the value */
                pmix_show_help("help-prte-rmaps-base.txt", "missing-value", true, "mapping policy",
                               "FILE", ck2[i]);
                PMIX_ARGV_FREE_COMPAT(ck2);
                return PRTE_ERR_SILENT;
            }
            if (NULL == jdata) {
                prte_rmaps_base.file = strdup(&ck2[i][5]);
            } else {
                prte_set_attribute(&jdata->attributes, PRTE_JOB_FILE, PRTE_ATTR_GLOBAL,
                                   &ck2[i][5], PMIX_STRING);
            }

        } else {
            /* unrecognized modifier */
            PMIX_ARGV_FREE_COMPAT(ck2);
            return PRTE_ERR_BAD_PARAM;
        }
    }
    PMIX_ARGV_FREE_COMPAT(ck2);
    return PRTE_SUCCESS;
}

int prte_rmaps_base_set_default_mapping(prte_job_t *jdata,
                                        prte_rmaps_options_t *options)
{
    prte_binding_policy_t bind;

    if (1 < options->cpus_per_rank) {
        /* assigning multiple cpus to a rank requires that we map to
         * objects that have multiple cpus in them, so default
         * to byslot if nothing else was specified by the user.
         */
        pmix_output_verbose(5, prte_rmaps_base_framework.framework_output,
                            "mca:rmaps mapping not given with multiple cpus/rank - using byslot");
        PRTE_SET_MAPPING_POLICY(jdata->map->mapping, PRTE_MAPPING_BYSLOT);

    } else if (PRTE_BINDING_POLICY_IS_SET(jdata->map->binding)) {
        /* they specified a binding policy, but not a mapping policy, so
         * map by the binding object */
        bind = PRTE_GET_BINDING_POLICY(jdata->map->binding);
        switch(bind) {
            case PRTE_BIND_TO_NONE:
                PRTE_SET_MAPPING_POLICY(jdata->map->mapping, PRTE_MAPPING_BYSLOT);
                break;
            case PRTE_BIND_TO_PACKAGE:
                PRTE_SET_MAPPING_POLICY(jdata->map->mapping, PRTE_MAPPING_BYPACKAGE);
                break;
            case PRTE_BIND_TO_NUMA:
                PRTE_SET_MAPPING_POLICY(jdata->map->mapping, PRTE_MAPPING_BYNUMA);
                break;
            case PRTE_BIND_TO_L3CACHE:
                PRTE_SET_MAPPING_POLICY(jdata->map->mapping, PRTE_MAPPING_BYL3CACHE);
                break;
            case PRTE_BIND_TO_L2CACHE:
                PRTE_SET_MAPPING_POLICY(jdata->map->mapping, PRTE_MAPPING_BYL2CACHE);
                break;
            case PRTE_BIND_TO_L1CACHE:
                PRTE_SET_MAPPING_POLICY(jdata->map->mapping, PRTE_MAPPING_BYL1CACHE);
                break;
            case PRTE_BIND_TO_CORE:
                PRTE_SET_MAPPING_POLICY(jdata->map->mapping, PRTE_MAPPING_BYCORE);
                break;
            case PRTE_BIND_TO_HWTHREAD:
                PRTE_SET_MAPPING_POLICY(jdata->map->mapping, PRTE_MAPPING_BYHWTHREAD);
                break;
            default:
                PRTE_SET_MAPPING_POLICY(jdata->map->mapping, PRTE_MAPPING_BYSLOT);
                break;
        }
        pmix_output_verbose(5, prte_rmaps_base_framework.framework_output,
                            "mca:rmaps mapping not given but binding set - using %s",
                            prte_rmaps_base_print_mapping(jdata->map->mapping));

    } else {
        if (options->nprocs <= 2) {
            // map by whatever CPU we are using
            if (options->use_hwthreads) {
                /* if we are using hwthread cpus, then map by those */
                pmix_output_verbose(options->verbosity, options->stream,
                                    "mca:rmaps mapping not given - using byhwthread");
                PRTE_SET_MAPPING_POLICY(jdata->map->mapping, PRTE_MAPPING_BYHWTHREAD);
            } else {
                /* otherwise map by core */
                pmix_output_verbose(options->verbosity, options->stream,
                                    "mca:rmaps mapping not given - using bycore");
                PRTE_SET_MAPPING_POLICY(jdata->map->mapping, PRTE_MAPPING_BYCORE);
            }
        } else {
            /* if package is available, map by that */
            if (NULL != hwloc_get_obj_by_type(prte_hwloc_topology, HWLOC_OBJ_PACKAGE, 0)) {
                pmix_output_verbose(5, prte_rmaps_base_framework.framework_output,
                                    "mca:rmaps mapping not set by user - using bypackage");
                PRTE_SET_MAPPING_POLICY(jdata->map->mapping, PRTE_MAPPING_BYPACKAGE);
            } else if (NULL != hwloc_get_obj_by_type(prte_hwloc_topology, HWLOC_OBJ_NUMANODE, 0)) {
                /* if NUMA is available, map by that */
                pmix_output_verbose(5, prte_rmaps_base_framework.framework_output,
                                    "mca:rmaps mapping not set by user - using bynuma");
                PRTE_SET_MAPPING_POLICY(jdata->map->mapping, PRTE_MAPPING_BYNUMA);
            } else {
                /* if we have neither, then just do by slot */
                pmix_output_verbose(5, prte_rmaps_base_framework.framework_output,
                                    "mca:rmaps mapping not given and no packages/NUMAs - using byslot");
                PRTE_SET_MAPPING_POLICY(jdata->map->mapping, PRTE_MAPPING_BYSLOT);
            }
        }
    }
    return PRTE_SUCCESS;
}

int prte_rmaps_base_set_mapping_policy(prte_job_t *jdata, char *inspec)
{
    char **ck, **range;
    char *ptr, *cptr, *val;
    prte_mapping_policy_t tmp;
    int rc, n;
    bool ppr = false;
    char *temp_parm, *temp_token, *parm_delimiter;

    /* set defaults */
    tmp = 0;

    pmix_output_verbose(5, prte_rmaps_base_framework.framework_output,
                        "%s rmaps:base set policy with %s",
                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                        (NULL == inspec) ? "NULL" : inspec);

    if (NULL == inspec) {
        return PRTE_SUCCESS;
    }

    /* see if a colon was included - if so, then we have a modifier */
    ck = PMIX_ARGV_SPLIT_COMPAT(inspec, ':');
    if (1 < PMIX_ARGV_COUNT_COMPAT(ck)) {
        if (0 == strcasecmp(ck[0], "ppr")) {
            if (4 < pmix_output_get_verbosity(prte_rmaps_base_framework.framework_output)) {
                pmix_asprintf(&cptr, "%s:%s", ck[1], ck[2]);
                pmix_output(prte_rmaps_base_framework.framework_output,
                            "%s rmaps:base policy %s modifiers %s provided",
                            PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), ck[0], cptr);
                free(cptr);
            }
            /* at this point, ck contains at least
             * two fields (specifying the #procs/obj and the object we are
             * to map by). we have to allow additional modifiers here - e.g.,
             * specifying #pe's/proc or oversubscribe - so check for modifiers. if
             * they are present, the rest of ck will look like "N:obj:mod1,mod2,mod3"
             */
            if (3 > PMIX_ARGV_COUNT_COMPAT(ck)) {
                /* this is an error - there had to be at least one
                 * colon to delimit the number from the object type
                 */
                pmix_show_help("help-prte-rmaps-base.txt", "invalid-pattern", true, inspec);
                PMIX_ARGV_FREE_COMPAT(ck);
                return PRTE_ERR_SILENT;
            }
            /* at this point, ck[1] contains the number of procs/resource,
             * ck[2] contains the string that describes
             * the object and ck[3] is either NULL or contains any
             * modifiers (i.e., "#:obj:mod1,mod2") */
            if (NULL != jdata) {
                /* save the pattern */
                pmix_asprintf(&cptr, "%s:%s", ck[1], ck[2]);
                prte_set_attribute(&jdata->attributes, PRTE_JOB_PPR, PRTE_ATTR_GLOBAL, cptr, PMIX_STRING);
                free(cptr);
            }
            PRTE_SET_MAPPING_POLICY(tmp, PRTE_MAPPING_PPR);
            PRTE_SET_MAPPING_DIRECTIVE(tmp, PRTE_MAPPING_GIVEN);
            ppr = true;
            if (NULL == ck[3]) {
                /* there are no modifiers, so we are done */
                PMIX_ARGV_FREE_COMPAT(ck);
                goto setpolicy;
            }
            cptr = ck[3];
        } else {
            pmix_output_verbose(5, prte_rmaps_base_framework.framework_output,
                                "%s rmaps:base policy %s modifiers %s provided",
                                PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), ck[0], ck[1]);
            cptr = ck[1];
        }
        if (PRTE_SUCCESS != (rc = check_modifiers(cptr, jdata, &tmp)) &&
            PRTE_ERR_TAKE_NEXT_OPTION != rc) {
            if (PRTE_ERR_BAD_PARAM == rc) {
                pmix_show_help("help-prte-rmaps-base.txt", "unrecognized-modifier", true, inspec);
                rc = PRTE_ERR_SILENT;
            }
            PMIX_ARGV_FREE_COMPAT(ck);
            return rc;
        }
        if (ppr) {
            /* we are done */
            PMIX_ARGV_FREE_COMPAT(ck);
            goto setpolicy;
        }
    }

    /* we get here if there was only one element in
     * the ck array. However, if the input string
     * started with a ':', then there is no specified
     * mapping policy for us to set, but we still
     * have to process the modifier */
    if (':' == inspec[0]) {
        if (PRTE_SUCCESS != (rc = check_modifiers(&inspec[1], jdata, &tmp)) &&
            PRTE_ERR_TAKE_NEXT_OPTION != rc) {
            if (PRTE_ERR_BAD_PARAM == rc) {
                pmix_show_help("help-prte-rmaps-base.txt", "unrecognized-modifier", true, inspec);
                rc = PRTE_ERR_SILENT;
            }
            PMIX_ARGV_FREE_COMPAT(ck);
            return rc;
        }
        PMIX_ARGV_FREE_COMPAT(ck);
        goto setpolicy;
    }

    cptr = ck[0];
    // check for an '=' and split out the value from the option
    if (NULL != (ptr = strchr(ck[0], '='))) {
        *ptr = '\0';
        cptr = strdup(ck[0]);
        *ptr = '='; // restore the option
        ++ptr;
        if (NULL == ptr) {
            /* malformed option */
            pmix_show_help("help-prte-rmaps-base.txt", "unrecognized-policy",
                           true, "mapping", ck[0]);
            PMIX_ARGV_FREE_COMPAT(ck);
            free(cptr);
            return PRTE_ERR_SILENT;
        }
        val = strdup(ptr);
    } else {
        cptr = strdup(ck[0]);
        val = NULL;
    }

    if (PMIX_CHECK_CLI_OPTION(cptr, PRTE_CLI_SLOT)) {
        PRTE_SET_MAPPING_POLICY(tmp, PRTE_MAPPING_BYSLOT);

    } else if (PMIX_CHECK_CLI_OPTION(cptr, PRTE_CLI_NODE)) {
        PRTE_SET_MAPPING_POLICY(tmp, PRTE_MAPPING_BYNODE);

    } else if (PMIX_CHECK_CLI_OPTION(cptr, PRTE_CLI_SEQ)) {
        /* there are several mechanisms by which the file specifying
         * the sequence can be passed, so not really feasible to check
         * it here */
        PRTE_SET_MAPPING_POLICY(tmp, PRTE_MAPPING_SEQ);

    } else if (PMIX_CHECK_CLI_OPTION(cptr, PRTE_CLI_CORE)) {
        if (prte_rmaps_base.require_hwtcpus) {
            PRTE_SET_MAPPING_POLICY(tmp, PRTE_MAPPING_BYHWTHREAD);
        } else {
            PRTE_SET_MAPPING_POLICY(tmp, PRTE_MAPPING_BYCORE);
        }

    } else if (PMIX_CHECK_CLI_OPTION(cptr, PRTE_CLI_L1CACHE)) {
        PRTE_SET_MAPPING_POLICY(tmp, PRTE_MAPPING_BYL1CACHE);

    } else if (PMIX_CHECK_CLI_OPTION(cptr, PRTE_CLI_L2CACHE)) {
        PRTE_SET_MAPPING_POLICY(tmp, PRTE_MAPPING_BYL2CACHE);

    } else if (PMIX_CHECK_CLI_OPTION(cptr, PRTE_CLI_L3CACHE)) {
        PRTE_SET_MAPPING_POLICY(tmp, PRTE_MAPPING_BYL3CACHE);

    } else if (PMIX_CHECK_CLI_OPTION(cptr, PRTE_CLI_NUMA)) {
        PRTE_SET_MAPPING_POLICY(tmp, PRTE_MAPPING_BYNUMA);

    } else if (PMIX_CHECK_CLI_OPTION(cptr, PRTE_CLI_PACKAGE)) {
        PRTE_SET_MAPPING_POLICY(tmp, PRTE_MAPPING_BYPACKAGE);

    } else if (PMIX_CHECK_CLI_OPTION(cptr, PRTE_CLI_RANKFILE)) {
        /* check that the file was given */
        if ((NULL == jdata && NULL == prte_rmaps_base.file) ||
            (NULL != jdata && !prte_get_attribute(&jdata->attributes, PRTE_JOB_FILE, NULL, PMIX_STRING))) {
            pmix_show_help("help-prte-rmaps-base.txt", "rankfile-no-filename", true);
            PMIX_ARGV_FREE_COMPAT(ck);
            free(cptr);
            if (NULL != val) {
                free(val);
            }
            return PRTE_ERR_BAD_PARAM;
        }
        /* if they asked for rankfile and didn't specify one, but did
         * provide one via MCA param, then use it */
        if (NULL != jdata) {
            if (!prte_get_attribute(&jdata->attributes, PRTE_JOB_FILE, NULL, PMIX_STRING)) {
                if (NULL == prte_rmaps_base.file) {
                    /* also not allowed */
                    pmix_show_help("help-prte-rmaps-base.txt", "rankfile-no-filename", true);
                    PMIX_ARGV_FREE_COMPAT(ck);
                    free(cptr);
                    if (NULL != val) {
                        free(val);
                    }
                    return PRTE_ERR_BAD_PARAM;
                }
                prte_set_attribute(&jdata->attributes, PRTE_JOB_FILE, PRTE_ATTR_GLOBAL,
                                   prte_rmaps_base.file, PMIX_STRING);
            }
        }
        PRTE_SET_MAPPING_POLICY(tmp, PRTE_MAPPING_BYUSER);

    } else if (PMIX_CHECK_CLI_OPTION(cptr, PRTE_CLI_HWT)) {
        PRTE_SET_MAPPING_POLICY(tmp, PRTE_MAPPING_BYHWTHREAD);
        /* if we are mapping processes to individual hwthreads, then
         * we need to treat those hwthreads as separate cpus
         */
        if (NULL == jdata) {
            prte_rmaps_base.hwthread_cpus = true;
        } else {
            prte_set_attribute(&jdata->attributes, PRTE_JOB_HWT_CPUS, PRTE_ATTR_GLOBAL,
                               NULL, PMIX_BOOL);
        }

    } else if (PMIX_CHECK_CLI_OPTION(cptr, PRTE_CLI_PELIST)) {
        if (NULL == jdata) {
            pmix_show_help("help-prte-rmaps-base.txt", "unsupported-default-policy", true,
                           "mapping", cptr);
            PMIX_ARGV_FREE_COMPAT(ck);
            free(cptr);
            if (NULL != val) {
                free(val);
            }
            return PRTE_ERR_SILENT;
        }
        if (NULL == val) {
            /* malformed option */
            pmix_show_help("help-prte-rmaps-base.txt", "unrecognized-policy",
                           true, "mapping", ck[0]);
            PMIX_ARGV_FREE_COMPAT(ck);
            free(cptr);
            if (NULL != val) {
                free(val);
            }
            return PRTE_ERR_SILENT;
        }
        /* Verify the list is composed of comma-delimited ranges */
        temp_token = strtok(val, ",");
        while (NULL != temp_token) {
            // check for range
            range = PMIX_ARGV_SPLIT_COMPAT(temp_token, '-');
            if (2 < PMIX_ARGV_COUNT_COMPAT(range)) {
                // can only have one '-' delimiter
                pmix_show_help("help-prte-rmaps-base.txt", "invalid-value", true,
                               "mapping", "PE-LIST", ck[0]);
                PMIX_ARGV_FREE_COMPAT(ck);
                PMIX_ARGV_FREE_COMPAT(range);
                free(cptr);
                if (NULL != val) {
                    free(val);
                }
             }
             for (n=0; NULL != range[n]; n++) {
                (void)strtol(range[n], &parm_delimiter, 10);
                if ('\0' != *parm_delimiter) {
                    pmix_show_help("help-prte-rmaps-base.txt", "invalid-value", true,
                                   "mapping", "PE-LIST", val);
                    PMIX_ARGV_FREE_COMPAT(ck);
                    PMIX_ARGV_FREE_COMPAT(range);
                    free(cptr);
                    if (NULL != val) {
                        free(val);
                    }
                    return PRTE_ERR_SILENT;
                }
            }
            PMIX_ARGV_FREE_COMPAT(range);
            temp_token = strtok(NULL, ",");
        }
        prte_set_attribute(&jdata->attributes, PRTE_JOB_CPUSET, PRTE_ATTR_GLOBAL,
                           ptr, PMIX_STRING);
        PRTE_SET_MAPPING_POLICY(tmp, PRTE_MAPPING_PELIST);
        PRTE_SET_MAPPING_DIRECTIVE(tmp, PRTE_MAPPING_GIVEN);

    } else {
        pmix_show_help("help-prte-rmaps-base.txt", "unrecognized-policy",
                       true, "mapping", cptr);
        PMIX_ARGV_FREE_COMPAT(ck);
        free(cptr);
        if (NULL != val) {
            free(val);
        }
        return PRTE_ERR_SILENT;
    }
    PMIX_ARGV_FREE_COMPAT(ck);
    free(cptr);
    if (NULL != val) {
        free(val);
    }
    PRTE_SET_MAPPING_DIRECTIVE(tmp, PRTE_MAPPING_GIVEN);

setpolicy:
    if (NULL == jdata) {
        prte_rmaps_base.mapping = tmp;
    } else {
        if (NULL == jdata->map) {
            PRTE_ERROR_LOG(PRTE_ERR_BAD_PARAM);
            return PRTE_ERR_BAD_PARAM;
        }
        jdata->map->mapping = tmp;
    }

    return PRTE_SUCCESS;
}

int prte_rmaps_base_set_default_ranking(prte_job_t *jdata,
                                        prte_rmaps_options_t *options)
{
    int rc;
    PRTE_HIDE_UNUSED_PARAMS(options);

    rc = prte_rmaps_base_set_ranking_policy(jdata, NULL);
    return rc;
}

int prte_rmaps_base_set_ranking_policy(prte_job_t *jdata, char *spec)
{
    prte_ranking_policy_t tmp;
    prte_mapping_policy_t mapping;

    /* set default */
    tmp = 0;

    if (NULL == spec) {
        if (NULL == jdata) {
            return PRTE_SUCCESS;
        }
        if (NULL == jdata->map) {
            jdata->map = PMIX_NEW(prte_job_map_t);
        }
        if (PRTE_RANKING_POLICY_IS_SET(jdata->map->ranking)) {
            return PRTE_SUCCESS;
        }
        mapping = PRTE_GET_MAPPING_POLICY(jdata->map->mapping);
        /* if mapping by-node, then default to rank-by node */
        if (PRTE_MAPPING_BYNODE == mapping) {
            PRTE_SET_RANKING_POLICY(tmp, PRTE_RANK_BY_NODE);

        } else if (PRTE_MAPPING_BYSLOT == mapping) {
            /* default to by-slot */
            PRTE_SET_RANKING_POLICY(tmp, PRTE_RANK_BY_SLOT);

        } else if (PRTE_MAPPING_SPAN & PRTE_GET_MAPPING_DIRECTIVE(jdata->map->mapping)) {
            /* default to by-span */
            PRTE_SET_RANKING_POLICY(tmp, PRTE_RANK_BY_SPAN);

        } else if (PRTE_MAPPING_BYHWTHREAD >= mapping &&
                   PRTE_MAPPING_BYNUMA <= mapping) {
            /* default to by-slot */
            PRTE_SET_RANKING_POLICY(tmp, PRTE_RANK_BY_FILL);

        } else {
            /* default to slot */
            PRTE_SET_RANKING_POLICY(tmp, PRTE_RANK_BY_SLOT);
        }
        jdata->map->ranking = tmp;
        return PRTE_SUCCESS;
    }

    if (PMIX_CHECK_CLI_OPTION(spec, PRTE_CLI_SLOT)) {
        PRTE_SET_RANKING_POLICY(tmp, PRTE_RANK_BY_SLOT);

    } else if (PMIX_CHECK_CLI_OPTION(spec, PRTE_CLI_NODE)) {
        PRTE_SET_RANKING_POLICY(tmp, PRTE_RANK_BY_NODE);

    } else if (PMIX_CHECK_CLI_OPTION(spec, PRTE_CLI_FILL)) {
        PRTE_SET_RANKING_POLICY(tmp, PRTE_RANK_BY_FILL);

    } else if (PMIX_CHECK_CLI_OPTION(spec, PRTE_CLI_SPAN)) {
        PRTE_SET_RANKING_POLICY(tmp, PRTE_RANK_BY_SPAN);

    } else {
        pmix_show_help("help-prte-rmaps-base.txt", "unrecognized-policy", true,
                       "ranking", spec);
        return PRTE_ERR_SILENT;
    }
    PRTE_SET_RANKING_DIRECTIVE(tmp, PRTE_RANKING_GIVEN);

    if (NULL == jdata) {
        prte_rmaps_base.ranking = tmp;
    } else {
        if (NULL == jdata->map) {
            jdata->map = PMIX_NEW(prte_job_map_t);
        }
        jdata->map->ranking = tmp;
    }

    return PRTE_SUCCESS;
}
