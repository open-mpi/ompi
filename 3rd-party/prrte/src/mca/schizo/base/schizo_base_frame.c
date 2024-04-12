/*
 * Copyright (c) 2015-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
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
#include "src/util/pmix_output.h"
#include "src/util/pmix_argv.h"
#include "src/util/pmix_os_dirpath.h"
#include "src/util/pmix_os_path.h"
#include "src/util/pmix_path.h"
#include "src/util/prte_cmd_line.h"
#include "src/util/pmix_show_help.h"

#include "src/mca/errmgr/errmgr.h"
#include "src/runtime/prte_globals.h"

#include "src/mca/schizo/base/base.h"
/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public pmix_mca_base_component_t struct.
 */

#include "src/mca/schizo/base/static-components.h"

/*
 * Global variables
 */
prte_schizo_base_t prte_schizo_base = {
    .active_modules = PMIX_LIST_STATIC_INIT,
    .test_proxy_launch = false
};

static int prte_schizo_base_register(pmix_mca_base_register_flag_t flags)
{
    PRTE_HIDE_UNUSED_PARAMS(flags);

    /* test proxy launch */
    prte_schizo_base.test_proxy_launch = false;
    pmix_mca_base_var_register("prte", "schizo", "base", "test_proxy_launch",
                               "Test proxy launches",
                               PMIX_MCA_BASE_VAR_TYPE_BOOL,
                               &prte_schizo_base.test_proxy_launch);
    return PRTE_SUCCESS;
}

static int prte_schizo_base_close(void)
{
    /* cleanup globals */
    PMIX_LIST_DESTRUCT(&prte_schizo_base.active_modules);

    return pmix_mca_base_framework_components_close(&prte_schizo_base_framework, NULL);
}

/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
static int prte_schizo_base_open(pmix_mca_base_open_flag_t flags)
{
    int rc;

    /* init the globals */
    PMIX_CONSTRUCT(&prte_schizo_base.active_modules, pmix_list_t);

    /* Open up all available components */
    rc = pmix_mca_base_framework_components_open(&prte_schizo_base_framework, flags);

    /* All done */
    return rc;
}

PMIX_MCA_BASE_FRAMEWORK_DECLARE(prte, schizo, "PRTE Schizo Subsystem", prte_schizo_base_register,
                                prte_schizo_base_open, prte_schizo_base_close,
                                prte_schizo_base_static_components,
                                PMIX_MCA_BASE_FRAMEWORK_FLAG_DEFAULT);

void prte_schizo_base_expose(char *param, char *prefix)
{
    char *value, *pm;

    value = strchr(param, '=');
    *value = '\0';
    ++value;
    pmix_asprintf(&pm, "%s%s", prefix, param);
    setenv(pm, value, true);
    free(pm);
    --value;
    *value = '=';
}

bool prte_schizo_base_check_qualifiers(char *directive,
                                       char **valid,
                                       char *qual)
{
    size_t n;
    char *v;

    for (n=0; NULL != valid[n]; n++) {
        if (PMIX_CHECK_CLI_OPTION(valid[n], qual)) {
            return true;
        }
    }
    v = PMIX_ARGV_JOIN_COMPAT(valid, ',');
    pmix_show_help("help-prte-rmaps-base.txt",
                   "unrecognized-qualifier", true,
                   directive, qual, v);
    free(v);
    return false;
}

bool prte_schizo_base_check_directives(char *directive,
                                       char **valid,
                                       char **quals,
                                       char *dir)
{
    size_t n, m;
    char **args, **qls, *v, *q;
    char *pproptions[] = {
        PRTE_CLI_SLOT,
        PRTE_CLI_HWT,
        PRTE_CLI_CORE,
        PRTE_CLI_L1CACHE,
        PRTE_CLI_L2CACHE,
        PRTE_CLI_L3CACHE,
        PRTE_CLI_NUMA,
        PRTE_CLI_PACKAGE,
        PRTE_CLI_NODE,
        NULL
    };
    bool found;

    /* if it starts with a ':', then these are just qualifiers */
    if (':' == dir[0]) {
        qls = PMIX_ARGV_SPLIT_COMPAT(&dir[1], ':');
        for (m=0; NULL != qls[m]; m++) {
            if (!prte_schizo_base_check_qualifiers(directive, quals, qls[m])) {
                PMIX_ARGV_FREE_COMPAT(qls);
                return false;
            }
        }
        PMIX_ARGV_FREE_COMPAT(qls);
        return true;
    }

    /* always accept the "help" directive */
    if (0 == strcasecmp(dir, "help") ||
        0 == strcasecmp(dir, "-help") ||
        0 == strcasecmp(dir, "--help")) {
        return true;
    }

    args = PMIX_ARGV_SPLIT_COMPAT(dir, ':');
    /* remove any '=' in the directive */
    if (NULL != (v = strchr(args[0], '='))) {
        *v = '\0';
    }
    for (n = 0; NULL != valid[n]; n++) {
        if (PMIX_CHECK_CLI_OPTION(args[0], valid[n])) {
            /* valid directive - check any qualifiers */
            if (NULL != args[1] && NULL != quals) {
                if (0 == strcmp(directive, PRTE_CLI_MAPBY) &&
                    0 == strcmp(args[0], PRTE_CLI_PPR)) {
                    /* unfortunately, this is a special case that
                     * must be checked separately due to the format
                     * of the qualifier */
                    if (3 > PMIX_ARGV_COUNT_COMPAT(args)) {
                        /* this is an error as there must be at least
                         * the "ppr" directive, a number, and then the
                         * resource type. There may also be additional
                         * qualifiers given, so the count could be greater
                         * than 3 - but it has to at least contain
                         * those three fields */
                        pmix_show_help("help-prte-rmaps-base.txt",
                                       "invalid-pattern", true,
                                       dir);
                        PMIX_ARGV_FREE_COMPAT(args);
                        return false;
                    }
                    v = NULL;
                    m = strtoul(args[1], &v, 10);
                    if (NULL != v && 0 < strlen(v)) {
                        /* the first entry had to be a pure number */
                        pmix_asprintf(&v, "ppr:[Number of procs/object]:%s", args[2]);
                        pmix_show_help("help-prte-rmaps-base.txt",
                                       "unrecognized-qualifier", true,
                                       directive, dir, v);
                        free(v);
                        PMIX_ARGV_FREE_COMPAT(args);
                        return false;
                    }
                    found = false;
                    for (m=0; NULL != pproptions[m]; m++) {
                        if (0 == strncasecmp(args[2], pproptions[m], strlen(args[2]))) {
                            found = true;
                            break;
                        }
                    }
                    if (!found) {
                        v = PMIX_ARGV_JOIN_COMPAT(pproptions, ':');
                        pmix_asprintf(&q, "ppr:%s:[%s]", args[1], v);
                        free(v);
                        pmix_show_help("help-prte-rmaps-base.txt",
                                       "unrecognized-qualifier", true,
                                       directive, dir, q);
                        free(q);
                        PMIX_ARGV_FREE_COMPAT(args);
                        return false;
                    }
                    if (NULL != args[3]) {
                        qls = PMIX_ARGV_SPLIT_COMPAT(args[3], ':');
                    } else {
                        PMIX_ARGV_FREE_COMPAT(args);
                        return true;
                    }
                } else {
                    qls = PMIX_ARGV_SPLIT_COMPAT(args[1], ':');
                }
               for (m=0; NULL != qls[m]; m++) {
                    if (!prte_schizo_base_check_qualifiers(directive, quals, qls[m])) {
                        PMIX_ARGV_FREE_COMPAT(qls);
                        PMIX_ARGV_FREE_COMPAT(args);
                        return false;
                    }
                }
                PMIX_ARGV_FREE_COMPAT(qls);
                PMIX_ARGV_FREE_COMPAT(args);
                return true;
            }
            PMIX_ARGV_FREE_COMPAT(args);
            return true;
        }
    }
    v = PMIX_ARGV_JOIN_COMPAT(valid, ':');
    pmix_show_help("help-prte-rmaps-base.txt",
                   "unrecognized-directive", true,
                   directive, dir, v);
    PMIX_ARGV_FREE_COMPAT(args);
    return false;
}

typedef struct {
    const char *alias;
    const char *name;
} prte_synonym_t;

static prte_synonym_t synonyms[] = {
    {.alias = PRTE_CLI_MACHINEFILE, .name = PRTE_CLI_HOSTFILE},
    {.alias = PRTE_CLI_WD, .name = PRTE_CLI_WDIR},
    {.alias = NULL, .name = NULL}
};

static const char* check_synonym(const char *alias)
{
    int n;

    for (n=0; NULL != synonyms[n].alias; n++) {
        if (0 == strcmp(alias, synonyms[n].alias)) {
            return synonyms[n].name;
        }
    }
    return NULL;
}

static char *limits[] = {
    PRTE_CLI_PATH,
    PRTE_CLI_WDIR,
    PRTE_CLI_PSET,
    PRTE_CLI_NP,
    PRTE_CLI_KEEPALIVE,
    NULL
};

static int check_ndirs(pmix_cli_item_t *opt)
{
    int n, count;
    char *param;

    for (n=0; NULL != limits[n]; n++) {
        if (0 == strcmp(opt->key, limits[n])) {
            count = PMIX_ARGV_COUNT_COMPAT(opt->values);
            if (1 > count) {
                param = PMIX_ARGV_JOIN_COMPAT(opt->values, ' ');
                pmix_show_help("help-schizo-base.txt", "too-many-instances", true,
                               param, opt->key, count, 1);
                return PRTE_ERR_SILENT;
            }
        }
    }
    return PRTE_SUCCESS;
}

/* the sanity checker is provided for DEVELOPERS as it checks that
 * the options contained in the cmd line being passed to PRRTE for
 * execution meet PRRTE requirements. Although it does emit
 * show_help messages, it really isn't intended for USERS - any
 * problems in translating user cmd lines to PRRTE internal
 * structs should be worked out by the developers */
int prte_schizo_base_sanity(pmix_cli_result_t *cmd_line)
{
    pmix_cli_item_t *opt, *newopt;
    int n, rc;
    const char *tgt;
    char **vtmp;

    char *mappers[] = {
        PRTE_CLI_SLOT,
        PRTE_CLI_HWT,
        PRTE_CLI_CORE,
        PRTE_CLI_L1CACHE,
        PRTE_CLI_L2CACHE,
        PRTE_CLI_L3CACHE,
        PRTE_CLI_NUMA,
        PRTE_CLI_PACKAGE,
        PRTE_CLI_NODE,
        PRTE_CLI_SEQ,
        PRTE_CLI_PPR,
        PRTE_CLI_RANKFILE,
        PRTE_CLI_PELIST,
        NULL
    };
    char *mapquals[] = {
        PRTE_CLI_PE,
        PRTE_CLI_SPAN,
        PRTE_CLI_OVERSUB,
        PRTE_CLI_NOOVER,
        PRTE_CLI_NOLOCAL,
        PRTE_CLI_HWTCPUS,
        PRTE_CLI_CORECPUS,
        PRTE_CLI_INHERIT,
        PRTE_CLI_NOINHERIT,
        PRTE_CLI_QFILE,
        PRTE_CLI_ORDERED,
        NULL
    };

    char *rankers[] = {
        PRTE_CLI_SLOT,
        PRTE_CLI_NODE,
        PRTE_CLI_FILL,
        PRTE_CLI_SPAN,
        NULL
    };
    char *rkquals[] = {
        NULL
    };

    char *binders[] = {
        PRTE_CLI_NONE,
        PRTE_CLI_HWT,
        PRTE_CLI_CORE,
        PRTE_CLI_L1CACHE,
        PRTE_CLI_L2CACHE,
        PRTE_CLI_L3CACHE,
        PRTE_CLI_NUMA,
        PRTE_CLI_PACKAGE,
        NULL
    };
    char *bndquals[] = {
        PRTE_CLI_OVERLOAD,
        PRTE_CLI_NOOVERLOAD,
        PRTE_CLI_IF_SUPP,
        NULL
    };

    char *outputs[] = {
        PRTE_CLI_TAG,
        PRTE_CLI_TAG_DET,
        PRTE_CLI_TAG_FULL,
        PRTE_CLI_RANK,
        PRTE_CLI_TIMESTAMP,
        PRTE_CLI_XML,
        PRTE_CLI_MERGE_ERROUT,
        PRTE_CLI_DIR,
        PRTE_CLI_FILE,
        NULL
    };
    char *outquals[] = {
        PRTE_CLI_NOCOPY,
        PRTE_CLI_RAW,
        NULL
    };

    char *displays[] = {
        PRTE_CLI_ALLOC,
        PRTE_CLI_MAP,
        PRTE_CLI_BIND,
        PRTE_CLI_MAPDEV,
        PRTE_CLI_TOPO,
        PRTE_CLI_CPUS,
        NULL
    };

    char *displayquals[] = {
        PRTE_CLI_PARSEABLE,
        PRTE_CLI_PARSABLE,
        NULL
    };

    char *rtos[] = {
        PRTE_CLI_ERROR_NZ,
        PRTE_CLI_NOLAUNCH,
        PRTE_CLI_SHOW_PROGRESS,
        PRTE_CLI_RECOVERABLE,
        PRTE_CLI_CONTINUOUS,
        PRTE_CLI_MAX_RESTARTS,
        PRTE_CLI_EXEC_AGENT,
        PRTE_CLI_STOP_ON_EXEC,
        PRTE_CLI_STOP_IN_INIT,
        PRTE_CLI_STOP_IN_APP,
        PRTE_CLI_TIMEOUT,
        PRTE_CLI_SPAWN_TIMEOUT,
        PRTE_CLI_REPORT_STATE,
        PRTE_CLI_STACK_TRACES,
        PRTE_CLI_REPORT_CHILD_SEP,
        PRTE_CLI_AGG_HELP,
        PRTE_CLI_NOTIFY_ERRORS,
        PRTE_CLI_OUTPUT_PROCTABLE,
        PRTE_CLI_FWD_ENVIRON,
        NULL
    };

    if (1 < pmix_cmd_line_get_ninsts(cmd_line, PRTE_CLI_MAPBY)) {
        pmix_show_help("help-schizo-base.txt", "multi-instances", true, PRTE_CLI_MAPBY);
        return PRTE_ERR_SILENT;
    }
    if (1 < pmix_cmd_line_get_ninsts(cmd_line, PRTE_CLI_RANKBY)) {
        pmix_show_help("help-schizo-base.txt", "multi-instances", true, PRTE_CLI_RANKBY);
        return PRTE_ERR_SILENT;
    }
    if (1 < pmix_cmd_line_get_ninsts(cmd_line, PRTE_CLI_BINDTO)) {
        pmix_show_help("help-schizo-base.txt", "multi-instances", true, PRTE_CLI_BINDTO);
        return PRTE_ERR_SILENT;
    }
    if (1 < pmix_cmd_line_get_ninsts(cmd_line, PRTE_CLI_DISPLAY)) {
        pmix_show_help("help-schizo-base.txt", "multi-instances", true, PRTE_CLI_DISPLAY);
        return PRTE_ERR_SILENT;
    }
    if (1 < pmix_cmd_line_get_ninsts(cmd_line, PRTE_CLI_RTOS)) {
        pmix_show_help("help-schizo-base.txt", "multi-instances", true, PRTE_CLI_RTOS);
        return PRTE_ERR_SILENT;
    }

    /* check for synonyms */
    PMIX_LIST_FOREACH(opt, &cmd_line->instances, pmix_cli_item_t) {
        if (NULL != (tgt = check_synonym(opt->key))) {
            if (NULL == opt->values) {
                // the presence is adequate
                if (NULL == pmix_cmd_line_get_param(cmd_line, tgt)) {
                    newopt = PMIX_NEW(pmix_cli_item_t);
                    newopt->key = strdup(tgt);
                    pmix_list_append(&cmd_line->instances, &newopt->super);
                }
            } else {
                for (n=0; NULL != opt->values[n]; n++) {
                    rc = prte_schizo_base_add_directive(cmd_line, opt->key, tgt,
                                                        opt->values[n], false);
                    if (PRTE_SUCCESS != rc) {
                        return rc;
                    }
                }
            }
        }
    }

    /* quick check that we have valid directives */
    opt = pmix_cmd_line_get_param(cmd_line, PRTE_CLI_MAPBY);
    if (NULL != opt) {
        if (!prte_schizo_base_check_directives(PRTE_CLI_MAPBY, mappers, mapquals, opt->values[0])) {
            return PRTE_ERR_SILENT;
        }
    }

    opt = pmix_cmd_line_get_param(cmd_line, PRTE_CLI_RANKBY);
    if (NULL != opt) {
        if (!prte_schizo_base_check_directives(PRTE_CLI_RANKBY, rankers, rkquals, opt->values[0])) {
            return PRTE_ERR_SILENT;
        }
    }

    opt = pmix_cmd_line_get_param(cmd_line, PRTE_CLI_BINDTO);
    if (NULL != opt) {
        if (!prte_schizo_base_check_directives(PRTE_CLI_BINDTO, binders, bndquals, opt->values[0])) {
            return PRTE_ERR_SILENT;
        }
    }

    /* the following have multiple directives */
    opt = pmix_cmd_line_get_param(cmd_line, PRTE_CLI_OUTPUT);
    if (NULL != opt) {
        vtmp = PMIX_ARGV_SPLIT_COMPAT(opt->values[0], ',');
        for (n=0; NULL != vtmp[n]; n++) {
            if (!prte_schizo_base_check_directives(PRTE_CLI_OUTPUT, outputs, outquals, vtmp[n])) {
                return PRTE_ERR_SILENT;
            }
        }
        PMIX_ARGV_FREE_COMPAT(vtmp);
    }

    opt = pmix_cmd_line_get_param(cmd_line, PRTE_CLI_DISPLAY);
    if (NULL != opt) {
        vtmp = PMIX_ARGV_SPLIT_COMPAT(opt->values[0], ',');
        for (n=0; NULL != vtmp[n]; n++) {
            if (!prte_schizo_base_check_directives(PRTE_CLI_DISPLAY, displays, displayquals, vtmp[n])) {
                return PRTE_ERR_SILENT;
            }
        }
        PMIX_ARGV_FREE_COMPAT(vtmp);
    }

    opt = pmix_cmd_line_get_param(cmd_line, PRTE_CLI_RTOS);
    if (NULL != opt) {
        vtmp = PMIX_ARGV_SPLIT_COMPAT(opt->values[0], ',');
        for (n=0; NULL != vtmp[n]; n++) {
            if (!prte_schizo_base_check_directives(PRTE_CLI_RTOS, rtos, NULL, vtmp[n])) {
                return PRTE_ERR_SILENT;
            }
        }
        PMIX_ARGV_FREE_COMPAT(vtmp);
    }

    // check too many values given to a single command line option
    PMIX_LIST_FOREACH(opt, &cmd_line->instances, pmix_cli_item_t) {
        rc = check_ndirs(opt);
        if (PRTE_SUCCESS != rc) {
            return rc;
        }
    }

    // check for map-by - bind-to conflicts
    opt = pmix_cmd_line_get_param(cmd_line, PRTE_CLI_MAPBY);
    newopt = pmix_cmd_line_get_param(cmd_line, PRTE_CLI_BINDTO);
    if (NULL != opt && NULL != newopt) {
        if (NULL != strcasestr(opt->values[0], "PE")) {
            /* if we are binding to a PE, then there is no conflict */
            if (NULL != strcasestr(newopt->values[0], "core") ||
                NULL != strcasestr(newopt->values[0], "hwt")) {
                return PRTE_SUCCESS;
            }
            pmix_show_help("help-schizo-base.txt", "binding-pe-conflict", true,
                           opt->values[0], newopt->values[0]);
            return PRTE_ERR_SILENT;
        }
    }

    return PRTE_SUCCESS;
}

int prte_schizo_base_parse_display(pmix_cli_item_t *opt, void *jinfo)
{
    int n, idx;
    pmix_status_t ret;
    char **targv, *ptr, *cptr;

    for (n=0; NULL != opt->values[n]; n++) {
        targv = PMIX_ARGV_SPLIT_COMPAT(opt->values[n], ',');
        for (idx = 0; NULL != targv[idx]; idx++) {
            /* check for qualifiers */
            cptr = strchr(targv[idx], ':');
            if (NULL != cptr) {
                *cptr = '\0';
                ++cptr;
                /* we only support one qualifier at present */
                if (PMIX_CHECK_CLI_OPTION(cptr, PRTE_CLI_PARSEABLE) ||
                    PMIX_CHECK_CLI_OPTION(cptr, PRTE_CLI_PARSABLE)) {
#ifdef PMIX_DISPLAY_PARSEABLE_OUTPUT
                    PMIX_INFO_LIST_ADD(ret, jinfo, PMIX_DISPLAY_PARSEABLE_OUTPUT, NULL, PMIX_BOOL);
                    if (PMIX_SUCCESS != ret) {
                        PMIX_ERROR_LOG(ret);
                        PMIX_ARGV_FREE_COMPAT(targv);
                        return ret;
                    }
#else
                    pmix_show_help("help-prte-rmaps-base.txt", "non-supporting-pmix", true,
                                   "display", cptr);
                    PMIX_ARGV_FREE_COMPAT(targv);
                    return PRTE_ERR_FATAL;
#endif
                } else {
                    pmix_show_help("help-prte-rmaps-base.txt", "unrecognized-qualifier", true,
                                   "display", cptr, "PARSEABLE,PARSABLE");
                    PMIX_ARGV_FREE_COMPAT(targv);
                    return PRTE_ERR_FATAL;
                }
            }

            if (PMIX_CHECK_CLI_OPTION(targv[idx], PRTE_CLI_ALLOC)) {
                PMIX_INFO_LIST_ADD(ret, jinfo, PMIX_DISPLAY_ALLOCATION, NULL, PMIX_BOOL);
                if (PMIX_SUCCESS != ret) {
                    PMIX_ERROR_LOG(ret);
                    PMIX_ARGV_FREE_COMPAT(targv);
                    return ret;
                }

            } else if (PMIX_CHECK_CLI_OPTION(targv[idx], PRTE_CLI_MAPDEV)) {
                PMIX_INFO_LIST_ADD(ret, jinfo, PMIX_DISPLAY_MAP_DETAILED, NULL, PMIX_BOOL);
                if (PMIX_SUCCESS != ret) {
                    PMIX_ERROR_LOG(ret);
                    PMIX_ARGV_FREE_COMPAT(targv);
                    return ret;
                }

            } else if (PMIX_CHECK_CLI_OPTION(targv[idx], PRTE_CLI_MAP)) {
                PMIX_INFO_LIST_ADD(ret, jinfo, PMIX_DISPLAY_MAP, NULL, PMIX_BOOL);
                if (PMIX_SUCCESS != ret) {
                    PMIX_ERROR_LOG(ret);
                    PMIX_ARGV_FREE_COMPAT(targv);
                    return ret;
                }

            } else if (PMIX_CHECK_CLI_OPTION(targv[idx], PRTE_CLI_BIND)) {
                PMIX_INFO_LIST_ADD(ret, jinfo, PMIX_REPORT_BINDINGS, NULL, PMIX_BOOL);
                if (PMIX_SUCCESS != ret) {
                    PMIX_ERROR_LOG(ret);
                    PMIX_ARGV_FREE_COMPAT(targv);
                    return ret;
                }

            } else if (PMIX_CHECK_CLI_OPTION(targv[idx], PRTE_CLI_TOPO)) {
                ptr = strchr(targv[idx], '=');
                if (NULL != ptr) {
                    ++ptr;
                    if ('\0' == *ptr) {
                        /* missing the value or value is invalid */
                        pmix_show_help("help-prte-rmaps-base.txt", "invalid-value", true,
                                       "display", "PROCESSORS", targv[idx]);
                        PMIX_ARGV_FREE_COMPAT(targv);
                        return PRTE_ERR_FATAL;
                    }
                }
                PMIX_INFO_LIST_ADD(ret, jinfo, PMIX_DISPLAY_TOPOLOGY, ptr, PMIX_STRING);
                if (PMIX_SUCCESS != ret) {
                    PMIX_ERROR_LOG(ret);
                    PMIX_ARGV_FREE_COMPAT(targv);
                    return ret;
                }
            } else if (PMIX_CHECK_CLI_OPTION(targv[idx], PRTE_CLI_CPUS)) {
                ptr = strchr(targv[idx], '=');
                if (NULL != ptr) {
                    ++ptr;
                    if ('\0' == *ptr) {
                        /* missing the value or value is invalid */
                        pmix_show_help("help-prte-rmaps-base.txt", "invalid-value", true,
                                       "display", "PROCESSORS", targv[idx]);
                        PMIX_ARGV_FREE_COMPAT(targv);
                        return PRTE_ERR_FATAL;
                    }
                }
#ifdef PMIX_DISPLAY_PROCESSORS
                PMIX_INFO_LIST_ADD(ret, jinfo, PMIX_DISPLAY_PROCESSORS, ptr, PMIX_STRING);
#else
                pmix_show_help("help-prte-rmaps-base.txt", "non-supporting-pmix", true,
                               "display", targv[idx]);
                PMIX_ARGV_FREE_COMPAT(targv);
                return PRTE_ERR_FATAL;
#endif
                if (PMIX_SUCCESS != ret) {
                    PMIX_ERROR_LOG(ret);
                    PMIX_ARGV_FREE_COMPAT(targv);
                    return ret;
                }
            }
        }
        PMIX_ARGV_FREE_COMPAT(targv);
    }

    return PRTE_SUCCESS;
}

int prte_schizo_base_parse_output(pmix_cli_item_t *opt, void *jinfo)
{
    char *outdir=NULL;
    char *outfile=NULL;
    char **targv, *ptr, *cptr, **options;
    int m, n, idx;
    pmix_status_t ret;

    for (n=0; NULL != opt->values[n]; n++) {
        targv = PMIX_ARGV_SPLIT_COMPAT(opt->values[0], ',');
        for (idx = 0; NULL != targv[idx]; idx++) {
            /* check for qualifiers */
            cptr = strchr(targv[idx], ':');
            if (NULL != cptr) {
                *cptr = '\0';
                ++cptr;
                /* could be multiple qualifiers, so separate them */
                options = PMIX_ARGV_SPLIT_COMPAT(cptr, ',');
                for (m=0; NULL != options[m]; m++) {

                    if (PMIX_CHECK_CLI_OPTION(options[m], PRTE_CLI_NOCOPY)) {
                        PMIX_INFO_LIST_ADD(ret, jinfo, PMIX_IOF_FILE_ONLY, NULL, PMIX_BOOL);
                        if (PMIX_SUCCESS != ret) {
                            PMIX_ERROR_LOG(ret);
                            PMIX_ARGV_FREE_COMPAT(targv);
                            PMIX_ARGV_FREE_COMPAT(options);
                            return ret;
                        }

                    } else if (PMIX_CHECK_CLI_OPTION(options[m], PRTE_CLI_PATTERN)) {
                        PMIX_INFO_LIST_ADD(ret, jinfo, PMIX_IOF_FILE_PATTERN, NULL, PMIX_BOOL);
                        if (PMIX_SUCCESS != ret) {
                            PMIX_ERROR_LOG(ret);
                            PMIX_ARGV_FREE_COMPAT(targv);
                            PMIX_ARGV_FREE_COMPAT(options);
                            return ret;
                        }

                    } else if (PMIX_CHECK_CLI_OPTION(options[m], PRTE_CLI_RAW)) {
                        PMIX_INFO_LIST_ADD(ret, jinfo, PMIX_IOF_OUTPUT_RAW, NULL, PMIX_BOOL);
                    }
                    if (PMIX_SUCCESS != ret) {
                        PMIX_ERROR_LOG(ret);
                        PMIX_ARGV_FREE_COMPAT(targv);
                        PMIX_ARGV_FREE_COMPAT(options);
                        return ret;
                    }
                }
                PMIX_ARGV_FREE_COMPAT(options);
            }
            if (0 == strlen(targv[idx])) {
                // only qualifiers were given
                continue;
            }
            /* remove any '=' sign in the directive */
            if (NULL != (ptr = strchr(targv[idx], '='))) {
                *ptr = '\0';
                ++ptr; // step over '=' sign
            }
            if (PMIX_CHECK_CLI_OPTION(targv[idx], PRTE_CLI_TAG)) {
                PMIX_INFO_LIST_ADD(ret, jinfo, PMIX_IOF_TAG_OUTPUT, NULL, PMIX_BOOL);
                if (PMIX_SUCCESS != ret) {
                    PMIX_ERROR_LOG(ret);
                    PMIX_ARGV_FREE_COMPAT(targv);
                    return ret;
                }

            } else if (PMIX_CHECK_CLI_OPTION(targv[idx], PRTE_CLI_TAG_DET)) {
                PMIX_INFO_LIST_ADD(ret, jinfo, PMIX_IOF_TAG_DETAILED_OUTPUT, NULL, PMIX_BOOL);
                if (PMIX_SUCCESS != ret) {
                    PMIX_ERROR_LOG(ret);
                    PMIX_ARGV_FREE_COMPAT(targv);
                    return ret;
                }

            } else if (PMIX_CHECK_CLI_OPTION(targv[idx], PRTE_CLI_TAG_FULL)) {
                PMIX_INFO_LIST_ADD(ret, jinfo, PMIX_IOF_TAG_FULLNAME_OUTPUT, NULL, PMIX_BOOL);
                if (PMIX_SUCCESS != ret) {
                    PMIX_ERROR_LOG(ret);
                    PMIX_ARGV_FREE_COMPAT(targv);
                    return ret;
                }

            } else if (PMIX_CHECK_CLI_OPTION(targv[idx], PRTE_CLI_RANK)) {
                PMIX_INFO_LIST_ADD(ret, jinfo, PMIX_IOF_RANK_OUTPUT, NULL, PMIX_BOOL);
                if (PMIX_SUCCESS != ret) {
                    PMIX_ERROR_LOG(ret);
                    PMIX_ARGV_FREE_COMPAT(targv);
                    return ret;
                }

            } else if (PMIX_CHECK_CLI_OPTION(targv[idx], PRTE_CLI_TIMESTAMP)) {
                PMIX_INFO_LIST_ADD(ret, jinfo, PMIX_IOF_TIMESTAMP_OUTPUT, NULL, PMIX_BOOL);
                if (PMIX_SUCCESS != ret) {
                    PMIX_ERROR_LOG(ret);
                    PMIX_ARGV_FREE_COMPAT(targv);
                    return ret;
                }

            } else if (PMIX_CHECK_CLI_OPTION(targv[idx], PRTE_CLI_XML)) {
                PMIX_INFO_LIST_ADD(ret, jinfo, PMIX_IOF_XML_OUTPUT, NULL, PMIX_BOOL);
                if (PMIX_SUCCESS != ret) {
                    PMIX_ERROR_LOG(ret);
                    PMIX_ARGV_FREE_COMPAT(targv);
                    return ret;
                }

            } else if (PMIX_CHECK_CLI_OPTION(targv[idx], PRTE_CLI_MERGE_ERROUT)) {
                PMIX_INFO_LIST_ADD(ret, jinfo, PMIX_IOF_MERGE_STDERR_STDOUT, NULL, PMIX_BOOL);
                if (PMIX_SUCCESS != ret) {
                    PMIX_ERROR_LOG(ret);
                    PMIX_ARGV_FREE_COMPAT(targv);
                    return ret;
                }

            } else if (PMIX_CHECK_CLI_OPTION(targv[idx], PRTE_CLI_DIR)) {
                if (NULL == ptr || '\0' == *ptr) {
                    pmix_show_help("help-prte-rmaps-base.txt",
                                   "missing-qualifier", true,
                                   "output", "directory", "directory");
                    PMIX_ARGV_FREE_COMPAT(targv);
                    return PRTE_ERR_FATAL;
                }
                if (NULL != outfile) {
                    pmix_show_help("help-prted.txt", "both-file-and-dir-set", true, outfile, ptr);
                    PMIX_ARGV_FREE_COMPAT(targv);
                    free(outfile);
                    return PRTE_ERR_FATAL;
                }
                /* If the given filename isn't an absolute path, then
                 * convert it to one so the name will be relative to
                 * the directory where prun was given as that is what
                 * the user will have seen */
                if (!pmix_path_is_absolute(ptr)) {
                    char cwd[PRTE_PATH_MAX];
                    if (NULL == getcwd(cwd, sizeof(cwd))) {
                        PMIX_ARGV_FREE_COMPAT(targv);
                        return PRTE_ERR_FATAL;
                    }
                    outdir = pmix_os_path(false, cwd, ptr, NULL);
                } else {
                    outdir = strdup(ptr);
                }
                PMIX_INFO_LIST_ADD(ret, jinfo, PMIX_IOF_OUTPUT_TO_DIRECTORY, outdir, PMIX_STRING);
                if (PMIX_SUCCESS != ret) {
                    PMIX_ERROR_LOG(ret);
                    PMIX_ARGV_FREE_COMPAT(targv);
                    return ret;
                }

            } else if (PMIX_CHECK_CLI_OPTION(targv[idx], PRTE_CLI_FILE)) {
                if (NULL == ptr || '\0' == *ptr) {
                    pmix_show_help("help-prte-rmaps-base.txt",
                                   "missing-qualifier", true,
                                   "output", "filename", "filename");
                    PMIX_ARGV_FREE_COMPAT(targv);
                    return PRTE_ERR_FATAL;
                }
                if (NULL != outdir) {
                    pmix_show_help("help-prted.txt", "both-file-and-dir-set", true, ptr, outdir);
                    PMIX_ARGV_FREE_COMPAT(targv);
                    return PRTE_ERR_FATAL;
                }
                /* If the given filename isn't an absolute path, then
                 * convert it to one so the name will be relative to
                 * the directory where prun was given as that is what
                 * the user will have seen */
                if (!pmix_path_is_absolute(ptr)) {
                    char cwd[PRTE_PATH_MAX];
                    if (NULL == getcwd(cwd, sizeof(cwd))) {
                        PMIX_ARGV_FREE_COMPAT(targv);
                        return PRTE_ERR_FATAL;
                    }
                    outfile = pmix_os_path(false, cwd, ptr, NULL);
                } else {
                    outfile = strdup(ptr);
                }
                PMIX_INFO_LIST_ADD(ret, jinfo, PMIX_IOF_OUTPUT_TO_FILE, outfile, PMIX_STRING);
                if (PMIX_SUCCESS != ret) {
                    PMIX_ERROR_LOG(ret);
                    PMIX_ARGV_FREE_COMPAT(targv);
                    return ret;
                }
            }
        }
        PMIX_ARGV_FREE_COMPAT(targv);
    }
    if (NULL != outdir) {
        free(outdir);
    }
    if (NULL != outfile) {
        free(outfile);
    }

    return PRTE_SUCCESS;
}

PMIX_CLASS_INSTANCE(prte_schizo_base_active_module_t,
                    pmix_list_item_t, NULL, NULL);
