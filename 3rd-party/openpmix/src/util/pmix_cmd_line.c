/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2013 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2012-2017 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2012-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2015-2017 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2016-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2017      IBM Corporation. All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "src/include/pmix_config.h"

#include "src/class/pmix_list.h"
#include "src/class/pmix_object.h"
#include "src/include/pmix_globals.h"
#include "src/util/pmix_argv.h"
#include "src/util/pmix_cmd_line.h"
#include "src/util/pmix_output.h"
#include "src/util/pmix_printf.h"
#include "src/util/pmix_show_help.h"

// Local functions
static int endswith(const char *str, const char *suffix)
{
    size_t lenstr, lensuffix;

    if (NULL == str || NULL == suffix) {
        return PMIX_ERR_BAD_PARAM;
    }

    lenstr = strlen(str);
    lensuffix = strlen(suffix);
    if (lensuffix > lenstr) {
        return PMIX_ERR_BAD_PARAM;
    }
    if (0 == strncmp(str + lenstr - lensuffix, suffix, lensuffix)) {
        return PMIX_SUCCESS;
    }
    return PMIX_ERR_BAD_PARAM;
}

static void check_store(const char *name, const char *option,
                        pmix_cli_result_t *results)
{
    pmix_cli_item_t *opt;

    PMIX_LIST_FOREACH(opt, &results->instances, pmix_cli_item_t) {
        if (0 == strcmp(opt->key, name)) {
            /* if the name is NULL, then this is just setting
             * a boolean value - the presence of the option in
             * the results is considered "true" */
            if (NULL != option) {
                PMIx_Argv_append_nosize(&opt->values, option);
            }
            return;
        }
    }

    // get here if this is new option
    opt = PMIX_NEW(pmix_cli_item_t);
    opt->key = strdup(name);
    pmix_list_append(&results->instances, &opt->super);
    /* if the name is NULL, then this is just setting
     * a boolean value - the presence of the option in
     * the results is considered "true" */
    if (NULL != option) {
        PMIx_Argv_append_nosize(&opt->values, option);
    }
    return;
}

int pmix_cmd_line_parse(char **pargv, char *shorts,
                        struct option myoptions[],
                        pmix_cmd_line_store_fn_t storefn,
                        pmix_cli_result_t *results,
                        char *helpfile)
{
    int option_index = 0;   /* getopt_long stores the option index here. */
    int n, m, opt, argc, argind;
    bool found;
    char *ptr, *str, **argv;
    pmix_cmd_line_store_fn_t mystore;

    /* the getopt_long parser reorders the input argv array, so
     * we have to protect it here - remove all leading/trailing
     * quotes to ensure we are looking at simple options/values */
    argv = pmix_argv_copy_strip(pargv);
    argc = PMIx_Argv_count(argv);
    // assign a default store_fn if one isn't provided
    if (NULL == storefn) {
        mystore = check_store;
    } else {
        mystore = storefn;
    }

    /* reset the parser - must be done each time we use it
     * to avoid hysteresis */
    optind = 0;
    opterr = 0;
    optopt = 0;
    optarg = NULL;

    if (1 == argc) {
        // nothing to parse
        goto done;
    }

    // run the parser
    while (1) {
        argind = optind;
        if (optind == argc || (optind > 0 && '-' != argv[optind][0])) {
            // This is the executable, or we are at the last argument.
            // Don't process any further.
            break;
        }
        opt = getopt_long(argc, argv, shorts, myoptions, &option_index);

        switch (opt) {
            case 0:
                /* if this is an MCA param of some type, store it */
                if (0 == endswith(myoptions[option_index].name, "mca")) {
                    /* format mca params as param:value - the optind value
                     * will have been incremented since the MCA param options
                     * require an argument */
                    pmix_asprintf(&str, "%s=%s", argv[optind-1], argv[optind]);
                    mystore(myoptions[option_index].name, str, results);
                    free(str);
                    ++optind;
                    break;
                }
                /* store actual option */
                mystore(myoptions[option_index].name, optarg, results);
                break;
            case 'h':
                /* the "help" option can optionally take an argument. Since
                 * the argument _is_ optional, getopt will _NOT_ increment
                 * optind, so argv[optind] is the potential argument */
                if (NULL == optarg &&
                    NULL != argv[optind]) {
                    /* strip any leading dashes */
                    ptr = argv[optind];
                    while ('-' == *ptr) {
                        ++ptr;
                    }
                    // check for standard options
                    if (0 == strcmp(ptr, "version") || 0 == strcmp(ptr, "V")) {
                        str = pmix_show_help_string("help-cli.txt", "version", false);
                        if (NULL != str) {
                            printf("%s", str);
                            free(str);
                        }
                        PMIx_Argv_free(argv);
                        return PMIX_OPERATION_SUCCEEDED;
                    }
                    if (0 == strcmp(ptr, "verbose") || 0 == strcmp(ptr, "v")) {
                        str = pmix_show_help_string("help-cli.txt", "verbose", false);
                        if (NULL != str) {
                            printf("%s", str);
                            free(str);
                        }
                        PMIx_Argv_free(argv);
                        return PMIX_OPERATION_SUCCEEDED;
                    }
                    if (0 == strcmp(ptr, "help") || 0 == strcmp(ptr, "h")) {
                        // they requested help on the "help" option itself
                        str = pmix_show_help_string("help-cli.txt", "help", false,
                                                    pmix_tool_basename, pmix_tool_basename,
                                                    pmix_tool_basename, pmix_tool_basename,
                                                    pmix_tool_basename, pmix_tool_basename,
                                                    pmix_tool_basename, pmix_tool_basename);
                        if (NULL != str) {
                            printf("%s", str);
                            free(str);
                        }
                        PMIx_Argv_free(argv);
                        return PMIX_OPERATION_SUCCEEDED;
                    }
                    /* see if we have help on that subject */
                    str = pmix_show_help_string(helpfile, ptr, false);
                    if (NULL == str) {
                        // let the user know we don't recognize that topic
                        str = pmix_show_help_string("help-cli.txt", "unknown-option", true,
                                                    ptr, pmix_tool_basename);
                        if (NULL != str) {
                            printf("%s", str);
                            free(str);
                        }
                    } else {
                        printf("%s", str);
                        free(str);
                    }
                    PMIx_Argv_free(argv);
                    return PMIX_OPERATION_SUCCEEDED;
                } else if (NULL == optarg) {
                    // high-level help request
                    str = pmix_show_help_string(helpfile, "usage", false,
                                                pmix_tool_basename, pmix_tool_org,
                                                pmix_tool_version,
                                                pmix_tool_basename,
                                                pmix_tool_msg);
                    if (NULL != str) {
                        printf("%s", str);
                        free(str);
                    }
                    PMIx_Argv_free(argv);
                    return PMIX_OPERATION_SUCCEEDED;
                } else {  // unrecognized option
                    str = pmix_show_help_string("help-cli.txt", "unrecognized-option", true,
                                                pmix_tool_basename, optarg);
                    if (NULL != str) {
                        printf("%s", str);
                        free(str);
                    }
                }
                PMIx_Argv_free(argv);
                return PMIX_ERR_SILENT;
            case 'V':
                str = pmix_show_help_string(helpfile, "version", false,
                                            pmix_tool_basename, pmix_tool_org,
                                            pmix_tool_version,
                                            pmix_tool_msg);
                if (NULL != str) {
                    printf("%s", str);
                    free(str);
                }
                // if they ask for the version, that is all we do
                PMIx_Argv_free(argv);
                return PMIX_OPERATION_SUCCEEDED;
            case 'v':
                /* if the argv at this point is not pointing at a string
                 * starting with "-v", then we just ignore it - the user
                 * has passed a string with multiple 'v's in it and we
                 * need to wait until the end */
                if (0 != strncmp(argv[optind-1], "-v", 2)) {
                    break;
                }
                /* we store the 'v' option with a value equal to the
                 * number of 'v's the user provided */
                n = strlen(&argv[optind-1][1]);
                pmix_asprintf(&str, "%d", n);
                mystore(myoptions[option_index].name, str, results);
                free(str);
                break;
            default:
                found = false;
                for (n=0; '\0' != shorts[n]; n++) {
                    int ascii = shorts[n];
                    if (opt == ascii) {
                        /* found it - now search for matching option. The
                         * getopt fn will have already incremented optind
                         * to point at the next argument.
                         * If this short option required an argument, then
                         * it will be indicated by a ':' in the next shorts
                         * spot and the argument will be in optarg.
                         *
                         * If the short option takes an optional argument, then
                         * it will be indicated by two ':' after the option - in
                         * this case optarg will contain the argument if given.
                         * Note that the proper form of the optional argument
                         * option is "-zfoo", where 'z' is the option and "foo"
                         * is the argument. Putting a space between the option
                         * and the argument is forbidden and results in reporting
                         * of 'z' without an argument - usually followed by
                         * incorrectly marking "foo" as the beginning of the
                         * command "tail" */
                        if (':' == shorts[n+1]) {
                            // could be an optional arg
                            if (':' == shorts[n+2]) {
                                /* in this case, the argument (if given) must be immediately
                                 * attached to the option */
                                ptr = argv[optind-1];
                                ptr += 2;  // step over the '-' and option
                            } else {
                                ptr = optarg;
                            }
                        } else {
                            ptr = NULL;
                        }
                        for (m=0; NULL != myoptions[m].name; m++) {
                            if (ascii == myoptions[m].val) {
                                if (PMIX_ARG_NONE == myoptions[m].has_arg) {
                                    /* if ptr isn't NULL, then that means we were given
                                     * an argument to an option that doesn't take one.
                                     * Report the error */
                                    if (NULL != ptr) {
                                        str = pmix_show_help_string("help-cli.txt", "short-arg-error", true,
                                                                    pmix_tool_basename, shorts[n], ptr);
                                        if (NULL != str) {
                                            printf("%s", str);
                                            free(str);
                                        }
                                        PMIx_Argv_free(argv);
                                        return PMIX_ERR_SILENT;
                                    }
                                    ptr = NULL;
                                } else if (0 == strcmp(myoptions[m].name, "np") &&
                                           0 == strcmp(optarg, "p")) {
                                    /* we special-case the very common "-np" option */
                                    ptr = argv[optind];
                                    ++optind;
                                }
                                mystore(myoptions[m].name, ptr, results);
                                found = true;
                                break;
                            }
                        }
                        if (found) {
                            break;
                        }
                        /* this could be one of the short options other than 'h' or 'V', so
                         * we have to check */
                        if (0 != argind && '-' != argv[argind][0]) {
                            // this was not an option
                            goto done;
                        }
                        if (0 == strcmp(argv[optind-1], "--")) {
                            // double-dash indicates separator between launcher
                            // directives and the application
                            results->tail = PMIx_Argv_copy(&argv[optind]);
                            PMIx_Argv_free(argv);
                            return PMIX_SUCCESS;
                        }
                        str = pmix_show_help_string("help-cli.txt", "short-no-long", true,
                                                    pmix_tool_basename, shorts[n]);
                        if (NULL != str) {
                            printf("%s", str);
                            free(str);
                        }
                        PMIx_Argv_free(argv);
                        return PMIX_ERR_SILENT;
                    }
                }
                if (found) {
                    break;
                }
                /* see if the option is in the list - if it is, then it is a
                 * "recognized" option but may be missing an argument. The
                 * getopt_long function declares these as "unrecognized", but
                 * we would like to provide a more user-friendly error message */
                for (n=0; NULL != myoptions[n].name; n++) {
                    /* skip the "--" prefix */
                    if (0 == strcmp(&argv[optind-1][2], myoptions[n].name)) {
                        /* the option is recognized - probably misssing
                         * an argument */
                        str = pmix_show_help_string("help-cli.txt", "missing-argument", true,
                                                    pmix_tool_basename, argv[optind-1],
                                                    pmix_tool_basename, &argv[optind-1][2]);
                        if (NULL != str) {
                            printf("%s", str);
                            free(str);
                        }
                        PMIx_Argv_free(argv);
                        return PMIX_ERR_SILENT;
                    }
                }
                if (0 == strcmp(argv[optind-1], "--")) {
                    // double-dash indicates separator between launcher
                    // directives and the application
                    goto done;
                }
                if (1 == optind) {
                    // command without any options
                    goto done;
                }
                str = pmix_show_help_string("help-cli.txt", "unregistered-option", true,
                                            pmix_tool_basename, argv[optind-1], pmix_tool_basename);
                if (NULL != str) {
                    printf("%s", str);
                    free(str);
                }
                PMIx_Argv_free(argv);
                return PMIX_ERR_SILENT;
        }
    }

done:
    if (optind < argc) {
        /* if this is an '&', it simply indicates that the executable
         * was being pushed into the background - ignore it */
        if (0 != strcmp(argv[optind], "&")) {
            results->tail = PMIx_Argv_copy(&argv[optind]);
        }
    }
    PMIx_Argv_free(argv);
    return PMIX_SUCCESS;
}

static void icon(pmix_cli_item_t *p)
{
    p->key = NULL;
    p->values = NULL;
}
static void ides(pmix_cli_item_t *p)
{
    if (NULL != p->key) {
        free(p->key);
    }
    if (NULL != p->values) {
        PMIx_Argv_free(p->values);
    }
}
PMIX_EXPORT PMIX_CLASS_INSTANCE(pmix_cli_item_t,
                                pmix_list_item_t,
                                icon, ides);

static void ocon(pmix_cli_result_t *p)
{
    PMIX_CONSTRUCT(&p->instances, pmix_list_t);
    p->tail = NULL;
}
static void odes(pmix_cli_result_t *p)
{
    PMIX_LIST_DESTRUCT(&p->instances);
    if (NULL != p->tail) {
        PMIx_Argv_free(p->tail);
    }
}
PMIX_EXPORT PMIX_CLASS_INSTANCE(pmix_cli_result_t,
                                pmix_object_t,
                                ocon, odes);
