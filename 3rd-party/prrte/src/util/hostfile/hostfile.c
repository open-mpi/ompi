/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2011-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015-2018 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2016      IBM Corporation.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "prte_config.h"
#include "constants.h"

#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif
#include <errno.h>
#include <string.h>
#include <sys/stat.h>

#include "src/class/pmix_list.h"
#include "src/mca/base/pmix_base.h"
#include "src/mca/mca.h"
#include "src/mca/prteinstalldirs/prteinstalldirs.h"
#include "src/util/pmix_argv.h"
#include "src/util/pmix_if.h"
#include "src/util/pmix_net.h"
#include "src/util/pmix_output.h"

#include "src/mca/errmgr/errmgr.h"
#include "src/mca/ras/base/base.h"
#include "src/runtime/prte_globals.h"
#include "src/util/name_fns.h"
#include "src/util/proc_info.h"
#include "src/util/pmix_show_help.h"

#include "src/util/hostfile/hostfile.h"
#include "src/util/hostfile/hostfile_lex.h"

static const char *cur_hostfile_name = NULL;

static void hostfile_parse_error(int token)
{
    switch (token) {
    case PRTE_HOSTFILE_STRING:
        pmix_show_help("help-hostfile.txt", "parse_error_string", true, cur_hostfile_name,
                       prte_util_hostfile_line, token, prte_util_hostfile_value.sval);
        break;
    case PRTE_HOSTFILE_IPV4:
    case PRTE_HOSTFILE_IPV6:
    case PRTE_HOSTFILE_INT:
        pmix_show_help("help-hostfile.txt", "parse_error_int", true, cur_hostfile_name,
                       prte_util_hostfile_line, token, prte_util_hostfile_value.ival);
        break;
    default:
        pmix_show_help("help-hostfile.txt", "parse_error", true, cur_hostfile_name,
                       prte_util_hostfile_line, token);
        break;
    }
}

/**
 * Return the integer following an = (actually may only return positive ints)
 */
static int hostfile_parse_int(void)
{
    if (PRTE_HOSTFILE_EQUAL != prte_util_hostfile_lex())
        return -1;
    if (PRTE_HOSTFILE_INT != prte_util_hostfile_lex())
        return -1;
    return prte_util_hostfile_value.ival;
}

/**
 * Return the string following an = (option to a keyword)
 */
static char *hostfile_parse_string(void)
{
    int rc;
    if (PRTE_HOSTFILE_EQUAL != prte_util_hostfile_lex()) {
        return NULL;
    }
    rc = prte_util_hostfile_lex();
    if (PRTE_HOSTFILE_STRING != rc) {
        return NULL;
    }
    return strdup(prte_util_hostfile_value.sval);
}

static int hostfile_parse_line(int token, pmix_list_t *updates,
                               pmix_list_t *exclude, bool keep_all)
{
    int rc;
    prte_node_t *node;
    bool got_max = false;
    char *value;
    char **argv;
    char *node_name = NULL;
    char *username = NULL;
    int cnt;
    int number_of_slots = 0;
    char buff[64];
    char *alias = NULL;

    if (PRTE_HOSTFILE_STRING == token || PRTE_HOSTFILE_HOSTNAME == token ||
        PRTE_HOSTFILE_INT == token || PRTE_HOSTFILE_IPV4 == token ||
        PRTE_HOSTFILE_IPV6 == token) {

        if (PRTE_HOSTFILE_INT == token) {
            snprintf(buff, 64, "%d", prte_util_hostfile_value.ival);
            value = buff;
        } else {
            value = prte_util_hostfile_value.sval;
        }
        argv = PMIX_ARGV_SPLIT_COMPAT(value, '@');

        cnt = PMIX_ARGV_COUNT_COMPAT(argv);
        if (1 == cnt) {
            node_name = strdup(argv[0]);
        } else if (2 == cnt) {
            username = strdup(argv[0]);
            node_name = strdup(argv[1]);
        } else {
            pmix_output(0, "WARNING: Unhandled user@host-combination\n"); /* XXX */
        }
        PMIX_ARGV_FREE_COMPAT(argv);

        // Strip off the FQDN if present, ignore IP addresses
        if (!pmix_net_isaddr(node_name)) {
            char *ptr;
            alias = strdup(node_name);
            if (NULL != (ptr = strchr(alias, '.'))) {
                *ptr = '\0';
            }
        }

        /* if the first letter of the name is '^', then this is a node
         * to be excluded. Remove the ^ character so the nodename is
         * usable, and put it on the exclude list
         */
        if ('^' == node_name[0]) {
            int i, len;
            len = strlen(node_name);
            for (i = 1; i < len; i++) {
                node_name[i - 1] = node_name[i];
            }
            node_name[len - 1] = '\0'; /* truncate */

            PMIX_OUTPUT_VERBOSE((3, prte_ras_base_framework.framework_output,
                                 "%s hostfile: node %s is being excluded",
                                 PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), node_name));

            /* see if this is another name for us */
            if (prte_check_host_is_local(node_name)) {
                /* Nodename has been allocated, that is for sure */
                free(node_name);
                node_name = strdup(prte_process_info.nodename);
            }

            /* Do we need to make a new node object?  First check to see
               if it's already in the exclude list */
            node = prte_node_match(exclude, node_name);
            if (NULL == node) {
                node = PMIX_NEW(prte_node_t);
                if (prte_keep_fqdn_hostnames || NULL == alias) {
                    node->name = strdup(node_name);
                } else {
                    node->name = strdup(alias);
                    node->rawname = strdup(node_name);
                }
                if (NULL != username) {
                    prte_set_attribute(&node->attributes, PRTE_NODE_USERNAME, PRTE_ATTR_LOCAL,
                                       username, PMIX_STRING);
                }
                if (NULL != alias && 0 != strcmp(alias, node->name)) {
                    // new node object, so alias must be unique
                    PMIX_ARGV_APPEND_NOSIZE_COMPAT(&node->aliases, alias);
                }
                pmix_list_append(exclude, &node->super);
            } else {
                /* the node name may not match the prior entry, so ensure we
                 * keep it if necessary */
                if (0 != strcmp(node_name, node->name)) {
                    PMIX_ARGV_APPEND_UNIQUE_COMPAT(&node->aliases, node_name);
                }
                free(node_name);
                if (NULL != alias && 0 != strcmp(alias, node->name)) {
                    PMIX_ARGV_APPEND_UNIQUE_COMPAT(&node->aliases, alias);
                }
            }
            if (NULL != alias) {
                free(alias);
            }
            return PRTE_SUCCESS;
        }

        /* this is not a node to be excluded, so we need to process it and
         * add it to the "include" list.
         */

        PMIX_OUTPUT_VERBOSE((3, prte_ras_base_framework.framework_output,
                             "%s hostfile: node %s is being included - keep all is %s",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), node_name,
                             keep_all ? "TRUE" : "FALSE"));

        /* Do we need to make a new node object? */
        if (keep_all || NULL == (node = prte_node_match(updates, node_name))) {
            node = PMIX_NEW(prte_node_t);
            if (prte_keep_fqdn_hostnames || NULL == alias) {
                node->name = strdup(node_name);
            } else {
                node->name = strdup(alias);
                node->rawname = strdup(node_name);
            }
            node->slots = 1;
            if (NULL != username) {
                prte_set_attribute(&node->attributes, PRTE_NODE_USERNAME, PRTE_ATTR_LOCAL, username,
                                   PMIX_STRING);
            }
            if (NULL != alias && 0 != strcmp(alias, node->name)) {
                // new node object, so alias must be unique
                PMIX_ARGV_APPEND_NOSIZE_COMPAT(&node->aliases, alias);
            }
            pmix_list_append(updates, &node->super);
        } else {
            /* this node was already found once - add a slot and mark slots as "given" */
            node->slots++;
            PRTE_FLAG_SET(node, PRTE_NODE_FLAG_SLOTS_GIVEN);
            /* the node name may not match the prior entry, so ensure we
             * keep it if necessary */
            if (0 != strcmp(node_name, node->name)) {
                PMIX_ARGV_APPEND_UNIQUE_COMPAT(&node->aliases, node_name);
            }
            free(node_name);
            if (NULL != alias && 0 != strcmp(alias, node->name)) {
                PMIX_ARGV_APPEND_UNIQUE_COMPAT(&node->aliases, alias);
            }
        }
    } else if (PRTE_HOSTFILE_RELATIVE == token) {
        /* store this for later processing */
        node = PMIX_NEW(prte_node_t);
        // Strip off the FQDN if present, ignore IP addresses
        if (!pmix_net_isaddr(prte_util_hostfile_value.sval)) {
            char *ptr;
            alias = strdup(prte_util_hostfile_value.sval);
            if (NULL != (ptr = strchr(alias, '.'))) {
                *ptr = '\0';
            } else {
                free(alias);
                alias = NULL;
            }
        }
        if (prte_keep_fqdn_hostnames || NULL == alias) {
            node->name = strdup(prte_util_hostfile_value.sval);
        } else {
            node->name = strdup(alias);
            node->rawname = strdup(prte_util_hostfile_value.sval);
        }
        if (NULL != alias && 0 != strcmp(alias, node->name)) {
            // new node object, so alias must be unique
            PMIX_ARGV_APPEND_NOSIZE_COMPAT(&node->aliases, alias);
            free(alias);
        }
        pmix_list_append(updates, &node->super);
    } else if (PRTE_HOSTFILE_RANK == token) {
        /* we can ignore the rank, but we need to extract the node name. we
         * first need to shift over to the other side of the equal sign as
         * this is where the node name will be
         */
        while (!prte_util_hostfile_done && PRTE_HOSTFILE_EQUAL != token) {
            token = prte_util_hostfile_lex();
        }
        if (prte_util_hostfile_done) {
            /* bad syntax somewhere */
            return PRTE_ERROR;
        }
        /* next position should be the node name */
        token = prte_util_hostfile_lex();
        if (PRTE_HOSTFILE_INT == token) {
            snprintf(buff, 64, "%d", prte_util_hostfile_value.ival);
            value = buff;
        } else {
            value = prte_util_hostfile_value.sval;
        }

        argv = PMIX_ARGV_SPLIT_COMPAT(value, '@');

        cnt = PMIX_ARGV_COUNT_COMPAT(argv);
        if (1 == cnt) {
            node_name = strdup(argv[0]);
        } else if (2 == cnt) {
            username = strdup(argv[0]);
            node_name = strdup(argv[1]);
        } else {
            pmix_output(0, "WARNING: Unhandled user@host-combination\n"); /* XXX */
        }
        PMIX_ARGV_FREE_COMPAT(argv);

        // Strip off the FQDN if present, ignore IP addresses
        if (!prte_keep_fqdn_hostnames && !pmix_net_isaddr(node_name)) {
            char *ptr;
            alias = strdup(node_name);
            if (NULL != (ptr = strchr(alias, '.'))) {
                *ptr = '\0';
            }
        }

        /* Do we need to make a new node object? */
        if (NULL == (node = prte_node_match(updates, node_name))) {
            node = PMIX_NEW(prte_node_t);
            node->name = strdup(node_name);
            node->slots = 1;
            if (NULL != username) {
                prte_set_attribute(&node->attributes, PRTE_NODE_USERNAME,
                                   PRTE_ATTR_LOCAL, username, PMIX_STRING);
            }
            pmix_list_append(updates, &node->super);
        } else {
            /* add a slot */
            node->slots++;
            /* the node name may not match the prior entry, so ensure we
             * keep it if necessary */
            if (0 != strcmp(node_name, node->name)) {
                PMIX_ARGV_APPEND_UNIQUE_COMPAT(&node->aliases, node_name);
            }
        }
        if (NULL != alias) {
            PMIX_ARGV_APPEND_UNIQUE_COMPAT(&node->aliases, alias);
            free(alias);
            node->rawname = strdup(node_name);
        }
        PMIX_OUTPUT_VERBOSE((1, prte_ras_base_framework.framework_output,
                             "%s hostfile: node %s slots %d nodes-given %s",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), node->name, node->slots,
                             PRTE_FLAG_TEST(node, PRTE_NODE_FLAG_SLOTS_GIVEN) ? "TRUE" : "FALSE"));
        /* mark the slots as "given" since we take them as being the
         * number specified via the rankfile
         */
        PRTE_FLAG_SET(node, PRTE_NODE_FLAG_SLOTS_GIVEN);
        /* skip to end of line */
        while (!prte_util_hostfile_done && PRTE_HOSTFILE_NEWLINE != token) {
            token = prte_util_hostfile_lex();
        }
        free(node_name);
        return PRTE_SUCCESS;
    } else {
        hostfile_parse_error(token);
        return PRTE_ERROR;
    }
    free(username);

    while (!prte_util_hostfile_done) {
        token = prte_util_hostfile_lex();

        switch (token) {
        case PRTE_HOSTFILE_DONE:
            goto done;

        case PRTE_HOSTFILE_NEWLINE:
            goto done;

        case PRTE_HOSTFILE_USERNAME:
            username = hostfile_parse_string();
            if (NULL != username) {
                prte_set_attribute(&node->attributes, PRTE_NODE_USERNAME, PRTE_ATTR_LOCAL, username,
                                   PMIX_STRING);
                free(username);
            }
            break;

        case PRTE_HOSTFILE_PORT:
            rc = hostfile_parse_int();
            if (rc < 0) {
                pmix_show_help("help-hostfile.txt", "port", true, cur_hostfile_name, rc);
                return PRTE_ERROR;
            }
            prte_set_attribute(&node->attributes, PRTE_NODE_PORT, PRTE_ATTR_LOCAL, &rc, PMIX_INT);
            break;

        case PRTE_HOSTFILE_COUNT:
        case PRTE_HOSTFILE_CPU:
        case PRTE_HOSTFILE_SLOTS:
            rc = hostfile_parse_int();
            if (rc < 0) {
                pmix_show_help("help-hostfile.txt", "slots", true, cur_hostfile_name, rc);
                pmix_list_remove_item(updates, &node->super);
                PMIX_RELEASE(node);
                return PRTE_ERROR;
            }
            if (PRTE_FLAG_TEST(node, PRTE_NODE_FLAG_SLOTS_GIVEN)) {
                /* multiple definitions were given for the
                 * slot count - this is not allowed
                 */
                pmix_show_help("help-hostfile.txt", "slots-given", true, cur_hostfile_name,
                               node->name);
                pmix_list_remove_item(updates, &node->super);
                PMIX_RELEASE(node);
                return PRTE_ERROR;
            }
            node->slots = rc;
            PRTE_FLAG_SET(node, PRTE_NODE_FLAG_SLOTS_GIVEN);

            /* Ensure that slots_max >= slots */
            if (node->slots_max != 0 && node->slots_max < node->slots) {
                node->slots_max = node->slots;
            }
            break;

        case PRTE_HOSTFILE_SLOTS_MAX:
            rc = hostfile_parse_int();
            if (rc < 0) {
                pmix_show_help("help-hostfile.txt", "max_slots", true, cur_hostfile_name,
                               ((size_t) rc));
                pmix_list_remove_item(updates, &node->super);
                PMIX_RELEASE(node);
                return PRTE_ERROR;
            }
            /* Only take this update if it puts us >= node_slots */
            if (rc >= node->slots) {
                if (node->slots_max != rc) {
                    node->slots_max = rc;
                    got_max = true;
                }
            } else {
                pmix_show_help("help-hostfile.txt", "max_slots_lt", true, cur_hostfile_name,
                               node->slots, rc);
                PRTE_ERROR_LOG(PRTE_ERR_BAD_PARAM);
                pmix_list_remove_item(updates, &node->super);
                PMIX_RELEASE(node);
                return PRTE_ERROR;
            }
            break;

        case PRTE_HOSTFILE_STRING:
        case PRTE_HOSTFILE_INT:
            /* just ignore it */
            break;

        default:
            hostfile_parse_error(token);
            pmix_list_remove_item(updates, &node->super);
            PMIX_RELEASE(node);
            return PRTE_ERROR;
        }
        if (number_of_slots > node->slots) {
            PRTE_ERROR_LOG(PRTE_ERR_BAD_PARAM);
            pmix_list_remove_item(updates, &node->super);
            PMIX_RELEASE(node);
            return PRTE_ERROR;
        }
    }

done:
    if (got_max && !PRTE_FLAG_TEST(node, PRTE_NODE_FLAG_SLOTS_GIVEN)) {
        node->slots = node->slots_max;
        PRTE_FLAG_SET(node, PRTE_NODE_FLAG_SLOTS_GIVEN);
    }

    return PRTE_SUCCESS;
}

/**
 * Parse the specified file into a node list.
 */

static int hostfile_parse(const char *hostfile, pmix_list_t *updates, pmix_list_t *exclude,
                          bool keep_all)
{
    int token;
    int rc = PRTE_SUCCESS;

    cur_hostfile_name = hostfile;

    prte_util_hostfile_done = false;
    prte_util_hostfile_in = fopen(hostfile, "r");
    if (NULL == prte_util_hostfile_in) {
        if (NULL == prte_default_hostfile || 0 != strcmp(prte_default_hostfile, hostfile)) {
            /* not the default hostfile, so not finding it
             * is an error
             */
            pmix_show_help("help-hostfile.txt", "no-hostfile", true, hostfile);
            rc = PRTE_ERR_SILENT;
            goto unlock;
        }
        /* if this is the default hostfile and it was given,
         * then it's an error
         */
        if (prte_default_hostfile_given) {
            pmix_show_help("help-hostfile.txt", "no-hostfile", true, hostfile);
            rc = PRTE_ERR_NOT_FOUND;
            goto unlock;
        }
        /* otherwise, not finding it is okay */
        rc = PRTE_SUCCESS;
        goto unlock;
    }

    while (!prte_util_hostfile_done) {
        token = prte_util_hostfile_lex();

        switch (token) {
        case PRTE_HOSTFILE_DONE:
            prte_util_hostfile_done = true;
            break;

        case PRTE_HOSTFILE_NEWLINE:
            break;

        /*
         * This looks odd, since we have several forms of host-definitions:
         *   hostname              just plain as it is, being a PRTE_HOSTFILE_STRING
         *   IP4s and user@IPv4s
         *   hostname.domain and user@hostname.domain
         */
        case PRTE_HOSTFILE_STRING:
        case PRTE_HOSTFILE_INT:
        case PRTE_HOSTFILE_HOSTNAME:
        case PRTE_HOSTFILE_IPV4:
        case PRTE_HOSTFILE_IPV6:
        case PRTE_HOSTFILE_RELATIVE:
        case PRTE_HOSTFILE_RANK:
            rc = hostfile_parse_line(token, updates, exclude, keep_all);
            if (PRTE_SUCCESS != rc) {
                goto unlock;
            }
            break;

        default:
            hostfile_parse_error(token);
            goto unlock;
        }
    }
    fclose(prte_util_hostfile_in);
    prte_util_hostfile_in = NULL;
    prte_util_hostfile_lex_destroy();

unlock:
    cur_hostfile_name = NULL;

    return rc;
}

/**
 * Parse the provided hostfile and add the nodes to the list.
 */

int prte_util_add_hostfile_nodes(pmix_list_t *nodes, char *hostfile)
{
    pmix_list_t exclude, adds;
    pmix_list_item_t *item;
    int rc, i;
    prte_node_t *nd, *node;
    bool found;

    PMIX_OUTPUT_VERBOSE((1, prte_ras_base_framework.framework_output,
                         "%s hostfile: checking hostfile %s for nodes",
                         PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), hostfile));

    PMIX_CONSTRUCT(&exclude, pmix_list_t);
    PMIX_CONSTRUCT(&adds, pmix_list_t);

    /* parse the hostfile and add any new contents to the list */
    if (PRTE_SUCCESS != (rc = hostfile_parse(hostfile, &adds, &exclude, false))) {
        goto cleanup;
    }

    /* check for any relative node directives */
    PMIX_LIST_FOREACH(node, &adds, prte_node_t) {
        if ('+' == node->name[0]) {
            pmix_show_help("help-hostfile.txt", "hostfile:relative-syntax", true, node->name);
            rc = PRTE_ERR_SILENT;
            goto cleanup;
        }
    }

    /* remove from the list of nodes those that are in the exclude list */
    while (NULL != (item = pmix_list_remove_first(&exclude))) {
        nd = (prte_node_t *) item;
        /* check for matches on nodes */
        PMIX_LIST_FOREACH(node, &adds, prte_node_t) {
            if (prte_nptr_match(nd, node)) {
                /* match - remove it */
                pmix_list_remove_item(&adds, &node->super);
                PMIX_RELEASE(node);
                break;
            }
        }
        PMIX_RELEASE(item);
    }

    /* transfer across all unique nodes */
    while (NULL != (item = pmix_list_remove_first(&adds))) {
        nd = (prte_node_t *) item;
        found = false;
        PMIX_LIST_FOREACH(node, nodes, prte_node_t) {
            if (prte_nptr_match(nd, node)) {
                found = true;
                break;
            }
        }
        if (found) {
            /* add this node name as alias */
            PMIX_ARGV_APPEND_UNIQUE_COMPAT(&node->aliases, nd->name);
            /* ensure all other aliases are also transferred */
            if (NULL != nd->aliases) {
                for (i=0; NULL != nd->aliases[i]; i++) {
                    PMIX_ARGV_APPEND_UNIQUE_COMPAT(&node->aliases, nd->aliases[i]);
                }
            }
           PMIX_RELEASE(item);
        } else {
            pmix_list_append(nodes, &nd->super);
            PMIX_OUTPUT_VERBOSE((1, prte_ras_base_framework.framework_output,
                                 "%s hostfile: adding node %s slots %d",
                                 PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), nd->name, nd->slots));
        }
    }

cleanup:
    PMIX_LIST_DESTRUCT(&exclude);
    PMIX_LIST_DESTRUCT(&adds);

    return rc;
}

/* Parse the provided hostfile and filter the nodes that are
 * on the input list, removing those that
 * are not found in the hostfile
 */
int prte_util_filter_hostfile_nodes(pmix_list_t *nodes, char *hostfile, bool remove)
{
    pmix_list_t newnodes, exclude;
    pmix_list_item_t *item1, *item2, *next, *item3;
    prte_node_t *node_from_list, *node_from_file, *node_from_pool, *node3;
    int rc = PRTE_SUCCESS;
    char *cptr;
    int num_empty, nodeidx;
    bool want_all_empty = false;
    pmix_list_t keep;
    bool found;

    PMIX_OUTPUT_VERBOSE((1, prte_ras_base_framework.framework_output,
                         "%s hostfile: filtering nodes through hostfile %s",
                         PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), hostfile));

    /* parse the hostfile and create local list of findings */
    PMIX_CONSTRUCT(&newnodes, pmix_list_t);
    PMIX_CONSTRUCT(&exclude, pmix_list_t);
    if (PRTE_SUCCESS != (rc = hostfile_parse(hostfile, &newnodes, &exclude, false))) {
        PMIX_DESTRUCT(&newnodes);
        PMIX_DESTRUCT(&exclude);
        return rc;
    }

    /* if the hostfile was empty, then treat it as a no-op filter */
    if (0 == pmix_list_get_size(&newnodes)) {
        PMIX_DESTRUCT(&newnodes);
        PMIX_DESTRUCT(&exclude);
        /* indicate that the hostfile was empty */
        return PRTE_ERR_TAKE_NEXT_OPTION;
    }

    /* remove from the list of newnodes those that are in the exclude list
     * since we could have added duplicate names above due to the */
    while (NULL != (item1 = pmix_list_remove_first(&exclude))) {
        node_from_file = (prte_node_t *) item1;
        /* check for matches on nodes */
        for (item2 = pmix_list_get_first(&newnodes); item2 != pmix_list_get_end(&newnodes);
             item2 = pmix_list_get_next(item2)) {
            prte_node_t *node = (prte_node_t *) item2;
            if (prte_nptr_match(node_from_file, node)) {
                /* match - remove it */
                pmix_list_remove_item(&newnodes, item2);
                PMIX_RELEASE(item2);
                break;
            }
        }
        PMIX_RELEASE(item1);
    }

    /* now check our nodes and keep or mark those that match. We can
     * destruct our hostfile list as we go since this won't be needed
     */
    PMIX_CONSTRUCT(&keep, pmix_list_t);
    while (NULL != (item2 = pmix_list_remove_first(&newnodes))) {
        node_from_file = (prte_node_t *) item2;

        next = pmix_list_get_next(item2);

        /* see if this is a relative node syntax */
        if ('+' == node_from_file->name[0]) {
            /* see if we specified empty nodes */
            if ('e' == node_from_file->name[1] || 'E' == node_from_file->name[1]) {
                /* request for empty nodes - do they want
                 * all of them?
                 */
                if (NULL != (cptr = strchr(node_from_file->name, ':'))) {
                    /* the colon indicates a specific # are requested */
                    cptr++; /* step past : */
                    num_empty = strtol(cptr, NULL, 10);
                } else {
                    /* want them all - set num_empty to max */
                    num_empty = INT_MAX;
                    want_all_empty = true;
                }
                /* search the list of nodes provided to us and find those
                 * that are empty
                 */
                item1 = pmix_list_get_first(nodes);
                while (0 < num_empty && item1 != pmix_list_get_end(nodes)) {
                    node_from_list = (prte_node_t *) item1;
                    next = pmix_list_get_next(item1); /* keep our place */
                    if (0 == node_from_list->slots_inuse) {
                        /* check to see if this node is explicitly called
                         * out later - if so, don't use it here
                         */
                        for (item3 = pmix_list_get_first(&newnodes);
                             item3 != pmix_list_get_end(&newnodes);
                             item3 = pmix_list_get_next(item3)) {
                            node3 = (prte_node_t *) item3;
                            if (prte_nptr_match(node3, node_from_list)) {
                                /* match - don't use it */
                                goto skipnode;
                            }
                        }
                        if (remove) {
                            /* remove item from list */
                            pmix_list_remove_item(nodes, item1);
                            /* xfer to keep list */
                            pmix_list_append(&keep, item1);
                        } else {
                            /* mark as included */
                            PRTE_FLAG_SET(node_from_list, PRTE_NODE_FLAG_MAPPED);
                        }
                        --num_empty;
                    }
                skipnode:
                    item1 = next;
                }
                /* did they get everything they wanted? */
                if (!want_all_empty && 0 < num_empty) {
                    pmix_show_help("help-hostfile.txt", "hostfile:not-enough-empty", true,
                                   num_empty);
                    rc = PRTE_ERR_SILENT;
                    goto cleanup;
                }
            } else if ('n' == node_from_file->name[1] || 'N' == node_from_file->name[1]) {
                /* they want a specific relative node #, so
                 * look it up on global pool
                 */
                nodeidx = strtol(&node_from_file->name[2], NULL, 10);
                node_from_pool = (prte_node_t *) pmix_pointer_array_get_item(prte_node_pool, nodeidx);
                if (NULL == node_from_pool) {
                    /* this is an error */
                    pmix_show_help("help-hostfile.txt", "hostfile:relative-node-not-found", true,
                                   nodeidx, node_from_file->name);
                    rc = PRTE_ERR_SILENT;
                    goto cleanup;
                }
                /* search the list of nodes provided to us and find it */
                for (item1 = pmix_list_get_first(nodes); item1 != pmix_list_get_end(nodes);
                     item1 = pmix_list_get_next(item1)) {
                    node_from_list = (prte_node_t *) item1;
                    if (prte_nptr_match(node_from_pool, node_from_list)) {
                        if (remove) {
                            /* match - remove item from list */
                            pmix_list_remove_item(nodes, item1);
                            /* xfer to keep list */
                            pmix_list_append(&keep, item1);
                        } else {
                            /* mark as included */
                            PRTE_FLAG_SET(node_from_list, PRTE_NODE_FLAG_MAPPED);
                        }
                        break;
                    }
                }
            } else {
                /* invalid relative node syntax */
                pmix_show_help("help-hostfile.txt", "hostfile:invalid-relative-node-syntax", true,
                               node_from_file->name);
                rc = PRTE_ERR_SILENT;
                goto cleanup;
            }
        } else {
            /* we are looking for a specific node on the list
             * search the provided list of nodes to see if this
             * one is found
             */
            found = false;
            for (item1 = pmix_list_get_first(nodes); item1 != pmix_list_get_end(nodes);
                 item1 = pmix_list_get_next(item1)) {
                node_from_list = (prte_node_t *) item1;
                /* we have converted all aliases for ourself
                 * to our own detected nodename */
                if (prte_nptr_match(node_from_file, node_from_list)) {
                    /* if the slot count here is less than the
                     * total slots avail on this node, set it
                     * to the specified count - this allows people
                     * to subdivide an allocation
                     */
                    if (PRTE_FLAG_TEST(node_from_file, PRTE_NODE_FLAG_SLOTS_GIVEN)
                        && node_from_file->slots < node_from_list->slots) {
                        node_from_list->slots = node_from_file->slots;
                    }
                    if (remove) {
                        /* remove the node from the list */
                        pmix_list_remove_item(nodes, item1);
                        /* xfer it to keep list */
                        pmix_list_append(&keep, item1);
                    } else {
                        /* mark as included */
                        PRTE_FLAG_SET(node_from_list, PRTE_NODE_FLAG_MAPPED);
                    }
                    found = true;
                    break;
                }
            }
            /* if the host in the newnode list wasn't found,
             * then that is an error we need to report to the
             * user and abort
             */
            if (!found) {
                pmix_show_help("help-hostfile.txt", "hostfile:extra-node-not-found", true, hostfile,
                               node_from_file->name);
                rc = PRTE_ERR_SILENT;
                goto cleanup;
            }
        }
        /* cleanup the newnode list */
        PMIX_RELEASE(item2);
    }

    /* if we still have entries on our hostfile list, then
     * there were requested hosts that were not in our allocation.
     * This is an error - report it to the user and return an error
     */
    if (0 != pmix_list_get_size(&newnodes)) {
        pmix_show_help("help-hostfile.txt", "not-all-mapped-alloc", true, hostfile);
        while (NULL != (item1 = pmix_list_remove_first(&newnodes))) {
            PMIX_RELEASE(item1);
        }
        PMIX_DESTRUCT(&newnodes);
        return PRTE_ERR_SILENT;
    }

    if (!remove) {
        /* all done */
        PMIX_DESTRUCT(&newnodes);
        return PRTE_SUCCESS;
    }

    /* clear the rest of the nodes list */
    while (NULL != (item1 = pmix_list_remove_first(nodes))) {
        PMIX_RELEASE(item1);
    }

    /* the nodes list has been cleared - rebuild it in order */
    while (NULL != (item1 = pmix_list_remove_first(&keep))) {
        pmix_list_append(nodes, item1);
    }

cleanup:
    PMIX_DESTRUCT(&newnodes);

    return rc;
}

int prte_util_get_ordered_host_list(pmix_list_t *nodes, char *hostfile)
{
    pmix_list_t exclude;
    pmix_list_item_t *item, *itm, *item2, *item1;
    char *cptr;
    int num_empty, i, nodeidx, startempty = 0;
    bool want_all_empty = false;
    prte_node_t *node_from_pool, *newnode;
    int rc;

    PMIX_OUTPUT_VERBOSE((1, prte_ras_base_framework.framework_output,
                         "%s hostfile: creating ordered list of hosts from hostfile %s",
                         PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), hostfile));

    PMIX_CONSTRUCT(&exclude, pmix_list_t);

    /* parse the hostfile and add the contents to the list, keeping duplicates */
    if (PRTE_SUCCESS != (rc = hostfile_parse(hostfile, nodes, &exclude, true))) {
        goto cleanup;
    }

    /* parse the nodes to process any relative node directives */
    item2 = pmix_list_get_first(nodes);
    while (item2 != pmix_list_get_end(nodes)) {
        prte_node_t *node = (prte_node_t *) item2;

        /* save the next location in case this one gets removed */
        item1 = pmix_list_get_next(item2);

        if ('+' != node->name[0]) {
            item2 = item1;
            continue;
        }

        /* see if we specified empty nodes */
        if ('e' == node->name[1] || 'E' == node->name[1]) {
            /* request for empty nodes - do they want
             * all of them?
             */
            if (NULL != (cptr = strchr(node->name, ':'))) {
                /* the colon indicates a specific # are requested */
                cptr++; /* step past : */
                num_empty = strtol(cptr, NULL, 10);
            } else {
                /* want them all - set num_empty to max */
                num_empty = INT_MAX;
                want_all_empty = true;
            }
            /* insert empty nodes into newnodes list in place of the current item.
             * since item1 is the next item, we insert in front of it
             */
            if (!prte_hnp_is_allocated && 0 == startempty) {
                startempty = 1;
            }
            for (i = startempty; 0 < num_empty && i < prte_node_pool->size; i++) {
                node_from_pool = (prte_node_t *) pmix_pointer_array_get_item(prte_node_pool, i);
                if (NULL == node_from_pool) {
                    continue;
                }
                if (0 == node_from_pool->slots_inuse) {
                    newnode = PMIX_NEW(prte_node_t);
                    newnode->name = strdup(node_from_pool->name);
                    /* if the slot count here is less than the
                     * total slots avail on this node, set it
                     * to the specified count - this allows people
                     * to subdivide an allocation
                     */
                    if (node->slots < node_from_pool->slots) {
                        newnode->slots = node->slots;
                    } else {
                        newnode->slots = node_from_pool->slots;
                    }
                    pmix_list_insert_pos(nodes, item1, &newnode->super);
                    /* track number added */
                    --num_empty;
                }
            }
            /* bookmark where we stopped in case they ask for more */
            startempty = i;
            /* did they get everything they wanted? */
            if (!want_all_empty && 0 < num_empty) {
                pmix_show_help("help-hostfile.txt", "hostfile:not-enough-empty", true, num_empty);
                rc = PRTE_ERR_SILENT;
                goto cleanup;
            }
            /* since we have expanded the provided node, remove
             * it from list
             */
            pmix_list_remove_item(nodes, item2);
            PMIX_RELEASE(item2);
        } else if ('n' == node->name[1] || 'N' == node->name[1]) {
            /* they want a specific relative node #, so
             * look it up on global pool
             */
            nodeidx = strtol(&node->name[2], NULL, 10);
            /* if the HNP is not allocated, then we need to
             * adjust the index as the node pool is offset
             * by one
             */
            if (!prte_hnp_is_allocated) {
                nodeidx++;
            }
            /* see if that location is filled */
            node_from_pool = (prte_node_t *) pmix_pointer_array_get_item(prte_node_pool, nodeidx);
            if (NULL == node_from_pool) {
                /* this is an error */
                pmix_show_help("help-hostfile.txt", "hostfile:relative-node-not-found", true,
                               nodeidx, node->name);
                rc = PRTE_ERR_SILENT;
                goto cleanup;
            }
            /* create the node object */
            newnode = PMIX_NEW(prte_node_t);
            newnode->name = strdup(node_from_pool->name);
            /* if the slot count here is less than the
             * total slots avail on this node, set it
             * to the specified count - this allows people
             * to subdivide an allocation
             */
            if (node->slots < node_from_pool->slots) {
                newnode->slots = node->slots;
            } else {
                newnode->slots = node_from_pool->slots;
            }
            /* insert it before item1 */
            pmix_list_insert_pos(nodes, item1, &newnode->super);
            /* since we have expanded the provided node, remove
             * it from list
             */
            pmix_list_remove_item(nodes, item2);
            PMIX_RELEASE(item2);
        } else {
            /* invalid relative node syntax */
            pmix_show_help("help-hostfile.txt", "hostfile:invalid-relative-node-syntax", true,
                           node->name);
            rc = PRTE_ERR_SILENT;
            goto cleanup;
        }

        /* move to next */
        item2 = item1;
    }

    /* remove from the list of nodes those that are in the exclude list */
    while (NULL != (item = pmix_list_remove_first(&exclude))) {
        prte_node_t *exnode = (prte_node_t *) item;
        /* check for matches on nodes */
        for (itm = pmix_list_get_first(nodes); itm != pmix_list_get_end(nodes);
             itm = pmix_list_get_next(itm)) {
            prte_node_t *node = (prte_node_t *) itm;
            if (prte_nptr_match(exnode, node)) {
                /* match - remove it */
                pmix_list_remove_item(nodes, itm);
                PMIX_RELEASE(itm);
                /* have to cycle through the entire list as we could
                 * have duplicates
                 */
            }
        }
        PMIX_RELEASE(item);
    }

cleanup:
    PMIX_DESTRUCT(&exclude);

    return rc;
}
