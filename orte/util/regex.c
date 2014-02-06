/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2013 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "orte_config.h"
#include "orte/types.h"
#include "orte/constants.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#ifdef HAVE_ARPA_INET_H
#include <arpa/inet.h>
#endif
#ifdef HAVE_NETDB_H
#include <netdb.h>
#endif
#ifdef HAVE_IFADDRS_H
#include <ifaddrs.h>
#endif

#include "opal/util/argv.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/odls/odls_types.h"
#include "orte/mca/rml/base/rml_contact.h"
#include "orte/mca/rmaps/rmaps_types.h"
#include "orte/util/show_help.h"
#include "orte/util/name_fns.h"
#include "orte/util/nidmap.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/ess/ess.h"

#include "orte/util/regex.h"

#define ORTE_MAX_NODE_PREFIX        50

static int regex_parse_node_ranges(char *base, char *ranges, int num_digits, char *suffix, char ***names);
static int regex_parse_node_range(char *base, char *range, int num_digits, char *suffix, char ***names);

int orte_regex_create(char *nodelist, char **regexp)
{
    char *node;
    char prefix[ORTE_MAX_NODE_PREFIX];
    int i, j, len, startnum, nodenum, numdigits;
    bool found, fullname;
    char *suffix, *sfx;
    orte_regex_node_t *ndreg;
    orte_regex_range_t *range;
    opal_list_t nodeids;
    opal_list_item_t *item, *itm2;
    char **regexargs = NULL, *tmp, *tmp2;
    char *cptr;

    /* define the default */
    *regexp = NULL;

    cptr = strchr(nodelist, ',');
    if (NULL == cptr) {
        /* if there is only one node, don't bother */
        *regexp = strdup(nodelist);
        return ORTE_SUCCESS;
    }

    /* setup the list of results */
    OBJ_CONSTRUCT(&nodeids, opal_list_t);

    /* cycle thru the array of nodenames */
    node = nodelist;
    while (NULL != (cptr = strchr(node, ',')) || 0 < strlen(node)) {
        if (NULL != cptr) {
            *cptr = '\0';
        }
        /* determine this node's prefix by looking for first non-alpha char */
        fullname = false;
        len = strlen(node);
        startnum = -1;
        memset(prefix, 0, ORTE_MAX_NODE_PREFIX);
        numdigits = 0;
        for (i=0, j=0; i < len; i++) {
            if (!isalpha(node[i])) {
                /* found a non-alpha char */
                if (!isdigit(node[i])) {
                    /* if it is anything but a digit, we just use
                     * the entire name
                     */
                    fullname = true;
                    break;
                }
                /* count the size of the numeric field - but don't
                 * add the digits to the prefix
                 */
                numdigits++;
                if (startnum < 0) {
                    /* okay, this defines end of the prefix */
                    startnum = i;
                }
                continue;
            }
            if (startnum < 0) {
                prefix[j++] = node[i];
            }
        }
        if (fullname || startnum < 0) {
            /* can't compress this name - just add it to the list */
            ndreg = OBJ_NEW(orte_regex_node_t);
            ndreg->prefix = strdup(node);
            opal_list_append(&nodeids, &ndreg->super);
            /* move to the next posn */
            if (NULL == cptr) {
                break;
            }
            node = cptr + 1;
            continue;
        }
        /* convert the digits and get any suffix */
        nodenum = strtol(&node[startnum], &sfx, 10);
        if (NULL != sfx) {
            suffix = strdup(sfx);
        } else {
            suffix = NULL;
        }
        /* is this nodeid already on our list? */
        found = false;
        for (item = opal_list_get_first(&nodeids);
             !found && item != opal_list_get_end(&nodeids);
             item = opal_list_get_next(item)) {
            ndreg = (orte_regex_node_t*)item;
            if (0 < strlen(prefix) && NULL == ndreg->prefix) {
                continue;
            }
            if (0 == strlen(prefix) && NULL != ndreg->prefix) {
                continue;
            }
            if (0 < strlen(prefix) && NULL != ndreg->prefix
                && 0 != strcmp(prefix, ndreg->prefix)) {
                continue;
            }
            if (NULL == suffix && NULL != ndreg->suffix) {
                continue;
            }
            if (NULL != suffix && NULL == ndreg->suffix) {
                continue;
            }
            if (NULL != suffix && NULL != ndreg->suffix &&
                0 != strcmp(suffix, ndreg->suffix)) {
                continue;
            }
            if (numdigits != ndreg->num_digits) {
                continue;
            }
            /* found a match - flag it */
            found = true;
            /* get the last range on this nodeid - we do this
             * to preserve order
             */
            range = (orte_regex_range_t*)opal_list_get_last(&ndreg->ranges);
            if (NULL == range) {
                /* first range for this nodeid */
                range = OBJ_NEW(orte_regex_range_t);
                range->start = nodenum;
                range->cnt = 1;
                opal_list_append(&ndreg->ranges, &range->super);
                break;
            }
            /* see if the node number is out of sequence */
            if (nodenum != (range->start + range->cnt)) {
                /* start a new range */
                range = OBJ_NEW(orte_regex_range_t);
                range->start = nodenum;
                range->cnt = 1;
                opal_list_append(&ndreg->ranges, &range->super);
                break;
            }
            /* everything matches - just increment the cnt */
            range->cnt++;
            break;
        }
        if (!found) {
            /* need to add it */
            ndreg = OBJ_NEW(orte_regex_node_t);
            if (0 < strlen(prefix)) {
                ndreg->prefix = strdup(prefix);
            }
            if (NULL != suffix) {
                ndreg->suffix = strdup(suffix);
            }
            ndreg->num_digits = numdigits;
            opal_list_append(&nodeids, &ndreg->super);
            /* record the first range for this nodeid - we took
             * care of names we can't compress above
             */
            range = OBJ_NEW(orte_regex_range_t);
            range->start = nodenum;
            range->cnt = 1;
            opal_list_append(&ndreg->ranges, &range->super);
        }
        if (NULL != suffix) {
            free(suffix);
        }
        /* move to the next posn */
        if (NULL == cptr) {
            break;
        }
        node = cptr + 1;
    }

    /* begin constructing the regular expression */
    while (NULL != (item = opal_list_remove_first(&nodeids))) {
        ndreg = (orte_regex_node_t*)item;
        
        /* if no ranges, then just add the name */
        if (0 == opal_list_get_size(&ndreg->ranges)) {
            if (NULL != ndreg->prefix) {
                /* solitary node */
                asprintf(&tmp, "%s", ndreg->prefix);
                opal_argv_append_nosize(&regexargs, tmp);
                free(tmp);
            }
            OBJ_RELEASE(ndreg);
            continue;
        }
        /* if there is only one range, and it has only one node in it,
         * then we don't want to use bracket notation - so treat that
         * case separately
         */
        if (1 == opal_list_get_size(&ndreg->ranges)) {
            /* must be at least one */
            range = (orte_regex_range_t*)opal_list_get_first(&ndreg->ranges);
            /* if there is only one node in the range, then
             * just add its name
             */
            if (1 == range->cnt) {
                if (NULL != ndreg->suffix) {
                    if (NULL != ndreg->prefix) {
                        asprintf(&tmp, "%s%d%s", ndreg->prefix, range->start, ndreg->suffix);
                    } else {
                        asprintf(&tmp, "%d%s", range->start, ndreg->suffix);
                    }
                } else {
                    if (NULL != ndreg->prefix) {
                        asprintf(&tmp, "%s%d", ndreg->prefix, range->start);
                    } else {
                        asprintf(&tmp, "%d", range->start);
                    }
                }
                opal_argv_append_nosize(&regexargs, tmp);
                free(tmp);
                OBJ_RELEASE(ndreg);
                continue;
            }
        }
        /* start the regex for this nodeid with the prefix */
        if (NULL != ndreg->prefix) {
            asprintf(&tmp, "%s[%d:", ndreg->prefix, ndreg->num_digits);
        } else {
            asprintf(&tmp, "[%d:", ndreg->num_digits);
        }
        /* add the ranges */
        while (NULL != (itm2 = opal_list_remove_first(&ndreg->ranges))) {
            range = (orte_regex_range_t*)itm2;
            if (1 == range->cnt) {
                asprintf(&tmp2, "%s%d,", tmp, range->start);
            } else {
                asprintf(&tmp2, "%s%d-%d,", tmp, range->start, range->start + range->cnt - 1);
            }
            free(tmp);
            tmp = tmp2;
            OBJ_RELEASE(range);
        }
        /* replace the final comma */
        tmp[strlen(tmp)-1] = ']';
        if (NULL != ndreg->suffix) {
            /* add in the suffix, if provided */
            asprintf(&tmp2, "%s%s", tmp, ndreg->suffix);
            free(tmp);
            tmp = tmp2;
        }
        opal_argv_append_nosize(&regexargs, tmp);
        free(tmp);
        OBJ_RELEASE(ndreg);
    }
    
    /* assemble final result */
    *regexp = opal_argv_join(regexargs, ',');
    /* cleanup */
    opal_argv_free(regexargs);

    OBJ_DESTRUCT(&nodeids);


    return ORTE_SUCCESS;
}

int orte_regex_extract_node_names(char *regexp, char ***names)
{
    int i, j, k, len, ret;
    char *base;
    char *orig, *suffix;
    bool found_range = false;
    bool more_to_come = false;
    int num_digits;

    if (NULL == regexp) {
        *names = NULL;
        return ORTE_SUCCESS;
    }
    
    orig = base = strdup(regexp);
    if (NULL == base) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    OPAL_OUTPUT_VERBOSE((1, orte_debug_output,
                         "%s regex:extract:nodenames: checking nodelist: %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         regexp));
    
    do {
        /* Find the base */
        len = strlen(base);
        for (i = 0; i <= len; ++i) {
            if (base[i] == '[') {
                /* we found a range. this gets dealt with below */
                base[i] = '\0';
                found_range = true;
                break;
            }
            if (base[i] == ',') {
                /* we found a singleton node, and there are more to come */
                base[i] = '\0';
                found_range = false;
                more_to_come = true;
                break;
            }
            if (base[i] == '\0') {
                /* we found a singleton node */
                found_range = false;
                more_to_come = false;
                break;
            }
        }
        if (i == 0 && !found_range) {
            /* we found a special character at the beginning of the string */
            orte_show_help("help-regex.txt", "regex:special-char", true, regexp);
            free(orig);
            return ORTE_ERR_BAD_PARAM;
        }
        
        if (found_range) {
            /* If we found a range, get the number of digits in the numbers */
            i++;  /* step over the [ */
            for (j=i; j < len; j++) {
                if (base[j] == ':') {
                    base[j] = '\0';
                    break;
                }
            }
            if (j >= len) {
                /* we didn't find the number of digits */
                orte_show_help("help-regex.txt", "regex:num-digits-missing", true, regexp);
                free(orig);
                return ORTE_ERR_BAD_PARAM;
            }
            num_digits = strtol(&base[i], NULL, 10);
            i = j + 1;  /* step over the : */
            /* now find the end of the range */
            for (j = i; j < len; ++j) {
                if (base[j] == ']') {
                    base[j] = '\0';
                    break;
                }
            }
            if (j >= len) {
                /* we didn't find the end of the range */
                orte_show_help("help-regex.txt", "regex:end-range-missing", true, regexp);
                free(orig);
                return ORTE_ERR_BAD_PARAM;
            }
            /* check for a suffix */
            if (j+1 < len && base[j+1] != ',') {
                /* find the next comma, if present */
                for (k=j+1; k < len && base[k] != ','; k++);
                if (k < len) {
                    base[k] = '\0';
                }
                suffix = strdup(&base[j+1]);
                if (k < len) {
                    base[k] = ',';
                }
                j = k-1;
            } else {
                suffix = NULL;
            }
            OPAL_OUTPUT_VERBOSE((1, orte_debug_output,
                                 "%s regex:extract:nodenames: parsing range %s %s %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 base, base + i, suffix));

            ret = regex_parse_node_ranges(base, base + i, num_digits, suffix, names);
            if (NULL != suffix) {
                free(suffix);
            }
            if (ORTE_SUCCESS != ret) {
                orte_show_help("help-regex.txt", "regex:bad-value", true, regexp);
                free(orig);
                return ret;
            }
            if (j+1 < len && base[j + 1] == ',') {
                more_to_come = true;
                base = &base[j + 2];
            } else {
                more_to_come = false;
            }
        } else {
            /* If we didn't find a range, just add the node */
            if(ORTE_SUCCESS != (ret = opal_argv_append_nosize(names, base))) {
                ORTE_ERROR_LOG(ret);
                free(orig);
                return ret;
            }
            /* step over the comma */
            i++;
            /* set base equal to the (possible) next base to look at */
            base = &base[i];
        }
    } while(more_to_come);
    
    free(orig);
    
    /* All done */
    return ret;
}


/*
 * Parse one or more ranges in a set
 *
 * @param base     The base text of the node name
 * @param *ranges  A pointer to a range. This can contain multiple ranges
 *                 (i.e. "1-3,10" or "5" or "9,0100-0130,250") 
 * @param ***names An argv array to add the newly discovered nodes to
 */
static int regex_parse_node_ranges(char *base, char *ranges, int num_digits, char *suffix, char ***names)
{
    int i, len, ret;
    char *start, *orig;
    
    /* Look for commas, the separator between ranges */
    
    len = strlen(ranges);
    for (orig = start = ranges, i = 0; i < len; ++i) {
        if (',' == ranges[i]) {
            ranges[i] = '\0';
            ret = regex_parse_node_range(base, start, num_digits, suffix, names);
            if (ORTE_SUCCESS != ret) {
                ORTE_ERROR_LOG(ret);
                return ret;
            }
            start = ranges + i + 1;
        }
    }
    
    /* Pick up the last range, if it exists */
    
    if (start < orig + len) {
        
        OPAL_OUTPUT_VERBOSE((1, orte_debug_output,
                             "%s regex:parse:ranges: parse range %s (2)",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), start));
        
        ret = regex_parse_node_range(base, start, num_digits, suffix, names);
        if (ORTE_SUCCESS != ret) {
            ORTE_ERROR_LOG(ret);
            return ret;
        }
    }
    
    /* All done */
    return ORTE_SUCCESS;
}


/*
 * Parse a single range in a set and add the full names of the nodes
 * found to the names argv
 *
 * @param base     The base text of the node name
 * @param *ranges  A pointer to a single range. (i.e. "1-3" or "5") 
 * @param ***names An argv array to add the newly discovered nodes to
 */
static int regex_parse_node_range(char *base, char *range, int num_digits, char *suffix, char ***names)
{
    char *str, tmp[132];
    size_t i, k, start, end;
    size_t base_len, len;
    bool found;
    int ret;
    
    if (NULL == base || NULL == range) {
        return ORTE_ERROR;
    }

    len = strlen(range);
    base_len = strlen(base);
    /* Silence compiler warnings; start and end are always assigned
     properly, below */
    start = end = 0;
    
    /* Look for the beginning of the first number */
    
    for (found = false, i = 0; i < len; ++i) {
        if (isdigit((int) range[i])) {
            if (!found) {
                start = atoi(range + i);
                found = true;
                break;
            }
        }
    }
    if (!found) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }
    
    /* Look for the end of the first number */
    
    for (found = false; i < len; ++i) {
        if (!isdigit(range[i])) {
            break;
        }
    }
    
    /* Was there no range, just a single number? */
    
    if (i >= len) {
        end = start;
        found = true;
    } else {
        /* Nope, there was a range.  Look for the beginning of the second
         * number
         */
        for (; i < len; ++i) {
            if (isdigit(range[i])) {
                end = strtol(range + i, NULL, 10);
                found = true;
                break;
            }
        }
    }
    if (!found) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }
    
    /* Make strings for all values in the range */
    
    len = base_len + num_digits + 32;
    if (NULL != suffix) {
        len += strlen(suffix);
    }
    str = (char *) malloc(len);
    if (NULL == str) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    for (i = start; i <= end; ++i) {
        memset(str, 0, len);
        strcpy(str, base);
        /* we need to zero-pad the digits */
        for (k=0; k < (size_t)num_digits; k++) {
            str[k+base_len] = '0';
        }
        memset(tmp, 0, 132);
        snprintf(tmp, 132, "%lu", (unsigned long)i);
        for (k=0; k < strlen(tmp); k++) {
            str[base_len + num_digits - k - 1] = tmp[strlen(tmp)-k-1];
        }
        /* if there is a suffix, add it */
        if (NULL != suffix) {
            strcat(str, suffix);
        }
        ret = opal_argv_append_nosize(names, str);
        if(ORTE_SUCCESS != ret) {
            ORTE_ERROR_LOG(ret);
            free(str);
            return ret;
        }
    }
    free(str);
    
    /* All done */
    return ORTE_SUCCESS;
}

/* Compute the #procs on each node given a regex of form
 * "#procs(x#nodes),#procs(x#nodes). In other words, an
 * expression of "4(x30) will be interpreted to mean four
 * procs on each of the next 30 nodes.
 */
int orte_regex_extract_ppn(int num_nodes, char *regexp, int **ppn)
{
    int *tmp;
    char *begptr, *endptr, *orig;
    int i, j, count, reps;
    
    /* init null answer */
    *ppn = NULL;
    
    tmp = (int *) malloc(sizeof(int) * num_nodes);
    if (NULL == tmp) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    memset(tmp, 0, sizeof(int) * num_nodes);
    
    orig = begptr = strdup(regexp);
    if (NULL == begptr) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        free(tmp);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    j = 0;
    while (begptr) {
        count = strtol(begptr, &endptr, 10);
        if ((endptr[0] == '(') && (endptr[1] == 'x')) {
            reps = strtol((endptr+2), &endptr, 10);
            if (endptr[0] == ')') {
                endptr++;
            }
        } else {
            reps = 1;
        }
        
        for (i = 0; i < reps && j < num_nodes; i++) {
            tmp[j++] = count;
        }
        
        if (*endptr == ',') {
            begptr = endptr + 1;
        } else if (*endptr == '\0' || j >= num_nodes) {
            break;
        } else {
            orte_show_help("help-regex.txt", "regex:bad-ppn", true, regexp);
            ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
            free(tmp);
            free(orig);
            return ORTE_ERR_BAD_PARAM;
        }
    }
    
    free(orig);
    
    /* return values */
    *ppn = tmp;
    
    return ORTE_SUCCESS;
}

static void range_construct(orte_regex_range_t *ptr)
{
    ptr->start = 0;
    ptr->cnt = 0;
}
OBJ_CLASS_INSTANCE(orte_regex_range_t,
                   opal_list_item_t,
                   range_construct, NULL);

static void orte_regex_node_construct(orte_regex_node_t *ptr)
{
    ptr->prefix = NULL;
    ptr->suffix = NULL;
    ptr->num_digits = 0;
    OBJ_CONSTRUCT(&ptr->ranges, opal_list_t);
}
static void orte_regex_node_destruct(orte_regex_node_t *ptr)
{
    opal_list_item_t *item;

    if (NULL != ptr->prefix) {
        free(ptr->prefix);
    }
    if (NULL != ptr->suffix) {
        free(ptr->suffix);
    }

    while (NULL != (item = opal_list_remove_first(&ptr->ranges))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&ptr->ranges);
}
OBJ_CLASS_INSTANCE(orte_regex_node_t,
                   opal_list_item_t,
                   orte_regex_node_construct,
                   orte_regex_node_destruct);

