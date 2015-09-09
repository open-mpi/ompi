/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2015 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014-2015 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2014      Artem Y. Polyakov <artpol84@gmail.com>.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <private/autogen/config.h>
#include <pmix/rename.h>
#include <private/types.h>
#include <private/pmix_stdint.h>

#include <pmix.h>
#include "src/include/pmix_globals.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include <fcntl.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <ctype.h>

#include "src/class/pmix_list.h"
#include "src/buffer_ops/buffer_ops.h"
#include "src/util/argv.h"
#include "src/util/error.h"
#include "src/util/output.h"
#include "src/server/pmix_server_ops.h"

static pmix_status_t regex_parse_value_ranges(char *base, char *ranges,
                                              int num_digits, char *suffix,
                                              char ***names);
static pmix_status_t regex_parse_value_range(char *base, char *range,
                                             int num_digits, char *suffix,
                                             char ***names);
static pmix_status_t pmix_regex_extract_nodes(char *regexp, char ***names);
static pmix_status_t pmix_regex_extract_ppn(char *regexp, char ***procs);

/* we need to pass three things to the client:
 *
 * (a) the list of nodes involved in this nspace
 *
 * (b) the hostname for each proc in this nspace
 *
 * (c) the list of procs on each node for reverse lookup
 */
void pmix_pack_proc_map(pmix_buffer_t *buf,
                        char **nodes, char **procs)
{
    pmix_kval_t kv;
    pmix_value_t val;
    pmix_status_t rc;
    pmix_buffer_t buf2;
    size_t i, nnodes;

    /* bozo check - need procs for each node */
    if (pmix_argv_count(nodes) != pmix_argv_count(procs)) {
        PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
        return;
    }

    PMIX_CONSTRUCT(&buf2, pmix_buffer_t);
    PMIX_CONSTRUCT(&kv, pmix_kval_t);
    kv.value = &val;
    val.type = PMIX_STRING;

    /* pass the number of nodes involved in this namespace */
    nnodes = pmix_argv_count(nodes);
    if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(&buf2, &nnodes, 1, PMIX_SIZE))) {
        PMIX_ERROR_LOG(rc);
        goto cleanup;
    }

    for (i=0; i < nnodes; i++) {
        /* pass the complete list of procs on this node */
        kv.key = nodes[i];
        val.data.string = procs[i];
        if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(&buf2, &kv, 1, PMIX_KVAL))) {
            PMIX_ERROR_LOG(rc);
            kv.key = NULL;
            val.data.string = NULL;
            goto cleanup;
        }
    }
    kv.key = NULL;
    val.data.string = NULL;

    /* pass the completed blob */
    kv.key = PMIX_MAP_BLOB;
    val.type = PMIX_BYTE_OBJECT;
    val.data.bo.bytes = buf2.base_ptr;
    val.data.bo.size = buf2.bytes_used;
    if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(buf, &kv, 1, PMIX_KVAL))) {
        PMIX_ERROR_LOG(rc);
    }
    kv.key = NULL;
    kv.value = NULL;
    val.data.bo.bytes = NULL;
    val.data.bo.size = 0;

 cleanup:
    PMIX_DESTRUCT(&buf2);
    PMIX_DESTRUCT(&kv);
    return;
}


pmix_status_t pmix_regex_parse_nodes(const char *regexp, char ***names)
{
    char *tmp, *ptr;
    pmix_status_t rc;

    /* set default */
    *names = NULL;

    /* protect against bozo */
    if (NULL == regexp) {
        return PMIX_SUCCESS;
    }

    /* protect the input string */
    tmp = strdup(regexp);
    /* strip the trailing bracket */
    tmp[strlen(tmp)-1] = '\0';

    /* the regex generator used to create this regex
     * is tagged at the beginning of the string */
    if (NULL == (ptr = strchr(tmp, '['))) {
        PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
        free(tmp);
        return PMIX_ERR_BAD_PARAM;
    }
    *ptr = '\0';
    ++ptr;

    /* if it was done by PMIx, use that parser */
    if (0 == strcmp(tmp, "pmix")) {
        if (PMIX_SUCCESS != (rc = pmix_regex_extract_nodes(ptr, names))) {
            PMIX_ERROR_LOG(rc);
        }
    } else {
        PMIX_ERROR_LOG(PMIX_ERR_NOT_SUPPORTED);
        rc = PMIX_ERR_NOT_SUPPORTED;
    }
    free(tmp);
    return rc;
}


pmix_status_t pmix_regex_parse_procs(const char *regexp, char ***procs)
{
    char *tmp, *ptr;
    pmix_status_t rc;

    /* set default */
    *procs = NULL;

    /* protect against bozo */
    if (NULL == regexp) {
        return PMIX_SUCCESS;
    }

    /* protect the input string */
    tmp = strdup(regexp);
    /* strip the trailing bracket */
    tmp[strlen(tmp)-1] = '\0';

    /* the regex generator used to create this regex
     * is tagged at the beginning of the string */
    if (NULL == (ptr = strchr(tmp, '['))) {
        PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
        free(tmp);
        return PMIX_ERR_BAD_PARAM;
    }
    *ptr = '\0';
    ++ptr;

    /* if it was done by PMIx, use that parser */
    if (0 == strcmp(tmp, "pmix")) {
        if (PMIX_SUCCESS != (rc = pmix_regex_extract_ppn(ptr, procs))) {
            PMIX_ERROR_LOG(rc);
        }
    } else {
        PMIX_ERROR_LOG(PMIX_ERR_NOT_SUPPORTED);
        rc = PMIX_ERR_NOT_SUPPORTED;
    }
    free(tmp);
    return rc;
}


static pmix_status_t pmix_regex_extract_nodes(char *regexp, char ***names)
{
    int i, j, k, len;
    pmix_status_t ret;
    char *base;
    char *orig, *suffix;
    bool found_range = false;
    bool more_to_come = false;
    int num_digits;

    /* set the default */
    *names = NULL;

    if (NULL == regexp) {
        return PMIX_SUCCESS;
    }

    orig = base = strdup(regexp);
    if (NULL == base) {
        PMIX_ERROR_LOG(PMIX_ERR_OUT_OF_RESOURCE);
        return PMIX_ERR_OUT_OF_RESOURCE;
    }

    PMIX_OUTPUT_VERBOSE((1, pmix_globals.debug_output,
                         "pmix:extract:nodes: checking list: %s", regexp));

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
                /* we found a singleton value, and there are more to come */
                base[i] = '\0';
                found_range = false;
                more_to_come = true;
                break;
            }
            if (base[i] == '\0') {
                /* we found a singleton value */
                found_range = false;
                more_to_come = false;
                break;
            }
        }
        if (i == 0 && !found_range) {
            /* we found a special character at the beginning of the string */
            free(orig);
            return PMIX_ERR_BAD_PARAM;
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
                free(orig);
                return PMIX_ERR_BAD_PARAM;
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
                free(orig);
                return PMIX_ERR_BAD_PARAM;
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
            PMIX_OUTPUT_VERBOSE((1, pmix_globals.debug_output,
                                 "regex:extract:nodes: parsing range %s %s %s",
                                 base, base + i, suffix));

            ret = regex_parse_value_ranges(base, base + i, num_digits, suffix, names);
            if (NULL != suffix) {
                free(suffix);
            }
            if (PMIX_SUCCESS != ret) {
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
            /* If we didn't find a range, just add the value */
            if(PMIX_SUCCESS != (ret = pmix_argv_append_nosize(names, base))) {
                PMIX_ERROR_LOG(ret);
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
 * @param base     The base text of the value name
 * @param *ranges  A pointer to a range. This can contain multiple ranges
 *                 (i.e. "1-3,10" or "5" or "9,0100-0130,250")
 * @param ***names An argv array to add the newly discovered values to
 */
static pmix_status_t regex_parse_value_ranges(char *base, char *ranges,
                                              int num_digits, char *suffix,
                                              char ***names)
{
    int i, len;
    pmix_status_t ret;
    char *start, *orig;

    /* Look for commas, the separator between ranges */

    len = strlen(ranges);
    for (orig = start = ranges, i = 0; i < len; ++i) {
        if (',' == ranges[i]) {
            ranges[i] = '\0';
            ret = regex_parse_value_range(base, start, num_digits, suffix, names);
            if (PMIX_SUCCESS != ret) {
                PMIX_ERROR_LOG(ret);
                return ret;
            }
            start = ranges + i + 1;
        }
    }

    /* Pick up the last range, if it exists */

    if (start < orig + len) {

        PMIX_OUTPUT_VERBOSE((1, pmix_globals.debug_output,
                             "regex:parse:ranges: parse range %s (2)", start));

        ret = regex_parse_value_range(base, start, num_digits, suffix, names);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            return ret;
        }
    }

    /* All done */
    return PMIX_SUCCESS;
}


/*
 * Parse a single range in a set and add the full names of the values
 * found to the names argv
 *
 * @param base     The base text of the value name
 * @param *ranges  A pointer to a single range. (i.e. "1-3" or "5")
 * @param ***names An argv array to add the newly discovered values to
 */
static pmix_status_t regex_parse_value_range(char *base, char *range,
                                             int num_digits, char *suffix,
                                             char ***names)
{
    char *str, tmp[132];
    size_t i, k, start, end;
    size_t base_len, len;
    bool found;
    pmix_status_t ret;

    if (NULL == base || NULL == range) {
        return PMIX_ERROR;
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
        PMIX_ERROR_LOG(PMIX_ERR_NOT_FOUND);
        return PMIX_ERR_NOT_FOUND;
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
        PMIX_ERROR_LOG(PMIX_ERR_NOT_FOUND);
        return PMIX_ERR_NOT_FOUND;
    }

    /* Make strings for all values in the range */

    len = base_len + num_digits + 32;
    if (NULL != suffix) {
        len += strlen(suffix);
    }
    str = (char *) malloc(len);
    if (NULL == str) {
        PMIX_ERROR_LOG(PMIX_ERR_OUT_OF_RESOURCE);
        return PMIX_ERR_OUT_OF_RESOURCE;
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
        ret = pmix_argv_append_nosize(names, str);
        if(PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            free(str);
            return ret;
        }
    }
    free(str);

    /* All done */
    return PMIX_SUCCESS;
}

static pmix_status_t pmix_regex_extract_ppn(char *regexp, char ***procs)
{
    char **rngs, **nds, *t, **ps=NULL;
    int i, j, k, start, end;

    /* split on semi-colons for nodes */
    nds = pmix_argv_split(regexp, ';');
    for (j=0; NULL != nds[j]; j++) {
        /* for each node, split it by comma */
        rngs = pmix_argv_split(nds[j], ',');
        /* parse each element */
        for (i=0; NULL != rngs[i]; i++) {
            /* look for a range */
            if (NULL == (t = strchr(rngs[i], '-'))) {
                /* just one value */
                pmix_argv_append_nosize(&ps, rngs[i]);
            } else {
                /* handle the range */
                *t = '\0';
                start = strtol(rngs[i], NULL, 10);
                ++t;
                end = strtol(t, NULL, 10);
                for (k=start; k <= end; k++) {
                    asprintf(&t, "%d", k);
                    pmix_argv_append_nosize(&ps, t);
                    free(t);
                }
            }
        }
        pmix_argv_free(rngs);
        /* create the node entry */
        t = pmix_argv_join(ps, ',');
        pmix_argv_append_nosize(procs, t);
        free(t);
        pmix_argv_free(ps);
        ps = NULL;
    }

    pmix_argv_free(nds);
    return PMIX_SUCCESS;
}
