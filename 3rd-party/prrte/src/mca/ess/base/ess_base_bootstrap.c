/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2021 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2009      Institut National de Recherche en Informatique
 *                         et Automatique. All rights reserved.
 * Copyright (c) 2011-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2017      IBM Corporation. All rights reserved.
 * Copyright (c) 2019      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * Copyright (c) 2023      Triad National Security, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "prte_config.h"
#include "constants.h"

#include <stdio.h>
#include <sys/types.h>
#ifdef HAVE_FCNTL_H
#    include <fcntl.h>
#endif
#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif

#include "src/util/proc_info.h"

#include "src/event/event-internal.h"
#include "src/hwloc/hwloc-internal.h"
#include "src/pmix/pmix-internal.h"
#include "src/util/pmix_os_path.h"
#include "src/util/pmix_environ.h"
#include "src/util/pmix_string_copy.h"

#include "src/mca/prteinstalldirs/base/base.h"
#include "src/rml/rml.h"
#include "src/rml/rml_contact.h"

#include "src/runtime/prte_globals.h"
#include "src/util/name_fns.h"
#include "src/util/session_dir.h"
#include "src/util/pmix_show_help.h"

#include "src/mca/ess/base/base.h"

static pmix_status_t regex_extract_nodes(char *regexp, char ***names);
static pmix_status_t regex_parse_value_ranges(char *base, char *ranges,
                                              int num_digits, char *suffix,
                                              char ***names);
static pmix_status_t regex_parse_value_range(char *base, char *range,
                                             int num_digits, char *suffix,
                                             char ***names);
static pmix_status_t read_file(char *regexp, char ***names);

#if PMIX_NUMERIC_VERSION < 0x00040205
static char *pmix_getline(FILE *fp)
{
    char *ret, *buff;
    char input[1024];

    ret = fgets(input, 1024, fp);
    if (NULL != ret) {
        input[strlen(input) - 1] = '\0'; /* remove newline */
        buff = strdup(input);
        return buff;
    }

    return NULL;
}
#endif

int prte_ess_base_bootstrap(void)
{
    char *path, *line, *ptr;
    FILE *fp;
    int n;
    char *cluster = NULL;
    char *ctrlhost = NULL;
    uint32_t ctrlport = UINT32_MAX;
    uint32_t prtedport = UINT32_MAX;
    char *dvmnodes = NULL;
    char *dvmtmpdir = NULL;
    char *sessiontmpdir = NULL;
    bool ctrllogjobstate = false;
    bool ctrllogprocstate = false;
    char *ctrllogpath = NULL;
    bool prtedlogjobstate = false;
    bool prtedlogprocstate = false;
    char *prtedlogpath = NULL;
    char **nodes = NULL;
    int rc = PRTE_ERR_SILENT;

    /* see if we can open a configuration file */
    path = pmix_os_path(false, prte_install_dirs.sysconfdir, "prte.conf", NULL);
    fp = fopen(path, "r");
    if (NULL == fp) {
        pmix_show_help("help-prte-runtime.txt", "bootstrap-not-found", true,
                       prte_process_info.nodename, path);
        free(path);
        return PRTE_ERR_SILENT;
    }

    while (NULL != (line = pmix_getline(fp))) {
        /* ignore if line is empty or comment */
        if (0 == strlen(line) || '#' == line[0]) {
            free(line);
            continue;
        }
        /* split on the '=' sign */
        if (NULL == (ptr = strchr(line, '='))) {
            /* bad file */
            pmix_show_help("help-prte-runtime.txt", "bootstrap-bad-entry", true,
                           prte_process_info.nodename, path, line);
            free(path);
            fclose(fp);
            return PRTE_ERR_SILENT;
        }
        *ptr = '\0';
        if (0 == strlen(line)) {   // missing the field name
            /* restore the '=' sign */
            *ptr = '=';
            pmix_show_help("help-prte-runtime.txt", "bootstrap-missing-field-name", true,
                           prte_process_info.nodename, path, ptr);
            free(path);
            fclose(fp);
            return PRTE_ERR_SILENT;
        }
        ++ptr;
        if (NULL == ptr) {    // missing the value
            pmix_show_help("help-prte-runtime.txt", "bootstrap-missing-value", true,
                           prte_process_info.nodename, path, line);
            free(path);
            fclose(fp);
            return PRTE_ERR_SILENT;
        }
        /* identify and cache the option */
        if (0 == strcmp(line, "ClusterName")) {
            cluster = strdup(ptr);
        } else if (0 == strcmp(line, "DVMControllerHost")) {
            ctrlhost = strdup(ptr);
        } else if (0 == strcmp(line, "DVMControllerPort")) {
            ctrlport = strtoul(ptr, NULL, 10);
        } else if (0 == strcmp(line, "PRTEDPort")) {
            prtedport = strtoul(ptr, NULL, 10);
        } else if (0 == strcmp(line, "DVMNodes")) {
            dvmnodes = strdup(ptr);
        } else if (0 == strcmp(line, "DVMTempDir")) {
            dvmtmpdir = strdup(ptr);
        } else if (0 == strcmp(line, "SessionTmpDir")) {
            sessiontmpdir = strdup(ptr);
        } else if (0 == strcmp(line, "ControllerLogJobState")) {
            if (0 == strcasecmp(ptr, "on") ||
                0 == strncasecmp(ptr, "true", 1)) {
                ctrllogjobstate = true;
            }
        } else if (0 == strcmp(line, "ControllerLogProcState")) {
            if (0 == strcasecmp(ptr, "on") ||
                0 == strncasecmp(ptr, "true", 1)) {
                ctrllogprocstate = true;
            }
        } else if (0 == strcmp(line, "ControllerLogPath")) {
            ctrllogpath = strdup(ptr);
        } else if (0 == strcmp(line, "PRTEDLogJobState")) {
            if (0 == strcasecmp(ptr, "on") ||
                0 == strncasecmp(ptr, "true", 1)) {
                prtedlogjobstate = true;
            }
        } else if (0 == strcmp(line, "PRTEDLogProcState")) {
            if (0 == strcasecmp(ptr, "on") ||
                0 == strncasecmp(ptr, "true", 1)) {
                prtedlogprocstate = true;
            }
        } else if (0 == strcmp(line, "PRTEDLogPath")) {
            prtedlogpath = strdup(ptr);
        }
        free(line);
    }
    fclose(fp);

    /* we require the node list */
    if (NULL == dvmnodes) {
        pmix_show_help("help-prte-runtime.txt", "bootstrap-missing-entry", true,
                       prte_process_info.nodename, path, "DVMNodes");
        goto cleanup;
    }
    /* we must be able to parse that list */
    rc = regex_extract_nodes(dvmnodes, &nodes);
    if (PMIX_SUCCESS != rc) {
        pmix_show_help("help-prte-runtime.txt", "bootstrap-bad-nodelist", true,
                       prte_process_info.nodename, path, dvmnodes,
                       PMIx_Error_string(rc));
        goto cleanup;
    }
    /* we must have the controller host so we can find it */
    if (NULL == ctrlhost) {
        pmix_show_help("help-prte-runtime.txt", "bootstrap-missing-entry", true,
                       prte_process_info.nodename, path, "DVMControllerHost");
        goto cleanup;
    }
    /* we must have a prted port so we can contact our parent */
    if (UINT32_MAX == prtedport) {
        pmix_show_help("help-prte-runtime.txt", "bootstrap-missing-entry", true,
                       prte_process_info.nodename, path, "DVMControllerPort");
        goto cleanup;
    }
    /* if we aren't given a controller port, default to the prted port */
    if (UINT32_MAX == ctrlport) {
        ctrlport = prtedport;
    }

    /* report the nodes */
    for (n=0; NULL != nodes[n]; n++) {
        pmix_output(0, "NODE[%d]: %s", n, nodes[n]);
    }

    rc = PRTE_SUCCESS;

cleanup:
    if (NULL != cluster) {
        free(cluster);
    }
    if (NULL != dvmnodes) {
        free(dvmnodes);
    }
    if (NULL != nodes) {
        PMIX_ARGV_FREE_COMPAT(nodes);
    }
    if (NULL != dvmtmpdir) {
        free(dvmtmpdir);
    }
    if (NULL != sessiontmpdir) {
        free(sessiontmpdir);
    }
    if (NULL != ctrllogpath) {
        free(ctrllogpath);
    }
    if (NULL != prtedlogpath) {
        free(prtedlogpath);
    }
    return rc;
}

static pmix_status_t regex_extract_nodes(char *regexp, char ***names)
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
        return PMIX_ERR_BAD_PARAM;
    }

    /* see what regex we were given. Supported options:
     *
     * file:<path> - file of names, one per line
     * <regex> - PMIx native regex, which is a comma-delimited
     *                list of ranges or names
     */
    if (0 == strncasecmp(regexp, "file:", 5)) {
        /* skip over the "file:" portion */
        ret = read_file(&regexp[5], names);
        return ret;
    }

    orig = base = strdup(regexp);
    if (NULL == base) {
        PMIX_ERROR_LOG(PMIX_ERR_OUT_OF_RESOURCE);
        return PMIX_ERR_OUT_OF_RESOURCE;
    }

    pmix_output_verbose(1, prte_ess_base_framework.framework_output,
                         "bootstrap:extract:nodes: checking list: %s",
                         regexp);

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
            i++; /* step over the [ */
            for (j = i; j < len; j++) {
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
            i = j + 1; /* step over the : */
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
            if (j + 1 < len && base[j + 1] != ',') {
                /* find the next comma, if present */
                for (k = j + 1; k < len && base[k] != ','; k++)
                    ;
                if (k < len) {
                    base[k] = '\0';
                }
                suffix = strdup(&base[j + 1]);
                if (k < len) {
                    base[k] = ',';
                }
                j = k - 1;
            } else {
                suffix = NULL;
            }
            pmix_output_verbose(1, prte_ess_base_framework.framework_output,
                                 "bootstrap:extract:nodes: parsing range %s %s %s",
                                 base, base + i, suffix);

            ret = regex_parse_value_ranges(base, base + i, num_digits, suffix, names);
            if (NULL != suffix) {
                free(suffix);
            }
            if (PMIX_SUCCESS != ret) {
                free(orig);
                return ret;
            }
            if (j + 1 < len && base[j + 1] == ',') {
                more_to_come = true;
                base = &base[j + 2];
            } else {
                more_to_come = false;
            }
        } else {
            /* If we didn't find a range, just add the value */
            PMIX_ARGV_APPEND_NOSIZE_COMPAT(names, base);
            /* step over the comma */
            i++;
            /* set base equal to the (possible) next base to look at */
            base = &base[i];
        }
    } while (more_to_come);

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
static pmix_status_t regex_parse_value_ranges(char *base, char *ranges, int num_digits,
                                              char *suffix, char ***names)
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

        pmix_output_verbose(1, prte_ess_base_framework.framework_output,
                             "bootstrap:parse:ranges: parse range %s (2)",
                             start);

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
static pmix_status_t regex_parse_value_range(char *base, char *range, int num_digits, char *suffix,
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
                start = strtol(range + i, NULL, 10);
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
        for (k = 0; k < (size_t) num_digits; k++) {
            str[k + base_len] = '0';
        }
        memset(tmp, 0, 132);
        pmix_snprintf(tmp, 132, "%lu", (unsigned long) i);
        for (k = 0; k < strlen(tmp); k++) {
            str[base_len + num_digits - k - 1] = tmp[strlen(tmp) - k - 1];
        }
        /* if there is a suffix, add it */
        if (NULL != suffix) {
            strcat(str, suffix);
        }
        PMIX_ARGV_APPEND_NOSIZE_COMPAT(names, str);
    }
    free(str);

    /* All done */
    return PMIX_SUCCESS;
}

static pmix_status_t read_file(char *regexp, char ***names)
{
    char *line;
    FILE *fp;

    fp = fopen(regexp, "r");
    if (NULL == fp) {
        return PMIX_ERR_BAD_PARAM;
    }
    while (NULL != (line = pmix_getline(fp))) {
        /* ignore if line is empty or comment */
        if (0 == strlen(line) || '#' == line[0]) {
            free(line);
            continue;
        }
        PMIX_ARGV_APPEND_NOSIZE_COMPAT(names, line);
        free(line);
    }
    fclose(fp);
    return PMIX_SUCCESS;
}
