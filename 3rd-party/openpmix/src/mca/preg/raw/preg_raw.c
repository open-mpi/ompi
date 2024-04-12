/*
 * Copyright (c) 2015-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2016-2019 IBM Corporation.  All rights reserved.
 * Copyright (c) 2018      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 *
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "src/include/pmix_config.h"

#ifdef HAVE_STRING_H
#    include <string.h>
#endif
#include <fcntl.h>
#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#    include <sys/types.h>
#endif
#include <ctype.h>

#include "include/pmix.h"
#include "pmix_common.h"

#include "src/mca/bfrops/base/base.h"
#include "src/util/pmix_argv.h"
#include "src/util/pmix_error.h"
#include "src/util/pmix_printf.h"

#include "preg_raw.h"
#include "src/mca/preg/base/base.h"

static pmix_status_t generate_node_regex(const char *input, char **regex);
static pmix_status_t generate_ppn(const char *input, char **ppn);
static pmix_status_t parse_nodes(const char *regexp, char ***names);
static pmix_status_t parse_procs(const char *regexp, char ***procs);
static pmix_status_t copy(char **dest, size_t *len, const char *input);
static pmix_status_t pack(pmix_buffer_t *buffer, const char *input);
static pmix_status_t unpack(pmix_buffer_t *buffer, char **regex);
static pmix_status_t release(char *regexp);

pmix_preg_module_t pmix_preg_raw_module = {
    .name = "raw",
    .generate_node_regex = generate_node_regex,
    .generate_ppn = generate_ppn,
    .parse_nodes = parse_nodes,
    .parse_procs = parse_procs,
    .copy = copy,
    .pack = pack,
    .unpack = unpack,
    .release = release
};

static pmix_status_t generate_node_regex(const char *input, char **regexp)
{
    if (0 == strncmp(input, "raw:", 4)) {
        *regexp = strdup(input);
    } else {
        pmix_asprintf(regexp, "raw:%s", input);
    }

    return PMIX_SUCCESS;
}

static pmix_status_t generate_ppn(const char *input, char **regexp)
{
    if (0 == strncmp(input, "raw:", 4)) {
        *regexp = strdup(input);
    } else {
        pmix_asprintf(regexp, "raw:%s", input);
    }

    return PMIX_SUCCESS;
}

static pmix_status_t parse_nodes(const char *regexp, char ***names)
{
    if (0 != strncmp(regexp, "raw:", 4)) {
        return PMIX_ERR_TAKE_NEXT_OPTION;
    }

    *names = PMIx_Argv_split(&regexp[4], ',');
    return PMIX_SUCCESS;
}
static pmix_status_t parse_procs(const char *regexp, char ***procs)
{
    if (0 != strncmp(regexp, "raw:", 4)) {
        return PMIX_ERR_TAKE_NEXT_OPTION;
    }

    *procs = PMIx_Argv_split(&regexp[4], ';');
    return PMIX_SUCCESS;
}

static pmix_status_t copy(char **dest, size_t *len, const char *input)
{
    if (0 != strncmp(input, "raw:", 4)) {
        return PMIX_ERR_TAKE_NEXT_OPTION;
    }

    *dest = strdup(input);
    *len = strlen(input) + 1;
    return PMIX_SUCCESS;
}

static pmix_status_t pack(pmix_buffer_t *buffer, const char *input)
{
    size_t slen;
    char *ptr;

    if (0 != strncmp(input, "raw:", 4)) {
        return PMIX_ERR_TAKE_NEXT_OPTION;
    }

    /* extract the size */
    slen = strlen(input) + 1; // retain the NULL terminator

    /* ensure the buffer has enough space */
    ptr = pmix_bfrop_buffer_extend(buffer, slen);
    if (NULL == ptr) {
        return PMIX_ERR_NOMEM;
    }

    /* xfer the data */
    memcpy(ptr, input, slen);
    buffer->bytes_used += slen;
    buffer->pack_ptr += slen;

    return PMIX_SUCCESS;
}

static pmix_status_t unpack(pmix_buffer_t *buffer, char **regex)
{
    char *ptr;

    ptr = buffer->unpack_ptr;

    if (0 != strncmp(ptr, "raw:", 4)) {
        return PMIX_ERR_TAKE_NEXT_OPTION;
    }

    *regex = strdup(ptr);
    buffer->unpack_ptr += strlen(ptr) + 1;

    if (NULL == *regex) {
        return PMIX_ERR_NOMEM;
    }
    return PMIX_SUCCESS;
}

static pmix_status_t release(char *regexp)
{
    if (NULL == regexp) {
        return PMIX_SUCCESS;
    }
    if (0 != strncmp(regexp, "raw:", 4)) {
        return PMIX_ERR_TAKE_NEXT_OPTION;
    }
    free(regexp);
    return PMIX_SUCCESS;
}
