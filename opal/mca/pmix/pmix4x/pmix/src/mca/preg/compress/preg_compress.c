/*
 * Copyright (c) 2015-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2016-2019 IBM Corporation.  All rights reserved.
 * Copyright (c) 2018      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <src/include/pmix_config.h>

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


#include <pmix_common.h>
#include <pmix.h>

#include "src/include/pmix_socket_errno.h"
#include "src/include/pmix_globals.h"
#include "src/util/argv.h"
#include "src/util/error.h"
#include "src/util/output.h"
#include "src/class/pmix_list.h"
#include "src/mca/bfrops/base/base.h"
#include "src/mca/gds/gds.h"
#include "src/client/pmix_client_ops.h"

#include "src/mca/pcompress/pcompress.h"
#include "src/mca/preg/base/base.h"
#include "preg_compress.h"

static pmix_status_t generate_node_regex(const char *input,
                                         char **regex);
static pmix_status_t generate_ppn(const char *input,
                                  char **ppn);
static pmix_status_t parse_nodes(const char *regexp,
                                 char ***names);
static pmix_status_t parse_procs(const char *regexp,
                                 char ***procs);
static pmix_status_t copy(char **dest, size_t *len, const char *input);
static pmix_status_t pack(pmix_buffer_t *buffer, const char *input);
static pmix_status_t unpack(pmix_buffer_t *buffer, char **regex);

pmix_preg_module_t pmix_preg_compress_module = {
    .name = "compress",
    .generate_node_regex = generate_node_regex,
    .generate_ppn = generate_ppn,
    .parse_nodes = parse_nodes,
    .parse_procs = parse_procs,
    .resolve_peers = pmix_preg_base_std_resolve_peers,
    .resolve_nodes = pmix_preg_base_std_resolve_nodes,
    .copy = copy,
    .pack = pack,
    .unpack = unpack
};

static pmix_status_t generate_node_regex(const char *input,
                                         char **regexp)
{
    char *result, *slen;
    size_t len;
    uint8_t *tmp;

    if (!pmix_compress.compress_string((char*)input, &tmp, &len)) {
        PMIX_ERROR_LOG(PMIX_ERR_TAKE_NEXT_OPTION);
        return PMIX_ERR_TAKE_NEXT_OPTION;
    }

    if (NULL == tmp) {
        PMIX_ERROR_LOG(PMIX_ERR_NOMEM);
        return PMIX_ERR_NOMEM;
    }

    /* convert the length to a string */
    if (0 > asprintf(&slen, "%lu", (unsigned long)len)) {
        free(tmp);
        return PMIX_ERR_NOMEM;
    }

    /* create the result */
    result = calloc(len + 6 + strlen(slen) + 1, sizeof(char));
    strcpy(result, "blob");
    result[5] = ':';
    strcpy(&result[6], slen);
    memcpy(&result[6 + strlen(slen) + 1], tmp, len);
    free(tmp);
    free(slen);

    *regexp = result;
    return PMIX_SUCCESS;
}

static pmix_status_t generate_ppn(const char *input,
                                  char **regexp)
{
    char *result, *slen;
    size_t len;
    uint8_t *tmp;

    if (!pmix_compress.compress_string((char*)input, &tmp, &len)) {
        PMIX_ERROR_LOG(PMIX_ERR_TAKE_NEXT_OPTION);
        return PMIX_ERR_TAKE_NEXT_OPTION;
    }

    if (NULL == tmp) {
        PMIX_ERROR_LOG(PMIX_ERR_NOMEM);
        return PMIX_ERR_NOMEM;
    }

    /* print the length */
    if (0 > asprintf(&slen, "%lu", (unsigned long)len)) {
        free(tmp);
        return PMIX_ERR_NOT_FOUND;
    }

    /* create the result */
    result = calloc(len + 6 + strlen(slen) + 1, sizeof(char));
    strcpy(result, "blob");
    /* leave a gap - calloc will have put a NULL in it */
    result[5] = ':';
    /* add the size */
    strcpy(&result[6], slen);
    /* leave a NULL gap at the end of the size */
    memcpy(&result[6] + strlen(slen) + 1, tmp, len);
    free(tmp);

    *regexp = result;
    return PMIX_SUCCESS;
}

static pmix_status_t parse_nodes(const char *regexp,
                                 char ***names)
{
    char *tmp, *ptr, **argv;
    size_t len;

    if (0 != strncmp(regexp, "blob", 4)) {
        return PMIX_ERR_TAKE_NEXT_OPTION;
    }

    len = strtoul(&regexp[6], &ptr, 10);
    ++ptr;  // step over NULL
    if (NULL == ptr) {
        return PMIX_ERR_BAD_PARAM;
    }
    /* malloc the space */
    tmp = malloc(len);
    if (NULL == tmp) {
        return PMIX_ERR_NOMEM;
    }

    if (!pmix_compress.decompress_string(&tmp, (uint8_t*)ptr, len)) {
        free(tmp);
        return PMIX_ERR_TAKE_NEXT_OPTION;
    }
    /* tmp now contains the comma-delimited list of node names */
    argv = pmix_argv_split(tmp, ',');
    free(tmp);
    *names = argv;
    return PMIX_SUCCESS;
}
static pmix_status_t parse_procs(const char *regexp,
                                 char ***procs)
{
    char *tmp, *ptr, **argv;
    size_t len;

    if (0 != strncmp(regexp, "blob", 4)) {
        return PMIX_ERR_TAKE_NEXT_OPTION;
    }

    /* extract the size */
    len = strtoul(&regexp[6], &ptr, 10);
    ++ptr;  // step over NULL
    /* malloc the space */
    tmp = malloc(len);
    if (NULL == tmp) {
        return PMIX_ERR_NOMEM;
    }

    if (!pmix_compress.decompress_string(&tmp, (uint8_t*)ptr, len)) {
        free(tmp);
        return PMIX_ERR_TAKE_NEXT_OPTION;
    }

    /* tmp now contains the semicolon-delimited list of procs */
    argv = pmix_argv_split(tmp, ';');
    free(tmp);
    *procs = argv;
    return PMIX_SUCCESS;
}

static pmix_status_t copy(char **dest, size_t *len, const char *input)
{
    size_t slen;
    char *tmp;

    if (0 != strncmp(input, "blob", 4)) {
        return PMIX_ERR_TAKE_NEXT_OPTION;
    }
    /* extract the size */
    slen = strtoul(&input[6], NULL, 10) + 6 + strlen(&input[6]);

    /* malloc the space */
    tmp = malloc(slen);
    if (NULL == tmp) {
        return PMIX_ERR_NOMEM;
    }

    /* copy the data */
    memcpy(tmp, input, slen);
    *dest = tmp;
    *len = slen;
    return PMIX_SUCCESS;
}

static pmix_status_t pack(pmix_buffer_t *buffer, const char *input)
{
    size_t slen;
    char *ptr;

    if (0 != strncmp(input, "blob", 4)) {
        return PMIX_ERR_TAKE_NEXT_OPTION;
    }

    /* extract the size */
    slen = strtoul(&input[6], NULL, 10) + 6 + strlen(&input[6]);

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
    size_t slen;
    char *ptr, *output;

    /* the value starts at the unpack_ptr */
    ptr = buffer->unpack_ptr;

    if (0 != strncmp(ptr, "blob", 4)) {
        return PMIX_ERR_TAKE_NEXT_OPTION;
    }

    /* extract the size */
    slen = strtoul(&ptr[6], NULL, 10) + 6 + strlen(&ptr[6]);

    /* get the space */
    output = (char*)malloc(slen);
    if (NULL == output) {
        *regex = NULL;
        return PMIX_ERR_NOMEM;
    }

    /* xfer the data */
    memcpy(output, ptr, slen);
    buffer->unpack_ptr += slen;
    *regex = output;

    return PMIX_SUCCESS;
}
