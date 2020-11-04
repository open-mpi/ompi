/*
 * Copyright (c) 2015-2020 Intel, Inc.  All rights reserved.
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

#include "src/include/pmix_config.h"

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


#include "include/pmix_common.h"
#include "include/pmix.h"

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
    .copy = copy,
    .pack = pack,
    .unpack = unpack
};

#define PREG_COMPRESS_PREFIX "blob: component=zlib: size="

static pmix_status_t pack_blob(const uint8_t *tmp, size_t len, char **regexp)
{
    char *result, *slen;
    int idx;

    /* convert the length to a string */
    if (0 > asprintf(&slen, "%lu", (unsigned long)len)) {
        return PMIX_ERR_NOMEM;
    }

    /* create the result */
    result = calloc(len + strlen(PREG_COMPRESS_PREFIX) + strlen(slen) + strlen(":") + 1, sizeof(char));
    idx = 0;
    strcpy(result, "blob:");
    idx += strlen("blob:") + 1;  // step over NULL terminator
    strcpy(&result[idx], "component=zlib:");
    idx += strlen("component=zlib:") + 1;   // step over NULL terminator
    strcpy(&result[idx], "size=");
    idx += strlen("size=");
    strcpy(&result[idx], slen);
    idx += strlen(slen);
    strcpy(&result[idx], ":");
    idx += strlen(":") + 1;    // step over NULL terminator
    memcpy(&result[idx], tmp, len);
    free(slen);
    *regexp = result;

    return PMIX_SUCCESS;
}

static pmix_status_t generate_node_regex(const char *input,
                                         char **regexp)
{
    size_t len;
    uint8_t *tmp;
    pmix_status_t rc;

    if (!pmix_compress.compress_string((char*)input, &tmp, &len)) {
        return PMIX_ERR_TAKE_NEXT_OPTION;
    }

    if (NULL == tmp) {
        return PMIX_ERR_NOMEM;
    }
    rc = pack_blob(tmp, len, regexp);
    free(tmp);

    return rc;
}

static pmix_status_t generate_ppn(const char *input,
                                  char **regexp)
{
    size_t len;
    uint8_t *tmp;
    pmix_status_t rc;

    if (!pmix_compress.compress_string((char*)input, &tmp, &len)) {
        return PMIX_ERR_TAKE_NEXT_OPTION;
    }

    if (NULL == tmp) {
        return PMIX_ERR_NOMEM;
    }

    if (NULL == tmp) {
        return PMIX_ERR_NOMEM;
    }
    rc = pack_blob(tmp, len, regexp);
    free(tmp);

    return rc;
}

static pmix_status_t parse_nodes(const char *regexp,
                                 char ***names)
{
    char *tmp, *ptr, **argv;
    size_t len;
    int idx;

    if (0 != strncmp(regexp, "blob", 4)) {
        return PMIX_ERR_TAKE_NEXT_OPTION;
    }
    idx = strlen(regexp) + 1;  // step over the NULL terminator

    /* ensure we were the one who generated this blob */
    if (0 != strncmp(&regexp[idx], "component=zlib:", strlen("component=zlib:"))) {
        return PMIX_ERR_TAKE_NEXT_OPTION;
    }
    idx += strlen("component=zlib:") + 1;  // step over the NULL terminator

    len = strtoul(&regexp[idx], &ptr, 10);
    ptr += 2;  // step over colon and NULL

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
    int idx;

    if (0 != strncmp(regexp, "blob", 4)) {
        return PMIX_ERR_TAKE_NEXT_OPTION;
    }
    idx = strlen(regexp) + 1;  // step over the NULL terminator

    /* ensure we were the one who generated this blob */
    if (0 != strncmp(&regexp[idx], "component=zlib:", strlen("component=zlib:"))) {
        return PMIX_ERR_TAKE_NEXT_OPTION;
    }
    idx += strlen("component=zlib:") + 1;  // step over the NULL terminator

    len = strtoul(&regexp[idx], &ptr, 10);
    ptr += 2;  // step over colon and NULL

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
    int idx;

    if (0 != strncmp(input, "blob", 4)) {
        return PMIX_ERR_TAKE_NEXT_OPTION;
    }
    idx = strlen(input) + 1;  // step over the NULL terminator

    /* ensure we were the one who generated this blob */
    if (0 != strncmp(&input[idx], "component=zlib:", strlen("component=zlib:"))) {
        return PMIX_ERR_TAKE_NEXT_OPTION;
    }
    idx += strlen("component=zlib:") + 1;  // step over the NULL terminator

    /* extract the size */
    slen = strtoul(&input[idx], NULL, 10) + strlen(PREG_COMPRESS_PREFIX) + strlen(&input[idx]) + 1;

    /* malloc the space */
    tmp = calloc(slen, sizeof(char));
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
    int idx;

    if (0 != strncmp(input, "blob", 4)) {
        return PMIX_ERR_TAKE_NEXT_OPTION;
    }
    idx = strlen(input) + 1;  // step over the NULL terminator

    /* ensure we were the one who generated this blob */
    if (0 != strncmp(&input[idx], "component=zlib:", strlen("component=zlib:"))) {
        return PMIX_ERR_TAKE_NEXT_OPTION;
    }
    idx += strlen("component=zlib:") + 1;  // step over the NULL terminator

    /* extract the size */
    slen = strtoul(&input[idx], NULL, 10) + strlen(PREG_COMPRESS_PREFIX) + strlen(&input[idx]) + 1;

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
    int idx;

    /* the value starts at the unpack_ptr */
    ptr = buffer->unpack_ptr;

    if (0 != strncmp(ptr, "blob", 4)) {
        return PMIX_ERR_TAKE_NEXT_OPTION;
    }
    idx = strlen(ptr) + 1;  // step over the NULL terminator

    /* ensure we were the one who generated this blob */
    if (0 != strncmp(&ptr[idx], "component=zlib:", strlen("component=zlib:"))) {
        return PMIX_ERR_TAKE_NEXT_OPTION;
    }
    idx += strlen("component=zlib:") + 1;  // step over the NULL terminator

    /* extract the size */
    slen = strtoul(&ptr[idx], NULL, 10) + strlen(PREG_COMPRESS_PREFIX) + strlen(&ptr[idx]) + 1;

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
