/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
 *
 * Copyright (c) 2014 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2018      Amazon.com, Inc. or its affiliates.  All Rights reserved.
 * Copyright (c) 2019-2020 Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "pmix_config.h"

#include <string.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/stat.h>
#if HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#include <zlib.h>

#include "src/util/pmix_environ.h"
#include "src/util/output.h"
#include "src/util/argv.h"
#include "src/util/pmix_environ.h"
#include "src/util/printf.h"

#include "include/pmix_common.h"
#include "src/util/basename.h"

#include "src/mca/pcompress/base/base.h"

#include "compress_zlib.h"

int pmix_compress_zlib_module_init(void)
{
    return PMIX_SUCCESS;
}

int pmix_compress_zlib_module_finalize(void)
{
    return PMIX_SUCCESS;
}

bool pmix_compress_zlib_compress_block(char *instring,
                                       uint8_t **outbytes,
                                       size_t *nbytes)
{
    z_stream strm;
    size_t len, outlen;
    uint8_t *tmp, *ptr;
    uint32_t inlen;
    int rc;

    /* set default output */
    *outbytes = NULL;

    /* setup the stream */
    inlen = strlen(instring);
    memset (&strm, 0, sizeof (strm));
    deflateInit (&strm, 9);

    /* get an upper bound on the required output storage */
    len = deflateBound(&strm, inlen);
    /* if this isn't going to result in a smaller footprint,
     * then don't do it */
    if (len >= inlen) {
        (void)deflateEnd(&strm);
        return false;
    }

    if (NULL == (tmp = (uint8_t*)malloc(len))) {
        (void)deflateEnd(&strm);
        return false;
    }
    strm.next_in = (uint8_t*)instring;
    strm.avail_in = strlen(instring);

    /* allocating the upper bound guarantees zlib will
     * always successfully compress into the available space */
    strm.avail_out = len;
    strm.next_out = tmp;

    rc = deflate (&strm, Z_FINISH);
    (void)deflateEnd (&strm);
    if (Z_OK != rc && Z_STREAM_END != rc) {
        free(tmp);
        return false;
    }

    /* allocate 4 bytes beyond the size reqd by zlib so we
     * can pass the size of the uncompressed string to the
     * decompress side */
    outlen = len - strm.avail_out + sizeof(uint32_t);
    ptr = (uint8_t*)malloc(outlen);
    if (NULL == ptr) {
        free(tmp);
        return false;
    }
    *outbytes = ptr;
    *nbytes = outlen;

    /* fold the uncompressed length into the buffer */
    memcpy(ptr, &inlen, sizeof(uint32_t));
    ptr += sizeof(uint32_t);
    /* bring over the compressed data */
    memcpy(ptr, tmp, outlen-sizeof(uint32_t));
    free(tmp);
    pmix_output_verbose(2, pmix_pcompress_base_framework.framework_output,
                        "COMPRESS INPUT STRING OF LEN %d OUTPUT SIZE %lu",
                        inlen, outlen-sizeof(uint32_t));
    return true;  // we did the compression
}

bool pmix_compress_zlib_uncompress_block(char **outstring,
                                         uint8_t *inbytes, size_t len)
{
    uint8_t *dest;
    int32_t len2;
    z_stream strm;
    int rc;

    /* set the default error answer */
    *outstring = NULL;

    /* the first 4 bytes contains the uncompressed size */
    memcpy(&len2, inbytes, sizeof(uint32_t));

    pmix_output_verbose(2, pmix_pcompress_base_framework.framework_output,
                        "DECOMPRESSING INPUT OF LEN %lu OUTPUT %d", len, len2);

    /* setting destination to the fully decompressed size, +1 to
     * hold the NULL terminator */
    dest = (uint8_t*)malloc(len2+1);
    if (NULL == dest) {
        return false;
    }
    memset(dest, 0, len2+1);

    memset (&strm, 0, sizeof (strm));
    if (Z_OK != inflateInit(&strm)) {
        free(dest);
        return false;
    }
    strm.avail_in = len;
    strm.next_in = (uint8_t*)(inbytes + sizeof(uint32_t));
    strm.avail_out = len2;
    strm.next_out = (uint8_t*)dest;

    rc = inflate (&strm, Z_FINISH);
    inflateEnd (&strm);
    /* ensure this is NULL terminated! */
    dest[len2] = '\0';
    *outstring = (char*)dest;
    pmix_output_verbose(2, pmix_pcompress_base_framework.framework_output,
                        "\tFINAL LEN: %lu CODE: %d", strlen(*outstring), rc);
    return true;
}
