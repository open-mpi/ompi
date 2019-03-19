/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
 *
 * Copyright (c) 2014 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2018      Amazon.com, Inc. or its affiliates.  All Rights reserved.
 * Copyright (c) 2019      Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include <string.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/stat.h>
#if HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#include <zlib.h>

#include "opal/util/opal_environ.h"
#include "opal/util/output.h"
#include "opal/util/argv.h"
#include "opal/util/opal_environ.h"
#include "opal/util/printf.h"

#include "opal/constants.h"
#include "opal/util/basename.h"

#include "opal/mca/compress/compress.h"
#include "opal/mca/compress/base/base.h"

#include "compress_zlib.h"

int opal_compress_zlib_module_init(void)
{
    return OPAL_SUCCESS;
}

int opal_compress_zlib_module_finalize(void)
{
    return OPAL_SUCCESS;
}

bool opal_compress_zlib_compress_block(uint8_t *inbytes,
                                       size_t inlen,
                                       uint8_t **outbytes,
                                       size_t *olen)
{
    z_stream strm;
    size_t len;
    uint8_t *tmp;

    if (inlen < opal_compress_base.compress_limit) {
        return false;
    }
    opal_output_verbose(2, opal_compress_base_framework.framework_output,
                        "COMPRESSING");

    /* set default output */
    *outbytes = NULL;
    *olen = 0;

    /* setup the stream */
    memset (&strm, 0, sizeof (strm));
    deflateInit (&strm, 9);

    /* get an upper bound on the required output storage */
    len = deflateBound(&strm, inlen);
    if (NULL == (tmp = (uint8_t*)malloc(len))) {
        return false;
    }
    strm.next_in = inbytes;
    strm.avail_in = inlen;

    /* allocating the upper bound guarantees zlib will
     * always successfully compress into the available space */
    strm.avail_out = len;
    strm.next_out = tmp;

    deflate (&strm, Z_FINISH);
    deflateEnd (&strm);

    *outbytes = tmp;
    *olen = len - strm.avail_out;
    opal_output_verbose(2, opal_compress_base_framework.framework_output,
                        "\tINSIZE %d OUTSIZE %d", (int)inlen, (int)*olen);
    return true;  // we did the compression
}

bool opal_compress_zlib_uncompress_block(uint8_t **outbytes, size_t olen,
                                         uint8_t *inbytes, size_t len)
{
    uint8_t *dest;
    z_stream strm;

    /* set the default error answer */
    *outbytes = NULL;
    opal_output_verbose(2, opal_compress_base_framework.framework_output, "DECOMPRESS");

    /* setting destination to the fully decompressed size */
    dest = (uint8_t*)malloc(olen);
    if (NULL == dest) {
        return false;
    }

    memset (&strm, 0, sizeof (strm));
    if (Z_OK != inflateInit(&strm)) {
        free(dest);
        return false;
    }
    strm.avail_in = len;
    strm.next_in = inbytes;
    strm.avail_out = olen;
    strm.next_out = dest;

    if (Z_STREAM_END != inflate (&strm, Z_FINISH)) {
        opal_output(0, "\tDECOMPRESS FAILED: %s", strm.msg);
    }
    inflateEnd (&strm);
    *outbytes = dest;
    opal_output_verbose(2, opal_compress_base_framework.framework_output,
                        "\tINSIZE: %d OUTSIZE %d", (int)len, (int)olen);
    return true;
}
