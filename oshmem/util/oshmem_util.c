/*
 * Copyright (c) 2014      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "oshmem_config.h"
#include <stdarg.h>
#include <stdio.h>

#include "opal/util/output.h"

#include "oshmem/constants.h"
#include "oshmem/util/oshmem_util.h"

void oshmem_output_verbose(int level, int output_id, const char* prefix,
    const char* file, int line, const char* function, const char* format, ...)
{
    va_list args;
    char *buff, *str;
    int ret;

    if (level < opal_output_get_verbosity(output_id)) {
        UNREFERENCED_PARAMETER(ret);

        va_start(args, format);

        ret = vasprintf(&str, format, args);
        assert(-1 != ret);

        ret = asprintf(&buff, "%s %s", prefix, str);
        assert(-1 != ret);

        opal_output(output_id, buff, file, line, function);

        va_end(args);

        free(buff);
        free(str);
    }
}

void oshmem_output(int output_id, const char* prefix, const char* file,
    int line, const char* function, const char* format, ...)
{
    va_list args;
    char *buff, *str;
    int ret = 0;

    UNREFERENCED_PARAMETER(ret);

    va_start(args, format);

    ret = vasprintf(&str, format, args);
    assert(-1 != ret);

    ret = asprintf(&buff, "%s %s", prefix, str);
    assert(-1 != ret);

    opal_output(output_id, buff, file, line, function);

    va_end(args);

    free(buff);
    free(str);
}
