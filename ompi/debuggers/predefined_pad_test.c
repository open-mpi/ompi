/*
 * Copyright (c) 2014 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/*
 * Simple test to print out how much space is left in the padding for
 * each of the predefined MPI object types.
 *
 * Warn if there is less than THRESHHOLD bytes of padding left; error
 * if there is no space left.
 */

#include "ompi_config.h"

#include <stdlib.h>

#include "opal_stdint.h"

#include "ompi/communicator/communicator.h"
#include "ompi/group/group.h"
#include "ompi/request/request.h"
#include "ompi/op/op.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/win/win.h"
#include "ompi/info/info.h"
#include "ompi/file/file.h"
#include "ompi/message/message.h"

static int warnings = 0;
static int errors = 0;

#define THRESHHOLD 32


#define PAD_CHECK(TYPE)                                                 \
    do {                                                                \
        size_t psize = sizeof(ompi_predefined_##TYPE##_t);              \
        size_t size = sizeof(ompi_##TYPE##_t);                          \
        size_t diff = psize - size;                                     \
        if (diff <= 0) {                                                \
            fprintf(stderr, "ERROR: Predefined " #TYPE " size: %" PRIsize_t ", " #TYPE " size: %" PRIsize_t " (%" PRIsize_t " bytes over)\n", psize, size, size - psize); \
        } else if (diff <= THRESHHOLD) {                                \
            fprintf(stderr, "WARNING: Predefined " #TYPE " has very little space left -- size : %" PRIsize_t ", " #TYPE " size: %" PRIsize_t " (%" PRIsize_t " bytes left)\n", psize, size, psize - size); \
        } else {                                                        \
            printf("Predefined " #TYPE " size : %" PRIsize_t ", " #TYPE " size: %" PRIsize_t " (%" PRIsize_t " bytes left)\n", psize, size, psize - size); \
        }                                                               \
    } while(0)

int main(int argc, char **argv)
{
    PAD_CHECK(communicator);
    PAD_CHECK(errhandler);
    PAD_CHECK(file);
    PAD_CHECK(win);
    PAD_CHECK(request);
    PAD_CHECK(info);
    PAD_CHECK(datatype);
    PAD_CHECK(group);
    PAD_CHECK(message);
    PAD_CHECK(op);

    if (warnings > 0) {
        fprintf(stderr, "NUMBER OF WARNINGS: %d\n", warnings);
    }
    if (errors > 0) {
        fprintf(stderr, "NUMBER OF ERRORS: %d\n", errors);
        exit(1);
    }

    return 0;
}
