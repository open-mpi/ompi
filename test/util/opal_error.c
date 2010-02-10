/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_NETDB_H
#include <netdb.h>
#endif
#include <errno.h>

#include "opal/util/error.h"
#include "opal/constants.h"
#include "opal/runtime/opal.h"
#include "orte/constants.h"

int
main(int argc, char *argv[])
{
    int i;
    int errors[] = { OPAL_SUCCESS, 
                     OPAL_ERROR, 
                     OPAL_ERR_OUT_OF_RESOURCE,
                     OPAL_ERR_NOT_FOUND, 
                     OPAL_ERR_BAD_PARAM,
                     OPAL_ERR_MAX + 10, /* bad value */
                     1 }; /* sentinal */
    char buf[1024];

    opal_init(&argc, &argv);

    for (i = 0 ; errors[i] <= 0 ; ++i) {
        printf("--> error code: %d\n", errors[i]);
        opal_perror(errors[i], "perror test");
        printf("strerror test: %s\n", opal_strerror(errors[i]));
        opal_strerror_r(errors[i], buf, sizeof(buf));
        printf("strerror_r test: %s\n", buf);
    }

    printf("--> error in errno test\n");
    errno = EINVAL;
    opal_perror(OPAL_ERR_IN_ERRNO, "perror test");
    errno = EINVAL;
    printf("strerror test: %s\n", opal_strerror(OPAL_ERR_IN_ERRNO));
    errno = EINVAL; 
    opal_strerror_r(OPAL_ERR_IN_ERRNO, buf, sizeof(buf));
    printf("strerror_r test: %s\n", buf);

    printf("--> orte error test\n");
    opal_perror(ORTE_ERR_BUFFER, "orte test");

    printf("--> orte unknown error test\n");
    opal_perror(ORTE_ERR_MAX + 10, "orte unknown test");

    printf("--> unknown error test\n");
    opal_perror(ORTE_ERR_MAX - 200, "unknown error");

    opal_finalize();

    return 0;
}
