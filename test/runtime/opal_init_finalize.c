/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

#include "opal/runtime/opal.h"
#include "opal/include/constants.h"

int
main(int argc, char *argv[])
{
    int ret;

    ret = opal_init();
    if (OPAL_SUCCESS != ret) {
        return (-1 * ret);
    }

    ret = opal_finalize();
    if (OPAL_SUCCESS != ret) {
        return (-1 * ret);
    }

    return 0;
}
