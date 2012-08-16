/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#include "ompi/include/ompi/constants.h"
#include "ompi/mca/bcol/ptpcoll/bcol_ptpcoll.h"

/*
 * Fanin routines - no user data
 */

int bcol_ptpcoll_fanin( bcol_function_args_t *input_args,
        struct mca_bcol_base_module_t *module)
{
    /* local variable */
    int ret=OMPI_SUCCESS;
    /* mca_bcol_ptpcoll_module_t *ptp_module=(mca_bcol_ptpcoll_module_t *) module; */

    /* done */
    return ret;
}
