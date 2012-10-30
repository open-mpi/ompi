/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "ompi_config.h"
#include <stdio.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#include "ompi/mca/bml/base/base.h"
#include "ompi/mca/btl/base/base.h"
#include "ompi/mca/bml/base/static-components.h"
#include "opal/mca/base/base.h"

int mca_bml_base_already_opened = 0;
opal_list_t mca_bml_base_components_available = {{0}};

#if OPAL_ENABLE_DEBUG_RELIABILITY
double mca_bml_base_error_rate_floor;
double mca_bml_base_error_rate_ceiling;
int    mca_bml_base_error_count;
#endif

int mca_bml_base_open(void) 
{
    /* See if we've already been here */
    if (++mca_bml_base_already_opened > 1) {
        return OMPI_SUCCESS;
    }

    if(OMPI_SUCCESS !=
       mca_base_components_open("bml", 0, mca_bml_base_static_components, 
                                &mca_bml_base_components_available, 
                                true)) {  
        return OMPI_ERROR; 
    }

#if OPAL_ENABLE_DEBUG_RELIABILITY
    do {
        int param, value;

        (void) mca_base_param_reg_int_name ("bml", "error_rate_floor", NULL, false, false,
                                            0, &value);
        mca_bml_base_error_rate_floor = (double) value;

        (void) mca_base_param_reg_int_name ("bml", "error_rate_ceiling", NULL, false, false,
                                            0, &value);
        mca_bml_base_error_rate_ceiling = (double) value;

        (void) mca_base_param_reg_int_name ("bml", "srand", NULL, false, false,
                                            1, &value);

        /* seed random number generator */
        if(value) {
            struct timeval tv;
            gettimeofday(&tv, NULL);
            srand(getpid() * tv.tv_usec);
        }

        /* initialize count */
        if(mca_bml_base_error_rate_ceiling > 0 
           && mca_bml_base_error_rate_floor <= mca_bml_base_error_rate_ceiling) {
            mca_bml_base_error_count = (int) ((mca_bml_base_error_rate_ceiling * rand())/(RAND_MAX+1.0));
        }
    } while (0);
#endif
    return mca_btl_base_open(); 
}

