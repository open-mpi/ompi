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

#include "pml_uniq.h"
#include "pml_uniq_sendreq.h"


int mca_pml_uniq_progress(void)
{
    mca_ptl_tstamp_t tstamp = 0;
    size_t i;
    int count = 0;

    /*
     * Progress each of the PTL modules
     */
    for(i=0; i<mca_pml_uniq.uniq_num_ptl_progress; i++) {
        int rc = mca_pml_uniq.uniq_ptl_progress[i](tstamp);
        if(rc > 0) {
            count += rc;
        }
    }
    return count;
}

