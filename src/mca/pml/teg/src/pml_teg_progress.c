/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "pml_teg.h"
#include "pml_teg_sendreq.h"


int mca_pml_teg_progress(void)
{
    mca_ptl_tstamp_t tstamp = 0;
    size_t i;
    int count;

    /*
     * Progress each of the PTL modules
     */
    for(i=0; i<mca_pml_teg.teg_num_ptl_components; i++) {
        mca_ptl_base_component_progress_fn_t progress = mca_pml_teg.teg_ptl_components[i]->ptlm_progress;
        if(NULL != progress) {
            int rc = progress(tstamp);
            if(rc < 0)
                return rc;
            count += rc;
        }
    }
    return OMPI_SUCCESS;
}

