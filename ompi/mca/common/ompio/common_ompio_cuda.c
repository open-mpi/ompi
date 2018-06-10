/*
 *  Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                          University Research and Technology
 *                          Corporation.  All rights reserved.
 *  Copyright (c) 2004-2016 The University of Tennessee and The University
 *                          of Tennessee Research Foundation.  All rights
 *                          reserved.
 *  Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                          University of Stuttgart.  All rights reserved.
 *  Copyright (c) 2004-2005 The Regents of the University of California.
 *                          All rights reserved.
 *  Copyright (c) 2008-2018 University of Houston. All rights reserved.
 *  $COPYRIGHT$
 *
 *  Additional copyrights may follow
 *
 *  $HEADER$
 */

#include "ompi_config.h"

#include "opal/datatype/opal_convertor.h"
#include "opal/datatype/opal_datatype_cuda.h"
#include "opal/mca/common/cuda/common_cuda.h"

#include "ompi/mca/io/ompio/io_ompio.h"
#include "common_ompio.h"
#include "common_ompio_cuda.h"

void mca_common_ompio_check_gpu_buf ( ompio_file_t *fh, const void *buf, int *is_gpu, 
				      int *is_managed)
{
    opal_convertor_t    convertor;  
    
    *is_gpu=0;
    *is_managed=0;
    
    convertor.flags=0;
    if ( opal_cuda_check_one_buf ( (char *)buf, &convertor ) ) {
        *is_gpu = 1;
        if ( convertor.flags & CONVERTOR_CUDA_UNIFIED ){
            *is_managed =1;
        }
    } 
    
    return;
}


void mca_common_ompio_register_buf ( ompio_file_t *fh, const void *buf, 
                                    size_t bufsize )
{
    mca_common_cuda_register ( ( char *)buf, bufsize,  (char *) fh->f_filename );
    return;
}

void mca_common_ompio_unregister_buf ( ompio_file_t *fh, void *buf )
{
    if ( NULL != fh ) {
        mca_common_cuda_unregister ( (char *)buf, (char *)fh->f_filename);
    }
    else {
        char dummy_filename[]="dummy_ompio_filename";
        mca_common_cuda_unregister ( (char *)buf, (char *)dummy_filename);
    }
    free (buf);
    return;
}

