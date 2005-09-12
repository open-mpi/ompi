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
#include <stdio.h>
#include "mca/bml/bml.h" 
#include "mca/bml/base/base.h"
#include "mca/btl/base/base.h"
#include "ompi/mca/bml/base/static-components.h"
#include "opal/mca/base/base.h"

opal_list_t mca_bml_base_components_available;


int mca_bml_base_open( void ) { 
    
    if(OMPI_SUCCESS !=
       mca_base_components_open("bml", 0, mca_bml_base_static_components, 
                                &mca_bml_base_components_available, 
                                true)) {  
        return OMPI_ERROR; 
    }
    return mca_btl_base_open(); 
}

