/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004 The Ohio State University.
 *                    All rights reserved.
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
#include "include/types.h"
#include "bmi_ib_sendfrag.h"
#include "bmi_ib_frag.h" 
#include "mca/bmi/bmi.h" 
#include "bmi_ib_endpoint.h" 

void mca_bmi_ib_sendfrag_complete( mca_bmi_ib_module_t * ib_bmi, mca_bmi_ib_send_frag_t* frag)
{ 
    frag->base.des_cbfunc(&ib_bmi->super,(mca_bmi_base_endpoint_t*) &frag->endpoint, &frag->base, frag->rc); 
        
} 
