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
#include "bmi_ib.h"
#include "bmi_ib_recvfrag.h" 


void mca_bmi_ib_process_recv(mca_bmi_ib_module_t *ib_bmi, mca_bmi_ib_recv_frag_t* frag)
{
   
    ompi_output(0, "%s:%d ib mca_bmi_ib_process_recv got back tag %d", __FILE__, __LINE__, frag->hdr->tag); 
    

}
