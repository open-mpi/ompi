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

#include "mca/gpr/base/base.h"



void mca_gpr_base_release_notify_msg(ompi_registry_notify_message_t *msg)
{
    ompi_registry_value_t *ptr;
    
    if (NULL != msg->segment) {
        free(msg->segment);
    }
    
    if (0 < ompi_list_get_size(&msg->data)) {
       while (NULL != (ptr = (ompi_registry_value_t*)ompi_list_remove_first(&msg->data))) {
            OBJ_RELEASE(ptr);
       }
    }
    
    free(msg);
}
