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

#ifndef MCA_BMI_TEMPLATE_ENDPOINT_H
#define MCA_BMI_TEMPLATE_ENDPOINT_H

#include "class/ompi_list.h"
#include "event/event.h"
#include "mca/pml/pml.h"
#include "mca/bmi/bmi.h"
#include "bmi_template_frag.h"
#include "bmi_template.h"
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

OBJ_CLASS_DECLARATION(mca_bmi_template_endpoint_t);

/**
 * An abstraction that represents a connection to a endpoint process.
 * An instance of mca_bmi_base_endpoint_t is associated w/ each process
 * and BMI pair at startup. However, connections to the endpoint
 * are established dynamically on an as-needed basis:
 */

struct mca_bmi_base_endpoint_t {
    ompi_list_item_t            super;

    struct mca_bmi_template_module_t* endpoint_bmi;
    /**< BMI instance that created this connection */

    struct mca_bmi_template_proc_t*   endpoint_proc;
    /**< proc structure corresponding to endpoint */
};

typedef struct mca_bmi_base_endpoint_t mca_bmi_base_endpoint_t;
typedef mca_bmi_base_endpoint_t  mca_bmi_template_endpoint_t;


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
