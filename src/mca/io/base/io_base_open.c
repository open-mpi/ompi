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

#include "class/ompi_free_list.h"
#include "util/output.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/base/mca_base_param.h"
#include "mca/io/io.h"
#include "mca/io/base/base.h"
#include "mca/io/base/io_base_request.h"


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */
#ifdef WIN32
    const mca_base_component_t *mca_io_base_static_components[] = {NULL};
#else 
#include "mca/io/base/static-components.h"
#endif

/*
 * Global variables; most of which are loaded by back-ends of MCA
 * variables
 */
int mca_io_base_param = -1;
int mca_io_base_output = -1;

bool mca_io_base_components_opened_valid = false;
ompi_list_t mca_io_base_components_opened;

bool mca_io_base_components_available_valid = false;
ompi_list_t mca_io_base_components_available;


/*
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int mca_io_base_open(void)
{
    /* Open an output stream for this framework */

    mca_io_base_output = ompi_output_open(NULL);

    /* Create some parameters */

    if (0 >
        mca_base_param_register_int("io", "base", "freelist_initial_size",
                                    "", 16) ||
        0 >
        mca_base_param_register_int("io", "base", "freelist_max_size",
                                    "", 64) ||
        0 >
        mca_base_param_register_int("io", "base", "freelist_increment",
                                    "", 16)) {
        return OMPI_ERROR;
    }

    /* Open up all available components */

    if (OMPI_SUCCESS != 
        mca_base_components_open("io", mca_io_base_output,
                                 mca_io_base_static_components, 
                                 &mca_io_base_components_opened)) {
        return OMPI_ERROR;
    }
    mca_io_base_components_opened_valid = true;

    /* Find the index of the MCA "io" param for selection */
    
    mca_io_base_param = mca_base_param_find("io", "base", NULL);

    /* Initialize some io framework resrouces */

    mca_io_base_component_init();

    /* Intialize the request progression code */

    mca_io_base_request_progress_init();

    /* All done */
    
    return OMPI_SUCCESS;
}
