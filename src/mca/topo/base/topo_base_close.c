/*
 * $HEADER$
 */
#include "ompi_config.h"

#include <stdio.h>

#include "util/output.h"
#include "include/constants.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/topo/topo.h"
#include "mca/topo/base/base.h"

int mca_topo_base_close(void) {
    /* We have to close all the modules which are open. This might either 
       be the list of opened modules or the list of available modules. 
       Note that the modules which are opened but are not available are 
       already closed */

    if (mca_topo_base_modules_opened_valid) {
        mca_base_modules_close (mca_topo_base_output,
                                &mca_topo_base_modules_opened, NULL);
        mca_topo_base_modules_opened_valid = false;
    } else if (mca_topo_base_modules_available_valid) {
        mca_base_modules_close (mca_topo_base_output,
                                &mca_topo_base_modules_available, NULL);
        mca_topo_base_modules_available_valid = false;
    }

    /* Close the output stream for this framework */
    ompi_output_close (mca_topo_base_output);

    /*
     * All done
     */
    return OMPI_SUCCESS;
}
