/*
 * $HEADER$
 */
#include "lam_config.h"

#include <stdio.h>

#include "constants.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/coll/coll.h"
#include "mca/coll/base/base.h"

int mca_topo_base_close(void) {
    extern lam_list_t mca_topo_base_modules_available;

    /*
     * Close all the available modules
     */
    mca_base_modules_close (mca_topo_base_output,
                            &mca_topo_base_modules_available, NULL);

    /*
     * All done
     */
    return LAM_SUCCESS;
}
