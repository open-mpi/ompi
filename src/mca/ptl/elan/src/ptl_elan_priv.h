/*
 * $HEADER$
 */
#ifndef MCA_PTL_ELAN_PRIV_H
#define MCA_PTL_ELAN_PRIV_H

#include <errno.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include "constants.h"
#include "event/event.h"
#include "util/if.h"
#include "util/argv.h"
#include "util/output.h"
#include "mca/pml/pml.h"
#include "mca/ptl/ptl.h"
#include "mca/ptl/base/ptl_base_sendreq.h"
#include "mca/base/mca_base_param.h"
#include "mca/base/mca_base_module_exchange.h"
#include "ptl_elan.h"
#include "ptl_elan_proc.h"
#include "ptl_elan_frag.h"
#include "ptl_elan_req.h"

/*#include "elan/init.h"*/
/*#include "elan/sys/misc_sys.h"*/
/*#include "elan/sys/init_sys.h"*/
/*#include "elan/elan.h"*/

/* Initialization and finalization routines */
int ompi_mca_ptl_elan_init (mca_ptl_elan_module_1_0_0_t *mp);
int ompi_mca_ptl_elan_setup (mca_ptl_elan_module_1_0_0_t *mp);
int ompi_mca_ptl_elan_fin (mca_ptl_elan_module_1_0_0_t *mp);

/* communication prototypes */

/* control, synchronization and state prototypes */

#endif
