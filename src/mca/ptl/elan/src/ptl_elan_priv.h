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

#define __elan4__

#include <elan/elan.h>
#include <elan/init.h>
#include <rms/rmscall.h>
#include <elan4/library.h>
#include <elan4/types.h>
#include <elan4/intcookie.h>
/*#include "./elan.h"*/
/*#include "./misc_sys.h"*/
/*#include "./init_sys.h"*/
#include "elan/sys/misc_sys.h"
#include "elan/sys/init_sys.h"

struct ompi_elan_railtable_t {
    int        rt_nrails;  
    int        rt_rail;   
    int        rt_railReal;
    int       *rt_table;  
    int       *rt_realRail;
    struct ompi_elan_rail_t *rt_allRails;
};
typedef struct ompi_elan_railtable_t ompi_elan_railtable_t;

struct ompi_elan_rail_t {
    struct elan4_ctx   *rail_ctx;
    ELAN4_SDRAM        *rail_sdram;
    E4_CmdQ	       *rail_cmdq;
    E4_CmdQ	       *rail_ecmdq;
    ELAN4_ALLOC        *rail_alloc;
    struct elan_sleep  *rail_sleepDescs;
    ELAN4_COOKIEPOOL   *rail_cpool;
    u_int               rail_index;
    ELAN_TPORT         *rail_tport;
    ompi_elan_railtable_t   *rail_railTable;
    
    void               *rail_estate; /* ADDR_SDRAM  r_estate; */
    int                 rail_railNo;
    E4_Addr             rail_tportrecvSym;
    E4_Addr             rail_atomicSym;
};
typedef struct ompi_elan_rail_t ompi_elan_rail_t;

/* Initialization and finalization routines */
int ompi_mca_ptl_elan_init (mca_ptl_elan_module_1_0_0_t *mp);
int ompi_mca_ptl_elan_setup (mca_ptl_elan_module_1_0_0_t *mp);
int ompi_mca_ptl_elan_fin (mca_ptl_elan_module_1_0_0_t *mp);
/* Accessory functions to deallocate the memory */
void elan_state_close(struct mca_ptl_elan_state_t *);
void elan_ptls_close(struct mca_ptl_elan_t **, size_t elan_num_ptls);
void elan_localproc_close(struct mca_ptl_elan_proc_t *); 

/* communication prototypes */

/* control, synchronization and state prototypes */

/* Just to get rid of a warning from elan4 libraies,
 * Many more needed but who cares. */
int elan4_block_inputter (ELAN4_CTX *ctx, unsigned blocked);


#endif
