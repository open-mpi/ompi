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
                                                                                 
#include "misc_sys.h"
#include "init_sys.h"

struct mca_ptl_elan_state_t {

    /* User configurable parameters */
    int          initialized;
    char        *elan_version;   /**< Version of the elan library */
    uint64_t     elan_debug;     /**< elan debug tracing output */
    uint64_t     elan_traced;    /**< elan TRACE output */
    uint64_t     elan_flags;
    FILE        *elan_debugfile; /* Debug output file handle      */
    int          elan_signalnum;

    size_t       main_size;   /**< size of Main memory allocator heap */
    size_t       elan_size;   /**< size of Elan memory allocator heap */
    void        *main_base;   /**< Main memory allocator heap base */
    void        *elan_base;   /**< Elan memory allocator heap base */

    /* other state parameters */

    unsigned int elan_vp;          /**< elan vpid, not ompi vpid */
    unsigned int elan_nvp;         /**< total # of elan vpid */
    int         *elan_localvps;    /**< mapping of localId to elan vp */
    int          elan_localid;   /**< # of local elan vpids */
    int          elan_numlocals;   /**< # of local elan vpids */
    int          elan_maxlocals;   /**< maximum # of local elan vpids */
    int          elan_nrails;      /**< # of rails */
    int          elan_rmsid;       /**< rms resource id */
    int          intcookie;       
    long         elan_pagesize;
    pid_t        elan_pid;

    /* TODO:
     *   Even though the elan threads are not utilized for now. 
     *   We provide memory/state control structures for later extensions.
     *   A simple type casting of ELAN_ESTATE can bring
     *   the complete structure of the ELAN_EPRIVSATE.
     */
    ELAN_LOCATION    elan_myloc;
    void            *elan_cap;    /**< job capability */
    ELAN_CTX        *elan_ctx;    /**< Elan ctx of the 0th rail */
    void            *elan_estate; /**< Elan state of the 0th rail */
    ELAN_RAIL      **elan_rail;   /**< pointers to Rail control struct for all rails */
    RAIL           **all_rails;   /**< all rails */
    ADDR_SDRAM      *all_estates;
    mca_ptl_elan_module_1_0_0_t *elan_module;
};
typedef struct mca_ptl_elan_state_t mca_ptl_elan_state_t;

/* Initialization and finalization routines */
int ompi_mca_ptl_elan_init( mca_ptl_elan_module_1_0_0_t * emp);
int ompi_mca_ptl_elan_finalize (mca_ptl_elan_module_1_0_0_t * emp);

/* communication prototypes */

/* control, synchronization and state prototypes */

/* Just to get rid of a warning from elan4 libraies,
 * Many more needed but who cares. */
int elan4_block_inputter (ELAN4_CTX *ctx, unsigned blocked);

#endif
