
#include <signal.h>
#include <unistd.h>
#include <stdio.h>

#define _ELAN4 
#define __elan4__

#include "ptl_elan.h"
#include "ptl_elan_priv.h"

mca_ptl_elan_state_t mca_ptl_elan_global_state;

static int  ompi_init_elan_qdma (mca_ptl_elan_state_t * ems);
static int  ompi_init_elan_sten (mca_ptl_elan_state_t * ems);
static int  ompi_init_elan_rdma (mca_ptl_elan_state_t * ems);
static int  ompi_init_elan_stat (mca_ptl_elan_state_t * ems);
static int  ompi_elan_attach_network (mca_ptl_elan_state_t * ems);

int
ompi_mca_ptl_elan_init ( mca_ptl_elan_module_1_0_0_t * emp)
{
    int         i;
    int        *rails;
    int         num_rails;

    int         alloc_mainsize;
    int         alloc_mainbase;
    int         alloc_elansize;
    int         alloc_elanbase;

    mca_ptl_elan_state_t *ems;

    ems = &mca_ptl_elan_global_state;
    ems->elan_module = emp;

    /* Initialise enough of state so we can call elan_exception() */
    ems->elan_version = ELAN_VERSION;
    ems->elan_ctx = NULL;
    ems->elan_rail = NULL;
    ems->elan_vp = ELAN_INVALID_PROCESS;
    ems->elan_nvp = 0;
    ems->elan_debug = 0;
    ems->elan_traced = 0;
    ems->elan_pagesize = sysconf (_SC_PAGESIZE);
    ems->elan_pid = getpid ();

    /* Default allocator parameters */
    ems->elan_flags = 0;
    ems->main_size = ELAN_ALLOC_SIZE;
    ems->elan_size = ELAN_ALLOCELAN_SIZE;
    ems->elan_flags |= 
	(EXCEPTIONCORE | EXCEPTIONTRACE | EXCEPTIONDBGDUMP);
    ems->elan_debugfile = (FILE*)NULL;
    ems->elan_signalnum = SIGABRT;

#ifdef ELAN_VERSION
    if (!elan_checkVersion (ELAN_VERSION)) {
        return OMPI_ERROR;
    }
#endif

    /* Allocate elan capability from the heap */
    ems->elan_cap =
        (ELAN_CAPABILITY *) malloc (sizeof (ELAN_CAPABILITY));

    if (NULL == ems->elan_cap) {
        return OMPI_ERROR;
    } else {
        memset (ems->elan_cap, 0, sizeof (ELAN_CAPABILITY));
    }

    /* Process the capability info supplied by RMS */
    if (getenv ("ELAN_AUTO") || getenv ("RMS_NPROCS")) {
        /* RMS generated capabilities */
        if (rms_getcap (0, ems->elan_cap)) {
            return OMPI_ERROR;
        }
    }

    if ((num_rails = ems->elan_nrails =
         elan_nrails (ems->elan_cap)) <= 0) {
        return OMPI_ERROR;
    }

    /* MULTI-RAIL: 
     * Allocate storage space for each Elan SDRAM state (ptrs) */

    ems->all_rails = (RAIL **) 
	malloc (sizeof (RAIL *) * num_rails);
    if (ems->all_rails == NULL) {
	/* FIXME: exception code */
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* MULTI-RAIL:
     * initialise each elan rail, using the physical rail info gleaned from 
     * the capability */
    if (NULL == (rails = (int *) malloc (sizeof (int) * num_rails))) {
        /* FIXME: exception code */
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    (void) elan_rails (ems->elan_cap, rails);

    if (NULL == (ems->elan_rail = (ELAN_RAIL **)
                 malloc (sizeof (ELAN_RAIL **) * (num_rails + 1)))) {
        /* FIXME: exception code */
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    ems->elan_rail[num_rails] = NULL;

    alloc_mainsize = ELAN_ALIGNUP (ems->main_size, ems->elan_pagesize);
    alloc_mainbase = (ADDR_ELAN) ((uintptr_t) ems->main_base);
    alloc_elansize = ELAN_ALIGNUP (ems->elan_size, ems->elan_pagesize);
    alloc_elanbase = (ADDR_ELAN) ((uintptr_t) ems->elan_base);

#if 0
    for (i = 0; i < num_rails; i++) {

        RAIL *rail;
        ELAN_ESTATE *estate;
        ELAN_EPRIVSTATE *priv_estate;

        /* Allocate the Main memory control structure for this rail */
        if (NULL == (rail = ems->all_rails[i] = 
		    (RAIL *) 
		    malloc (sizeof (RAIL)))) {
	    /* FIXME: exception code */
            return OMPI_ERROR;
        }

        memset (rail, 0, sizeof (RAIL));

        if (NULL == (rail->r_ctx = elan4_init (rails[i]))) {
	    /* FIXME: exception code */
            return OMPI_ERROR;
        }

        if (NULL == (rail->r_sdram = elan4_open_sdram (rails[i],
                                                       0,
                                                       alloc_elansize))) {
	    /* FIXME: exception code */
            return OMPI_ERROR;
        }

        if (NULL == (rail->r_alloc =
                     elan4_createAllocator (ems->main_size,
                                            rail->r_sdram, 0,
                                            ems->elan_size))) {
	    /* FIXME: exception code */
            return OMPI_ERROR;
        }

        if (elan4_set_standard_mappings (rail->r_ctx) < 0
            || elan4_set_required_mappings (rail->r_ctx) < 0) {
	    /* FIXME: exception code */
            return OMPI_ERROR;
        }

        /* Now allocate the SDRAM Elan control structure for this rail */
        if (NULL == (estate = ems->all_estates[i] =
                     elan4_allocElan(rail->r_alloc, ELAN_ALIGN,
                                 sizeof (ELAN_EPRIVSTATE)))) {
	    /* FIXME: exception code */
            return OMPI_ERROR;
        }

        priv_estate = (ELAN_EPRIVSTATE *) estate;
        memset (priv_estate, 0, sizeof (ELAN_EPRIVSTATE));

        /* Allocate a command port for non sten functions etc */
        if (NULL == (rail->r_cmdq = elan4_alloc_cmdq (rail->r_ctx,
                                              rail->r_alloc, CQ_Size8K,
                                              CQ_ModifyEnableBit |
                                              CQ_WriteEnableBit |
                                              CQ_WaitEventEnableBit |
                                              CQ_SetEventEnableBit |
                                              CQ_ThreadStartEnableBit,
                                              NULL))) {
	    /* FIXME: exception code */
            return OMPI_ERROR;
        }

        /* Allocate a command port for thread rescheduling etc */
        if (NULL == (rail->r_ecmdq = elan4_alloc_cmdq (rail->r_ctx,
                                               rail->r_alloc, CQ_Size8K,
                                               CQ_EnableAllBits,
                                               NULL))) {
	    /* FIXME: exception code */
            return OMPI_ERROR;
        }

        /* save the rail pointers */
        ems->elan_rail[i] = (ELAN_RAIL *) rail;
        rail->r_estate = estate;
        rail->r_railNo = rails[i];

	estate->alloc = rail->r_alloc;
	estate->vp    = ems->elan_vp;
	estate->debugFlags = ems->elan_flags;
	estate->debugFd = 1;
	priv_estate->pageSize = ems->elan_pagesize;

#if 0
        {
	    ompi_elan_railtable_t *rt;

            if (NULL == (rt = (ompi_elan_railtable_t *) malloc (
			    sizeof (ompi_elan_railtable_t)))) {
		/* FIXME: exception code */
                return OMPI_ERROR;
            }
            memset (rt, 0, sizeof (ompi_elan_railtable_t));

            rt->rt_nrails = 1;
            rt->rt_rail = 0;
            rt->rt_railReal = i;
            rt->rt_allRails = (struct RAIL *) 
		&(ems->all_rails[i]);

            rail->r_railTable = rt;
        }
#endif
    }                           /* for each rail */

    free (rails);

    ems->elan_ctx = ems->elan_rail[0]->r_ctx;
    ems->elan_estate = (void *) ems->all_estates[0];

    ompi_elan_attach_network (ems);

    /* Set the rms_resourceId */
    if (rms_getprgid (getpid (), &ems->elan_rmsid) < 0) {
        ems->elan_rmsid = -1;
    }

    /* Now open ourselves to the network */
    for (i = 0; ems->elan_rail[i]; i++) {
        elan4_block_inputter (ems->elan_rail[i]->r_ctx, 0);
    }
#endif

    return (OMPI_SUCCESS);
}

int
ompi_mca_ptl_elan_setup (mca_ptl_elan_state_t * ems)
{
    /* TODO: 
     * a) init transport structure for all rails 
     *    including STEN, QDMA and RDMA 
     * b) For each possible transport mechanism, allocate:
     *    send/recv descriptors; 
     *    system buffer;
     *    event structure for transport control
     * c) initialize STAT (including SYNC) structures.
     */
    if (OMPI_SUCCESS != ompi_init_elan_qdma (ems)) {
        return OMPI_ERROR;
    }

    if (OMPI_SUCCESS != ompi_init_elan_rdma (ems)) {
        return OMPI_ERROR;
    }

    if (OMPI_SUCCESS != ompi_init_elan_sten (ems)) {
        return OMPI_ERROR;
    }

    if (OMPI_SUCCESS != ompi_init_elan_stat (ems)) {
        return OMPI_ERROR;
    }

    return (OMPI_SUCCESS);
}

int
ompi_mca_ptl_elan_fin (mca_ptl_elan_state_t * ems)
{
    return (OMPI_SUCCESS);
}

/* Attach to the network:
 * a) First add myself into the capability
 * b) For each rail, 
 *    Block the inputter for any incoming traffic
 *    attach to the device
 *    Find out the location
 *    Fill out the vp in main and elan structures.
 *    create a cookiePool for threading control
 * c) Allocate a cookiePool for export Oth Rail
 * d) Find out total vpids, localVpids, localId and number of locals
 * e) Allocate a bcastVp (not used for now) and a railTable
 */
static int
ompi_elan_attach_network (mca_ptl_elan_state_t * ems)
{
    int         i, vp, *vps, num_rails;
    ELAN_LOCATION loc;
    ELAN_CAPABILITY *cap = ems->elan_cap;

    num_rails = ems->elan_nrails;

#if 1
    if (ems->elan_attached) {
        /* already successfully attached */
        return OMPI_SUCCESS;
    } else {
        ems->elan_attached = 1;
    }

    for (i = 0; i < num_rails; i++) {
        RAIL       *rail = ems->all_rails[i];
        ELAN_LOCATION loc;

        /* Add all virtual process from 0 to (nvp-1) */
        if (elan4_add_p2pvp (rail->r_ctx, 0, cap) < 0) {
            /* FIXME: exception code */
            return OMPI_ERROR;
        }

        /* block the inputter until we are really ready */
        elan4_block_inputter (rail->r_ctx, 1);

        if (elan4_attach (rail->r_ctx, cap)) {
            /* FIXME: exception code */
            return OMPI_ERROR;
        }

        /* NB: We should have same vp for all capabilities */
        loc.loc_node = ((ELAN4_CTX *) rail->r_ctx)->ctx_position.pos_nodeid
            - cap->cap_lownode;
        loc.loc_context = cap->cap_mycontext - cap->cap_lowcontext;

        /* TODO: debug code for loc */
        if (cap->cap_type == ELAN_CAP_TYPE_HWTEST)
            ems->elan_vp = 0;
        else
            ems->elan_vp = elan_location2vp (loc, ems->elan_cap);

        /* Initialise the Elan version of this data */
        ((ELAN_ESTATE *) (rail->r_estate))->vp = ems->elan_vp;

        /* Allocate a cookie pool for the thread processor 
         * and copy to sdram */
        rail->r_cpool =
            elan4_allocCookiePool (rail->r_ctx, ems->elan_vp);
        ((ELAN_EPRIVSTATE *) rail->r_estate)->tcookie =
            rail->r_cpool->cp_cookie;
    }

#if 0
    /* Allocate a cookie pool for the exported API (0th rail) */
    ems->cpool = elan4_allocCookiePool (ems->elan_ctx, ems->elan_vp);

    loc = elan_vp2location (ems->elan_vp, ems->elan_cap);

    /* update THREAD elan_dbg info of debugfile */
    if (ems->elan_debugfile && fileno (ems->elan_debugfile) != -1) {
        for (i = 0; i < num_rails; i++) {
            ELAN4_CTX *ctx = ems->elan_rail[i]->rail_ctx;

            /* Convert FILE * stream to fd for THRD output */
            ((ELAN_ESTATE *) ems->all_estates[i])->debugFd =
                fileno (ems->elan_debugfile);

            /* Also re-direct libelan4 output to this file */
            elan4_set_debugfd (ctx, fileno (ems->elan_debugfile));
        }
    }
#endif

    /* Determine the number of processes described by the capability */
    ems->elan_nvp = elan_nvps (ems->elan_cap);

    if (ems->elan_vp >= ems->elan_nvp) {
       	/* revert the initiation that has done so far */
	/* FIXME: exception code */
	return OMPI_ERROR;
    }

    /* XXX: We do not need to keep track of the elan state other than
     * the NIC memory resource */

    /* Set all to non local initially */
    for (vp = 0; vp < ems->elan_nvp; vp++)
        ems->elan_localvps[vp] = -1;

    /* Stash our own process location */
    ems->elan_myloc = elan_vp2location (ems->elan_vp, ems->elan_cap);
    ems->elan_maxlocals = elan_maxlocal (ems->elan_cap);
    ems->elan_numlocals = elan_nlocal (ems->elan_myloc.loc_node,
                                         ems->elan_cap);

    /* Allocate more than we need to keep the heap in sync */
    if (NULL == (vps = (int *) malloc (sizeof (int) * ems->elan_nvp))) {
        /* FIXME: exception code */
        return OMPI_ERROR;
    }

    /* Fill out the local vp array */
    elan_localvps (ems->elan_myloc.loc_node,
                   ems->elan_cap, vps, ems->elan_numlocals);

    for (i = 0; i < ems->elan_numlocals; i++) {
        int         localvp = vps[i];

        /* This is a local process */
        ems->elan_localvps[localvp] = i;

        if (localvp == ems->elan_vp) {
            ems->elan_localid = i;
        }
    }

    /* TODO: Debug code here for elan_state */

#if 0
    /*
     * We need to create one of these maps in each rail estate ctx too
     */
    for (i = 0; i < num_rails; i++) {
        RAIL       *rail = ems->all_rails[i];
        int        *localIdsElan;

        /* Allocate a copy of this in the SDRAM of each rail */
        if (0 == (localIdsElan = elan4_allocElan(rail->r_alloc, ELAN_ALIGN,
                                             ems->elan_nvp *
                                             sizeof (int)))) {
            /* FIXME: exception code */
            return OMPI_ERROR;
        }

        for (vp = 0; vp < ems->elan_nvp; vp++) {
            localIdsElan[vp] = ems->elan_localvps[vp];
        }
    }
#endif

    /* Done with vps array now */
    free (vps);
#endif

    return (OMPI_SUCCESS);
}

static int
ompi_init_elan_qdma (mca_ptl_elan_state_t * mp)
{
    return (OMPI_SUCCESS);
}

static int
ompi_init_elan_rdma (mca_ptl_elan_state_t * mp)
{
    return (OMPI_SUCCESS);
}

static int
ompi_init_elan_sten (mca_ptl_elan_state_t * mp)
{
    return (OMPI_SUCCESS);
}

static int
ompi_init_elan_stat (mca_ptl_elan_state_t * mp)
{
    return (OMPI_SUCCESS);
}
