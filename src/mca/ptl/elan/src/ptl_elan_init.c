
#include <signal.h>
#include <unistd.h>
#include <stdio.h>

#include "ptl_elan.h"
#include "elan/sys/misc_sys.h"
#include "elan/sys/init_sys.h"
#include "elan/elan.h"
#include "elan/init.h"
#include "rms/rmscall.h"
#include "elan4/library.h"
#include "ptl_elan_priv.h"

static int  ompi_init_elan_qdma (mca_ptl_elan_module_1_0_0_t * mp);
static int  ompi_init_elan_sten (mca_ptl_elan_module_1_0_0_t * mp);
static int  ompi_init_elan_rdma (mca_ptl_elan_module_1_0_0_t * mp);
static int  ompi_init_elan_stat (mca_ptl_elan_module_1_0_0_t * mp);
static int  ompi_elan_attach_network (mca_ptl_elan_state_t * state);

int
ompi_mca_ptl_elan_init (mca_ptl_elan_module_1_0_0_t * mp)
{
    int         i;
    int        *rails;
    int         num_rails;

    int         alloc_mainsize;
    int         alloc_mainbase;
    int         alloc_elansize;
    int         alloc_elanbase;

    mca_ptl_elan_state_t *state;

    /* Allocate the elan (priv)state structure off the heap. 
     * it is not available to the Elan thread. Another structure
     * for the Elan thread is to be used that instead
     */
    mp->elan_state = (mca_ptl_elan_state_t *)
        malloc (sizeof (mca_ptl_elan_state_t));

    if (NULL == mp->elan_state)
        return (OMPI_ERROR);

    memset ((void *) mp->elan_state, 0, sizeof (mca_ptl_elan_state_t));
    state = mp->elan_state;

    /* Initialise enough of state so we can call elan_exception() */
    state->elan_version = ELAN_VERSION;
    state->elan_ctx = NULL;
    state->elan_rail = NULL;
    state->elan_vp = ELAN_INVALID_PROCESS;
    state->elan_nvp = 0;
    state->elan_debug = 0;
    state->elan_traced = 0;
    state->elan_pagesize = sysconf (_SC_PAGESIZE);
    state->elan_pid = getpid ();

    /* Default allocator parameters */
    state->elan_flags = 0;
    state->main_size = ELAN_ALLOC_SIZE;
    state->elan_size = ELAN_ALLOCELAN_SIZE;
    state->elan_flags |= 
	(EXCEPTIONCORE | EXCEPTIONTRACE | EXCEPTIONDBGDUMP);
    state->elan_debugfile = (FILE*)NULL;
    state->elan_signalnum = SIGABRT;

#ifdef ELAN_VERSION
    if (!elan_checkVersion (ELAN_VERSION)) {
        return OMPI_ERROR;
    }
#endif

    /* Allocate elan capability from the heap */
    state->elan_cap =
        (ELAN_CAPABILITY *) malloc (sizeof (ELAN_CAPABILITY));

    if (NULL == state->elan_cap) {
        return OMPI_ERROR;
    } else {
        memset (state->elan_cap, 0, sizeof (ELAN_CAPABILITY));
    }

    /* Process the capability info supplied by RMS */
    if (getenv ("ELAN_AUTO") || getenv ("RMS_NPROCS")) {
        /* RMS generated capabilities */
        if (rms_getcap (0, state->elan_cap)) {
            return OMPI_ERROR;
        }
    }

    if ((num_rails = state->elan_nrails =
         elan_nrails (state->elan_cap)) <= 0) {
        return OMPI_ERROR;
    }

    /* MULTI-RAIL: 
     * Allocate storage space for each Elan rail info (ptrs) 
     * Allocate storage space for each Elan SDRAM state (ptrs) */

    state->all_rails = (ompi_elan_rail_t**) 
	malloc (sizeof (ompi_elan_rail_t *) * num_rails);
    state->all_estates = (ELAN_ESTATE **)
        malloc (sizeof (ELAN_ESTATE *) * num_rails);
    if (state->all_estates == NULL || state->all_rails == NULL) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* MULTI-RAIL:
     * initialise each elan rail, using the physical rail info gleaned from 
     * the capability */
    if (NULL == (rails = (int *) malloc (sizeof (int) * num_rails))) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    (void) elan_rails (state->elan_cap, rails);
    if (NULL == (state->elan_rail = (ELAN_RAIL **)
                 malloc (sizeof (ELAN_RAIL **) * (num_rails + 1)))) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    state->elan_rail[num_rails] = NULL;
    alloc_mainsize = ELAN_ALIGNUP (state->main_size, state->elan_pagesize);
    alloc_mainbase = (ADDR_ELAN) ((uintptr_t) state->main_base);
    alloc_elansize = ELAN_ALIGNUP (state->elan_size, state->elan_pagesize);
    alloc_elanbase = (ADDR_ELAN) ((uintptr_t) state->elan_base);

    for (i = 0; i < num_rails; i++) {

        ompi_elan_rail_t *rail;
        ELAN_ESTATE *estate;
        ELAN_EPRIVSTATE *priv_estate;

        /* Allocate the Main memory control structure for this rail */
        if (NULL == (rail = state->all_rails[i] = 
		    (ompi_elan_rail_t *) 
		    malloc (sizeof (ompi_elan_rail_t)))) {
            return OMPI_ERROR;
        }

        memset (rail, 0, sizeof (ompi_elan_rail_t));

        if (NULL == (rail->rail_ctx = elan4_init (rails[i]))) {
            return OMPI_ERROR;
        }

        if (NULL == (rail->rail_sdram = elan4_open_sdram (rails[i],
                                                       0,
                                                       alloc_elansize))) {
            return OMPI_ERROR;
        }

        if (NULL == (rail->rail_alloc =
                     elan4_createAllocator (state->main_size,
                                            rail->rail_sdram, 0,
                                            state->elan_size))) {
            return OMPI_ERROR;
        }

        if (elan4_set_standard_mappings (rail->rail_ctx) < 0
            || elan4_set_required_mappings (rail->rail_ctx) < 0) {
            return OMPI_ERROR;
        }

        /* Now allocate the SDRAM Elan control structure for this rail */
        if (NULL == (estate = state->all_estates[i] =
                     elan4_allocElan(rail->rail_alloc, ELAN_ALIGN,
                                 sizeof (ELAN_EPRIVSTATE)))) {
            return OMPI_ERROR;
        }

        priv_estate = (ELAN_EPRIVSTATE *) estate;
        memset (priv_estate, 0, sizeof (ELAN_EPRIVSTATE));

        /* Allocate a command port for non sten functions etc */
        if (NULL == (rail->rail_cmdq = elan4_alloc_cmdq (rail->rail_ctx,
                                              rail->rail_alloc, CQ_Size8K,
                                              CQ_ModifyEnableBit |
                                              CQ_WriteEnableBit |
                                              CQ_WaitEventEnableBit |
                                              CQ_SetEventEnableBit |
                                              CQ_ThreadStartEnableBit,
                                              NULL))) {
            return OMPI_ERROR;
        }

        /* Allocate a command port for thread rescheduling etc */
        if (NULL == (rail->rail_ecmdq = elan4_alloc_cmdq (rail->rail_ctx,
                                               rail->rail_alloc, CQ_Size8K,
                                               CQ_EnableAllBits,
                                               NULL))) {
            return OMPI_ERROR;
        }

        /* Make the rail pointers 'external' */
        state->elan_rail[i] = (ELAN_RAIL *) rail;

        rail->rail_estate = estate;
        rail->rail_railNo = rails[i];

        {
	    ompi_elan_railtable_t *rt;

            if (NULL == (rt = (ompi_elan_railtable_t *) malloc (
			    sizeof (ompi_elan_railtable_t)))) {
                return OMPI_ERROR;
            }
            memset (rt, 0, sizeof (ompi_elan_railtable_t));

            rt->rt_nrails = 1;
            rt->rt_rail = 0;
            rt->rt_railReal = i;
            rt->rt_allRails = (struct ompi_elan_rail_t *) 
		&(state->all_rails[i]);

            rail->rail_railTable = rt;
        }
    }                           /* for each rail */

    free (rails);

    state->elan_ctx = state->elan_rail[0]->rail_ctx;
    state->elan_estate = (void *) state->all_estates[0];

    ompi_elan_attach_network (state);

    /* Set the rms_resourceId */
    if (rms_getprgid (getpid (), &state->elan_rmsid) < 0) {
        state->elan_rmsid = -1;
    }

    /* Now open ourselves to the network */
    for (i = 0; state->elan_rail[i]; i++) {
        elan4_block_inputter (state->elan_rail[i]->rail_ctx, 0);
    }

    return (OMPI_SUCCESS);
}

int
ompi_mca_ptl_elan_setup (mca_ptl_elan_module_1_0_0_t * mp)
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
    if (OMPI_SUCCESS != ompi_init_elan_qdma (mp)) {
        return OMPI_ERROR;
    }

    if (OMPI_SUCCESS != ompi_init_elan_rdma (mp)) {
        return OMPI_ERROR;
    }

    if (OMPI_SUCCESS != ompi_init_elan_sten (mp)) {
        return OMPI_ERROR;
    }

    if (OMPI_SUCCESS != ompi_init_elan_stat (mp)) {
        return OMPI_ERROR;
    }

    return (OMPI_SUCCESS);
}

int
ompi_mca_ptl_elan_fin (mca_ptl_elan_module_1_0_0_t * mp)
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
ompi_elan_attach_network (mca_ptl_elan_state_t * state)
{
    int         i, vp, *vps, num_rails;
    ELAN_LOCATION loc;
    ELAN_CAPABILITY *cap = state->elan_cap;

    num_rails = state->elan_nrails;

    if (state->elan_attached) {
        /* already successfully attached */
        return OMPI_SUCCESS;
    } else {
        state->elan_attached = 1;
    }

    for (i = 0; i < num_rails; i++) {
        ompi_elan_rail_t       *rail = state->all_rails[i];
        ELAN_LOCATION loc;

        /* Add all virtual process from 0 to (nvp-1) */
        if (elan4_add_p2pvp (rail->rail_ctx, 0, cap) < 0) {
            /* FIXME: exception code */
            return OMPI_ERROR;
        }

        /* block the inputter until we are really ready */
        elan4_block_inputter (rail->rail_ctx, 1);

        if (elan4_attach (rail->rail_ctx, cap)) {
            /* FIXME: exception code */
            return OMPI_ERROR;
        }

        /* NB: We should have same vp for all capabilities */
        loc.loc_node = ((ELAN4_CTX *) rail->rail_ctx)->ctx_position.pos_nodeid
            - cap->cap_lownode;
        loc.loc_context = cap->cap_mycontext - cap->cap_lowcontext;

        /* TODO: debug code for loc */
        if (cap->cap_type == ELAN_CAP_TYPE_HWTEST)
            state->elan_vp = 0;
        else
            state->elan_vp = elan_location2vp (loc, state->elan_cap);

        /* Initialise the Elan version of this data */
        ((ELAN_ESTATE *) rail->rail_estate)->vp = state->elan_vp;

        /* Allocate a cookie pool for the thread processor 
         * and copy to sdram */
        rail->rail_cpool =
            elan4_allocCookiePool (rail->rail_ctx, state->elan_vp);
        ((ELAN_EPRIVSTATE *) rail->rail_estate)->tcookie =
            rail->rail_cpool->cp_cookie;
    }

    /* Allocate a cookie pool for the exported API (0th rail) */
    state->elan_cpool = elan4_allocCookiePool (
	    state->elan_ctx, state->elan_vp);

    loc = elan_vp2location (state->elan_vp, state->elan_cap);

    /* update THREAD elan_dbg info of debugfile */
    if (state->elan_debugfile && fileno (state->elan_debugfile) != -1) {
        for (i = 0; i < num_rails; i++) {
            ELAN4_CTX *ctx = state->elan_rail[i]->rail_ctx;

            /* Convert FILE * stream to fd for THRD output */
            ((ELAN_ESTATE *) state->all_estates[i])->debugFd =
                fileno (state->elan_debugfile);

            /* Also re-direct libelan4 output to this file */
            elan4_set_debugfd (ctx, fileno (state->elan_debugfile));
        }
    }

    /* Determine the number of processes described by the capability */
    state->elan_nvp = elan_nvps (state->elan_cap);

#if 0
    /* FIXME: fix the following rail table stuff */
    state->railTable = _elan_railtable_create (elan_state);
    state->elan_rail = _elan_railtable_getRailSelf (elan_state,
                                               elan_state->railTable);

    if (NULL == (state->elan_localIds = (int *)
                 MALLOC (elan_state->nvp * sizeof (int)))) {
        /* FIXME: exception code */
        return OMPI_ERROR;
    }
#endif

    /* Set all to non local initially */
    for (vp = 0; vp < state->elan_nvp; vp++)
        state->elan_localvps[vp] = -1;

    /* Stash our own process location */
    state->elan_myloc = elan_vp2location (state->elan_vp, state->elan_cap);
    state->elan_maxlocals = elan_maxlocal (state->elan_cap);
    state->elan_numlocals = elan_nlocal (state->elan_myloc.loc_node,
                                         state->elan_cap);

    /* Allocate more than we need to keep the heap in sync */
    if (NULL == (vps = (int *) malloc (sizeof (int) * state->elan_nvp))) {
        /* FIXME: exception code */
        return OMPI_ERROR;
    }

    /* Fill out the local vp array */
    elan_localvps (state->elan_myloc.loc_node,
                   state->elan_cap, vps, state->elan_numlocals);

    for (i = 0; i < state->elan_numlocals; i++) {
        int         localvp = vps[i];

        /* This is a local process */
        state->elan_localvps[localvp] = i;

        if (localvp == state->elan_vp) {
            state->elan_localid = i;
        }
    }

    /* TODO: Debug code here for elan_state */

#if 1
    /*
     * We need to create one of these maps in each rail estate ctx too
     */
    for (i = 0; i < num_rails; i++) {
        ompi_elan_rail_t       *rail = state->all_rails[i];
        ELAN_ESTATE *estate = state->all_estates[i];
        int        *localIdsElan;

        /* Allocate a copy of this in the SDRAM of each rail */
        if (0 == (localIdsElan = elan4_allocElan(rail->rail_alloc, ELAN_ALIGN,
                                             state->elan_nvp *
                                             sizeof (int)))) {
            /* FIXME: exception code */
            return OMPI_ERROR;
        }

        estate->localIds = elan4_main2elan( (ELAN4_CTX*)rail->rail_ctx, 
		(void*)localIdsElan);

        for (vp = 0; vp < state->elan_nvp; vp++) {
            localIdsElan[vp] = state->elan_localvps[vp];
        }

        estate->nLocalIds = state->elan_numlocals;
        estate->localId = state->elan_localid;
        estate->nvp = state->elan_nvp;

        /* Update thread info on primary rail too */
        ((ELAN_EPRIVSTATE *) estate)->rail = (ELAN_RAIL **) state->elan_rail;
    }
#endif

    /* Done with vps array now */
    free (vps);

    return (OMPI_SUCCESS);
}

static int
ompi_init_elan_qdma (mca_ptl_elan_module_1_0_0_t * mp)
{
    return (OMPI_SUCCESS);
}

static int
ompi_init_elan_rdma (mca_ptl_elan_module_1_0_0_t * mp)
{
    return (OMPI_SUCCESS);
}

static int
ompi_init_elan_sten (mca_ptl_elan_module_1_0_0_t * mp)
{
    return (OMPI_SUCCESS);
}

static int
ompi_init_elan_stat (mca_ptl_elan_module_1_0_0_t * mp)
{
    return (OMPI_SUCCESS);
}
