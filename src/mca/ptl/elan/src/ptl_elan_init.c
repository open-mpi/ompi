
#include "ptl_elan.h"

static int  ompi_init_elan_qdma (mca_ptl_elan_module_1_0_0_t * mp);
static int  ompi_init_elan_sten (mca_ptl_elan_module_1_0_0_t * mp);
static int  ompi_init_elan_rdma (mca_ptl_elan_module_1_0_0_t * mp);
static int  ompi_init_elan_stat (mca_ptl_elan_module_1_0_0_t * mp);
static int  ompi_elan_attach_network (mca_ptl_elan_state_t * state);

int
ompi_mca_ptl_elan_init (mca_ptl_elan_module_1_0_0_t * mp)
{
#if ELAN_COMP
    int         i;
    int        *rails;
    int         num_rails;
    int         max_backoff;
    int         max_fastbackoff;

    int         alloc_mainsize;
    int         alloc_mainbase;
    int         alloc_elansize;
    int         alloc_elanbase;

    mca_ptl_elan_state_t *state;

    max_backoff = elan_rup2 (ELAN_MAXBACKOFF) - 1;
    max_fastbackoff = elan_rup2 (ELAN_MAXFASTBACKOFF) - 1;

    /* Allocate the elan (priv)state structure off the heap. 
     * it is not available to the Elan thread. Another structure
     * for the Elan thread is to be used that instead
     */
    mp->elan_state = (mca_ptl_elan_state_t *)
        malloc (sizeof (mca_ptl_elan_state_t));

    if (NULL == mp->elan_state)
        return (NULL);

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
    state->main_size = ELAN_ALLOC_SIZE;
    state->elan_size = ELAN_ALLOCELAN_SIZE;
    state->elan_flags |=
        (EXCEPTIONCORE | EXCEPTIONTRACE | EXCEPTIONDBGDUMP);
    state->elan_debugfile = NULL;
    state->elan_signalnum = SIGABRT;

#ifdef ELAN_VERSION
    if (!elan_checkVersion (ELAN_VERSION)) {
        return OMPI_ERROR;
    }
#endif

    /* FIXME: when to initialize it */
    OBJ_CONSTRUCT (&state->state_lock, ompi_mutex_t);

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

    state->all_rails = (RAIL **) malloc (sizeof (RAIL *) * num_rails);
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
        RAIL       *rail;
        ELAN_ESTATE *estate;
        ELAN_EPRIVSTATE *priv_estate;

        /* Allocate the Main memory control structure for this rail */
        if (NULL == (rail = state->all_rails[i] =
                     (RAIL *) malloc (sizeof (RAIL)))) {
            return OMPI_ERROR;
        }
        memset (rail, 0, sizeof (RAIL));

        /* TODO: Debug the state of rails */

        if (NULL == (rail->r_ctx = elan4_init (rails[i]))) {
            return OMPI_ERROR;
        }

        if (NULL == (rail->r_sdram = elan4_open_sdram (rails[i],
                                                       0,
                                                       alloc_elansize))) {
            return OMPI_ERROR;
        }

        if (NULL == (rail->r_alloc =
                     elan4_createAllocator (state->alloc_mainsize,
                                            rail->r_sdram, 0,
                                            state->alloc_elansize))) {
            return OMPI_ERROR;
        }

        if (elan4_set_standard_mappings (rail->r_ctx) < 0
            || elan4_set_required_mappings (rail->r_ctx) < 0) {
            return OMPI_ERROR;
        }

        if (elan4_register_trap_handler (rail->r_ctx,
                                         UTS_UNIMP_INSTR, UTS_TPROC,
                                         elan_unimp_handler, NULL) < 0) {
            return OMPI_ERROR;
        }

        /* Now allocate the SDRAM Elan control structure for this rail */
        if (NULL == (estate = mp->all_estates[i] =
                     ALLOC_ELAN (rail, ELAN_ALIGN,
                                 sizeof (ELAN_EPRIVSTATE)))) {
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
            return OMPI_ERROR;
        }

        /* Allocate a command port for thread rescheduling etc */
        if (NULL == (rail->r_ecmdq = elan4_alloc_cmdq (rail->r_ctx,
                                               rail->r_alloc, CQ_Size8K,
                                               CQ_EnableAllBits,
                                               NULL))) {
            return OMPI_ERROR;
        }

        priv_estate->cport =
            MAIN2ELAN (rail->r_ctx, rail->r_ecmdq->cmdq_mapping);

        /* Make the rail pointers 'external' */
        state->elan_rail[i] = (ELAN_RAIL *) rail;

        /* TODO: Find out how the following code work. */
        {
            ELAN_SLEEP *es;

            /* Allocate and then free an ELAN_SLEEP DESC to 
             * keep heap in sync */
            es = _elan_allocSleepDesc (state, rail);
            _elan_freeSleepDesc (state, rail, es);
        }

        estate->alloc = rail->r_alloc;
        estate->vp = state->vp; /* still ELAN_INVALID_PROCESS */
        estate->debugFlags = state->debugFlags;

        priv_estate->debugFd = 1;       /* stdout */
        priv_estate->maxBackOff = max_backoff;
        priv_estate->maxFastBackoff = max_fastbackoff;
        priv_estate->pageSize = state->elan_pagesize;

        rail->r_estate = estate;
        rail->r_railNo = rails[i];

        {
            RAILTABLE  *rt;

            if (NULL == (rt = (RAILTABLE *) malloc (sizeof (RAILTABLE)))) {
                return OMPI_ERROR;
            }
            memset (rt, 0, sizeof (RAILTABLE));

            rt->rt_nrails = 1;
            rt->rt_rail = 0;
            rt->rt_railReal = rail;
            rt->rt_allRails = &(state->all_rails[rail]);
            /* Debug the state, see _elan_railtable_createSingle */

            rail->r_railTable = rt;
        }

    }                           /* for each rail */

    free (rails);

    state->elan_ctx = state->rail[0]->rail_ctx;
    state->elan_estate = (void *) state->all_estates[0];

    /*_elan_eventInit(state);*/
    /*atexit(_elan_atExitCallBack); */

    ompi_elan_attach_network (state);

    /* Set the rms_resourceId */
    if (rms_getprgid (getpid (), &state->elan_rmsid) < 0) {
        state->elan_rmsid = -1;
    }

    /* Now open ourselves to the network */
    for (i = 0; state->elan_rail[i]; i++) {
        elan4_block_inputter (state->elan_rail[i]->rail_ctx, 0);
    }
#endif

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
#if MCA_PTL_ELAN_STAGE_TWO
    if (OMPI_SUCCESS != ompi_init_elan_rdma (mp)) {
        return OMPI_ERROR;
    }

    if (OMPI_SUCCESS != ompi_init_elan_sten (mp)) {
        return OMPI_ERROR;
    }

    if (OMPI_SUCCESS != ompi_init_elan_stat (mp)) {
        return OMPI_ERROR;
    }
#endif

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
#if ELAN_COMP
    int         i, vp, *vps, num_rails;
    ELAN_LOCATION loc;
    ELAN_CAPABILITY *cap = state->elan_cap;

    num_rails = state->elan_nrails;

    if (state->attached) {
        /* already successfully attached */
        return;
    } else {
        state->attached = 1;
    }

    for (i = 0; i < num_rails; i++) {
        RAIL       *rail = state->all_rails[i];
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
        loc.loc_node = ((CTX_ELAN *) rail->r_ctx)->ctx_position.pos_nodeid
            - cap->cap_lownode;
        loc.loc_context = cap->cap_mycontext - cap->cap_lowcontext;

        /* TODO: debug code for loc */
        if (cap->cap_type == ELAN_CAP_TYPE_HWTEST)
            state->elan_vp = 0;
        else
            state->elan_vp = elan_location2vp (loc, state->elan_cap);

        /* Initialise the Elan version of this data */
        ((ELAN_ESTATE *) rail->r_estate)->vp = state->elan_vp;

        /* Allocate a cookie pool for the thread processor 
         * and copy to sdram */
        rail->r_cpool =
            elan4_allocCookiePool (rail->r_ctx, state->elan_vp);
        ((ELAN_EPRIVSTATE *) rail->r_estate)->tcookie =
            rail->r_cpool->cp_cookie;
    }

    /* Allocate a cookie pool for the exported API (0th rail) */
    state->cpool = elan4_allocCookiePool (state->elan_ctx, state->elan_vp);

    loc = elan_vp2location (state->elan_vp, state->elan_cap);

    /* update THREAD elan_dbg info of debugfile */
    if (state->elan_debugfile && fileno (state->elan_debugfile) != -1) {
        for (i = 0; i < num_rails; i++) {
            CTX_ELAN   *ctx = state->elan_rail[i]->rail_ctx;

            /* Convert FILE * stream to fd for THRD output */
            ((ELAN_ESTATE *) state->all_estates[i])->debugFd =
                fileno (state->elan_debugfile);

            /* Also re-direct libelan4 output to this file */
            elan4_set_debugfd (ctx, fileno (state->elan_debugfile));
        }
    }

    /* Determine the number of processes described by the capability */
    state->elan_nvp = elan_nvps (state->elan_cap);

    /* Start allocating bcastVps a little way after the pt2pt ones */
    state->bcastVp = state->baseBcastVp = state->elan_nvp + 16;
    if (state->elan_vp >= elan_state->elan_nvp) {
        /* FIXME: exception code */
        return OMPI_ERROR;
    }

    /* FIXME: fix the following rail table stuff */
    state->railTable = _elan_railtable_create (elan_state);
    state->rail = _elan_railtable_getRailSelf (elan_state,
                                               elan_state->railTable);

    if (NULL == (state->elan_localIds = (int *)
                 MALLOC (elan_state->nvp * sizeof (int)))) {
        /* FIXME: exception code */
        return OMPI_ERROR;
    }

    /* Set all to non local initially */
    for (vp = 0; vp < state->elan_nvp; vp++)
        state->elan_localvps[vp] = -1;

    /* Stash our own process location */
    state->myloc = elan_vp2location (state->elan_vp, state->elan_cap);
    state->elan_maxlocals = elan_maxlocal (elan_state->cap);
    state->elan_numlocals = elan_nlocal (state->myloc.loc_node,
                                         state->elan_cap);

    /* Allocate more than we need to keep the heap in sync */
    if (NULL == (vps = (int *) malloc (sizeof (int) * elan_state->nvp))) {
        /* FIXME: exception code */
        return OMPI_ERROR;
    }

    /* Fill out the local vp array */
    elan_localvps (state->myloc.loc_node,
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

    /*
     * We need to create one of these maps in each rail estate ctx too
     */
    for (i = 0; i < num_rails; i++) {
        RAIL       *rail = state->all_rails[i];
        ELAN_ESTATE *estate = state->all_estates[i];
        int        *localIdsElan;

        /* Allocate a copy of this in the SDRAM of each rail */
        if (0 == (localIdsElan = ALLOC_ELAN (rail, ELAN_ALIGN,
                                             elan_state->nvp *
                                             sizeof (int)))) {
            /* FIXME: exception code */
            return OMPI_ERROR;
        }

        estate->localIds = SDRAM2ELAN (rail->r_ctx, localIdsElan);

        for (vp = 0; vp < state->elan_nvp; vp++) {
            localIdsElan[vp] = state->elan_localvps[vp];
        }

        estate->nLocalIds = state->elan_numlocals;
        estate->localId = state->elan_localid;
        estate->nvp = state->elan_nvp;

        /* Update thread info on primary rail too */
        ((ELAN_EPRIVSTATE *) estate)->rail = state->elan_rail;
    }

    /* Done with vps array now */
    free (vps);
#endif

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
