/*
 * $HEADER$
 */

#include <signal.h>
#include <unistd.h>
#include <stdio.h>

#include "ptl_elan.h"
#include "ptl_elan_priv.h"

#define _ELAN4

mca_ptl_elan_state_t mca_ptl_elan_global_state;

static int
ompi_mca_ptl_elan_setup (mca_ptl_elan_state_t * ems)
{
    mca_ptl_elan_component_t *emp;
    int         rail_count;

    START_FUNC(PTL_ELAN_DEBUG_INIT);

    rail_count = ems->elan_nrails;
    emp = ems->elan_component;
    emp->num_modules = 0;
    emp->modules = malloc (rail_count * sizeof (mca_ptl_elan_module_t *));
    if (NULL == emp->modules) {
        ompi_output (0,
                     "[%s:%d] error in malloc for ptl references \n",
                     __FILE__, __LINE__);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* Initialiaze PTL's */
    do {
        char        param[256];
        mca_ptl_elan_module_t *ptl;

        ptl = malloc (sizeof (mca_ptl_elan_module_t));
        if (NULL == ptl) {
            ompi_output (0,
                         "[%s:%d] error in malloc for ptl structures \n",
                         __FILE__, __LINE__);
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        memcpy (ptl, &mca_ptl_elan_module, sizeof (mca_ptl_elan_module));
        emp->modules[emp->num_modules] = ptl;

        /* MCA related structures */

        ptl->ptl_ni_local = emp->num_modules;
        ptl->ptl_ni_total = rail_count;

        /* allow user to specify per rail bandwidth and latency */
        sprintf (param, "bandwidth_elanrail%d", emp->num_modules);
        ptl->super.ptl_bandwidth =
            mca_ptl_elan_param_register_int (param, 1000);
        sprintf (param, "latency_elanrail%d", emp->num_modules);
        ptl->super.ptl_latency =
            mca_ptl_elan_param_register_int (param, 1);

        /* Setup elan related structures such as ctx, rail */
        ptl->ptl_elan_rail = ems->elan_rail[emp->num_modules];
        ptl->ptl_elan_ctx  = ems->elan_rail[emp->num_modules]->rail_ctx;
        ptl->elan_vp  = ems->elan_vp;
        ptl->elan_nvp = ems->elan_nvp;
	OBJ_CONSTRUCT (&ptl->recv_frags, ompi_list_t);
	OBJ_CONSTRUCT (&ptl->send_frags, ompi_list_t);
	OBJ_CONSTRUCT (&ptl->pending_acks, ompi_list_t);
        emp->num_modules++;
    } while (emp->num_modules < rail_count);

    /* Allocating all the communication strcutures for PTL's, */
    if (OMPI_SUCCESS != ompi_init_elan_qdma (emp, rail_count)) {
        return OMPI_ERROR;
    }

    /* 
     * XXX: Leave the following later after testing of QDMA is done
     */
    if (OMPI_SUCCESS != ompi_init_elan_putget (emp, rail_count)) {
        return OMPI_ERROR;
    }

    /* 
     * XXX: initialize STAT (including SYNC) structures.
     */
    if (OMPI_SUCCESS != ompi_init_elan_stat (emp, rail_count)) {
        return OMPI_ERROR;
    }

    END_FUNC(PTL_ELAN_DEBUG_INIT);
    return (OMPI_SUCCESS);
}

/* Attach to the network */
static int  elan_attached = 0;
static int
ompi_elan_attach_network (mca_ptl_elan_state_t * ems)
{
    int           i; 
    int           vp;
    int           *vps;
    int           num_rails;

    ELAN_LOCATION loc;
    ELAN_CAPABILITY *cap;
   
    cap = ems->elan_cap;
    num_rails = ems->elan_nrails;

    if (elan_attached) {
        /* already successfully attached */
        return OMPI_SUCCESS;
    } else {
        elan_attached = 1;
    }

    for (i = 0; i < num_rails; i++) {
        RAIL       *rail = ems->all_rails[i];
        ELAN_LOCATION loc;

        /* Add all virtual process from 0 to (nvp-1) */
        if (elan4_add_p2pvp (rail->r_ctx, 0, cap) < 0) {
            ompi_output (0,
                         "[%s:%d] error in adding vp to elan capability \n",
                         __FILE__, __LINE__);
            return OMPI_ERROR;
        }

        /* block the inputter until we are really ready */
        elan4_block_inputter (rail->r_ctx, 1);

        if (elan4_attach (rail->r_ctx, cap)) {
            ompi_output (0,
                         "[%s:%d] error in attaching to the network \n",
                         __FILE__, __LINE__);
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
        rail->r_cpool = elan4_allocCookiePool (rail->r_ctx, ems->elan_vp);
    }

    loc = elan_vp2location (ems->elan_vp, ems->elan_cap);

    /* update THREAD elan_dbg info of debugfile */
    if (ems->elan_debugfile && fileno (ems->elan_debugfile) != -1) {
        for (i = 0; i < num_rails; i++) {
            ELAN4_CTX  *ctx = ems->elan_rail[i]->rail_ctx;

            /* Convert FILE * stream to fd for THRD output */
            ((ELAN_ESTATE *) ems->all_estates[i])->debugFd =
                fileno (ems->elan_debugfile);

            /* Also re-direct libelan4 output to this file */
            elan4_set_debugfd (ctx, fileno (ems->elan_debugfile));
        }
    }

    /* Determine the number of processes described by the capability */
    ems->elan_nvp = elan_nvps (ems->elan_cap);

    if (ems->elan_vp >= ems->elan_nvp) {
        ompi_output (0,
                     "[%s:%d] error getting vp and nvp from capability \n",
                     __FILE__, __LINE__);
        return OMPI_ERROR;
    }

    /* Allocate more than we need to keep the heap in sync */
    ems->elan_localvps = (int *) malloc (sizeof (int) * (ems->elan_nvp + 1));

    if (NULL == ems->elan_localvps) {
        ompi_output (0,
                     "[%s:%d] error in malloc for elan_localvps \n", 
		     __FILE__, __LINE__);
        return OMPI_ERROR;
    }

    /* Set all to non local initially */
    for (vp = 0; vp < ems->elan_nvp; vp++)
        ems->elan_localvps[vp] = -1;

    /* Stash our own process location */
    ems->elan_myloc = elan_vp2location (ems->elan_vp, ems->elan_cap);
    ems->elan_maxlocals = elan_maxlocal (ems->elan_cap);
    ems->elan_numlocals =
        elan_nlocal (ems->elan_myloc.loc_node, ems->elan_cap);

    /* Allocate more than we need to keep the heap in sync */
    if (NULL == (vps = (int *) malloc (sizeof (int) * ems->elan_nvp))) {
        ompi_output (0,
                     "[%s:%d] error in malloc for vps \n", __FILE__,
                     __LINE__);
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

    /* Done with vps array now */
    free (vps);

    return (OMPI_SUCCESS);
}

static void
ompi_module_elan_close_ptls (mca_ptl_elan_component_t * emp,
                             int num_rails)
{
    int i;
    struct mca_ptl_elan_module_t *ptl; 

    /* FIXME: find the ones that are still there and free them */
    for (i = 0; i < num_rails; i ++ ) {
	ptl = emp->modules[i];
	if (NULL == ptl) continue;
	OBJ_DESTRUCT (&(ptl->recv_frags));
	OBJ_DESTRUCT (&(ptl->send_frags));
	OBJ_DESTRUCT (&(ptl->pending_acks));
    }
}

static void
ompi_module_elan_close_procs (mca_ptl_elan_component_t * emp,
                              int num_rails)
{
    /* FIXME: find the ones that are still there and free them */
}

ELAN_SLEEP *
ompi_init_elan_sleepdesc (mca_ptl_elan_state_t * ems,
                          RAIL * rail)
{
    ELAN_SLEEP *es;

    /* XXX: asking the caller to hold the lock */
    es = MALLOC (sizeof (ELAN_SLEEP));
    OMPI_PTL_ELAN_CHECK_UNEX (es, NULL, NULL, 0);
    memset (es, 0, sizeof (ELAN_SLEEP));

    /* Assign next interrupt cookie value */
    es->es_cookie = ems->intcookie++;

    /* XXX, rail[0] is choosen instead this rail */
    if (elan4_alloc_intcookie (ems->elan_rail[0]->rail_ctx,
                               es->es_cookie) < 0) {
        ompi_output (0,
                     "[%s:%d] Failed to allocate IRQ cookie \n",
                     __FILE__, __LINE__);
    }

    es->es_cmdBlk = ALLOC_ELAN (rail, E4_EVENTBLOCK_SIZE,
                                E4_EVENTBLOCK_SIZE);
    OMPI_PTL_ELAN_CHECK_UNEX (es->es_cmdBlk, 0, NULL, 0);

    /*Allocate a pair of command queues for blocking waits with */
    es->es_cmdq = OMPI_PTL_ELAN_ALLOC_CMDQ(rail->r_ctx, 
				    rail->r_alloc,
                                    CQ_Size1K,
                                    (CQ_WriteEnableBit |
                                    CQ_WaitEventEnableBit), NULL);
    OMPI_PTL_ELAN_CHECK_UNEX (es->es_cmdq, NULL, NULL, 0);

    /* This command queue used to fire the IRQ via 
       a cmd port copy event */
    es->es_ecmdq = OMPI_PTL_ELAN_ALLOC_CMDQ (rail->r_ctx, 
				    rail->r_alloc,
				    CQ_Size1K, /* CQ_EnableAllBits, */
				    (CQ_WriteEnableBit 
				    | CQ_InterruptEnableBit), 
				    NULL);
    OMPI_PTL_ELAN_CHECK_UNEX (es->es_ecmdq, NULL, NULL, 0);
    es->es_next = NULL;

    /* XXX: asking the caller to release the lock */
    return es;
}

int
mca_ptl_elan_state_init (mca_ptl_elan_component_t * emp)
{
    int         i;
    int        *rails;
    int         num_rails;

    int         alloc_mainsize;
    int         alloc_mainbase;
    int         alloc_elansize;
    int         alloc_elanbase;

    mca_ptl_elan_state_t *ems;

    START_FUNC(PTL_ELAN_DEBUG_INIT);

    ems = &mca_ptl_elan_global_state;

    /* Hook two of them togther */
    ems->elan_component = emp;
    emp->elan_ctrl   = ems;

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
    ems->elan_waittype = ELAN_POLL_EVENT;       /* or ELAN_WAIT_EVENT */
    ems->main_size = ELAN_ALLOC_SIZE;
    ems->elan_size = ELAN_ALLOCELAN_SIZE;
    ems->elan_flags |= (EXCEPTIONCORE | EXCEPTIONTRACE | EXCEPTIONDBGDUMP);
    ems->elan_debugfile = (FILE *) NULL;
    ems->elan_signalnum = SIGABRT;

#ifdef ELAN_VERSION
    if (!elan_checkVersion (ELAN_VERSION)) {
        ompi_output (0,
                     "Elan version is not compatible with %s \n",
                     ELAN_VERSION);
        return OMPI_ERROR;
    }
#endif

    /* Allocate elan capability from the heap */
    ems->elan_cap = (ELAN_CAPABILITY *) malloc (sizeof (ELAN_CAPABILITY));
    OMPI_PTL_ELAN_CHECK_UNEX (ems->elan_cap, NULL, OMPI_ERROR, 0);
    memset (ems->elan_cap, 0, sizeof (ELAN_CAPABILITY));

    /* Process the capability info supplied by RMS */
    if (getenv ("ELAN_AUTO") || getenv ("RMS_NPROCS")) {
        /* RMS generated capabilities */
        if (rms_getcap (0, ems->elan_cap)) {
            ompi_output (0,
                         "[%s:%d] error in gettting elan capability \n",
                         __FILE__, __LINE__);
            return OMPI_ERROR;
        }
    }

    if ((num_rails = ems->elan_nrails = elan_nrails (ems->elan_cap)) <= 0) {
        ompi_output (0,
                     "[%s:%d] error in gettting number of rails \n",
                     __FILE__, __LINE__);
        return OMPI_ERROR;
    }

    ems->all_rails = (RAIL **) malloc (sizeof (RAIL *) * num_rails);
    OMPI_PTL_ELAN_CHECK_UNEX (ems->all_rails, NULL,
                              OMPI_ERR_OUT_OF_RESOURCE, 0);

    ems->all_estates = (ADDR_SDRAM *) 
	malloc (sizeof (ELAN_ESTATE *) * num_rails);
    OMPI_PTL_ELAN_CHECK_UNEX (ems->all_estates, NULL,
                              OMPI_ERR_OUT_OF_RESOURCE, 0);

    rails = (int *) malloc (sizeof (int) * num_rails);
    OMPI_PTL_ELAN_CHECK_UNEX (rails, NULL, OMPI_ERR_OUT_OF_RESOURCE, 0);
    (void) elan_rails (ems->elan_cap, rails);

    ems->elan_rail = (ELAN_RAIL **) malloc (sizeof (ELAN_RAIL **)
                                            * (num_rails + 1));
    OMPI_PTL_ELAN_CHECK_UNEX (ems->elan_rail, NULL,
                              OMPI_ERR_OUT_OF_RESOURCE, 0);
    ems->elan_rail[num_rails] = NULL;

    alloc_mainsize = ELAN_ALIGNUP (ems->main_size, ems->elan_pagesize);
    alloc_mainbase = (ADDR_ELAN) ((uintptr_t) ems->main_base);
    alloc_elansize = ELAN_ALIGNUP (ems->elan_size, ems->elan_pagesize);
    alloc_elanbase = (ADDR_ELAN) ((uintptr_t) ems->elan_base);

    /* XXX: Magic Quadrics number for the starting cookie value */
    ems->intcookie = 42;
    ems->rail_intcookie = (int *) malloc (sizeof (int) * (num_rails + 1));
    OMPI_PTL_ELAN_CHECK_UNEX (ems->rail_intcookie, NULL,
                              OMPI_ERR_OUT_OF_RESOURCE, 0);
    memset ((void*)ems->elan_cap, 0, (num_rails + 1) * sizeof (int));
    ems->rail_intcookie[num_rails] = 0;

    for (i = 0; i < num_rails; i++) {

        RAIL       *rail;
        ELAN_ESTATE *estate;
        ELAN_EPRIVSTATE *priv_estate;
        ELAN_SLEEP *es;

        /* Allocate the Main memory control structure for this rail */
        rail = ems->all_rails[i] = (RAIL *) malloc (sizeof (RAIL));
        OMPI_PTL_ELAN_CHECK_UNEX (rail, NULL, OMPI_ERROR, 0);
        memset (rail, 0, sizeof (RAIL));

        rail->r_ctx = elan4_init (rails[i]);
        OMPI_PTL_ELAN_CHECK_UNEX (rail->r_ctx, NULL, OMPI_ERROR, 0);

        rail->r_sdram = elan4_open_sdram (rails[i], 0, alloc_elansize);
        OMPI_PTL_ELAN_CHECK_UNEX (rail->r_sdram, NULL, OMPI_ERROR, 0);

        rail->r_alloc = elan4_createAllocator (ems->main_size,
                                               rail->r_sdram, 0,
                                               ems->elan_size);
        OMPI_PTL_ELAN_CHECK_UNEX (rail->r_alloc, NULL, OMPI_ERROR, 0);

        if (elan4_set_standard_mappings (rail->r_ctx) < 0
            || elan4_set_required_mappings (rail->r_ctx) < 0) {
            ompi_output (0,
                         "[%s:%d] error setting memory mapping for rail %d \n",
                         __FILE__, __LINE__, rails[i]);
            return OMPI_ERROR;
        }

        /* Now allocate the SDRAM Elan control structure for this rail */
        estate = ems->all_estates[i] = elan4_allocElan (rail->r_alloc,
                                                        ELAN_ALIGN,
                                                        sizeof
                                                        (ELAN_EPRIVSTATE));
        OMPI_PTL_ELAN_CHECK_UNEX (estate, NULL, OMPI_ERROR, 0);

        priv_estate = (ELAN_EPRIVSTATE *) estate;
        memset (priv_estate, 0, sizeof (ELAN_EPRIVSTATE));

        /* Allocate a command port for non sten functions etc */
        rail->r_cmdq = OMPI_PTL_ELAN_ALLOC_CMDQ (rail->r_ctx,
					 rail->r_alloc,
                                         CQ_Size8K,
                                         (CQ_ModifyEnableBit |
                                         CQ_WriteEnableBit |
                                         CQ_WaitEventEnableBit |
                                         CQ_SetEventEnableBit |
                                         CQ_ThreadStartEnableBit), NULL);
        OMPI_PTL_ELAN_CHECK_UNEX (rail->r_cmdq, NULL, OMPI_ERROR, 0);

        /* Allocate a command port for thread rescheduling etc */
        rail->r_ecmdq = OMPI_PTL_ELAN_ALLOC_CMDQ (rail->r_ctx, 
					  rail->r_alloc,
                                          CQ_Size8K, 
					  CQ_EnableAllBits,
                                          NULL);
        OMPI_PTL_ELAN_CHECK_UNEX (rail->r_ecmdq, NULL, OMPI_ERROR, 0);

        priv_estate->cport = elan4_main2elan (rail->r_ctx,
					      rail->r_ecmdq->cmdq_mapping);

        /* Save the rail pointers */
        ems->elan_rail[i] = (ELAN_RAIL *) rail;
        ems->rail_intcookie[i] = ems->intcookie;

        /* Allocate a Sleep Desc */
        es = ompi_init_elan_sleepdesc (ems, rail);

        /* XXX: put a lock and hold a lock */
        es->es_next = rail->r_sleepDescs;
        rail->r_sleepDescs = es;
        /* XXX: release the lock */

        estate->alloc = rail->r_alloc;
        estate->vp = ems->elan_vp;
        estate->debugFlags = ems->elan_flags;
        estate->debugFd = 1;
        priv_estate->pageSize = ems->elan_pagesize;
        rail->r_estate = estate;
        rail->r_railNo = rails[i];

        {
            struct railtable *rt;
            rt = (struct railtable *) malloc (sizeof (struct railtable));
            OMPI_PTL_ELAN_CHECK_UNEX (rt, NULL, OMPI_ERROR, 0);
            memset (rt, 0, sizeof (struct railtable));

            rt->rt_nrails = 1;
            rt->rt_rail = 0;
            rt->rt_railReal = i;
            rt->rt_allRails = (RAIL **) & (ems->all_rails[i]);
            rail->r_railTable = rt;
        }
    } /* for each rail */

    /* Free the local variable */
    free (rails);

    ems->elan_ctx = ems->elan_rail[0]->rail_ctx;
    ems->elan_estate = (void *) ems->all_estates[0];

    /* Attach to the device and open to the network */
    ompi_elan_attach_network (ems);

    /* Set the rms_resourceId */
    if (rms_getprgid (getpid (), &ems->elan_rmsid) < 0) {
        ems->elan_rmsid = -1;
    }

    /* Now open ourselves to the network */
    for (i = 0; ems->elan_rail[i]; i++) {
        elan4_block_inputter (ems->elan_rail[i]->rail_ctx, 0);
    }

    /* Setup communication infrastructure and construct PTL's */
    if (OMPI_SUCCESS != ompi_mca_ptl_elan_setup (ems)) {
        ompi_output (0,
                     "[%s:%d] error in setting up elan "
                     "communication state machines for elan PTL's.\n",
                     __FILE__, __LINE__);
        return OMPI_ERROR;
    }

    END_FUNC(PTL_ELAN_DEBUG_INIT);
    return (OMPI_SUCCESS);
}

int
mca_ptl_elan_state_finalize (mca_ptl_elan_component_t * emp)
{
    int     i;
    int     num_rails;
    mca_ptl_elan_state_t *ems;

    START_FUNC(PTL_ELAN_DEBUG_FIN);

    ems = &mca_ptl_elan_global_state;
    num_rails = ems->elan_nrails;

    ompi_module_elan_close_ptls (emp, num_rails);
    ompi_module_elan_close_procs (emp, num_rails);

    for (i = 0; i < num_rails; i++) {
        RAIL       *rail;

        rail = ems->all_rails[i];
        free (rail->r_railTable);

        /* Free the memory from the rail allocator */
        elan4_freeMain (rail->r_alloc, rail->r_ecmdq);
        elan4_freeMain (rail->r_alloc, rail->r_cmdq);
        elan4_freeElan (rail->r_alloc, ems->all_estates[i]);

        /* Since the cookie allocated from rail[0], be consistent here */
        elan4_free_intcookie (ems->all_rails[0]->r_ctx,
                              ems->rail_intcookie[i]);

        elan4_destroyAllocator (rail->r_alloc);
        elan4_close_sdram (rail->r_sdram);

        /*elan4_fini (rail->r_ctx); Not working yet from libelan */

        /* Free the rail structure used one the array of pointers 
         * to the RAILs, either all_rails for elan_rails */
        free (ems->all_rails[i]);
    }

    free (ems->elan_rail);
    free (ems->all_estates);
    free (ems->all_rails);
    free (ems->elan_cap);

    END_FUNC(PTL_ELAN_DEBUG_FIN);
    return (OMPI_SUCCESS);
}

#if OMPI_PTL_ELAN_THREADING
/* XXX: 
 * Create threads per PTL and have them up running, 
 * Blocking on the completion queues */
int
mca_ptl_elan_thread_init (mca_ptl_elan_component_t * emp)
{
    int     num_rails;
    mca_ptl_elan_state_t *ems;

    START_FUNC(PTL_ELAN_DEBUG_INIT);

    ems = &mca_ptl_elan_global_state;
    num_rails = ems->elan_nrails;

    END_FUNC(PTL_ELAN_DEBUG_INIT);
    return (OMPI_SUCCESS);
}


/* XXX: 
 * Send signal/interrupt(s) to threads and wait for them to join */
int
mca_ptl_elan_thread_close (mca_ptl_elan_component_t * emp)
{
    int     num_rails;
    mca_ptl_elan_state_t *ems;

    START_FUNC(PTL_ELAN_DEBUG_FIN);

    ems = &mca_ptl_elan_global_state;
    num_rails = ems->elan_nrails;

    END_FUNC(PTL_ELAN_DEBUG_FIN);
    return (OMPI_SUCCESS);
}
#endif /* End of OMPI_PTL_ELAN_THREADING */

