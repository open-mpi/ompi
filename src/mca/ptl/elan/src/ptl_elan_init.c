
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
    ems->elan_cap = (ELAN_CAPABILITY *) malloc (sizeof (ELAN_CAPABILITY));

    if (NULL == ems->elan_cap) {
	ompi_output(0, 
		"[%s:%d] error in allocating memory for elan capability \n",
		__FILE__, __LINE__);
        return OMPI_ERROR;
    } else {
        memset (ems->elan_cap, 0, sizeof (ELAN_CAPABILITY));
    }

    /* Process the capability info supplied by RMS */
    if (getenv ("ELAN_AUTO") || getenv ("RMS_NPROCS")) {
        /* RMS generated capabilities */
        if (rms_getcap (0, ems->elan_cap)) {
	    ompi_output(0, 
		    "[%s:%d] error in gettting elan capability \n",
		    __FILE__, __LINE__);
            return OMPI_ERROR;
        }
    }

    if ((num_rails = ems->elan_nrails =
         elan_nrails (ems->elan_cap)) <= 0) {
	ompi_output(0, 
		"[%s:%d] error in gettting number of rails \n",
		__FILE__, __LINE__);
        return OMPI_ERROR;
    }

    ems->all_rails = (RAIL **) malloc (sizeof (RAIL *) * num_rails);
    if (ems->all_rails == NULL) {
	ompi_output(0, 
		"[%s:%d] error in allocating memory for all_rails\n",
		__FILE__, __LINE__);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    ems->all_estates = (ADDR_SDRAM *)
       	malloc (sizeof(ELAN_ESTATE *) * num_rails);
    if (ems->all_estates == NULL) {
	ompi_output(0, 
		"[%s:%d] error in allocating memory for all_estates\n",
		__FILE__, __LINE__);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    if (NULL == (rails = (int *) malloc (sizeof (int) * num_rails))) {

	ompi_output(0, 
		"[%s:%d] error in allocating memory \n",
		__FILE__, __LINE__);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    (void) elan_rails (ems->elan_cap, rails);

    if (NULL == (ems->elan_rail = (ELAN_RAIL **)
                 malloc (sizeof (ELAN_RAIL **) * (num_rails + 1)))) {
	ompi_output(0, 
		"[%s:%d] error in allocating memory for elan_rail \n",
		__FILE__, __LINE__);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    ems->elan_rail[num_rails] = NULL;

    alloc_mainsize = ELAN_ALIGNUP (ems->main_size, ems->elan_pagesize);
    alloc_mainbase = (ADDR_ELAN) ((uintptr_t) ems->main_base);
    alloc_elansize = ELAN_ALIGNUP (ems->elan_size, ems->elan_pagesize);
    alloc_elanbase = (ADDR_ELAN) ((uintptr_t) ems->elan_base);

    /* Magic quadrics number for the starting cookie value */
    ems->intcookie = 42;

    for (i = 0; i < num_rails; i++) {

        RAIL *rail;
        ELAN_ESTATE *estate;
        ELAN_EPRIVSTATE *priv_estate;

        /* Allocate the Main memory control structure for this rail */
        if (NULL == (rail = ems->all_rails[i] = 
		    (RAIL *) malloc (sizeof (RAIL)))) {
	    ompi_output(0, 
		    "[%s:%d] error in allocating memory for all_rails[i]\n",
		    __FILE__, __LINE__);
            return OMPI_ERROR;
        }
        memset (rail, 0, sizeof (RAIL));

        if (NULL == (rail->r_ctx = elan4_init (rails[i]))) {
	    ompi_output(0, 
		    "[%s:%d] error in initializing rail %d \n",
		    __FILE__, __LINE__, rails[i]);
            return OMPI_ERROR;
        }

        if (NULL == (rail->r_sdram = elan4_open_sdram (rails[i],
                                                       0,
                                                       alloc_elansize))) {
	    ompi_output(0, 
		    "[%s:%d] error opening sdram for rail %d \n",
		    __FILE__, __LINE__, rails[i]);
            return OMPI_ERROR;
        }

        if (NULL == (rail->r_alloc =
                     elan4_createAllocator (ems->main_size,
                                            rail->r_sdram, 0,
                                            ems->elan_size))) {
	    ompi_output(0, 
		    "[%s:%d] error creating allocator for rail %d \n",
		    __FILE__, __LINE__, rails[i]);
            return OMPI_ERROR;
        }

        if (elan4_set_standard_mappings (rail->r_ctx) < 0
            || elan4_set_required_mappings (rail->r_ctx) < 0) {
	    ompi_output(0, 
		    "[%s:%d] error setting memory mapping for rail %d \n",
		    __FILE__, __LINE__, rails[i]);
            return OMPI_ERROR;
        }

        /* Now allocate the SDRAM Elan control structure for this rail */
        if (NULL == (estate = ems->all_estates[i] =
                     elan4_allocElan(rail->r_alloc, ELAN_ALIGN,
                                 sizeof (ELAN_EPRIVSTATE)))) {
	    ompi_output(0, 
		    "[%s:%d] error in allocating memory "
		    "for estate from rail %d \n",
		    __FILE__, __LINE__, rails[i]);
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
	    ompi_output(0, 
		    "[%s:%d] error in allocating command port "
		    "for rail %d \n",
		    __FILE__, __LINE__, rails[i]);
            return OMPI_ERROR;
        }

        /* Allocate a command port for thread rescheduling etc */
        if (NULL == (rail->r_ecmdq = elan4_alloc_cmdq (rail->r_ctx,
                                               rail->r_alloc, CQ_Size8K,
                                               CQ_EnableAllBits,
                                               NULL))) {
	    ompi_output(0, 
		    "[%s:%d] error in allocating thread command port "
		    "for rail %d \n",
		    __FILE__, __LINE__, rails[i]);
            return OMPI_ERROR;
        }

	priv_estate->cport = MAIN2ELAN(rail->r_ctx,
		rail->r_ecmdq->cmdq_mapping);

        /* save the rail pointers */
        ems->elan_rail[i] = (ELAN_RAIL *) rail;

	estate->alloc = rail->r_alloc;
	estate->vp    = ems->elan_vp;
	estate->debugFlags = ems->elan_flags;
	estate->debugFd = 1;
	priv_estate->pageSize = ems->elan_pagesize;

        rail->r_estate = estate;
        rail->r_railNo = rails[i];

        {
	    /*ompi_elan_railtable_t *rt;*/
	    struct railtable   *rt;	
            if (NULL == (rt = (struct railtable *) 
			malloc (sizeof (struct railtable)))) {
	    ompi_output(0, 
		    "[%s:%d] error in allocating memory for railTable \n"
		    __FILE__, __LINE__);
                return OMPI_ERROR;
            }
            memset (rt, 0, sizeof (struct railtable));

            rt->rt_nrails = 1;
            rt->rt_rail = 0;
            rt->rt_railReal = i;
            rt->rt_allRails = (RAIL **) &(ems->all_rails[i]);
            rail->r_railTable = rt;
        }
    }                           /* for each rail */

    /* Free the local variable */
    free (rails);

    ems->elan_ctx = ems->elan_rail[0]->rail_ctx;
    ems->elan_estate = (void *) ems->all_estates[0];

#if 0
    /* Leave the junky code here to remind me later */
    _elan_eventInit(privState);
    elan_setDebugHandler(state, (ELAN_DBGH)_elan_allocDbg, state);
    atexit(_elan_atExitCallBack);
#endif

    ompi_elan_attach_network (ems);

    /* Set the rms_resourceId */
    if (rms_getprgid (getpid (), &ems->elan_rmsid) < 0) {
        ems->elan_rmsid = -1;
    }

    /* Now open ourselves to the network */
    for (i = 0; ems->elan_rail[i]; i++) {
        elan4_block_inputter (ems->elan_rail[i]->rail_ctx, 0);
    }

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
 */
static int
ompi_elan_attach_network (mca_ptl_elan_state_t * ems)
{
    static int  elan_attached = 0;
    int         i, vp, *vps, num_rails;
    ELAN_LOCATION loc;
    ELAN_CAPABILITY *cap = ems->elan_cap;

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
	    ompi_output(0, 
		    "[%s:%d] error in adding vp to elan capability \n",
		    __FILE__, __LINE__);
            return OMPI_ERROR;
        }

        /* block the inputter until we are really ready */
        elan4_block_inputter (rail->r_ctx, 1);

        if (elan4_attach (rail->r_ctx, cap)) {
	    ompi_output(0, 
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
        rail->r_cpool =
            elan4_allocCookiePool (rail->r_ctx, ems->elan_vp);
        ((ELAN_EPRIVSTATE *) rail->r_estate)->tcookie =
            rail->r_cpool->cp_cookie;
    }

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

    /* Determine the number of processes described by the capability */
    ems->elan_nvp = elan_nvps (ems->elan_cap);

    if (ems->elan_vp >= ems->elan_nvp) {
	ompi_output(0, 
		"[%s:%d] error getting vp and nvp from capability \n",
		__FILE__, __LINE__);
	return OMPI_ERROR;
    }

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
	ompi_output(0, 
		"[%s:%d] error in malloc for vps \n",
		__FILE__, __LINE__);
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
