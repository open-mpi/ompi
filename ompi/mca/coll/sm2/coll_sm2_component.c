/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 * @file
 *
 * Most of the description of the data layout is in the
 * coll_sm_module.c file.
 */

#include "ompi_config.h"
#include <unistd.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <fcntl.h>

#include "ompi/constants.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/coll/coll.h"
#include "opal/util/show_help.h"
#include "coll_sm2.h"
#include "ompi/mca/coll/base/base.h"
#include "orte/mca/rml/rml.h"

/* debug */
#include <signal.h>

extern int debug_print;
extern int my_debug_rank;
extern void debug_module(void);

void dbg_handler(int my_signal) {
/* debug_print=1; */
 debug_module();
 return;
}
/* end debug */

/*
 * Public string showing the coll ompi_sm V2 component version number
 */
const char *mca_coll_sm2_component_version_string =
    "Open MPI sm-V2 collective MCA component version " OMPI_VERSION;


/*
 * Local functions
 */

static int sm2_open(void);
static int sm2_close(void);

static inline int mca_coll_sm2_param_register_int(
        const char* param_name, int default_value)
{
    int id = mca_base_param_register_int("coll","sm2",param_name,NULL,default_value);
    int param_value = default_value;
    mca_base_param_lookup_int(id,&param_value);
    return param_value;
}

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

mca_coll_sm2_component_t mca_coll_sm2_component = {

    /* First, fill in the super (mca_coll_base_component_1_1_0_t) */

    {
        /* First, the mca_component_t struct containing meta
           information about the component itself */
        
        {
            /* Indicate that we are a coll v1.1.0 component (which
               also implies a specific MCA version) */

            MCA_COLL_BASE_VERSION_1_1_0,

            /* Component name and version */

            "sm-v2",
            OMPI_MAJOR_VERSION,
            OMPI_MINOR_VERSION,
            OMPI_RELEASE_VERSION,

            /* Component open and close functions */

            sm2_open,
            sm2_close,
        },
        
        /* Next the MCA v1.1.0 component meta data */

        {
            /* The component is not checkpoint ready */
            MCA_BASE_METADATA_PARAM_NONE
        },

        /* Initialization / querying functions */
        
        mca_coll_sm2_init_query,
        mca_coll_sm2_comm_query,
    },

    /* sm-component specifc information */

    /* (default) priority */
    0,

};

/*
 * Open the component
 */
static int sm2_open(void)
{
/* debug */
    int retVal;
    struct sigaction new_sigact;
/* end debug */

    /* local variables */
    mca_coll_sm2_component_t *cs = &mca_coll_sm2_component;

    /* set component priority */
    cs->sm2_priority=
        mca_coll_sm2_param_register_int("sm2_priority",0);

    /* set control region size (bytes), per proc */
    cs->sm2_ctl_size_per_proc=
        mca_coll_sm2_param_register_int("sm2_ctl_size_per_proc",2*sizeof(long long));

    /* initialize control region allocted */
    cs->sm2_ctl_size_allocated=0;

    /* set control region alignment (bytes) */
    cs->sm2_ctl_alignment=
        mca_coll_sm2_param_register_int("sm2_ctl_alignment",getpagesize());

    /* Min data Segment size (bytes) - per proc */
    cs->sm2_data_seg_size=
        mca_coll_sm2_param_register_int("sm2_data_seg_size",0);

    /* Max data Segment size (bytes) - per proc */
    cs->sm2_max_data_seg_size=
        mca_coll_sm2_param_register_int("sm2_max_data_seg_size",20*getpagesize());

    /* initialize control region allocted */
    cs->sm2_data_size_allocated=0;

    /* Data region alignment (bytes) - per proc */
    cs->sm2_data_alignment=
        mca_coll_sm2_param_register_int("sm2_data_alignment",CACHE_LINE_SIZE);

    /* Number of memory banks */
    cs->sm2_num_mem_banks=
        mca_coll_sm2_param_register_int("sm2_num_mem_banks",2);

    /* Number of regions per memory bank */
    cs->sm2_num_regions_per_bank=
        mca_coll_sm2_param_register_int("sm2_num_regions_per_bank",8);

    /* Order of buffer management  Barrier Tree */
    cs->order_barrier_tree=
        mca_coll_sm2_param_register_int("order_barrier_tree",2);

    /* Order of reduction Tree */
    cs->order_reduction_tree=
        mca_coll_sm2_param_register_int("order_reduction_tree",2);

    /* Order of fan-out read Tree */
    cs->order_fanout_read_tree=
        mca_coll_sm2_param_register_int("order_fanout_read_tree",4);

    /* number of polling loops to allow pending resources to
     * complete their work
     */
    cs->n_poll_loops=
        mca_coll_sm2_param_register_int("n_poll_loops",4);

/* debug */
    /*
    new_sigact.sa_handler=dbg_handler;
    sigemptyset(&(new_sigact.sa_mask));

    retVal=sigaction(SIGUSR2,&new_sigact,NULL);
    */
    signal(SIGUSR2,dbg_handler);
/* end debug */

    return OMPI_SUCCESS;
}


/*
 * Close the component
 */
static int sm2_close(void)
{
    return OMPI_SUCCESS;
}

/* query to see if the component is available for use, and can 
 * satisfy the thread and progress requirements 
 */
int mca_coll_sm2_init_query(bool enable_progress_threads,
        bool enable_mpi_threads)
{
    /* at this stage there is no reason to disaulify this component */

    /* done */
    return OMPI_SUCCESS;
}
