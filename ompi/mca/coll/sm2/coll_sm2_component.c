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

#include "ompi/constants.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/coll/coll.h"
#include "opal/util/show_help.h"
#include "coll_sm2.h"
#include "ompi/mca/coll/base/base.h"


/*
 * Public string showing the coll ompi_sm V2 component version number
 */
const char *mca_coll_sm_component_version_string =
    "Open MPI sm-V2 collective MCA component version " OMPI_VERSION;


/*
 * Local functions
 */
static int sm2_module_enable(struct mca_coll_base_module_1_1_0_t *module,
        struct ompi_communicator_t *comm);

/*
 * Local functions
 */

static int sm2_open(void);
static int sm2_close(void);

static bool have_local_peers(ompi_group_t *group, size_t size)
{
    size_t i;
    ompi_proc_t *proc;

    for (i = 0; i < size; ++i) {
        proc = ompi_group_peer_lookup(group,i);
        if (0 == (proc->proc_flags & OMPI_PROC_FLAG_LOCAL)) {
            return false;
        }
    }
    return true;
}

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
    /* JMS temporarily lowered until we can get more testing */
    0,

};


static void                
mca_coll_sm2_module_construct(mca_coll_sm2_module_t *module)
{
    /* debug */
    fprintf(stderr," sm2 constructor called \n");
    fflush(stderr);
    /* end debug */
}

static void                
mca_coll_sm2_module_destruct(mca_coll_sm2_module_t *module)
{
    /* debug */
    fprintf(stderr," sm2 destructor called \n");
    fflush(stderr);
    /* end debug */
}


/*
 * Open the component
 */
static int sm2_open(void)
{
    /* local variables */
    int size;
    mca_coll_sm2_component_t *cs = &mca_coll_sm2_component;

    /* set component priority */
    cs->sm2_priority=
        mca_coll_sm2_param_register_int("sm_priority",0);

    /* set control region size (bytes), per proc */
    cs->sm2_ctl_size_per_proc=
        mca_coll_sm2_param_register_int("sm2_ctl_size_per_proc",sizeof(int));

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
        mca_coll_sm2_param_register_int("sm2_max_data_seg_size",8*getpagesize());

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


/* query to see if the module is available for use on the given
 * communicator, and if so, what it's priority is.  This is where
 * the backing shared-memory file is created.
 */
struct mca_coll_base_module_1_1_0_t *
mca_coll_sm2_comm_query(struct ompi_communicator_t *comm, int *priority)
{
    /* local variables */
    mca_coll_sm2_module_t *sm_module;
    size_t coll_sm2_comm_backing_file_size=0;
    size_t coll_sm2_per_proc_segment_size=0;
    int group_size;
    size_t alignment,size,size_tot,size_tot_per_proc_per_seg;
    size_t tot_size_per_bank,size_tot_per_segment;
    size_t tot_size_mem_banks;
    size_t ctl_memory_per_proc_per_segment;
    size_t mem_management_per_proc_per_block;
    size_t mem_management_per_proc;
    size_t mem_management_total;
    size_t size_sm2_backing_file;

    /*
     * This is activated only for intra-communicators
     */
    if (OMPI_COMM_IS_INTER(comm) ) {
        return NULL;
    }

    /*
     * Use only if more than on proc in the communicator
     */
    if (1 == ompi_comm_size(comm) ) {
        return NULL;
    }

    /* check to see if all procs are on the same node, and therefore
     *   can communicate using shared memory
     */
    if ( !have_local_peers(comm->c_local_group, ompi_comm_size(comm))) {
        return NULL;
    }

    /* Get our priority */
    *priority = mca_coll_sm2_component.sm2_priority;

    /* allocate and initialize an sm-v2  module */
    sm_module = OBJ_NEW(mca_coll_sm2_module_t);

    sm_module->super.coll_module_enable = sm2_module_enable;
    sm_module->super.ft_event        = NULL;
    sm_module->super.coll_allgather  = NULL;
    sm_module->super.coll_allgatherv = NULL;
    sm_module->super.coll_allreduce  = NULL;
    sm_module->super.coll_alltoall   = NULL;
    sm_module->super.coll_alltoallv  = NULL;
    sm_module->super.coll_alltoallw  = NULL;
    sm_module->super.coll_barrier    = NULL;
    sm_module->super.coll_bcast      = NULL;
    sm_module->super.coll_exscan     = NULL;
    sm_module->super.coll_gather     = NULL;
    sm_module->super.coll_gatherv    = NULL;
    sm_module->super.coll_reduce     = NULL;
    sm_module->super.coll_reduce_scatter = NULL;
    sm_module->super.coll_scan       = NULL;
    sm_module->super.coll_scatter    = NULL;
    sm_module->super.coll_scatterv   = NULL;

    /* 
     * create backing file 
     */

    /*
     * set group size
     */
    group_size=ompi_comm_size(comm);

    /* 
     * get control region size 
     */ 
    /* just enough place for one flag per process */
    ctl_memory_per_proc_per_segment=sizeof(int);
    if( mca_coll_sm2_component.sm2_ctl_size_per_proc > ctl_memory_per_proc_per_segment )
        ctl_memory_per_proc_per_segment=mca_coll_sm2_component.sm2_ctl_size_per_proc;
    ctl_memory_per_proc_per_segment=ctl_memory_per_proc_per_segment * group_size ;
   
    /* pad this up to the alignment needed by the data segment, as the
     * that data segment will directly follow the control segment in
     * memory.
     */
    alignment=mca_coll_sm2_component.sm2_data_alignment;
    ctl_memory_per_proc_per_segment=
        (alignment + ctl_memory_per_proc_per_segment -1) / alignment;
    ctl_memory_per_proc_per_segment*=alignment;
    mca_coll_sm2_component.sm2_ctl_size_allocated=ctl_memory_per_proc_per_segment;

    /* get data region size - allocation happens on a page granularity, with
     * a minimum of a page allocated per proc, so adjust to this
     */
    size=mca_coll_sm2_component.sm2_data_seg_size;
    if( size > mca_coll_sm2_component.sm2_max_data_seg_size )
        size=mca_coll_sm2_component.sm2_max_data_seg_size;
    size_tot_per_proc_per_seg=size+ mca_coll_sm2_component.sm2_ctl_size_allocated;
    if( size_tot_per_proc_per_seg < getpagesize())
        size_tot_per_proc_per_seg=getpagesize();
    /* round this up to the nearest integer page-size multiple */
    size_tot_per_proc_per_seg= ( size_tot_per_proc_per_seg + getpagesize() - 1)/
        getpagesize();
    size_tot_per_proc_per_seg*=getpagesize();

    /* compute segment memory needed */
    size_tot_per_segment=group_size * size_tot_per_proc_per_seg ;

    /* compute memory per bank */
    tot_size_per_bank=size_tot_per_segment*mca_coll_sm2_component.sm2_num_regions_per_bank;

    /* compute total memory in the memory banks */
    tot_size_mem_banks=tot_size_per_bank*mca_coll_sm2_component.sm2_num_mem_banks;

    /* compute the amount of memory needed for the anynchromous barriers used to
     *   manage the memory resources.
     */
    /* for each bank, 2 sets of barrier buffers */
    mem_management_per_proc_per_block= 2 * CACHE_LINE_SIZE ;
    /* add in number of banks */
    mem_management_per_proc= mem_management_per_proc_per_block *
        mca_coll_sm2_component.sm2_num_mem_banks;
    /* round up to page multiples */
    mem_management_per_proc=(mem_management_per_proc +
            getpagesize() -1 ) / getpagesize();
    mem_management_per_proc*=getpagesize();

    /* total memory management required */
    mem_management_total=mem_management_per_proc * group_size;

    /* total size of backing file */
    size_sm2_backing_file=mem_management_total+tot_size_mem_banks;

    /* allocate backing file */

    /* initialize local counters */

    /* set pointers */

    /* touch pages to apply memory affinity - Note: do we really need this or will
     * the algorithms do this */

    /* return */
    return &(sm_module->super);

}

/*
    * Init module on the communicator
     */
static int
sm2_module_enable(struct mca_coll_base_module_1_1_0_t *module,
                         struct ompi_communicator_t *comm)
{
    /* local variables */
    char output_buffer[2*MPI_MAX_OBJECT_NAME];

    memset(&output_buffer[0],0,sizeof(output_buffer));
    snprintf(output_buffer,sizeof(output_buffer),"%s (cid %d)", comm->c_name,
                       comm->c_contextid);
    opal_output_verbose(10, mca_coll_base_output,
            "coll:sm2:enable: new communicator: %s", output_buffer);

    /* All done */
    return OMPI_SUCCESS;
}


OBJ_CLASS_INSTANCE(mca_coll_sm2_module_t,
                   mca_coll_base_module_1_1_0_t,
                   mca_coll_sm2_module_construct,
                   mca_coll_sm2_module_destruct);
