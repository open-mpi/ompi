/*
 * $HEADER$
 */

#ifndef OMPI_COMMUNICATOR_H
#define OMPI_COMMUNICATOR_H

#include "class/ompi_object.h"
#include "errhandler/errhandler.h"
#include "threads/mutex.h"
#include "util/output.h"
#include "mpi.h"
#include "group/group.h"
#include "mca/coll/coll.h"
#include "mca/topo/topo.h"
#include "class/ompi_hash_table.h"
#include "attribute/attribute.h"
#include "request/request.h"

extern ompi_class_t ompi_communicator_t_class;

#define OMPI_COMM_INTER     0x00000001
#define OMPI_COMM_CART      0x00000002
#define OMPI_COMM_GRAPH     0x00000004
#define OMPI_COMM_NAMEISSET 0x00000008
#define OMPI_COMM_ISFREED   0x00000010
#define OMPI_COMM_INTRINSIC 0x00000020

/* some utility #defines */
#define OMPI_COMM_IS_INTER(comm) ((comm)->c_flags & OMPI_COMM_INTER)
#define OMPI_COMM_IS_INTRA(comm) (!((comm)->c_flags & OMPI_COMM_INTER))
#define OMPI_COMM_IS_CART(comm) ((comm)->c_flags & OMPI_COMM_CART)
#define OMPI_COMM_IS_GRAPH(comm) ((comm)->c_flags & OMPI_COMM_GRAPH)
#define OMPI_COMM_IS_INTRINSIC(comm) ((comm)->c_flags & OMPI_COMM_INTRINSIC)

/** 
 * Modes reqquired for accquiring the new comm-id.
 * The first (INTER/INTRA) indicates whether the
 * input comm was an inter/intra-comm, the second
 * whether the new communicator will be an inter/intra 
 *comm
 */
#define OMPI_COMM_INTRA_INTRA 0x00000020
#define OMPI_COMM_INTRA_INTER 0x00000040
#define OMPI_COMM_INTER_INTRA 0x00000080
#define OMPI_COMM_INTER_INTER 0x00000100

extern ompi_pointer_array_t ompi_mpi_communicators; 

struct ompi_communicator_t {
    ompi_object_t               c_base; 
    char  c_name[MPI_MAX_OBJECT_NAME];
    uint32_t              c_contextid;
    int                     c_my_rank;
    uint32_t                  c_flags; /* flags, e.g. intercomm, 
                                          topology, etc. */

    ompi_group_t        *c_local_group;
    ompi_group_t       *c_remote_group;
  
    /* Attributes */
    ompi_hash_table_t       *c_keyhash;

    int c_cube_dim; /**< inscribing cube dimension */

    /* Hooks for topo module to hang things */
    mca_topo_1_0_0_t c_topo; /**< structure of function pointers */
    mca_topo_comm_t *c_topo_comm; /**<structure containing information
                                    *about the topology */

    /* index in Fortran <-> C translation array */

    int c_f_to_c_index;

    /* Error handling.  This field does not have the "c_" prefix so
       that the OMPI_ERRHDL_* macros can find it, regardless of whether
       it's a comm, window, or file. */

    ompi_errhandler_t *error_handler;

    /* Hooks for PML to hang things */
    struct mca_pml_comm_t* c_pml_comm;

    /* Hooks for collectives to hang things */

    mca_coll_1_0_0_t c_coll;
    struct mca_coll_comm_t* c_coll_comm;

    /* VPS: This will be moved in the coll module later on */
    ompi_request_t **bcast_lin_reqs;
    ompi_request_t **bcast_log_reqs;

};
typedef struct ompi_communicator_t ompi_communicator_t;


/**
 * is this a valid communicator
 */
static inline int ompi_comm_invalid(ompi_communicator_t* comm)
{
    if ( comm->c_flags & OMPI_COMM_ISFREED ) 
        return true;
    else
        return false;
}

/**
 * rank w/in the communicator
 */
static inline int ompi_comm_rank(ompi_communicator_t* comm)
{
    return comm->c_my_rank;
}
/**
 * size of the communicator
 */
static inline int ompi_comm_size(ompi_communicator_t* comm)
{
    return comm->c_local_group->grp_proc_count;
}

/**
 * size of the remote group for inter-communicators.
 * returns zero for an intra-communicator
 */
static inline int ompi_comm_remote_size(ompi_communicator_t* comm)
{
    if ( comm->c_flags & OMPI_COMM_INTER ) 
        return comm->c_remote_group->grp_proc_count;
    else 
        return 0;
}

/* return pointer to communicator associated with context id cid,
 * No error checking is done*/
static inline ompi_communicator_t *ompi_comm_lookup(uint32_t cid) 
{ 
    /* array of pointers to communicators, indexed by context ID */
    extern ompi_pointer_array_t ompi_mpi_communicators;
    return (ompi_communicator_t*)ompi_pointer_array_get_item(&ompi_mpi_communicators, cid);
}

static inline ompi_proc_t* ompi_comm_peer_lookup(ompi_communicator_t* comm, int peer_id)
{
#if OMPI_ENABLE_DEBUG
    if(peer_id >= comm->c_remote_group->grp_proc_count) {
        ompi_output(0, "ompi_comm_lookup_peer: invalid peer index (%d)", peer_id);
        return (ompi_proc_t *) NULL;
    }
#endif
    return comm->c_remote_group->grp_proc_pointers[peer_id];
}

static inline bool ompi_comm_peer_invalid(ompi_communicator_t* comm, int peer_id)
{
    if(peer_id < 0 || peer_id >= comm->c_remote_group->grp_proc_count) {
        return true;
    }
    return false;
}

static inline int ompi_cube_dim(int nprocs) {
    int dim;
    size_t size;

    if (1 > nprocs) return OMPI_ERROR;
    for(dim = 0, size = 1; size < (size_t)nprocs; ++dim, size <<= 1);

    return dim;
}


#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

    /** 
     * Initialise MPI_COMM_WORLD and MPI_COMM_SELF 
     */
    int ompi_comm_init(void);
    int ompi_comm_link_function(void);

    /** 
     * extract the local group from a communicator 
     */
    int ompi_comm_group ( ompi_communicator_t *comm, ompi_group_t **group );

    /**
     * create a communicator based on a group 
     */
    int ompi_comm_create ( ompi_communicator_t* comm, ompi_group_t *group, 
                          ompi_communicator_t** newcomm );

    /**
     * split a communicator based on color and key. Parameters
     * are identical to the MPI-counterpart of the function.
     * 
     * @param comm: input communicator
     * @param color
     * @param key
     *
     * @
     */
    int ompi_comm_split ( ompi_communicator_t *comm, int color, int key, 
                         ompi_communicator_t** newcomm );
    
    /**
     * free a communicator 
     */
    int ompi_comm_free ( ompi_communicator_t **comm );

    /**
     * allocate a new communicator structure 
     * @param local_group_size
     * @param remote_group_size
     *
     * this routine allocates the structure, the according local and
     * remote groups, the proc-arrays in the local and remote group.
     * It furthermore sets the fortran index correctly, 
     * and sets all other elements to zero.
     */
    ompi_communicator_t* ompi_comm_allocate ( int local_group_size, 
                                            int remote_group_size );

    /**
     * allocate new communicator ID
     * @param comm:       original comm
     * @param bridgecomm: bridge comm for intercomm_create
     * @param mode: combination of input and output communicator
     *              OMPI_COMM_INTRA_INTRA, OMPI_COMM_INTRA_INTER,
     *              OMPI_COMM_INTER_INTRA, OMPI_COMM_INTER_INTER
     *
     * This routine has to be thread safe in the final version.
     */
    int ompi_comm_nextcid ( ompi_communicator_t* comm, 
                           ompi_communicator_t* bridgecomm, 
                           int local_leader, 
                           int remote_leader, 
                           int mode);


    /**
     * shut down the communicator infrastructure.
     */
    int ompi_comm_finalize (void);

    /**
     * This is THE routine, where all the communicator stuff
     * is really set.
     */
    ompi_communicator_t* ompi_comm_set ( int mode,
                                       ompi_communicator_t* oldcomm,
                                       ompi_communicator_t* bridgecomm,
                                       int local_size, 
                                       ompi_proc_t **local_procs,
                                       int remote_size,
                                       ompi_proc_t **remote_procs,
                                       ompi_hash_table_t *attr,
                                       ompi_errhandler_t *errh, 
                                       mca_base_module_t *collmodule, 
                                       mca_base_module_t *topomodule, 
                                       int local_leader,
                                       int remote_leader);
    /**
     * This is a short-hand routine used in intercomm_create.
     * The routine makes sure, that all processes have afterwards
     * a list of ompi_proc_t pointers for the remote group.
     */
    ompi_proc_t **ompi_comm_get_rprocs ( ompi_communicator_t *local_comm, 
                                       ompi_communicator_t *bridge_comm, 
                                       int local_leader,
                                       int remote_leader,
                                       int tag,
                                       int rsize);

    /**
     * This is a routine determining whether the local or the
     * remote group will be first in the new intra-comm.
     * Just used from within MPI_Intercomm_merge.
     */
    int ompi_comm_determine_first ( ompi_communicator_t *intercomm,
                                   int high );

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* OMPI_COMMUNICATOR_H */
