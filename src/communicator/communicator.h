/*
 * $HEADER$
 */

#ifndef LAM_COMMUNICATOR_H
#define LAM_COMMUNICATOR_H

#include "lfc/lam_object.h"
#include "errhandler/errhandler.h"
#include "threads/mutex.h"
#include "util/output.h"
#include "mpi.h"
#include "group/group.h"
#include "mca/coll/coll.h"
#include "lfc/lam_hash_table.h"
#include "attribute/attribute.h"

extern lam_class_t lam_communicator_t_class;

#define LAM_COMM_INTER     0x00000001
#define LAM_COMM_CART      0x00000002
#define LAM_COMM_GRAPH     0x00000004
#define LAM_COMM_NAMEISSET 0x00000008
#define LAM_COMM_ISFREED   0x00000010

/* modes reqquired for accquiring the new comm-id */
#define LAM_COMM_INTRA_INTRA 0x00000020
#define LAM_COMM_INTRA_INTER 0x00000040
#define LAM_COMM_INTER_INTRA 0x00000080
#define LAM_COMM_INTER_INTER 0x00000100

struct lam_communicator_t {
    lam_object_t               c_base; 
    char  c_name[MPI_MAX_OBJECT_NAME];
    uint32_t              c_contextid;
    int                     c_my_rank;
    uint32_t                  c_flags; /* flags, e.g. intercomm, 
                                          topology, etc. */

    lam_group_t        *c_local_group;
    lam_group_t       *c_remote_group;
  
    /* Attributes */
    lam_hash_table_t       *c_keyhash;

    /* Topology information */
    int c_cube_dim; /**< Inscribing cube dimension */
    int c_topo_type; /**< Topology type */
    int c_topo_nprocs; /**< Number of processes */
    int c_topo_ndims; /**< Number of cart dimensions */
    int c_topo_nedges; /**< Graph edges */
    int *c_topo_dims; /**< Cart dimensions */
    int *c_topo_coords; /**< Cart coordinates */
    int *c_topo_index; /**< Graph indices */
    int *c_topo_edges; /**< Graph edges */

    /* index in Fortran <-> C translation array */

    int c_f_to_c_index;

    /* Error handling.  This field does not have the "c_" prefix so
       that the LAM_ERRHDL_* macros can find it, regardless of whether
       it's a comm, window, or file. */

    lam_errhandler_t *error_handler;

    /* Hooks for PML to hang things */
    struct mca_pml_comm_t* c_pml_comm;

    /* Hooks for collectives to hang things */

    mca_coll_1_0_0_t c_coll;
    struct mca_coll_comm_t* c_coll_comm;
};
typedef struct lam_communicator_t lam_communicator_t;


/**
 * is this a valid communicator
 */
static inline int lam_comm_invalid(lam_communicator_t* comm)
{
    return false;
}
/**
 * rank w/in the communicator
 */
static inline int lam_comm_rank(lam_communicator_t* comm)
{
    return comm->c_my_rank;
}
/**
 * size of the communicator
 */
static inline int lam_comm_size(lam_communicator_t* comm)
{
    return comm->c_local_group->grp_proc_count;
}

/**
 * size of the remote group for inter-communicators.
 * returns zero for an intra-communicator
 */
static inline int lam_comm_remote_size(lam_communicator_t* comm)
{
    if ( comm->c_flags & LAM_COMM_INTER ) 
        return comm->c_remote_group->grp_proc_count;
    else 
        return 0;
}

/* return pointer to communicator associated with context id cid,
 * No error checking is done*/
static inline lam_communicator_t *lam_comm_lookup(uint32_t cid) 
{ 
    /* array of pointers to communicators, indexed by context ID */
    extern lam_pointer_array_t lam_mpi_communicators;
    return (lam_communicator_t*)lam_pointer_array_get_item(&lam_mpi_communicators, cid);
}

static inline lam_proc_t* lam_comm_peer_lookup(lam_communicator_t* comm, int peer_id)
{
#if LAM_ENABLE_DEBUG
    if(peer_id >= comm->c_remote_group->grp_proc_count) {
        lam_output(0, "lam_comm_lookup_peer: invalid peer index (%d)", peer_id);
        return (lam_proc_t *) NULL;
    }
#endif
    return comm->c_remote_group->grp_proc_pointers[peer_id];
}

static inline bool lam_comm_peer_invalid(lam_communicator_t* comm, int peer_id)
{
    if(peer_id < 0 || peer_id >= comm->c_remote_group->grp_proc_count) {
        return true;
    }
    return false;
}



#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

    /** 
     * Initialise MPI_COMM_WORLD and MPI_COMM_SELF 
     */
    int lam_comm_init(void);
    int lam_comm_link_function(void);

    /** 
     * extract the local group from a communicator 
     */
    int lam_comm_group ( lam_communicator_t *comm, lam_group_t **group );

    /**
     * create a communicator based on a group 
     */
    int lam_comm_create ( lam_communicator_t* comm, lam_group_t *group, 
                          lam_communicator_t** newcomm );

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
    int lam_comm_split ( lam_communicator_t *comm, int color, int key, 
                         lam_communicator_t** newcomm );
    
    /**
     * free a communicator 
     */
    int lam_comm_free ( lam_communicator_t **comm );

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
    lam_communicator_t* lam_comm_allocate ( int local_group_size, 
                                            int remote_group_size );

    /**
     * allocate new communicator ID
     * @param mode: combination of input and output communicator
     *              LAM_COMM_INTRA_INTRA, LAM_COMM_INTRA_INTER,
     *              LAM_COMM_INTER_INTRA, LAM_COMM_INTER_INTER
     *
     * This routine has to be thread safe in the final version.
     */
    int lam_comm_nextcid (lam_communicator_t* comm, int mode);


    /**
     * shut down the communicator infrastructure.
     */
    int lam_comm_finalize (void);
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* LAM_COMMUNICATOR_H */
