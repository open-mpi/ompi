/*
 * $HEADER$
 */

#ifndef LAM_COMMUNICATOR_H
#define LAM_COMMUNICATOR_H

#include "stdint.h"
#include "lfc/lam_object.h"
#include "threads/mutex.h"
#include "util/output.h"
#include "mpi.h"
#include "group/group.h"
#include "mca/coll/coll.h"
#include "lfc/lam_hash_table.h"


extern lam_class_t lam_communicator_t_class;


struct lam_communicator_t {
    lam_object_t c_base;
    char c_name[MPI_MAX_OBJECT_NAME];
    uint32_t c_contextid;
    int c_my_rank;

    lam_group_t *c_local_group;
    lam_group_t *c_remote_group;
  
    /* Attributes */
    lam_hash_table_t *keyhash;

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

    /* Error handling */

    MPI_Errhandler c_error_handler;

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
    return comm->c_remote_group->grp_proc_count;
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
    int lam_comm_init(void);
    int lam_comm_link_function(void);
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* LAM_COMMUNICATOR_H */
