/* 
 * $HEADER 
 */

#ifndef MCA_COLL_SM_EXPORT_H
#define MCA_COLL_SM_EXPORT_H

#include "ompi_config.h"

#include "mpi.h"
#include "mca/mca.h"
#include "mca/coll/coll.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

#define PUB(foo) mca_coll_sm##foo

/* Structure for sm collective module, per communicator. The structure 
   mainly stores memory pointers to the specific poritions in the shared
   memory area. Each shared memory area is reserved for special functions.
   The shared memory is split between two types of areas. One is control 
   section that stores shared flags used during synchronization, while other
   section is purely used to pass messages from one process to other. */

typedef struct mca_coll_base_module_comm_t {

    /* VPS: SM collective specific data to be cached on each comm goes
       here */

} mca_coll_base_module_comm_t;


/*
 * Globally exported variables
 */

extern const mca_coll_base_component_1_0_0_t mca_coll_sm_component;


    /*
     * coll API functions
     */

    int mca_coll_sm_open(void);
    int mca_coll_sm_close(void);

    int mca_coll_sm_init_query(bool *allow_multi_user_threads,
			       bool *have_hidden_threads);

    const struct mca_coll_base_module_1_0_0_t *
    mca_coll_sm_comm_query(struct ompi_communicator_t *comm, int *priority);

    int mca_coll_sm_comm_unquery(struct ompi_communicator_t *comm);

    const struct mca_coll_base_module_1_0_0_t *
    mca_coll_sm_module_init(struct ompi_communicator_t *comm);

    int mca_coll_sm_module_finalize(struct ompi_communicator_t *comm);

  
    int mca_coll_sm_barrier(struct ompi_communicator_t *comm);
    int mca_coll_sm_bcast(void *buff, int count, 
			  struct ompi_datatype_t *datatype, int root, 
			  struct ompi_communicator_t *comm);
    int mca_coll_sm_scatter(void *sbuf, int scount,
			    struct ompi_datatype_t *sdtype, void *rbuf,
			    int rcount, struct ompi_datatype_t *rdtype,
			    int root, struct ompi_communicator_t *comm);
    int mca_coll_sm_reduce(void *sbuf, void* rbuf, int count, 
			   struct ompi_datatype_t *dtype, 
			   struct ompi_op_t *op, 
			   int root, struct ompi_communicator_t *comm);
    int mca_coll_sm_gather(void *sbuf, int scount,
			   struct ompi_datatype_t *sdtype, void *rbuf,
			   int rcount, struct ompi_datatype_t *rdtype,
			   int root, struct ompi_communicator_t *comm);
    int mca_coll_sm_alltoall(void *sbuf, int scount,
			     struct ompi_datatype_t *sdtype, void *rbuf,
			     int rcount, struct ompi_datatype_t *rdtype,
			     struct ompi_communicator_t *comm);
    int mca_coll_sm_allgather(void *sbuf, int scount,
			      struct ompi_datatype_t *sdtype, void *rbuf,
			      int rcount, struct ompi_datatype_t *rdtype,
			      struct ompi_communicator_t *comm);

    /* VPS: Any other module utility function prototypes can go in here */

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* MCA_COLL_SM_EXPORT_H */




