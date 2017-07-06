/*
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_COLL_SPACC_EXPORT_H
#define MCA_COLL_SPACC_EXPORT_H

#include "ompi_config.h"

#include "mpi.h"
#include "ompi/mca/coll/coll.h"

BEGIN_C_DECLS

/* Globally exported variables */
extern int mca_coll_spacc_stream;
extern int mca_coll_spacc_priority;
extern int mca_coll_spacc_verbose;

/* API functions */

int mca_coll_spacc_init_query(bool enable_progress_threads,
                              bool enable_mpi_threads);
mca_coll_base_module_t
*mca_coll_spacc_comm_query(struct ompi_communicator_t *comm, int *priority);

int mca_coll_spacc_module_enable(mca_coll_base_module_t *module,
                                 struct ompi_communicator_t *comm);

int mca_coll_spacc_allreduce_intra_redscat_allgather(
    const void *sbuf, void *rbuf, int count, struct ompi_datatype_t *dtype,
    struct ompi_op_t *op, struct ompi_communicator_t *comm,
    mca_coll_base_module_t *module);

int mca_coll_spacc_reduce_intra_redscat_gather(
    const void *sbuf, void *rbuf, int count, struct ompi_datatype_t *dtype,
    struct ompi_op_t *op, int root, struct ompi_communicator_t *comm,
    mca_coll_base_module_t *module);

/*
 * coll API functions
 */

/* API functions */

int ompi_coll_spacc_init_query(bool enable_progress_threads,
                                bool enable_mpi_threads);

mca_coll_base_module_t *
ompi_coll_spacc_comm_query(struct ompi_communicator_t *comm, int *priority);

struct mca_coll_spacc_component_t {
    /* Base coll component */
    mca_coll_base_component_2_0_0_t super;

    /* MCA parameter: priority of this component */
    int spacc_priority;

    /* global stuff that I need the component to store */

    /* MCA parameters first */
};

/*
 * Convenience typedef
 */
typedef struct mca_coll_spacc_component_t mca_coll_spacc_component_t;

/*
 * Global component instance
 */
OMPI_MODULE_DECLSPEC extern mca_coll_spacc_component_t mca_coll_spacc_component;

struct mca_coll_spacc_module_t {
    mca_coll_base_module_t super;
};
typedef struct mca_coll_spacc_module_t mca_coll_spacc_module_t;
OBJ_CLASS_DECLARATION(mca_coll_spacc_module_t);

#endif  /* MCA_COLL_SPACC_EXPORT_H */
