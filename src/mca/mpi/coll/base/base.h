/*
 * $HEADER$
 */

#ifndef MCA_COLL_BASE_H
#define MCA_COLL_BASE_H

#include "lam_config.h"

#include "mpi.h"
#include "lam/lfc/list.h"
#include "lam/util/cmd_line.h"
#include "mca/mpi/coll/coll.h"


/*
 * Global functions for MCA overall collective open and close
 */

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
  int mca_coll_base_open(lam_cmd_line_t *cmd);
  int mca_coll_base_query(void);
  int mca_coll_base_close(void);

  int mca_coll_base_init_comm(MPI_Comm comm);
  int mca_coll_base_get_param(MPI_Comm comm, int keyval);
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif


/*
 * Public variables
 */

extern int mca_coll_base_verbose;
extern int mca_coll_base_did;
extern int mca_coll_base_crossover;
extern int mca_coll_base_associative;
extern int mca_coll_base_reduce_crossover;
extern lam_list_t *mca_coll_base_opened;
extern lam_list_t *mca_coll_base_available;


/*
 * Global instance of array of pointers to statically-linked coll
 * modules.  Will be filled in by configure.
 */

extern const mca_base_module_t **mca_coll_base_static_modules;


#endif /* MCA_COLL_H */
