/*
 * $HEADER$
 */

#ifndef MCA_PML_BASE_H
#define MCA_PML_BASE_H

#include "lam_config.h"

#include "mca/mca.h"
#include "mca/mpi/pml/pml.h"


/*
 * Global functions for the PML
 */

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
  int mca_pml_base_close(void);
  int mca_pml_base_open(lam_cmd_line_t *cmd);
  int mca_pml_base_query(void);
  int mca_pml_base_init(void);
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

/*
 * Public variables
 */

extern lam_list_t *mca_pml_base_opened;
extern lam_list_t *mca_pml_base_available;

/*
 * Global instance of array of pointers to mca_base_module_t.  Will
 * effectively be filled in by configure.
 */

extern const mca_base_module_t **mca_pml_base_modules;

#endif /* MCA_PML_BASE_H */
