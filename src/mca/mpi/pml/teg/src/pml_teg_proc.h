/*
 * $HEADER$
 */

#ifndef MCA_PML_PROC_H
#define MCA_PML_PROC_H

#include "mpi/proc/proc.h"
#include "mca/mpi/pml/teg/ptl_array.h"

extern lam_class_info_t mca_pml_teg_proc_cls;

/*
 *  Structure associated w/ lam_proc_t that contains data specific
 *  to the PML. Note that this name is not PML specific.
 */

struct mca_pml_proc_t {
   lam_list_item_t super;
   lam_proc_t *proc_lam;
   mca_ptl_array_t proc_ptl_first;
   mca_ptl_array_t proc_ptl_next;
};
typedef struct mca_pml_proc_t mca_pml_proc_t;


void mca_pml_teg_proc_init(mca_pml_proc_t*);
void mca_pml_teg_proc_destroy(mca_pml_proc_t*);

#endif

