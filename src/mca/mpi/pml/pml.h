/*
 * $HEADER$
 */

#ifndef LAM_MCA_PML_H
#define LAM_MCA_PML_H

#include "mca/mca.h"
#include "proc.h"
#include "lam.h"
#include "lam/lfc/list.h"


/*
 * Types for each API function pointer.
 *
 * JMS: This is an example -- there will need to be a query function,
 * but I have no idea what the arguments will be.
 */

typedef int (*mca_pml_query_fn_t)(int *priority, int *thread_min, 
                                  int *thread_max);

/*
 * Action functions
 *
 * JMS: Again, this is an example.
 */

typedef int (*mci_pml_addprocs_fn_t)(lam_proc_t **procs, int nprocs);
typedef const struct mca_pml_actions_1_0_0 *
  (*mca_pml_init_1_0_0_fn_t)(struct _proc **procs, int nprocs,
                             int *max_tag, int *max_cid);


/*
 * Struct used to pass pml MCA information from the each pml instance
 * back to the MCA framework.
 */

typedef struct mca_pml_1_0_0 {
  mca_1_0_0_t mp_meta_info;

  /* pml API function pointers */

  mca_pml_query_fn_t mp_query;
  mca_pml_init_1_0_0_fn_t mp_init;
} mca_pml_1_0_0_t;

typedef struct mca_pml_actions_1_0_0 {

  /* pml API action function pointers */

  mca_pml_addprocs_fn_t mpa_addprocs;

  /* Flags */

  bool mp_tv_queue_support;
} mca_pml_actions_1_0_0_t;


/*
 * Set the default type to use version 1.1.0 of the SSI RPI struct 
 */

typedef mca_pml_1_1_0_t mca_pml_t;
typedef mca_pml_actions_1_1_0_t mca_pml_actions_t;


/*
 * Global functions for MCA: overall pml open and close
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

extern lam_list_t *lam_ssi_rpi_base_opened;
extern lam_list_t *lam_ssi_rpi_base_available;

/*
 * Global instance of array of pointers to lam_ssi_rpi_t.  Will
 * effectively be filled in by configure.
 */

extern const mca_t **mca_pml_modules;

#endif /* LAM_MCA_PML_H */
