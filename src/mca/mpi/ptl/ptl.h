/*
 * $HEADER$
 */

#ifndef LAM_MCA_PTL_H
#define LAM_MCA_PTL_H

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

typedef int (*mca_ptl_query_fn_t)(int *priority, int *thread_min, 
                                  int *thread_max);

/*
 * Action functions
 *
 * JMS: Again, this is an example.
 */

typedef int (*mci_ptl_addprocs_fn_t)(lam_proc_t **procs, int nprocs);
typedef const struct mca_ptl_actions_1_0_0 *
  (*mca_ptl_init_1_0_0_fn_t)(struct _proc **procs, int nprocs,
                             int *max_tag, int *max_cid);


/*
 * Struct used to pass ptl MCA information from the each ptl instance
 * back to the MCA framework.
 */

typedef struct mca_ptl_1_0_0 {
  mca_1_0_0_t mp_meta_info;

  /* ptl API function pointers */

  mca_ptl_query_fn_t mp_query;
  mca_ptl_init_1_0_0_fn_t mp_init;
} mca_ptl_1_0_0_t;

typedef struct mca_ptl_actions_1_0_0 {

  /* ptl API action function pointers */

  mca_ptl_addprocs_fn_t mpa_addprocs;

  /* Flags */

  bool mp_tv_queue_support;
} mca_ptl_actions_1_0_0_t;


/*
 * Set the default type to use version 1.1.0 of the SSI RPI struct 
 */

typedef mca_ptl_1_1_0_t mca_ptl_t;
typedef mca_ptl_actions_1_1_0_t mca_ptl_actions_t;


/*
 * Global functions for MCA: overall ptl open and close
 */

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
  int mca_ptl_base_close(void);
  int mca_ptl_base_open(lam_cmd_line_t *cmd);
  int mca_ptl_base_query(void);
  int mca_ptl_base_init(void);
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

extern const mca_t **mca_ptl_modules;

#endif /* LAM_MCA_PTL_H */
