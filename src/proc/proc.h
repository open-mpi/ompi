/*
 * $HEADER$
 */

#ifndef LAM_PROC
#define LAM_PROC

#include "types.h"
#include "lfc/lam_list.h"


extern lam_class_t lam_proc_t_class;


struct lam_proc_t {
    lam_list_item_t           super;       /* allow proc to be placed on a list */
    lam_job_handle_t          proc_job;    /* identifies a unique job */
    uint32_t                  proc_vpid;   /* process identifier w/in the job */
    struct mca_pml_proc_t*    proc_pml;    /* PML specific proc data */
    struct mca_base_modex_t*  proc_modex;  /* MCA module exchange data */

  /* JMS: need to have the following information:

     - endian info
     - type size info
     - peer parallel job id
     - how am i [mpi] connected (bitmap): spawn (parent/child), 
                                          connect, accept, joint
  */
};
typedef struct lam_proc_t lam_proc_t;


/**
 * Query the run-time environment and build list of available proc instances.
 */
int lam_proc_init(void);

/**
 * Returns the list of proc instances associated with this job.
 */
lam_proc_t** lam_proc_world(size_t* size);

/**
 * Returns the list of all known proc instances.
 */
lam_proc_t** lam_proc_all(size_t* size);

/**
 * Returns a list (of one) proc instances.
 */
lam_proc_t** lam_proc_self(size_t* size);

/**
 * Returns the proc instance corresponding to the local proc.
 */
static inline lam_proc_t* lam_proc_local(void) 
{
    extern lam_proc_t* lam_proc_local_proc;
    return lam_proc_local_proc;
}

#endif /* LAM_PROC */

