/*
 * $HEADER$
 */

#ifndef LAM_PROC
#define LAM_PROC

#include "lam/types.h"
#include "lam/lfc/list.h"


extern lam_class_info_t lam_proc_t_class_info;


struct lam_proc_t {
    lam_list_item_t        super;       /* allow proc to be placed on a list */
    lam_job_handle_t       proc_job;    /* identifies a unique job */
    uint32_t               proc_vpid;   /* process identifier w/in the job */
    struct mca_pml_proc_t* proc_pml;    /* PML specific proc data */

  /* JMS: need to have the following information:

     - endian info
     - type size info
     - peer parallel job id
     - how am i [mpi] connected (bitmap): spawn (parent/child), 
                                          connect, accept, joint
  */
};
typedef struct lam_proc_t lam_proc_t;


void  lam_proc_construct(lam_proc_t*);
void  lam_proc_destruct(lam_proc_t*);

static inline lam_proc_t* lam_proc_local(void) 
{
    extern lam_proc_t* lam_proc_self;
    return lam_proc_self;
}

#endif /* LAM_PROC */

