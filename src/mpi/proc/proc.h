/*
 * $HEADER$
 */

#ifndef LAM_PROC
#define LAM_PROC

#include "lam/lfc/list.h"


extern lam_class_info_t lam_proc_cls;
extern lam_dbl_list_t lam_procs;


struct lam_proc_t {
    lam_dbl_item_t         super;       /* allow proc to be placed on a list */
    pid_t                  proc_id; 
    int                    proc_gid;    /* globally unique identifier? */
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


void lam_proc_init(lam_proc_t*);
void lam_proc_destroy(lam_proc_t*);


#endif /* LAM_PROC */

