/*
 * $HEADER$
 */

#ifndef OMPI_PROC
#define OMPI_PROC

#include "include/types.h"
#include "class/ompi_list.h"
#include "datatype/datatype.h"
#include "threads/mutex.h"


extern ompi_class_t ompi_proc_t_class;


struct ompi_proc_t {
    ompi_list_item_t           super;       /* allow proc to be placed on a list */
    ompi_job_handle_t          proc_job;    /* identifies a unique job */
    uint32_t                  proc_vpid;   /* process identifier w/in the job */
    struct mca_pml_proc_t*    proc_pml;    /* PML specific proc data */
    struct mca_base_modex_t*  proc_modex;  /* MCA module exchange data */
    int                       proc_arch;
    ompi_convertor_t*          proc_convertor;
    ompi_mutex_t               proc_lock;

  /* JMS: need to have the following information:
     - how am i [mpi] connected (bitmap): spawn (parent/child), 
                                          connect, accept, joint
  */
};
typedef struct ompi_proc_t ompi_proc_t;


/**
 * Query the run-time environment and build list of available proc instances.
 */
int ompi_proc_init(void);

/**
 * Returns the list of proc instances associated with this job.
 */
ompi_proc_t** ompi_proc_world(size_t* size);

/**
 * Returns the list of all known proc instances.
 */
ompi_proc_t** ompi_proc_all(size_t* size);

/**
 * Returns a list (of one) proc instances.
 */
ompi_proc_t** ompi_proc_self(size_t* size);

/**
 * Returns the proc instance corresponding to the local proc.
 */
static inline ompi_proc_t* ompi_proc_local(void) 
{
    extern ompi_proc_t* ompi_proc_local_proc;
    return ompi_proc_local_proc;
}

/**
 * Returns the proc instance for a given vpid 
*/
ompi_proc_t * ompi_proc_find ( ompi_job_handle_t jobid, uint32_t vpid );

#endif /* OMPI_PROC */

