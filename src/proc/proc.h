/*
 * $HEADER$
 */

#ifndef OMPI_PROC
#define OMPI_PROC

#include "include/types.h"
#include "class/ompi_list.h"
#include "datatype/datatype.h"
#include "threads/mutex.h"
#include "mca/ns/ns.h"


extern ompi_class_t ompi_proc_t_class;


struct ompi_proc_t {
    ompi_list_item_t          super;       /* allow proc to be placed on a list */
    ompi_process_name_t       proc_name;
    struct mca_pml_proc_t*    proc_pml;    /* PML specific proc data */
    struct mca_base_modex_t*  proc_modex;  /* MCA module exchange data */
    int                       proc_arch;
    ompi_convertor_t*         proc_convertor;
    ompi_mutex_t              proc_lock;

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
 * Returns the proc instance for a given name 
*/
ompi_proc_t * ompi_proc_find ( const ompi_process_name_t* name );


/**
 * INPUT: ompi_proc_t **proclist : list of process pointers
 * INPUT: int proclistsize       : lenght of the proclist array
 * OUTPUT:  char **namebuf       : a buffer pointer to an array
 *                                 of packed ompi_process_name_t objects
 * OUTPUT:  size_t *namebuflen   : length of namebuf

 * The function takes a list of ompi_proc_t pointers (e.g. as given in groups) 
 * and returns a buffer, which contains a list of 'packed' ompi_process_name_t 
 * objects, in the same order as provided in proclist.
 * By 'packed' we mean, that the buffer should be able to be sent across
 * heterogeneous environments (e.g. it has to be big endian, ilp32 ).
 * This buffer could be sent using MPI_Send using MPI_BYTE or OOB_Send.
 *
 * Return values:
 *  OMPI_SUCCESS               on success
 *  OMPI_ERR_OUT_OF_RESOURCE   buffer could not be allocated
 *  OMPI_ERROR:                other errors
 */
int ompi_proc_get_namebuf_by_proc ( ompi_proc_t **proclist, int proclistsize,
                                    char **namebuf, int *namebuflen );

/**
 * Since ompi_proc_get_namebuf_by_proc allocates the namebuf, we decided to 
 * define a second function to return this buffer. It is up to the implementation,
 * whether it frees the buffer or reuses it.
 *
 * Return value:
 *   OMPI_SUCCESS               on success
 *
 */

int ompi_proc_namebuf_returnbuf ( char *namebuf );


/**
 * INPUT: char *namebuf            : buffer containing the opaque, portable
 *                                   form of the process_names
 * INPUT: size_t namebuflen        : length of namebuf
 * INPUT: int proclistsize         : number of expected proc-pointres
 * OUTPUT: ompi_proc_t ***proclist : list of process pointers
 *
 * This function 'unpacks' the ompi_process_name_t structures and looks
 * the according ompi_proc_t structure up. If the 'environment' does not
 * find a proc-structure, it tries to look it up from the name_service or 
 * any other service involved in such an operation (this is required for 
 * the dynamic MPI-2 scenarios). The buffer allocated by 
 * ompi_proc_get_proclist will not be returned to the 'environment'.
 *
 * Return value:
 *   OMPI_SUCCESS               on success
 *   OMPI_ERROR                 else
 */

int ompi_proc_get_proclist (char *namebuf, int namebuflen,
                            int proclistsize, ompi_proc_t ***proclist);

/**
 * identical to ompi_proc_get_namebuf_by_proc, the input is however a list 
 * of process_name_t structures instead of ompi_proc_t structures. This is 
 * required for some OOB routines.
 */

int ompi_proc_get_namebuf_by_name ( ompi_process_name_t **namelist, int namelistsize, 
                                    char **namebuf, int *namebuflen );

/**
 * similar to ompi_proc_get_proclist, returns however a list of 
 * ompi_process_name_t pointers.
 */
int ompi_proc_get_namelist ( char *namebuf, int namebuflen,
                             int namelistlen, ompi_process_name_t ***namelist );


#endif /* OMPI_PROC */

