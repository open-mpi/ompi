/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OMPI_PROC
#define OMPI_PROC

#include "ompi/types.h"
#include "opal/class/opal_list.h"
#include "orte/dss/dss_types.h"
#include "opal/threads/mutex.h"

#include "orte/mca/ns/ns_types.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
OMPI_DECLSPEC extern opal_class_t ompi_proc_t_class;

struct ompi_proc_t {
    /** allow proc to be placed on a list */
    opal_list_item_t          super;
    /** this process' name */
    orte_process_name_t       proc_name;
    /** BML specific proc data */
    struct mca_bml_base_endpoint_t* proc_bml;
    /** PML specific proc data */
    struct mca_pml_base_endpoint_t* proc_pml;
    /** MCA module exchange data */
    opal_object_t*            proc_modex;
    /** architecture of this process */
    uint32_t                  proc_arch;
    /** process-wide convertor */
    struct ompi_convertor_t*  proc_convertor;
    /** process-wide lock */
    opal_mutex_t              proc_lock;
    /** Keep the hostname around for debugging purposes */
    char *proc_hostname;
    /** flags for this proc */
    uint8_t                   proc_flags;
};
/**
 * Convenience typedef
 */
typedef struct ompi_proc_t ompi_proc_t;
OMPI_DECLSPEC extern ompi_proc_t* ompi_proc_local_proc;

/* 
 * Flags 
 */

/**
 * Flag to indicate that the proc is on the same node as the local proc
 */
#define OMPI_PROC_FLAG_LOCAL  0x01


/**
 * Query the run-time environment and build list of available proc instances.
 */
int ompi_proc_init(void);

/**
 * Release the processes at the end of the application
 */
int ompi_proc_finalize(void);

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
ompi_proc_t * ompi_proc_find ( const orte_process_name_t* name );



/**
 * INPUT: ompi_proc_t **proclist : list of process pointers
 * INPUT: int proclistsize       : lenght of the proclist array
 * IN/OUT: orte_buffer_t *buf    : an orte_buffer containing the packed names
 * 
 * This function takes a list of ompi_proc_t pointers (e.g. as given
 * in groups) and returns a orte buffer containing all information
 * needed to add the proc to a remote list.  This includes the ORTE
 * process name, the architecture, and the hostname.  Ordering is
 * maintained.  The buffer is packed to be sent to a remote node with
 * different architecture (endian or word size).  The buffer can be
 * dss unloaded to be sent using MPI or send using rml_send_packed.
 *
 * Return values:
 *  OMPI_SUCCESS               on success
 *  OMPI_ERROR:                other errors
 */
int ompi_proc_pack(ompi_proc_t **proclist, int proclistsize,
                   orte_buffer_t *buf);



/**
 * INPUT: orte_buffer_t *buf       : orte_buffer containing the packed names
 * INPUT: int proclistsize         : number of expected proc-pointres
 * OUTPUT: ompi_proc_t ***proclist : list of process pointers
 *
 * This function unpacks a packed list of ompi_proc_t structures and
 * returns the ordered list of proc structures.  If the given proc is
 * already "known", the architecture and hostname information in the
 * buffer is ignored.  If the proc is "new" to this process, it will
 * be added to the global list of known procs, with information
 * provided in the buffer.  The lookup actions are always entirely
 * local.  The proclist returned is a list of pointers to all procs in
 * the buffer, whether they were previously known or are new to this
 * process.  PML_ADD_PROCS will be called on the list of new processes
 * discovered during this operation.
 *
 * Return value:
 *   OMPI_SUCCESS               on success
 *   OMPI_ERROR                 else
 */
int ompi_proc_unpack(orte_buffer_t *buf, int proclistsize, ompi_proc_t ***proclist);


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif /* OMPI_PROC */

