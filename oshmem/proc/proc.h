/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#ifndef OSHMEM_PROC_PROC_H
#define OSHMEM_PROC_PROC_H

#include "oshmem_config.h"
#include "oshmem/types.h"
#include "oshmem/constants.h"

#include "oshmem/mca/scoll/scoll.h"

#include "opal/class/opal_list.h"
#include "opal/dss/dss_types.h"
#include "opal/mca/hwloc/hwloc.h"

#include "orte/types.h"
#include "orte/runtime/orte_globals.h"

#include "ompi/communicator/communicator.h"

BEGIN_C_DECLS

/* ******************************************************************** */

struct oshmem_group_t;

#define OSHMEM_PE_INVALID   (-1)

/**
 * Remote Open SHMEM process structure
 * 
 * Remote Open SHMEM process structure.  Each process contains exactly
 * one oshmem_proc_t structure for each remote process it knows about.
 */
struct oshmem_proc_t {
    /** allow proc to be placed on a list */
    opal_list_item_t                super;
    /** this process' name */
    orte_process_name_t             proc_name;
    /* endpoint data */
    void *proc_endpoints[OMPI_PROC_ENDPOINT_TAG_MAX];
    /** architecture of this process */
    uint32_t                        proc_arch;
    /** flags for this proc */
    opal_hwloc_locality_t           proc_flags;
    /** Base convertor for the proc described by this process */
    struct opal_convertor_t        *proc_convertor;
    /** A pointer to the name of this host - data is
     * actually stored in the RTE
     */
    char                           *proc_hostname;

    /* 
     * All transport channels are globally ordered. 
     * pe(s) can talk to each other via subset of transports
     * these holds indexes of each transport into global array
     *  proc -> id, where id can be btl id in yoda or mxm ptl id
     *  in ikrit 
     *  spml is supposed to fill this during add_procs()
     **/
    int                             num_transports;
    char                           *transport_ids;
};

typedef struct oshmem_proc_t oshmem_proc_t;
OBJ_CLASS_DECLARATION(oshmem_proc_t);

/**
 * Group of Open SHMEM processes structure
 * 
 * Set of processes used in collective operations.
 */
struct oshmem_group_t {
    opal_object_t               base;
    int                         id;             /**< index in global array */
    int                         my_pe;
    int                         proc_count;     /**< number of processes in group */
    int                         is_member;   /* true if my_pe is part of the group, participate in collectives */
    struct oshmem_proc_t      **proc_array;     /**< list of pointers to ompi_proc_t structures
                                 for each process in the group */
    opal_list_t                 peer_list;

    /* Collectives module interface and data */
    mca_scoll_base_group_scoll_t g_scoll;
    ompi_communicator_t*         ompi_comm;
};
typedef struct oshmem_group_t oshmem_group_t;
OSHMEM_DECLSPEC OBJ_CLASS_DECLARATION(oshmem_group_t);

OSHMEM_DECLSPEC extern oshmem_group_t* oshmem_group_all;
OSHMEM_DECLSPEC extern oshmem_group_t* oshmem_group_self;
OSHMEM_DECLSPEC extern oshmem_group_t* oshmem_group_null;

/**
 * @private
 *
 * Pointer to the oshmem_proc_t structure for the local process
 *
 * Pointer to the oshmem_proc_t structure for the local process.
 *
 * @note This pointer is declared here to allow inline functions
 * within this header file to access the local process quickly.
 * Please use oshmem_proc_local() instead.
 */
OSHMEM_DECLSPEC extern oshmem_proc_t* oshmem_proc_local_proc;

/* ******************************************************************** */

/**
 * Initialize the OSHMEM process subsystem
 *
 * Initialize the Open SHMEM process subsystem.  This function will
 * query the run-time environment and build a list of the proc
 * instances in the current pe set.  The local information not
 * easily determined by the run-time ahead of time (architecture and
 * hostname) will be published during this call.
 *
 * @note While an oshmem_proc_t will exist with mostly valid information
 * for each process in the pe set at the conclusion of this
 * call, some information will not be immediately available.  This
 * includes the architecture and hostname, which will be available by
 * the conclusion of the stage gate.
 *
 * @retval OSHMEM_SUCESS  System successfully initialized
 * @retval OSHMEM_ERROR   Initialization failed due to unspecified error
 */
OSHMEM_DECLSPEC int oshmem_proc_init(void);

/**
 * Set the arch of each proc in the oshmem_proc_list
 *
 * In some environments, SHMEM procs are required to exchange their
 * arch via a modex operation during mpi_init. In other environments,
 * the arch is determined by other mechanisms and provided to the
 * proc directly. To support both mechanisms, we provide a separate
 * function to set the arch of the procs -after- the modex operation
 * has completed in mpi_init.
 *
 * @retval OSHMEM_SUCCESS Archs successfully set
 * @retval OSHMEM_ERROR   Archs could not be initialized
 */
OSHMEM_DECLSPEC int oshmem_proc_set_arch(void);

/**
 * Finalize the OSHMEM Process subsystem
 *
 * Finalize the Open SHMEM process subsystem.  This function will
 * release all memory created during the life of the application,
 * including all oshmem_proc_t structures.
 *
 * @retval OSHMEM_SUCCESS  System successfully finalized
 */
OSHMEM_DECLSPEC int oshmem_proc_finalize(void);

/**
 * Returns the list of proc instances associated with this job.
 *
 * Returns the list of proc instances associated with this job.  Given
 * the current association between a job and an pe set, this
 * function provides the process instances for the current
 * pe set.
 *
 * @note The reference count of each process in the array is
 * NOT incremented - the caller is responsible for ensuring the
 * correctness of the reference count once they are done with
 * the array.
 *
 * @param[in] size     Number of processes in the oshmem_proc_t array
 *
 * @return Array of pointers to proc instances in the current
 * pe set, or NULL if there is an internal failure.
 */
OSHMEM_DECLSPEC oshmem_proc_t** oshmem_proc_world(size_t* size);

/**
 * Returns the list of all known proc instances.
 *
 * Returns the list of all known proc instances, including those in
 * other pe sets.  It is possible that we may no longer be
 * connected to some of the procs returned (in the SHMEM sense of the
 * word connected).  In a strictly SHMEM-1 application, this function
 * will return the same information as oshmem_proc_world().
 *
 * @note The reference count of each process in the array is
 * incremented and the caller is responsible for releasing each
 * process in the array, as well as freeing the array.
 *
 * @param[in] size     Number of processes in the oshmem_proc_t array
 *
 * @return Array of pointers to proc instances in the current
 * known universe, or NULL if there is an internal failure.
 */
OSHMEM_DECLSPEC oshmem_proc_t** oshmem_proc_all(size_t* size);

/**
 * Returns a list of the local process
 *
 * Returns a list containing the local process (and only the local
 * process).  Has calling semantics similar to oshmem_proc_world() and
 * oshmem_proc_all().
 *
 * @note The reference count of each process in the array is
 * incremented and the caller is responsible for releasing each
 * process in the array, as well as freeing the array.
 *
 * @param[in] size     Number of processes in the oshmem_proc_t array
 *
 * @return Array of pointers to proc instances in the current
 * known universe, or NULL if there is an internal failure.
 */
OSHMEM_DECLSPEC oshmem_proc_t** oshmem_proc_self(size_t* size);

/**
 * Returns a pointer to the local process
 *
 * Returns a pointer to the local process.  Unlike oshmem_proc_self(),
 * the reference count on the local proc instance is not modified by
 * this function.
 *
 * @return Pointer to the local process structure
 */
static inline oshmem_proc_t* oshmem_proc_local(void)
{
    return oshmem_proc_local_proc;
}

/**
 * Returns the proc instance for a given name 
 *
 * Returns the proc instance for the specified process name.  The
 * reference count for the proc instance is not incremented by this
 * function.
 *
 * @param[in] name     The process name to look for
 *
 * @return Pointer to the process instance for \c name
 */
OSHMEM_DECLSPEC oshmem_proc_t * oshmem_proc_find(const orte_process_name_t* name);

/**
 * Pack proc list into portable buffer
 *
 * This function takes a list of oshmem_proc_t pointers (e.g. as given
 * in groups) and returns a orte buffer containing all information
 * needed to add the proc to a remote list.  This includes the ORTE
 * process name, the architecture, and the hostname.  Ordering is
 * maintained.  The buffer is packed to be sent to a remote node with
 * different architecture (endian or word size).  The buffer can be
 * dss unloaded to be sent using SHMEM or send using rml_send_packed().
 * 
 * @param[in] proclist     List of process pointers
 * @param[in] proclistsize Length of the proclist array
 * @param[in,out] buf      An orte_buffer containing the packed names.  
 *                         The buffer must be constructed but empty when
 *                         passed to this function
 * @retval OSHMEM_SUCCESS    Success
 * @retval OSHMEM_ERROR      Unspecified error
 */
OSHMEM_DECLSPEC int oshmem_proc_pack(oshmem_proc_t **proclist,
                                     int proclistsize,
                                     opal_buffer_t *buf);

/**
 * Unpack a portable buffer of procs
 *
 * This function unpacks a packed list of oshmem_proc_t structures and
 * returns the ordered list of proc structures.  If the given proc is
 * already "known", the architecture and hostname information in the
 * buffer is ignored.  If the proc is "new" to this process, it will
 * be added to the global list of known procs, with information
 * provided in the buffer.  The lookup actions are always entirely
 * local.  The proclist returned is a list of pointers to all procs in
 * the buffer, whether they were previously known or are new to this
 * process.
 *
 * @note In previous versions of this function, The PML's add_procs()
 * function was called for any new processes discovered as a result of
 * this operation.  That is no longer the case -- the caller must use
 * the newproclist information to call add_procs() if necessary.
 *
 * @note The reference count for procs created as a result of this
 * operation will be set to 1.  Existing procs will not have their
 * reference count changed.  The reference count of a proc at the
 * return of this function is the same regardless of whether NULL is
 * provided for newproclist.  The user is responsible for freeing the
 * newproclist array.
 *
 * @param[in] buf          orte_buffer containing the packed names
 * @param[in] proclistsize number of expected proc-pointres
 * @param[out] proclist    list of process pointers
 * @param[out] newproclistsize Number of new procs added as a result
 *                         of the unpack operation.  NULL may be
 *                         provided if information is not needed.
 * @param[out] newproclist List of new procs added as a result of
 *                         the unpack operation.  NULL may be
 *                         provided if informationis not needed.
 *
 * Return value:
 *   OSHMEM_SUCCESS               on success
 *   OSHMEM_ERROR                 else
 */
OSHMEM_DECLSPEC int oshmem_proc_unpack(opal_buffer_t *buf,
                                       int proclistsize,
                                       oshmem_proc_t ***proclist,
                                       int *newproclistsize,
                                       oshmem_proc_t ***newproclist);

static inline int oshmem_proc_pe(oshmem_proc_t *proc)
{
    return (proc ? (int) proc->proc_name.vpid : -1);
}

/**
 * Initialize the OSHMEM process predefined groups
 *
 * Initialize the Open SHMEM process predefined groups.  This function will
 * query the run-time environment and build a list of the proc
 * instances in the current pe set.  The local information not
 * easily determined by the run-time ahead of time (architecture and
 * hostname) will be published during this call.
 *
 * @note This is primarily used once during SHMEM setup.
 *
 * @retval OSHMEM_SUCESS  System successfully initialized
 * @retval OSHMEM_ERROR   Initialization failed due to unspecified error
 */
OSHMEM_DECLSPEC int oshmem_proc_group_init(void);

/**
 * Finalize the OSHMEM process predefined groups
 *
 * Initialize the Open SHMEM process predefined groups.  This function will
 * query the run-time environment and build a list of the proc
 * instances in the current pe set.  The local information not
 * easily determined by the run-time ahead of time (architecture and
 * hostname) will be published during this call.
 *
 * @note This is primarily used once during SHMEM setup.
 *
 * @retval OSHMEM_SUCESS  System successfully initialized
 * @retval OSHMEM_ERROR   Initialization failed due to unspecified error
 */
OSHMEM_DECLSPEC int oshmem_proc_group_finalize(void);

/**
 * Create processes group.
 *
 * Returns the list of known proc instances located in this group.
 *
 * @param[in] pe_start     The lowest PE in the active set.
 * @param[in] pe_stride    The log (base 2) of the stride between consecutive 
 *                         PEs in the active set.
 * @param[in] pe_size      The number of PEs in the active set.
 *
 * @return Array of pointers to proc instances in the current
 * known universe, or NULL if there is an internal failure.
 */
OSHMEM_DECLSPEC oshmem_group_t* oshmem_proc_group_create(int pe_start,
                                                         int pe_stride,
                                                         size_t pe_size);

/**
 * Destroy processes group.
 *
 */
OSHMEM_DECLSPEC void oshmem_proc_group_destroy(oshmem_group_t* group);

static inline oshmem_proc_t *oshmem_proc_group_all(int pe)
{
    return oshmem_group_all->proc_array[pe];
}

static inline oshmem_proc_t* oshmem_proc_group_find(oshmem_group_t* group,
                                                    int pe)
{
    int i = 0;
    oshmem_proc_t* proc = NULL;

    if (OPAL_LIKELY(group)) {
        if (OPAL_LIKELY(group == oshmem_group_all)) {
            /* To improve performance use direct index. It is feature of oshmem_group_all */
            proc = group->proc_array[pe];
        } else {
            for (i = 0; i < group->proc_count; i++) {
                if (pe == oshmem_proc_pe(group->proc_array[i])) {
                    proc = group->proc_array[i];
                    break;
                }
            }
        }
    } else {
        orte_process_name_t name;

        name.jobid = ORTE_PROC_MY_NAME->jobid;
        name.vpid = pe;
        proc = oshmem_proc_find(&name);
    }

    return proc;
}

static inline int oshmem_proc_group_find_id(oshmem_group_t* group, int pe)
{
    int i = 0;
    int id = -1;

    if (group) {
        for (i = 0; i < group->proc_count; i++) {
            if (pe == oshmem_proc_pe(group->proc_array[i])) {
                id = i;
                break;
            }
        }
    }

    return id;
}

static inline int oshmem_proc_group_is_member(oshmem_group_t *group)
{
    return group->is_member;
}

static inline int oshmem_num_procs(void)
{
    extern opal_list_t oshmem_proc_list;

    if (!oshmem_group_all)
        return opal_list_get_size(&oshmem_proc_list);

    return oshmem_group_all->proc_count;
}

static inline int oshmem_my_proc_id(void)
{
    return oshmem_group_self->my_pe;
}

static inline int oshmem_get_transport_id(int pe)
{
    oshmem_proc_t *proc;

    proc = oshmem_proc_group_find(oshmem_group_all, pe);

    return (int) proc->transport_ids[0];
}

static inline int oshmem_get_transport_count(int pe)
{
    oshmem_proc_t *proc;
    proc = oshmem_proc_group_find(oshmem_group_all, pe);
    return proc->num_transports;
}

END_C_DECLS

#endif /* OSHMEM_PROC_PROC_H */
