/*
 * Copyright (c) 2013-2015 Mellanox Technologies, Inc.
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

#include "ompi/proc/proc.h"
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
    return (oshmem_proc_t *)ompi_proc_local_proc;
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
static inline oshmem_proc_t *oshmem_proc_for_find(const orte_process_name_t name)
{
    return (oshmem_proc_t *)ompi_proc_find(&name);
}

static inline oshmem_proc_t *oshmem_proc_find(int pe)
{
    orte_process_name_t name;

    name.jobid = ORTE_PROC_MY_NAME->jobid;
    name.vpid = pe;
    return oshmem_proc_for_find(name);
}

static inline int oshmem_proc_pe(oshmem_proc_t *proc)
{
    return (proc ? (int) proc->proc_name.vpid : -1);
}

#define OSHMEM_PROC_JOBID(PROC)    (((orte_process_name_t*)&((PROC)->proc_name))->jobid)
#define OSHMEM_PROC_VPID(PROC)     (((orte_process_name_t*)&((PROC)->proc_name))->vpid)

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
        proc = oshmem_proc_for_find(name);
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
    return (oshmem_group_all ?
        oshmem_group_all->proc_count : (int)opal_list_get_size(&ompi_proc_list));
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
