/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2014 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2016      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_SPML_H
#define MCA_SPML_H

#include "oshmem_config.h"
#include "oshmem/types.h"
#include "oshmem/constants.h"

#include "opal_stdint.h"
#include "oshmem/mca/mca.h"
#include "opal/mca/btl/btl.h"
#include "oshmem/proc/proc.h"

#include "oshmem/mca/sshmem/sshmem.h"

BEGIN_C_DECLS

/*
 * SPML component types
 */

/**
 * MCA->PML Called by MCA framework to initialize the component.
 *
 * @param priority (OUT) Relative priority or ranking used by MCA to
 * selected a component.
 *
 * @param enable_progress_threads (IN) Whether this component is
 * allowed to run a hidden/progress thread or not.
 *
 * @param enable_mpi_threads (IN) Whether support for multiple MPI
 * threads is enabled or not (i.e., MPI_THREAD_MULTIPLE), which
 * indicates whether multiple threads may invoke this component
 * simultaneously or not.
 */
typedef enum {
    MCA_SPML_BASE_PUT_SYNCHRONOUS,
    MCA_SPML_BASE_PUT_COMPLETE,
    MCA_SPML_BASE_PUT_BUFFERED,
    MCA_SPML_BASE_PUT_READY,
    MCA_SPML_BASE_PUT_STANDARD,
    MCA_SPML_BASE_PUT_SIZE
} mca_spml_base_put_mode_t;

typedef struct mca_spml_base_module_1_0_0_t * (*mca_spml_base_component_init_fn_t)(int *priority,
                                                                                   bool enable_progress_threads,
                                                                                   bool enable_mpi_threads);

typedef int (*mca_spml_base_component_finalize_fn_t)(void);

/**
 * SPML component version and interface functions.
 */
struct mca_spml_base_component_2_0_0_t {
    mca_base_component_t spmlm_version;
    mca_base_component_data_t spmlm_data;
    mca_spml_base_component_init_fn_t spmlm_init;
    mca_spml_base_component_finalize_fn_t spmlm_finalize;
};
typedef struct mca_spml_base_component_2_0_0_t mca_spml_base_component_2_0_0_t;
typedef mca_spml_base_component_2_0_0_t mca_spml_base_component_t;

/**
 * MCA management functions.
 */

static inline char *mca_spml_base_mkey2str(sshmem_mkey_t *mkey)
{
    static char buf[64];

    if (mkey->len == 0) {
        snprintf(buf, sizeof(buf), "mkey: base=%p len=%d key=%" PRIu64, mkey->va_base, mkey->len, mkey->u.key);
    } else {
        snprintf(buf, sizeof(buf), "mkey: base=%p len=%d data=0x%p", mkey->va_base, mkey->len, mkey->u.data);
    }

    return buf;
}

/**
 * Downcall from MCA layer to enable the PML/BTLs.
 *
 * @param   enable  Enable/Disable SPML forwarding
 * @return          OSHMEM_SUCCESS or failure status.
 */
typedef int (*mca_spml_base_module_enable_fn_t)(bool enable);

/**
 * Waits for an int variable to change on the local PE.
 * Blocked until the variable is not equal to value.
 *
 * @param addr   Address of the variable to pool on.
 * @param value  The value to pool on. Pool until the value held in addr is different than value.
 * @return       OSHMEM_SUCCESS or failure status.
 */
typedef int (*mca_spml_base_module_wait_fn_t)(void* addr,
                                              int cmp,
                                              void* value,
                                              int datatype);

/**
 * Test for an int variable to change on the local PE.
 *
 * @param addr   Address of the variable to pool on.
 * @param value  The value to pool on. Pool until the value held in addr is different than value.
 * @param out_value Return value to indicated if variable is equal to given cmp value.
 * @return       OSHMEM_SUCCESS or failure status.
 */
typedef int (*mca_spml_base_module_test_fn_t)(void* addr,
                                              int cmp,
                                              void* value,
                                              int datatype,
                                              int *out_value);

/**
 * deserialize remote mkey
 *
 * @param mkey remote mkey
 */
typedef void (*mca_spml_base_module_mkey_unpack_fn_t)(shmem_ctx_t ctx, sshmem_mkey_t *, uint32_t segno, int remote_pe, int tr_id);

/**
 * If possible, get a pointer to the remote memory described by the mkey
 *
 * @param dst_addr  address of the symmetric variable
 * @param mkey      remote memory key
 * @param pe        remote PE
 *
 * @return pointer to remote memory or NULL
 */
typedef void * (*mca_spml_base_module_mkey_ptr_fn_t)(const void *dst_addr, sshmem_mkey_t *mkey, int pe);

/**
 * free resources used by deserialized remote mkey
 *
 * @param mkey remote mkey
 */
typedef void (*mca_spml_base_module_mkey_free_fn_t)(sshmem_mkey_t *, int pe);

/**
 * Register (Pinn) a buffer of 'size' bits starting in address addr
 *
 * @param addr   base address of the registered buffer.
 * @param size   the size of the buffer to be registered.
 * @param seg_id sysv segment id
 * @param count  number of internal transports (btls) that registered memory
 * @return       array of mkeys (one mkey per "btl") or NULL on failure
 *
 */
typedef sshmem_mkey_t * (*mca_spml_base_module_register_fn_t)(void *addr,
                                                                size_t size,
                                                                uint64_t shmid,
                                                                int *count);

/**
 * deregister memory pinned by register()
 */
typedef int (*mca_spml_base_module_deregister_fn_t)(sshmem_mkey_t *mkeys);

/**
 * try to fill up mkeys that can be used to reach remote pe.
 * @param pe         remote pe
 * @param seg 0 - symmetric heap, 1 - static data, everything else are static data in .so
 * @param mkeys      mkeys array
 *
 * @return OSHMEM_SUCCSESS if keys are found
 */
typedef int (*mca_spml_base_module_oob_get_mkeys_fn_t)(shmem_ctx_t ctx, int pe,
                                                       uint32_t seg,
                                                       sshmem_mkey_t *mkeys);

/**
 * For each proc setup a datastructure that indicates the BTLs
 * that can be used to reach the destination.
 *
 * @param procs  A list of all procs participating in the parallel application.
 * @param nprocs The number of procs in the parallel application.
 * @return OSHMEM_SUCCESS or failure status.
 *
 */
typedef int (*mca_spml_base_module_add_procs_fn_t)(struct oshmem_group_t* group,
                                                   size_t nprocs);
typedef int (*mca_spml_base_module_del_procs_fn_t)(struct oshmem_group_t* group,
                                                   size_t nprocs);


/**
 * Create a communication context.
 *
 * @param options  The set of options requested for the given context.
 * @param ctx      A handle to the newly created context.
 * @return         OSHMEM_SUCCESS or failure status.
 */
typedef int (*mca_spml_base_module_ctx_create_fn_t)(long options, shmem_ctx_t *ctx);


/**
 * Destroy a communication context.
 *
 * @param ctx      Handle to the context that will be destroyed.
 */
typedef void (*mca_spml_base_module_ctx_destroy_fn_t)(shmem_ctx_t ctx);

/**
 * Transfer data to a remote pe.
 *
 * @param ctx      The context object this routine is working on.
 * @param dst_addr The address in the remote PE of the object being written.
 * @param size     The number of bytes to be written.
 * @param src_addr An address on the local PE holdng the value to be written.
 * @param dst      The remote PE to be written to.
 * @return         OSHMEM_SUCCESS or failure status.
 */
typedef int (*mca_spml_base_module_put_fn_t)(shmem_ctx_t ctx,
                                             void *dst_addr,
                                             size_t size,
                                             void *src_addr,
                                             int dst);

/**
 * These routines provide the means for copying contiguous data to another PE without
 * blocking the caller. These routines return before the data has been delivered to the
 * remote PE.
 *
 * @param ctx      The context object this routine is working on.
 * @param dst_addr The address in the remote PE of the object being written.
 * @param size     The number of bytes to be written.
 * @param src_addr An address on the local PE holdng the value to be written.
 * @param dst      The remote PE to be written to.
 * @param handle   The address of a handle to be passed to shmem_wait_nb() or
 *                 shmem_test_nb() to wait or poll for the completion of the transfer.
 * @return         OSHMEM_SUCCESS or failure status.
 */
typedef int (*mca_spml_base_module_put_nb_fn_t)(shmem_ctx_t ctx,
                                                void *dst_addr,
                                                size_t size,
                                                void *src_addr,
                                                int dst,
                                                void **handle);



/**
 * The put-with-signal routines provide a method for copying data from a
 * contiguous local data object to a data object on a specified PE and
 * subsequently updating a remote flag to signal completion.
 *
 * @param ctx      A context handle specifying the context on which to perform the
 *                 operation. When this argument is not provided, the operation is
 *                 performed on the default context.
 * @param dst_addr The address in the remote PE of the object being written.
 * @param size     The number of bytes to be written.
 * @param src_addr An address on the local PE holdng the value to be written.
 * @param sig_addr Symmetric address of the signal data object to be updated on the
 *                 remote PE as a signal.
 * @param signal   Unsigned 64-bit value that is used for updating the remote sig_addr
 *                 signal data object.
 * @param sig_op   Signal operator that represents the type of update to be performed
 *                 on the remote sig_addr signal data object.
 * @param pe       PE number of the remote PE.
 *
 * @return         OSHMEM_SUCCESS or failure status.
 */

typedef int (*mca_spml_base_module_put_signal_fn_t)(shmem_ctx_t ctx,
                                                    void* dst_addr,
                                                    size_t size,
                                                    void* src_addr,
                                                    uint64_t *sig_addr,
                                                    uint64_t signal,
                                                    int sig_op,
                                                    int dst);


/**
 * The nonblocking put-with-signal routines provide a method for copying data
 * from a contiguous local data object to a data object on a specified PE and
 * subsequently updating a remote flag to signal completion.
 *
 * @param ctx      A context handle specifying the context on which to perform the
 *                 operation. When this argument is not provided, the operation is
 *                 performed on the default context.
 * @param dst_addr The address in the remote PE of the object being written.
 * @param size     The number of bytes to be written.
 * @param src_addr An address on the local PE holdng the value to be written.
 * @param sig_addr Symmetric address of the signal data object to be updated on the
 *                 remote PE as a signal.
 * @param signal   Unsigned 64-bit value that is used for updating the remote sig_addr
 *                 signal data object.
 * @param sig_op   Signal operator that represents the type of update to be performed
 *                 on the remote sig_addr signal data object.
 * @param pe       PE number of the remote PE.
 *
 * @return         OSHMEM_SUCCESS or failure status.
 */
typedef int (*mca_spml_base_module_put_signal_nb_fn_t) (shmem_ctx_t ctx,
                                            void* dst_addr,
                                            size_t size,
                                            void* src_addr,
                                            uint64_t *sig_addr,
                                            uint64_t signal,
                                            int sig_op,
                                            int dst);

/*
 *  Wait on an array of variables on the local PE until all variables
 *  meet the specified wait condition.
 *
 *
 *  @param  ivars       Symmetric address of an array of remotely accessible
 *                      data objects. The type of ivars should match that
 *                      implied in the SYNOPSIS section.
 *  @param  cmp         A comparison operator that compares elements of ivars with cmp_value.
 *  @param  cmp_value   The value to be compared with the objects pointed to by ivars.
 *  @param  nelems      The number of elements in the ivars array.
 *  @param  status      Local address of an optional mask array of length nelems that indicates
 *                      which elements in ivars are excluded from the wait set.
 *  @param  datatype    Type of the objects
 *
 *  @return             None
 */
typedef void(*mca_spml_base_module_wait_until_all_fn_t)(void *ivars,
                                                    int cmp,
                                                    void *cmp_value,
                                                    size_t nelems,
                                                    const int *status,
                                                    int datatype);

/*
 *  Wait on an array of variables on the local PE until any one variable
 *  meets the specified wait condition.
 *
 *  @param  ivars       Symmetric address of an array of remotely accessible
 *                      data objects. The type of ivars should match that
 *                      implied in the SYNOPSIS section.
 *  @param  cmp         A comparison operator that compares elements of ivars with cmp_value.
 *  @param  cmp_value   The value to be compared with the objects pointed to by ivars.
 *  @param  nelems      The number of elements in the ivars array.
 *  @param  status      Local address of an optional mask array of length nelems that indicates
 *                      which elements in ivars are excluded from the wait set.
 *  @param  datatype    Type of the objects
 *
 *  @return             Returns the index of an element in the ivars array that satisfies the 
 *                      wait condition. If the wait set is empty, this routine returns SIZE_MAX.
 */
typedef size_t (*mca_spml_base_module_wait_until_any_fn_t)(void *ivars,
                                                    int cmp,
                                                    void *cmp_value,
                                                    size_t nelems,
                                                    const int *status,
                                                    int datatype);


/*
 *  Wait on an array of variables on the local PE until at least one variable
 *  meets the specified wait condition.
 *
 *  @param  ivars       Symmetric address of an array of remotely accessible
 *                      data objects. The type of ivars should match that
 *                      implied in the SYNOPSIS section.
 *  @param  cmp         A comparison operator that compares elements of ivars with cmp_value.
 *  @param  cmp_value   The value to be compared with the objects pointed to by ivars.
 *  @param  nelems      The number of elements in the ivars array.
 *  @param  indices     Local address of an array of indices of length at least nelems into
 *                      ivars that satisfied the wait condition.
 *  @param  status      Local address of an optional mask array of length nelems that indicates
 *                      which elements in ivars are excluded from the wait set.
 *  @param  datatype    Type of the objects
 *
 *  @return             Returns the number of indices returned in the indices array. If the wait 
 *                      set is empty, this routine returns 0.
 */
typedef size_t (*mca_spml_base_module_wait_until_some_fn_t)(void *ivars,
                                                    int cmp,
                                                    void *cmp_value,
                                                    size_t nelems,
                                                    size_t *indices,
                                                    const int *status,
                                                    int datatype);


/*
 *  Wait on an array of variables on the local PE until all variables meet the
 *  specified wait conditions.
 *
 *  @param  ivars       Symmetric address of an array of remotely accessible
 *                      data objects. The type of ivars should match that
 *                      implied in the SYNOPSIS section.
 *  @param  cmp         A comparison operator that compares elements of ivars with elements
 *                      of cmp_values.
 *  @param  cmp_values  Local address of an array of length nelems containing values to be
 *                      compared with the respective objects in ivars.
 *  @param  nelems      The number of elements in the ivars array.
 *  @param  status      Local address of an optional mask array of length nelems that indicates
 *                      which elements in ivars are excluded from the wait set.
 *  @param  datatype    Type of the objects
 *
 *  @return             None
 *                      
 */
typedef void (*mca_spml_base_module_wait_until_all_vector_fn_t)(void *ivars,
                                                    int cmp,
                                                    void *cmp_values,
                                                    size_t nelems,
                                                    const int *status,
                                                    int datatype);

/*
 *  Wait on an array of variables on the local PE until any one variable
 *  meets the specified wait condition.
 *
 *  @param  ivars       Symmetric address of an array of remotely accessible
 *                      data objects. The type of ivars should match that
 *                      implied in the SYNOPSIS section.
 *  @param  cmp         A comparison operator that compares elements of ivars with cmp_value.
 *  @param  cmp_value   The value to be compared with the objects pointed to by ivars.
 *  @param  nelems      The number of elements in the ivars array.
 *  @param  status      Local address of an optional mask array of length nelems that indicates
 *                      which elements in ivars are excluded from the wait set.
 *  @param  datatype    Type of the objects
 *
 *  @return             Returns the index of an element in the ivars array that satisfies the 
 *                      test condition. If the test set is empty or no conditions in the test 
 *                      set are satisfied, this routine returns SIZE_MAX.
 */
typedef size_t (*mca_spml_base_module_wait_until_any_vector_fn_t)(void *ivars,
                                                    int cmp,
                                                    void *cmp_value,
                                                    size_t nelems,
                                                    const int *status,
                                                    int datatype);

/*
 *  Wait on an array of variables on the local PE until at least one variable meets the
 *  its specified wait condition.
 *
 *  @param  ivars       Symmetric address of an array of remotely accessible
 *                      data objects. The type of ivars should match that
 *                      implied in the SYNOPSIS section.
 *  @param  cmp         A comparison operator that compares elements of ivars with elements
 *                      of cmp_values.
 *  @param  cmp_values  Local address of an array of length nelems containing values to be
 *                      compared with the respective objects in ivars.
 *  @param  nelems      The number of elements in the ivars array.
 *  @param  indices     Local address of an array of indices of length at least nelems into ivars
 *                      that satisfied the wait condition.
 *  @param  status      Local address of an optional mask array of length nelems that indicates
 *                      which elements in ivars are excluded from the wait set.
 *  @param  datatype    Type of the objects
 *
 *  @return             Returns the number of indices returned in the indices array. If the test 
 *                      set is empty, this routine returns 0.
 */
typedef size_t (*mca_spml_base_module_wait_until_some_vector_fn_t)(void *ivars,
                                                    int cmp,
                                                    void *cmp_value,
                                                    size_t nelems,
                                                    size_t *indices,
                                                    const int *status,
                                                    int datatype);


/*
 *  Indicate whether all variables within an array of variables on the local PE meet
 *  a specified test condition.
 *
 *
 *  @param  ivars       Symmetric address of an array of remotely accessible
 *                      data objects. The type of ivars should match that
 *                      implied in the SYNOPSIS section.
 *  @param  cmp         A comparison operator that compares elements of ivars with cmp_value.
 *  @param  cmp_value   The value to be compared with the objects pointed to by ivars.
 *  @param  nelems      The number of elements in the ivars array.
 *  @param  status      Local address of an optional mask array of length nelems that indicates
 *                      which elements in ivars are excluded from the test set.
 *  @param  datatype    Type of the objects
 *
 *  @return             Returns 1 if all variables in ivars satisfy the test condition or if 
 *                      nelems is 0, otherwise this routine returns 0.
 */
typedef int (*mca_spml_base_module_test_all_fn_t)(void *ivars,
                                                    int cmp,
                                                    void *cmp_value,
                                                    size_t nelems,
                                                    const int *status,
                                                    int datatype);

/*
 *  Indicate whether any one variable within an array of variables on the local PE meets
 *  a specified test condition.
 *
 *  @param  ivars       Symmetric address of an array of remotely accessible
 *                      data objects. The type of ivars should match that
 *                      implied in the SYNOPSIS section.
 *  @param  cmp         A comparison operator that compares elements of ivars with cmp_value.
 *  @param  cmp_value   The value to be compared with the objects pointed to by ivars.
 *  @param  nelems      The number of elements in the ivars array.
 *  @param  status      Local address of an optional mask array of length nelems that indicates
 *                      which elements in ivars are excluded from the test set.
 *  @param  datatype    Type of the objects
 *
 *  @return             Returns the index of an element in the ivars array that satisfies the 
 *                      test condition. If the test set is empty or no conditions in the test
 *                      set are satisfied, this routine returns SIZE_MAX..
 */
typedef size_t (*mca_spml_base_module_test_any_fn_t)(void *ivars,
                                                    int cmp,
                                                    void *cmp_value,
                                                    size_t nelems,
                                                    const int *status,
                                                    int datatype);


/*
 *  Indicate whether at least one variable within an array of variables on the local PE meets
 *  a specified test condition.
 *
 *  @param  ivars       Symmetric address of an array of remotely accessible
 *                      data objects. The type of ivars should match that
 *                      implied in the SYNOPSIS section.
 *  @param  cmp         A comparison operator that compares elements of ivars with cmp_value.
 *  @param  cmp_value   The value to be compared with the objects pointed to by ivars.
 *  @param  nelems      The number of elements in the ivars array.
 *  @param  indices     Local address of an array of indices of length at least nelems into
 *                      ivars that satisfied the wait condition.
 *  @param  status      Local address of an optional mask array of length nelems that indicates
 *                      which elements in ivars are excluded from the test set.
 *  @param  datatype    Type of the objects
 *
 *  @return             Returns the number of indices returned in the indices array. If the test 
 *                      set is empty, this routine returns 0.
 */
typedef size_t (*mca_spml_base_module_test_some_fn_t)(void *ivars,
                                                    int cmp,
                                                    void *cmp_value,
                                                    size_t nelems,
                                                    size_t *indices,
                                                    const int *status,
                                                    int datatype);


/*
 *  Indicate whether all variables within an array of variables on the local PE meet the
 *  specified test conditions.
 *
 *  @param  ivars       Symmetric address of an array of remotely accessible
 *                      data objects. The type of ivars should match that
 *                      implied in the SYNOPSIS section.
 *  @param  cmp         A comparison operator that compares elements of ivars with elements
 *                      of cmp_values.
 *  @param  cmp_values  Local address of an array of length nelems containing values to be
 *                      compared with the respective objects in ivars.
 *  @param  nelems      The number of elements in the ivars array.
 *  @param  status      Local address of an optional mask array of length nelems that indicates
 *                      which elements in ivars are excluded from the test set.
 *  @param  datatype    Type of the objects
 *
 *  @return             Returns 1 if all variables in ivars satisfy the test conditions or if 
 *                      nelems is 0, otherwise this routine returns 0.
 */
typedef int (*mca_spml_base_module_test_all_vector_fn_t)(void *ivars,
                                                    int cmp,
                                                    void *cmp_values,
                                                    size_t nelems,
                                                    const int *status,
                                                    int datatype);

/*
 *  Indicate whether any one variable within an array of variables on the local PE meets
 *  its specified test condition.
 *
 *  @param  ivars       Symmetric address of an array of remotely accessible
 *                      data objects. The type of ivars should match that
 *                      implied in the SYNOPSIS section.
 *  @param  cmp         A comparison operator that compares elements of ivars with cmp_value.
 *  @param  cmp_values  Local address of an array of length nelems containing values to be
 *                      compared with the respective objects in ivars.
 *  @param  nelems      The number of elements in the ivars array.
 *  @param  status      Local address of an optional mask array of length nelems that indicates
 *                      which elements in ivars are excluded from the test set.
 *  @param  datatype    Type of the objects
 *
 *  @return             OSHMEM_SUCCESS or failure status.
 */
typedef size_t (*mca_spml_base_module_test_any_vector_fn_t)(void *ivars, int cmp,
                                                            void *cmp_values,
                                                            size_t nelems,
                                                            const int *status,
                                                            int datatype);

/*
 *  Indicate whether at least one variable within an array of variables on the local PE meets
 *  its specified test condition.
 *
 *  @param  ivars       Symmetric address of an array of remotely accessible
 *                      data objects. The type of ivars should match that
 *                      implied in the SYNOPSIS section.
 *  @param  cmp         A comparison operator that compares elements of ivars with elements
 *                      of cmp_values.
 *  @param  cmp_values  Local address of an array of length nelems containing values to be
 *                      compared with the respective objects in ivars.
 *  @param  nelems      The number of elements in the ivars array.
 *  @param  indices     Local address of an array of indices of length at least nelems into ivars
 *                      that satisfied the wait condition.
 *  @param  status      Local address of an optional mask array of length nelems that indicates
 *                      which elements in ivars are excluded from the test set.
 *  @param  datatype    Type of the objects
 *
 *  @return             OSHMEM_SUCCESS or failure status.
 */
typedef size_t (*mca_spml_base_module_test_some_vector_fn_t)(void *ivars,
                                                             int cmp,
                                                             void *cmp_values,
                                                             size_t nelems,
                                                             size_t *indices,
                                                             const int *status,
                                                             int datatype);

/*
 *  Registers the arrival of a PE at a synchronization point.
 *  This routine does not return until all other PEs in a given
 *  OpenSHMEM team or active set arrive at this synchronization point.
 *
 *
 *  @param  team       An OpenSHMEM team handle.
 *
 *  @return            OSHMEM_SUCCESS or failure status.
 *                     Zero on successful local completion. Nonzero otherwise.
 */
typedef int (*mca_spml_base_module_team_sync_fn_t)(shmem_team_t team);


/*
 *  Returns the number of the calling PE within a specified team.
 *
 *  @param  team       An OpenSHMEM team handle.
 *
 *  @return            The number of the calling PE within the specified
 *                     team, or the value -1 if the team handle compares
 *                     equal to SHMEM_TEAM_INVALID
 */
typedef int (*mca_spml_base_module_team_my_pe_fn_t)(shmem_team_t team);


/*
 *  Returns the number of PEs in a specified team.
 *
 *  @param  team       An OpenSHMEM team handle.
 *
 *  @return            The number of PEs in the specified team, or the
 *                     value -1 if the team handle compares equal to
 *                     SHMEM_TEAM_INVALID.
 */
typedef int (*mca_spml_base_module_team_n_pes_fn_t)(shmem_team_t team);



/*
 *  Return the configuration parameters of a given team
 *
 *  @param  team            An OpenSHMEM team handle.
 *
 *  @param  config_mask     The bitwise mask representing the set of
 *                          configuration parameters to fetch from the
 *                          given team.
 *
 *  @param  config          A pointer to the configuration parameters for the
 *                          given team.
 *
 *
 *  @return                 OSHMEM_SUCCESS or failure status.
 *
 */
typedef int (*mca_spml_base_module_team_get_config_fn_t)(shmem_team_t team, long
        config_mask, shmem_team_config_t *config);

/*
 *  Translate a given PE number from one team to the corresponding PE number in
 *  another team.
 *
 *  @param  src_team    An OpenSHMEM team handle.
 *  @param  src_pe      A PE number in src_team.
 *  @param  dest_team   An OpenSHMEM team handle.
 *
 *
 *  @return             The specified PE’s number in the dest_team, or a value
 *                      of -1 if any team handle arguments are invalid or the
 *                      src_pe is not in both the source and destination teams.
 */
typedef int (*mca_spml_base_module_team_translate_pe_fn_t)(shmem_team_t src_team,
        int src_pe, shmem_team_t dest_team);



/*
 *  Create a new OpenSHMEM team from a subset of the existing parent team PEs,
 *  where the subset is defined by the PE triplet (start, stride, and size)
 *  supplied to the routine.
 *
 *  @param  parent_team    An OpenSHMEM team handle.
 *  @param  start          The lowest PE number of the subset of PEs from the parent team
 *                         that will form the new team.
 *  @param  stride         The stride between team PE numbers in the parent team that comprise the subset
 *                         of PEs that will form the new team.
 *  @param  size           The number of PEs from the parent team in the subset of PEs that
 *                         will form the new team. size must be a positive integer.
 *  @param  config         A pointer to the configuration parameters for the new team.
 *  @param  config_mask    The bitwise mask representing the set of configuration parameters
 *                         to use from config.
 *  @param  new_team       An OpenSHMEM team handle. Upon successful creation, it references an OpenSHMEM
 *                         team that contains the subset of all PEs in the parent team
 *                         specified by the PE triplet provided.m
 *
 *
 *  @return                OSHMEM_SUCCESS or failure status.
 *
 */
typedef int (*mca_spml_base_module_team_split_strided_fn_t)(shmem_team_t
        parent_team, int start, int stride, int size, const shmem_team_config_t
        *config, long config_mask, shmem_team_t *new_team);


/*
 *  Create two new teams by splitting an existing parent team into two subsets
 *  based on a 2D Cartesian space defined by the xrange argument and a y
 *  dimension that is derived from xrange and the parent team size.
 *
 *  @param  parent_team    An OpenSHMEM team handle.
 *  @param  xrange         A positive integer representing the number of elements in the first dimension.
 *  @param  xaxis_config   A pointer to the configuration parameters for the new x-axis team.
 *  @param  xaxis_mask     The bitwise mask representing the set of configuration parameters to
 *                         use from xaxis_config.
 *  @param  xaxis_team     A new PE team handle representing a PE subset consisting of all the
 *                         PEs that have the same coordinate along the y-axis as the calling PE..
 *  @param  yaxis_config   A pointer to the configuration parameters for the new y-axis team.
 *  @param  yaxis_mask     The bitwise mask representing the set of configuration parameters to use
 *                         from yaxis_config.
 *  @param  yaxis_team     A new PE team handle representing a PE subset consisting of all the PEs
 *                         that have the same coordinate along the x-axis as the calling PE.
 *
 *  @return                OSHMEM_SUCCESS or failure status.
 *
 */
typedef int (*mca_spml_base_module_team_split_2d_fn_t)(shmem_team_t parent_team,
        int xrange, const shmem_team_config_t *xaxis_config, long xaxis_mask,
        shmem_team_t *xaxis_team, const shmem_team_config_t *yaxis_config, long
        yaxis_mask, shmem_team_t *yaxis_team);


/*
 *  Destroy an existing team.
 *
 *  @param  team           An OpenSHMEM team handle.
 *
 *  @return                OSHMEM_SUCCESS or failure status.
 *
 */
typedef int (*mca_spml_base_module_team_destroy_fn_t)(shmem_team_t team);

/*
 *  Retrieve the team associated with the communication context.
 *
 *  @param  ctx            A handle to a communication context.
 *  @param  team           A pointer to a handle to the associated PE team.
 *
 *  @return                OSHMEM_SUCCESS or failure status.
 *
 */
typedef int (*mca_spml_base_module_team_get_fn_t)(shmem_ctx_t ctx, shmem_team_t *team);

/*
 *  Create a communication context from a team.
 *
 *  @param  team           An OpenSHMEM team handle.
 *  @param  options        The set of options requested for the given context.
 *  @param  ctx            A handle to the newly created context.
 *
 *  @return                OSHMEM_SUCCESS or failure status.
 *
 */
typedef int (*mca_spml_base_module_team_create_ctx_fn_t)(shmem_team_t team, long options, shmem_ctx_t *ctx);

/*
 *  Exchanges a fixed amount of contiguous data blocks between all pairs of
 *  PEs participating in the collective routine..
 *
 *  @param  team       An OpenSHMEM team handle.
 *  @param  dest       Symmetric address of a data object large enough to
 *                     receive the combined total of nelems elements from each PE in the active set.
 *  @param  source     Symmetric address of a data object that contains nelems elements of data
 *                     for each PE in the active set, ordered according to destination PE.
 *  @param  nelems     The number of elements to exchange for each PE.
 *  @param  datatype   Datatype of the elements
 *
 *  @return            OSHMEM_SUCCESS or failure status.
 *
 */
typedef int (*mca_spml_base_module_team_alltoall_fn_t)(shmem_team_t team, void
        *dest, const void *source, size_t nelems, int datatype);

/*
 *  Exchanges a fixed amount of strided data blocks between all pairs of PEs
 *  participating in the collective routine.
 *
 *  @param  team       An OpenSHMEM team handle.
 *  @param  dest       Symmetric address of a data object large enough to
 *                     receive the combined total of nelems elements from each PE in the active set.
 *  @param  source     Symmetric address of a data object that contains nelems elements of data
 *                     for each PE in the active set, ordered according to destination PE.
 *  @param  dst        The stride between consecutive elements of the dest data object. The stride
 *                     is scaled by the element size. A value of 1 indicates contiguous data.
 *  @param  sst        The stride between consecutive elements of the source data object. The stride
 *                     is scaled by the element size. A value of 1 indicates contiguous data
 *  @param  nelems     The number of elements to exchange for each PE.
 *  @param  datatype   Datatype of the elements
 *
 *  @return            OSHMEM_SUCCESS or failure status.
 *
 */
typedef int (*mca_spml_base_module_team_alltoalls_fn_t)(shmem_team_t team, void
        *dest, const void *source, ptrdiff_t dst, ptrdiff_t sst, size_t nelems,
        int datatype);


/*
 *  Broadcasts a block of data from one PE to one or more destination PEs.
 *
 *  @param  team       An OpenSHMEM team handle.
 *  @param  dest       Symmetric address of destination data object.
 *  @param  source     Symmetric address of the source data object.
 *  @param  nelems     The number of elements in source and dest arrays
 *  @param  PE_root    Zero-based ordinal of the PE, with respect to the team or
 *                     active set, from which the data is copied..
 *  @param  datatype   Datatype of the elements
 *
 *  @return            OSHMEM_SUCCESS or failure status.
 *
 */
typedef int (*mca_spml_base_module_team_broadcast_fn_t)(shmem_team_t team, void
        *dest, const void *source, size_t nelems, int PE_root, int datatype);



/*
 *  Concatenates blocks of data from multiple PEs to an array in every PE participating in
 *  the collective routine.
 *
 *  @param  team       An OpenSHMEM team handle.
 *  @param  dest       Symmetric address of an array large enough to accept the
 *                     concatenation of the source arrays on all participating PEs.
 *  @param  source     Symmetric address of the source data object.
 *  @param  nelems     The number of elements in source array.
 *  @param  datatype   Datatype of the elements
 *
 *  @return            OSHMEM_SUCCESS or failure status.
 *
 */
typedef int (*mca_spml_base_module_team_collect_fn_t)(shmem_team_t team, void
        *dest, const void *source, size_t nelems, int datatype);
typedef int (*mca_spml_base_module_team_fcollect_fn_t)(shmem_team_t team, void
        *dest, const void *source, size_t nelems, int datatype);

/*
 *  Performs a math reduction across a set of PEs.
 *
 *  @param  team       An OpenSHMEM team handle.
 *  @param  dest       Symmetric address of an array, of length nreduce elements,
 *                     to receive the result of the reduction routines.
 *  @param  source     Symmetric address of an array, of length nreduce elements, that
 *                     contains one element for each separate reduction routine.
 *  @param  nreduce    The number of elements in the dest and source arrays.
 *  @param  operation  Operations from list of supported oshmem ops
 *  @param  datatype   Datatype of the elements
 *
 *  @return            OSHMEM_SUCCESS or failure status.
 *
 */
typedef int (*mca_spml_base_module_team_reduce_fn_t)(shmem_team_t team, void
        *dest, const void *source, size_t nreduce, int operation, int datatype);


/**
 * Blocking data transfer from remote PE.
 * Read data from remote PE.
 *
 * @param ctx      The context object this routine is working on.
 * @param dst_addr The address on the local PE, to write the result of the get operation to.
 * @param size     The number of bytes to be read.
 * @param src_addr The address on the remote PE, to read from.
 * @param src      The ID of the remote PE.
 * @return         OSHMEM_SUCCESS or failure status.
 */
typedef int (*mca_spml_base_module_get_fn_t)(shmem_ctx_t ctx,
                                             void *dst_addr,
                                             size_t size,
                                             void *src_addr,
                                             int src);

/**
 * Non-blocking data transfer from remote PE.
 * Read data from remote PE.
 *
 * @param ctx      The context object this routine is working on.
 * @param dst_addr The address on the local PE, to write the result of the get operation to.
 * @param size     The number of bytes to be read.
 * @param src_addr The address on the remote PE, to read from.
 * @param src      The ID of the remote PE.
 * @param handle   The address of a handle to be passed to shmem_wait_nb() or
 *                 shmem_test_nb() to wait or poll for the completion of the transfer.
 * @return         - OSHMEM_SUCCESS or failure status.
 */
typedef int (*mca_spml_base_module_get_nb_fn_t)(shmem_ctx_t ctx,
                                               void *dst_addr,
                                               size_t size,
                                               void *src_addr,
                                               int src,
                                               void **handle);

/**
 *  Post a receive and wait for completion.
 *
 *  @param buf (IN)         User buffer.
 *  @param count (IN)       The number of bytes to be sent.
 *  @param src (IN)         The ID of the remote PE.
 *  @return                 OSHMEM_SUCCESS or failure status.
 */
typedef int (*mca_spml_base_module_recv_fn_t)(void *buf, size_t count, int src);

/**
 *  Post a send request and wait for completion.
 *
 *  @param buf (IN)         User buffer.
 *  @param count (IN)       The number of bytes to be sent.
 *  @param dst (IN)         The ID of the remote PE.
 *  @param mode (IN)        Send mode (STANDARD,BUFFERED,SYNCHRONOUS,READY)
 *  @return                 OSHMEM_SUCCESS or failure status.
 */
typedef int (*mca_spml_base_module_send_fn_t)(void *buf,
                                              size_t count,
                                              int dst,
                                              mca_spml_base_put_mode_t mode);

/**
 *  The routine transfers the data asynchronously from the source PE to all
 *  PEs in the OpenSHMEM job. The routine returns immediately. The source and
 *  target buffers are reusable only after the completion of the routine.
 *  After the data is transferred to the target buffers, the counter object
 *  is updated atomically. The counter object can be read either using atomic
 *  operations such as shmem_atomic_fetch or can use point-to-point synchronization
 *  routines such as shmem_wait_until and shmem_test.
 *
 *  Shmem_quiet may be used for completing the operation, but not required for
 *  progress or completion. In a multithreaded OpenSHMEM program, the user
 *  (the OpenSHMEM program) should ensure the correct ordering of
 *  shmemx_alltoall_global calls.
 *
 *  @param dest        A symmetric data object that is large enough to receive
 *                     “size” bytes of data from each PE in the OpenSHMEM job.
 *  @param source      A symmetric data object that contains “size” bytes of data
 *                     for each PE in the OpenSHMEM job.
 *  @param size        The number of bytes to be sent to each PE in the job.
 *  @param counter     A symmetric data object to be atomically incremented after
 *                     the target buffer is updated.
 *
 *  @return            OSHMEM_SUCCESS or failure status.
 */
typedef int (*mca_spml_base_module_put_all_nb_fn_t)(void *dest,
                                                    const void *source,
                                                    size_t size,
                                                    long *counter);

/**
 * Assures ordering of delivery of put() requests
 *
 * @param ctx      - The context object this routine is working on.
 * @return         - OSHMEM_SUCCESS or failure status.
 */
typedef int (*mca_spml_base_module_fence_fn_t)(shmem_ctx_t ctx);

/**
 * Wait for completion of all outstanding put() requests
 *
 * @param ctx      - The context object this routine is working on.
 * @return         - OSHMEM_SUCCESS or failure status.
 */
typedef int (*mca_spml_base_module_quiet_fn_t)(shmem_ctx_t ctx);

/**
 * Waits for completion of a non-blocking put or get issued by the calling PE.
 *
 * @return         - OSHMEM_SUCCESS or failure status.
 */
typedef int (*mca_spml_base_module_wait_nb_fn_t)(void *);

/**
 * Called by memheap when memory is allocated by shmalloc(),
 * shcalloc(), shmemalign() or shrealloc()
 *
 * @param addr   base address of the registered buffer.
 * @param size   the size of the buffer to be registered.
 */
typedef void (*mca_spml_base_module_memuse_hook_fn_t)(void *, size_t);

/**
 *  SPML instance.
 */
struct mca_spml_base_module_1_0_0_t {

    mca_spml_base_module_add_procs_fn_t spml_add_procs;
    mca_spml_base_module_del_procs_fn_t spml_del_procs;

    mca_spml_base_module_enable_fn_t spml_enable;
    mca_spml_base_module_register_fn_t spml_register;
    mca_spml_base_module_deregister_fn_t spml_deregister;
    mca_spml_base_module_oob_get_mkeys_fn_t spml_oob_get_mkeys;

    mca_spml_base_module_ctx_create_fn_t spml_ctx_create;
    mca_spml_base_module_ctx_destroy_fn_t spml_ctx_destroy;

    mca_spml_base_module_put_fn_t spml_put;
    mca_spml_base_module_put_nb_fn_t spml_put_nb;
    mca_spml_base_module_put_signal_fn_t spml_put_signal;
    mca_spml_base_module_put_signal_nb_fn_t spml_put_signal_nb;

    mca_spml_base_module_get_fn_t spml_get;
    mca_spml_base_module_get_nb_fn_t spml_get_nb;

    mca_spml_base_module_recv_fn_t spml_recv;
    mca_spml_base_module_send_fn_t spml_send;

    mca_spml_base_module_wait_fn_t                   spml_wait;
    mca_spml_base_module_wait_nb_fn_t                spml_wait_nb;
    mca_spml_base_module_wait_until_all_fn_t         spml_wait_until_all;
    mca_spml_base_module_wait_until_any_fn_t         spml_wait_until_any;
    mca_spml_base_module_wait_until_some_fn_t        spml_wait_until_some;
    mca_spml_base_module_wait_until_all_vector_fn_t  spml_wait_until_all_vector;
    mca_spml_base_module_wait_until_any_vector_fn_t  spml_wait_until_any_vector;
    mca_spml_base_module_wait_until_some_vector_fn_t spml_wait_until_some_vector;

    mca_spml_base_module_test_fn_t                   spml_test;
    mca_spml_base_module_test_all_fn_t               spml_test_all;
    mca_spml_base_module_test_any_fn_t               spml_test_any;
    mca_spml_base_module_test_some_fn_t              spml_test_some;
    mca_spml_base_module_test_all_vector_fn_t        spml_test_all_vector;
    mca_spml_base_module_test_any_vector_fn_t        spml_test_any_vector;
    mca_spml_base_module_test_some_vector_fn_t       spml_test_some_vector;

    mca_spml_base_module_team_sync_fn_t              spml_team_sync;
    mca_spml_base_module_team_my_pe_fn_t             spml_team_my_pe;
    mca_spml_base_module_team_n_pes_fn_t             spml_team_n_pes;
    mca_spml_base_module_team_get_config_fn_t        spml_team_get_config;
    mca_spml_base_module_team_translate_pe_fn_t      spml_team_translate_pe;
    mca_spml_base_module_team_split_strided_fn_t     spml_team_split_strided;
    mca_spml_base_module_team_split_2d_fn_t          spml_team_split_2d;
    mca_spml_base_module_team_destroy_fn_t           spml_team_destroy;
    mca_spml_base_module_team_get_fn_t               spml_team_get;
    mca_spml_base_module_team_create_ctx_fn_t        spml_team_create_ctx;

    mca_spml_base_module_team_alltoall_fn_t          spml_team_alltoall;
    mca_spml_base_module_team_alltoalls_fn_t         spml_team_alltoalls;
    mca_spml_base_module_team_broadcast_fn_t         spml_team_broadcast;
    mca_spml_base_module_team_collect_fn_t           spml_team_collect;
    mca_spml_base_module_team_fcollect_fn_t          spml_team_fcollect;
    mca_spml_base_module_team_reduce_fn_t            spml_team_reduce;

    mca_spml_base_module_fence_fn_t spml_fence;
    mca_spml_base_module_quiet_fn_t spml_quiet;

    mca_spml_base_module_mkey_unpack_fn_t spml_rmkey_unpack;
    mca_spml_base_module_mkey_free_fn_t   spml_rmkey_free;
    mca_spml_base_module_mkey_ptr_fn_t    spml_rmkey_ptr;

    mca_spml_base_module_memuse_hook_fn_t spml_memuse_hook;
    mca_spml_base_module_put_all_nb_fn_t  spml_put_all_nb;
    void *self;
};

typedef struct mca_spml_base_module_1_0_0_t mca_spml_base_module_1_0_0_t;
typedef mca_spml_base_module_1_0_0_t mca_spml_base_module_t;

/*
 * Macro for use in components that are of type spml
 */
#define MCA_SPML_BASE_VERSION_2_0_0 \
    OSHMEM_MCA_BASE_VERSION_2_1_0("spml", 2, 0, 0)

/*
 * macro for doing direct call / call through struct
 */
#if MCA_oshmem_spml_DIRECT_CALL

#include MCA_oshmem_spml_DIRECT_CALL_HEADER

#define MCA_SPML_CALL_STAMP(a, b) mca_spml_ ## a ## _ ## b
#define MCA_SPML_CALL_EXPANDER(a, b) MCA_SPML_CALL_STAMP(a,b)
#define MCA_SPML_CALL(a) MCA_SPML_CALL_EXPANDER(MCA_oshmem_spml_DIRECT_CALL_COMPONENT, a)

#else
#define MCA_SPML_CALL(a) mca_spml.spml_ ## a
#endif

OSHMEM_DECLSPEC extern mca_spml_base_module_t mca_spml;

END_C_DECLS
#endif /* MCA_SPML_H */
