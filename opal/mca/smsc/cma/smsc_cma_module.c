/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2010-2014 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014-2018 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2020      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2021      Google, Inc. All rights reserved.
 * Copyright (c) 2022      IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "opal_config.h"

#include "opal/mca/pmix/pmix-internal.h"
#include "opal/mca/smsc/base/base.h"
#include "opal/mca/smsc/cma/smsc_cma_internal.h"

#if HAVE_LINUX_KCMP_H
#    include <linux/kcmp.h>       /* kcmp: Definition of KCMP_* constants */
#endif /* HAVE_LINUX_KCMP_H */
#if HAVE_SYS_SYSCALL_H
#    include <sys/syscall.h>      /* kcmp: Definition of SYS_* constants */
#endif /* HAVE_SYS_SYSCALL_H */

#if OPAL_CMA_NEED_SYSCALL_DEFS
#    include "opal/sys/cma.h"
#else
#    include <sys/uio.h>
#endif /* OPAL_CMA_NEED_SYSCALL_DEFS */

OBJ_CLASS_INSTANCE(mca_smsc_cma_endpoint_t, opal_object_t, NULL, NULL);

mca_smsc_endpoint_t *mca_smsc_cma_get_endpoint(opal_proc_t *peer_proc)
{
    mca_smsc_cma_endpoint_t *endpoint = OBJ_NEW(mca_smsc_cma_endpoint_t);
    if (OPAL_UNLIKELY(NULL == endpoint)) {
        return NULL;
    }

    endpoint->super.proc = peer_proc;

    int rc;
    size_t modex_size;
    mca_smsc_cma_modex_t *modex;
    OPAL_MODEX_RECV_IMMEDIATE(rc, &mca_smsc_cma_component.smsc_version, &peer_proc->proc_name,
                              (void **) &modex, &modex_size);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
        OBJ_RELEASE(endpoint);
        return NULL;
    }

    ino_t my_ns_id = mca_smsc_cma_get_user_ns_id();
    if (modex->user_ns_id != my_ns_id) {
        opal_output_verbose(MCA_BASE_VERBOSE_ERROR, opal_smsc_base_framework.framework_output,
                            "mca_smsc_cma_module_get_endpoint: can not proceed. processes are in "
                            "difference namespaces");
        /* can't use CMA with this peer */
        OBJ_RELEASE(endpoint);
        free(modex);
        return NULL;
    }

#if OPAL_CMA_KCMP_AVAIL
    /* Check if CAP_SYS_PTRACE capability is allowed between these two processes
     * Calling process_vm_readv/writev requires CAP_SYS_PTRACE. We can use kcmp
     * to check if these two processes share a kernel resource. Since kcmp
     * also requires CAP_SYS_PTRACE it is a good proxy for process_vm_readv/writev.
     */
    rc = syscall(SYS_kcmp, getpid(), modex->pid, KCMP_VM, 0, 0);
    if(rc < 0) {
        opal_output_verbose(MCA_BASE_VERBOSE_ERROR, opal_smsc_base_framework.framework_output,
                            "mca_smsc_cma_module_get_endpoint: can not proceed. processes do not have "
                            "the necessary permissions (i.e., CAP_SYS_PTRACE). "
                            "PID %d <-> %d (rc = %d) (errno: %d: %s)",
                            getpid(), modex->pid, rc, errno, strerror(errno));
        /* can't use CMA with this peer */
        OBJ_RELEASE(endpoint);
        free(modex);
        return NULL;
    }
#endif /* OPAL_CMA_KCMP_AVAIL */

    endpoint->pid = modex->pid;
    return &endpoint->super;
}

void mca_smsc_cma_return_endpoint(mca_smsc_endpoint_t *endpoint)
{
    OBJ_RELEASE(endpoint);
}

static inline void mca_smsc_cma_iov_advance(struct iovec *iov, ssize_t length)
{
    iov->iov_base = (void *) ((uintptr_t) iov->iov_base + length);
    iov->iov_len -= length;
}

int mca_smsc_cma_copy_to(mca_smsc_endpoint_t *endpoint, void *local_address, void *remote_address,
                         size_t size, void *reg_handle)
{
    /* ignore the registration handle as it is not used for CMA */
    (void) reg_handle;

    mca_smsc_cma_endpoint_t *cma_endpoint = (mca_smsc_cma_endpoint_t *) endpoint;

    /*
     * According to the man page :
     * "On success, process_vm_readv() returns the number of bytes read and
     * process_vm_writev() returns the number of bytes written.  This return
     * value may be less than the total number of requested bytes, if a
     * partial read/write occurred.  (Partial transfers apply at the
     * granularity of iovec elements.  These system calls won't perform a
     * partial transfer that splits a single iovec element.)".
     * So since we use a single iovec element, the returned size should either
     * be 0 or size, and the do loop should not be needed here.
     * We tried on various Linux kernels with size > 2 GB, and surprisingly,
     * the returned value is always 0x7ffff000 (fwiw, it happens to be the size
     * of the larger number of pages that fits a signed 32 bits integer).
     * We do not know whether this is a bug from the kernel, the libc or even
     * the man page, but for the time being, we do as is process_vm_readv() could
     * return any value.
     */
    struct iovec src_iov = {
        .iov_base = local_address,
        .iov_len = size,
    };
    struct iovec dst_iov = {
        .iov_base = remote_address,
        .iov_len = size,
    };
    ssize_t ret;
    do {
        ret = process_vm_writev(cma_endpoint->pid, &src_iov, 1, &dst_iov, 1, 0);
        if (0 > ret) {
            if (ESRCH == errno) {
                OPAL_OUTPUT_VERBOSE((MCA_BASE_VERBOSE_ERROR,
                                     opal_smsc_base_framework.framework_output,
                                     "CMA wrote %ld, expected %lu, errno = %d", (long) ret,
                                     (unsigned long) size, errno));
                return OPAL_ERROR;
            }
            OPAL_OUTPUT_VERBOSE((MCA_BASE_VERBOSE_ERROR, opal_smsc_base_framework.framework_output,
                                 "CMA wrote %ld, expected %lu, errno = %d", (long) ret,
                                 (unsigned long) size, errno));
            return OPAL_ERROR;
        }
        mca_smsc_cma_iov_advance(&src_iov, ret);
        mca_smsc_cma_iov_advance(&dst_iov, ret);
    } while (0 < src_iov.iov_len);

    return OPAL_SUCCESS;
}

int mca_smsc_cma_copy_from(mca_smsc_endpoint_t *endpoint, void *local_address, void *remote_address,
                           size_t size, void *reg_handle)
{
    /* ignore the registration handle as it is not used for CMA */
    (void) reg_handle;

    mca_smsc_cma_endpoint_t *cma_endpoint = (mca_smsc_cma_endpoint_t *) endpoint;

    /*
     * According to the man page :
     * "On success, process_vm_readv() returns the number of bytes read and
     * process_vm_writev() returns the number of bytes written.  This return
     * value may be less than the total number of requested bytes, if a
     * partial read/write occurred.  (Partial transfers apply at the
     * granularity of iovec elements.  These system calls won't perform a
     * partial transfer that splits a single iovec element.)".
     * So since we use a single iovec element, the returned size should either
     * be 0 or size, and the do loop should not be needed here.
     * We tried on various Linux kernels with size > 2 GB, and surprisingly,
     * the returned value is always 0x7ffff000 (fwiw, it happens to be the size
     * of the larger number of pages that fits a signed 32 bits integer).
     * We do not know whether this is a bug from the kernel, the libc or even
     * the man page, but for the time being, we do as is process_vm_readv() could
     * return any value.
     */
    struct iovec src_iov = {
        .iov_base = remote_address,
        .iov_len = size,
    };
    struct iovec dst_iov = {
        .iov_base = local_address,
        .iov_len = size,
    };
    ssize_t ret;
    do {
        ret = process_vm_readv(cma_endpoint->pid, &dst_iov, 1, &src_iov, 1, 0);
        if (0 > ret) {
            if (ESRCH == errno) {
                OPAL_OUTPUT_VERBOSE((MCA_BASE_VERBOSE_ERROR,
                                     opal_smsc_base_framework.framework_output,
                                     "CMA read %ld, expected %lu, errno = %d", (long) ret,
                                     (unsigned long) size, errno));
                return OPAL_ERROR;
            }
            OPAL_OUTPUT_VERBOSE((MCA_BASE_VERBOSE_ERROR, opal_smsc_base_framework.framework_output,
                                 "CMA read %ld, expected %lu, errno = %d\n", (long) ret,
                                 (unsigned long) size, errno));
            return OPAL_ERROR;
        }
        mca_smsc_cma_iov_advance(&src_iov, ret);
        mca_smsc_cma_iov_advance(&dst_iov, ret);
    } while (0 < src_iov.iov_len);

    return OPAL_SUCCESS;
}

/* unsupported interfaces defined to support MCA direct */
void *mca_smsc_cma_map_peer_region(mca_smsc_endpoint_t *endpoint, uint64_t flags,
                                   void *remote_address, size_t size, void **local_mapping)
{
    return NULL;
}

void mca_smsc_cma_unmap_peer_region(void *ctx)
{
}

void *mca_smsc_cma_register_region(void *local_address, size_t size)
{
    return NULL;
}

void mca_smsc_cma_deregister_region(void *reg_data)
{
}

mca_smsc_module_t mca_smsc_cma_module = {
    .get_endpoint = mca_smsc_cma_get_endpoint,
    .return_endpoint = mca_smsc_cma_return_endpoint,
    .copy_to = mca_smsc_cma_copy_to,
    .copy_from = mca_smsc_cma_copy_from,
};
