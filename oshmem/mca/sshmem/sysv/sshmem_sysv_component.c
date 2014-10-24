/*
 * Copyright (c) 2014      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "oshmem_config.h"

#ifdef HAVE_SYS_MMAN_H
#include <sys/mman.h>
#endif /* HAVE_SYS_MMAN_H */
#ifdef HAVE_STRING_H
#include <string.h>
#endif /* HAVE_STRING_H */
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */

#ifdef HAVE_SYS_IPC_H
#include <sys/ipc.h>
#endif /* HAVE_SYS_IPC_H */
#if HAVE_SYS_SHM_H
#include <sys/shm.h>
#endif /* HAVE_SYS_SHM_H */
#if HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif /* HAVE_SYS_STAT_H */

#include "opal/constants.h"
#include "opal/util/show_help.h"
#include "opal/util/output.h"
#include "opal/util/sys_limits.h"

#include "oshmem/mca/sshmem/sshmem.h"
#include "oshmem/mca/sshmem/base/base.h"

#include "sshmem_sysv.h"

/* public string showing the shmem ompi_sysv component version number */
const char *mca_sshmem_sysv_component_version_string =
    "OSHMEM sysv sshmem MCA component version " OSHMEM_VERSION;

/* local functions */
static int sysv_register (void);
static int sysv_open(void);
static int sysv_query(mca_base_module_t **module, int *priority);
static int sysv_runtime_query(mca_base_module_t **module,
                              int *priority,
                              const char *hint);

/* instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */
mca_sshmem_sysv_component_t mca_sshmem_sysv_component = {
    /* ////////////////////////////////////////////////////////////////////// */
    /* super */
    /* ////////////////////////////////////////////////////////////////////// */
    {
        /* common MCA component data */
        {
            MCA_SSHMEM_BASE_VERSION_2_0_0,

            /* component name and version */
            "sysv",
            OSHMEM_MAJOR_VERSION,
            OSHMEM_MINOR_VERSION,
            OSHMEM_RELEASE_VERSION,

            /* component open */
            sysv_open,
            /* component close */
            NULL,
            /* component query */
            sysv_query,
            sysv_register
        },
        /* MCA v2.0.0 component meta data */
        {
            /* the component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },
        sysv_runtime_query,
    },
    /* ////////////////////////////////////////////////////////////////////// */
    /* sysv component-specific information */
    /* see: shmem_sysv.h for more information */
};

/* ////////////////////////////////////////////////////////////////////////// */
static int
sysv_runtime_query(mca_base_module_t **module,
                   int *priority,
                   const char *hint)
{
    char c     = 'j';
    int shmid  = -1;
    char *a    = NULL;
    char *addr = NULL;
    struct shmid_ds tmp_buff;
    int flags;
    int ret;

    ret = OSHMEM_SUCCESS;

    *priority = 0;
    *module = NULL;

    /* if we are here, then let the run-time test games begin */

#if defined (SHM_HUGETLB)
    if (mca_sshmem_sysv_component.use_hp != 0) {
         flags = IPC_CREAT | IPC_EXCL | S_IRUSR | S_IWUSR | SHM_HUGETLB;
        if (-1 == (shmid = shmget(IPC_PRIVATE, sshmem_sysv_gethugepagesize(), flags))) {
            if (mca_sshmem_sysv_component.use_hp == 1) {
                mca_sshmem_sysv_component.use_hp = 0;
                ret = OSHMEM_ERR_NOT_AVAILABLE;
                goto out;
            }
            mca_sshmem_sysv_component.use_hp = 0;
        }
        else if ((void *)-1 == (addr = shmat(shmid, NULL, 0))) {
            shmctl(shmid, IPC_RMID, NULL);
            if (mca_sshmem_sysv_component.use_hp == 1) {
                mca_sshmem_sysv_component.use_hp = 0;
                ret = OSHMEM_ERR_NOT_AVAILABLE;
                goto out;
            }
            mca_sshmem_sysv_component.use_hp = 0;
        }
    }
#else
    if (mca_sshmem_sysv_component.use_hp == 1) {
        mca_sshmem_sysv_component.use_hp = 0;
        ret = OSHMEM_ERR_NOT_AVAILABLE;
        goto out;
    }
    mca_sshmem_sysv_component.use_hp = 0;
#endif

    if (0 == mca_sshmem_sysv_component.use_hp) {
        flags = IPC_CREAT | IPC_EXCL | S_IRUSR | S_IWUSR;
        if (-1 == (shmid = shmget(IPC_PRIVATE, (size_t)(opal_getpagesize()), flags))) {
            ret = OSHMEM_ERR_NOT_AVAILABLE;
            goto out;
        }
        else if ((void *)-1 == (addr = shmat(shmid, NULL, 0))) {
            shmctl(shmid, IPC_RMID, NULL);
            ret = OSHMEM_ERR_NOT_AVAILABLE;
            goto out;
        }
    }

    /* protect against lazy establishment - may not be needed, but can't hurt */
    a = addr;
    *a = c;

    if (-1 == shmctl(shmid, IPC_RMID, NULL)) {
        goto out;
    }
    else if (-1 == shmctl(shmid, IPC_STAT, &tmp_buff)) {
        goto out;
    }
    /* all is well - rainbows and butterflies */
    else {
        *priority = mca_sshmem_sysv_component.priority;
        *module = (mca_base_module_t *)&mca_sshmem_sysv_module.super;
    }

  out:
    if ((char *)-1 != addr) {
        shmdt(addr);
    }
    return ret;
}

/* ////////////////////////////////////////////////////////////////////////// */
static int
sysv_register(void)
{
    /* ////////////////////////////////////////////////////////////////////// */
    /* (default) priority - set lower than mmap's priority */
    mca_sshmem_sysv_component.priority = 30;
    (void) mca_base_component_var_register(&mca_sshmem_sysv_component.super.base_version,
                                           "priority", "Priority for the sshmem sysv "
                                           "component (default: 30)", MCA_BASE_VAR_TYPE_INT,
                                           NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_3,
                                           MCA_BASE_VAR_SCOPE_ALL_EQ,
                                           &mca_sshmem_sysv_component.priority);

    mca_sshmem_sysv_component.use_hp = -1;
    mca_base_component_var_register (&mca_sshmem_sysv_component.super.base_version,
                                           "use_hp", "Huge pages usage "
                                           "[0 - off, 1 - on, -1 - auto] (default: -1)", MCA_BASE_VAR_TYPE_INT,
                                           NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_4,
                                           MCA_BASE_VAR_SCOPE_ALL_EQ,
                                           &mca_sshmem_sysv_component.use_hp);

    return OSHMEM_SUCCESS;
}

/* ////////////////////////////////////////////////////////////////////////// */
static int
sysv_open(void)
{
    return OSHMEM_SUCCESS;
}

/* ////////////////////////////////////////////////////////////////////////// */
static int
sysv_query(mca_base_module_t **module, int *priority)
{
    *priority = mca_sshmem_sysv_component.priority;
    *module = (mca_base_module_t *)&mca_sshmem_sysv_module.super;
    return OSHMEM_SUCCESS;
}

