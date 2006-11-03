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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"

/* This component will only be compiled on Linux, where we are
   guaranteed to have <unistd.h> and friends */
#include <stdio.h>

/* the affinity macros are only exposed if __USE_GNU is set (at least,
   on Suse Linux for PPC).  However, __USE_GNU causes problems for
   some compilers (IBM XL) with stdio.h.  So include stdio.h, then
   set __USE_GNU. */
#ifndef __USE_GNU
#define __USE_GNU 1
#endif
#include <sched.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#include "opal/constants.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/paffinity/paffinity.h"
#include "opal/mca/paffinity/base/base.h"
#include "paffinity_linux.h"


/*
 * Local functions
 */
static int linux_module_init(void);
static int linux_module_get_num_procs(int *num_procs);
static int linux_module_set(int id);
static int linux_module_get(int *id);

/*
 * Linux paffinity module
 */
static const opal_paffinity_base_module_1_0_0_t module = {

    /* Initialization function */

    linux_module_init,

    /* Module function pointers */

    linux_module_get_num_procs,
    linux_module_set,
    linux_module_get
};


const opal_paffinity_base_module_1_0_0_t *
opal_paffinity_linux_component_query(int *query)
{
    int param;

    param = mca_base_param_find("paffinity", "linux", "priority");
    mca_base_param_lookup_int(param, query);

    return &module;
}


static int linux_module_init(void)
{
    /* Nothing to do */

    return OPAL_SUCCESS;
}


static int linux_module_get_num_procs(int *num_procs)
{
    *num_procs = sysconf(_SC_NPROCESSORS_ONLN);
    return OPAL_SUCCESS;
}


/************************************************************************
   See the note in paffinity_linux.h -- there are at least 3 different
   ways that Linux's sched_setaffinity()/sched_getaffinity() are
   implemented.  Hence, rather than trying to pepper #if's all
   throughout the code, just have multiple implementations of these
   module functions.
 ************************************************************************/

#if !defined(HAVE_CPU_SET_T)

/************************************************************************
   If we don't have cpu_set_t, then we have the "old style" Linux
   sched_setaffinity():

   int sched_setaffinity(pid_t pid, unsigned int len, unsigned long
   *mask);
   int sched_getaffinity(pid_t pid, unsigned int len, unsigned long
   *mask);

   We do not have the CPU_ZERO(), CPU_SET(), CPU_ISSET(), etc. macros.
 ************************************************************************/

static int make_mask(unsigned int *len, unsigned long **mask)
{
    int num_procs;

    linux_module_get_num_procs(&num_procs);
    *len = num_procs / 8;
    if (*len != (unsigned int)(num_procs * 8)) {
        ++*len;
    }

    /* Ensure *len is a multiple of sizeof(long) */

    if (*len != (*len / sizeof(long)) * sizeof(long)) {
        *len += sizeof(long) - (*len % sizeof(long));
    }

    /* Malloc */

    *mask = malloc(*len);
    if (NULL == *mask) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }
    memset(*mask, 0, *len);

    return OPAL_SUCCESS;
}


static int linux_module_set(int id)
{
    int ret, num_procs, byte, bit;
    unsigned int len;
    unsigned long *mask;

    linux_module_get_num_procs(&num_procs);
    if (id >= num_procs || id < 0) {
        return OPAL_ERR_BAD_PARAM;
    }

    if (OPAL_SUCCESS != (ret = make_mask(&len, &mask))) {
        return ret;
    }
    byte = id / 8;
    bit = id % 8;
    ((char *) mask)[byte] = 1 << bit;

    ret = sched_setaffinity(0, len, mask);

    bit = errno;
    free(mask);
    errno = bit;
    return (0 == ret) ? OPAL_SUCCESS : OPAL_ERR_IN_ERRNO;
}


static int linux_module_get(int *id)
{
    int ret, save, byte, bit;
    unsigned int i, len;
    unsigned long *mask;

    if (OPAL_SUCCESS != (ret = make_mask(&len, &mask))) {
        return ret;
    }

    if (0 != sched_getaffinity(0, len, mask)) {
        save = errno;
        free(mask);
        errno = save;
        return OPAL_ERR_IN_ERRNO;
    }
    *id = -1;
    for (i = 0; i < sizeof(mask); ++i) {
        byte = i / 8;
        bit = i % 8;
        if (0 != (mask[byte] & (1 << bit))) {
            *id = i;
            break;
        }
    }
    free(mask);
    if (-1 == *id) {
        return OPAL_ERR_NOT_FOUND;
    }

    return OPAL_SUCCESS;
}

#else

/************************************************************************
   In this case, we're using the following prototypes for the affinity
   functions (case 2)

   int sched_setaffinity (pid_t __pid, size_t __cpusetsize, const
   cpu_set_t *__cpuset);
   int sched_getaffinity (pid_t __pid, size_t __cpusetsize, const
   cpu_set_t *__cpuset);

   or

   int sched_setaffinity (pid_t __pid, const cpu_set_t *__cpuset);
   int sched_getaffinity (pid_t __pid, const cpu_set_t *__cpuset);

   The only difference is the missing size_t parameter, which is easy
   to determine inline, below.

   We also have the CPU_ZERO(), CPU_SET(), CPU_ISSET(), etc. macros
   (but CPU_ZERO() may not work, so use the indirection
   OMPI_CPU_ZERO()).
 ************************************************************************/

static int linux_module_set(int id)
{
    int num_procs;
    cpu_set_t mask;

    linux_module_get_num_procs(&num_procs);
    if (id >= num_procs || id < 0) {
        return OPAL_ERR_BAD_PARAM;
    }

    OMPI_CPU_ZERO(&mask);
    CPU_SET(id, &mask);

    if (0 != sched_setaffinity(0, 
#if OPAL_PAFFINITY_LINUX_SCHED_SETAFF_NUM_PARAMS == 3
                               sizeof(mask), 
#endif
                               &mask)) {
        return OPAL_ERR_IN_ERRNO;
    }
    return OPAL_SUCCESS;
}


static int linux_module_get(int *id)
{
    unsigned int i;
    cpu_set_t mask;

    OMPI_CPU_ZERO(&mask);
    if (0 != sched_getaffinity(0, 
#if OPAL_PAFFINITY_LINUX_SCHED_SETAFF_NUM_PARAMS == 3
                               sizeof(mask), 
#endif
                               &mask)) {
        return OPAL_ERR_IN_ERRNO;
    }
    *id = -1;
    for (i = 0; i < sizeof(mask); ++i) {
        if (CPU_ISSET(i, &mask)) {
            *id = i;
            break;
        }
    }
    if (-1 == *id) {
        return OPAL_ERR_NOT_FOUND;
    }

    return OPAL_SUCCESS;
}

#endif
