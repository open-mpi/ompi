/* oshmem/include/shmem-compat.h. This file contains OpenSHMEM lagacy API  */
/*
 * Copyright (c) 2014      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OSHMEM_SHMEM_COMPAT_H
#define OSHMEM_SHMEM_COMPAT_H

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/*
 * Legacy API
 */
OSHMEM_DECLSPEC  int num_pes(void);
OSHMEM_DECLSPEC  int my_pe(void);

/* old init/destruct functions - not in the open shmem spec but still supported */
OSHMEM_DECLSPEC  void shmem_init(void);
OSHMEM_DECLSPEC  void shmem_finalize(void) OSHMEM_DESTRUCTOR;
OSHMEM_DECLSPEC  int shmem_n_pes(void);
OSHMEM_DECLSPEC  int shmem_my_pe(void);

OSHMEM_DECLSPEC  void shmem_put(void *target, const void *source, size_t len, int pe);
OSHMEM_DECLSPEC  void shmem_get(void *target, const void *source, size_t len, int pe);
OSHMEM_DECLSPEC  void globalexit(int status);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* OSHMEM_SHMEM_COMPAT_H */
