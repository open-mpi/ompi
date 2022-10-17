/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2010 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2008      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2010-2011 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2011      NVIDIA Corporation.  All rights reserved.
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2019      Triad National Security, LLC. All rights
 *                         reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include <errno.h>
#ifdef HAVE_FCNTL_H
#    include <fcntl.h>
#endif /* HAVE_FCNTL_H */
#include <string.h>
#if OPAL_HAVE_SOLARIS && !defined(_POSIX_C_SOURCE)
#    define _POSIX_C_SOURCE 200112L /* Required for shm_{open,unlink} decls */
#    include <sys/mman.h>
#    undef _POSIX_C_SOURCE
#else
#    ifdef HAVE_SYS_MMAN_H
#        include <sys/mman.h>
#    endif /* HAVE_SYS_MMAN_H */
#endif
#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif /* HAVE_UNISTD_H */
#ifdef HAVE_SYS_TYPES_H
#    include <sys/types.h>
#endif /* HAVE_SYS_TYPES_H */
#ifdef HAVE_NETDB_H
#    include <netdb.h>
#endif /* HAVE_NETDB_H */

#include "opal/mca/shmem/base/base.h"
#include "opal/mca/shmem/shmem.h"
#include "opal/runtime/opal.h"
#include "opal/util/output.h"
#include "opal/util/show_help.h"

#include "shmem_posix.h"
#include "shmem_posix_common_utils.h"

static char const lookup_table[] = {
    '+', '-', '0', '1', '2', '3', '4', '5',
    '6', '7', '8', '9', 'a', 'b', 'c', 'd',
    'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l',
    'm', 'n', 'o', 'p', 'q', 'r', 's', 't',
    'u', 'v', 'w', 'x', 'y', 'z', 'A', 'B',
    'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J',
    'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R',
    'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'
};

static size_t prepare_name(char * posix_file_name_buff, size_t size) {

    /* Get the process's pid and print in base-64. */
    uint32_t const pid = (uint32_t)getpid();
    char const p0 = lookup_table[(pid >> 18) & 0x3F];
    char const p1 = lookup_table[(pid >> 12) & 0x3F];
    char const p2 = lookup_table[(pid >>  6) & 0x3F];
    char const p3 = lookup_table[(pid >>  0) & 0x3F];

    /* Get the user's uid and print in base-64. */
    uint32_t const uid = (uint32_t)getuid();
    char const u0 = lookup_table[(uid >> 18) & 0x3F];
    char const u1 = lookup_table[(uid >> 12) & 0x3F];
    char const u2 = lookup_table[(uid >>  6) & 0x3F];
    char const u3 = lookup_table[(uid >>  0) & 0x3F];

    /* Combine prefix, uid, and pid in filename buffer. */
    int const l = snprintf(posix_file_name_buff, size, "%s%c%c%c%c%c%c%c%c",
                           OPAL_SHMEM_POSIX_FILE_NAME_PREFIX,
                           u0, u1, u2, u3, p0, p1, p2, p3);
    return l <= 0 ? 0 : l >= size ? size : (size_t)l;

}

/* ////////////////////////////////////////////////////////////////////////// */
int shmem_posix_shm_open(char *posix_file_name_buff, size_t size)
{
    int attempt = 0, fd = -1;

    /* Workaround for simultaneous posix shm_opens on the same node (e.g.
     * multiple Open MPI jobs sharing a node) by prefixing filename with
     * user and process IDs. Name collision during component runtime will
     * happen, so protect against it by trying several times and include
     * the try number in the filename.
     *   format: /ompixxxxyyyyzz
     *   where xxxx  is the lower 3 bytes of uid in base-64
     *         yyyy  is the lower 3 bytes of pid in base-64
     *         zz    is the try number in hexadecimal
     * See comment in shmem_posix.h that explains why we try to keep the
     * path this short.
     */

    size_t const prefix_length = prepare_name(posix_file_name_buff, size);
    char * const suffix = posix_file_name_buff + prefix_length;
    size_t const suffix_length = size - prefix_length;

    do {
        snprintf(suffix, suffix_length, "%02x", attempt++);
        /* the check for the existence of the object and its creation if it
         * does not exist are performed atomically.
         */
        if (-1 == (fd = shm_open(posix_file_name_buff, O_CREAT | O_EXCL | O_RDWR, 0600))) {
            int err = errno;
            /* the object already exists, so try again with a new name */
            if (EEXIST == err) {
                continue;
            }
            /* a "real" error occurred. fd is already set to -1, so get out
             * of here. we can't be selected :-(.
             */
            else {
                opal_output_verbose(10, opal_shmem_base_framework.framework_output,
                                    "shmem_posix_shm_open: disqualifying posix because "
                                    "shm_open(2) failed with error: %s (errno %d)\n",
                                    strerror(err), err);
                break;
            }
        }
        /* we found an available file name */
        else {
            break;
        }
    } while (attempt < OPAL_SHMEM_POSIX_MAX_ATTEMPTS);

    /* if we didn't find a name, let the user know that we tried and failed */
    if (attempt >= OPAL_SHMEM_POSIX_MAX_ATTEMPTS) {
        opal_output(0, "shmem: posix: file name search - max attempts exceeded."
                       "cannot continue with posix.\n");
    }
    return fd;
}
