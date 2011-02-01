/* -*- Mode: C; c-basic-offset:4 ; -*-
 * vim: ts=8 sts=4 sw=4 noexpandtab
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#ifndef _AD_ZOIDFS_COMMON_H
#define _AD_ZOIDFS_COMMON_H
#include "ad_zoidfs.h"

/* The ESTALE problem: 
 * The IO forwarding protocol can respond to any call with ESTALE, which means
 * the handle upon which that call operates has expired from the metadata
 * cache.  We thus wrap any zoidfs routine (expr) in this macro.  
 *
 * ROMIO stores the filename in the ADIOI_File structrue (fd), so we can always
 * re-lookup in response to ESTALE */

#define NO_STALE(ret, fd, handle_p, expr)                               \
    do {                                                                \
        (ret) = (expr);                                                 \
        while ((ret) == ZFSERR_STALE) {                                 \
            /* lookup again */                                          \
            (ret) = zoidfs_lookup(NULL, NULL, (fd)->filename,           \
                                  (zoidfs_handle_t*)((fd)->fs_ptr), ZOIDFS_NO_OP_HINT);    \
            if ((ret) == ZFS_OK) {                                      \
                *((ADIOI_ZOIDFS_object*)handle_p)                       \
                    = *((ADIOI_ZOIDFS_object*)((fd)->fs_ptr));          \
                /* re-execute the expr with new handle */               \
                (ret) = (expr);                                         \
            }                                                           \
        }                                                               \
    } while (0)

void ADIOI_ZOIDFS_Init(int rank, int *error_code );
void ADIOI_ZOIDFS_makeattribs(zoidfs_sattr_t * attribs);
void ADIOI_ZOIDFS_End(int *error_code);
int ADIOI_ZOIDFS_End_call(MPI_Comm comm, int keyval, 
	void *attribute_val, void *extra_state);
int ADIOI_ZOIDFS_error_convert(int error);

#endif
