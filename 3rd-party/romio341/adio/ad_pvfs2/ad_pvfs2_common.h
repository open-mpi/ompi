/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#ifndef AD_PVFS2_COMMON_H_INCLUDED
#define AD_PVFS2_COMMON_H_INCLUDED
#include "ad_pvfs2.h"

/* useful values:
 *  0:          no debugging
 *  CLIENT_DEBUG:   debug client state machines
 */
#define ADIOI_PVFS2_DEBUG_MASK 0


struct ADIOI_PVFS2_fs_s {
    PVFS_object_ref object_ref;
    PVFS_credentials credentials;
} ADIOI_PVFS2_fs_s;

typedef struct ADIOI_PVFS2_fs_s ADIOI_PVFS2_fs;


void ADIOI_PVFS2_Init(int *error_code);
void ADIOI_PVFS2_makeattribs(PVFS_sys_attr * attribs);
void ADIOI_PVFS2_makecredentials(PVFS_credentials * credentials);
void ADIOI_PVFS2_End(int *error_code);
int ADIOI_PVFS2_End_call(MPI_Comm comm, int keyval, void *attribute_val, void *extra_state);
int ADIOI_PVFS2_error_convert(int pvfs_error);

#endif /* AD_PVFS2_COMMON_H_INCLUDED */
