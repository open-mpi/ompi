/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#include "ad_pvfs2.h"

#include "adio.h"

struct ADIOI_Fns_struct ADIO_PVFS2_operations = {
    ADIOI_PVFS2_Open,   /* Open */
    ADIOI_SCALEABLE_OpenColl,   /* OpenColl */
    ADIOI_PVFS2_ReadContig,     /* ReadContig */
    ADIOI_PVFS2_WriteContig,    /* WriteContig */
    ADIOI_GEN_ReadStridedColl,  /* ReadStridedColl */
    ADIOI_GEN_WriteStridedColl, /* WriteStridedColl */
    ADIOI_GEN_SeekIndividual,   /* SeekIndividual */
    ADIOI_PVFS2_Fcntl,  /* Fcntl */
    ADIOI_PVFS2_SetInfo,        /* SetInfo */
    ADIOI_PVFS2_ReadStrided,    /* ReadStrided */
    ADIOI_PVFS2_WriteStrided,   /* WriteStrided */
    ADIOI_PVFS2_Close,  /* Close */
#ifdef HAVE_MPI_GREQUEST_EXTENSIONS
    ADIOI_PVFS2_IReadContig,    /* IreadContig */
    ADIOI_PVFS2_IWriteContig,   /* IwriteContig */
#else
    ADIOI_FAKE_IreadContig,     /* IreadContig */
    ADIOI_FAKE_IwriteContig,    /* IwriteContig */
#endif
    ADIOI_FAKE_IODone,  /* ReadDone */
    ADIOI_FAKE_IODone,  /* WriteDone */
    ADIOI_FAKE_IOComplete,      /* ReadComplete */
    ADIOI_FAKE_IOComplete,      /* WriteComplete */
    ADIOI_FAKE_IreadStrided,    /* IreadStrided */
    ADIOI_FAKE_IwriteStrided,   /* IwriteStrided */
    ADIOI_PVFS2_Flush,  /* Flush */
    ADIOI_PVFS2_Resize, /* Resize */
    ADIOI_PVFS2_Delete, /* Delete */
    ADIOI_PVFS2_Feature,
    "PVFS2: the PVFS v2 or OrangeFS file systems",
    ADIOI_GEN_IreadStridedColl, /* IreadStridedColl */
    ADIOI_GEN_IwriteStridedColl,        /* IwriteStridedColl */
#if defined(F_SETLKW64)
    ADIOI_GEN_SetLock   /* SetLock */
#else
    ADIOI_GEN_SetLock64 /* SetLock */
#endif
};
