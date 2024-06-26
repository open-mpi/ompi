/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#include "ad_lustre.h"

struct ADIOI_Fns_struct ADIO_LUSTRE_operations = {
    ADIOI_LUSTRE_Open,  /* Open */
    ADIOI_GEN_OpenColl, /* OpenColl */
    ADIOI_LUSTRE_ReadContig,    /* ReadContig */
    ADIOI_LUSTRE_WriteContig,   /* WriteContig */
    ADIOI_GEN_ReadStridedColl,  /* ReadStridedColl */
    ADIOI_LUSTRE_WriteStridedColl,      /* WriteStridedColl */
    ADIOI_GEN_SeekIndividual,   /* SeekIndividual */
    ADIOI_GEN_Fcntl,    /* Fcntl */
    ADIOI_LUSTRE_SetInfo,       /* SetInfo */
    ADIOI_GEN_ReadStrided,      /* ReadStrided */
    ADIOI_LUSTRE_WriteStrided,  /* WriteStrided */
    ADIOI_GEN_Close,    /* Close */
#if defined(ROMIO_HAVE_WORKING_AIO) && !defined(CRAY_XT_LUSTRE)
    ADIOI_GEN_IreadContig,      /* IreadContig */
    ADIOI_GEN_IwriteContig,     /* IwriteContig */
#else
    ADIOI_FAKE_IreadContig,     /* IreadContig */
    ADIOI_FAKE_IwriteContig,    /* IwriteContig */
#endif
    ADIOI_GEN_IODone,   /* ReadDone */
    ADIOI_GEN_IODone,   /* WriteDone */
    ADIOI_GEN_IOComplete,       /* ReadComplete */
    ADIOI_GEN_IOComplete,       /* WriteComplete */
    ADIOI_GEN_IreadStrided,     /* IreadStrided */
    ADIOI_GEN_IwriteStrided,    /* IwriteStrided */
    ADIOI_GEN_Flush,    /* Flush */
    ADIOI_GEN_Resize,   /* Resize */
    ADIOI_GEN_Delete,   /* Delete */
    ADIOI_GEN_Feature,  /* Features */
    "LUSTRE:",
    ADIOI_GEN_IreadStridedColl, /* IreadStridedColl */
    ADIOI_GEN_IwriteStridedColl,        /* IwriteStridedColl */
#if defined(F_SETLKW64)
    ADIOI_GEN_SetLock   /* SetLock */
#else
    ADIOI_GEN_SetLock64 /* SetLock */
#endif
};
