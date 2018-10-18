/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/* 
 *   ad_panfs.c
 *
 *   Copyright (C) 2001 University of Chicago.
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ad_panfs.h"

/* adioi.h has the ADIOI_Fns_struct define */
#include "adioi.h"

#ifndef ROMIOCONF_H_INCLUDED
#include "romioconf.h"
#define ROMIOCONF_H_INCLUDED
#endif


struct ADIOI_Fns_struct ADIO_PANFS_operations = {
#ifdef HAVE_PAN_FS_CLIENT_RAIDN_ENCODING_T
    ADIOI_PANFS_Open6, /* Open, using newer Panasas features */
#else
    ADIOI_PANFS_Open,  /* open, but using Panasas5 and earlier features */
#endif
    ADIOI_GEN_OpenColl,
    ADIOI_PANFS_ReadContig, /* ReadContig */
    ADIOI_PANFS_WriteContig, /* WriteContig */
    ADIOI_GEN_ReadStridedColl, /* ReadStridedColl */
    ADIOI_GEN_WriteStridedColl, /* WriteStridedColl */
    ADIOI_GEN_SeekIndividual, /* SeekIndividual */
    ADIOI_GEN_Fcntl, /* Fcntl */
    ADIOI_PANFS_SetInfo, /* SetInfo */
    ADIOI_GEN_ReadStrided, /* ReadStrided */
    ADIOI_GEN_WriteStrided, /* WriteStrided */
    ADIOI_GEN_Close, /* Close */
#ifdef ROMIO_HAVE_WORKING_AIO
    ADIOI_GEN_IreadContig, /* IreadContig */
    ADIOI_GEN_IwriteContig, /* IwriteContig */
#else
    ADIOI_FAKE_IreadContig, /* IreadContig */
    ADIOI_FAKE_IwriteContig, /* IwriteContig */
#endif
    ADIOI_GEN_IODone, /* ReadDone */
    ADIOI_GEN_IODone, /* WriteDone */
    ADIOI_GEN_IOComplete, /* ReadComplete */
    ADIOI_GEN_IOComplete, /* WriteComplete */
    ADIOI_GEN_IreadStrided, /* IreadStrided */
    ADIOI_GEN_IwriteStrided, /* IwriteStrided */
    ADIOI_GEN_Flush, /* Flush */
    ADIOI_PANFS_Resize, /* Resize */
    ADIOI_GEN_Delete, /* Delete */
    ADIOI_GEN_Feature,
    "PANFS: Panasas PanFS",
    ADIOI_GEN_IreadStridedColl, /* IreadStridedColl */
    ADIOI_GEN_IwriteStridedColl /* IwriteStridedColl */
};
