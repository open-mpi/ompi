/* ---------------------------------------------------------------- */
/* (C)Copyright IBM Corp.  2007, 2008                               */
/* ---------------------------------------------------------------- */
/**
 * \file ad_gpfs.c
 * \brief ???
 */

/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   Copyright (C) 2001 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */
#include "ad_gpfs.h"

/* adioi.h has the ADIOI_Fns_struct define */
#include "adioi.h"

struct ADIOI_Fns_struct ADIO_GPFS_operations = {
    ADIOI_GPFS_Open, /* Open */
    ADIOI_GEN_OpenColl, /* Collective open */
    ADIOI_GEN_ReadContig, /* ReadContig */
    ADIOI_GEN_WriteContig, /* WriteContig */
    ADIOI_GPFS_ReadStridedColl, /* ReadStridedColl */
    ADIOI_GPFS_WriteStridedColl, /* WriteStridedColl */
    ADIOI_GEN_SeekIndividual, /* SeekIndividual */
    ADIOI_GEN_Fcntl, /* Fcntl */
#if defined(BGQPLATFORM) || defined(PEPLATFORM)
    ADIOI_GPFS_SetInfo, /* SetInfo for BlueGene or PE */
#else
    ADIOI_GEN_SetInfo, /* SetInfo for any platform besides BlueGene or PE */
#endif
    ADIOI_GEN_ReadStrided, /* ReadStrided */
    ADIOI_GEN_WriteStrided, /* WriteStrided */
    ADIOI_GPFS_Close, /* Close */
#ifdef ROMIO_HAVE_WORKING_AIO
#warning Consider BG support for NFS before enabling this.
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
    ADIOI_GPFS_Flush, /* Flush */
    ADIOI_GEN_Resize, /* Resize */
    ADIOI_GEN_Delete, /* Delete */
    ADIOI_GEN_Feature, /* Features */
#ifdef BGQPLATFORM
    "GPFS+BGQ: IBM GPFS for Blue Gene",
#elif PEPLATFORM
    "GPFS+PE: IBM GPFS for PE",
#else
    "GPFS: IBM GPFS",
#endif
    ADIOI_GEN_IreadStridedColl, /* IreadStridedColl */
    ADIOI_GEN_IwriteStridedColl /* IwriteStridedColl */
};
