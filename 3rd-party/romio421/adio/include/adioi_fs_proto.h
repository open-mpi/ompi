/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */


#ifndef ADIOI_FS_PROTO_H_INCLUDED
#define ADIOI_FS_PROTO_H_INCLUDED

#ifdef ROMIO_NFS
extern struct ADIOI_Fns_struct ADIO_NFS_operations;
/* prototypes are in adio/ad_nfs/ad_nfs.h */
#endif

#ifdef ROMIO_PANFS
extern struct ADIOI_Fns_struct ADIO_PANFS_operations;
/* prototypes are in adio/ad_panfs/ad_panfs.h */
#endif

#ifdef ROMIO_UFS
extern struct ADIOI_Fns_struct ADIO_UFS_operations;
/* prototypes are in adio/ad_ufs/ad_ufs.h */
#endif

#ifdef ROMIO_XFS
extern struct ADIOI_Fns_struct ADIO_XFS_operations;
/* prototypes are in adio/ad_xfs/ad_xfs.h */
#endif

#ifdef ROMIO_LUSTRE
extern struct ADIOI_Fns_struct ADIO_LUSTRE_operations;
/* prototypes are in adio/ad_lustre/ad_lustre.h */
#endif

#ifdef ROMIO_PVFS2
extern struct ADIOI_Fns_struct ADIO_PVFS2_operations;
/* prototypes are in adio/ad_pvfs2/ad_pvfs2.h */
#endif

#ifdef ROMIO_TESTFS
extern struct ADIOI_Fns_struct ADIO_TESTFS_operations;
/* prototypes are in adio/ad_testfs/ad_testfs.h */
#endif

#ifdef ROMIO_GPFS
extern struct ADIOI_Fns_struct ADIO_GPFS_operations;
/* prototypes are in adio/ad_gpfs/ad_gpfs.h */
#endif

#ifdef ROMIO_IME
/* prototypes are in adio/ad_im/ad_im.h */
extern struct ADIOI_Fns_struct ADIO_IME_operations;
#endif

#ifdef ROMIO_DAOS
/* prototypes are in adio/ad_daos/ad_daos.h */
extern struct ADIOI_Fns_struct ADIO_DAOS_operations;
#endif

#ifdef ROMIO_QUOBYTEFS
/* prototypes are in adio/ad_quobytefs/ad_quobytefs.h */
extern struct ADIOI_Fns_struct ADIO_QUOBYTEFS_operations;
extern void ADIOI_QUOBYTEFS_CreateAdapter(const char *, int *);
#endif

#endif /* ADIOI_FS_PROTO_H_INCLUDED */
