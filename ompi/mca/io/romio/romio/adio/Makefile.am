#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

include $(top_srcdir)/Makefile.options

# Conditionals whether to build each subdir or not

if BUILD_GRIDFTP
GRIDFTP_DIR = ad_gridftp
GRIDFTP_LIB = ad_gridftp/libadio_gridftp.la
else
GRIDFTP_DIR =
GRIDFTP_LIB =
endif

if BUILD_HFS
HFS_DIR = ad_hfs
HFS_LIB = ad_hfs/libadio_hfs.la
else
HFS_DIR =
HFS_LIB =
endif

if BUILD_NFS
NFS_DIR = ad_nfs
NFS_LIB = ad_nfs/libadio_nfs.la
else
NFS_DIR =
NFS_LIB =
endif

# Currently disabled
#if BUILD_NTFS
#NTFS_DIR = ad_ntfs
#NTFS_LIB = ad_ntfs/libadio_ntfs.la
#else
#NTFS_DIR =
#NTFS_LIB =
#endif

if BUILD_PANFS
PANFS_DIR = ad_panfs
PANFS_LIB = ad_panfs/libadio_panfs.la
else
PANFS_DIR =
PANFS_LIB =
endif

if BUILD_PFS
PFS_DIR = ad_pfs
PFS_LIB = ad_pfs/libadio_pfs.la
else
PFS_DIR =
PFS_LIB =
endif

if BUILD_PIOFS
PIOFS_DIR = ad_piofs
PIOFS_LIB = ad_piofs/libadio_piofs.la
else
PIOFS_DIR =
PIOFS_LIB =
endif

if BUILD_PVFS
PVFS_DIR = ad_pvfs
PVFS_LIB = ad_pvfs/libadio_pvfs.la
else
PVFS_DIR =
PVFS_LIB =
endif

if BUILD_PVFS2
PVFS2_DIR = ad_pvfs2
PVFS2_LIB = ad_pvfs2/libadio_pvfs2.la
else
PVFS2_DIR =
PVFS2_LIB =
endif

if BUILD_SFS
SFS_DIR = ad_sfs
SFS_LIB = ad_sfs/libadio_sfs.la
else
SFS_DIR =
SFS_LIB =
endif

if BUILD_TESTFS
TESTFS_DIR = ad_testfs
TESTFS_LIB = ad_testfs/libadio_testfs.la
else
TESTFS_DIR =
TESTFS_LIB =
endif

if BUILD_UFS
UFS_DIR = ad_ufs
UFS_LIB = ad_ufs/libadio_ufs.la
else
UFS_DIR =
UFS_LIB =
endif

if BUILD_XFS
XFS_DIR = ad_xfs
XFS_LIB = ad_xfs/libadio_xfs.la
else
XFS_DIR =
XFS_LIB =
endif

SUBDIRS = common include \
	$(GRIDFTP_DIR) $(HFS_DIR) $(NFS_DIR) $(NTFS_DIR) $(PANFS_DIR) \
	$(PFS_DIR) $(PIOFS_DIR) $(PVFS_DIR) $(PVFS2_DIR) $(SFS_DIR) \
	$(TESTFS_DIR) $(UFS_DIR) $(XFS_DIR)
DIST_SUBDIRS = common include \
        ad_gridftp ad_hfs ad_nfs ad_ntfs ad_panfs ad_pfs ad_piofs ad_pvfs \
	ad_pvfs2 ad_sfs ad_testfs ad_ufs ad_xfs

# Library
noinst_LTLIBRARIES = libadio.la
libadio_la_SOURCES =
libadio_la_LIBADD = \
        common/libadio_common.la \
        $(GRIDFTP_LIB) $(HFS_LIB) $(NFS_LIB) $(NTFS_LIB) $(PANFS_LIB) \
        $(PFS_LIB) $(PIOFS_LIB) $(PVFS_LIB) $(PVFS2_LIB) $(SFS_LIB) \
        $(TESTFS_LIB) $(UFS_LIB) $(XFS_LIB)
libadio_la_DEPENDENCIES = \
        $(libadio_la_LIBADD)

