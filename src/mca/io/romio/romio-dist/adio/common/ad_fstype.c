/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: ad_fstype.c,v 1.30 2002/12/03 23:38:23 David Exp $    
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

/* This file is quickly becoming the single one, outside the ADIO
 * implementations, which has "what ADIO components are built in" code in it.
 */

#include "adio.h"

#if (defined(HPUX) || defined(SPPUX) || defined(IRIX) || defined(SOLARIS) || defined(AIX) || defined(DEC) || defined(CRAY))
#include <sys/statvfs.h>
#endif
#ifdef LINUX
#include <sys/vfs.h>
/* #include <linux/nfs_fs.h> this file is broken in newer versions of linux */
#define NFS_SUPER_MAGIC 0x6969
#endif
#ifdef FREEBSD
#include <sys/param.h>
#include <sys/mount.h>
#endif
#ifdef PARAGON
#include <nx.h>
#include <pfs/pfs.h>
#include <sys/mount.h>
#endif
#ifdef SX4
#include <sys/stat.h>
#endif
#ifdef ROMIO_PVFS
#include "pvfs_config.h"
#include <sys/param.h>
#endif
#ifdef tflops
#include <sys/mount.h>
#endif

#ifdef HAVE_UNISTD_H
/* Needed for readlink */
#include <unistd.h>
#endif

#ifndef ROMIO_NTFS
static void ADIO_FileSysType_parentdir(char *filename, char **dirnamep);
#endif
static void ADIO_FileSysType_prefix(char *filename, int *fstype, 
				    int *error_code);
static void ADIO_FileSysType_fncall(char *filename, int *fstype, 
				    int *error_code);

/*
 ADIO_FileSysType_parentdir - determines a string pathname for the
 parent directory of a given filename.

Input Parameters:
. filename - pointer to file name character array

Output Parameters:
. dirnamep - pointer to location in which to store a pointer to a string

 Note that the caller should free the memory located at the pointer returned
 after the string is no longer needed.
*/
#ifndef ROMIO_NTFS
#ifndef PATH_MAX
#define PATH_MAX 65535
#endif
static void ADIO_FileSysType_parentdir(char *filename, char **dirnamep)
{
    int err;
    char *dir, *slash;
    struct stat statbuf;
    
    err = lstat(filename, &statbuf);

    if (err || (!S_ISLNK(statbuf.st_mode))) {
	/* no such file, or file is not a link; these are the "normal"
	 * cases where we can just return the parent directory.
	 */
	dir = strdup(filename);
    }
    else {
	/* filename is a symlink.  we've presumably already tried
	 * to stat it and found it to be missing (dangling link),
	 * but this code doesn't care if the target is really there
	 * or not.
	 */
	char *linkbuf;

	linkbuf = ADIOI_Malloc(PATH_MAX+1);
	err = readlink(filename, linkbuf, PATH_MAX+1);
	if (err) {
	    /* something strange has happened between the time that
	     * we determined that this was a link and the time that
	     * we attempted to read it; punt and use the old name.
	     */
	    dir = strdup(filename);
	}
	else {
	    /* successfully read the link */
	    dir = strdup(linkbuf);
	    ADIOI_Free(linkbuf);
	}
    }

    slash = strrchr(dir, '/');
    if (!slash) strcpy(dir, ".");
    else {
	if (slash == dir) *(dir + 1) = 0;
	else *slash = '\0';
    }

    *dirnamep = dir;
    return;
}
#endif /* ROMIO_NTFS */

/*
 ADIO_FileSysType_fncall - determines the file system type for a given file 
 using a system-dependent function call

Input Parameters:
. filename - pointer to file name character array

Output Parameters:
. fstype - location in which to store file system type (ADIO_XXX)
. error_code - location in which to store error code

 MPI_SUCCESS is stored in the location pointed to by error_code on success.

 This function is used by MPI_File_open() and MPI_File_delete() to determine 
 file system type.  Most other functions use the type which is stored when the 
 file is opened.
 */
static void ADIO_FileSysType_fncall(char *filename, int *fstype, int *error_code)
{
#ifndef ROMIO_NTFS
    char *dir;
    int err;
#endif
#if (defined(HPUX) || defined(SPPUX) || defined(IRIX) || defined(SOLARIS) || defined(AIX) || defined(DEC) || defined(CRAY))
    struct statvfs vfsbuf;
#endif
#if (defined(LINUX) || defined(FREEBSD) || defined(tflops))
    struct statfs fsbuf;
#endif
#ifdef PARAGON
    struct estatfs ebuf;
#endif
#ifdef SX4
    struct stat sbuf;
#endif

    *error_code = MPI_SUCCESS;

#if (defined(HPUX) || defined(SPPUX) || defined(IRIX) || defined(SOLARIS) || defined(AIX) || defined(DEC) || defined(CRAY))
    do {
	err = statvfs(filename, &vfsbuf);
    } while (err && (errno == ESTALE));

    if (err && (errno == ENOENT)) {
	ADIO_FileSysType_parentdir(filename, &dir);
	err = statvfs(dir, &vfsbuf);
	free(dir);
    }

    if (err) *error_code = MPI_ERR_UNKNOWN;
    else {
	/* FPRINTF(stderr, "%s\n", vfsbuf.f_basetype); */
	if (!strncmp(vfsbuf.f_basetype, "nfs", 3)) *fstype = ADIO_NFS;
	else {
# if (defined(HPUX) || defined(SPPUX))
#    ifdef HFS
	    *fstype = ADIO_HFS;
#    else
            *fstype = ADIO_UFS;
#    endif
# else
	    if (!strncmp(vfsbuf.f_basetype, "xfs", 3)) *fstype = ADIO_XFS;
	    else if (!strncmp(vfsbuf.f_basetype, "piofs", 4)) *fstype = ADIO_PIOFS;
	    else *fstype = ADIO_UFS;
# endif
	}
    }
#elif defined(LINUX)
    do {
	err = statfs(filename, &fsbuf);
    } while (err && (errno == ESTALE));

    if (err && (errno == ENOENT)) {
	ADIO_FileSysType_parentdir(filename, &dir);
	err = statfs(dir, &fsbuf);
	free(dir);
    }

    if (err) *error_code = MPI_ERR_UNKNOWN;
    else {
	/* FPRINTF(stderr, "%d\n", fsbuf.f_type);*/
	if (fsbuf.f_type == NFS_SUPER_MAGIC) *fstype = ADIO_NFS;
# ifdef ROMIO_PVFS
	else if (fsbuf.f_type == PVFS_SUPER_MAGIC) *fstype = ADIO_PVFS;
# endif
	else *fstype = ADIO_UFS;
    }
#elif (defined(FREEBSD) && defined(HAVE_MOUNT_NFS))
    do {
	err = statfs(filename, &fsbuf);
    } while (err && (errno == ESTALE));

    if (err && (errno == ENOENT)) {
	ADIO_FileSysType_parentdir(filename, &dir);
	err = statfs(dir, &fsbuf);
	free(dir);
    }

    if (err) *error_code = MPI_ERR_UNKNOWN;
    else {
# if (__FreeBSD_version>300004)
	if ( !strncmp("nfs",fsbuf.f_fstypename,3) ) *fstype = ADIO_NFS;
# else
	if (fsbuf.f_type == MOUNT_NFS) *fstype = ADIO_NFS;
# endif
	else *fstype = ADIO_UFS;
    }
#elif defined(PARAGON)
    do {
	err = statpfs(filename, &ebuf, 0, 0);
    } while (err && (errno == ESTALE));

    if (err && (errno == ENOENT)) {
	ADIO_FileSysType_parentdir(filename, &dir);
	err = statpfs(dir, &ebuf, 0, 0);
	free(dir);
    }

    if (err) *error_code = MPI_ERR_UNKNOWN;
    else {
	if (ebuf.f_type == MOUNT_NFS) *fstype = ADIO_NFS;
	else if (ebuf.f_type == MOUNT_PFS) *fstype = ADIO_PFS;
	else *fstype = ADIO_UFS;
    }
#elif defined(tflops)
    do {
	err = statfs(filename, &fsbuf);
    } while (err && (errno == ESTALE));

    if (err && (errno == ENOENT)) {
	ADIO_FileSysType_parentdir(filename, &dir);
	err = statfs(dir, &fsbuf);
	free(dir);
    }

    if (err) *error_code = MPI_ERR_UNKNOWN;
    else {
	if (fsbuf.f_type == MOUNT_NFS) *fstype = ADIO_NFS;
	else if (fsbuf.f_type == MOUNT_PFS) *fstype = ADIO_PFS;
	else *fstype = ADIO_UFS;
    }
#elif defined(SX4)
    do {
	err = stat(filename, &sbuf);
    } while (err && (errno == ESTALE));

    if (err && (errno == ENOENT)) {
	ADIO_FileSysType_parentdir(filename, &dir);
	err = stat(dir, &sbuf);
	free(dir);
    }
    
    if (err) *error_code = MPI_ERR_UNKNOWN;
    else {
	if (!strcmp(sbuf.st_fstype, "nfs")) *fstype = ADIO_NFS;
	else *fstype = ADIO_SFS;
    }
#else
    /* on other systems, make NFS the default */
# ifdef ROMIO_NTFS
    *fstype = ADIO_NTFS;
# else
    *fstype = ADIO_NFS;   
# endif
    *error_code = MPI_SUCCESS;
#endif
}

/*
  ADIO_FileSysType_prefix - determines file system type for a file using 
  a prefix on the file name.  upper layer should have already determined
  that a prefix is present.

Input Parameters:
. filename - path to file, including prefix (xxx:)

Output Parameters:
. fstype - pointer to integer in which to store file system type (ADIO_XXX)
. error_code - pointer to integer in which to store error code

  Returns MPI_SUCCESS in error_code on success.  Filename not having a prefix
  is considered an error. Except for on Windows systems where the default is NTFS.

 */
static void ADIO_FileSysType_prefix(char *filename, int *fstype, int *error_code)
{
    *error_code = MPI_SUCCESS;

    if (!strncmp(filename, "pfs:", 4) || !strncmp(filename, "PFS:", 4)) {
	*fstype = ADIO_PFS;
    }
    else if (!strncmp(filename, "piofs:", 6) || !strncmp(filename, "PIOFS:", 6)) {
	*fstype = ADIO_PIOFS;
    }
    else if (!strncmp(filename, "ufs:", 4) || !strncmp(filename, "UFS:", 4)) {
	*fstype = ADIO_UFS;
    }
    else if (!strncmp(filename, "nfs:", 4) || !strncmp(filename, "NFS:", 4)) {
	*fstype = ADIO_NFS;
    }
    else if (!strncmp(filename, "hfs:", 4) || !strncmp(filename, "HFS:", 4)) {
	*fstype = ADIO_HFS;
    }
    else if (!strncmp(filename, "xfs:", 4) || !strncmp(filename, "XFS:", 4)) {
	*fstype = ADIO_XFS;
    }
    else if (!strncmp(filename, "sfs:", 4) || !strncmp(filename, "SFS:", 4)) {
	*fstype = ADIO_SFS;
    }
    else if (!strncmp(filename, "pvfs:", 5) || !strncmp(filename, "PVFS:", 5)) {
	*fstype = ADIO_PVFS;
    }
    else if (!strncmp(filename, "testfs:", 7) 
	     || !strncmp(filename, "TESTFS:", 7))
    {
	*fstype = ADIO_TESTFS;
    }
    else {
#ifdef ROMIO_NTFS
	*fstype = ADIO_NTFS;
#else
	*fstype = 0;
	*error_code = MPI_ERR_UNKNOWN;
#endif
    }
}

/*@
    ADIO_ResolveFileType - determines file system type and operations from
                           file name string; this is a collective call

Input Parameters:
. comm - communicator across which collective open is performed
. filename - name of file (string)

Output Parameters:
. fstype - (pointer to) int holding file system type
. ops - (address of) pointer to table of valid file operations
. error_code - (pointer to) int holding error code

Notes:
This code used to be in MPI_File_open(), but it has been moved into here in 
order to clean things up.  The goal is to separate all this "did we compile
for this fs type" code from the MPI layer and also to introduce the ADIOI_Fns
tables in a reasonable way. -- Rob, 06/06/2001
@*/
void ADIO_ResolveFileType(MPI_Comm comm, char *filename, int *fstype, 
			  ADIOI_Fns **ops, int *error_code)
{
#ifndef PRINT_ERR_MSG
    static char myname[] = "ADIO_RESOLVEFILETYPE";
#endif
    int myerrcode, file_system, min_code;
    char *tmp;

    file_system = -1;
    tmp = strchr(filename, ':');
    if (!tmp) {
	/* no prefix; use system-dependent function call to determine type */
	ADIO_FileSysType_fncall(filename, &file_system, &myerrcode);
	if (myerrcode != MPI_SUCCESS) {
#ifdef PRINT_ERR_MSG
	    FPRINTF(stderr, "ADIO_ResolveFileType: Can't determine the file-system type. Check the filename/path you provided and try again. Otherwise, prefix the filename with a string to indicate the type of file sytem (piofs:, pfs:, nfs:, ufs:, hfs:, xfs:, sfs:, pvfs:).\n");
	    MPI_Abort(MPI_COMM_WORLD, 1);
#else
	    myerrcode = MPIR_Err_setmsg(MPI_ERR_IO, MPIR_ERR_NO_FSTYPE,
					myname, (char *) 0, (char *) 0);
	    *error_code = ADIOI_Error(MPI_FILE_NULL, myerrcode, myname);
	    return;
#endif
	}

	/* ensure that everyone came up with the same file system type */
	MPI_Allreduce(&file_system, &min_code, 1, MPI_INT, MPI_MIN, comm);
	if (min_code == ADIO_NFS) file_system = ADIO_NFS;

    }
    else {
	/* prefix specified; just match via prefix and assume everyone got 
	 * the same thing.
	 *
	 * perhaps we should have this code go through the allreduce as well?
	 */
	ADIO_FileSysType_prefix(filename, &file_system, &myerrcode);
	if (myerrcode != MPI_SUCCESS) {
#ifdef PRINT_ERR_MSG
	    FPRINTF(stderr, "ADIO_ResolveFileType: Can't determine the file-system type from the specified prefix. Check the filename/path and prefix you provided and try again.\n");
	    MPI_Abort(MPI_COMM_WORLD, 1);
#else
	    myerrcode = MPIR_Err_setmsg(MPI_ERR_IO, MPIR_ERR_NO_FSTYPE,
					myname, (char *) 0, (char *) 0);
	    *error_code = ADIOI_Error(MPI_FILE_NULL, myerrcode, myname);
	    return;
#endif
	}
    }

    /* verify that we support this file system type and set ops pointer */
    if (file_system == ADIO_PFS) {
#ifndef PFS
# ifdef PRINT_ERR_MSG
	FPRINTF(stderr, "ADIO_ResolveFileType: ROMIO has not been configured to use the PFS file system\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
# else
	myerrcode = MPIR_Err_setmsg(MPI_ERR_IO, MPIR_ERR_NO_PFS,
				    myname, (char *) 0, (char *) 0);
	*error_code = ADIOI_Error(MPI_FILE_NULL, myerrcode, myname);
	return;
# endif
#else
	*ops = &ADIO_PFS_operations;
#endif
    }
    if (file_system == ADIO_PIOFS) {
#ifndef PIOFS
# ifdef PRINT_ERR_MSG
	FPRINTF(stderr, "ADIO_ResolveFileType: ROMIO has not been configured to use the PIOFS file system\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
# else
	myerrcode = MPIR_Err_setmsg(MPI_ERR_IO, MPIR_ERR_NO_PIOFS,
				     myname, (char *) 0, (char *) 0);
	*error_code = ADIOI_Error(MPI_FILE_NULL, myerrcode, myname);
	return;
# endif
#else
	*ops = &ADIO_PIOFS_operations;
#endif
    }
    if (file_system == ADIO_UFS) {
#ifndef UFS
# ifdef PRINT_ERR_MSG
	FPRINTF(stderr, "ADIO_ResolveFileType: ROMIO has not been configured to use the UFS file system\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
# else
	myerrcode = MPIR_Err_setmsg(MPI_ERR_IO, MPIR_ERR_NO_UFS,
				     myname, (char *) 0, (char *) 0);
	*error_code = ADIOI_Error(MPI_FILE_NULL, myerrcode, myname);
	return;
# endif
#else
	*ops = &ADIO_UFS_operations;
#endif
    }
    if (file_system == ADIO_NFS) {
#ifndef NFS
# ifdef PRINT_ERR_MSG
	FPRINTF(stderr, "ADIO_ResolveFileType: ROMIO has not been configured to use the NFS file system\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
# else
	myerrcode = MPIR_Err_setmsg(MPI_ERR_IO, MPIR_ERR_NO_NFS,
				     myname, (char *) 0, (char *) 0);
	*error_code =  ADIOI_Error(MPI_FILE_NULL, myerrcode, myname);
	return;
# endif
#else
	*ops = &ADIO_NFS_operations;
#endif
    }
    if (file_system == ADIO_HFS) {
#ifndef HFS
# ifdef PRINT_ERR_MSG
	FPRINTF(stderr, "ADIO_ResolveFileType: ROMIO has not been configured to use the HFS file system\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
# else
	myerrcode = MPIR_Err_setmsg(MPI_ERR_IO, MPIR_ERR_NO_HFS,
				     myname, (char *) 0, (char *) 0);
	*error_code = ADIOI_Error(MPI_FILE_NULL, myerrcode, myname);
	return;
# endif
#else
	*ops = &ADIO_HFS_operations;
#endif
    }
    if (file_system == ADIO_XFS) {
#ifndef XFS
# ifdef PRINT_ERR_MSG
	FPRINTF(stderr, "ADIO_ResolveFileType: ROMIO has not been configured to use the XFS file system\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
# else
	myerrcode = MPIR_Err_setmsg(MPI_ERR_IO, MPIR_ERR_NO_XFS,
				     myname, (char *) 0, (char *) 0);
	*error_code = ADIOI_Error(MPI_FILE_NULL, myerrcode, myname);
	return;
# endif
#else
	*ops = &ADIO_XFS_operations;
#endif
    }
    if (file_system == ADIO_SFS) {
#ifndef SFS
# ifdef PRINT_ERR_MSG
	FPRINTF(stderr, "ADIO_ResolveFileType: ROMIO has not been configured to use the SFS file system\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
# else
	myerrcode = MPIR_Err_setmsg(MPI_ERR_IO, MPIR_ERR_NO_SFS,
				     myname, (char *) 0, (char *) 0);
	*error_code = ADIOI_Error(MPI_FILE_NULL, myerrcode, myname);
	return;
# endif
#else
	*ops = &ADIO_SFS_operations;
#endif
    }
    if (file_system == ADIO_PVFS) {
#ifndef ROMIO_PVFS
# ifdef PRINT_ERR_MSG
	FPRINTF(stderr, "ADIO_ResolveFileType: ROMIO has not been configured to use the PVFS file system\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
# else
	myerrcode = MPIR_Err_setmsg(MPI_ERR_IO, MPIR_ERR_NO_PVFS,
				     myname, (char *) 0, (char *) 0);
	*error_code = ADIOI_Error(MPI_FILE_NULL, myerrcode, myname);
	return;
# endif
#else
	*ops = &ADIO_PVFS_operations;
#endif
    }
    if (file_system == ADIO_NTFS) {
#ifndef ROMIO_NTFS
# ifdef PRINT_ERR_MSG
	FPRINTF(stderr, "ADIO_ResolveFileType: ROMIO has not been configured to use the NTFS file system\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
# else
	myerrcode = MPIR_Err_setmsg(MPI_ERR_IO, MPIR_ERR_NO_NTFS,
				     myname, (char *) 0, (char *) 0);
	*error_code = ADIOI_Error(MPI_FILE_NULL, myerrcode, myname);
	return;
# endif
#else
	*ops = &ADIO_NTFS_operations;
#endif
    }
    if (file_system == ADIO_TESTFS) {
#ifndef ROMIO_TESTFS
# ifdef PRINT_ERR_MSG
	FPRINTF(stderr, "ADIO_ResolveFileType: ROMIO has not been configured to use the TESTFS file system\n");
	MPI_Abort(MPI_COMM_WORLD, 1);
# else
	myerrcode = MPIR_Err_setmsg(MPI_ERR_IO, MPIR_ERR_NO_TESTFS,
				     myname, (char *) 0, (char *) 0);
	*error_code = ADIOI_Error(MPI_FILE_NULL, myerrcode, myname);
	return;
# endif
#else
	*ops = &ADIO_TESTFS_operations;
#endif
    }
    *error_code = MPI_SUCCESS;
    *fstype = file_system;
    return;
}
