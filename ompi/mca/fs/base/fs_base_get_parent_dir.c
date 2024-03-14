/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008-2016 University of Houston. All rights reserved.
 * Copyright (c) 2015-2016 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2018      Cisco Systems, Inc.  All rights reserved
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#include "ompi_config.h"

#include <stdio.h>

#include "opal/mca/base/base.h"
#include "opal/util/path.h"
#include "opal/util/printf.h"
#include "opal/util/string_copy.h"

#include "ompi/mca/mca.h"
#include "ompi/mca/fs/fs.h"
#include "ompi/mca/fs/base/base.h"
#include "ompi/mca/common/ompio/common_ompio.h"

/*
 * Be careful moving this include.
 * It's easy to hit problems similar to that reported in
 * https://github.com/systemd/systemd/issues/8507
 */
#ifdef HAVE_SYS_MOUNT_H
#include <sys/mount.h>
#endif

void mca_fs_base_get_parent_dir ( char *filename, char **dirnamep)
{
    char *dir = NULL, *slash;

    if (strlen(filename) < 1) {
        opal_asprintf(dirnamep, ".%s", OPAL_PATH_SEP);
        return;
    }

    if (!mca_fs_base_is_link(filename)) {
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
        mca_fs_base_get_real_filename(filename, &dir);
    }

    slash = strrchr(dir, '/');
    if (!slash) {
        // It is guaranteed in this case that "dir" will be at least 2
        // characters long.
        opal_string_copy(dir, ".", 2);
    } else {
	if (slash == dir) {
            *(dir + 1) = '\0';
        } else {
            *slash = '\0';
        }
    }

    *dirnamep = dir;
    return;
}

int  mca_fs_base_get_fstype(char *fname )
{
    int ompio_type = UFS;
    char *fstype=NULL;
    bool ret = opal_path_nfs ( fname, &fstype );

    if ( false == ret ) {
        char *dir;
        mca_fs_base_get_parent_dir (fname, &dir );
        ret = opal_path_nfs (dir, &fstype);
        free(dir);
        if ( false == ret ) {
            return ompio_type;
        }
    }
    if ( 0 == strncasecmp(fstype, "lustre", sizeof("lustre")) ) {
        ompio_type = LUSTRE;
    }
    else if ( 0 == strncasecmp(fstype, "pvfs2", sizeof("pvfs2"))) {
        ompio_type = PVFS2;
    }
    else if ( 0 == strncasecmp(fstype, "ime", sizeof("ime"))) {
        ompio_type = IME;
    }
    else if ( 0 == strncasecmp(fstype, "gpfs", sizeof("gpfs"))) {
        ompio_type = GPFS;
    }

    free (fstype);
    return ompio_type;
}

int mca_fs_base_get_mpi_err(int errno_val)
{
    int ret;
    switch (errno_val) {
        case EACCES:
            ret = MPI_ERR_ACCESS;
            break;
        case ENAMETOOLONG:
        case EISDIR:
            ret = MPI_ERR_BAD_FILE;
            break;
        case ENOENT:
            ret = MPI_ERR_NO_SUCH_FILE;
            break;
        case EROFS:
            ret = MPI_ERR_READ_ONLY;
            break;
        case EEXIST:
            ret = MPI_ERR_FILE_EXISTS;
            break;
        case ENOSPC:
            ret = MPI_ERR_NO_SPACE;
            break;
        case EDQUOT:
            ret = MPI_ERR_QUOTA;
            break;
        case ETXTBSY:
            ret = MPI_ERR_FILE_IN_USE;
            break;
        case EBADF:
            ret = MPI_ERR_FILE;
            break;
        default:
            ret = MPI_ERR_OTHER;
            break;
    }
    return ret;
}

int mca_fs_base_get_file_perm(ompio_file_t *fh)
{
    int old_mask;
    int perm = fh->f_perm;

    if (OMPIO_PERM_NULL == perm) {
        old_mask = umask(022);
        umask(old_mask);
        perm = old_mask ^ 0666;
    }
    return perm;
}

int mca_fs_base_get_file_amode(int rank, int access_mode)
{
    int amode = 0;

    if (access_mode & MPI_MODE_RDONLY) {
        amode = amode | O_RDONLY;
    }
    if (access_mode & MPI_MODE_WRONLY) {
        amode = amode | O_WRONLY;
    }
    if (access_mode & MPI_MODE_RDWR) {
        amode = amode | O_RDWR;
    }

    /* MODE_CREATE and MODE_EXCL should only be set by one process */
    if(OMPIO_ROOT == rank) {
        if (access_mode & MPI_MODE_CREATE) {
            amode = amode | O_CREAT;
        }
        if (access_mode & MPI_MODE_EXCL) {
            amode = amode | O_EXCL;
        }
    }

    return amode;
}
