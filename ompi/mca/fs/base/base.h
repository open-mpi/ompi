/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2008-2018 University of Houston. All rights reserved.
 * Copyright (c) 2018      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 */

/**
 * @file
 *
 * MCA fs base framework public interface functions.
 */

#ifndef MCA_FS_BASE_H
#define MCA_FS_BASE_H

#include "ompi_config.h"

#include "mpi.h"
#include "opal/mca/base/base.h"
#include "ompi/mca/common/ompio/common_ompio.h"
#include "ompi/mca/fs/fs.h"


#ifdef HAVE_SYS_STATFS_H
#include <sys/statfs.h> /* or <sys/vfs.h> */
#endif
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

BEGIN_C_DECLS

OMPI_DECLSPEC int mca_fs_base_file_select(struct ompio_file_t *file,
                            mca_base_component_t *preferred);

OMPI_DECLSPEC int mca_fs_base_file_unselect(struct ompio_file_t *file);

OMPI_DECLSPEC int mca_fs_base_find_available(bool enable_progress_threads,
                                             bool enable_mpi_threads);

OMPI_DECLSPEC int mca_fs_base_init_file (struct ompio_file_t *file);

OMPI_DECLSPEC int mca_fs_base_get_param (struct ompio_file_t *file, int keyval);
OMPI_DECLSPEC void mca_fs_base_get_parent_dir (char *filename, char **dirnamep);
OMPI_DECLSPEC int  mca_fs_base_get_fstype(char *fname);
OMPI_DECLSPEC int mca_fs_base_get_mpi_err(int errno_val);
OMPI_DECLSPEC int mca_fs_base_get_file_perm(ompio_file_t *fh);
OMPI_DECLSPEC int mca_fs_base_get_file_amode(int rank, int access_mode);

OMPI_DECLSPEC int mca_fs_base_file_delete (char* file_name, struct opal_info_t *info);
OMPI_DECLSPEC int mca_fs_base_file_sync (ompio_file_t *fh);
OMPI_DECLSPEC int mca_fs_base_file_get_size (ompio_file_t *fh, OMPI_MPI_OFFSET_TYPE *size);
OMPI_DECLSPEC int mca_fs_base_file_set_size (ompio_file_t *fh, OMPI_MPI_OFFSET_TYPE size);
OMPI_DECLSPEC int mca_fs_base_file_close (ompio_file_t *fh);


static inline bool mca_fs_base_is_link (const char *filename)
{
    int err;
    bool ret = true;
    struct stat statbuf;

    err = lstat(filename, &statbuf);

    if (err || (!S_ISLNK(statbuf.st_mode))) {
        ret = false;
    }

    return ret;
}

static inline void mca_fs_base_get_real_filename (const char *filename, char **rfilename)
{
    int namelen;
    char linkbuf[OPAL_PATH_MAX+1];

    namelen = readlink(filename, linkbuf, OPAL_PATH_MAX);
    if (namelen == -1) {
        /* something strange has happened between the time that
         * we determined that this was a link and the time that
         * we attempted to read it; punt and use the old name.
         */
        *rfilename = strdup(filename);
    }
    else {
        /* successfully read the link */
        linkbuf[namelen] = '\0'; /* readlink doesn't null terminate */
        if ('/' == linkbuf[0]) {
            /* absolute symlink target — use directly */
            *rfilename = strdup(linkbuf);
        } else {
            /* Relative symlink target: resolve relative to the directory
             * that contains the symlink, not the current working directory.
             * Find the last '/' in the symlink path itself. */
            const char *slash = strrchr(filename, '/');
            if (NULL == slash) {
                /* Symlink is in the current directory; relative target is
                 * already correct relative to CWD. */
                *rfilename = strdup(linkbuf);
            } else {
                /* Prepend the directory portion (up to and including the
                 * last '/') of the symlink path to the relative target. */
                size_t dir_len = (size_t)(slash - filename) + 1; /* include '/' */
                size_t link_len = (size_t) namelen;
                char *full_path = (char *) malloc(dir_len + link_len + 1);
                if (NULL == full_path) {
                    *rfilename = strdup(filename);
                } else {
                    memcpy(full_path, filename, dir_len);
                    memcpy(full_path + dir_len, linkbuf, link_len + 1);
                    *rfilename = full_path;
                }
            }
        }
    }
}

/*
 * Globals
 */

OMPI_DECLSPEC extern mca_base_framework_t ompi_fs_base_framework;

END_C_DECLS

#endif /* MCA_BASE_FS_H */
