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
 * Copyright (c) 2008-2011 University of Houston. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "ompi_config.h"
#include <stdio.h>

#include "ompi/class/ompi_free_list.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"

#include "ompi/mca/fs/fs.h"
#include "ompi/mca/fs/base/base.h"

#ifdef HAVE_SYS_STATFS_H
#include <sys/statfs.h> /* or <sys/vfs.h> */ 
#endif
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#ifdef HAVE_SYS_MOUNT_H
#include <sys/mount.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif


void mca_fs_base_get_parent_dir ( char *filename, char **dirnamep)
{
    int err;
    char *dir = NULL, *slash;
    struct stat statbuf;
    


    err = lstat(filename, &statbuf);

/* no symlink on Windows */
#ifdef __WINDOWS__
    dir = strdup(filename);
#else
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
	int namelen;
	char *linkbuf;

	linkbuf = malloc(PATH_MAX+1);
	namelen = readlink(filename, linkbuf, PATH_MAX+1);
	if (namelen == -1) {
	    /* something strange has happened between the time that
	     * we determined that this was a link and the time that
	     * we attempted to read it; punt and use the old name.
	     */
	    dir = strdup(filename);
	}
	else {
	    /* successfully read the link */
	    linkbuf[namelen] = '\0'; /* readlink doesn't null terminate */
	    dir = strdup(linkbuf);
	    free(linkbuf);
	}
    }
#endif

    slash = strrchr(dir, '/');
    if (!slash) strncpy(dir, ".", 2);
    else {
	if (slash == dir) *(dir + 1) = '\0';
	else *slash = '\0';
    }

    *dirnamep = dir;
    return;
}
