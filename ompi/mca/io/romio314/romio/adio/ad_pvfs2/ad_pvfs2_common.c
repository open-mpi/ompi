/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/* 
 *   Copyright (C) 2003 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ad_pvfs2.h"
#include "ad_pvfs2_common.h"
#include <unistd.h>
#include <sys/types.h>
#include <time.h>
#include <stdlib.h>

/* maybe give romio access to the globalconfig struct */
/* keyval hack to both tell us if we've already initialized pvfs2 and also
 * close it down when mpi exits */
int ADIOI_PVFS2_Initialized = MPI_KEYVAL_INVALID;

void ADIOI_PVFS2_End(int *error_code)
{
    int ret;
    static char myname[] = "ADIOI_PVFS2_END";

    ret = PVFS_sys_finalize();

    /* --BEGIN ERROR HANDLING-- */
    if (ret != 0 ) {
	*error_code = MPIO_Err_create_code(MPI_SUCCESS,
					   MPIR_ERR_RECOVERABLE,
					   myname, __LINE__,
					   ADIOI_PVFS2_error_convert(ret),
					   "Error in PVFS_sys_finalize", 0);
	return;
    }
    /* --END ERROR HANDLING-- */

    *error_code = MPI_SUCCESS;
}

int ADIOI_PVFS2_End_call(MPI_Comm comm, int keyval, 
			 void *attribute_val, void *extra_state)
{
    int error_code;
    ADIOI_PVFS2_End(&error_code);
    MPI_Keyval_free(&keyval);
    return error_code;
}

void ADIOI_PVFS2_Init(int *error_code )
{
    int ret;
    static char myname[] = "ADIOI_PVFS2_INIT";
    char * ncache_timeout;

    /* do nothing if we've already fired up the pvfs2 interface */
    if (ADIOI_PVFS2_Initialized != MPI_KEYVAL_INVALID) {
	*error_code = MPI_SUCCESS;
	return;
    }

    /* for consistency, we should disable the pvfs2 ncache.  If the
     * environtment variable is already set, assume a  user knows it
     * won't be a problem */
    ncache_timeout = getenv("PVFS2_NCACHE_TIMEOUT");
    if (ncache_timeout == NULL )
	setenv("PVFS2_NCACHE_TIMEOUT", "0", 1);

    ret = PVFS_util_init_defaults();
    if (ret < 0 ) {
	*error_code = MPIO_Err_create_code(MPI_SUCCESS,
					   MPIR_ERR_RECOVERABLE,
					   myname, __LINE__,
					   ADIOI_PVFS2_error_convert(ret),
					   "Error in PVFS_util_init_defaults",
					   0);
	PVFS_perror("PVFS_util_init_defaults", ret);

	return;
    }
    
    MPI_Keyval_create(MPI_NULL_COPY_FN, ADIOI_PVFS2_End_call,
		      &ADIOI_PVFS2_Initialized, (void *)0); 
    /* just like romio does, we make a dummy attribute so we 
     * get cleaned up */
    MPI_Attr_put(MPI_COMM_SELF, ADIOI_PVFS2_Initialized, (void *)0);
}

void ADIOI_PVFS2_makeattribs(PVFS_sys_attr * attribs)
{
    memset(attribs, 0, sizeof(PVFS_sys_attr));
    
    attribs->owner = geteuid();
    attribs->group = getegid();
    attribs->perms = 0644;
    attribs->mask =  PVFS_ATTR_SYS_ALL_SETABLE;
    attribs->atime = time(NULL);
    attribs->mtime = attribs->atime;
    attribs->ctime = attribs->atime;
}


void ADIOI_PVFS2_makecredentials(PVFS_credentials * credentials)
{
    memset(credentials, 0, sizeof(PVFS_credentials));

    PVFS_util_gen_credentials(credentials);
}

int ADIOI_PVFS2_error_convert(int pvfs_error)
{
    switch(pvfs_error)
    {
	case PVFS_EPERM:
	case PVFS_EACCES:
	    return MPI_ERR_ACCESS;
	case PVFS_ENOENT:
	case PVFS_ENXIO:
	case PVFS_ENODEV:
	    return MPI_ERR_NO_SUCH_FILE;
	case PVFS_EIO:
	    return MPI_ERR_IO;
	case PVFS_EEXIST:
	    return MPI_ERR_FILE_EXISTS;
	case PVFS_ENOTDIR: /* ??? */
	case PVFS_EISDIR: /* ??? */
	case PVFS_ENAMETOOLONG:
	    return MPI_ERR_BAD_FILE;
	case PVFS_EINVAL:
	    return MPI_ERR_FILE;
	case PVFS_EFBIG: /* ??? */
	case PVFS_ENOSPC:
	    return MPI_ERR_NO_SPACE;
	case PVFS_EROFS:
	    return MPI_ERR_READ_ONLY;
	case PVFS_ENOSYS:
	    return MPI_ERR_UNSUPPORTED_OPERATION;
	    /* PVFS does not support quotas */
	case EDQUOT:
	    return MPI_ERR_QUOTA;
	case PVFS_ENOMEM:
	    return MPI_ERR_INTERN;
	default:
	    return MPI_UNDEFINED;
    }

}

/* 
 * vim: ts=8 sts=4 sw=4 noexpandtab 
 */
