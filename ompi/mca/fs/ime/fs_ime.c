/*
 * Copyright (c) 2018      DataDirect Networks. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ime_native.h"

#include "ompi_config.h"
#include "mpi.h"
#include "ompi/mca/fs/fs.h"
#include "ompi/mca/fs/base/base.h"
#include "ompi/mca/fs/ime/fs_ime.h"

/*
 * *******************************************************************
 * ************************ actions structure ************************
 * *******************************************************************
 */
static mca_fs_base_module_1_0_0_t ime =  {
    mca_fs_ime_module_init, /* initialise after being selected */
    mca_fs_ime_module_finalize, /* close a module on a communicator */
    mca_fs_ime_file_open,
    mca_fs_ime_file_close,
    mca_fs_ime_file_delete,
    mca_fs_ime_file_set_size,
    mca_fs_ime_file_get_size,
    mca_fs_ime_file_sync
};
/*
 * *******************************************************************
 * ************************* structure ends **************************
 * *******************************************************************
 */

/*
 * Private variables
 */
static int mca_fs_ime_IS_INITIALIZED = 0;

/*
 * Function decls
 */
int mca_fs_ime_component_init_query(bool enable_progress_threads,
                                      bool enable_mpi_threads)
{
    /* Nothing to do */

   return OMPI_SUCCESS;
}

struct mca_fs_base_module_1_0_0_t *
mca_fs_ime_component_file_query (ompio_file_t *fh, int *priority)
{
    /* IME should only be used for paths starting with ime: or IME:
       Therefore, this function will return a NULL module when no IME
       path is detected. */

    char *tmp;

    *priority = mca_fs_ime_priority;

    tmp = strchr (fh->f_filename, ':');
    if (!tmp) {
        /* The communicator might be NULL if we only want to delete the file */
        if (OMPIO_ROOT == fh->f_rank || MPI_COMM_NULL == fh->f_comm) {
            fh->f_fstype = mca_fs_base_get_fstype ( fh->f_filename );
        }
        if (fh->f_comm != MPI_COMM_NULL) {
            fh->f_comm->c_coll->coll_bcast (&(fh->f_fstype),
                                            1,
                                            MPI_INT,
                                            OMPIO_ROOT,
                                            fh->f_comm,
                                            fh->f_comm->c_coll->coll_bcast_module);
        }
    }
    else {
        if (!strncmp(fh->f_filename, DEFAULT_IME_PREFIX_NO_FWD_SLASH, 
                     IME_FILE_PREFIX_LEN_NO_FWD_SLASH)){
            fh->f_fstype = IME;
        }
    }

    /* According to my understanding, a valid module should be returned
       as long as a valid FS type is detected. (This isn't what is done
       for LUSTRE or PVFS2)
     */
    if (IME == fh->f_fstype) {
        if (*priority < FS_IME_INCREASED_PRIORITY) {
            *priority = FS_IME_INCREASED_PRIORITY;
        }
        return &ime;
    }

   return NULL;
}

int mca_fs_ime_component_file_unquery (ompio_file_t *file)
{
   /* This function might be needed for some purposes later. for now it
    * does not have anything to do since there are no steps which need
    * to be undone if this module is not selected */

   return OMPI_SUCCESS;
}

int mca_fs_ime_module_init (ompio_file_t *file)
{
    /* Make sure the file type is not overwritten by the last queried
     * component */
    file->f_fstype = IME;

    if (mca_fs_ime_IS_INITIALIZED == 0) {
        mca_fs_ime_IS_INITIALIZED = 1;
        ime_native_init();
    }
    return OMPI_SUCCESS;
}

int mca_fs_ime_module_finalize (ompio_file_t *file)
{
    /*
     * Nothing to do here:
     * We can't finalize IME here because other files might
     * still be using it. Instead, IME is finalized when
     * the OMPIO component is closed.
     */
    
    return OMPI_SUCCESS;
}

int mca_fs_ime_native_fini()
{
    int ret;
    if (mca_fs_ime_IS_INITIALIZED == 0) {
        return OMPI_SUCCESS;
    }

    /* We don't actually need to reset this variable since
        mca_fs_ime_native_fini is only called once:
        when OMPIO is closed
    */
    mca_fs_ime_IS_INITIALIZED = 0;

    ret = ime_native_finalize();
    if (ret != 0) {
        return OMPI_ERROR;
    }
    
    return OMPI_SUCCESS;
}