/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "file/file.h"
#include "class/ompi_list.h"
#include "mpi/runtime/params.h"


/*
 * Table for Fortran <-> C file handle conversion
 */
ompi_pointer_array_t ompi_file_f_to_c_table; 

/*
 * MPI_FILE_NULL
 */
ompi_file_t  ompi_mpi_file_null;


/*
 * Local functions
 */
static void file_constructor(ompi_file_t *obj);
static void file_destructor(ompi_file_t *obj);


/*
 * Class instance for ompi_file_t
 */
OBJ_CLASS_INSTANCE(ompi_file_t,
                   ompi_object_t,
                   file_constructor,
                   file_destructor);


/*
 * Initialize file handling bookeeping
 */
int ompi_file_init(void)
{
    /* Setup file array */

    OBJ_CONSTRUCT(&ompi_file_f_to_c_table, ompi_pointer_array_t); 

    /* Setup MPI_FILE_NULL.  Note that it will have the default error
       handler of MPI_ERRORS_RETURN, per MPI-2:9.7 (p265).  */

    OBJ_CONSTRUCT(&ompi_mpi_file_null, ompi_file_t);
    ompi_mpi_file_null.f_comm = &ompi_mpi_comm_null;
    OBJ_RETAIN(ompi_mpi_file_null.f_comm);
    ompi_mpi_file_null.f_f_to_c_index = 0;
    ompi_pointer_array_set_item(&ompi_file_f_to_c_table, 0,
                                &ompi_mpi_file_null);

    /* All done */

    return OMPI_SUCCESS;
}


/*
 * Create a file handle
 */
int ompi_file_open(ompi_communicator_t *comm, char *filename, 
                   int amode, ompi_info_t *info, ompi_file_t **fh)
{
    ompi_file_t *file;

    file = OBJ_NEW(ompi_file_t);
    if (NULL == file) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* Save the params */

    file->f_comm = comm;
    OBJ_RETAIN(comm);
    file->f_filename = strdup(filename);
    file->f_amode = amode;

    /* Select a module and actually open the file */

    /* JMS fill in here */

    /* All done */

    *fh = file;
    return OMPI_SUCCESS;
}


/*
 * Close a file handle
 */
int ompi_file_close(ompi_file_t **file) 
{
    (*file)->f_flags |= OMPI_FILE_ISCLOSED;
    OBJ_RELEASE(*file);
    *file = MPI_FILE_NULL;
    return OMPI_SUCCESS;
}


/*
 * Shut down the MPI_File bookkeeping
 */
int ompi_file_finalize(void)
{
    size_t i, max;
    size_t num_unnamed;
    ompi_file_t *file;

    /* Release MPI_FILE_NULL.  Do this so that we don't get a bogus leak
       report on it.  Plus, it's statically allocated, so we don't want
     to call OBJ_RELEASE on it. */

    OBJ_DESTRUCT(&ompi_mpi_file_null);
    ompi_pointer_array_set_item(&ompi_file_f_to_c_table, 0, NULL);

    /* Iterate through all the file handles and destroy them.  Note
       that this also takes care of destroying MPI_FILE_NULL. */

    max = ompi_pointer_array_get_size(&ompi_file_f_to_c_table);
    for (num_unnamed = i = 0; i < max; ++i) {
        file = ompi_pointer_array_get_item(&ompi_file_f_to_c_table, i);
        
        /* If the file was closed but still exists because the user
           told us to never free handles, then do an OBJ_RELEASE it
           and all is well.  Then get the value again and see if it's
           actually been freed. */

        if (NULL != file && ompi_debug_no_free_handles && 
            0 == (file->f_flags & OMPI_FILE_ISCLOSED)) {
            OBJ_RELEASE(file);
            file = ompi_pointer_array_get_item(&ompi_file_f_to_c_table, i);
        } 
        
        if (NULL != file) {

            /* If the user wanted warnings about MPI object leaks,
               print out a message */

            if (ompi_debug_show_handle_leaks) {
                ++num_unnamed;
            }

            OBJ_RELEASE(file);
        }
        /* Don't bother setting each element back down to NULL; it
           would just take a lot of thread locks / unlocks and since
           we're destroying everything, it isn't worth it */
    }
    if (num_unnamed > 0) {
        ompi_output(0, "WANRING: %d unnamed MPI_File handles still allocated at MPI_FINALIZE", num_unnamed);
    }
    OBJ_DESTRUCT(&ompi_file_f_to_c_table);
  
    /* All done */

    return OMPI_SUCCESS;
}


/*
 * Constructor
 */
static void file_constructor(ompi_file_t *file)
{
    /* Initialize the MPI_FILE_OPEN params */

    file->f_comm = NULL;
    file->f_filename = NULL;
    file->f_amode = 0;

    /* Initialize flags */

    file->f_flags = 0;

    /* Initialize the fortran <--> C translation index */

    file->f_f_to_c_index = ompi_pointer_array_add(&ompi_file_f_to_c_table, 
                                                  file);

    /* Initialize the error handler.  Per MPI-2:9.7 (p265), the
       default error handler on file handles is the error handler on
       MPI_FILE_NULL, which starts out as MPI_ERRORS_RETURN (but can
       be changed by invoking MPI_FILE_SET_ERRHANDLER on
       MPI_FILE_NULL). */

    if (file != &ompi_mpi_file_null) {
        file->error_handler = ompi_mpi_file_null.error_handler;
    } else {
        file->error_handler = &ompi_mpi_errors_return;
    }
    OBJ_RETAIN(file->error_handler);

    /* Initialize the module */

    file->f_io_version = OMPI_IO_VERSION_NONE;
    memset(&(file->f_io_selected_module), 0, 
           sizeof(file->f_io_selected_module));
    file->f_selected_data = NULL;

    /* If the user doesn't want us to ever free it, then add an extra
       RETAIN here */

    if (ompi_debug_no_free_handles) {
        OBJ_RETAIN(&(file->super));
    }
}


/*
 * Destructor
 */
static void file_destructor(ompi_file_t *file)
{
    /* Finalize the module */

    switch (file->f_io_version) {
    case OMPI_IO_VERSION_1_0_0:
#if 0
        /* JMS need to implement */
        file->f_io_selected_module.v1_0_0.iom_finalize(file);
#endif
        break;
        
    case OMPI_IO_VERSION_2_0_0:
        /* JMS fill in here */
        break;
        
    default:
        /* All other cases are uninitialized or MPI_FILE_NULL */
        break;
    }
    
    /* Finalize the data members */

    if (NULL != file->f_comm) {
        OBJ_RELEASE(file->f_comm);
#if OMPI_ENABLE_DEBUG
        file->f_comm = NULL;
#endif
    }

    if (NULL != file->f_filename) {
        free(file->f_filename);
#if OMPI_ENABLE_DEBUG
        file->f_filename = NULL;
#endif
    }

    if (NULL != file->error_handler) {
        OBJ_RELEASE(file->error_handler);
#if OMPI_ENABLE_DEBUG
        file->error_handler = NULL;
#endif
    }

    /* Reset the f_to_c table entry */

    if (MPI_UNDEFINED != file->f_f_to_c_index &&
        NULL != ompi_pointer_array_get_item(&ompi_file_f_to_c_table,
                                            file->f_f_to_c_index)) {
        ompi_pointer_array_set_item(&ompi_file_f_to_c_table,
                                    file->f_f_to_c_index, NULL);
    }
}
