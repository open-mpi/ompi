/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "attribute/attribute.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Keyval_create = PMPI_Keyval_create
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Keyval_create";


OMPI_EXPORT
int MPI_Keyval_create(MPI_Copy_function *copy_attr_fn,
                      MPI_Delete_function *delete_attr_fn,
                      int *keyval, void *extra_state) 
{
    int ret;
    ompi_attribute_fn_ptr_union_t copy_fn;
    ompi_attribute_fn_ptr_union_t del_fn;

    if (MPI_PARAM_CHECK) {
      OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
      if (NULL == keyval) {
        return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_KEYVAL, 
                                      FUNC_NAME);
      } else if ((NULL == copy_attr_fn) || (NULL == delete_attr_fn)) {
	    return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG, 
					 FUNC_NAME);
      }
    }
    copy_fn.attr_communicator_copy_fn = copy_attr_fn;
    del_fn.attr_communicator_delete_fn = delete_attr_fn;

    ret = ompi_attr_create_keyval(COMM_ATTR, copy_fn, 
				 del_fn, keyval, extra_state, 0);
    OMPI_ERRHANDLER_RETURN(ret, MPI_COMM_WORLD, MPI_ERR_OTHER, FUNC_NAME);
}
