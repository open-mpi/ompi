/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "attribute/attribute.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Type_delete_attr = PMPI_Type_delete_attr
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Type_delete_attr";


int MPI_Type_delete_attr (MPI_Datatype type, int type_keyval)
{
   int ret;

   if (MPI_PARAM_CHECK) {
      OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
      if (NULL == type || MPI_DATATYPE_NULL == type) {
         return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, 
                                       MPI_ERR_TYPE, 
                                       FUNC_NAME);
      }
   }
  
   ret = ompi_attr_delete(TYPE_ATTR, type, type->d_keyhash, type_keyval, 0);
   OMPI_ERRHANDLER_RETURN(ret, MPI_COMM_WORLD,
			  MPI_ERR_OTHER, FUNC_NAME);  
}
