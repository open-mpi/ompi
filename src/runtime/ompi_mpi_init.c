/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "include/constants.h"
#include "runtime/runtime.h"
#include "mpi.h"
#include "runtime/runtime.h"
#include "communicator/communicator.h"
#include "group/group.h"
#include "errhandler/errhandler.h"
#include "op/op.h"
#include "mca/base/base.h"
#include "mca/base/base.h"
#include "mca/allocator/base/base.h"
#include "mca/allocator/allocator.h"
#include "mca/mpool/base/base.h"
#include "mca/mpool/mpool.h"
#include "mca/ptl/ptl.h"
#include "mca/ptl/base/base.h"
#include "mca/pml/pml.h"
#include "mca/pml/base/base.h"
#include "mca/coll/coll.h"
#include "mca/coll/base/base.h"


/*
 * Global variables and symbols for the MPI layer
 */

bool ompi_mpi_initialized = false;
bool ompi_mpi_finalized = false;
/* As a deviation from the norm, this variable is extern'ed in
   src/mpi/interface/c/bindings.h because it is already included in
   all MPI function imlementation files */
bool ompi_mpi_param_check = true;

bool ompi_mpi_thread_multiple = false;
int ompi_mpi_thread_requested = MPI_THREAD_SINGLE;
int ompi_mpi_thread_provided = MPI_THREAD_SINGLE;


int ompi_mpi_init(int argc, char **argv, int requested, int *provided)
{
    int ret, param, value;
    bool allow_multi_user_threads;
    bool have_hidden_threads;
    ompi_proc_t** procs;
    size_t nprocs;

    /* Become a OMPI process */

    if (OMPI_SUCCESS != (ret = ompi_init(argc, argv))) {
        return ret;
    }

    /* Open up the MCA */

    if (OMPI_SUCCESS != (ret = mca_base_open())) {
        return ret;
    }

    /* Join the run-time environment */
    if (OMPI_SUCCESS != (ret = ompi_rte_init(&allow_multi_user_threads, &have_hidden_threads))) {
        return ret;
    }

    /* initialize ompi procs */
    if (OMPI_SUCCESS != (ret = ompi_proc_init())) {
        return ret;
    }

    /* Open up relevant MCA modules.    Do not open io, topo, or one
         module types here -- they are loaded upon demand (i.e., upon
         relevant constructors). */
    if (OMPI_SUCCESS != (ret = mca_allocator_base_open())) {
        /* JMS show_help */
        return ret;
    }
    if (OMPI_SUCCESS != (ret = mca_mpool_base_open())) {
        /* JMS show_help */
        return ret;
    }
    if (OMPI_SUCCESS != (ret = mca_pml_base_open())) {
        /* JMS show_help */
        return ret;
    }
    if (OMPI_SUCCESS != (ret = mca_ptl_base_open())) {
        /* JMS show_help */
        return ret;
    }
    if (OMPI_SUCCESS != (ret = mca_coll_base_open())) {
        /* JMS show_help */
        return ret;
    }

    /* Select which pml, ptl, and coll modules to use, and determine the
         final thread level */

    if (OMPI_SUCCESS != 
            (ret = mca_base_init_select_modules(requested, allow_multi_user_threads,
             have_hidden_threads, provided))) {
        /* JMS show_help */
        return ret;
    }

    /* Query the coll */

    if (OMPI_SUCCESS != (ret = mca_coll_base_query()))
	return ret;

     /* initialize error handlers */
     if (OMPI_SUCCESS != (ret = ompi_errhandler_init())) {
         return ret;
     }
     
     /* initialize groups  */
     if (OMPI_SUCCESS != (ret = ompi_group_init())) {
         return ret;
     }

     /* initialize attribute meta-data structure for comm/win/dtype */
     if (OMPI_SUCCESS != (ret = ompi_attr_init())) {
	 return ret;
     }

     /* initialize communicators */
     if (OMPI_SUCCESS != (ret = ompi_comm_init())) {
         return ret;
     }

     /* initialize datatypes */
     if (OMPI_SUCCESS != (ret = ompi_ddt_init())) {
         return ret;
     }

     /* initialize ops */
     if (OMPI_SUCCESS != (ret = ompi_op_init())) {
         return ret;
     }

     /* If we have run-time MPI parameter checking possible, register
        an MCA paramter to find out if the user wants it on or off by
        default */
     param = mca_base_param_register_int("mpi", NULL, "error_check", NULL, 1);
     mca_base_param_lookup_int(param, &value);
     ompi_mpi_param_check = (bool) value; 

     /* do module exchange */
     if (OMPI_SUCCESS != (ret = mca_base_modex_exchange())) {
         return ret;
     }

     /* add all ompi_proc_t's to PML */
     if (NULL == (procs = ompi_proc_world(&nprocs)))
         return OMPI_ERROR;
     if (OMPI_SUCCESS != (ret = mca_pml.pml_add_procs(procs, nprocs))) {
         free(procs);
         return ret;
     }
     free(procs);

     /* start PTL's */
     param = 1;
     if (OMPI_SUCCESS != (ret = mca_pml.pml_control(MCA_PTL_ENABLE, &param, sizeof(param))))
         return ret;

     /* save the resulting thread levels */

    ompi_mpi_thread_requested = requested;
    *provided = ompi_mpi_thread_provided;
    ompi_mpi_thread_multiple = (ompi_mpi_thread_provided == MPI_THREAD_MULTIPLE);

    /* Init coll for the comms */

    if (OMPI_ERROR == mca_coll_base_init_comm(MPI_COMM_SELF))
	return OMPI_ERROR;

    if (OMPI_ERROR == mca_coll_base_init_comm(MPI_COMM_WORLD))
	return OMPI_ERROR;

    /* Wait for everyone to initialize */
    /* Change the Barrier call to the backend call */

    if (MPI_SUCCESS != (ret = MPI_Barrier(MPI_COMM_WORLD)))
	return ret;

    /* All done */

    ompi_mpi_initialized = true;
    ompi_mpi_finalized = false;
    return MPI_SUCCESS;
}
