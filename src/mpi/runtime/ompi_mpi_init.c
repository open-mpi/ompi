/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "include/constants.h"
#include "mpi/runtime/mpiruntime.h"
#include "mpi/runtime/params.h"
#include "runtime/runtime.h"
#include "util/sys_info.h"
#include "util/proc_info.h"
#include "mpi.h"
#include "communicator/communicator.h"
#include "group/group.h"
#include "info/info.h"
#include "util/common_cmd_line.h"
#include "errhandler/errhandler.h"
#include "errhandler/errcode.h"
#include "errhandler/errclass.h"
#include "errhandler/errcode-internal.h"
#include "op/op.h"
#include "file/file.h"

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
#include "mca/topo/topo.h"
#include "mca/topo/base/base.h"
#include "mca/io/io.h"
#include "mca/io/base/base.h"


/*
 * Global variables and symbols for the MPI layer
 */

bool ompi_mpi_initialized = false;
bool ompi_mpi_finalized = false;

bool ompi_mpi_thread_multiple = false;
int ompi_mpi_thread_requested = MPI_THREAD_SINGLE;
int ompi_mpi_thread_provided = MPI_THREAD_SINGLE;


int ompi_mpi_init(int argc, char **argv, int requested, int *provided)
{
    int ret, param;
    bool allow_multi_user_threads;
    bool have_hidden_threads;
    ompi_proc_t** procs;
    size_t nprocs;

    /* Save command line parameters */

    if (OMPI_SUCCESS != (ret = ompi_common_cmd_line_init(argc, argv))) {
        /* JMS show_help */
        printf("show_help: ompi_mpi_init failed in ompi_common_cmd_line_init\n");
        return ret;
    }

    /* Become a OMPI process */

    if (OMPI_SUCCESS != (ret = ompi_init(argc, argv))) {
        /* JMS show_help */
        printf("show_help: ompi_mpi_init failed in ompi_init\n");
        return ret;
    }

    /* Open up the MCA */

    if (OMPI_SUCCESS != (ret = mca_base_open())) {
        /* JMS show_help */
        printf("show_help: ompi_mpi_init failed in mca_base_open\n");
        return ret;
    }

    /* Join the run-time environment */
    allow_multi_user_threads = true;
    have_hidden_threads = false;
    if (OMPI_SUCCESS != (ret = ompi_rte_init(&allow_multi_user_threads,
                                             &have_hidden_threads))) {
        /* JMS show_help */
        printf("show_help: ompi_mpi_init failed in mca_rte_init\n");
        return ret;
    }

    /* Once we've joined the RTE, see if any MCA parameters were
       passed to the MPI level */

    if (OMPI_SUCCESS != (ret = ompi_mpi_register_params())) {
        /* JMS show_help */
        printf("show_help: ompi_mpi_init failed in mca_mpi_register_params\n");
        return ret;
    }

    /* initialize ompi procs */
    if (OMPI_SUCCESS != (ret = ompi_proc_init())) {
        /* JMS show_help */
        printf("show_help: ompi_mpi_init failed in mca_proc_init\n");
        return ret;
    }

    /* Open up relevant MCA modules. */

    if (OMPI_SUCCESS != (ret = mca_allocator_base_open())) {
        /* JMS show_help */
        printf("show_help: ompi_mpi_init failed in mca_allocator_base_open\n");
        return ret;
    }
    if (OMPI_SUCCESS != (ret = mca_mpool_base_open())) {
        /* JMS show_help */
        printf("show_help: ompi_mpi_init failed in mca_mpool_base_open\n");
        return ret;
    }
    if (OMPI_SUCCESS != (ret = mca_pml_base_open())) {
        /* JMS show_help */
        printf("show_help: ompi_mpi_init failed in mca_pml_base_open\n");
        return ret;
    }
    if (OMPI_SUCCESS != (ret = mca_ptl_base_open())) {
        /* JMS show_help */
        printf("show_help: ompi_mpi_init failed in mca_ptl_base_open\n");
        return ret;
    }
    if (OMPI_SUCCESS != (ret = mca_coll_base_open())) {
        /* JMS show_help */
        printf("show_help: ompi_mpi_init failed in mca_coll_base_open\n");
        return ret;
    }
    if (OMPI_SUCCESS != (ret = mca_topo_base_open())) {
        /* JMS show_help */
        printf("show_help: ompi_mpi_init failed in mca_topo_base_open\n");
        return ret;
    }
    if (OMPI_SUCCESS != (ret = mca_io_base_open())) {
        /* JMS show_help */
        printf("show_help: ompi_mpi_init failed in mca_io_base_open\n");
        return ret;
    }

    /* Select which pml, ptl, and coll modules to use, and determine the
         final thread level */

    if (OMPI_SUCCESS != 
            (ret = mca_base_init_select_components(requested, 
                                                   allow_multi_user_threads,
                                                   have_hidden_threads, 
                                                   provided))) {
        /* JMS show_help */
        printf("show_help: ompi_mpi_init failed in mca_base_init_select_modules\n");
        return ret;
    }

     /* initialize info */
     if (OMPI_SUCCESS != (ret = ompi_info_init())) {
     /* JMS show_help */
         printf("show_help: ompi_mpi_init failed in ompi_info_init\n");
         return ret;
     }
     /* initialize error handlers */
     if (OMPI_SUCCESS != (ret = ompi_errhandler_init())) {
         /* JMS show_help */
         printf("show_help: ompi_mpi_init failed in ompi_errhandler_init\n");
         return ret;
     }

     /* initialize error codes */
     if (OMPI_SUCCESS != (ret = ompi_mpi_errcode_init())) {
         /* JMS show_help */
         printf("show_help: ompi_mpi_init failed in ompi_errcode_init\n");
         return ret;
     }

     /* initialize error classes */
     if (OMPI_SUCCESS != (ret = ompi_errclass_init())) {
         /* JMS show_help */
         printf("show_help: ompi_mpi_init failed in ompi_errclass_init\n");
         return ret;
     }

     /* initialize internal error codes */
     if (OMPI_SUCCESS != (ret = ompi_errcode_intern_init())) {
         /* JMS show_help */
         printf("show_help: ompi_mpi_init failed in ompi_errcode_internal_init\n");
         return ret;
     }
     
     /* initialize groups  */
     if (OMPI_SUCCESS != (ret = ompi_group_init())) {
         /* JMS show_help */
         printf("show_help: ompi_mpi_init failed in ompi_group_init\n");
         return ret;
     }

     /* initialize attribute meta-data structure for comm/win/dtype */
     if (OMPI_SUCCESS != (ret = ompi_attr_init())) {
         /* JMS show_help */
         printf("show_help: ompi_mpi_init failed in ompi_attr_init\n");
	 return ret;
     }

     /* initialize communicators */
     if (OMPI_SUCCESS != (ret = ompi_comm_init())) {
         /* JMS show_help */
         printf("show_help: ompi_mpi_init failed in ompi_comm_init\n");
         return ret;
     }

     /* initialize datatypes */
     if (OMPI_SUCCESS != (ret = ompi_ddt_init())) {
         /* JMS show_help */
         printf("show_help: ompi_mpi_init failed in ompi_ddt_init\n");
         return ret;
     }

     /* initialize ops */
     if (OMPI_SUCCESS != (ret = ompi_op_init())) {
         /* JMS show_help */
         printf("show_help: ompi_mpi_init failed in ompi_op_init\n");
         return ret;
     }

     /* initialize file handles */
     if (OMPI_SUCCESS != (ret = ompi_file_init())) {
         /* JMS show_help */
         printf("show_help: ompi_mpi_init failed in ompi_file_init\n");
         return ret;
     }

     /* do module exchange */
     if (OMPI_SUCCESS != (ret = mca_base_modex_exchange())) {
         /* JMS show_help */
         printf("show_help: ompi_mpi_init failed in mca_base_modex_exchange\n");
         return ret;
     }

     /* add all ompi_proc_t's to PML */
     if (NULL == (procs = ompi_proc_world(&nprocs))) {
         /* JMS show_help */
         printf("show_help: ompi_mpi_init failed in NULL proc world!\n");
         return OMPI_ERROR;
     }
     if (OMPI_SUCCESS != (ret = mca_pml.pml_add_procs(procs, nprocs))) {
         /* JMS show_help */
         printf("show_help: ompi_mpi_init failed in pml_add_procs!\n");
         free(procs);
         return ret;
     }
     free(procs);

     /* start PTL's */
     param = 1;
     if (OMPI_SUCCESS != 
         (ret = mca_pml.pml_control(MCA_PTL_ENABLE, &param, sizeof(param)))) {
         /* JMS show_help */
         printf("show_help: ompi_mpi_init failed in pml_control!\n");
         return ret;
     }

     /* save the resulting thread levels */

    ompi_mpi_thread_requested = requested;
    *provided = ompi_mpi_thread_provided;
    ompi_mpi_thread_multiple = (ompi_mpi_thread_provided == 
                                MPI_THREAD_MULTIPLE);

    /* Init coll for the comms */

    if (OMPI_ERROR == mca_coll_base_comm_select(MPI_COMM_SELF, NULL)) {
         /* JMS show_help */
         printf("show_help: ompi_mpi_init failed in mca_coll_base_comm_select SELF!\n");
	return OMPI_ERROR;
    }

    if (OMPI_ERROR == mca_coll_base_comm_select(MPI_COMM_WORLD, NULL)) {
         /* JMS show_help */
         printf("show_help: ompi_mpi_init failed in mca_coll_base_comm_select WORLD!\n");
	return OMPI_ERROR;
    }

    /* Wait for everyone to initialize */

    if (MPI_SUCCESS != (ret = 
                        MPI_COMM_WORLD->c_coll.coll_barrier(MPI_COMM_WORLD))) {
         /* JMS show_help */
         printf("show_help: ompi_mpi_init failed in WORLD barrier!\n");
	return ret;
    }

    /* All done */

    ompi_mpi_initialized = true;
    ompi_mpi_finalized = false;
    return MPI_SUCCESS;
}
