/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "mpi.h"
#include "attribute/attribute.h"
#include "util/proc_info.h"
#include "util/bufpack.h"
#include "errhandler/errclass.h"
#include "communicator/communicator.h"
#include "mca/ns/ns.h"
#include "mca/ns/base/base.h"
#include "mca/gpr/gpr.h"
#include "mca/gpr/base/base.h"


/*
 * Private functions
 */
static int set(int keyval, void *value);


/*
 * Back-end for attribute values
 */
static int attr_tag_ub = MPI_TAG_UB_VALUE;
static char *attr_host = NULL;
static int attr_io = 1;
static int attr_wtime_is_global = 0;

/* Filled in at run-time, below */
static int attr_appnum = -1;
/* Filled in at run-time, below */
static int attr_universe_size = -1;

#if 0
/* JMS for when we implement windows */
static int attr_win_base = 0;
static int attr_win_size = 0;
static int attr_win_disp_unit = 0;
#endif

#if 0
/* JMS for when we implement IMPI */
static int attr_impi_client_size = 0;
static int attr_impi_client_color = 0;
static int attr_impi_host_size = 0;
static int attr_impi_host_color = 0;
#endif


int ompi_attr_create_predefined(void)
{
    int num, err;
    ompi_list_t *universe;
    ompi_list_item_t *item;
    ompi_registry_value_t *reg_value;
    ompi_buffer_t buffer;
    char *bogus;
    ompi_process_name_t *name;

    /* Set some default values */

    attr_appnum = (int) ompi_name_server.get_jobid(ompi_process_info.name);

    /* Query the registry to find out how many CPUs there will be.
       This will only return a non-empty list in a persistent
       universe.  If we don't have a persistent universe, then just
       default to the size of MPI_COMM_WORLD. 

       JMS: I think we need more here -- there are cases where you
       wouldn't have a persistent universe but still may have a
       comm_size(COMM_WORLD) != UNIVERSE_SIZE.  For example, say you
       reserve 8 CPUs in a batch environment and then run ./master,
       where the master is supposed to SPAWN the other processes.
       Perhaps need some integration with the LLM here...?  [shrug] */

    universe = ompi_registry.get(OMPI_REGISTRY_OR, "ompi-vm", NULL);
    attr_universe_size = 0;
    if (0 == ompi_list_get_size(universe)) {
        attr_universe_size = ompi_comm_size(MPI_COMM_WORLD);
    } else {
        for (item = ompi_list_remove_first(universe);
             NULL != item;
             item = ompi_list_remove_first(universe)) {
            reg_value = (ompi_registry_value_t *) item;
            buffer = (ompi_buffer_t) reg_value->object;
            
            /* Node name */
            ompi_unpack_string(buffer, &bogus);
            free(bogus);
            
            /* Process name */
            ompi_unpack(buffer, &name, 1, OMPI_NAME);
            free(name);
            
            /* OOB contact info */
            ompi_unpack_string(buffer, &bogus);
            free(bogus);
            
            /* Process slot count */
            ompi_unpack(buffer, &num, 1, OMPI_INT32);
            attr_universe_size += num;
            
            /* Discard the rest */
            ompi_buffer_free(buffer);
            OBJ_RELEASE(item);
        }
    }
    OBJ_RELEASE(universe);

    /* DO NOT CHANGE THE ORDER OF CREATING THESE KEYVALS!  This order
       strictly adheres to the order in mpi.h.  If you change the
       order here, you must change the order in mpi.h as well! */

    if (OMPI_SUCCESS != (err = set(MPI_TAG_UB, &attr_tag_ub)) ||
        OMPI_SUCCESS != (err = set(MPI_HOST, &attr_host)) ||
        OMPI_SUCCESS != (err = set(MPI_IO, &attr_io)) ||
        OMPI_SUCCESS != (err = set(MPI_WTIME_IS_GLOBAL,
                                   &attr_wtime_is_global)) ||
        OMPI_SUCCESS != (err = set(MPI_APPNUM, &attr_appnum)) ||
        OMPI_SUCCESS != (err = set(MPI_LASTUSEDCODE,
                                   &ompi_errclass_lastused)) ||
        OMPI_SUCCESS != (err = set(MPI_UNIVERSE_SIZE, &attr_universe_size)) ||
#if 0
        /* JMS for when we implement windows */
        /* JMS BE SURE TO READ ALL OF MPI-2 4.12.7 BEFORE IMPLEMENTING
           THESE ADDRESS-VALUED ATTRIBUTES! */
        OMPI_SUCCESS != (err = set(MPI_WIN_BASE, &attr_win_base)) ||
        OMPI_SUCCESS != (err = set(MPI_WIN_SIZE, &attr_win_size)) ||
        OMPI_SUCCESS != (err = set(MPI_WIN_DISP_UNIT, &attr_win_disp_unit)) ||
#endif
#if 0
        /* JMS For when we implement IMPI */
        OMPI_SUCCESS != (err = set(MPI_IMPI_CLIENT_SIZE,
                                   &attr_impi_client_size)) ||
        OMPI_SUCCESS != (err = set(MPI_IMPI_CLIENT_COLOR, 
                                   &attr_impi_client_color)) ||
        OMPI_SUCCESS != (err = set(MPI_IMPI_HOST_SIZE,
                                   &attr_impi_host_size)) ||
        OMPI_SUCCESS != (err = set(MPI_IMPI_HOST_COLOR, 
                                   &attr_impi_host_color)) ||
#endif
        0) {
        return err;
    }

    return OMPI_SUCCESS;
}


static int set(int target_keyval, void *value)
{
    int err;
    int keyval;
    ompi_attribute_fn_ptr_union_t copy;
    ompi_attribute_fn_ptr_union_t del;

    keyval = -1;
    copy.attr_communicator_copy_fn = MPI_COMM_DUP_FN;
    del.attr_communicator_delete_fn = MPI_COMM_NULL_DELETE_FN;
    err = ompi_attr_create_keyval(COMM_ATTR, copy, del,
                                  &keyval, NULL, OMPI_KEYVAL_PREDEFINED);
    if (keyval != target_keyval || OMPI_SUCCESS != err) {
        return err;
    }
    err = ompi_attr_set(COMM_ATTR, MPI_COMM_WORLD,
                        &MPI_COMM_WORLD->c_keyhash, keyval, value, 1);
    if (OMPI_SUCCESS != err) {
        return err;
    }

    return OMPI_SUCCESS;
}
