/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "mpi.h"
#include "attribute/attribute.h"


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

static int attr_appnum = 0;
static int attr_lastusedcode = 0;
static int attr_universe_size = 0;
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
    int err;

    /* Set some default values */

    /* JMS fill in values here */

    /* DO NOT CHANGE THE ORDER OF CREATING THESE KEYVALS!  This order
       strictly adheres to the order in mpi.h.  If you change the
       order here, you must change the order in mpi.h as well! */

    if (OMPI_SUCCESS != (err = set(MPI_TAG_UB, &attr_tag_ub)) ||
        OMPI_SUCCESS != (err = set(MPI_HOST, &attr_host)) ||
        OMPI_SUCCESS != (err = set(MPI_IO, &attr_io)) ||
        OMPI_SUCCESS != (err = set(MPI_WTIME_IS_GLOBAL,
                                   &attr_wtime_is_global)) ||
        OMPI_SUCCESS != (err = set(MPI_APPNUM, &attr_appnum)) ||
        OMPI_SUCCESS != (err = set(MPI_LASTUSEDCODE, &attr_lastusedcode)) ||
        OMPI_SUCCESS != (err = set(MPI_UNIVERSE_SIZE, &attr_universe_size)) ||
#if 0
        /* JMS for when we implement windows */
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
        printf("BADNESS 1: keyval %d, expected %d\n", keyval, target_keyval);
        return err;
    }
    err = ompi_attr_set(COMM_ATTR, MPI_COMM_WORLD,
                        MPI_COMM_WORLD->c_keyhash, keyval, value, 1);
    if (OMPI_SUCCESS != err) {
        printf("BADNESS 2: keyval: %d\n", keyval);
    }

    return OMPI_SUCCESS;
}
