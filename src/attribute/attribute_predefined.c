/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/**
 * @file
 *
 * Setup the predefined attributes in MPI.
 *
 * A number of pre-defined attributes are created here, most of which
 * are exactly what one would expect, but there are a few exceptions
 * -- so they're documented here.
 *
 * Predefined attributes are integer-valued or address-valued (per
 * MPI-2; see section 4.12.7, keeping in mind that Example 4.13 is
 * totally wrong -- see src/attribute/attribute.h for a lengthy
 * explanation of this).  
 *
 * The only address-valued attribute is MPI_WIN_BASE.  We treat it as
 * if it were set from C.  All other attributes are integer-valued.
 * We treat them as if they were set from Fortran MPI-1 (i.e.,
 * MPI_ATTR_PUT) or Fortran MPI-2 (i.e., MPI_xxx_ATTR_SET).  Most
 * attributes are MPI-1 integer-valued, meaning that they are the size
 * of MPI_Fint (INTEGER).  But MPI_WIN_SIZE and MPI_WIN_DISP_UNIT are
 * MPI-2 integer-valued, meaning that they are the size of MPI_Aint
 * (INTEGER(KIND=MPI_ADDRESS_KIND)).
 *
 * MPI_TAG_UB is set to a fixed upper limit.
 *
 * MPI_HOST is set to MPI_PROC_NULL (per MPI-1, see 7.1.1, p192).
 *
 * MPI_IO is set to MPI_ANY_SOURCE.  We may need to revist this.
 *
 * MPI_WTIME_IS_GLOBAL is set to 0 (a conservative answer).
 *
 * MPI_APPNUM is set as the result of a GPR subscription.
 *
 * MPI_LASTUSEDCODE is set to an initial value and is reset every time
 * MPI_ADD_ERROR_CLASS is invoked.  Its copy function is set to
 * MPI_COMM_NULL_COPY_FN, meaning that *only* MPI_COMM_WORLD will have
 * this attribute value.  As such, we only have to update
 * MPI_COMM_WORLD when this value changes (i.e., since this is an
 * integer-valued attribute, we have to update this attribute on every
 * communicator -- using NULL_COPY_FN ensures that only MPI_COMM_WORLD
 * has this attribute value set).
 *
 * MPI_UNIVERSE_SIZE is set as the result of a GPR subscription.
 *
 * MPI_WIN_BASE is an address-valued attribute, and is set directly
 * from MPI_WIN_CREATE.  MPI_WIN_SIZE and MPI_WIN_DISP_UNIT are both
 * integer-valued attributes, *BUT* at least the MPI_WIN_SIZE is an
 * MPI_Aint, so in terms of consistency, both should be the same --
 * hence, we treat them as MPI-2 Fortran integer-valued attributes.
 * All three of these atrributes have NULL_COPY_FN copy functions; it
 * doesn't make sense to copy them to new windows (because they're
 * values specific and unique to each window) -- especially when
 * WIN_CREATE will explicitly set them on new windows anyway.
 *
 * These are not supported yet, but are included here for consistency:
 *
 * MPI_IMPI_CLIENT_SIZE, MPI_IMPI_CLIENT_COLOR, MPI_IMPI_HOST_SIZE,
 * and MPI_IMPI_HOST_COLOR are integer-valued attributes.
 */

#include "ompi_config.h"

#include "mpi.h"

#include "attribute/attribute.h"

#include "errhandler/errclass.h"
#include "communicator/communicator.h"
#include "util/proc_info.h"
#include "util/sys_info.h"
#include "mca/ns/ns.h"
#include "mca/gpr/gpr.h"
#include "mca/errmgr/errmgr.h"


/*
 * Private functions
 */
static int create_comm(int target_keyval, bool want_inherit);
#if OMPI_WANT_MPI2_ONE_SIDED
/* JMS for when we implement windows */
static int create_win(int target_keyval);
#endif
static int set_f(int keyval, MPI_Fint value);


int ompi_attr_create_predefined(void)
{
    orte_gpr_notify_id_t rc;
    int ret;
    orte_gpr_value_t trig, *trig1;
    orte_gpr_subscription_t sub, *sub1;
    orte_jobid_t job;
    
    /* Create all the keyvals */

    /* DO NOT CHANGE THE ORDER OF CREATING THESE KEYVALS!  This order
       strictly adheres to the order in mpi.h.  If you change the
       order here, you must change the order in mpi.h as well! */
    
    if (OMPI_SUCCESS != (ret = create_comm(MPI_TAG_UB, true)) ||
        OMPI_SUCCESS != (ret = create_comm(MPI_HOST, true)) ||
        OMPI_SUCCESS != (ret = create_comm(MPI_IO, true)) ||
        OMPI_SUCCESS != (ret = create_comm(MPI_WTIME_IS_GLOBAL, true)) ||
        OMPI_SUCCESS != (ret = create_comm(MPI_APPNUM, true)) ||
        OMPI_SUCCESS != (ret = create_comm(MPI_LASTUSEDCODE, false)) ||
        OMPI_SUCCESS != (ret = create_comm(MPI_UNIVERSE_SIZE, true)) ||
#if OMPI_WANT_MPI2_ONE_SIDED
        /* JMS for when we implement windows */
        OMPI_SUCCESS != (ret = create_win(MPI_WIN_BASE)) ||
        OMPI_SUCCESS != (ret = create_win(MPI_WIN_SIZE)) ||
        OMPI_SUCCESS != (ret = create_win(MPI_WIN_DISP_UNIT)) ||
#endif
#if 0
        /* JMS For when we implement IMPI */
        OMPI_SUCCESS != (ret = create_comm(MPI_IMPI_CLIENT_SIZE, true)) ||
        OMPI_SUCCESS != (ret = create_comm(MPI_IMPI_CLIENT_COLOR, true)) ||
        OMPI_SUCCESS != (ret = create_comm(MPI_IMPI_HOST_SIZE, true)) ||
        OMPI_SUCCESS != (ret = create_comm(MPI_IMPI_HOST_COLOR, true)) ||
#endif
        0) {
        return ret;
    }

    /* Set default values for everything except UNIVERSE_SIZE and
       APPNUM. */

    if (OMPI_SUCCESS != (ret = set_f(MPI_TAG_UB, MPI_TAG_UB_VALUE)) ||
        OMPI_SUCCESS != (ret = set_f(MPI_HOST, MPI_PROC_NULL)) ||
        OMPI_SUCCESS != (ret = set_f(MPI_IO, MPI_ANY_SOURCE)) ||
        OMPI_SUCCESS != (ret = set_f(MPI_WTIME_IS_GLOBAL, 0)) ||
        OMPI_SUCCESS != (ret = set_f(MPI_LASTUSEDCODE, 
                                     ompi_errclass_lastused)) ||
#if 0
        /* JMS For when we implement IMPI */
        OMPI_SUCCESS != (ret = set(MPI_IMPI_CLIENT_SIZE,
                                   &attr_impi_client_size)) ||
        OMPI_SUCCESS != (ret = set(MPI_IMPI_CLIENT_COLOR, 
                                   &attr_impi_client_color)) ||
        OMPI_SUCCESS != (ret = set(MPI_IMPI_HOST_SIZE,
                                   &attr_impi_host_size)) ||
        OMPI_SUCCESS != (ret = set(MPI_IMPI_HOST_COLOR, 
                                   &attr_impi_host_color)) ||
#endif
        0) {
        return ret;
    }

    /* Now that those are all created, setup the trigger to get the
       UNIVERSE_SIZE and APPNUM values once everyone has passed
       stg1. */

    if (ORTE_SUCCESS != (ret = orte_ns.get_jobid(&job, orte_process_info.my_name))) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }
    
    OBJ_CONSTRUCT(&sub, orte_gpr_subscription_t);
    sub.addr_mode = ORTE_GPR_TOKENS_OR | ORTE_GPR_KEYS_OR;
    sub.segment = strdup(ORTE_NODE_SEGMENT);
    if (NULL == sub.segment) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&sub);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    sub.tokens = NULL; /* wildcard - look at all containers */
    sub.num_tokens = 0;
    sub.num_keys = 1;
    sub.keys = (char**)malloc(sizeof(char*));
    if (NULL == sub.keys) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&sub);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    sub.keys[0] = strdup(ORTE_NODE_SLOTS_KEY);
    if (NULL == sub.keys[0]) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&sub);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    sub.cbfunc = ompi_attr_create_predefined_callback;
    sub.user_tag = NULL;
    
    /* setup the trigger information */
    OBJ_CONSTRUCT(&trig, orte_gpr_value_t);
    trig.addr_mode = ORTE_GPR_TOKENS_XAND;
    if (ORTE_SUCCESS != (ret = orte_schema.get_job_segment_name(&(trig.segment), job))) {
        ORTE_ERROR_LOG(ret);
        OBJ_DESTRUCT(&sub);
        OBJ_DESTRUCT(&trig);
        return ret;
    }
    trig.tokens = (char**)malloc(sizeof(char*));
    if (NULL == trig.tokens) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&sub);
        OBJ_DESTRUCT(&trig);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    trig.tokens[0] = strdup(ORTE_JOB_GLOBALS);
    trig.num_tokens = 1;

    trig.cnt = 2;
    trig.keyvals = (orte_gpr_keyval_t**)malloc(2*sizeof(orte_gpr_keyval_t*));
    if (NULL == trig.keyvals) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&sub);
        OBJ_DESTRUCT(&trig);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    trig.keyvals[0] = OBJ_NEW(orte_gpr_keyval_t);
    if (NULL == trig.keyvals[0]) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&sub);
        OBJ_DESTRUCT(&trig);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    trig.keyvals[0]->key = strdup(ORTE_JOB_SLOTS_KEY);
    trig.keyvals[0]->type = ORTE_NULL;
    
    trig.keyvals[1] = OBJ_NEW(orte_gpr_keyval_t);
    if (NULL == trig.keyvals[1]) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&sub);
        OBJ_DESTRUCT(&trig);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    trig.keyvals[1]->key = strdup(ORTE_PROC_NUM_AT_STG1);
    trig.keyvals[1]->type = ORTE_NULL;
    
    /* do the subscription */
    sub1 = &sub;
    trig1 = &trig;
    ret = orte_gpr.subscribe(
         ORTE_GPR_TRIG_CMP_LEVELS | ORTE_GPR_TRIG_MONITOR_ONLY |
         ORTE_GPR_TRIG_ONE_SHOT,
         1, &sub1,
         1, &trig1,
         &rc);
     if(ORTE_SUCCESS != ret) {
         ompi_output(0, "ompi_attr_create_predefined: subscribe failed");
         OBJ_DESTRUCT(&sub);
         OBJ_DESTRUCT(&trig);
         return OMPI_ERROR;
     }
     OBJ_DESTRUCT(&sub);
     OBJ_DESTRUCT(&trig);
     return OMPI_SUCCESS;
}


void ompi_attr_create_predefined_callback(
    orte_gpr_notify_data_t *data,
    void *cbdata)
{
    size_t i, j;
    orte_gpr_keyval_t **keyval;
    orte_gpr_value_t **value;
    orte_jobid_t job;
    unsigned int universe_size = 0;

    /* Set some default values */

    if (ORTE_SUCCESS != orte_ns.get_jobid(&job, orte_process_info.my_name)) {
        return;
    }

    /* Per conversation between Jeff, Edgar, and Ralph - this needs to
     * be fixed to properly determine the appnum.  Ignore errors here;
     * there's no way to propagate the error up, so just try to keep
     * going.
     */
    set_f(MPI_APPNUM, (MPI_Fint) job);

    /* Query the gpr to find out how many CPUs there will be.
       This will only return a non-empty list in a persistent
       universe.  If we don't have a persistent universe, then just
       default to the size of MPI_COMM_WORLD. 

       JMS: I think we need more here -- there are cases where you
       wouldn't have a persistent universe but still may have a
       comm_size(COMM_WORLD) != UNIVERSE_SIZE.  For example, say you
       reserve 8 CPUs in a batch environment and then run ./master,
       where the master is supposed to SPAWN the other processes.
       Perhaps need some integration with the LLM here...?  [shrug] */

    /* RHC: Needed to change this code so it wouldn't issue a gpr.get
     * during the compound command phase of mpi_init. Since all you need
     * is to have the data prior to dtypes etc., and since this function
     * is called right before we send the compound command, I've changed
     * it to a subscription and a callback function. This allows you to
     * get the data AFTER the compound command executes. Nothing else
     * happens in-between anyway, so this shouldn't cause a problem.
     */

    if (0 == data->cnt) {  /* no data returned */
        universe_size = ompi_comm_size(MPI_COMM_WORLD);
    } else {
        value = data->values;
        for (i=0; i < data->cnt; i++) {
            if (0 < value[i]->cnt) {  /* make sure some data was returned here */
                keyval = value[i]->keyvals;
                for (j=0; j < value[i]->cnt; j++) {
                    /* make sure we don't get confused - all slot counts
                     * are in size_t fields
                     */
                    if (ORTE_SIZE == keyval[j]->type) {
                        /* Process slot count */
                        universe_size += keyval[j]->value.size;
                    }
                }
            }
        }
    }

    /* Same as above -- ignore errors here because there's nothing we
       can do if there's any error anyway */

    set_f(MPI_UNIVERSE_SIZE, universe_size);
    return;
}


static int create_comm(int target_keyval, bool want_inherit)
{
    int err;
    int keyval;
    ompi_attribute_fn_ptr_union_t copy;
    ompi_attribute_fn_ptr_union_t del;

    keyval = -1;
    copy.attr_communicator_copy_fn = 
        want_inherit ? MPI_COMM_DUP_FN : MPI_COMM_NULL_COPY_FN;
    del.attr_communicator_delete_fn = MPI_COMM_NULL_DELETE_FN;
    err = ompi_attr_create_keyval(COMM_ATTR, copy, del,
                                  &keyval, NULL, OMPI_KEYVAL_PREDEFINED);
    if (MPI_SUCCESS != err) {
        return err;
    }
    if (target_keyval != keyval) {
        return OMPI_ERR_BAD_PARAM;
    }
    return OMPI_SUCCESS;
}


#if OMPI_WANT_MPI2_ONE_SIDED
/* JMS for when we implement windows */
static int create_win(int target_keyval)
{
    int err;
    int keyval;
    ompi_attribute_fn_ptr_union_t copy;
    ompi_attribute_fn_ptr_union_t del;

    keyval = -1;
    copy.attr_win_copy_fn = MPI_WIN_NULL_COPY_FN;
    del.attr_win_delete_fn = MPI_WIN_NULL_DELETE_FN;
    err = ompi_attr_create_keyval(WIN_ATTR, copy, del,
                                  &keyval, NULL, OMPI_KEYVAL_PREDEFINED);
    if (MPI_SUCCESS != err) {
        return err;
    }
    if (target_keyval != keyval) {
        return OMPI_ERR_BAD_PARAM;
    }
    return OMPI_SUCCESS;
}
#endif


static int set_f(int keyval, MPI_Fint value)
{
    return ompi_attr_set_fortran_mpi1(COMM_ATTR, MPI_COMM_WORLD,
                                      &MPI_COMM_WORLD->c_keyhash, 
                                      keyval, value, 
                                      true, true);
}
