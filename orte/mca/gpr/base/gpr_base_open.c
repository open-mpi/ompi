/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
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

#include "orte_config.h"

#include "orte/orte_constants.h"

#include "opal/util/output.h"
#include "opal/util/trace.h"

#include "orte/dss/dss.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/gpr/base/base.h"



/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_module_t struct.
 */

#include "orte/mca/gpr/base/static-components.h"

/* JMS: This is only INT_MAX until bug 1345 is fixed, because this
   value is used to set an MAC parameter, which can [currently] only
   take an int. */
#define ORTE_GPR_ARRAY_MAX_SIZE INT_MAX
#define ORTE_GPR_ARRAY_BLOCK_SIZE 512



/*
 * globals
 */

/** KEYVAL **/
/* constructor - used to initialize state of keyval instance */
static void orte_gpr_keyval_construct(orte_gpr_keyval_t* keyval)
{
    keyval->key = NULL;
    keyval->value = NULL;
}

/* destructor - used to free any resources held by instance */
static void orte_gpr_keyval_destructor(orte_gpr_keyval_t* keyval)
{

    if (NULL != keyval->key) free(keyval->key);
    if (NULL != keyval->value) OBJ_RELEASE(keyval->value);
}

/* define instance of opal_class_t */
OBJ_CLASS_INSTANCE(
    orte_gpr_keyval_t,              /* type name */
    opal_list_item_t,               /* parent "class" name */
    orte_gpr_keyval_construct,      /* constructor */
    orte_gpr_keyval_destructor);    /* destructor */



/** VALUE **/
/* constructor - used to initialize state of registry value instance */
static void orte_gpr_value_construct(orte_gpr_value_t* reg_val)
{
    reg_val->addr_mode = 0;
    reg_val->segment = NULL;
    reg_val->cnt = 0;
    reg_val->keyvals = NULL;
    reg_val->num_tokens = 0;
    reg_val->tokens = NULL;
}

/* destructor - used to free any resources held by instance */
static void orte_gpr_value_destructor(orte_gpr_value_t* reg_val)
{
    char **tokens;
    orte_std_cntr_t i;

    if (NULL != reg_val->segment) free(reg_val->segment);

    if (0 < reg_val->cnt && NULL != reg_val->keyvals) {
        for (i=0; i < reg_val->cnt; i++) {
            if (NULL != reg_val->keyvals[i])
                OBJ_RELEASE(reg_val->keyvals[i]);
        }
       free(reg_val->keyvals);
    }

    if (0 < reg_val->num_tokens && NULL != reg_val->tokens) {
        tokens = reg_val->tokens;
        for (i=0; i < reg_val->num_tokens; i++) {
            if(NULL != tokens[i])
                free(tokens[i]);
        }
        free(tokens);
    }
}

/* define instance of opal_class_t */
OBJ_CLASS_INSTANCE(
           orte_gpr_value_t,  /* type name */
           opal_object_t, /* parent "class" name */
           orte_gpr_value_construct, /* constructor */
           orte_gpr_value_destructor); /* destructor */


/** NOTIFY DATA **/
/* constructor - used to initialize state of registry value instance */
static void orte_gpr_notify_data_construct(orte_gpr_notify_data_t* ptr)
{
    ptr->target = NULL;
    ptr->id = ORTE_GPR_SUBSCRIPTION_ID_MAX;
    ptr->remove = false;
    ptr->cnt = 0;
    orte_pointer_array_init(&(ptr->values), (orte_std_cntr_t)orte_gpr_array_block_size,
                            (orte_std_cntr_t)orte_gpr_array_max_size,
                            (orte_std_cntr_t)orte_gpr_array_block_size);

}

/* destructor - used to free any resources held by instance */
static void orte_gpr_notify_data_destructor(orte_gpr_notify_data_t* ptr)
{
    orte_std_cntr_t i, j;
    orte_gpr_value_t **values;

    if (NULL != ptr->target) free(ptr->target);

    if (NULL != ptr->values) {
        values = (orte_gpr_value_t**)(ptr->values)->addr;
        for (i=0, j=0; j < ptr->cnt &&
                       i < (ptr->values)->size; i++) {
            if (NULL != values[i]) {
                j++;
                OBJ_RELEASE(values[i]);
            }
        }
        OBJ_RELEASE(ptr->values);
    }
}

/* define instance of opal_class_t */
OBJ_CLASS_INSTANCE(
            orte_gpr_notify_data_t,  /* type name */
            opal_object_t, /* parent "class" name */
            orte_gpr_notify_data_construct, /* constructor */
            orte_gpr_notify_data_destructor); /* destructor */


/** SUBSCRIPTION **/
/* constructor - used to initialize state of registry subscription instance */
static void orte_gpr_subscription_construct(orte_gpr_subscription_t* sub)
{
    sub->name = NULL;
    sub->id = ORTE_GPR_SUBSCRIPTION_ID_MAX;
    sub->action = 0;
    sub->cnt = 0;
    sub->values = NULL;
    sub->cbfunc = NULL;
    sub->user_tag = NULL;
}

/* destructor - used to free any resources held by instance */
static void orte_gpr_subscription_destructor(orte_gpr_subscription_t* sub)
{
    orte_std_cntr_t i;

    if (NULL != sub->name) free(sub->name);

    if (0 < sub->cnt && NULL != sub->values) {
        for (i=0; i < sub->cnt; i++) {
            OBJ_RELEASE(sub->values[i]);
        }
        free(sub->values);
    }
}

/* define instance of opal_class_t */
OBJ_CLASS_INSTANCE(
         orte_gpr_subscription_t,  /* type name */
         opal_object_t, /* parent "class" name */
         orte_gpr_subscription_construct, /* constructor */
         orte_gpr_subscription_destructor); /* destructor */


/** TRIGGER **/
/* constructor - used to initialize state of registry subscription instance */
static void orte_gpr_trigger_construct(orte_gpr_trigger_t* trig)
{
    trig->name = NULL;
    trig->id = ORTE_GPR_TRIGGER_ID_MAX;
    trig->action = 0;
    trig->cnt = 0;
    trig->values = NULL;
    trig->cbfunc = NULL;
    trig->user_tag = NULL;
}

/* destructor - used to free any resources held by instance */
static void orte_gpr_trigger_destructor(orte_gpr_trigger_t* trig)
{
    orte_std_cntr_t i;

    if (NULL != trig->name) free(trig->name);

    if (0 < trig->cnt && NULL != trig->values) {
        for (i=0; i < trig->cnt; i++) OBJ_RELEASE(trig->values[i]);
        free(trig->values);
    }

}

/* define instance of opal_class_t */
OBJ_CLASS_INSTANCE(
         orte_gpr_trigger_t,  /* type name */
         opal_object_t, /* parent "class" name */
         orte_gpr_trigger_construct, /* constructor */
         orte_gpr_trigger_destructor); /* destructor */


/** NOTIFY MESSAGE */
/* constructor - used to initialize notify message instance */
static void orte_gpr_notify_message_construct(orte_gpr_notify_message_t* msg)
{
    msg->msg_type = 0;
    msg->target = NULL;
    msg->id = ORTE_GPR_TRIGGER_ID_MAX;
    msg->remove = false;
    msg->cnt = 0;
    orte_pointer_array_init(&(msg->data), (orte_std_cntr_t)orte_gpr_array_block_size,
                            (orte_std_cntr_t)orte_gpr_array_max_size,
                            (orte_std_cntr_t)orte_gpr_array_block_size);
}

/* destructor - used to free any resources held by instance */
static void orte_gpr_notify_message_destructor(orte_gpr_notify_message_t* msg)
{
    orte_std_cntr_t i, j;
    orte_gpr_notify_data_t **data;

    if (NULL != msg->target) free(msg->target);

    if (NULL != msg->data) {
        data = (orte_gpr_notify_data_t**)(msg->data)->addr;
        for (i=0, j=0; j < msg->cnt &&
                       i < (msg->data)->size; i++) {
            if (NULL != data[i]) {
                j++;
                OBJ_RELEASE(data[i]);
            }
        }
        OBJ_RELEASE(msg->data);
    }

}

/* define instance of opal_class_t */
OBJ_CLASS_INSTANCE(
            orte_gpr_notify_message_t,            /* type name */
            opal_object_t,                             /* parent "class" name */
            orte_gpr_notify_message_construct,    /* constructor */
            orte_gpr_notify_message_destructor);  /* destructor */


/*
 * Global variables
 */
int orte_gpr_base_output = -1;
size_t orte_gpr_array_max_size, orte_gpr_array_block_size;
orte_gpr_base_module_t orte_gpr;
bool orte_gpr_base_selected = false;
opal_list_t orte_gpr_base_components_available;
mca_gpr_base_component_t orte_gpr_base_selected_component;
opal_mutex_t orte_gpr_mutex;


/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int orte_gpr_base_open(void)
{
    int param, value, rc, id;
    orte_data_type_t tmp;
    opal_output_stream_t kill_prefix;

    OPAL_TRACE(5);

    /* Debugging / verbose output */
    /** setup the structure to kill the blasted prefix that opal_output
     * now defaults to including so the output can be legible again!
     */
    OBJ_CONSTRUCT(&kill_prefix, opal_output_stream_t);
    kill_prefix.lds_want_stderr = true;
    kill_prefix.lds_prefix = NULL;
    
    param = mca_base_param_reg_int_name("gpr_base", "verbose",
                                        "Verbosity level for the gpr framework",
                                        false, false, 0, &value);
    if (value != 0) {
        kill_prefix.lds_verbose_level = value;
    }
    orte_gpr_base_output = opal_output_open(&kill_prefix);
    OBJ_DESTRUCT(&kill_prefix);

    id = mca_base_param_register_int("gpr", "base", "maxsize", NULL,
                                     ORTE_GPR_ARRAY_MAX_SIZE);
    mca_base_param_lookup_int(id, &param);
    orte_gpr_array_max_size = (size_t)param;

    id = mca_base_param_register_int("gpr", "base", "blocksize", NULL,
                                     ORTE_GPR_ARRAY_BLOCK_SIZE);
    mca_base_param_lookup_int(id, &param);
    orte_gpr_array_block_size = (size_t)param;

    /* register the base data types with the DPS */
    tmp = ORTE_GPR_CMD;
    if (ORTE_SUCCESS != (rc = orte_dss.register_type(orte_gpr_base_pack_cmd,
                                          orte_gpr_base_unpack_cmd,
                                          (orte_dss_copy_fn_t)orte_gpr_base_copy_cmd,
                                          (orte_dss_compare_fn_t)orte_gpr_base_compare_cmd,
                                          (orte_dss_size_fn_t)orte_gpr_base_std_size,
                                          (orte_dss_print_fn_t)orte_gpr_base_std_print,
                                          (orte_dss_release_fn_t)orte_gpr_base_std_release,
                                          ORTE_DSS_UNSTRUCTURED,
                                          "ORTE_GPR_CMD", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    tmp = ORTE_GPR_SUBSCRIPTION_ID;
    if (ORTE_SUCCESS != (rc = orte_dss.register_type(orte_gpr_base_pack_subscription_id,
                                          orte_gpr_base_unpack_subscription_id,
                                          (orte_dss_copy_fn_t)orte_gpr_base_copy_subscription_id,
                                          (orte_dss_compare_fn_t)orte_gpr_base_compare_subscription_id,
                                          (orte_dss_size_fn_t)orte_gpr_base_std_size,
                                          (orte_dss_print_fn_t)orte_gpr_base_std_print,
                                          (orte_dss_release_fn_t)orte_gpr_base_std_release,
                                          ORTE_DSS_UNSTRUCTURED,
                                          "ORTE_GPR_SUBSCRIPTION_ID", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    tmp = ORTE_GPR_TRIGGER_ID;
    if (ORTE_SUCCESS != (rc = orte_dss.register_type(orte_gpr_base_pack_trigger_id,
                                          orte_gpr_base_unpack_trigger_id,
                                          (orte_dss_copy_fn_t)orte_gpr_base_copy_trigger_id,
                                          (orte_dss_compare_fn_t)orte_gpr_base_compare_trigger_id,
                                          (orte_dss_size_fn_t)orte_gpr_base_std_size,
                                          (orte_dss_print_fn_t)orte_gpr_base_std_print,
                                          (orte_dss_release_fn_t)orte_gpr_base_std_release,
                                          ORTE_DSS_UNSTRUCTURED,
                                          "ORTE_GPR_TRIGGER_ID", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    tmp = ORTE_GPR_NOTIFY_ACTION;
    if (ORTE_SUCCESS != (rc = orte_dss.register_type(orte_gpr_base_pack_notify_action,
                                          orte_gpr_base_unpack_notify_action,
                                          (orte_dss_copy_fn_t)orte_gpr_base_copy_notify_action,
                                          (orte_dss_compare_fn_t)orte_gpr_base_compare_notify_action,
                                          (orte_dss_size_fn_t)orte_gpr_base_std_size,
                                          (orte_dss_print_fn_t)orte_gpr_base_std_print,
                                          (orte_dss_release_fn_t)orte_gpr_base_std_release,
                                          ORTE_DSS_UNSTRUCTURED,
                                          "ORTE_GPR_NOTIFY_ACTION", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    tmp = ORTE_GPR_TRIGGER_ACTION;
    if (ORTE_SUCCESS != (rc = orte_dss.register_type(orte_gpr_base_pack_trigger_action,
                                          orte_gpr_base_unpack_trigger_action,
                                          (orte_dss_copy_fn_t)orte_gpr_base_copy_trigger_action,
                                          (orte_dss_compare_fn_t)orte_gpr_base_compare_trigger_action,
                                          (orte_dss_size_fn_t)orte_gpr_base_std_size,
                                          (orte_dss_print_fn_t)orte_gpr_base_std_print,
                                          (orte_dss_release_fn_t)orte_gpr_base_std_release,
                                          ORTE_DSS_UNSTRUCTURED,
                                          "ORTE_GPR_TRIGGER_ACTION", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    tmp = ORTE_GPR_NOTIFY_MSG_TYPE;
    if (ORTE_SUCCESS != (rc = orte_dss.register_type(orte_gpr_base_pack_notify_msg_type,
                                          orte_gpr_base_unpack_notify_msg_type,
                                          (orte_dss_copy_fn_t)orte_gpr_base_copy_notify_msg_type,
                                          (orte_dss_compare_fn_t)orte_gpr_base_compare_notify_msg_type,
                                          (orte_dss_size_fn_t)orte_gpr_base_std_size,
                                          (orte_dss_print_fn_t)orte_gpr_base_std_print,
                                          (orte_dss_release_fn_t)orte_gpr_base_std_release,
                                          ORTE_DSS_UNSTRUCTURED,
                                          "ORTE_GPR_NOTIFY_MSG_TYPE", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    tmp = ORTE_GPR_ADDR_MODE;
    if (ORTE_SUCCESS != (rc = orte_dss.register_type(orte_gpr_base_pack_addr_mode,
                                          orte_gpr_base_unpack_addr_mode,
                                          (orte_dss_copy_fn_t)orte_gpr_base_copy_addr_mode,
                                          (orte_dss_compare_fn_t)orte_gpr_base_compare_addr_mode,
                                          (orte_dss_size_fn_t)orte_gpr_base_std_size,
                                          (orte_dss_print_fn_t)orte_gpr_base_std_print,
                                          (orte_dss_release_fn_t)orte_gpr_base_std_release,
                                          ORTE_DSS_UNSTRUCTURED,
                                          "ORTE_GPR_ADDR_MODE", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    tmp = ORTE_GPR_KEYVAL;
    if (ORTE_SUCCESS != (rc = orte_dss.register_type(orte_gpr_base_pack_keyval,
                                          orte_gpr_base_unpack_keyval,
                                          (orte_dss_copy_fn_t)orte_gpr_base_copy_keyval,
                                          (orte_dss_compare_fn_t)orte_gpr_base_compare_keyval,
                                          (orte_dss_size_fn_t)orte_gpr_base_size_keyval,
                                          (orte_dss_print_fn_t)orte_gpr_base_print_keyval,
                                          (orte_dss_release_fn_t)orte_gpr_base_std_obj_release,
                                          ORTE_DSS_STRUCTURED,
                                          "ORTE_GPR_KEYVAL", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    tmp = ORTE_GPR_VALUE;
    if (ORTE_SUCCESS != (rc = orte_dss.register_type(orte_gpr_base_pack_value,
                                          orte_gpr_base_unpack_value,
                                          (orte_dss_copy_fn_t)orte_gpr_base_copy_gpr_value,
                                          (orte_dss_compare_fn_t)orte_gpr_base_compare_gpr_value,
                                          (orte_dss_size_fn_t)orte_gpr_base_size_gpr_value,
                                          (orte_dss_print_fn_t)orte_gpr_base_print_gpr_value,
                                          (orte_dss_release_fn_t)orte_gpr_base_std_obj_release,
                                          ORTE_DSS_STRUCTURED,
                                          "ORTE_GPR_VALUE", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    tmp = ORTE_GPR_SUBSCRIPTION;
    if (ORTE_SUCCESS != (rc = orte_dss.register_type(orte_gpr_base_pack_subscription,
                                          orte_gpr_base_unpack_subscription,
                                          (orte_dss_copy_fn_t)orte_gpr_base_copy_subscription,
                                          (orte_dss_compare_fn_t)orte_gpr_base_compare_subscription,
                                          (orte_dss_size_fn_t)orte_gpr_base_size_subscription,
                                          (orte_dss_print_fn_t)orte_gpr_base_print_subscription,
                                          (orte_dss_release_fn_t)orte_gpr_base_std_obj_release,
                                          ORTE_DSS_STRUCTURED,
                                          "ORTE_GPR_SUBSCRIPTION", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    tmp = ORTE_GPR_TRIGGER;
    if (ORTE_SUCCESS != (rc = orte_dss.register_type(orte_gpr_base_pack_trigger,
                                          orte_gpr_base_unpack_trigger,
                                          (orte_dss_copy_fn_t)orte_gpr_base_copy_trigger,
                                          (orte_dss_compare_fn_t)orte_gpr_base_compare_trigger,
                                          (orte_dss_size_fn_t)orte_gpr_base_size_trigger,
                                          (orte_dss_print_fn_t)orte_gpr_base_print_trigger,
                                          (orte_dss_release_fn_t)orte_gpr_base_std_obj_release,
                                          ORTE_DSS_STRUCTURED,
                                          "ORTE_GPR_TRIGGER", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    tmp = ORTE_GPR_NOTIFY_DATA;
    if (ORTE_SUCCESS != (rc = orte_dss.register_type(orte_gpr_base_pack_notify_data,
                                          orte_gpr_base_unpack_notify_data,
                                          (orte_dss_copy_fn_t)orte_gpr_base_copy_notify_data,
                                          (orte_dss_compare_fn_t)orte_gpr_base_compare_notify_data,
                                          (orte_dss_size_fn_t)orte_gpr_base_size_notify_data,
                                          (orte_dss_print_fn_t)orte_gpr_base_print_notify_data,
                                          (orte_dss_release_fn_t)orte_gpr_base_std_obj_release,
                                          ORTE_DSS_STRUCTURED,
                                          "ORTE_GPR_NOTIFY_DATA", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    tmp = ORTE_GPR_NOTIFY_MSG;
    if (ORTE_SUCCESS != (rc = orte_dss.register_type(orte_gpr_base_pack_notify_msg,
                                          orte_gpr_base_unpack_notify_msg,
                                          (orte_dss_copy_fn_t)orte_gpr_base_copy_notify_msg,
                                          (orte_dss_compare_fn_t)orte_gpr_base_compare_notify_msg,
                                          (orte_dss_size_fn_t)orte_gpr_base_size_notify_msg,
                                          (orte_dss_print_fn_t)orte_gpr_base_print_notify_msg,
                                          (orte_dss_release_fn_t)orte_gpr_base_std_obj_release,
                                          ORTE_DSS_STRUCTURED,
                                          "ORTE_GPR_NOTIFY_MSG", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* Open up all available components */
    if (ORTE_SUCCESS !=
        mca_base_components_open("gpr",
                                 orte_gpr_base_output,
                                 mca_gpr_base_static_components,
                                 &orte_gpr_base_components_available, true)) {
        return ORTE_ERROR;
    }

    /* All done */

    return ORTE_SUCCESS;
}
