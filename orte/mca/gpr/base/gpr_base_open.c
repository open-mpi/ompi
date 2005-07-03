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

#include "orte_config.h"
#include "include/orte_constants.h"
#include "include/constants.h"

#include "util/output.h"

#include "dps/dps.h"
#include "mca/errmgr/errmgr.h"

#include "mca/gpr/base/base.h"



/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_module_t struct.
 */

#include "mca/gpr/base/static-components.h"

/*
 * globals
 */

/** KEYVAL **/
/* constructor - used to initialize state of keyval instance */
static void orte_gpr_keyval_construct(orte_gpr_keyval_t* keyval)
{
    keyval->key = NULL;
    keyval->type = ORTE_NULL;
    keyval->value.i32 = 0;
}

/* destructor - used to free any resources held by instance */
static void orte_gpr_keyval_destructor(orte_gpr_keyval_t* keyval)
{
    orte_byte_object_t *byteptr;
    
    if (NULL != keyval->key) {
        free(keyval->key);
    }
    if (ORTE_BYTE_OBJECT == keyval->type) {
        byteptr = &(keyval->value.byteobject);
        if (NULL != byteptr->bytes) {
            free(byteptr->bytes);
        }
    } else if (ORTE_STRING == keyval->type) {
        if (NULL != keyval->value.strptr)
            free(keyval->value.strptr);
    } else if (ORTE_APP_CONTEXT == keyval->type) {
        if (NULL != keyval->value.app_context)
            OBJ_RELEASE(keyval->value.app_context);
    }
}

/* define instance of opal_class_t */
OBJ_CLASS_INSTANCE(
    orte_gpr_keyval_t,              /* type name */
    opal_object_t,                  /* parent "class" name */
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
    size_t i;

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
    ptr->id = ORTE_GPR_SUBSCRIPTION_ID_MAX;
    ptr->cnt = 0;
    ptr->values = NULL;
}

/* destructor - used to free any resources held by instance */
static void orte_gpr_notify_data_destructor(orte_gpr_notify_data_t* ptr)
{
    size_t i;

    if (0 < ptr->cnt && NULL != ptr->values) {
        for (i=0; i < ptr->cnt; i++) {
            if (NULL != ptr->values[i])
                OBJ_RELEASE(ptr->values[i]);
        }
       free(ptr->values);
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
    size_t i;

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
}

/* destructor - used to free any resources held by instance */
static void orte_gpr_trigger_destructor(orte_gpr_trigger_t* trig)
{
    size_t i;

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
    msg->cnt = 0;
    msg->data = NULL;
}

/* destructor - used to free any resources held by instance */
static void orte_gpr_notify_message_destructor(orte_gpr_notify_message_t* msg)
{
    size_t i;
    
    if (0 < msg->cnt && NULL != msg->data) {
        for (i=0; i < msg->cnt; i++) {
            if (NULL != msg->data[i]) OBJ_RELEASE(msg->data[i]);
        }
        free(msg->data);
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
orte_gpr_base_module_t orte_gpr;
bool orte_gpr_base_selected = false;
ompi_list_t orte_gpr_base_components_available;
mca_gpr_base_component_t orte_gpr_base_selected_component;
ompi_mutex_t orte_gpr_mutex;


/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int orte_gpr_base_open(void)
{
    int param, value, rc;
    orte_data_type_t tmp;

    /* Debugging / verbose output */
    
    param = mca_base_param_register_int("gpr", "base", "verbose",
                                        NULL, 0);
    mca_base_param_lookup_int(param, &value);
    if (value != 0) {
        orte_gpr_base_output = ompi_output_open(NULL);
    } else {
        orte_gpr_base_output = -1;
    }

    /* register the base data types with the DPS */
    tmp = ORTE_GPR_CMD;
    if (ORTE_SUCCESS != (rc = orte_dps.register_type(orte_gpr_base_pack_cmd,
                                          orte_gpr_base_unpack_cmd,
                                          "ORTE_GPR_CMD", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    tmp = ORTE_GPR_SUBSCRIPTION_ID;
    if (ORTE_SUCCESS != (rc = orte_dps.register_type(orte_gpr_base_pack_subscription_id,
                                          orte_gpr_base_unpack_subscription_id,
                                          "ORTE_GPR_SUBSCRIPTION_ID", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    tmp = ORTE_GPR_TRIGGER_ID;
    if (ORTE_SUCCESS != (rc = orte_dps.register_type(orte_gpr_base_pack_trigger_id,
                                          orte_gpr_base_unpack_trigger_id,
                                          "ORTE_GPR_TRIGGER_ID", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    tmp = ORTE_GPR_NOTIFY_ACTION;
    if (ORTE_SUCCESS != (rc = orte_dps.register_type(orte_gpr_base_pack_notify_action,
                                          orte_gpr_base_unpack_notify_action,
                                          "ORTE_GPR_NOTIFY_ACTION", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    tmp = ORTE_GPR_TRIGGER_ACTION;
    if (ORTE_SUCCESS != (rc = orte_dps.register_type(orte_gpr_base_pack_trigger_action,
                                          orte_gpr_base_unpack_trigger_action,
                                          "ORTE_GPR_TRIGGER_ACTION", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    tmp = ORTE_GPR_ADDR_MODE;
    if (ORTE_SUCCESS != (rc = orte_dps.register_type(orte_gpr_base_pack_addr_mode,
                                          orte_gpr_base_unpack_addr_mode,
                                          "ORTE_GPR_ADDR_MODE", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    tmp = ORTE_KEYVAL;
    if (ORTE_SUCCESS != (rc = orte_dps.register_type(orte_gpr_base_pack_keyval,
                                          orte_gpr_base_unpack_keyval,
                                          "ORTE_KEYVAL", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    tmp = ORTE_GPR_VALUE;
    if (ORTE_SUCCESS != (rc = orte_dps.register_type(orte_gpr_base_pack_value,
                                          orte_gpr_base_unpack_value,
                                          "ORTE_GPR_VALUE", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    tmp = ORTE_GPR_SUBSCRIPTION;
    if (ORTE_SUCCESS != (rc = orte_dps.register_type(orte_gpr_base_pack_subscription,
                                          orte_gpr_base_unpack_subscription,
                                          "ORTE_GPR_SUBSCRIPTION", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    tmp = ORTE_GPR_TRIGGER;
    if (ORTE_SUCCESS != (rc = orte_dps.register_type(orte_gpr_base_pack_trigger,
                                          orte_gpr_base_unpack_trigger,
                                          "ORTE_GPR_TRIGGER", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    tmp = ORTE_GPR_NOTIFY_DATA;
    if (ORTE_SUCCESS != (rc = orte_dps.register_type(orte_gpr_base_pack_notify_data,
                                          orte_gpr_base_unpack_notify_data,
                                          "ORTE_GPR_NOTIFY_DATA", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* Open up all available components */

    if (OMPI_SUCCESS != 
        mca_base_components_open("gpr", 0, mca_gpr_base_static_components, 
                                 &orte_gpr_base_components_available, true)) {
        return ORTE_ERROR;
    }

    /* All done */
    
    return ORTE_SUCCESS;
}
