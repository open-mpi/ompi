/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

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

/* constructor - used to initialize state of registry value instance */
static void ompi_registry_value_construct(ompi_registry_value_t* reg_val)
{
    reg_val->object = NULL;
    reg_val->object_size = 0;
}

/* destructor - used to free any resources held by instance */
static void ompi_registry_value_destructor(ompi_registry_value_t* reg_val)
{
    if (NULL != reg_val->object) {
	free(reg_val->object);
    }
}

/* define instance of ompi_class_t */
OBJ_CLASS_INSTANCE(
		   ompi_registry_value_t,  /* type name */
		   ompi_list_item_t, /* parent "class" name */
		   ompi_registry_value_construct, /* constructor */
		   ompi_registry_value_destructor); /* destructor */


/* constructor - used to initialize state of compound_cmd_value instance */
static void ompi_registry_compound_cmd_results_construct(ompi_registry_compound_cmd_results_t* results)
{
    OBJ_CONSTRUCT(&results->data, ompi_list_t);
}

/* destructor - used to free any resources held by instance */
static void ompi_registry_compound_cmd_results_destructor(ompi_registry_compound_cmd_results_t* results)
{
    OBJ_DESTRUCT(&results->data);
}

/* define instance of ompi_class_t */
OBJ_CLASS_INSTANCE(
		   ompi_registry_compound_cmd_results_t,     /* type name */
		   ompi_list_item_t,                /* parent "class" name */
		   ompi_registry_compound_cmd_results_construct,   /* constructor */
		   ompi_registry_compound_cmd_results_destructor); /* destructor */


/* constructor - used to initialize state of index_value instance */
static void ompi_registry_index_value_construct(ompi_registry_index_value_t* value)
{
    value->token = NULL;
}

/* destructor - used to free any resources held by instance */
static void ompi_registry_index_value_destructor(ompi_registry_index_value_t* value)
{
    if (value->token) {
	free(value->token);
    }
}

/* define instance of ompi_class_t */
OBJ_CLASS_INSTANCE(
		   ompi_registry_index_value_t,     /* type name */
		   ompi_list_item_t,                /* parent "class" name */
		   ompi_registry_index_value_construct,   /* constructor */
		   ompi_registry_index_value_destructor); /* destructor */


/* constructor - used to initialize state of test results instance */
static void ompi_registry_internal_test_results_construct(ompi_registry_internal_test_results_t* results)
{
    results->test = NULL;
    results->message = NULL;
}

/* destructor - used to free any resources held by instance */
static void ompi_registry_internal_test_results_destructor(ompi_registry_internal_test_results_t* results)
{
    if (NULL != results->test) {
	free(results->test);
    }
    if (NULL != results->message) {
	free(results->message);
    }
}

/* define instance of ompi_class_t */
OBJ_CLASS_INSTANCE(
		   ompi_registry_internal_test_results_t,            /* type name */
		   ompi_list_item_t,                                 /* parent "class" name */
		   ompi_registry_internal_test_results_construct,    /* constructor */
		   ompi_registry_internal_test_results_destructor);  /* destructor */


/* constructor - used to initialize notify idtag list instance */
static void mca_gpr_idtag_list_construct(mca_gpr_idtag_list_t* req)
{
    req->id_tag = OMPI_REGISTRY_NOTIFY_ID_MAX;
}

/* destructor - used to free any resources held by instance */
static void mca_gpr_idtag_list_destructor(mca_gpr_idtag_list_t* req)
{
}

/* define instance of ompi_class_t */
OBJ_CLASS_INSTANCE(
		   mca_gpr_idtag_list_t,    /* type name */
		   ompi_list_item_t,                /* parent "class" name */
		   mca_gpr_idtag_list_construct,    /* constructor */
		   mca_gpr_idtag_list_destructor);  /* destructor */


/* constructor - used to initialize notify message instance */
static void ompi_registry_notify_message_construct(ompi_registry_notify_message_t* msg)
{
    msg->segment = NULL;
    msg->owning_job = 0;
    OBJ_CONSTRUCT(&msg->data, ompi_list_t);
    msg->trig_action = OMPI_REGISTRY_NOTIFY_NONE;
    msg->trig_synchro = OMPI_REGISTRY_SYNCHRO_MODE_NONE;
    msg->num_tokens = 0;
    msg->tokens = NULL;
}

/* destructor - used to free any resources held by instance */
static void ompi_registry_notify_message_destructor(ompi_registry_notify_message_t* msg)
{
    uint32_t i;
    char **tokptr;
    ompi_registry_value_t *ptr;

    if (NULL != msg->segment) {
	free(msg->segment);
    }

    while (NULL != (ptr = (ompi_registry_value_t*)ompi_list_remove_first(&msg->data))) {
	OBJ_RELEASE(ptr);
    }
    OBJ_DESTRUCT(&msg->data);

    for (i=0, tokptr=msg->tokens; i < msg->num_tokens; i++, tokptr++) {
	free(*tokptr);
    }

    if (NULL != msg->tokens) {
	free(msg->tokens);
    }
}

/* define instance of ompi_class_t */
OBJ_CLASS_INSTANCE(
		   ompi_registry_notify_message_t,            /* type name */
		   ompi_object_t,                             /* parent "class" name */
		   ompi_registry_notify_message_construct,    /* constructor */
		   ompi_registry_notify_message_destructor);  /* destructor */


/*
 * Global variables
 */
int mca_gpr_base_output = -1;
mca_gpr_base_module_t ompi_registry;
bool mca_gpr_base_selected = false;
ompi_list_t mca_gpr_base_components_available;
mca_gpr_base_component_t mca_gpr_base_selected_component;
ompi_mutex_t mca_gpr_mutex;


/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int mca_gpr_base_open(void)
{

  /* Open up all available components */

  if (OMPI_SUCCESS != 
      mca_base_components_open("gpr", 0, mca_gpr_base_static_components, 
                            &mca_gpr_base_components_available)) {
    return OMPI_ERROR;
  }

  /* setup output for debug messages */
  if (!ompi_output_init) {  /* can't open output */
      return OMPI_ERROR;
  }

  mca_gpr_base_output = ompi_output_open(NULL);

  /* All done */

  return OMPI_SUCCESS;
}
