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

#include <stdio.h>
#include <string.h>

#include "orte/runtime/runtime.h"
#include "opal/util/output.h"
#include "orte/util/proc_info.h"
#include "opal/util/argv.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ns/ns.h"
#include "orte/mca/oob/oob.h"
#include "orte/mca/oob/base/base.h"


OBJ_CLASS_INSTANCE(
    mca_oob_t,
    opal_list_item_t,
    NULL,
    NULL
);

OBJ_CLASS_INSTANCE(
    mca_oob_base_info_t,
    opal_list_item_t,
    NULL,
    NULL
);

/**
 * Parse contact info string into process name and list of uri strings.
 */

int mca_oob_parse_contact_info(
    const char* contact_info,
    orte_process_name_t* name,
    char*** uri)
{
    orte_process_name_t* proc_name;
    int rc;

    /* parse the process name */
    char* cinfo = strdup(contact_info);
    char* ptr = strchr(cinfo, ';');
    if(NULL == ptr) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        free(cinfo);
        return ORTE_ERR_BAD_PARAM;
    }
    *ptr = '\0';
    ptr++;
    if (ORTE_SUCCESS != (rc = orte_ns.convert_string_to_process_name(&proc_name, cinfo))) {
        ORTE_ERROR_LOG(rc);
        free(cinfo);
        return rc;
    }
    *name = *proc_name;
    free(proc_name);

    if (NULL != uri) {
	/* parse the remainder of the string into an array of uris */
	*uri = opal_argv_split(ptr, ';');
    }
    free(cinfo);
    return ORTE_SUCCESS;
}


/**
 * Function for selecting one module from all those that are
 * available.
 *
 * Call the init function on all available modules.
 */
int mca_oob_base_init(void)
{
    opal_list_item_t *item;
    mca_base_component_list_item_t *cli;
    mca_oob_base_component_t *component;
    mca_oob_t *module;
    mca_oob_t *s_module = NULL;
    int  s_priority = -1;

    char** include = opal_argv_split(mca_oob_base_include, ',');
    char** exclude = opal_argv_split(mca_oob_base_exclude, ',');

    /* Traverse the list of available modules; call their init functions. */
    for (item = opal_list_get_first(&mca_oob_base_components);
        item != opal_list_get_end(&mca_oob_base_components);
        item = opal_list_get_next(item)) {
        mca_oob_base_info_t *inited;

        cli = (mca_base_component_list_item_t *) item;
        component = (mca_oob_base_component_t *) cli->cli_component;

        /* if there is an include list - item must be in the list to be included */
        if ( NULL != include ) {
            char** argv = include;
            bool found = false;
            while(argv && *argv) {
                if(strcmp(component->oob_base.mca_component_name,*argv) == 0) {
                    found = true;
                    break;
                }
                argv++;
            }
            if(found == false) {
                continue;
            }
                                                                                                                     
        /* otherwise - check the exclude list to see if this item has been specifically excluded */
        } else if ( NULL != exclude ) {
            char** argv = exclude;
            bool found = false;
            while(argv && *argv) {
                if(strcmp(component->oob_base.mca_component_name,*argv) == 0) {
                    found = true;
                    break;
                }
                argv++;
            }
            if(found == true) {
                continue;
            }
        }
                                                                                                                     

        if (NULL == component->oob_init) {
            opal_output_verbose(10, mca_oob_base_output, "mca_oob_base_init: no init function; ignoring component");
        } else {
            int priority = -1;
            module = component->oob_init(&priority);
            if (NULL != module) {
                inited = OBJ_NEW(mca_oob_base_info_t);
                inited->oob_component = component;
                inited->oob_module = module;
                opal_list_append(&mca_oob_base_modules, &inited->super);

                /* setup highest priority oob channel */
                if(priority > s_priority) {
                    s_priority = priority;
                    s_module = module;
                }
            }
        }
    }
    /* set the global variable to point to the first initialize module */
    if(s_module == NULL) {
        opal_output_verbose(10, mca_oob_base_output, "mca_oob_base_init: no OOB modules available\n");
      return ORTE_ERROR;
   }

   mca_oob = *s_module;
   return ORTE_SUCCESS;
}


                                                                                  
/**
*  Obtains the contact info (oob implementation specific) URI strings through
*  which this process can be contacted on an OOB channel.
*
*  @return  A null terminated string.
*
*  The caller is responsible for freeing the returned string.
*/
                                                                                                             
char* mca_oob_get_contact_info()
{
    char *proc_name=NULL;
    char *proc_addr = mca_oob.oob_get_addr();
    char *contact_info=NULL;
    int rc;
    
    if (ORTE_SUCCESS != (rc = orte_ns.get_proc_name_string(&proc_name,
                                            orte_process_info.my_name))) {
        ORTE_ERROR_LOG(rc);
        return NULL;
    }
    if (0 > asprintf(&contact_info, "%s;%s", proc_name, proc_addr)) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
    }
    free(proc_name);
    free(proc_addr);
    return contact_info;
}


/**
*  Setup the contact information for the seed daemon - which
*  is passed as an MCA parameter. 
*
*  @param  seed  
*/
                                                                                                             
int mca_oob_set_contact_info(const char* contact_info)
{
    orte_process_name_t name;
    char** uri;
    char** ptr;
    int rc = mca_oob_parse_contact_info(contact_info, &name, &uri);
    if(rc != ORTE_SUCCESS)
        return rc;

    for(ptr = uri; ptr != NULL && *ptr != NULL; ptr++) {
        opal_list_item_t* item;
        for (item =  opal_list_get_first(&mca_oob_base_modules);
             item != opal_list_get_end(&mca_oob_base_modules);
             item =  opal_list_get_next(item)) {
            mca_oob_base_info_t* base = (mca_oob_base_info_t *) item;
            if (strncmp(base->oob_component->oob_base.mca_component_name, *ptr,
                strlen(base->oob_component->oob_base.mca_component_name)) == 0)
                base->oob_module->oob_set_addr(&name, *ptr);
        }
    }

    if(uri != NULL) {
        opal_argv_free(uri);
    }
    return ORTE_SUCCESS;
}

/**
* Called to request the selected oob components to
* register their address with the seed deamon.
*/

int mca_oob_base_module_init(void)
{
  opal_list_item_t* item;

  /* Initialize all modules after oob/gpr/ns have initialized */
  for (item =  opal_list_get_first(&mca_oob_base_modules);
       item != opal_list_get_end(&mca_oob_base_modules);
       item =  opal_list_get_next(item)) {
    mca_oob_base_info_t* base = (mca_oob_base_info_t *) item;
    if (NULL != base->oob_module->oob_init)
        base->oob_module->oob_init();
  }
  return ORTE_SUCCESS;
}

