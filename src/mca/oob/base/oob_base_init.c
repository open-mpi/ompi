/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "runtime/runtime.h"
#include "util/output.h"
#include "util/proc_info.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/pcmclient/pcmclient.h"
#include "mca/pcmclient/base/base.h"
#include "mca/ns/base/base.h"
#include "mca/pcm/pcm.h"
#include "mca/oob/oob.h"
#include "mca/oob/base/base.h"


OBJ_CLASS_INSTANCE(
    mca_oob_t,
    ompi_list_item_t,
    NULL,
    NULL
);

OBJ_CLASS_INSTANCE(
    mca_oob_base_info_t,
    ompi_list_item_t,
    NULL,
    NULL
);

ompi_process_name_t mca_oob_name_seed;
ompi_process_name_t mca_oob_name_self;
ompi_process_name_t mca_oob_name_any;                                                                                                                          

/**
 * Parse contact info string into process name and list of uri strings.
 */

static int mca_oob_base_parse_contact_info(
    char* contact_info,
    ompi_process_name_t* name,
    char*** uri)
{
    /* parse the process name */
    char* ptr = strchr(contact_info, ';');
    if(NULL == ptr)
        return OMPI_ERR_BAD_PARAM;
    *ptr = '\0';
    ptr++;
    *name = *ns_base_convert_string_to_process_name(contact_info);

    /* parse the remainder of the string into an array of uris */
    *uri = ompi_argv_split(ptr, ';');
    return OMPI_SUCCESS;
}


/**
 * Function for selecting one module from all those that are
 * available.
 *
 * Call the init function on all available modules.
 */
int mca_oob_base_init(bool *user_threads, bool *hidden_threads)
{
    ompi_list_item_t *item;
    mca_base_component_list_item_t *cli;
    mca_oob_base_info_t * first;
    mca_oob_base_component_t *component;
    mca_oob_t *module;
    extern ompi_list_t mca_oob_base_components;
    ompi_process_name_t *self;
    int i, id;
    char* seed;
    char** uri = NULL;

    char** include = ompi_argv_split(mca_oob_base_include, ',');
    char** exclude = ompi_argv_split(mca_oob_base_exclude, ',');

    /* setup local name */
    self = mca_pcmclient.pcmclient_get_self();
    if(NULL == self) {
        ompi_output(0, "mca_oob_base_init: could not get pcmclient self pointer");
        return OMPI_ERROR;
    }
    mca_oob_name_self = *self;

    /* setup wildcard name */
    mca_oob_name_any = *ompi_name_server.create_process_name(
        MCA_NS_BASE_CELLID_MAX,
        MCA_NS_BASE_JOBID_MAX,
        MCA_NS_BASE_VPID_MAX);

    /* setup seed daemons name and address */
    id = mca_base_param_register_string("oob","base","seed",NULL,NULL);
    mca_base_param_lookup_string(id,&seed);
    if(seed == NULL) {
        /* we are seed daemon */
        mca_oob_name_seed = mca_oob_name_self;
    } else {
        /* resolve name of seed daemon */
        mca_oob_base_parse_contact_info(seed,&mca_oob_name_seed, &uri);
        if(NULL == uri || NULL == *uri) {
            ompi_output(0, "mca_oob_base_init: unable to parse seed contact info.");
            return OMPI_ERROR; 
        }
    }

    /* Traverse the list of available modules; call their init functions. */
    for (item = ompi_list_get_first(&mca_oob_base_components);
        item != ompi_list_get_end(&mca_oob_base_components);
        item = ompi_list_get_next(item)) {
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
            ompi_output_verbose(10, mca_oob_base_output, "mca_oob_base_init: no init function; ignoring component");
        } else {
            module = component->oob_init(user_threads, hidden_threads);
            if (NULL == module) {
                ompi_output_verbose(10, mca_oob_base_output, "mca_oob_base_init: oob_init returned failure");
            } else {
              inited = OBJ_NEW(mca_oob_base_info_t);
              inited->oob_component = component;
              inited->oob_module = module;
              ompi_list_append(&mca_oob_base_modules, &inited->super);

              /* lookup contact info for this component */
              if(NULL != uri) {
                  for(i=0; NULL != uri[i]; i++) {
                     if(strncmp(uri[i], component->oob_base.mca_component_name, 
                        strlen(component->oob_base.mca_component_name)) == 0) { 
                        module->oob_set_seed(uri[i]);
                     }
                  }
               }
            }
        }
    }
    if(uri != NULL) {
        ompi_argv_free(uri);
    }

    /* set the global variable to point to the first initialize module */
    if (0 < ompi_list_get_size(&mca_oob_base_modules)) {
      first = (mca_oob_base_info_t *) ompi_list_get_first(&mca_oob_base_modules);
      mca_oob = *first->oob_module; 
      return OMPI_SUCCESS;
    } else {
      printf("No OOB modules available!\n");
      fflush(stdout);
      return OMPI_ERROR;
    }
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
    char *proc_name = ns_base_get_proc_name_string(MCA_OOB_NAME_SELF);
    char *proc_addr = mca_oob.oob_get_addr();
    size_t size = strlen(proc_name) + 1 + strlen(proc_addr) + 1;
    char *contact_info = malloc(size);
    sprintf(contact_info, "%s;%s", proc_name, proc_addr);
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
                                                                                                             
int mca_oob_set_contact_info(const char* seed)
{
    /* TSW - fix this - currently just stuff the parameter in the environment */
    setenv("OMPI_MCA_oob_base_seed", seed, 1);
    return OMPI_SUCCESS;
}
                                                                                  
/**
*  Obtains the contact info (oob implementation specific) URI strings through
*  which this process can be contacted on an OOB channel.
*
*  @return  A null terminated string.
*
*  The caller is responsible for freeing the returned string.
*/
                                                                                                             
/**
* Called to request the selected oob components to
* register their address with the seed deamon.
*/

int mca_oob_base_register(void)
{
  ompi_list_item_t* item;

  /* Initialize all modules after oob/gpr/ns have initialized */
  for (item =  ompi_list_get_first(&mca_oob_base_modules);
       item != ompi_list_get_end(&mca_oob_base_modules);
       item =  ompi_list_get_next(item)) {
    mca_oob_base_info_t* base = (mca_oob_base_info_t *) item;
    if (NULL != base->oob_module->oob_init)
        base->oob_module->oob_init();
  }
  return OMPI_SUCCESS;
}
                                                                                                                                           

