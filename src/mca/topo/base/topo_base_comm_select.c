/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "class/ompi_list.h"
#include "runtime/runtime.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/topo/topo.h"
#include "mca/topo/base/base.h"
#include "util/output.h"
#include "communicator/communicator.h"


static void fill_null_pointers(mca_topo_t *actions);
/*
 * This structure is needed so that we can close the modules 
 * which are not selected but were opened. mca_base_modules_close
 * which does this job for us requires a ompi_list_t which contains
 * these modules
 */
struct queried_module_t {
    ompi_list_item_t super;
    mca_topo_base_module_t *om_module;
    mca_topo_t *om_actions;
};
typedef struct queried_module_t queried_module_t;

OBJ_CLASS_INSTANCE(queried_module_t, ompi_list_item_t, NULL, NULL);

/*
 * Only one topo module can be attached to each communicator.
 *
 * This module calls the query funtion on all the modules 
 * that were detected by topo_base_open. This function is 
 * called on a per-communicator basis. This function has the 
 * following function. 
 *
 * 1. Iterate over the list of available_modules
 * 2. Call the query function on each of these modules.
 * 3. query function returns the structure containing pointers
 * to its functions and the priority of this module. 
 * 4. Select the module with the highest priority
 * 5. Call the init function on its actions so that it does the
 *    right setup for the communicator
 * 6. Call finalize on all the other modules which returned 
 * their actions but were unfortunate to not get selected
 */  

int mca_topo_base_comm_select (struct ompi_communicator_t *comm,
                          struct mca_base_module_t *preferred) {
    int priority; 
    int best_priority; 
    char name[MPI_MAX_OBJECT_NAME+32];
    ompi_list_item_t *item; 
    mca_base_component_priority_list_item_t *cpli;
    mca_topo_base_module_t *module; 
    mca_topo_base_module_t *preferred_module = NULL; 
    mca_topo_base_module_t *best_module;
    mca_topo_t *actions; 
    ompi_list_t queried;
    queried_module_t *om;
    char *str;
    int err;

  /* Announce */

    /* ANJU:
     * check for names array .... mca_base_param_ */

  
    snprintf(name, sizeof(name), "%s (cid %d)", comm->c_name,
               comm->c_contextid);
    name[sizeof(name) - 1] = '\0';
    ompi_output_verbose(10, mca_topo_base_output,
                        "topo:base:comm_select: new communicator: %s",
                        name);

  /* Check and see if a preferred module was provided. If it was provided
      then it should be used (if possible) */
    if (NULL != preferred) {
         
        /* We have a preferred module. Check if it is available
           and if so, whether it wants to run */
         
         str = &(preferred->mca_module_name[0]);
         
         ompi_output_verbose(10, mca_topo_base_output,
                             "topo:base:comm_select: Checking preferred module: %s",
                             str);

         /* query the module for its priority and get its actions 
            structure. This is necessary to proceed */

         actions = preferred_module->topom_comm_query (&priority);
             
         if (NULL != actions && 
             NULL != actions->topo_module_init &&
             NULL != actions->topo_graph_map &&
             NULL != actions->topo_cart_map) {

             /* this query seems to have returned something legitimate and 
             * we can now go ahead and initialize the communicator with it
             * but first, the functions which are null need to be filled in */

             fill_null_pointers (actions);
             comm->c_topo = actions;
             return actions->topo_module_init(comm);
         } 
            /* His preferred module is present, but is unable to run. This is 
             * not a good sign. We should try selecting some other component
             * We let it fall through and select from the list of available
             * components
             */
     } /*end fo selection for preferred module */

    /*
     * We fall till here if one of the two things happened:
     * 1. The preferred module was provided but for some reason was not able
     *    to be selected
     * 2. No preferred module was provided
     *
     * All we need to do is to go through the list of available modules and find 
     * the one which has the highest priority and use that for this communicator
     */ 

    best_module = NULL;
    best_priority = -1;
    OBJ_CONSTRUCT(&queried, ompi_list_t);

    for (item = ompi_list_get_first(&mca_topo_base_modules_available);
         item != ompi_list_get_end(&mca_topo_base_modules_available);
         item = ompi_list_get_next(item)) {
       /*
        * convert the ompi_list_item_t returned into the proper type
        */
       cpli = (mca_base_component_priority_list_item_t *) item;
       module = (mca_topo_base_module_t *) cpli->cpli_component;

       ompi_output_verbose(10, mca_topo_base_output,
                          "select: initialising %s module %s",
                          module->topom_version.mca_type_name,
                          module->topom_version.mca_module_name);

       /*
        * we can call the query function only if there is a function :-)
        */
       if (NULL == module->topom_comm_query) {
          ompi_output_verbose(10, mca_topo_base_output,
                             "select: no query, ignoring the module");
       } else {
           /*
            * call the query function and see what it returns
            */ 
           actions = module->topom_comm_query (&priority);

           if (NULL == actions ||
               NULL == actions->topo_module_init ||
               NULL == actions->topo_graph_map  ||
               NULL == actions->topo_cart_map) {
               /*
                * query did not return any action which can be used
                */ 
               ompi_output_verbose(10, mca_topo_base_output,
                                  "select: query returned failure");
           } else {
               ompi_output_verbose(10, mca_topo_base_output,
                                  "select: query returned priority &d",
                                  priority);
               /* 
                * is this the best module we have found till now. Check if 
                * this module has cart_map and graph_map implemented. Everything
                * else can be covered using base functions.
                */
               if (priority > best_priority) {
                   best_priority = priority;
                   best_module = module;
               }

               om = OBJ_NEW(queried_module_t);
               /*
                * check if we have run out of space
                */
               if (NULL == om) {
                   OBJ_DESTRUCT(&queried);
                   return OMPI_ERR_OUT_OF_RESOURCE;
               }
               om->om_module = module;
               om->om_actions = actions; 
               ompi_list_append(&queried, (ompi_list_item_t *)om); 
           } /* end else of if (NULL == actions) */
       } /* end else of if (NULL == module->topom_init) */
    } /* end for ... end of traversal */

    /*
     * Now we have alist of modules which successfully returned their actions struct.
     * One of these modules has the best priority. The rest have to be comm_unqueried 
     * to counter the effects of comm_query'ing them. Finalize happens only on modules
     * which should are initialized.
     */
    if (NULL == best_module) {
       /*
        * This typically means that there was no module which was able
        * to run properly this time. So, we need to abort
        * JMS replace with show_help
        */
        OBJ_DESTRUCT(&queried);
        return OMPI_ERROR;
    }

    /*
     * We now have a list of modules which have successfully returned
     * their priorities from the query. We now have to unquery() those
     * modules which have not been selected and init() the module which
     * was selected
     */ 
    for (item = ompi_list_remove_first(&queried);
         NULL != item;
         item = ompi_list_remove_first(&queried)) {
        om = (queried_module_t *) item;
        if (om->om_module == best_module) {
           /*
            * this is the chosen module, we have to initialise
            * the actions of this module.
            *
            * ANJU: a module might not have all the functions defined.
            * Whereever a function pointer is null in the actions 
            * structure we need to fill it in with the base structure
            * function pointers. This is yet to be done 
            */ 

            /*
             * We don return here coz we still need to go through
             * and elease the other objects
             */

            fill_null_pointers (om->om_actions);
            comm->c_topo = om->om_actions;
            err = actions->topo_module_init(comm);

         } else {
            /*
             * this is not the "choosen one", finalize
             */
             if (NULL != om->om_module->topom_comm_unquery) {
                /* unquery the module only if they have some
                 * clean up job to do. Modules which are queried 
                 * but do not actually do anything typically do not 
                 * have a unquery. Hence this check is necessary
                 */
                 (void) om->om_module->topom_comm_unquery(comm);
                 ompi_output_verbose(10, mca_topo_base_output,
                                     "select: module %s is not selected",
                                     om->om_module->topom_version.mca_module_name);
               } /* end if */
          } /* if not best module */
          OBJ_RELEASE(om);
    } /* traversing through the entire list */
    
    ompi_output_verbose(10, mca_topo_base_output,
                       "select: module %s selected",
                        module->topom_version.mca_module_name);

    OBJ_DESTRUCT(&queried);

    return err;
}

/*
 * This function fills in the null function pointers, in other words,
 * those functions which are not implemented by the module with the 
 * pointers from the base function. Somewhere, I need to incoroporate 
 * a check for the common minimum funtions being implemented by the 
 * module atleast. If not, this module cannot be considered.
 */ 
static void fill_null_pointers(mca_topo_t *actions) {

#define CHECK_FOR_NULL_FUNCTION_POINTER(name) \
   if (NULL == actions->topo_##name) { \
      actions->topo_##name = mca_topo_base_##name; \
   }

   CHECK_FOR_NULL_FUNCTION_POINTER(cart_coords);
   CHECK_FOR_NULL_FUNCTION_POINTER(cart_create); 
   CHECK_FOR_NULL_FUNCTION_POINTER(cartdim_get);
   CHECK_FOR_NULL_FUNCTION_POINTER(cart_rank);
   CHECK_FOR_NULL_FUNCTION_POINTER(cart_shift);
   CHECK_FOR_NULL_FUNCTION_POINTER(cart_sub);
   CHECK_FOR_NULL_FUNCTION_POINTER(graph_create); 
   CHECK_FOR_NULL_FUNCTION_POINTER(graph_get);
   CHECK_FOR_NULL_FUNCTION_POINTER(graphdims_get);
   CHECK_FOR_NULL_FUNCTION_POINTER(graph_neighbors);
   CHECK_FOR_NULL_FUNCTION_POINTER(graph_neighbors_count);

#undef CHECK_FOR_NULL_FUNCTION_POINTER
}
