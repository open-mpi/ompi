/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mca/mca.h"
#include "lfc/lam_list.h"
#include "runtime/runtime.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/topo/topo.h"
#include "mca/topo/base/base.h"


/*
 * This structure is needed so that we can close the modules 
 * which are not selected but were opened. mca_base_modules_close
 * which does this job for us requires a lam_list_t which contains
 * these modules
 */
struct opened_module_t {
    lam_list_item_t super;
    mca_topo_base_module_t *om_module;
    mca_topo_t *om_actions;
};
typedef struct opened_module_t opened_module_t;

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

int mca_topo_base_select (mca_topo_t *selected, 
                          bool *allow_multi_user_thread,
                          bool *have_hidden_threads) {

    int priority; 
    int best_priority; 
    bool user_threads;
    bool hidden_threads;
    lam_list_item_t *item; 
    mca_base_module_list_item_t *mli;
    mca_topo_base_module_t *module; 
    mca_topo_base_module_t *best_module;
    mca_topo_t *actions; 
    opened_module_t *om;

    /*
     * Traverse the list of opened modules.
     */
    best_module = NULL;
    best_priority = -1;
    OBJ_CONSTRUCT(&opened, lam_list_t);

    for (item = lam_list_get_first(&mca_topo_base_modules_available);
         item != lam_list_get_end(&mca_topo_base_modules_available);
         item = lam_list_get_next(item)) {
       /*
        * convert the lam_list_item_t returned into the proper type
        */
       mli = (mca_base_module_list_item_t *) item;
       module = (mca_topo_base_module_t *) mli->mli_module;

       lam_output_verbose(10, mca_topo_base_output,
                          "select: initialising %s module %s",
                          module->topom_version.mca_type_name,
                          module->topom_version.mca_module_name);

       /*
        * we can call the query function only if there is a function :-)
        */
       if (NULL == module->topom_query) {
          lam_output_verbose(10, mca_topo_base_output,
                             "select: no query, ignoring the module");
       } else {
           /*
            * call the query function and see what it returns
            */ 
           actions = module->topom_query (&priority,
                                          &user_threads,
                                          &hidden_threads);
           if (NULL == actions) {
               /*
                * query did not return any action which can be used
                */ 
               lam_output_verbose(10, mca_topo_base_module,
                                  "select: query returned failure");
           } else {
               lam_output_verbose(10, mca_topo_base_module,
                                  "select: query returned priority &d",
                                  priority);
               /* 
                * is this the best module we have found till now
                */
               if (priority > best_priority) {
                   best_priority = priority;
                   best_user_threads = user_threads;
                   best_hidden_threads = hidden_threads;
                   best_module = module;
               }

               om = (opened_module_t *) malloc(sizeof(opened_module_t)); 
               /*
                * check if we have run out of space
                */
               if (NULL == om) {
                   return LAM_ERR_OUT_OF_RESOURCE;
               }
               OBJ_CONSTRUCT(om, lam_list_item_t);
               om->om_module = module;
               om->om_actions = actions; 
               lam_list_append(&opened, (lam_list_item_t *)om); 
           } /* end else of if (NULL == actions) */
       } /* end else of if (NULL == module->topom_init) */
    } /* end for ... end of traversal */

    /*
     * Now we have alist of modules which successfully initialised.
     * This list is contained in opened. Also, we have the highest
     * priority returned which is of the module "best_module". We now
     * have to close all those modules which "lost" the race to 
     * get chosen
     */
    if (NULL == best_module) {
       /*
        * This typically means that there was no module which was able
        * to run properly this time. So, we need to abort
        * JMS replace with show_help
        */
       lam_abort(1, "No topo module avaliable. This should not happen");
    }

    /*
     * We now have a list of modules which have successfully returned
     * their priorities from the query. We now have to finalize() those
     * modules which have not been selected and init() the module which
     * was selected
     */ 
    for (item = lam_list_remove_first(&opened);
         NULL != item;
         item = lam_list_remove_first(&opened)) {
            om = (opened_module_t *) item;
            if (om->om_module == best_module) {
                /*
                 * this is the chosen module, we have to initialise
                 * the actions of this module. Also, save the actions
                 * in selected since we are going to deallocate om
                 * at the end of the loop. For now I am ignoring the init
                 * function call of the module since I dont know what
                 * it is supposed to do as of now. Someday this will have
                 * arguments.
                 *
                 * ANJU: a module might not have all the functions defined.
                 * Whereever a function pointer is null in the actions 
                 * structure we need to fill it in with the base structure
                 * function pointers. This is yet to be done 
                 */ 
                if (NULL != om->om_actions->init) { 
                    /*
                     * commenting this out for now since I m
                     * not sure of the calling conventions
                    (void)om->om_actions->init();
                    */
                }
                mca_topo_base_selected_module = *best_module;
                mca_topo = *(om->om_actions);
                *selected = *(om->om_actions);
                *allow_multi_user_threads = best_user_threads;
                *have_hidden_threads = best_hidden_threads;
            } else {
              /*
               * this is not the "choosen one", finalize
               */
               if (NULL != om->om_module->topom_finalize) {
                  /* finalise the module only if they have some
                   * clean up job to do. Modules which are opened
                   * but do not actually do anything typically do not 
                   * have a finalize. Hence this check is necessary
                   */
                   om->om_module->topom_finalize();
                   lam_output_verbose(10, mca_topo_base_output,
                                      "select: module %s is not selected",
                                      om->om_module->topom_version.mca_module_name);
               } /* end if */
            } /* if not best module */
            free(om);
    } /* traversing through the entire list */

    /*
     * save the selected module
     */
     lam_output_verbose(10, mca_topo_base_output,
                       "select: module %s selected",
                        module->topom_version.mca_module_name);

   /*
    * I think we are done :-)
    */
    return LAM_SUCCESS;
}
