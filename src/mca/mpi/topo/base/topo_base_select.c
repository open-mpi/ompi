/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mca/mca.h"
#include "lam/lfc/lam_list.h"
#include "lam/runtime/runtime.h"
#include "mca/mca.h"
#include "mca/lam/base/base.h"
#include "mca/mpi/topo/topo.h"
#include "mca/mpi/topo/base/base.h"


/*
 * This structure is needed so that we can close the modules 
 * which are not selected but were opened. mca_base_modules_close
 * which does this job for us requires a lam_list_t which contains
 * these modules
 */
struct opened_module_t {
    lam_list_item_t super;
    mca_topo_base_module_t *om_module;
};
typedef struct opened_module_t opened_module_t;

/*
 * Only one topo module can be attached to each communicator.
 *
 * This function calls the init function on all the modules which
 * were opened by mca_base_modules_open. init function for each 
 * module returns the priority for each module and hence this
 * function is able to select on particular module from a plethora
 * of modules available to implement topology. The selected module 
 * will have all its function pointers saved and returned to the
 * caller
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

       if (NULL == module->topom_int) {
          /*
           * there is no initialisation function in this module
           */
          lam_output_verbose(10, mca_topo_base_output,
                             "select: no init, ignoring the module");
       } else {
           actions = module->topom_init (&priority,
                                         &user_threads,
                                         &hidden_threads);
           if (NULL == actions) {
               lam_output_verbose(10, mca_topo_base_module,
                                  "select: init returned failure");
           } else {
               lam_output_verbose(10, mca_topo_base_module,
                                  "select: init returned priority &d",
                                  priority);
               if (priority > best_priority) {
                   /* 
                    * this module is the best we have found till now
                    */
                   best_priority = priority;
                   best_user_threads = user_threads;
                   best_hidden_threads = hidden_threads;
                   best_module = module;
               }

               om = (opened_module_t *) malloc(sizeof(opened_module_t)); 
               if (NULL == om) {
                   /*
                    * out of space I guess
                    */
                   return LAM_ERR_OUT_OF_RESOURCE;
               }
               OBJ_CONSTRUCT(om, lam_list_item_t);
               om->om_module = module;
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
        *
        * JMS replace with show_help
        */
       lam_abort(1, "No topo module avaliable. This should not happen");
    }

    for (item = lam_list_remove_first(&opened);
         NULL != item;
         item = lam_list_remove_first(&opened)) {
            om = (opened_module_t *) item;
            if (om->om_module != best_module) {
              /*
               * this is not the "choosen one", discard this   
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
     * Now we have to unload all the modules which were not selected 
     * through mca_base_modules_close. After this function call, the
     * mca_topo_base_available_modules will contain only the selected
     * module
     */
    mca_base_modules_close(mca_topo_base_output,
                           &mca_topo_base_modules_available,
                           (mca_base_module_t *) best_module);

    /*
     * save the selected module
     */
   mca_topo_base_selected_module = *best_module;
   mca_topo = *actions;
   *selected = *actions;
   *allow_multi_user_threads = best_user_threads;
   *have_hidden_threads = best_hidden_threads;
   lam_output_verbose(10, mca_topo_base_output,
                      "select: module %s selected",
                      module->topom_version.mca_module_name);

   /*
    * I think we are done :-)
    */
    return LAM_SUCCESS;
}
