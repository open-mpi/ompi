/*
 * $HEADER$
 *
 */

/** @file
 * MCA coll base framework public interface functions.
 *
 * These functions are normally invoked by the back-ends of:
 *
 * - The back-ends of MPI_Init() and MPI_Finalize()
 * - Communuicactor constructors (e.g., MPI_Comm_split()) and
 *   destructors (e.g., MPI_Comm_free())
 * - The laminfo command
 */

#ifndef MCA_COLL_BASE_H
#define MCA_COLL_BASE_H

#include "ompi_config.h"

#include "mpi.h"
#include "class/ompi_list.h"
#include "mca/coll/coll.h"


/*
 * Global functions for MCA overall collective open and close
 */

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
  /**
   * Initialize the coll MCA framework
   *
   * @retval OMPI_SUCCESS Upon success
   * @retval OMPI_ERROR Upon failure
   *
   * This must be the first function invoked in the coll MCA
   * framework.  It initializes the coll MCA framework, finds and
   * opens coll modules, etc.
   *
   * This function is invoked during ompi_mpi_init() and during the
   * initialization of the special case of the laminfo command.
   * 
   * This function fills in the internal global variable
   * mca_coll_base_modules_opened, which is a list of all coll modules
   * that were successfully opened.  This variable should \em only be
   * used by other coll base functions -- it is not considered a
   * public interface member -- and is only mentioned here for
   * completeness.
   */
  int mca_coll_base_open(void);

  /**
   * Create list of available coll modules.
   *
   * @param allow_multi_user_threads Will be set to true if any of the
   * available modules will allow multiple user threads
   * @param have_hidden_threads Will be set to true if any of the
   * available modules have hidden threads.
   *
   * @retval OMPI_SUCCESS If one or more coll modules are available.
   * @retval OMPI_ERROR If no coll modules are found to be available.
   *
   * This function is invoked during ompi_mpi_init() to query all
   * successfully opened coll modules and create a list of all
   * available coll modules.
   *
   * This function traverses the (internal global variable)
   * mca_coll_base_modules_opened list and queries each module to see
   * if it ever might want to run during this MPI process.  It creates
   * another internal global variable list named
   * mca_coll_base_modules_available, consisting of a list of modules
   * that are available for selection when communicators are created.
   * This variable should \em only be used by other coll base
   * functions -- it is not considered a public interface member --
   * and is only mentioned here for completeness.
   */
  int mca_coll_base_find_available(bool *allow_multi_user_threads, 
                                   bool *have_hidden_threads);

  /**
   * Select an available module for a new communicator.
   *
   * @param comm Communicator that the module will be selected for.
   * @param preferred The module that is preferred for this
   * communicator (or NULL).
   *
   * @return OMPI_SUCCESS Upon success.
   * @return OMPI_ERROR Upon failure.
   *
   * Note that the types of the parameters have "struct" in them
   * (e.g., ompi_communicator_t" vs. a plain "ompi_communicator_t") to
   * avoid an include file loop.  All similar types (e.g., "struct
   * ompi_communicator_t *", "ompi_communicator_t *", and "MPI_Comm")
   * are all typedef'ed to be the same, so the fact that we use struct
   * here in the prototype is ok.
   *
   * This function is invoked when a new communicator is created and a
   * coll module needs to be selected for it.  It should be invoked
   * near the end of the communicator creation process such that
   * almost everything else is functional on the communicator (e.g.,
   * point-to-point communication).  
   *
   * This function invokes the selection process for coll modules,
   * which works as follows:
   *
   * - If the \em preferred argument is NULL, the selection set is
   *   defined to be all the modules found during
   *   mca_coll_base_find_available().  
   * - If \em preferred is not NULL, then the selection set is just
   *   that module.  (However, in this mode, we may make 2 passes
   *   through the selection process -- more on this below).
   * - All modules in the selection set are queried to see if they
   *   want to run with that communicator.  All modules that want to
   *   run are ranked by their priority and the highest priority
   *   module is selected.  All non-selected modules have their
   *   "unquery" function invoked to let them know that they were not
   *   selected.
   * - The selected module will have its "init" function invoked to
   *   let it know that it was selected.
   * - If we fall through this entire process and no module is
   *   selected \em and the \em preferred argument is not NULL, then
   *   run the entire process again as if the \em preferred argument
   *   was NULL (i.e., use the entire available set of modules).
   *
   * At the end of this process, we'll either have a single module
   * that is selected and initialized for the communicator, or no
   * module was selected and an error is returned up the stack.
   *
   * Note that new communicators may be created as a result of
   * invoking this function.  Specifically: this function is called in
   * the depths of communicator creation, but during the execution of
   * this function, new communicators may be created, and therefore
   * communicator creation functions may be re-entered (albiet with
   * different arguments).
   */
  int mca_coll_base_comm_select(struct ompi_communicator_t *comm,
                                struct mca_base_module_t *preferred);

  /**
   * Finalize a coll module on a specific communicator.
   *
   * @param comm The communicator that is being destroyed.
   *
   * @retval OMPI_SUCCESS Always.
   *
   * Note that the type of the parameter is only a "struct
   * ompi_communicator_t" (vs. a plain "ompi_communicator_t") to avoid
   * an include file loop.  The types "struct ompi_communicator_t *",
   * "ompi_communicator_t *", and "MPI_Comm" are all typedef'ed to be
   * the same, so the fact that we use struct here in the prototype is
   * ok.
   *
   * This function is invoked near the beginning of the destruction of
   * a communicator.  It finalizes the coll module associated with the
   * communicator (e.g., allowing the module to clean up and free any
   * resources allocated for that communicator).  Note that similar to
   * mca_coll_base_select(), as result of this function, other
   * communicators may also be destroyed.
   */
  int mca_coll_base_comm_unselect(struct ompi_communicator_t *comm);

  /**
   * Finalize the coll usage on a communicator.
   *
   * @param comm The communicator that is being destroyed.
   *
   * @retval OMPI_SUCCESS Always.
   */
  int mca_coll_base_comm_finalize(struct ompi_communicator_t *comm);

  /**
   * Shut down the coll MCA framework.
   *
   * @retval OMPI_SUCCESS Always
   *
   * This function shuts down everything in the coll MCA framework,
   * and is called during ompi_mpi_finalize() and the special case of
   * the laminfo command.
   *
   * It must be the last function invoked on the coll MCA framework.
   */
  int mca_coll_base_close(void);
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif


/*
 * Globals
 */

/**
 * Index number from the "coll" MCA parameter, created when the coll
 * framework is initialized and used during scope selection.
 */
extern int mca_coll_base_param;
/**
 * Coll framework debugging stream ID used with ompi_output() and
 * ompi_output_verbose().
 */
extern int mca_coll_base_output;

/**
 * JMS should this move to the basic module?
 */
extern int mca_coll_base_crossover;
/**
 * JMS should this move to the basic module?
 */
extern int mca_coll_base_associative;
/**
 * JMS should this move to the basic module?
 */
extern int mca_coll_base_reduce_crossover;
/**
 * JMS should this move to the basic module?
 */
extern int mca_coll_base_bcast_collmaxlin;
/**
 * JMS should this move to the basic module?
 */
extern int mca_coll_base_bcast_collmaxdim;

/**
 * Indicator as to whether the list of opened coll modules is valid or
 * not.
 */
extern bool mca_coll_base_components_opened_valid;
/**
 * List of all opened modules; created when the coll framework is
 * initialized and destroyed when we reduce the list to all available
 * coll modules.
 */
extern ompi_list_t mca_coll_base_components_opened;
/**
 * Indicator as to whether the list of available coll modules is valid
 * or not.
 */
extern bool mca_coll_base_components_available_valid;
/**
 * List of all available modules; created by reducing the list of open
 * modules to all those who indicate that they may run during this
 * process.
 */
extern ompi_list_t mca_coll_base_components_available;

/**
 * Pointer to the "basic" component so that it can be found easily
 * (since the "basic" component is fairly special -- it's the lowest
 * common denominator between all coll components and may be used
 * interchangably).
 */
extern const mca_coll_base_module_1_0_0_t *mca_coll_base_basic_component;

#endif /* MCA_BASE_COLL_H */
