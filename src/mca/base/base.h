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

#ifndef MCA_BASE_H
#define MCA_BASE_H

#include "ompi_config.h"

#include "libltdl/ltdl.h"

#include "class/ompi_object.h"

/*
 * These units are large enough to warrant their own .h files
 */
#include "mca/base/mca_base_param.h"
#include "mca/base/mca_base_module_exchange.h"
#include "mca/base/mca_base_msgbuf.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/*
 * Structure for making plain lists of components
 */
struct mca_base_component_list_item_t {
  ompi_list_item_t super;
  const mca_base_component_t *cli_component;
};
typedef struct mca_base_component_list_item_t mca_base_component_list_item_t;
OBJ_CLASS_DECLARATION(mca_base_component_list_item_t);


/*
 * Structure for making priority lists of components
 */
struct mca_base_component_priority_list_item_t {
  mca_base_component_list_item_t super;

  int cpli_priority;
  bool cpli_allow_multi_user_threads;
  bool cpli_have_hidden_threads;
};
typedef struct mca_base_component_priority_list_item_t 
  mca_base_component_priority_list_item_t;
OBJ_CLASS_DECLARATION(mca_base_component_priority_list_item_t);


/*
 * Public variables
 */
OMPI_DECLSPEC extern int mca_base_param_component_path;


/*
 * Public functions
 */

  /**
   * First function called in the MCA.
   *
   * @return OMPI_SUCCESS Upon success
   * @return OMPI_ERROR Upon failure
   * 
   * This function starts up the entire MCA.  It initializes a bunch
   * of built-in MCA parameters, and initialized the MCA component
   * repository.
   *
   * It must be the first MCA function invoked.  It is normally
   * invoked during ompi_mpi_init() and specifically invoked in the
   * special case of the laminfo command.
   */
OMPI_DECLSPEC   int mca_base_open(void);

  /**
   * Last function called in the MCA
   *
   * @return OMPI_SUCCESS Upon success
   * @return OMPI_ERROR Upon failure
   *
   * This function closes down the entire MCA.  It clears all MCA
   * parameters and closes down the MCA component respository.  
   *
   * It must be the last MCA function invoked.  It is normally invoked
   * during ompi_mpi_finalize() and specifically invoked during the
   * special case of the laminfo command.
   */
OMPI_DECLSPEC  int mca_base_close(void);

  /* mca_base_cmd_line.c */

OMPI_DECLSPEC  int mca_base_cmd_line_setup(ompi_cmd_line_t *cmd);
OMPI_DECLSPEC  int mca_base_cmd_line_process_args(ompi_cmd_line_t *cmd);
OMPI_DECLSPEC  int mca_base_cmd_line_process_arg(const char *param, const char *value);

  /* mca_base_component_compare.c */
  
OMPI_DECLSPEC  int mca_base_component_compare_priority(mca_base_component_priority_list_item_t *a,
                                       mca_base_component_priority_list_item_t *b);
OMPI_DECLSPEC  int mca_base_component_compare(const mca_base_component_t *a,
                              const mca_base_component_t *b);
  int mca_base_component_compatible(const mca_base_component_t *a,
                              const mca_base_component_t *b);

  /* mca_base_component_find.c */

OMPI_DECLSPEC  int mca_base_component_find(const char *directory, const char *type,
                           const mca_base_component_t *static_components[],
                           ompi_list_t *found_components);

  /* mca_base_component_register.c */

OMPI_DECLSPEC  int mca_base_component_repository_initialize(void);
OMPI_DECLSPEC  int mca_base_component_repository_retain(char *type, 
                                        lt_dlhandle component_handle, 
                                        const mca_base_component_t *component_struct);
OMPI_DECLSPEC  int mca_base_component_repository_link(const char *src_type, 
                                      const char *src_name,
                                      const char *depend_type,
                                      const char *depend_name);
OMPI_DECLSPEC  void mca_base_component_repository_release(const mca_base_component_t *component);
OMPI_DECLSPEC  void mca_base_component_repository_finalize(void);

  /* mca_base_components_open.c */

OMPI_DECLSPEC  int mca_base_components_open(const char *type_name, int output_id,
                            const mca_base_component_t **static_components,
                            ompi_list_t *components_available);

  /* mca_base_components_close.c */
  
OMPI_DECLSPEC  int mca_base_components_close(int output_id, ompi_list_t *components_available, 
                             const mca_base_component_t *skip);

  /* mca_base_init_select_components.c */
  
OMPI_DECLSPEC  int mca_base_init_select_components(int requested, 
                                   bool allow_multi_user_threads,
                                   bool have_hidden_threads, int *provided);
  
#if 0
  /* JMS Not implemented yet */
  int mca_base_init_callback(mca_base_init_cb_t func);
  int mca_base_init_callbacks_invoke(void);
  int mca_base_component_select(int requested);
  
  int mca_base_param_associate(int index, int keyval);
  int mca_base_param_lookup_int(int index, MPI_Comm comm);
  char *mca_base_param_lookup_string(int index, MPI_Comm comm);
#endif

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* MCA_BASE_H */
