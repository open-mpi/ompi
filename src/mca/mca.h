/*
 * $HEADER$
 */

#ifndef LAM_MCA_H
#define LAM_MCA_H

#include "mpi.h"
#include "lam/util/cmd_line.h"

/*
 * Types for each function
 */

typedef int (*mca_base_open_module_fn_t)(lam_cmd_line_t *cmd);
typedef int (*mca_base_close_module_fn_t)(void);


/*
 * Max length of some strings used later
 */

#define MCA_BASE_MAX_TYPE_NAME_LEN 32
#define MCA_BASE_MAX_MODULE_NAME_LEN 64

/*
 * The MCA guarantees that every module has three known pieces of
 * information: the MCA version that it conforms to, the type name and
 * version that it conforms to, and its own name and version.  This
 * triple will be present in *all* modules (regardless of MCA, type,
 * and module version), and identifies the module as well as how it
 * can be used.
 *
 * This information directly implies the data layout and content of
 * the rest module structure.  For example, MCA v1.0.0 specifies that
 * after this triple will be an instance of
 * mca_base_module_data_1_0_0_t, specifying module open and close
 * functions and a flag indicating whether the module is
 * checkpointable or not.
 *
 */

/*
 * Base/super for all modules, regardless of MCA version, type
 * version, or module version.
 */
struct mca_base_module_t {

  /* Version of the MCA */

  int mca_major_version;
  int mca_minor_version;
  int mca_release_version;

  /* The type's name and API version */

  char mca_type_name[MCA_BASE_MAX_TYPE_NAME_LEN];
  int mca_type_major_version;
  int mca_type_minor_version;
  int mca_type_release_version;

  /* The module's name and version */

  char mca_module_name[MCA_BASE_MAX_MODULE_NAME_LEN];
  int mca_module_major_version;
  int mca_module_minor_version;
  int mca_module_release_version;

  /* Functions for opening and closing the module */

  mca_base_open_module_fn_t mca_open_module;
  mca_base_close_module_fn_t mca_close_module;
};
typedef struct mca_base_module_t mca_base_module_t;

/*
 * Meta data for MCA v1.0.0 modules
 */
struct mca_base_module_data_1_0_0_t {

  /* Does this module support checkpoint or not? */

  bool mca_is_checkpointable;
};
typedef struct mca_base_module_data_1_0_0_t mca_base_module_data_1_0_0_t;

/*
 * Macro for module author convenience
 */
#define MCA_BASE_VERSION_1_0_0 1, 0, 0


/*
 * Structure for making priority lists of modules
 */
struct mca_base_module_priority_t {
  int lsm_priority;
  int lsm_thread_min, lsm_thread_max;
  mca_base_module_t *lsm_module;
};
typedef struct mca_base_module_priority_t mca_base_module_priority_t;


/*
 * Global functions for MCA
 */

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
  int mca_base_open(lam_cmd_line_t *cmd);
  int mca_base_close(void);

  int mca_base_arg_setup(lam_cmd_line_t *cmd);
  int mca_base_arg_process(lam_cmd_line_t *cmd);
  int mca_base_arg_process_one(char *type, char *arg);

  int mca_base_module_check(char *name, char *module, int is_default);
  int mca_base_module_compare(mca_base_module_t *a, mca_base_module_t *b);
  int mca_base_module_find(char *directory, char *type, 
                           mca_base_module_t *static_modules[], 
                           mca_base_module_t ***modules_out);
#if 0
  /* JMS add after the lbltdl stuff is done */
  int mca_base_module_register(char *type, lt_dlhandle module_handle, 
                               mca_t *module_struct);
#endif
  int mca_base_module_registry_init(void);
  int mca_base_module_registry_finalize(void);
  int mca_base_module_registry_link(const char *src_type, 
                                    const char *src_name,
                                    const char *depend_type,
                                    const char *depend_name);
  void mca_base_module_registry_unuse(mca_base_module_t *module);
  int mca_base_module_registry_use(const char *type, const char *name);

#if 0
  /* JMS Add after debug streams added */
  int mca_base_set_verbose(int index, lam_debug_stream_info_t *lds,
                           int *level, int *did);
#endif

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* LAM_MCA_H */
