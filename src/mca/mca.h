/*
 * $HEADER$
 */

#ifndef LAM_MCA_H
#define LAM_MCA_H

#include "mpi.h"
#include "lam/util/cmd_line.h"
#include "lam/lfc/array.h"

/*
 * Types for each function
 */

typedef int (*mca_open_module_fn_t)(lam_cmd_line_t *cmd);
typedef int (*mca_close_module_fn_t)(void);
typedef int (*mca_mpi_init_callback_fn_t)(void);
typedef int (*mca_alloc_mem_fn_t)(MPI_Aint size, MPI_Info info, void **base);
typedef int (*mca_free_mem_fn_t)(void *base);


/*
 * Struct of meta data necessary in every MCA module, regardless of
 * its type.
 */

#define MCA_BASE_MAX_TYPE_NAME_LEN 32
#define MCA_BASE_MAX_MODULE_NAME_LEN 64

struct mca_module_1_0_0_t {

  /* Integer version numbers indicating which MCA API version this
     module conforms to. */

  int mca_major_version;
  int mca_minor_version;
  int mca_release_version;

  /* Information about the type */

  char mca_type_name[MCA_BASE_MAX_TYPE_NAME_LEN];
  int mca_type_major_version;
  int mca_type_minor_version;
  int mca_type_release_version;

  /* Information about the module itself */

  char mca_module_name[MCA_BASE_MAX_MODULE_NAME_LEN];
  int mca_module_major_version;
  int mca_module_minor_version;
  int mca_module_release_version;

  /* Functions for opening and closing the module */

  mca_open_module_fn_t mca_open_module;
  mca_close_module_fn_t mca_close_module;

  /* Does this module support checkpoint or not? */

  bool mca_is_checkpointable;
};
typedef struct mca_module_1_0_0_t mca_module_1_0_0_t;

/*
 * Set the default type to use version 1.0.0 of the MCA struct 
 */

typedef mca_module_1_0_0_t mca_module_t;


/*
 * Structure for making priority lists of modules
 */

struct mca_module_priority_t {
  int lsm_priority;
  int lsm_thread_min, lsm_thread_max;
  mca_t *lsm_module;
};
typedef struct mca_module_priority_t mca_module_priority_t;


/*
 * Types for MCA parameters
 */

typedef enum {
  MCA_BASE_PARAM_TYPE_INT,
  MCA_BASE_PARAM_TYPE_STRING,

  MCA_BASE_PARAM_TYPE_MAX
} mca_base_param_type_t;

typedef union {
  int intval;
  char *stringval;
} mca_base_param_storage_t;

#define MCA_BASE_PARAM_INFO ((void*) -1)

struct mca_base_param_t {
  mca_base_param_type_t lsbp_type;
  char *lsbp_type_name;
  char *lsbp_module_name;
  char *lsbp_param_name;
  char *lsbp_full_name;

  int lsbp_keyval;
  char *lsbp_env_var_name;

  mca_base_param_storage_t lsbp_default_value;
};
struct mca_base_param_t mca_base_param_t;


/*
 * Variable holding the array of registered MCA parameters
 */

extern lam_array_t *mca_base_params;


/*
 * Global functions for MCA
 */

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
  int mca_base_arg_setup(lam_cmd_line_t *cmd);
  int mca_base_arg_process(lam_cmd_line_t *cmd);
  int mca_base_arg_process_one(char *type, char *arg);

  int mca_base_close(void);
  int mca_base_open(lam_cmd_line_t *cmd);

  int mca_base_module_check(char *name, char *module, int is_default);
  int mca_base_module_compare(mca_module_t *a, mca_module_t *b);
  int mca_base_module_find(char *directory, char *type, 
                           mca_t *static_modules[], 
                           mca_t ***modules_out);
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
  void mca_base_module_registry_unuse(mca_t *module);
  int mca_base_module_registry_use(const char *type, const char *name);

  int mca_base_mpi_init_callback(mca_mpi_init_callback_fn_t func);
  int mca_base_mpi_init_callbacks_invoke(void);
  int mca_base_mpi_module_select(int requested);

#if 0
  /* JMS Are these necessary in L8? */
  struct in_addr mca_base_hostmap(struct in_addr *addr, char *keyname);
#endif
  void mca_base_hostmap_finalize(void);
#if 0
  /* JMS Add after debug streams added */
  int mca_base_set_verbose(int index, lam_debug_stream_info_t *lds,
                           int *level, int *did);
#endif

  int mca_base_param_register_int(char *type_name, char *module_name,
                                  char *param_name, char *mca_param_name,
                                  int default_value);
  int mca_base_param_register_string(char *type_name, char *module_name,
                                     char *param_name, 
                                     char *mca_param_name,
                                     char *default_value);
  int mca_base_param_lookup_int(int index);
  char *mca_base_param_lookup_string(int index);
  int mca_base_param_find(char *type, char *module, char *param);
  int mca_base_param_finalize(void);

  int mca_base_param_kv_associate(int index, int keyval);
  int mca_base_param_kv_lookup_int(int index, MPI_Comm comm);
  char *mca_base_param_kv_lookup_string(int index, MPI_Comm comm);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* LAM_MCA_H */
