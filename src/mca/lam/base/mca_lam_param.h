/*
 * $HEADER$
 */

#ifndef LAM_MCA_BASE_PARAM_H
#define LAM_MCA_BASE_PARAM_H

#include "mpi.h"
#include "lam/lfc/array.h"
#include "lam/lfc/object.h"
#include "lam/util/cmd_line.h"

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
  lam_array_item_t super;

  mca_base_param_type_t mbp_type;
  char *mbp_type_name;
  char *mbp_module_name;
  char *mbp_param_name;
  char *mbp_full_name;

  int mbp_keyval;
  char *mbp_env_var_name;

  mca_base_param_storage_t mbp_default_value;
};
typedef struct mca_base_param_t mca_base_param_t;


/*
 * Variable holding the array of registered MCA parameters
 */

extern lam_array_t mca_base_params;


/*
 * Global functions for MCA
 */

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
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

#endif /* LAM_MCA_BASE_PARAM_H */
