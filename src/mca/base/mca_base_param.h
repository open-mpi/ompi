/*
 * $HEADER$
 */

#ifndef LAM_MCA_BASE_PARAM_H
#define LAM_MCA_BASE_PARAM_H

#include "mpi.h"

/*
 * Types for MCA parameters
 */

typedef union {
  int intval;
  char *stringval;
} mca_base_param_storage_t;

#define MCA_BASE_PARAM_INFO ((void*) -1)


/*
 * The following types are really in this public .h file so that
 * laminfo can see them.  No one else should use them!
 */
typedef enum {
  MCA_BASE_PARAM_TYPE_INT,
  MCA_BASE_PARAM_TYPE_STRING,

  MCA_BASE_PARAM_TYPE_MAX
} mca_base_param_type_t;

struct mca_base_param_t {
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
 * Global functions for MCA
 */

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
  int mca_base_param_register_int(const char *type_name, 
                                  const char *module_name,
                                  const char *param_name, 
                                  const char *mca_param_name,
                                  int default_value);
  int mca_base_param_register_string(const char *type_name, 
                                     const char *module_name,
                                     const char *param_name, 
                                     const char *mca_param_name,
                                     const char *default_value);
  int mca_base_param_lookup_int(int index, int *value);
  int mca_base_param_lookup_string(int index, char **value);
  int mca_base_param_find(const char *type, const char *module, 
                          const char *param);
  int mca_base_param_finalize(void);

#if 0
  /* JMS these belong in libmpi */
  int mca_base_param_kv_associate(int index, int keyval);
  int mca_base_param_kv_lookup_int(int index, MPI_Comm comm);
  char *mca_base_param_kv_lookup_string(int index, MPI_Comm comm);
#endif
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* LAM_MCA_BASE_PARAM_H */
