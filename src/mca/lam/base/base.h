/*
 * $HEADER$
 */

#ifndef MCA_LAM_BASE_H
#define MCA_LAM_BASE_H

#include "mca/ltdl.h"

/*
 * These units are large enough to warrant their own .h files
 */
#include "mca/lam/base/mca_base_param.h"
#include "mca/lam/base/mca_base_module_exchange.h"


/*
 * Public variables
 */
extern int mca_base_param_module_path;



/*
 * Public functions
 */
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

  /* mca_base_open.c */

  int mca_base_open(void);

  /* mca_base_close.c */

  int mca_base_close(void);

  /* mca_base_cmd_line.c */

  int mca_base_cmd_line_setup(lam_cmd_line_t *cmd);
  int mca_base_cmd_line_process_args(lam_cmd_line_t *cmd);
  int mca_base_cmd_line_process_arg(const char *param, const char *value);

  /* mca_base_module_compare.c */

  int mca_base_module_compare(mca_base_module_priority_t *a,
                              mca_base_module_priority_t *b);

  /* mca_base_module_find.c */

  int mca_base_module_find(const char *directory, const char *type,
                           mca_base_module_t *static_modules[],
                           lam_list_t *found_modules);

  /* mca_base_module_register.c */

  int mca_base_module_registry_init(void);
  int mca_base_module_registry_retain(char *type, lt_dlhandle module_handle, 
                                      mca_base_module_t *module_struct);
  int mca_base_module_registry_link(const char *src_type, 
                                    const char *src_name,
                                    const char *depend_type,
                                    const char *depend_name);
  void mca_base_module_registry_release(mca_base_module_t *module);
  void mca_base_module_registry_finalize(void);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* MCA_LAM_BASE_H */
