/*
 * $HEADER$
 */

#ifndef MCA_BASE_H
#define MCA_BASE_H

#include "libltdl/ltdl.h"

#include "mpi.h"

/*
 * These units are large enough to warrant their own .h files
 */
#include "mca/base/mca_base_param.h"
#include "mca/base/mca_base_module_exchange.h"
#include "mca/base/mca_base_msgbuf.h"


/*
 * Structure for making plain lists of modules
 */
struct mca_base_module_list_item_t {
  ompi_list_item_t super;
  const mca_base_module_t *mli_module;
};
typedef struct mca_base_module_list_item_t mca_base_module_list_item_t;


/*
 * Structure for making priority lists of modules
 */
struct mca_base_module_priority_list_item_t {
  ompi_list_item_t super;

  int mpli_priority;
  int mpli_thread_min, mpli_thread_max;
  mca_base_module_t *mpli_module;
};
typedef struct mca_base_module_priority_list_item_t 
  mca_base_module_priority_list_item_t;


/*
 * Alloc and free mem functions
 */
typedef int (*mca_base_alloc_mem_fn_t)(MPI_Aint size, MPI_Info info, 
                                       void **base);
typedef int (*mca_base_free_mem_fn_t)(void *base);

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

    int mca_base_cmd_line_setup(ompi_cmd_line_t *cmd);
    int mca_base_cmd_line_process_args(ompi_cmd_line_t *cmd);
    int mca_base_cmd_line_process_arg(const char *param, const char *value);

    /* mca_base_module_compare.c */

    int mca_base_module_compare_priority(mca_base_module_priority_list_item_t *a,
                                         mca_base_module_priority_list_item_t *b);
    int mca_base_module_compare(mca_base_module_t *a,
                                mca_base_module_t *b);

    /* mca_base_module_find.c */

    int mca_base_module_find(const char *directory, const char *type,
                             const mca_base_module_t *static_modules[],
                             ompi_list_t *found_modules);

    /* mca_base_module_register.c */

    int mca_base_module_repository_initialize(void);
    int mca_base_module_repository_retain(char *type, 
                                          lt_dlhandle module_handle, 
                                          const mca_base_module_t *module_struct);
    int mca_base_module_repository_link(const char *src_type, 
                                        const char *src_name,
                                        const char *depend_type,
                                        const char *depend_name);
    void mca_base_module_repository_release(const mca_base_module_t *module);
    void mca_base_module_repository_finalize(void);

    /* mca_base_modules_open.c */

    int mca_base_modules_open(const char *type_name, int output_id,
                              const mca_base_module_t **static_modules,
                              ompi_list_t *modules_available);

    /* mca_base_modules_close.c */

    int mca_base_modules_close(int output_id, ompi_list_t *modules_available, 
                               const mca_base_module_t *skip);


    /* mca_base_mem.c */

    int mca_base_alloc_mem(MPI_Aint size, MPI_Info info, void *baseptr);
    int mca_base_free_mem(void *baseptr);

    /* mca_base_init_select_modules.c */

    int mca_base_init_select_modules(int requested, 
                                     bool allow_multi_user_threads,
                                     bool have_hidden_threads, int *provided);

#if 0
    /* JMS Not implemented yet */
    int mca_base_init_callback(mca_base_init_cb_t func);
    int mca_base_init_callbacks_invoke(void);
    int mca_base_module_select(int requested);

    int mca_base_param_associate(int index, int keyval);
    int mca_base_param_lookup_int(int index, MPI_Comm comm);
    char *mca_base_param_lookup_string(int index, MPI_Comm comm);
#endif

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* MCA_BASE_H */
