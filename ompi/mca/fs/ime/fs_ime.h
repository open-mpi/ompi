/*
 * Copyright (c) 2018      DataDirect Networks. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_FS_IME_H
#define MCA_FS_IME_H

#include "ompi_config.h"
#include "ompi/mca/mca.h"
#include "ompi/mca/fs/fs.h"
#include "ompi/mca/common/ompio/common_ompio.h"

extern int mca_fs_ime_priority;
extern int mca_fs_ime_lock_algorithm;

#define FS_IME_LOCK_AUTO            0
#define FS_IME_BASE_PRIORITY        20
#define FS_IME_INCREASED_PRIORITY   50

BEGIN_C_DECLS

int mca_fs_ime_component_init_query(bool enable_progress_threads,
                                        bool enable_mpi_threads);
struct mca_fs_base_module_1_0_0_t *
mca_fs_ime_component_file_query (ompio_file_t *fh, int *priority);
int mca_fs_ime_component_file_unquery (ompio_file_t *file);

int mca_fs_ime_module_init (ompio_file_t *file);
int mca_fs_ime_module_finalize (ompio_file_t *file);

int mca_fs_ime_native_fini();

OMPI_MODULE_DECLSPEC extern mca_fs_base_component_2_0_0_t mca_fs_ime_component;
/*
 * ******************************************************************
 * ********* functions which are implemented in this module *********
 * ******************************************************************
 */

int mca_fs_ime_file_open (struct ompi_communicator_t *comm,
                          const char *filename,
                          int amode,
                          struct opal_info_t *info,
                          ompio_file_t *fh);

int mca_fs_ime_file_close (ompio_file_t *fh);

int mca_fs_ime_file_delete (char *filename,
                            struct opal_info_t *info);

int mca_fs_ime_file_set_size (ompio_file_t *fh,
                              OMPI_MPI_OFFSET_TYPE size);

int mca_fs_ime_file_get_size (ompio_file_t *fh,
                              OMPI_MPI_OFFSET_TYPE *size);

int mca_fs_ime_file_sync (ompio_file_t *fh);

/*
 * ******************************************************************
 * ************ functions implemented in this module end ************
 * ******************************************************************
 */

END_C_DECLS

#endif /* MCA_FS_IME_H */
