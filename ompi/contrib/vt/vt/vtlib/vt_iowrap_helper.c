/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2006, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich GmbH, Federal
 * Republic of Germany
 *
 * See the file COPYRIGHT in the package base directory for details
 **/

#include "vt_iowrap_helper.h"
#include "vt_defs.h"
#include "vt_error.h"
#include "vt_trc.h"

#include <stdlib.h>
#include <assert.h>
#include <sys/resource.h>

#if (defined (VT_OMPI) || defined (VT_OMP))
#   include <omp.h>
#endif


static int global_handle_counter = 1;

uint32_t file_group_id_stdio = 0;
uint32_t file_group_id_rest = 0;
int max_open_files = 0;
vampir_file_t *fd_to_vampirid = NULL;


static void store_vampir_file_id(int fd, uint32_t file_id,
    uint32_t file_group_id)
{
  vampir_file_t *file_ptr;

  assert(fd<max_open_files);
#if defined (VT_OMPI) || defined (VT_OMP)
  file_ptr = &fd_to_vampirid[(omp_get_thread_num()*max_open_files)+fd];
#else
  file_ptr = &fd_to_vampirid[fd];
#endif
  file_ptr->vampir_file_id = file_id;
  file_ptr->vampir_file_group_id = file_group_id;
# if defined (VT_OMPI) || defined (VT_OMP)
#   pragma omp critical (vt_iofile_1)
  {
# endif
    file_ptr->handle_id = global_handle_counter++;
# if defined (VT_OMPI) || defined (VT_OMP)
  }
# endif
}

vampir_file_t *get_vampir_file(int fd)
{
  vampir_file_t *file_ptr;

  assert(fd<max_open_files);
#if defined (VT_OMPI) || defined (VT_OMP)
  file_ptr = &fd_to_vampirid[(omp_get_thread_num()*max_open_files)+fd];
#else
  file_ptr = &fd_to_vampirid[fd];
#endif
  return file_ptr;
}

/** read max open files per process from */
int get_max_open_files(void)
{
  struct rlimit rlp;
  if (getrlimit(RLIMIT_NOFILE, &rlp)!=0)
    vt_error_msg("");
  return rlp.rlim_cur;
}

int get_total_open_files(int max_open_files)
{
#if defined (VT_OMPI) || defined (VT_OMP)
  /* alloc numthreads * max_open_files entries */
  return (max_open_files * omp_get_thread_num());
#else
  return max_open_files;
#endif
}

void vt_iofile_open(const char* fname, int fd)
{
  uint32_t fid;
  uint32_t gid;

  /* fprintf( stderr, "opening file: %s\n", fname); */

  gid=(fd<3) ? file_group_id_stdio : file_group_id_rest;
  fid = vt_def_fileio(fname, gid);
  store_vampir_file_id(fd, fid, gid);
}

