/* -*- C -*-
 * 
 * $HEADER$
 *
 */

#include "lam_config.h"

#include "mca/lam/oob/oob.h"
#include "mca/lam/oob/cofs/src/oob_cofs.h"
#include "lam/util/malloc.h"

#include <stdio.h>
#include <sys/types.h>
#include <dirent.h>
#include <string.h>
#include <unistd.h>

static int blocking_recv_posted = 0;
static int do_recv(lam_job_handle_t job_handle, int* tag, int* vpid, 
                   void** data, size_t* data_len);

int
mca_oob_cofs_send(lam_job_handle_t job_handle, int vpid, int tag, 
                  void* data, size_t data_len)
{
  FILE *fp;
  size_t wlen;
  char msg_file[LAM_PATH_MAX];
  char msg_file_tmp[LAM_PATH_MAX];

  /* create the file and open it... */
  snprintf(msg_file, LAM_PATH_MAX, "%s/%s_%d_%d_%d_%d.msg", mca_oob_cofs_comm_loc,
           job_handle, mca_oob_cofs_my_vpid, vpid, tag, mca_oob_cofs_serial);
  snprintf(msg_file_tmp, LAM_PATH_MAX, "%s/.%s_%d_%d_%d_%d.msg", mca_oob_cofs_comm_loc,
           job_handle, mca_oob_cofs_my_vpid, vpid, tag, mca_oob_cofs_serial);

  fp = fopen(msg_file_tmp, "w");
  if (fp == NULL) {
    return LAM_ERR_OUT_OF_RESOURCE;
  }

  /* BWB - do network byte ordering... */
  /* write size */
  wlen = fwrite(&data_len, sizeof(size_t), 1, fp);
  if (wlen != 1) {
    fclose(fp);
    unlink(msg_file_tmp);
    return LAM_ERR_OUT_OF_RESOURCE;
  }

  /* write packet */
  wlen = fwrite(data, 1, data_len, fp);
  if (wlen != data_len) {
    fclose(fp);
    unlink(msg_file_tmp);
    return LAM_ERR_OUT_OF_RESOURCE;
  }

  /* publish the thing... */
  fclose(fp);
  rename(msg_file_tmp, msg_file);

  return LAM_SUCCESS;
}


int
mca_oob_cofs_recv(lam_job_handle_t job_handle, int* tag, int* vpid, 
                  void** data, size_t* data_len)
{
  int ret = LAM_ERR_WOULD_BLOCK;
  blocking_recv_posted = 1;
  while (ret == LAM_ERR_WOULD_BLOCK) {
    ret = do_recv(job_handle, tag, vpid, data, data_len);
  }
  blocking_recv_posted = 0;
  return ret;
}


int
mca_oob_cofs_recv_nb(lam_job_handle_t job_handle, int* tag, int* vpid, 
                     void** data, size_t* data_len)
{
  if (blocking_recv_posted != 0) {
    return LAM_ERR_WOULD_BLOCK;
  }

  return do_recv(job_handle, tag, vpid, data, data_len);
}


int
mca_oob_cofs_recv_cb(lam_job_handle_t job_handle, int tag, 
                     mca_oob_recv_cb_t callback)
{
  return LAM_ERR_NOT_SUPPORTED;
}


static char*
find_match(lam_job_handle_t job_handle, int* tag, int* vpid)
{
  DIR* dir;
  struct dirent *ent;
  char tmp_handle[LAM_PATH_MAX];
  int tmp_tag, tmp_vpid, tmp_myvpid, tmp_serial;
  int ret;
  bool found = false;
  char best_name[LAM_PATH_MAX];
  int best_tag, best_vpid;
  
  dir = opendir(mca_oob_cofs_comm_loc);
  if (dir == NULL) {
    return NULL;
  }

  while ((ent = readdir(dir)) != NULL) {
    if (ent->d_name[0] = '.') continue;
    ret = sscanf(ent->d_name, "%s_%d_%d_%d_%d.msg", tmp_handle, &tmp_myvpid, 
                 &tmp_vpid, &tmp_tag, &tmp_serial);
    if (ret != 5) continue;

    if (strcmp(tmp_handle, job_handle)) continue;
    if (tmp_myvpid != mca_oob_cofs_my_vpid) continue;
    if (*tag != MCA_OOB_ANY_TAG && tmp_tag != *tag) continue;

    /* do best one here... */
    found = true;
    strcpy(best_name, ent->d_name);
    best_tag = tmp_tag;
    best_vpid = tmp_vpid;
    break;
  }

  closedir(dir);
  if (found) {
    *tag = best_tag;
    *vpid = best_vpid;
    return strdup(best_name);
  } else {
    return NULL;
  }
}


static int
do_recv(lam_job_handle_t job_handle, int* tag, int* vpid, 
        void** data, size_t* data_len)
{
  char *fname;
  FILE *fp;
  size_t rlen;

  fname = find_match(job_handle, tag, vpid);
  if (fname == NULL) {
    return LAM_ERR_WOULD_BLOCK;
  }

  fp = fopen(fname, "r");
  if (fp == NULL) {
    free(fname);
    return LAM_ERROR;
  }

  unlink(fname);

  rlen = fread(data_len, sizeof(size_t), 1, fp);
  if (rlen != 1) {
    free(fname);
    fclose(fp);
    return LAM_ERROR;
  }

  *data = (void*) malloc(*data_len);
  if (*data_len == NULL) {
    free(fname);
    fclose(fp);
    *data_len = 0;
    return LAM_ERROR;
  }

  rlen = fread(*data, 1, *data_len, fp);
  if (rlen != *data_len) {
    free(fname);
    fclose(fp);
    free(*data);
    *data_len = 0;
    return LAM_ERROR;
  }

  free(fname);
  fclose(fp);

  return LAM_SUCCESS;
}
