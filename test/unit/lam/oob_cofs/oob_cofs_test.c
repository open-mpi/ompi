#include "mca/lam/oob/oob.h"
#include "mca/lam/oob/cofs/src/oob_cofs.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int
main(int argc, char* argv[])
{
  int ret;
  char *tmp;
  int priority = 0;
  struct mca_oob_1_0_0_t *init_ret;
  int target_vpid, source_vpid, source_tag;
  size_t source_len;
  char *source_data;
  char buffer[2048];
  int msg_count = 0;

  if (argc != 3) {
    printf("usage: %s my_vpid target_vpid\n", argv[0]);
    exit(1);
  }

  tmp = malloc(strlen("MCA_OOB_BASE_VPID") + strlen(argv[1]) + 2);
  sprintf(tmp, "MCA_OOB_BASE_VPID=%s", argv[1]);
  putenv(tmp);

  target_vpid = atoi(argv[2]);
  
  ret = mca_oob_cofs_open(NULL);
  if (ret != LAM_SUCCESS) {
    printf("mca_oob_cofs_open returned %d\n", ret);
    exit(1);
  }

  ret = mca_oob_cofs_query(&priority);
  if (ret != LAM_SUCCESS) {
    printf("mca_oob_cofs_query returned %d\n", ret);
    exit(1);
  } else {
    printf("mca_oob_cofs_query said \"go\" with priority %d\n", priority);
  }

  init_ret = mca_oob_cofs_init();
  if (init_ret == NULL) {
    printf("mca_oob_cofs_init returned NULL\n");
    exit(1);
  }

  printf("#\n# Sending Messages\n#\n\n");
  for (msg_count = 0 ; msg_count < 20 ; ++msg_count) {
    sprintf(buffer, "%s's message number %d\n", argv[1], msg_count);
    printf("%d %d: %s\n", target_vpid, 1, buffer);
    ret = mca_oob_cofs_send("foobar", target_vpid, 1, buffer, strlen(buffer) + 1);
    if (ret != LAM_SUCCESS) {
      printf("mca_oob_cofs_send failed on msg_count %d\n", msg_count);
      exit(1);
    }
  }

  printf("#\n# Receiving Messages\n#\n\n");
  for (msg_count = 0 ; msg_count < 20 ; ++msg_count) {
    source_tag = 1;
    ret = mca_oob_cofs_recv("foobar", target_vpid, &source_tag, (void**) &source_data, &source_len);
    if (ret != LAM_SUCCESS) {
      printf("mca_oob_cofs_recv failed on msg_count %d, %d\n", msg_count, ret);
      exit(1);
    }
    printf("%d %d: %s\n", source_vpid, source_tag, source_data);
    free(source_data);
  }

  printf("#\n# Finished\n#\n\n");
  return 0;
}
