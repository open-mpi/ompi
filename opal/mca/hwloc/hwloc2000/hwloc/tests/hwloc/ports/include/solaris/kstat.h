/*
 * Copyright © 2009-2017 Inria.  All rights reserved.
 * Copyright © 2009 Université Bordeaux
 * See COPYING in top-level directory.
 */

#ifndef HWLOC_PORT_SOLARIS_KSTAT_H
#define HWLOC_PORT_SOLARIS_KSTAT_H

#include <inttypes.h>

typedef long kid_t;

#define KSTAT_STRLEN 31

typedef struct kstat {
  char ks_module[KSTAT_STRLEN];
  char ks_name[KSTAT_STRLEN];
  int ks_instance;
  struct kstat *ks_next;
} kstat_t;

typedef struct kstat_named {
  unsigned char data_type;
  union {
    char c[16];
    int32_t i32;
    uint32_t ui32;
    struct {
      union {
	char *ptr;
      } addr;
    } str;
  } value;
} kstat_named_t;

typedef struct kstat_ctl {
  kstat_t *kc_chain;
} kstat_ctl_t;

#define KSTAT_DATA_CHAR 1
#define KSTAT_DATA_INT32 1
#define KSTAT_DATA_UINT32 2
#define KSTAT_DATA_INT64 3
#define KSTAT_DATA_UINT64 4
#define KSTAT_DATA_STRING 9

kstat_ctl_t *kstat_open(void);
kid_t kstat_read(kstat_ctl_t *, kstat_t *, void *);
void *kstat_data_lookup(kstat_t *, char *);
int kstat_close(kstat_ctl_t *);

#endif /* HWLOC_PORT_SOLARIS_KSTAT_H */
