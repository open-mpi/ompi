/*
 * Copyright © 2009-2017 Inria.  All rights reserved.
 * Copyright © 2009-2011 Université Bordeaux
 * See COPYING in top-level directory.
 */

#ifndef HWLOC_PORT_SOLARIS_SYS_LGRP_USER_H
#define HWLOC_PORT_SOLARIS_SYS_LGRP_USER_H

#include <sys/processor.h>

typedef int lgrp_cookie_t;
#define LGRP_COOKIE_NONE 0
typedef long lgrp_id_t;
typedef long long lgrp_mem_size_t;

typedef enum lgrp_content {
	LGRP_CONTENT_ALL,
	LGRP_CONTENT_HIERARCHY = LGRP_CONTENT_ALL,
	LGRP_CONTENT_DIRECT
} lgrp_content_t;

typedef enum lgrp_view {
	LGRP_VIEW_CALLER,
	LGRP_VIEW_OS
} lgrp_view_t;

typedef enum lgrp_mem_size_flag {
	LGRP_MEM_SZ_FREE,
	LGRP_MEM_SZ_INSTALLED
} lgrp_mem_size_flag_t;

typedef enum lgrp_rsrc {
        LGRP_RSRC_MEM
} lgrp_rsrc_t;

typedef enum lgrp_lat_between {
	LGRP_LAT_CPU_TO_MEM
} lgrp_lat_between_t;

typedef enum lgrp_affinity {
	LGRP_AFF_NONE,
	LGRP_AFF_WEAK,
	LGRP_AFF_STRONG
} lgrp_affinity_t;

lgrp_cookie_t lgrp_init(lgrp_view_t view);

int lgrp_nlgrps(lgrp_cookie_t cookie);
lgrp_id_t lgrp_root(lgrp_cookie_t cookie);
int lgrp_cpus(lgrp_cookie_t cookie, lgrp_id_t lgrp, processorid_t *cpuids, unsigned int count, int content);
int lgrp_children(lgrp_cookie_t cookie, lgrp_id_t parent, lgrp_id_t *lgrp_array, unsigned int lgrp_array_size);
lgrp_mem_size_t lgrp_mem_size(lgrp_cookie_t cookie, lgrp_id_t lgrp, lgrp_mem_size_flag_t type, lgrp_content_t content);
int lgrp_resources(lgrp_cookie_t cookie, lgrp_id_t lgrp, lgrp_id_t *lgrps, unsigned int count, lgrp_rsrc_t type);
int lgrp_latency_cookie(lgrp_cookie_t cookie, lgrp_id_t from, lgrp_id_t to, lgrp_lat_between_t between);
int lgrp_affinity_set(idtype_t idtype, id_t id, lgrp_id_t lgrp, lgrp_affinity_t aff);
lgrp_affinity_t lgrp_affinity_get(idtype_t idtype, id_t id, lgrp_id_t lgrp);

int lgrp_fini(lgrp_cookie_t cookie);

/* Should actually be in sys/mman.h, but don't want to interfere with the system one */
#define MADV_ACCESS_DEFAULT 6
#define MADV_ACCESS_LWP     7
#define MADV_ACCESS_MANY    8

#endif /* HWLOC_PORT_SOLARIS_SYS_LGRP_USER_H */
