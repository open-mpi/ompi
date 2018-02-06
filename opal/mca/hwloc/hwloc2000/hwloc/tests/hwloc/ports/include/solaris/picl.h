/*
 * Copyright © 2011 inria.  All rights reserved.
 * See COPYING in top-level directory.
 */

#ifndef HWLOC_PORT_SOLARIS_PICL_H
#define HWLOC_PORT_SOLARIS_PICL_H

#include <stdint.h>
#include <string.h>

#define PICL_PROPNAMELEN_MAX    256

typedef uint64_t picl_nodehdl_t;
typedef uint64_t picl_prophdl_t;

typedef enum {
  PICL_PTYPE_UNKNOWN,
  PICL_PTYPE_INT,
  PICL_PTYPE_UNSIGNED_INT,
  PICL_PTYPE_TABLE,
  PICL_PTYPE_CHARSTRING
} picl_prop_type_t;

typedef struct {
  picl_prop_type_t type;
  size_t size;
  char name[PICL_PROPNAMELEN_MAX];
} picl_propinfo_t;

typedef enum {
  PICL_SUCCESS,
  PICL_WALK_TERMINATE,
  PICL_ENDOFLIST
} picl_errno_t;

extern int picl_initialize(void);
extern int picl_shutdown(void);
extern int picl_get_root(picl_nodehdl_t *nodehandle);
extern int picl_get_first_prop(picl_nodehdl_t nodeh, picl_prophdl_t *proph);
extern int picl_get_next_prop(picl_prophdl_t proph, picl_prophdl_t *nexth);
extern int picl_get_propinfo(picl_prophdl_t proph, picl_propinfo_t *pi);
extern int picl_get_propval(picl_prophdl_t proph, void *valbuf, size_t sz);
extern int picl_get_next_by_row(picl_prophdl_t thish, picl_prophdl_t *proph);
extern int picl_get_next_by_col(picl_prophdl_t thish, picl_prophdl_t *proph);
extern int picl_walk_tree_by_class(picl_nodehdl_t rooth, const char *classname, void *c_args, int (*callback_fn)(picl_nodehdl_t hdl, void *args));

#endif /* HWLOC_PORT_SOLARIS_PICL_H */
