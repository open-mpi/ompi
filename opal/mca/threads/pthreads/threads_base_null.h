/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OPAL_MCA_TIMER_BASE_TIMER_BASE_NULL_H
#define OPAL_MCA_TIMER_BASE_TIMER_BASE_NULL_H

opal_class_t opal_condition_t_class;
opal_class_t opal_mutex_t_class;
opal_class_t opal_recursive_mutex_t_class;

opal_thread_t *opal_thread_get_self(void) {
	return NULL;
}

bool opal_thread_self_compare(opal_thread_t *t) {
	return 0;
}

int sync_wait_mt(void *p) {
	return 0;
}

int opal_thread_join(opal_thread_t *t, void **thr_return) {
  return 0;
}

void opal_thread_set_main() {
}

int opal_thread_start(opal_thread_t *t) {
  return 0;
}
opal_class_t opal_thread_t_class;

typedef int opal_tsd_key_t;
typedef int opal_tsd_destructor_t;

int opal_tsd_key_create(opal_tsd_key_t *key, opal_tsd_destructor_t destructor)
{
  return 0;
}
bool opal_uses_threads = 0;
int opal_tsd_keys_destruct()
{
  return 0;
}
#endif
