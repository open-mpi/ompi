/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

#ifndef ATOMIC_TEST_H_
#define ATOMIC_TEST_H_

extern int atomic_verbose;

#define TEST_REPS 100

int atomic_spinlock_test(ompi_lock_t *lock, int count, int id);
int atomic_spinlock_test_th(ompi_lock_t *lock, int count, 
                            int id, int thr_count);

#endif
