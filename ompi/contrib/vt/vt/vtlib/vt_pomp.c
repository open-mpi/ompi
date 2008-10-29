/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2008, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#include <stdio.h>
#include <stdlib.h>

#include "pomp_lib.h"

#include "vt_fbindings.h"
#include "vt_pform.h"
#include "vt_pomp.h"
#include "vt_omplock.h"
#include "vt_ompreg.h"
#include "vt_trc.h"

void POMP_Atomic_enter(struct ompregdescr* r) {
  GUARDED_ENTER(rid);
}

void POMP_Atomic_exit(struct ompregdescr* r) {
  GUARDED_EXIT();
}

void POMP_Barrier_enter(struct ompregdescr* r) {
  GUARDED_COLL_ENTER_2('b', rid, brid);
}

void POMP_Barrier_exit(struct ompregdescr* r) {
  GUARDED_COLL_EXIT();
}

void POMP_Flush_enter(struct ompregdescr* r) {
  GUARDED_ENTER(rid);
}

void POMP_Flush_exit(struct ompregdescr* r) {
  GUARDED_EXIT();
}

void POMP_Critical_begin(struct ompregdescr* r) {
  if ( IS_POMP_TRACE_ON ) {
    struct VTRegDescr* data = (struct VTRegDescr*)(r->data);
    uint64_t time = vt_pform_wtime();
    vt_omp_alock(&time, data->brid);
    vt_enter(&time, data->sbrid);
  }
}

void POMP_Critical_end(struct ompregdescr* r) {
  if ( IS_POMP_TRACE_ON ) {
    struct VTRegDescr* data = (struct VTRegDescr*)(r->data);
    uint64_t time = vt_pform_wtime();
    vt_exit(&time);
    vt_omp_rlock(&time, data->brid);
  }
}

void POMP_Critical_enter(struct ompregdescr* r) {
  GUARDED_ENTER(rid);
}

void POMP_Critical_exit(struct ompregdescr* r) {
  GUARDED_EXIT();
}

void POMP_For_enter(struct ompregdescr* r) {
  GUARDED_ENTER_2('p', sbrid, rid);
}

void POMP_For_exit(struct ompregdescr* r) {
  GUARDED_EXIT();
}

void POMP_Master_begin(struct ompregdescr* r) {
  GUARDED_ENTER(rid);
}

void POMP_Master_end(struct ompregdescr* r) {
  GUARDED_EXIT();
}

void POMP_Parallel_begin(struct ompregdescr* r) {
  if ( IS_POMP_TRACE_ON ) {
    struct VTRegDescr* data = (struct VTRegDescr*)(r->data);
    uint64_t time = vt_pform_wtime();
    vt_omp_parallel_begin();
    vt_enter(&time, data->rid);
  }
}

void POMP_Parallel_end(struct ompregdescr* r) {
  if ( IS_POMP_TRACE_ON ) {
    uint64_t time = vt_pform_wtime();
    vt_exit(&time);
    vt_omp_parallel_end();
  }
}

void POMP_Parallel_fork(struct ompregdescr* r) {
  if ( IS_POMP_TRACE_ON ) {
    uint64_t time = vt_pform_wtime();
    vt_omp_fork(&time);
  }
}

void POMP_Parallel_join(struct ompregdescr* r) {
  if ( IS_POMP_TRACE_ON ) {
    uint64_t time = vt_pform_wtime();
    vt_omp_join(&time);
  }
}

void POMP_Section_begin(struct ompregdescr* r) {
  GUARDED_ENTER(sbrid);
}

void POMP_Section_end(struct ompregdescr* r) {
  GUARDED_EXIT();
}

void POMP_Sections_enter(struct ompregdescr* r) {
  GUARDED_ENTER_2('p', sbrid, rid);
}

void POMP_Sections_exit(struct ompregdescr* r) {
  GUARDED_EXIT();
}

void POMP_Single_begin(struct ompregdescr* r) {
  GUARDED_ENTER(sbrid);
}

void POMP_Single_end(struct ompregdescr* r) {
  GUARDED_EXIT();
}

void POMP_Single_enter(struct ompregdescr* r) {
  GUARDED_ENTER(rid);
}

void POMP_Single_exit(struct ompregdescr* r) {
  GUARDED_EXIT();
}

void POMP_Workshare_enter(struct ompregdescr* r) {
  GUARDED_ENTER_2('p', sbrid, rid);
}

void POMP_Workshare_exit(struct ompregdescr* r) {
  GUARDED_EXIT();
}

/*
 *----------------------------------------------------------------
 * C Wrapper for OpenMP API
 *----------------------------------------------------------------
 */

void POMP_Init_lock(omp_lock_t *s) {
  if ( IS_POMP_TRACE_ON ) {
    uint64_t time = vt_pform_wtime();
    vt_enter(&time, vt_omp_regid[VT__OMP_INIT_LOCK]);
    omp_init_lock(s);
    vt_lock_init(s);
    time = vt_pform_wtime();
    vt_exit(&time);
  } else {
    omp_init_lock(s);
    vt_lock_init(s);
  }
}

void POMP_Destroy_lock(omp_lock_t *s) {
  if ( IS_POMP_TRACE_ON ) {
    uint64_t time = vt_pform_wtime();
    vt_enter(&time, vt_omp_regid[VT__OMP_DESTROY_LOCK]);
    omp_destroy_lock(s);
    vt_lock_destroy(s);
    time = vt_pform_wtime();
    vt_exit(&time);
  } else {
    omp_destroy_lock(s);
    vt_lock_destroy(s);
  }
}

void POMP_Set_lock(omp_lock_t *s) {
  if ( IS_POMP_TRACE_ON ) {
    uint64_t time = vt_pform_wtime();
    vt_enter(&time, vt_omp_regid[VT__OMP_SET_LOCK]);
    omp_set_lock(s);
    time = vt_pform_wtime();
    vt_omp_alock(&time, vt_lock_id(s));
    vt_exit(&time);
  } else {
    omp_set_lock(s);
  }
}

void POMP_Unset_lock(omp_lock_t *s) {
  if ( IS_POMP_TRACE_ON ) {
    uint64_t time = vt_pform_wtime();
    vt_enter(&time, vt_omp_regid[VT__OMP_UNSET_LOCK]);
    omp_unset_lock(s);
    time = vt_pform_wtime();
    vt_omp_rlock(&time, vt_lock_id(s));
    vt_exit(&time);
  } else {
    omp_unset_lock(s);
  }
}

int  POMP_Test_lock(omp_lock_t *s) {
  if ( IS_POMP_TRACE_ON ) {
    int result;
    uint64_t time = vt_pform_wtime();

    vt_enter(&time, vt_omp_regid[VT__OMP_TEST_LOCK]);
    result = omp_test_lock(s);
    time = vt_pform_wtime();
    if (result) vt_omp_alock(&time, vt_lock_id(s));
    vt_exit(&time);
    return result;     
  } else {
    return omp_test_lock(s);
  }
}

void POMP_Init_nest_lock(omp_nest_lock_t *s) {
  if ( IS_POMP_TRACE_ON ) {
    uint64_t time = vt_pform_wtime();
    vt_enter(&time, vt_omp_regid[VT__OMP_INIT_NEST_LOCK]);
    omp_init_nest_lock(s);
    vt_lock_init(s);
    time = vt_pform_wtime();
    vt_exit(&time);
  } else {
    omp_init_nest_lock(s);
    vt_lock_init(s);
  }
}

void POMP_Destroy_nest_lock(omp_nest_lock_t *s) {
  if ( IS_POMP_TRACE_ON ) {
    uint64_t time = vt_pform_wtime();
    vt_enter(&time, vt_omp_regid[VT__OMP_DESTROY_NEST_LOCK]);
    omp_destroy_nest_lock(s);
    vt_lock_destroy(s);
    time = vt_pform_wtime();
    vt_exit(&time);
  } else {
    omp_destroy_nest_lock(s);
    vt_lock_destroy(s);
  }
}

void POMP_Set_nest_lock(omp_nest_lock_t *s) {
  if ( IS_POMP_TRACE_ON ) {
    uint64_t time = vt_pform_wtime();
    vt_enter(&time, vt_omp_regid[VT__OMP_SET_NEST_LOCK]);
    omp_set_nest_lock(s);
    time = vt_pform_wtime();
    vt_omp_alock(&time, vt_lock_id(s));
    vt_exit(&time);
  } else {
    omp_set_nest_lock(s);
  }
}

void POMP_Unset_nest_lock(omp_nest_lock_t *s) {
  if ( IS_POMP_TRACE_ON ) {
    uint64_t time = vt_pform_wtime();
    vt_enter(&time, vt_omp_regid[VT__OMP_UNSET_NEST_LOCK]);
    omp_unset_nest_lock(s);
    time = vt_pform_wtime();
    vt_omp_rlock(&time, vt_lock_id(s));
    vt_exit(&time);
  } else {
    omp_unset_nest_lock(s);
  }
}

int POMP_Test_nest_lock(omp_nest_lock_t *s) {
  if ( IS_POMP_TRACE_ON ) {
    int result;
    uint64_t time = vt_pform_wtime();

    vt_enter(&time, vt_omp_regid[VT__OMP_TEST_NEST_LOCK]);
    result = omp_test_nest_lock(s);
    time = vt_pform_wtime();
    if (result) vt_omp_alock(&time, vt_lock_id(s));
    vt_exit(&time);
    return result;
  } else {
    return omp_test_nest_lock(s);
  }
}

/*
 *----------------------------------------------------------------
 * Fortran Wrapper for OpenMP API
 *----------------------------------------------------------------
 */

/* macro for one-step declaration and definition of functions */
#define DEF_FPOMP_FUNC(function)  \
function; /* declaration */ \
function  /* definition */

DEF_FPOMP_FUNC(void POMP_Init_lock_f(omp_lock_t *s)) {
  if ( IS_POMP_TRACE_ON ) {
    uint64_t time = vt_pform_wtime();
    vt_enter(&time, vt_omp_regid[VT__OMP_INIT_LOCK]);
    omp_init_lock(s);
    vt_lock_init(s);
    time = vt_pform_wtime();
    vt_exit(&time);
  } else {
    omp_init_lock(s);
    vt_lock_init(s);
  }
} VT_GENERATE_F77_BINDINGS(pomp_init_lock, POMP_INIT_LOCK,
			   POMP_Init_lock_f,
			   (omp_lock_t *s),
			   (s))

DEF_FPOMP_FUNC(void POMP_Destroy_lock_f(omp_lock_t *s)) {
  if ( IS_POMP_TRACE_ON ) {
    uint64_t time = vt_pform_wtime();
    vt_enter(&time, vt_omp_regid[VT__OMP_DESTROY_LOCK]);
    omp_destroy_lock(s);
    vt_lock_destroy(s);
    time = vt_pform_wtime();
    vt_exit(&time);
  } else {
    omp_destroy_lock(s);
    vt_lock_destroy(s);
  }
} VT_GENERATE_F77_BINDINGS(pomp_destroy_lock, POMP_DESTROY_LOCK,
			   POMP_Destroy_lock_f,
			   (omp_lock_t *s),
			   (s))

DEF_FPOMP_FUNC(void POMP_Set_lock_f(omp_lock_t *s)) {
  if ( IS_POMP_TRACE_ON ) {
    uint64_t time = vt_pform_wtime();
    vt_enter(&time, vt_omp_regid[VT__OMP_SET_LOCK]);
    omp_set_lock(s);
    time = vt_pform_wtime();
    vt_omp_alock(&time, vt_lock_id(s));
    vt_exit(&time);
  } else {
    omp_set_lock(s);
  }
} VT_GENERATE_F77_BINDINGS(pomp_set_lock, POMP_SET_LOCK,
			   POMP_Set_lock_f,
			   (omp_lock_t *s),
			   (s))

DEF_FPOMP_FUNC(void POMP_Unset_lock_f(omp_lock_t *s)) {
  if ( IS_POMP_TRACE_ON ) {
    uint64_t time = vt_pform_wtime();
    vt_enter(&time, vt_omp_regid[VT__OMP_UNSET_LOCK]);
    omp_unset_lock(s);
    time = vt_pform_wtime();
    vt_omp_rlock(&time, vt_lock_id(s));
    vt_exit(&time);
  } else {
    omp_unset_lock(s);
  }
} VT_GENERATE_F77_BINDINGS(pomp_unset_lock, POMP_UNSET_LOCK,
			   POMP_Unset_lock_f,
			   (omp_lock_t *s),
			   (s))

DEF_FPOMP_FUNC(int POMP_Test_lock_f(omp_lock_t *s)) {
  if ( IS_POMP_TRACE_ON ) {
    int result;
    uint64_t time = vt_pform_wtime();

    vt_enter(&time, vt_omp_regid[VT__OMP_TEST_LOCK]);
    result = omp_test_lock(s);
    time = vt_pform_wtime();
    if (result) vt_omp_alock(&time, vt_lock_id(s));
    vt_exit(&time);
    return result;     
  } else {
    return omp_test_lock(s);
  }
} VT_GENERATE_F77_BINDINGS(pomp_test_lock, POMP_TEST_LOCK,
			   POMP_Test_lock_f,
			   (omp_lock_t *s),
			   (s))

#ifndef __osf__
DEF_FPOMP_FUNC(void POMP_Init_nest_lock_f(omp_nest_lock_t *s)) {
  if ( IS_POMP_TRACE_ON ) {
    uint64_t time = vt_pform_wtime();
    vt_enter(&time, vt_omp_regid[VT__OMP_INIT_NEST_LOCK]);
    omp_init_nest_lock(s);
    vt_lock_init(s);
    time = vt_pform_wtime();
    vt_exit(&time);
  } else {
    omp_init_nest_lock(s);
    vt_lock_init(s);
  }
} VT_GENERATE_F77_BINDINGS(pomp_init_nest_lock, POMP_INIT_NEST_LOCK,
			   POMP_Init_nest_lock_f,
			   (omp_nest_lock_t *s),
			   (s))

DEF_FPOMP_FUNC(void POMP_Destroy_nest_lock_f(omp_nest_lock_t *s)) {
  if ( IS_POMP_TRACE_ON ) {
    uint64_t time = vt_pform_wtime();
    vt_enter(&time, vt_omp_regid[VT__OMP_DESTROY_NEST_LOCK]);
    omp_destroy_nest_lock(s);
    vt_lock_destroy(s);
    time = vt_pform_wtime();
    vt_exit(&time);
  } else {
    omp_destroy_nest_lock(s);
    vt_lock_destroy(s);
  }
} VT_GENERATE_F77_BINDINGS(pomp_destroy_nest_lock, POMP_DESTROY_NEST_LOCK,
			   POMP_Destroy_nest_lock_f,
			   (omp_nest_lock_t *s),
			   (s))

DEF_FPOMP_FUNC(void POMP_Set_nest_lock_f(omp_nest_lock_t *s)) {
  if ( IS_POMP_TRACE_ON ) {
    uint64_t time = vt_pform_wtime();
    vt_enter(&time, vt_omp_regid[VT__OMP_SET_NEST_LOCK]);
    omp_set_nest_lock(s);
    time = vt_pform_wtime();
    vt_omp_alock(&time, vt_lock_id(s));
    vt_exit(&time);
  } else {
    omp_set_nest_lock(s);
  }
} VT_GENERATE_F77_BINDINGS(pomp_set_nest_lock, POMP_SET_NEST_LOCK,
			   POMP_Set_nest_lock_f,
			   (omp_nest_lock_t *s),
			   (s))

DEF_FPOMP_FUNC(void POMP_Unset_nest_lock_f(omp_nest_lock_t *s)) {
  if ( IS_POMP_TRACE_ON ) {
    uint64_t time = vt_pform_wtime();
    vt_enter(&time, vt_omp_regid[VT__OMP_UNSET_NEST_LOCK]);
    omp_unset_nest_lock(s);
    time = vt_pform_wtime();
    vt_omp_rlock(&time, vt_lock_id(s));
    vt_exit(&time);
  } else {
    omp_unset_nest_lock(s);
  }
} VT_GENERATE_F77_BINDINGS(pomp_unset_nest_lock, POMP_UNSET_NEST_LOCK,
			   POMP_Unset_nest_lock_f,
			   (omp_nest_lock_t *s),
			   (s))

DEF_FPOMP_FUNC(int POMP_Test_nest_lock_f(omp_nest_lock_t *s)) {
  if ( IS_POMP_TRACE_ON ) {
    int result;
    uint64_t time = vt_pform_wtime();

    vt_enter(&time, vt_omp_regid[VT__OMP_TEST_NEST_LOCK]);
    result = omp_test_nest_lock(s);
    time = vt_pform_wtime();
    if (result) vt_omp_alock(&time, vt_lock_id(s));
    vt_exit(&time);
    return result;
  } else {
    return omp_test_nest_lock(s);
  }
} VT_GENERATE_F77_BINDINGS(pomp_test_nest_lock, POMP_TEST_NEST_LOCK,
			   POMP_Test_nest_lock_f,
			   (omp_nest_lock_t *s),
			   (s))

#endif
