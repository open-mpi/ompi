/*
 * Copyright (c) 2018      UT-Battelle, LLC. All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OPAL_MEMPROF_H
#define OPAL_MEMPROF_H

BEGIN_C_DECLS

#if OPAL_ENABLE_MEM_PROFILE

    /*
     * NOTE: Must configure OMPI w/ '--enable-mem-profile',
     *       otherwise the macros are no-ops.
     *       Tested with TAU v1.27.1 and v1.28 and PDT 3.25
     *       for gathering memory profiling data about OMPI.
     *
     *       Example usage:
     *         orterun -np 2 tau_exec -T mpi,pdt ring_c
     *         tau_mem_summarize.py . > ring-np2.csv
     */

  void __attribute__((weak)) Tau_track_class_allocation(const char * name, size_t size);
  void __attribute__((weak)) Tau_track_class_deallocation(const char * name, size_t size);
  void __attribute__((weak)) Tau_start_class_allocation(const char * name, size_t size, int include_in_parent);
  void __attribute__((weak)) Tau_stop_class_allocation(const char * name, int record);
  void __attribute__((weak)) Tau_start_class_deallocation(const char * name, size_t size, int include_in_parent);
  void __attribute__((weak)) Tau_stop_class_deallocation(const char * name, int record);

  #define OPAL_MEMPROF_TRACK_ALLOC(name, size) \
    do { \
          Tau_track_class_allocation(name, size); \
       } while(0)

  #define OPAL_MEMPROF_TRACK_DEALLOC(name, size) \
    do { \
          Tau_track_class_deallocation(name, size); \
       } while(0)

  #define OPAL_MEMPROF_START_ALLOC(name, size, include_in_parent) \
    do { \
        Tau_start_class_allocation(name, size, include_in_parent); \
       } while(0)

  #define OPAL_MEMPROF_STOP_ALLOC(name, record) \
    do { \
        Tau_stop_class_allocation(name, record); \
       } while(0)

  #define OPAL_MEMPROF_START_DEALLOC(name, size, include_in_parent) \
    do { \
        Tau_start_class_deallocation(name, size, include_in_parent); \
       } while(0)

  #define OPAL_MEMPROF_STOP_DEALLOC(name, record) \
    do { \
        Tau_stop_class_deallocation(name, record); \
       } while(0)

#else

  #define OPAL_MEMPROF_TRACK_ALLOC(name, size) \
    do { } while(0)

  #define OPAL_MEMPROF_TRACK_DEALLOC(name, size) \
    do { } while(0)

  #define OPAL_MEMPROF_START_ALLOC(name, size, include_in_parent) \
    do { } while(0)

  #define OPAL_MEMPROF_STOP_ALLOC(name, record) \
    do { } while(0)

  #define OPAL_MEMPROF_START_DEALLOC(name, size, include_in_parent) \
    do { } while(0)

  #define OPAL_MEMPROF_STOP_DEALLOC(name, record) \
    do { } while(0)

#endif /* OPAL_ENABLE_MEM_PROFILE */

END_C_DECLS

#endif /* OPAL_MEMPROF_H */
