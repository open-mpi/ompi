/*
 * Copyright (c) 2019      UT-Battelle, LLC. All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"
#include "opal/util/memprof.h"


void __attribute__((weak)) Tau_track_class_allocation(const char * name,
                                                      size_t size) {

}

void __attribute__((weak)) Tau_track_class_deallocation(const char * name,
                                                        size_t size) {

}

void __attribute__((weak)) Tau_start_class_allocation(const char * name,
                                                      size_t size,
                                                      int include_in_parent) {

}

void __attribute__((weak)) Tau_stop_class_allocation(const char * name,
                                                     int record) {

}

void __attribute__((weak)) Tau_start_class_deallocation(const char * name,
                                                        size_t size,
                                                        int include_in_parent) {

}

void __attribute__((weak)) Tau_stop_class_deallocation(const char * name,
                                                       int record) {

}
