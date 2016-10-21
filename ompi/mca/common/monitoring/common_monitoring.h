/*
 * Copyright (c) 2016 Inria.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_COMMON_MONITORING_H
#define MCA_COMMON_MONITORING_H

BEGIN_C_DECLS

#include <ompi_config.h>

#define MCA_MONITORING_MAKE_VERSION MCA_BASE_MAKE_VERSION(component, \
                                                          OMPI_MAJOR_VERSION, \
                                                          OMPI_MINOR_VERSION, \
                                                          OMPI_RELEASE_VERSION)

void finalize_monitoring( void );
int filter_monitoring( void );
/* void mca_pml_monitoring_reset( void ); */
/* int ompi_mca_pml_monitoring_flush(int fd, char* filename); */

END_C_DECLS

#endif  /* MCA_COMMON_MONITORING_H */
