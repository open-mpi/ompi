/*
 * Copyright (c) 2013      Sandia National Laboratories. All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */

#ifndef MCA_OMPI_RTE_PMI_INTERNAL_H
#define MCA_OMPI_RTE_PMI_INTERNAL_H

BEGIN_C_DECLS

extern int ompi_rte_pmi_name_init(void);
extern int ompi_rte_pmi_name_fini(void);

extern int ompi_rte_pmi_db_init(void);
extern void ompi_rte_pmi_db_fini(void);

#define OMPI_DB_BIND_LEVEL   "ompi.bind_level"
#define OMPI_DB_BIND_INDEX   "ompi.bind_index"
#define OMPI_DB_NODERANK     "ompi.noderank"
#define OMPI_DB_RTE_INFO     "ompi.rte-info"

END_C_DECLS

#endif
