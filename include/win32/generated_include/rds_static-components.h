/*
 * $HEADER$
 */

extern const mca_base_component_t mca_rds_resfile_component;
extern const mca_base_component_t mca_rds_hostfile_component;

const mca_base_component_t *mca_rds_base_static_components[] = {
   &mca_rds_resfile_component, 
   &mca_rds_hostfile_component, 
  NULL
};
