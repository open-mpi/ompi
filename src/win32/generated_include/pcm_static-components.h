/*
 * $HEADER$
 */

extern const mca_base_component_t mca_pcm_rsh_component;
extern const mca_base_component_t mca_pcm_ompid_component;
extern const mca_base_component_t mca_pcm_rms_component;

const mca_base_component_t *mca_pcm_base_static_components[] = {
  &mca_pcm_rsh_component, 
  &mca_pcm_ompid_component, 
  &mca_pcm_rms_component, 
  NULL
};
