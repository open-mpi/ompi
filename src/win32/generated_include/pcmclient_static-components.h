/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

extern const mca_base_component_t mca_pcmclient_singleton_component;
extern const mca_base_component_t mca_pcmclient_seed_component;
extern const mca_base_component_t mca_pcmclient_env_component;

const mca_base_component_t *mca_pcmclient_base_static_components[] = {
  &mca_pcmclient_singleton_component, 
  &mca_pcmclient_seed_component, 
  &mca_pcmclient_env_component, 
  NULL
};
