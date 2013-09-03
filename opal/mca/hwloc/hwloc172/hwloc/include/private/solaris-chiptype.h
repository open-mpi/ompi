/*
 * Copyright (c) 2009-2010 Oracle and/or its affiliates.  All rights reserved. 
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#ifdef HWLOC_INSIDE_PLUGIN
/*
 * these declarations are internal only, they are not available to plugins
 * (functions below are internal static symbols).
 */
#error This file should not be used in plugins
#endif


#ifndef HWLOC_PRIVATE_SOLARIS_CHIPTYPE_H
#define HWLOC_PRIVATE_SOLARIS_CHIPTYPE_H

/* SPARC Chip Modes. */
#define MODE_UNKNOWN            0
#define MODE_SPITFIRE           1
#define MODE_BLACKBIRD          2
#define MODE_CHEETAH            3
#define MODE_SPARC64_VI         4
#define MODE_T1                 5
#define MODE_T2                 6
#define MODE_SPARC64_VII        7
#define MODE_ROCK               8

/* SPARC Chip Implementations. */
#define IMPL_SPARC64_VI         0x6
#define IMPL_SPARC64_VII        0x7
#define IMPL_SPITFIRE           0x10
#define IMPL_BLACKBIRD          0x11
#define IMPL_SABRE              0x12
#define IMPL_HUMMINGBIRD        0x13
#define IMPL_CHEETAH            0x14
#define IMPL_CHEETAHPLUS        0x15
#define IMPL_JALAPENO           0x16
#define IMPL_JAGUAR             0x18
#define IMPL_PANTHER            0x19
#define IMPL_NIAGARA            0x23
#define IMPL_NIAGARA_2          0x24
#define IMPL_ROCK               0x25

/* Default Mfg, Cache, Speed settings */
#define TI_MANUFACTURER         0x17
#define TWO_MEG_CACHE           2097152
#define SPITFIRE_SPEED          142943750

char* hwloc_solaris_get_chip_type(void);
char* hwloc_solaris_get_chip_model(void);

#endif /* HWLOC_PRIVATE_SOLARIS_CHIPTYPE_H */
