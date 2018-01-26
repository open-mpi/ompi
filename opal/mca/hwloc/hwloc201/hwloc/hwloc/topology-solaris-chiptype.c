/*
 * Copyright © 2009-2010 Oracle and/or its affiliates.  All rights reserved.
 * Copyright © 2013 Université Bordeaux.  All rights reserved.
 * Copyright © 2016-2017 Inria.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <private/autogen/config.h>
#include <hwloc.h>
#include <private/solaris-chiptype.h>
#include <private/misc.h>
#include <private/debug.h>

#include <stdlib.h>
#include <strings.h>

#ifdef HAVE_PICL_H
#include <sys/systeminfo.h>
#include <picl.h>

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

/*****************************************************************************
   Order of this list is important for the assign_value and
   assign_string_value routines
*****************************************************************************/

static const char* items[] = {
#define INDEX_PROCESSOR_TYPE 0
  "ProcessorType",
#define INDEX_BRAND_STRING 1
  "brand-string",
#define INDEX_COMPATIBLE 2
  "compatible",
#define INDEX_IMPLEMENTATION 3
  "implementation#",

/* the following groups must be contigous from L1I to L3 each */
#define INDEX_L1I_CACHE_SIZE 4
  "l1-icache-size",
#define INDEX_L1D_CACHE_SIZE 5
  "l1-dcache-size",
#define INDEX_L2I_CACHE_SIZE 6
  "l2-icache-size",
#define INDEX_L2D_CACHE_SIZE 7
  "l2-dcache-size",
#define INDEX_L3_CACHE_SIZE 8
  "l3-cache-size",

#define INDEX_L1I_CACHE_LINESIZE 9
  "l1-icache-line-size",
#define INDEX_L1D_CACHE_LINESIZE 10
  "l1-dcache-line-size",
#define INDEX_L2I_CACHE_LINESIZE 11
  "l2-icache-line-size",
#define INDEX_L2D_CACHE_LINESIZE 12
  "l2-dcache-line-size",
#define INDEX_L3_CACHE_LINESIZE 13
  "l3-cache-line-size",

#define INDEX_L1I_CACHE_ASSOCIATIVITY 14
  "l1-icache-associativity",
#define INDEX_L1D_CACHE_ASSOCIATIVITY 15
  "l1-dcache-associativity",
#define INDEX_L2I_CACHE_ASSOCIATIVITY 16
  "l2-icache-associativity",
#define INDEX_L2D_CACHE_ASSOCIATIVITY 17
  "l2-dcache-associativity",
#define INDEX_L3_CACHE_ASSOCIATIVITY 18
  "l3-cache-associativity",

#define INDEX_L2U_CACHE_SIZE 19
  "l2-cache-size",
#define INDEX_L2U_CACHE_LINESIZE 20
  "l2-cache-line-size",
#define INDEX_L2U_CACHE_ASSOCIATIVITY 21
  "l2-cache-associativity",

#define INDEX_SL2_CACHE_SIZE 22
  "sectored-l2-cache-size",
#define INDEX_SL2_CACHE_LINESIZE 23
  "sectored-l2-cache-line-size",
#define INDEX_SL2_CACHE_ASSOCIATIVITY 24
  "sectored-l2-cache-associativity",
};

#define NUM_ITEMS (sizeof(items) / sizeof(items[0]))

/*****************************************************************************
SPARC strings for chip modes and implementation
*****************************************************************************/
static const char* sparc_modes[] = {
#define MODE_UNKNOWN            0
    "UNKNOWN",
#define MODE_SPITFIRE           1
    "SPITFIRE",
#define MODE_BLACKBIRD          2
    "BLACKBIRD",
#define MODE_CHEETAH            3
    "CHEETAH",
#define MODE_SPARC64_VI         4
    "SPARC64_VI",
#define MODE_T1                 5
    "T1",
#define MODE_T2                 6
    "T2",
/* needs T4, T3 and T2+ ? */
#define MODE_SPARC64_VII        7
    "SPARC64_VII",
#define MODE_ROCK               8
    "ROCK",
#define MODE_T5                 9
    "T5",
#define MODE_T6                 10
    "T6",
#define MODE_M7                 11
    "M7",
#define MODE_S7                 12
    "S7",
#define MODE_M8                 13
    "M8"
};

/*****************************************************************************
Default values are for Unknown so we can build up from there.
*****************************************************************************/

static int  called_cpu_probe      = 0;
static char dss_chip_type[PICL_PROPNAMELEN_MAX];
static char dss_chip_model[PICL_PROPNAMELEN_MAX];
static long dss_chip_mode         = MODE_UNKNOWN;

struct hwloc_solaris_chip_info_s chip_info;

/*****************************************************************************
Assigns values based on the value of index.  For this reason, the order of
the items array is important.
*****************************************************************************/
static void assign_value(int index, long long val) {
  if (index == INDEX_IMPLEMENTATION) {
    /* implementation#  T1, T2, and Rock do not have this, see RFE 6615268 */
    long dss_chip_impl = val;
    if (dss_chip_impl == IMPL_SPITFIRE) {
      dss_chip_mode = MODE_SPITFIRE;
    }
    else if ((dss_chip_impl >= IMPL_BLACKBIRD) &&
             (dss_chip_impl <= IMPL_HUMMINGBIRD)) {
      dss_chip_mode = MODE_BLACKBIRD;
    }
    else if ((dss_chip_impl >= IMPL_CHEETAH) &&
             (dss_chip_impl <= IMPL_PANTHER)) {
      dss_chip_mode = MODE_CHEETAH;
    }
    else if (dss_chip_impl == IMPL_SPARC64_VI) {
      dss_chip_mode = MODE_SPARC64_VI;
    }
    else if (dss_chip_impl == IMPL_NIAGARA) {
      dss_chip_mode = MODE_T1;
    }
    else if (dss_chip_impl == IMPL_NIAGARA_2) {
      dss_chip_mode = MODE_T2;
    }
    else if (dss_chip_impl == IMPL_SPARC64_VII) {
      dss_chip_mode = MODE_SPARC64_VII;
    }
    else if (dss_chip_impl == IMPL_ROCK) {
      dss_chip_mode = MODE_ROCK;
    }
  }

  else if ((index >= INDEX_L1I_CACHE_SIZE) && (index <= INDEX_L3_CACHE_SIZE)) {
    /* make sure our indexes and the target structure are ordered the same */
    HWLOC_BUILD_ASSERT(INDEX_L1I_CACHE_SIZE+HWLOC_SOLARIS_CHIP_INFO_L1I == INDEX_L1I_CACHE_SIZE);
    HWLOC_BUILD_ASSERT(INDEX_L1I_CACHE_SIZE+HWLOC_SOLARIS_CHIP_INFO_L1D == INDEX_L1D_CACHE_SIZE);
    HWLOC_BUILD_ASSERT(INDEX_L1I_CACHE_SIZE+HWLOC_SOLARIS_CHIP_INFO_L2I == INDEX_L2I_CACHE_SIZE);
    HWLOC_BUILD_ASSERT(INDEX_L1I_CACHE_SIZE+HWLOC_SOLARIS_CHIP_INFO_L2D == INDEX_L2D_CACHE_SIZE);
    HWLOC_BUILD_ASSERT(INDEX_L1I_CACHE_SIZE+HWLOC_SOLARIS_CHIP_INFO_L3  == INDEX_L3_CACHE_SIZE);

    chip_info.cache_size[index-INDEX_L1I_CACHE_SIZE] = val;
  }
  else if ((index >= INDEX_L1I_CACHE_LINESIZE) && (index <= INDEX_L3_CACHE_LINESIZE)) {
    /* make sure our indexes and the target structure are ordered the same */
    HWLOC_BUILD_ASSERT(INDEX_L1I_CACHE_LINESIZE+HWLOC_SOLARIS_CHIP_INFO_L1I == INDEX_L1I_CACHE_LINESIZE);
    HWLOC_BUILD_ASSERT(INDEX_L1I_CACHE_LINESIZE+HWLOC_SOLARIS_CHIP_INFO_L1D == INDEX_L1D_CACHE_LINESIZE);
    HWLOC_BUILD_ASSERT(INDEX_L1I_CACHE_LINESIZE+HWLOC_SOLARIS_CHIP_INFO_L2I == INDEX_L2I_CACHE_LINESIZE);
    HWLOC_BUILD_ASSERT(INDEX_L1I_CACHE_LINESIZE+HWLOC_SOLARIS_CHIP_INFO_L2D == INDEX_L2D_CACHE_LINESIZE);
    HWLOC_BUILD_ASSERT(INDEX_L1I_CACHE_LINESIZE+HWLOC_SOLARIS_CHIP_INFO_L3  == INDEX_L3_CACHE_LINESIZE);

    chip_info.cache_linesize[index-INDEX_L1I_CACHE_LINESIZE] = val;
  }
  else if ((index >= INDEX_L1I_CACHE_ASSOCIATIVITY) && (index <= INDEX_L3_CACHE_ASSOCIATIVITY)) {
    /* make sure our indexes and the target structure are ordered the same */
    HWLOC_BUILD_ASSERT(INDEX_L1I_CACHE_ASSOCIATIVITY+HWLOC_SOLARIS_CHIP_INFO_L1I == INDEX_L1I_CACHE_ASSOCIATIVITY);
    HWLOC_BUILD_ASSERT(INDEX_L1I_CACHE_ASSOCIATIVITY+HWLOC_SOLARIS_CHIP_INFO_L1D == INDEX_L1D_CACHE_ASSOCIATIVITY);
    HWLOC_BUILD_ASSERT(INDEX_L1I_CACHE_ASSOCIATIVITY+HWLOC_SOLARIS_CHIP_INFO_L2I == INDEX_L2I_CACHE_ASSOCIATIVITY);
    HWLOC_BUILD_ASSERT(INDEX_L1I_CACHE_ASSOCIATIVITY+HWLOC_SOLARIS_CHIP_INFO_L2D == INDEX_L2D_CACHE_ASSOCIATIVITY);
    HWLOC_BUILD_ASSERT(INDEX_L1I_CACHE_ASSOCIATIVITY+HWLOC_SOLARIS_CHIP_INFO_L3  == INDEX_L3_CACHE_ASSOCIATIVITY);

    chip_info.cache_associativity[index-INDEX_L1I_CACHE_ASSOCIATIVITY] = val;
  }

  /* store L2U info in L2D with l2_unified flag */
  else if (index == INDEX_L2U_CACHE_SIZE) {
    chip_info.cache_size[HWLOC_SOLARIS_CHIP_INFO_L2D] = val;
    chip_info.l2_unified = 1;
  }
  else if (index == INDEX_L2U_CACHE_LINESIZE) {
    chip_info.cache_linesize[HWLOC_SOLARIS_CHIP_INFO_L2D] = val;
    chip_info.l2_unified = 1;
  }
  else if (index == INDEX_L2U_CACHE_ASSOCIATIVITY) {
    chip_info.cache_associativity[HWLOC_SOLARIS_CHIP_INFO_L2D] = val;
    chip_info.l2_unified = 1;
  }

  /* assume sectored L2 is identical to L2u for size/linesize/associativity */
  else if (index == INDEX_SL2_CACHE_SIZE) {
    chip_info.cache_size[HWLOC_SOLARIS_CHIP_INFO_L2D] = val;
    chip_info.l2_unified = 1;
  }
  else if (index == INDEX_SL2_CACHE_LINESIZE) {
    chip_info.cache_linesize[HWLOC_SOLARIS_CHIP_INFO_L2D] = val;
    chip_info.l2_unified = 1;
  }
  else if (index == INDEX_SL2_CACHE_ASSOCIATIVITY) {
    chip_info.cache_associativity[HWLOC_SOLARIS_CHIP_INFO_L2D] = val;
    chip_info.l2_unified = 1;
  }
}

/*****************************************************************************
Assigns values based on the value of index.  For this reason, the order of
the items array is important.
*****************************************************************************/
static void assign_string_value(int index, char* string_val) {
  if (index == INDEX_COMPATIBLE) { /* compatible */
    if (strncasecmp(string_val, "FJSV,SPARC64-VI",
                    PICL_PROPNAMELEN_MAX) == 0) {
      dss_chip_mode = MODE_SPARC64_VI;
    }
    else if (strncasecmp(string_val, "SUNW,UltraSPARC-T1",
                         PICL_PROPNAMELEN_MAX) == 0) {
      dss_chip_mode = MODE_T1;
    }
    else if (strncasecmp(string_val, "SUNW,UltraSPARC-T2",
                         PICL_PROPNAMELEN_MAX) == 0) {
      dss_chip_mode = MODE_T2;
    }
    else if (strncasecmp(string_val, "FJSV,SPARC64-VII",
                         PICL_PROPNAMELEN_MAX) == 0) {
      dss_chip_mode = MODE_SPARC64_VII;
    }
    else if (strncasecmp(string_val, "SUNW,Rock",
                         PICL_PROPNAMELEN_MAX) == 0) {
      dss_chip_mode = MODE_ROCK;
    }
    else if (strncasecmp(string_val, "SPARC-T5",
			 PICL_PROPNAMELEN_MAX) == 0) {
      dss_chip_mode = MODE_T5;
    }
    else if (strncasecmp(string_val, "SPARC-T6", /* not actually tested */
			 PICL_PROPNAMELEN_MAX) == 0) {
      dss_chip_mode = MODE_T6;
    }
    else if (strncasecmp(string_val, "SPARC-M7",
			 PICL_PROPNAMELEN_MAX) == 0) {
      dss_chip_mode = MODE_M7;
    }
    else if (strncasecmp(string_val, "SPARC-S7",
			 PICL_PROPNAMELEN_MAX) == 0) {
      dss_chip_mode = MODE_S7;
    }
    else if (strncasecmp(string_val, "SPARC-M8",
			 PICL_PROPNAMELEN_MAX) == 0) {
      dss_chip_mode = MODE_M8;
    }
  } else if (index == INDEX_PROCESSOR_TYPE) {  /* ProcessorType */
      strncpy(&dss_chip_type[0], string_val, PICL_PROPNAMELEN_MAX);
  } else if (index == INDEX_BRAND_STRING) { /* brand-string */
      strncpy(&dss_chip_model[0], string_val, PICL_PROPNAMELEN_MAX);
  }
}

/*****************************************************************************
Gets called by probe_cpu.  Cycles through the table values until we find
what we are looking for.
*****************************************************************************/
static void search_table(int index, picl_prophdl_t table_hdl) {

  picl_prophdl_t  col_hdl;
  picl_prophdl_t  row_hdl;
  picl_propinfo_t p_info;
  int             val;
  char            string_val[PICL_PROPNAMELEN_MAX];

  for (val = picl_get_next_by_col(table_hdl, &row_hdl); val != PICL_ENDOFLIST;
       val = picl_get_next_by_col(row_hdl, &row_hdl)) {
    if (val == PICL_SUCCESS) {
      for (col_hdl = row_hdl; val != PICL_ENDOFLIST;
           val = picl_get_next_by_row(col_hdl, &col_hdl)) {
        if (val == PICL_SUCCESS) {
          val = picl_get_propinfo(col_hdl, &p_info);
          if (val == PICL_SUCCESS) {
            if (p_info.type == PICL_PTYPE_CHARSTRING) {
              val = picl_get_propval(col_hdl, &string_val, sizeof(string_val));
              if (val == PICL_SUCCESS) {
                assign_string_value(index, string_val);
              }
            }
          }
        }
      }
    }
  }
}

/*****************************************************************************
Gets called by picl_walk_tree_by_class.  Then it cycles through the properties
until we find what we are looking for.  Once we are done, we return
PICL_WALK_TERMINATE to stop picl_walk_tree_by_class from traversing the tree.

Note that PICL_PTYPE_UNSIGNED_INT and PICL_PTYPE_INT can either be 4-bytes
or 8-bytes.
*****************************************************************************/
static int probe_cpu(picl_nodehdl_t node_hdl, void* dummy_arg __hwloc_attribute_unused) {

  picl_prophdl_t  p_hdl;
  picl_prophdl_t  table_hdl;
  picl_propinfo_t p_info;
  long long       long_long_val;
  unsigned int    uint_val;
  unsigned int    index;
  int             int_val;
  int             val;
  char            string_val[PICL_PROPNAMELEN_MAX];

  val = picl_get_first_prop(node_hdl, &p_hdl);
  while (val == PICL_SUCCESS) {
    called_cpu_probe = 1;
    val = picl_get_propinfo(p_hdl, &p_info);
    if (val == PICL_SUCCESS) {
      for (index = 0; index < NUM_ITEMS; index++) {
        if (strcasecmp(p_info.name, items[index]) == 0) {
          if (p_info.type == PICL_PTYPE_UNSIGNED_INT) {
            if (p_info.size == sizeof(uint_val)) {
              val = picl_get_propval(p_hdl, &uint_val, sizeof(uint_val));
              if (val == PICL_SUCCESS) {
                long_long_val = uint_val;
                assign_value(index, long_long_val);
              }
            }
            else if (p_info.size == sizeof(long_long_val)) {
              val = picl_get_propval(p_hdl, &long_long_val,
                                     sizeof(long_long_val));
              if (val == PICL_SUCCESS) {
                assign_value(index, long_long_val);
              }
            }
          }
          else if (p_info.type == PICL_PTYPE_INT) {
            if (p_info.size == sizeof(int_val)) {
              val = picl_get_propval(p_hdl, &int_val, sizeof(int_val));
              if (val == PICL_SUCCESS) {
                long_long_val = int_val;
                assign_value(index, long_long_val);
              }
            }
            else if (p_info.size == sizeof(long_long_val)) {
              val = picl_get_propval(p_hdl, &long_long_val,
                                     sizeof(long_long_val));
              if (val == PICL_SUCCESS) {
                assign_value(index, long_long_val);
              }
            }
          }
          else if (p_info.type == PICL_PTYPE_CHARSTRING) {
            val = picl_get_propval(p_hdl, &string_val, sizeof(string_val));
            if (val == PICL_SUCCESS) {
              assign_string_value(index, string_val);
            }
          }
          else if (p_info.type == PICL_PTYPE_TABLE) {
            val = picl_get_propval(p_hdl, &table_hdl, p_info.size);
            if (val == PICL_SUCCESS) {
              search_table(index, table_hdl);
            }
          }
          break;
	}
      }
    }

    val = picl_get_next_prop(p_hdl, &p_hdl);
  }
  return PICL_WALK_TERMINATE;
}


/*****************************************************************************
Initializes, gets the root, then walks the picl tree looking for information

Currently, the "core" class is only needed for OPL systems
*****************************************************************************/

static void probe_picl(void)
{
  char *env;
  int ret;

  memset(&chip_info, 0, sizeof(chip_info));

  /* if we ever see a heterogeneous platform, we'll need to parse PICL attributes for each CPU
   * (which means returning PICL_WALK_CONTINUE instead of PICL_WALK_TERMINATE above)
   * instead of using the first CPU PICL attributes for the entire machine.
   *
   * Use this env var to disable the PICL homogeneous-only parsing in the meantime.
   */
  env = getenv("HWLOC_PICL_HETEROGENEOUS");
  if (env && atoi(env))
    return;

  ret = picl_initialize();
  if (ret == PICL_SUCCESS) {
    picl_nodehdl_t root;
    ret = picl_get_root(&root);
    if (ret == PICL_SUCCESS) {
      ret = picl_walk_tree_by_class(root, "cpu", (void *)NULL, probe_cpu);
      ret = picl_walk_tree_by_class(root, "core", (void *)NULL, probe_cpu);
    }
    picl_shutdown();
  }

  if (called_cpu_probe) {
#if (defined HWLOC_X86_64_ARCH) || (defined HWLOC_X86_32_ARCH)
    /* PICL returns some corrupted chip_type strings on x86,
     * and CPUType only used on Sparc anyway, at least for now.
     * So we just ignore this attribute on x86. */
    dss_chip_type[0] = '\0';
#endif
    if (dss_chip_mode != MODE_UNKNOWN) { /* SPARC chip */
      strncpy(dss_chip_model, sparc_modes[dss_chip_mode],
	      PICL_PROPNAMELEN_MAX);
    }
  } else {
    /* no picl information on machine available */
    sysinfo(SI_HW_PROVIDER, dss_chip_type, PICL_PROPNAMELEN_MAX);
    sysinfo(SI_PLATFORM, dss_chip_model, PICL_PROPNAMELEN_MAX);
  }

#ifdef HWLOC_DEBUG
  {
    unsigned i;
    for(i=0; i<sizeof(chip_info.cache_size)/sizeof(*chip_info.cache_size); i++)
      hwloc_debug("PICL gave cache #%u size %lu line %u associativity %u\n",
		  i, chip_info.cache_size[i], chip_info.cache_linesize[i], chip_info.cache_associativity[i]);
  }
#endif

  /* make sure strings are null-terminated */
  dss_chip_type[sizeof(dss_chip_type)-1] = '\0';
  dss_chip_model[sizeof(dss_chip_model)-1] = '\0';

  /* setup the info struct */
  if (dss_chip_type[0])
    chip_info.type = dss_chip_type;
  if (dss_chip_model[0])
    chip_info.model = dss_chip_model;
}

void hwloc_solaris_get_chip_info(struct hwloc_solaris_chip_info_s *info)
{
  static int probe_done = 0;
  if (!probe_done) {
    probe_picl();
    probe_done = 1;
  }

  memcpy(info, &chip_info, sizeof(*info));
}

#else /* !HAVE_PICL_H */
void hwloc_solaris_get_chip_info(struct hwloc_solaris_chip_info_s *info)
{
  memset(info, 0, sizeof(*info));
}
#endif /* !HAVE_PICL_H */
