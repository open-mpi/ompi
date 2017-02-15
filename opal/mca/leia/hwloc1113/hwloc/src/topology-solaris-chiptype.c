/*
 * Copyright © 2009-2010 Oracle and/or its affiliates.  All rights reserved.
 * Copyright © 2013 Université Bordeaux.  All rights reserved.
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
#include <stdlib.h>
#include <strings.h>

#ifdef HAVE_PICL_H
#include <sys/systeminfo.h>
#include <picl.h>

/*****************************************************************************
   Order of this list is important for the assign_value and
   assign_string_value routines
*****************************************************************************/

static const char* items[] = {
  "clock-frequency",
  "cpu-mhz",
  "ecache-size",
  "l2-cache-size",
  "sectored-l2-cache-size",
  "implementation#",
  "manufacturer#",
  "compatible",
  "ProcessorType",
  "vendor-id",
  "brand-string"
};

#define NUM_ITEMS (sizeof(items) / sizeof(items[0]))

/*****************************************************************************
SPARC strings for chip modes and implementation
*****************************************************************************/
static const char* sparc_modes[] = {
    "UNKNOWN",
    "SPITFIRE",
    "BLACKBIRD",
    "CHEETAH",
    "SPARC64_VI",
    "T1",
    "T2",
    "SPARC64_VII",
    "ROCK"
};

/*****************************************************************************
Default values are for Unknown so we can build up from there.
*****************************************************************************/

static long dss_chip_mode         = MODE_UNKNOWN;
static long dss_chip_impl         = IMPL_SPITFIRE;
static long dss_chip_cache        = TWO_MEG_CACHE;
static long dss_chip_manufacturer = TI_MANUFACTURER;
static long long dss_chip_speed   = SPITFIRE_SPEED;
static char dss_chip_type[PICL_PROPNAMELEN_MAX];
static char dss_chip_model[PICL_PROPNAMELEN_MAX];
static int  called_cpu_probe      = 0;

/*****************************************************************************
Assigns values based on the value of index.  For this reason, the order of
the items array is important.
*****************************************************************************/
static void assign_value(int index, long long val) {
  if (index == 0) {  /* clock-frequency */
    dss_chip_speed = val;
  }
  if (index == 1) {  /* cpu-mhz */
    dss_chip_speed = val * 1000000; /* Scale since value was in MHz */
  }
  else if ((index >= 2) && (index <= 4)) {
    /* ecache-size, l2-cache-size, sectored-l2-cache-size */
    dss_chip_cache = val;
  }
  else if (index == 5) {
    /* implementation#  T1, T2, and Rock do not have this, see RFE 6615268 */
    dss_chip_impl = val;
    if (dss_chip_impl == IMPL_SPITFIRE) {
      dss_chip_mode = 1;
    }
    else if ((dss_chip_impl >= IMPL_BLACKBIRD) &&
             (dss_chip_impl <= IMPL_HUMMINGBIRD)) {
      dss_chip_mode = 2;
    }
    else if ((dss_chip_impl >= IMPL_CHEETAH) &&
             (dss_chip_impl <= IMPL_PANTHER)) {
      dss_chip_mode = 3;
    }
    else if (dss_chip_impl == IMPL_SPARC64_VI) {
      dss_chip_mode = 4;
    }
    else if (dss_chip_impl == IMPL_NIAGARA) {
      dss_chip_mode = 5;
    }
    else if (dss_chip_impl == IMPL_NIAGARA_2) {
      dss_chip_mode = 6;
    }
    else if (dss_chip_impl == IMPL_SPARC64_VII) {
      dss_chip_mode = 7;
    }
    else if (dss_chip_impl == IMPL_ROCK) {
      dss_chip_mode = 8;
    }
  }
  else if (index == 6) { /* manufacturer# */
    dss_chip_manufacturer = val;
  }
}

/*****************************************************************************
Assigns values based on the value of index.  For this reason, the order of
the items array is important.
*****************************************************************************/
static void assign_string_value(int index, char* string_val) {
  if (index == 7) { /* compatible */
    if (strncasecmp(string_val, "FJSV,SPARC64-VI",
                    PICL_PROPNAMELEN_MAX) == 0) {
      dss_chip_mode = 4;
    }
    else if (strncasecmp(string_val, "SUNW,UltraSPARC-T1",
                         PICL_PROPNAMELEN_MAX) == 0) {
      dss_chip_mode = 5;
    }
    else if (strncasecmp(string_val, "SUNW,UltraSPARC-T2",
                         PICL_PROPNAMELEN_MAX) == 0) {
      dss_chip_mode = 6;
    }
    else if (strncasecmp(string_val, "FJSV,SPARC64-VII",
                         PICL_PROPNAMELEN_MAX) == 0) {
      dss_chip_mode = 7;
    }
    else if (strncasecmp(string_val, "SUNW,Rock",
                         PICL_PROPNAMELEN_MAX) == 0) {
      dss_chip_mode = 8;
    }
  } else if (index == 8) {  /* ProcessorType */
      strncpy(&dss_chip_type[0], string_val, PICL_PROPNAMELEN_MAX);
  } else if (index == 10) { /* brand-string */
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
        } else if (index == NUM_ITEMS-1) {
	  if (p_info.type == PICL_PTYPE_CHARSTRING) {
            val = picl_get_propval(p_hdl, &string_val, sizeof(string_val));
            if (val == PICL_SUCCESS) {
            }
	  }
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
char* hwloc_solaris_get_chip_type(void) {
  picl_nodehdl_t root;
  int            val;
  static char chip_type[PICL_PROPNAMELEN_MAX];

  val = picl_initialize();
  if (val != PICL_SUCCESS) { /* Can't initialize session with PICL daemon */
      return(NULL);
  }
  val = picl_get_root(&root);
  if (val != PICL_SUCCESS) {  /* Failed to get root node of the PICL tree */
      return(NULL);
  }
  val = picl_walk_tree_by_class(root, "cpu", (void *)NULL, probe_cpu);
  val = picl_walk_tree_by_class(root, "core", (void *)NULL, probe_cpu);
  picl_shutdown();

  if (called_cpu_probe) {
#if (defined HWLOC_X86_64_ARCH) || (defined HWLOC_X86_32_ARCH)
      /* PICL returns some corrupted chip_type strings on x86,
       * and CPUType only used on Sparc anyway, at least for now.
       * So we just ignore this attribute on x86. */
#else
      strncpy(chip_type, dss_chip_type, PICL_PROPNAMELEN_MAX);
#endif
  } else {
      /* no picl information on machine available */
      sysinfo(SI_HW_PROVIDER, chip_type, PICL_PROPNAMELEN_MAX);
  }
  return(chip_type);
}

/*****************************************************************************
Initializes, gets the root, then walks the picl tree looking for information

Currently, the "core" class is only needed for OPL systems
*****************************************************************************/
char *hwloc_solaris_get_chip_model(void) {

    if (called_cpu_probe) {
	if (dss_chip_mode != MODE_UNKNOWN) { /* SPARC chip */
	    strncpy(dss_chip_model, sparc_modes[dss_chip_mode],
		    PICL_PROPNAMELEN_MAX);
	}
    } else {
	/* no picl information on machine available */
	sysinfo(SI_PLATFORM, dss_chip_model, PICL_PROPNAMELEN_MAX);
    }
    return(dss_chip_model);
}

#else
char* hwloc_solaris_get_chip_type(void) {
  return NULL;
}
char *hwloc_solaris_get_chip_model(void) {
  return NULL;
}
#endif
