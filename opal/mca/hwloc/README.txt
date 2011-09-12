12 Sep 2011

Notes for hwloc component maintainers:

1. There can only be *1* hwloc version component at a time.
   Specifically: if there are multiple hwlocXYZ components (i.e.,
   different versions of hwloc), then they must all be .ompi_ignore'd
   except for 1.  This is because we currently m4_include all of the
   underlying hwloc's .m4 files -- if there are multiple hwlocXYZ
   components, I don't know if m4 will barf at the multiple,
   conflicting AC_DEFUNs, or whether it'll just do something
   completely undefined.

1a. As a consequence, if you're adding a new hwloc version component,
   you'll need to .ompi_ignore all others while you're testing the new
   one. 

2. If someone wants to fix #1 someday, we might be able to do what we
   do for libevent: OMPI_CONFIG_SUBDIR (instead of slurping in hwloc's
   .m4 files).
