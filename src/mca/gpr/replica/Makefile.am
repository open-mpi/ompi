#
# $HEADER$
#

# Use the top-level Makefile.options

include $(top_ompi_srcdir)/config/Makefile.options

sources = \
        gpr_replica_component.c \
        gpr_replica_recv_proxy_msgs.c \
        gpr_replica_xmit_alerts.c \
        gpr_replica.h \
        gpr_replica_del_index.c \
        gpr_replica_put_get.c \
        gpr_replica_dump.c \
        gpr_replica_cleanup.c \
        gpr_replica_subscribe.c \
        gpr_replica_compound_cmd.c \
        gpr_replica_synchro.c \
        gpr_replica_mode_ops.c \
        gpr_replica_messaging.c \
        gpr_replica_internals_dict_ops.c \
        gpr_replica_internals_segment_ops.c \
        gpr_replica_internals_trigger_ops.c \
        gpr_replica_test_internals.c \
        gpr_replica_internals.h

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if OMPI_BUILD_gpr_replica_DSO
component_noinst =
component_install = mca_gpr_replica.la
else
component_noinst = libmca_gpr_replica.la
component_install =
endif

mcacomponentdir = $(libdir)/openmpi
mcacomponent_LTLIBRARIES = $(component_install)
mca_gpr_replica_la_SOURCES = $(sources)
mca_gpr_replica_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_gpr_replica_la_SOURCES =$(sources)
libmca_gpr_replica_la_LDFLAGS = -module -avoid-version
