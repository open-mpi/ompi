if AMBUILDMPI
VTMPILIB = libvt.mpi.a libvt.fmpi.a
else
VTMPILIB =
endif

if AMBUILDOMP
VTOMPLIB = libvt.omp.a
else
VTOMPLIB =
endif

if AMBUILDHYB
VTHYBLIB = libvt.ompi.a
else
VTHYBLIB =
endif

lib_LIBRARIES = libvt.a $(VTMPILIB) $(VTOMPLIB) $(VTHYBLIB)

OPARILIBDIR = $(top_builddir)/tools/opari/lib
OPARIINCLUDE = -I$(top_srcdir)/tools/opari/lib

if AMEXTERNOTF
OTFINCLUDE = $(OTFINCDIR)
else
OTFINCLUDE = -I$(top_srcdir)/extlib/otf/otflib -I$(top_builddir)/extlib/otf/otflib
endif

if AMHAVEMEMHOOKS
MEMHOOKSOURCES = vt_memhook.c vt_memreg.c
MEMHOOKCFLAGS = -DVT_MEMHOOK
else
MEMHOOKSOURCES =
MEMHOOKCFLAGS =
endif

if AMBUILDIOWRAP
IOWRAPSOURCES = vt_iowrap.c vt_iowrap_helper.c
IOWRAPCFLAGS = -DVT_IOWRAP
else
IOWRAPSOURCES =
IOWRAPCFLAGS =
endif

if AMHAVEPAPI
METRINCLUDE = $(PAPIINCDIR)
METRSOURCES = vt_metric_papi.c
METRCFLAGS = -DVT_METR
else
METRINCLUDE =
METRSOURCES =
METRCFLAGS =
endif

if AMBUILDCOMPINST_GNU
COMPGNUSOURCES = vt_comp_gnu.c
else
COMPGNUSOURCES =
endif

if AMBUILDCOMPINST_INTEL
COMPINTELSOURCES = vt_comp_gnu.c
else
COMPINTELSOURCES =
endif

if AMBUILDCOMPINST_PGI
COMPPGISOURCES = vt_comp_pgi.c
else
COMPPGISOURCES =
endif

if AMBUILDCOMPINST_PHAT
COMPPHATSOURCES = vt_comp_phat.c
else
COMPPHATSOURCES =
endif

if AMBUILDCOMPINST_XL
COMPXLSOURCES = vt_comp_xl.c
else
COMPXLSOURCES =
endif

if AMBUILDCOMPINST_FTRACE
COMPFTRACESOURCES = vt_comp_ftrace.c vt_comp_ftrace_1.s
else
COMPFTRACESOURCES =
endif

COMPSOURCES = $(COMPGNUSOURCES) $(COMPINTELSOURCES) \
              $(COMPPGISOURCES) $(COMPPHATSOURCES) $(COMPXLSOURCES) \
              $(COMPHITACHISOURCES) $(COMPFTRACESOURCES)

if AMHAVEDYNINST
DYNISOURCES = vt_comp_dyninst.c
else
DYNISOURCES =
endif

INCLUDES = $(OPARIINCLUDE) $(OTFINCLUDE) $(METRINCLUDE)

include_HEADERS = \
	vt_user.h \
	vt_user.inc \
	vt_user_comment.h \
	vt_user_comment.inc \
	vt_user_count.h \
	vt_user_count.inc

RFGCFLAGS = -DRFG
RFGSOURCES = \
	rfg_regions.c \
	rfg_filter.c \
	rfg_groups.c
RFGHEADERS = \
	rfg.h \
	rfg_filter.h \
	rfg_groups.h \
	rfg_regions.h \
	rfg_strmkrs.h

VT_COMMON_SOURCES = \
	$(RFGHEADERS) \
	vt_comp.h \
	vt_defs.h \
	vt_env.h \
	vt_error.h \
	vt_fbindings.h \
	vt_inttypes.h \
	vt_iowrap.h \
	vt_iowrap_helper.h \
	vt_memhook.h \
	vt_memreg.h \
	vt_metric.h \
	vt_mpicom.h \
	vt_mpireg.h \
	vt_mpireq.h \
	vt_omplock.h \
	vt_ompreg.h \
	vt_otf_gen.h \
	vt_otf_sum.h \
	vt_pform.h \
	vt_pomp.h \
	vt_sync.h \
	vt_thrd.h \
	vt_trc.h \
	$(COMPSOURCES) \
	$(DYNISOURCES) \
	$(MEMHOOKSOURCES) \
	$(IOWRAPSOURCES) \
	$(METRSOURCES) \
	$(RFGSOURCES) \
	vt_env.c \
	vt_error.c \
	vt_metmap.c \
	vt_otf_gen.c \
	vt_otf_sum.c \
	vt_otf_trc.c \
	vt_pform_@PLATFORM@.c \
	vt_pomp_base.c \
	vt_thrd.c \
	vt_user_comment.c \
	vt_user_count.c \
	vt_user_region.c

VT_MPI_SOURCES = \
	vt_mpicom.c \
	vt_mpireg.c \
	vt_mpireq.c \
	vt_mpiwrap.c \
	vt_sync.c

VT_OMP_SOURCES = \
	vt_omplock.c \
	vt_ompreg.c \
	vt_pomp.c \
	$(OPARILIBDIR)/pomp_fwrapper.c

VT_HYB_SOURCES = \
	$(VT_MPI_SOURCES) \
	$(VT_OMP_SOURCES)

CC = $(MPICC)
AM_CFLAGS = $(RFGCFLAGS) $(MEMHOOKCFLAGS) $(IOWRAPCFLAGS) $(METRCFLAGS)

libvt_a_LIBADD = $(top_builddir)/util/util.$(OBJEXT) $(top_builddir)/util/installdirs.$(OBJEXT)
libvt_a_SOURCES = \
	$(VT_COMMON_SOURCES) \
	$(OPARILIBDIR)/pomp_fwrapper_base.c

libvt_mpi_a_CFLAGS = -DVT_MPI $(AM_CFLAGS) $(MPICFLAGS) $(MPIINCDIR)
libvt_mpi_a_LIBADD = pomp_fwrapper_base.$(OBJEXT) $(top_builddir)/util/util.$(OBJEXT) $(top_builddir)/util/installdirs.$(OBJEXT)
libvt_mpi_a_SOURCES = \
	$(VT_COMMON_SOURCES) \
	$(VT_MPI_SOURCES)

libvt_omp_a_CFLAGS = $(OMPFLAG) -DVT_OMP $(AM_CFLAGS)
libvt_omp_a_LIBADD = pomp_fwrapper_base.$(OBJEXT) $(top_builddir)/util/util.$(OBJEXT) $(top_builddir)/util/installdirs.$(OBJEXT)
libvt_omp_a_SOURCES = \
	$(VT_COMMON_SOURCES) \
	$(VT_OMP_SOURCES)

libvt_ompi_a_CFLAGS = $(OMPFLAG) -DVT_OMPI $(AM_CFLAGS) $(MPICFLAGS) $(MPIINCDIR)
libvt_ompi_a_LIBADD = pomp_fwrapper_base.$(OBJEXT) $(top_builddir)/util/util.$(OBJEXT) $(top_builddir)/util/installdirs.$(OBJEXT)
libvt_ompi_a_SOURCES = \
	$(VT_COMMON_SOURCES) \
	$(VT_HYB_SOURCES)

libvt_fmpi_a_CFLAGS = $(AM_CFLAGS) $(MPICFLAGS) $(MPIINCDIR)
libvt_fmpi_a_SOURCES = vt_fmpiwrap.c

EXTRA_DIST = \
	vt_pform_altix.c \
	vt_pform_bgl.c \
	vt_pform_crayt3e.c \
	vt_pform_crayx1.c \
	vt_pform_crayxt.c \
	vt_pform_generic.c \
	vt_pform_ibm.c \
	vt_pform_linux.c \
	vt_pform_macos.c \
	vt_pform_necsx.c \
	vt_pform_origin.c \
	vt_pform_sun.c

