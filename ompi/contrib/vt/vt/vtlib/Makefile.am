if AMBUILDMPI
VTMPILIBRARY = libvt-mpi.la
else
VTMPILIBRARY =
endif

if AMBUILDMT
VTMTLIBRARY = libvt-mt.la
else
VTMTLIBRARY =
endif

if AMBUILDHYBRID
VTHYBLIBRARY = libvt-hyb.la
else
VTHYBLIBRARY =
endif

if AMBUILDJAVA
VTJAVALIBRARY = libvt-java.la
else
VTJAVALIBRARY =
endif

if AMBUILDOPARI
VTPOMPLIBRARY = libvt-pomp.la
else
VTPOMPLIBRARY =
endif

if AMBUILDFMPIWRAPLIB
VTFMPILIBRARY = libvt-fmpi.la
else
VTFMPILIBRARY =
endif

lib_LTLIBRARIES = \
	libvt.la \
	$(VTMPILIBRARY) \
	$(VTMTLIBRARY) \
	$(VTHYBLIBRARY) \
	$(VTJAVALIBRARY) \
	$(VTPOMPLIBRARY) \
	$(VTFMPILIBRARY)

if AMEXTERNOTF
OTFCFLAGS = $(OTFINCDIR)
OTFLIBRARY = $(OTFLIBDIR) $(OTFLIB)
OTFDEPENDENCIES =
else
OTFCFLAGS = -I$(top_srcdir)/extlib/otf/otflib -I$(top_builddir)/extlib/otf/otflib
OTFLIBRARY = $(top_builddir)/extlib/otf/otflib/libotf.la
OTFDEPENDENCIES = $(OTFLIBRARY)
endif

if AMHAVEPTHREAD
PTHREADSOURCES = vt_pthreadreg.c vt_pthreadwrap.c
PTHREADCFLAGS = -DVT_PTHREAD
PTHREADHEADERS = vt_wrap_pthread.h
else
PTHREADSOURCES =
PTHREADCFLAGS =
PTHREADHEADERS =
endif

if AMHAVEOMP
OMPSOURCES = vt_ompreg.c
OMPCFLAGS = -DVT_OMP
else
OMPSOURCES =
OMPCFLAGS =
endif

if AMHAVETHREADS
if AMTHRDPTHREAD
THRDSOURCES = vt_thrd_pthread.c 
THRDCFLAGS = -DVT_THRD_PTHREAD $(PTHREAD_CFLAGS)
endif
if AMTHRDOMP
THRDSOURCES = vt_thrd_omp.c
THRDCFLAGS = -DVT_THRD_OMP $(OPENMP_CFLAGS)
endif
else
THRDSOURCES =
THRDCFLAGS =
endif

if AMHAVEMPI2CONST
FMPI2CONSTSOURCES = vt_fmpiconst_2.f
else
FMPI2CONSTSOURCES =
endif

if AMHAVEMPI2IO
MPI2IOSOURCES = vt_mpifile.c
else
MPI2IOSOURCES =
endif

if AMBUILDETIMESYNC
ETIMESYNCSOURCES = vt_esync.c
ETIMESYNCCFLAGS = -DVT_ETIMESYNC
else
ETIMESYNCSOURCES =
ETIMESYNCCFLAGS =
endif

if AMHAVEUNIMCI
UNIMCISOURCES = vt_unimci.c
UNIMCICFLAGS = -DVT_UNIMCI $(UNIMCI_CFLAGS)
else
UNIMCISOURCES =
UNIMCICFLAGS =
endif

if AMBUILDLIBWRAP
LIBWRAPSOURCES = vt_libwrap.c
LIBWRAPHEADERS = vt_libwrap.h
LIBWRAPCFLAGS = -DVT_LIBWRAP
else
LIBWRAPSOURCES =
LIBWRAPHEADERS =
LIBWRAPCFLAGS =
endif

if AMBUILDLIBCWRAP
LIBCWRAPSOURCES = vt_libcwrap.c
LIBCWRAPCFLAGS = -DVT_LIBCWRAP
else
LIBCWRAPSOURCES =
LIBCWRAPCFLAGS =
endif

if AMHAVEFORK
FORKSOURCES = vt_fork.c
else
FORKSOURCES =
endif

if AMBUILDIOWRAP
IOWRAPSOURCES = vt_iowrap.c vt_iowrap_helper.c
IOWRAPCFLAGS = -DVT_IOWRAP
else
IOWRAPSOURCES =
IOWRAPCFLAGS =
endif

if AMHAVEMEMHOOKS
MEMHOOKSOURCES = vt_memhook.c
MEMHOOKCFLAGS = -DVT_MEMHOOK
else
MEMHOOKSOURCES =
MEMHOOKCFLAGS =
endif

if AMHAVEGETCPU
GETCPUSOURCES = vt_getcpu.c
GETCPUCFLAGS = -DVT_GETCPU
else
GETCPUSOURCES =
GETCPUCFLAGS =
endif

if AMHAVERUSAGE
RUSAGESOURCES = vt_rusage.c
RUSAGECFLAGS = -DVT_RUSAGE
else
RUSAGESOURCES =
RUSAGECFLAGS =
endif

if AMHAVEMETRICS
if AMHAVEPAPI
METRSOURCES = vt_metric_papi.c
METRCFLAGS = -DVT_METR=\"PAPI\" $(PAPIINCDIR)
METRLIBRARY = $(PAPILIBDIR) $(PAPILIB)
endif
if AMHAVECPC
METRSOURCES = vt_metric_cpc.c
METRCFLAGS = -DVT_METR=\"CPC\" $(CPCINCDIR)
METRLIBRARY = $(CPCLIBDIR) $(CPCLIB)
endif
if AMHAVENECSXCNTR
METRSOURCES = vt_metric_necsx.c vt_metric_necsx_1.s
METRCFLAGS = -DVT_METR=\"NECSX\"
METRLIBRARY =
endif
else
METRSOURCES =
METRCFLAGS =
METRLIBRARY =
endif

if AMHAVECOMPINST
if AMBUILDCOMPINST_GNU
COMPINSTSOURCES = vt_comp_gnu.c
endif
if AMBUILDCOMPINST_PGI
COMPINSTSOURCES = vt_comp_pgi.c
endif
if AMBUILDCOMPINST_PHAT
COMPINSTSOURCES = vt_comp_phat.c
endif
if AMBUILDCOMPINST_XL
COMPINSTSOURCES = vt_comp_xl.c
endif
if AMBUILDCOMPINST_FTRACE
COMPINSTSOURCES = vt_comp_ftrace.c vt_comp_ftrace_1.s
endif
if AMBUILDCOMPINST_OPENUH
COMPINSTSOURCES = vt_comp_openuh.c
endif
else
COMPINSTSOURCES =
endif

if AMHAVEDYNINST
DYNINSTSOURCES = vt_dyninst.c
else
DYNINSTSOURCES =
endif

if AMHAVEDL
DLCFLAGS = $(DLINCDIR)
DLLIBRARY = $(DLLIBDIR) $(DLLIB)
else
DLCFLAGS =
DLLIBRARY =
endif

if AMHAVEMATH
MATHLIBRARY = $(MATHLIB)
else
MATHLIBRARY =
endif

include_HEADERS = \
	vt_user.h \
	vt_user.inc \
	vt_user_comment.h \
	vt_user_comment.inc \
	vt_user_control.h \
	vt_user_control.inc \
	vt_user_count.h \
	vt_user_count.inc \
	vt_user_marker.h \
	vt_user_marker.inc \
	vt_user_region.h \
	vt_user_region.inc \
	$(PTHREADHEADERS) \
	$(LIBWRAPHEADERS)

VT_COMMON_SOURCES = \
	rfg.h \
	rfg_filter.h \
	rfg_groups.h \
	rfg_regions.h \
	vt_comp.h \
	vt_defs.h \
	vt_env.h \
	vt_error.h \
	vt_esync.h \
	vt_fbindings.h \
	vt_fork.h \
	vt_getcpu.h \
	vt_inttypes.h \
	vt_iowrap.h \
	vt_iowrap_helper.h \
	vt_java.h \
	vt_libcwrap.h \
	vt_memhook.h \
	vt_metric.h \
	vt_mpicom.h \
	vt_mpifile.h \
	vt_mpireg.h \
	vt_mpireq.h \
	vt_ompreg.h \
	vt_otf_gen.h \
	vt_otf_sum.h \
	vt_pform.h \
	vt_pthreadreg.h \
	vt_rusage.h \
	vt_sync.h \
	vt_thrd.h \
	vt_trc.h \
	vt_unimci.h \
	$(COMPINSTSOURCES) \
	$(DYNINSTSOURCES) \
	$(LIBWRAPSOURCES) \
	$(LIBCWRAPSOURCES) \
	$(IOWRAPSOURCES) \
	$(MEMHOOKSOURCES) \
	$(GETCPUSOURCES) \
	$(RUSAGESOURCES) \
	$(METRSOURCES) \
	rfg_filter.c \
	rfg_groups.c \
	rfg_regions.c \
	vt_env.c \
	vt_error.c \
	vt_fbindings.c \
	vt_otf_gen.c \
	vt_otf_sum.c \
	vt_otf_trc.c \
	vt_pform_@PLATFORM@.c \
	vt_thrd.c \
	vt_user_comment.c \
	vt_user_control.c \
	vt_user_count.c \
	vt_user_marker.c \
	vt_user_region.c

VT_SEQ_SOURCES = \
	$(FORKSOURCES)

VT_MPI_SOURCES = \
	vt_mpicom.c \
	vt_mpireg.c \
	vt_mpireq.c \
	vt_mpiwrap.c \
	vt_sync.c \
	$(MPI2IOSOURCES) \
	$(ETIMESYNCSOURCES) \
	$(UNIMCISOURCES)

VT_MT_SOURCES = \
	$(THRDSOURCES) \
	$(PTHREADSOURCES) \
	$(OMPSOURCES)

VT_HYB_SOURCES = \
	$(VT_MPI_SOURCES) \
	$(VT_MT_SOURCES)

VT_JAVA_SOURCES = \
	vt_java.c \
	vt_thrd_java.c

VT_POMP_SOURCES = \
	vt_pomp.h \
	vt_pomp.c \
	vt_fpomp.c

VT_FMPI_SOURCES = \
	vt_fmpiconst.h \
	vt_fmpiconst.c \
	vt_fmpiwrap.c \
	vt_fmpiconst_1.f \
	$(FMPI2CONSTSOURCES)

CC = $(MPICC)
AM_CFLAGS = $(OTFCFLAGS) $(DLCFLAGS) $(LIBWRAPCFLAGS) $(LIBCWRAPCFLAGS) $(IOWRAPCFLAGS) $(MEMHOOKCFLAGS) $(GETCPUCFLAGS) $(RUSAGECFLAGS) $(METRCFLAGS)

libvt_la_LIBADD = $(top_builddir)/util/libutil.la $(OTFLIBRARY) $(METRLIBRARY) $(DLLIBRARY) $(MATHLIBRARY)
libvt_la_DEPENDENCIES = $(top_builddir)/util/libutil.la $(OTFDEPENDENCIES)
libvt_la_SOURCES = $(VT_COMMON_SOURCES) $(VT_SEQ_SOURCES)

libvt_mpi_la_CFLAGS = -DVT_MPI $(AM_CFLAGS) $(MPICFLAGS) $(MPIINCDIR) $(ETIMESYNCCFLAGS) $(UNIMCICFLAGS)
libvt_mpi_la_LIBADD = $(top_builddir)/util/libutil.la $(OTFLIBRARY) $(METRLIBRARY) $(DLLIBRARY) $(MATHLIBRARY)
libvt_mpi_la_DEPENDENCIES = $(top_builddir)/util/libutil.la $(OTFDEPENDENCIES)
libvt_mpi_la_SOURCES = $(VT_COMMON_SOURCES) $(VT_MPI_SOURCES)

libvt_mt_la_CFLAGS = -DVT_MT $(AM_CFLAGS) $(PTHREADCFLAGS) $(OMPCFLAGS) $(THRDCFLAGS)
libvt_mt_la_LIBADD = $(top_builddir)/util/libutil.la $(OTFLIBRARY) $(METRLIBRARY) $(DLLIBRARY) $(MATHLIBRARY)
libvt_mt_la_DEPENDENCIES = $(top_builddir)/util/libutil.la $(OTFDEPENDENCIES)
libvt_mt_la_SOURCES = $(VT_COMMON_SOURCES) $(VT_MT_SOURCES)

libvt_hyb_la_CFLAGS = -DVT_HYB $(AM_CFLAGS) $(MPICFLAGS) $(MPIINCDIR) $(PTHREADCFLAGS) $(OMPCFLAGS) $(THRDCFLAGS) $(ETIMESYNCCFLAGS) $(UNIMCICFLAGS)
libvt_hyb_la_LIBADD = $(top_builddir)/util/libutil.la $(OTFLIBRARY) $(METRLIBRARY) $(DLLIBRARY) $(MATHLIBRARY)
libvt_hyb_la_DEPENDENCIES = $(top_builddir)/util/libutil.la $(OTFDEPENDENCIES)
libvt_hyb_la_SOURCES = $(VT_COMMON_SOURCES) $(VT_HYB_SOURCES)

libvt_java_la_CFLAGS = -DVT_JAVA $(AM_CFLAGS) $(JVMTIINCDIR)
libvt_java_la_LDFLAGS = -shared
libvt_java_la_LIBADD = $(top_builddir)/util/libutil.la $(OTFLIBRARY) $(METRLIBRARY)
libvt_java_la_DEPENDENCIES = $(top_builddir)/util/libutil.la $(OTFDEPENDENCIES)
libvt_java_la_SOURCES = $(VT_COMMON_SOURCES) $(VT_JAVA_SOURCES)

libvt_pomp_la_CFLAGS = -static -prefer-pic $(AM_CFLAGS) $(OPENMP_CFLAGS) -I$(top_srcdir)/tools/opari/lib
libvt_pomp_la_LDFLAGS = -static
libvt_pomp_la_SOURCES = $(VT_POMP_SOURCES)

F77 = $(MPIF77)
libvt_fmpi_la_CFLAGS = $(libvt_mpi_la_CFLAGS)
libvt_fmpi_la_FFLAGS = $(MPIFFLAGS) $(FMPIINCDIR)
libvt_fmpi_la_SOURCES = $(VT_FMPI_SOURCES)

EXTRA_DIST = \
	vt_pform_altix.c \
	vt_pform_bgl.c \
	vt_pform_bgp.c \
	vt_pform_crayt3e.c \
	vt_pform_crayx1.c \
	vt_pform_crayxt.c \
	vt_pform_generic.c \
	vt_pform_ibm.c \
	vt_pform_linux.c \
	vt_pform_macos.c \
	vt_pform_necsx.c \
	vt_pform_origin.c \
	vt_pform_sicortex.c \
	vt_pform_sun.c

DISTCLEANFILES = \
	vt_mpireg.gen.c \
	vt_mpireg.gen.h \
	vt_mpiwrap.gen.c \
	vt_fmpiwrap.gen.c

