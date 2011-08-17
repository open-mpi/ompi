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
if AMMACOS
install-exec-hook:
	(cd $(DESTDIR)$(libdir); \
	rm -f libvt-java.jnilib; \
	$(LN_S) libvt-java$(SHREXT) libvt-java.jnilib)
endif
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

if AMBUILDMPIUNIFYLIB
VTMPIUNIFYLIBSOURCES = 
VTMPIUNIFYLIBCFLAGS = -DVT_MPIUNIFYLIB -I$(top_srcdir)/tools/vtunify/mpi -I$(top_builddir)/tools/vtunify/mpi
VTMPIUNIFYLIBLIBRARY = $(top_builddir)/tools/vtunify/mpi/libvt-mpi-unify.la
VTMPIUNIFYLIBDEPENDENCIES = $(VTMPIUNIFYLIBLIBRARY)
$(VTMPIUNIFYLIBLIBRARY):
	@($(am__cd) $(top_builddir)/tools/vtunify/mpi && $(MAKE) $(AM_MAKEFLAGS) libvt-mpi-unify.la)
install-exec-hook-pre:
	@($(am__cd) $(top_builddir)/tools/vtunify/mpi && $(MAKE) $(AM_MAKEFLAGS) install)
install: install-exec-hook-pre
	@$(MAKE) $(AM_MAKEFLAGS) install-am
else
VTMPIUNIFYLIBCFLAGS =
VTMPIUNIFYLIBLIBRARY =
VTMPIUNIFYLIBDEPENDENCIES =
endif

if AMHAVEPTHREAD
PTHREADSOURCES = vt_pthreadreg.c vt_pthreadwrap.c
PTHREADCFLAGS = -DVT_PTHREAD
else
PTHREADSOURCES =
PTHREADCFLAGS =
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
LIBWRAPCFLAGS = -DVT_LIBWRAP
else
LIBWRAPSOURCES =
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

GPUSOURCES =
GPUHEADERS = vt_gpu.h
GPUCFLAGS =
GPULIBRARY =

if AMBUILDCUDAWRAP
#GPUSOURCES += vt_cudawrap.c vt_cuda.c
GPUHEADERS += vt_cuda_driver_api.h
GPUCFLAGS += -DVT_CUDAWRAP $(CUDATKINCDIR)
endif

if AMBUILDCUDARTWRAP
GPUSOURCES += vt_cudartwrap.c vt_cudart.c vt_gpu.c
GPUHEADERS += vt_cudartwrap.h vt_cuda_runtime_api.h
GPUCFLAGS += -DVT_CUDARTWRAP $(CUDATKINCDIR)
if AMBUILDCUPTI
if AMBUILDCUDAWRAP
GPUSOURCES += vt_cudacupti.c
GPUHEADERS += vt_cudacupti.h vt_cupti_events.h
GPULIBRARY += $(CUPTILIBDIR) $(CUPTILIB)
GPUCFLAGS += -DVT_CUDACUPTI $(CUPTIINCDIR)
endif
endif
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

if AMHAVEPLUGINCNTR
PLUGINCNTRSOURCES = vt_plugin_cntr.c
PLUGINCNTRCFLAGS = -DVT_PLUGIN_CNTR
else
PLUGINCNTRSOURCES =
PLUGINCNTRCFLAGS =
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

VT_COMMON_SOURCES = \
	vt_comp.h \
	$(GPUHEADERS) \
	vt_defs.h \
	vt_env.h \
	vt_error.h \
	vt_esync.h \
	vt_fbindings.h \
	vt_fork.h \
	vt_getcpu.h \
	vt_gpu.h \
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
	vt_plugin_cntr_int.h \
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
	$(GPUSOURCES) \
	$(MEMHOOKSOURCES) \
	$(GETCPUSOURCES) \
	$(PLUGINCNTRSOURCES) \
	$(RUSAGESOURCES) \
	$(METRSOURCES) \
	vt_env.c \
	vt_error.c \
	vt_fbindings.c \
	vt_otf_gen.c \
	vt_otf_sum.c \
	vt_pform_@PLATFORM@.c \
	vt_thrd.c \
	vt_trc.c \
	vt_user_comment.c \
	vt_user_control.c \
	vt_user_count.c \
	vt_user_marker.c \
	vt_user_message.c \
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
	vt_jvmti.h \
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
AM_CFLAGS = -I$(top_srcdir)/include -I$(top_builddir)/include -I$(top_srcdir)/rfg $(OTFCFLAGS) $(DLCFLAGS) $(LIBWRAPCFLAGS) $(GPUCFLAGS) $(LIBCWRAPCFLAGS) $(IOWRAPCFLAGS) $(MEMHOOKCFLAGS) $(GETCPUCFLAGS) $(RUSAGECFLAGS) $(METRCFLAGS) $(PLUGINCNTRCFLAGS)

libvt_la_LIBADD = $(top_builddir)/util/libvt_util.la $(top_builddir)/rfg/libvt_rfg.la $(OTFLIBRARY) $(METRLIBRARY) $(GPULIBRARY) $(DLLIBRARY) $(MATHLIBRARY)
libvt_la_DEPENDENCIES = $(top_builddir)/util/libvt_util.la $(top_builddir)/rfg/libvt_rfg.la $(OTFDEPENDENCIES)
libvt_la_SOURCES = $(VT_COMMON_SOURCES) $(VT_SEQ_SOURCES)

libvt_mpi_la_CFLAGS = -DVT_MPI $(AM_CFLAGS) $(MPICFLAGS) $(MPIINCDIR) $(VTMPIUNIFYLIBCFLAGS) $(ETIMESYNCCFLAGS) $(UNIMCICFLAGS)
libvt_mpi_la_LIBADD = $(top_builddir)/util/libvt_util.la $(top_builddir)/rfg/libvt_rfg.la $(OTFLIBRARY) $(VTMPIUNIFYLIBLIBRARY) $(MPILIBDIR) $(PMPILIB) $(METRLIBRARY) $(GPULIBRARY) $(DLLIBRARY) $(MATHLIBRARY)
libvt_mpi_la_DEPENDENCIES = $(top_builddir)/util/libvt_util.la $(top_builddir)/rfg/libvt_rfg.la $(OTFDEPENDENCIES) $(VTMPIUNIFYLIBDEPENDENCIES)
libvt_mpi_la_SOURCES = $(VT_COMMON_SOURCES) $(VT_MPI_SOURCES)

libvt_mt_la_CFLAGS = -DVT_MT $(AM_CFLAGS) $(PTHREADCFLAGS) $(OMPCFLAGS) $(THRDCFLAGS)
libvt_mt_la_LIBADD = $(top_builddir)/util/libvt_util.la $(top_builddir)/rfg/libvt_rfg.la $(OTFLIBRARY) $(METRLIBRARY) $(GPULIBRARY) $(DLLIBRARY) $(MATHLIBRARY)
libvt_mt_la_DEPENDENCIES = $(top_builddir)/util/libvt_util.la $(top_builddir)/rfg/libvt_rfg.la $(OTFDEPENDENCIES)
libvt_mt_la_SOURCES = $(VT_COMMON_SOURCES) $(VT_MT_SOURCES)

libvt_hyb_la_CFLAGS = -DVT_HYB $(AM_CFLAGS) $(MPICFLAGS) $(MPIINCDIR) $(VTMPIUNIFYLIBCFLAGS) $(PTHREADCFLAGS) $(OMPCFLAGS) $(THRDCFLAGS) $(ETIMESYNCCFLAGS) $(UNIMCICFLAGS)
libvt_hyb_la_LIBADD = $(top_builddir)/util/libvt_util.la $(top_builddir)/rfg/libvt_rfg.la $(OTFLIBRARY) $(VTMPIUNIFYLIBLIBRARY) $(MPILIBDIR) $(PMPILIB) $(METRLIBRARY) $(GPULIBRARY) $(DLLIBRARY) $(MATHLIBRARY)
libvt_hyb_la_DEPENDENCIES = $(top_builddir)/util/libvt_util.la $(top_builddir)/rfg/libvt_rfg.la $(OTFDEPENDENCIES) $(VTMPIUNIFYLIBDEPENDENCIES)
libvt_hyb_la_SOURCES = $(VT_COMMON_SOURCES) $(VT_HYB_SOURCES)

libvt_java_la_CFLAGS = -DVT_JAVA $(AM_CFLAGS) $(JVMTIINCDIR)
libvt_java_la_LDFLAGS = -shared
libvt_java_la_LIBADD = $(top_builddir)/util/libvt_util.la $(top_builddir)/rfg/libvt_rfg.la $(OTFLIBRARY) $(METRLIBRARY) $(GPULIBRARY)
libvt_java_la_DEPENDENCIES = $(top_builddir)/util/libvt_util.la $(top_builddir)/rfg/libvt_rfg.la $(OTFDEPENDENCIES)
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
	vt_pform_crayxe.c \
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

