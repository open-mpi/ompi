noinst_LTLIBRARIES =

if MCA_BUILD_ompi_op_has_avx2_support
noinst_LTLIBRARIES += libfoo.la
nodist_libfoo_la_SOURCES = foo.c
foo.c:
	cd .. && env HOMEBREW_ARCHFLAGS=@MCA_BUILD_OP_AVX2_FLAGS@ make liblocal_ops_avx2.la
	touch foo.c
endif

if MCA_BUILD_ompi_op_has_avx512_support
noinst_LTLIBRARIES += libbar.la
nodist_libbar_la_SOURCES = bar.c
bar.c:
	cd .. && env HOMEBREW_ARCHFLAGS=@MCA_BUILD_OP_AVX512_FLAGS@ make liblocal_ops_avx512.la
	touch bar.c
endif

clean-local:
	rm -f foo.c bar.c
