ACLOCAL_AMFLAGS = -I config/m4

if AMHAVEPYTHONSWIG
OTFLIB_PY = otflib_py
else
OTFLIB_PY =
endif

SUBDIRS = \
	otflib \
	$(OTFLIB_PY) \
	tools \
	docu

EXTRA_DIST= \
	otf_vc08.sln \
	config/otf_get_version.sh \
	tests/hello/Makefile \
	tests/hello/hello_otf.c \
	tests/otf_python/groups.py \
	tests/otf_python/test_read.py \
	tests/otf_python/test_read_qt.py \
	tests/otf_python/test_write.py \
	VERSION
