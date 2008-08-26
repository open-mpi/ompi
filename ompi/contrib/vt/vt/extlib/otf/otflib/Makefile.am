INCLUDES = $(ZLIB_INCLUDE_LINE)

if AMDEPZLIB
LIBS += $(ZLIB_LIB_LINE)
endif

include_HEADERS = otf.h \
	OTF_CopyHandler.h \
	OTF_Definitions.h \
	OTF_File.h \
	OTF_FileManager.h \
	OTF_Filenames.h \
	OTF_HandlerArray.h \
	OTF_inttypes.h \
	OTF_MasterControl.h \
	OTF_RBuffer.h \
	OTF_RStream.h \
	OTF_Reader.h \
	OTF_WBuffer.h \
	OTF_WStream.h \
	OTF_Writer.h

nodist_include_HEADERS = OTF_inttypes_unix.h

noinst_HEADERS = OTF_Keywords.h \
	         OTF_Parse.h \
	         OTF_Platform.h \
	         OTF_Platform_unix.h

lib_LTLIBRARIES = libotf.la

libotf_la_SOURCES = \
	OTF_CopyHandler.c \
	OTF_File.c \
	OTF_FileManager.c \
	OTF_Filenames.c \
	OTF_HandlerArray.c \
	OTF_MasterControl.c \
	OTF_Parse.c \
	OTF_Platform.c \
	OTF_RBuffer.c \
	OTF_Reader.c \
	OTF_RStream.c \
	OTF_WBuffer.c \
	OTF_Writer.c \
	OTF_WStream.c \
	OTF_inttypes.c

EXTRA_DIST = \
	OTF_Platform_win.h \
	OTF_inttypes_win.h \
	libotf_vs05.vcproj


.PHONY: doc
doc:
	doxygen
