#
# Copyright (c) 2014-2017 Intel, Inc. All rights reserved.
# Copyright (c) 2014      Artem Y. Polyakov <artpol84@gmail.com>.
#                         All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

headers += \
        src/client/pmix_client_ops.h

sources += \
        src/client/pmix_client.c \
        src/client/pmix_client_fence.c \
        src/client/pmix_client_get.c \
        src/client/pmix_client_pub.c \
        src/client/pmix_client_spawn.c \
        src/client/pmix_client_connect.c

if WANT_PMIX_BACKWARD
sources += \
        src/client/pmi1.c \
        src/client/pmi2.c
endif
