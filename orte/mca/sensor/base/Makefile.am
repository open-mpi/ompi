#
# Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved. 
#
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

headers += \
        base/base.h

libmca_sensor_la_SOURCES += \
        base/sensor_base_open.c

if !ORTE_DISABLE_FULL_SUPPORT

headers += \
        base/sensor_private.h

libmca_sensor_la_SOURCES += \
        base/sensor_base_close.c \
        base/sensor_base_select.c \
        base/sensor_base_scale.c

endif
