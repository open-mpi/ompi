#!/bin/sh
#
# Copyright (c) 2017      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved.

# Provide a way to override build date for reproducible build results
# See https://reproducible-builds.org/ for why this is good.

date --date="@${SOURCE_DATE_EPOCH:-$(date +%s)}" "$@"
