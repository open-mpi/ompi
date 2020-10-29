#!/bin/sh
#
# Copyright (c) 2017      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved.

# Provide a way to override build date for reproducible build results
# See https://reproducible-builds.org/ for why this is good.

# There are several different flavors of date(1) out there.
# Try a few different CLI options for date(1) to see which one works.

SOURCE_DATE_EPOCH="${SOURCE_DATE_EPOCH:-$(date +%s)}"
date -u -d "@$SOURCE_DATE_EPOCH" "$@" 2>/dev/null || date -u -r "$SOURCE_DATE_EPOCH" "$@" 2>/dev/null || date -u "$@"
