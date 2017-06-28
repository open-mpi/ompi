#!/bin/sh

# Copyright (c) 2017      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.

date --date="@${SOURCE_DATE_EPOCH:-$(date +%s)}"
