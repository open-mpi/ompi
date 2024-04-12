#!/bin/bash
#
# Copyright (c) 2020      Intel, Inc.  All rights reserved.
# Copyright (c) 2020      IBM Corporation.  All rights reserved.

docker exec -it -u prteuser -w  /home/prteuser/ --env COLUMNS=`tput cols` --env LINES=`tput lines` $USER-node00 bash
