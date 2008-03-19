apstat -a `apstat -r | awk '{if ($1!=A && $1=='$BATCH_PARTITION_ID') { print $2}}'` -r -v | egrep  "(nid [0-9]+)" -o | awk '{print $2}'
