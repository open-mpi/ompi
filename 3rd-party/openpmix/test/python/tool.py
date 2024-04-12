#!/usr/bin/env python3

from pmix import *
from os import *
import time

def main():
    foo = PMIxTool()
    print("Testing PMIx Tool ", foo.get_version())
    # construct a session tmpdir
    uid = os.geteuid()
    gid = os.getegid()
    basename = os.path.basename(__file__)
    tmpdir = os.getenv('TMPDIR')
    sessiondir = os.path.join(tmpdir, basename + '.session.' + str(uid) + '.' + str(gid))
    info = [{'key':PMIX_LAUNCHER, 'value':True, 'val_type':PMIX_BOOL},
            {'key':PMIX_SERVER_TOOL_SUPPORT, 'value':True, 'val_type':PMIX_BOOL},
            {'key':PMIX_SERVER_TMPDIR, 'value':sessiondir, 'val_type':PMIX_STRING}]
    (my_result, myproc) = foo.init(info)
    if 0 != my_result:
        print("FAILED TO INIT", foo.error_string(my_result))
        exit(1)

    # construct an application to spawn
    app = {'cmd':'hostname', 'maxprocs':1}
    foo.spawn(None, [app])
    time.sleep(2.0)
    # finalize
    foo.finalize()
    print("Tool finalize complete")
if __name__ == '__main__':
    main()
