#!/usr/bin/env python3

from pmix import *
import signal, time
import os
import select
import subprocess

global killer

class GracefulKiller:
  kill_now = False
  def __init__(self):
    signal.signal(signal.SIGINT, self.exit_gracefully)
    signal.signal(signal.SIGTERM, self.exit_gracefully)

  def exit_gracefully(self,signum, frame):
    self.kill_now = True

def clientconnected(proc:tuple is not None):
    print("CLIENT CONNECTED", proc)
    return PMIX_SUCCESS

def clientfinalized(proc:tuple is not None):
    print("CLIENT FINALIZED", proc)
    return PMIX_SUCCESS

def clientfence(args:dict is not None):
    print("SERVER FENCE", args)
    return PMIX_SUCCESS

def main():
    try:
        foo = PMIxServer()
    except:
        print("FAILED TO CREATE SERVER")
        exit(1)
    print("Testing server version ", foo.get_version())
    args = [{'key':PMIX_SERVER_SCHEDULER, 'value':'T', 'val_type':PMIX_BOOL}]
    map = {'clientconnected': clientconnected,
           'clientfinalized': clientfinalized,
           'fencenb': clientfence}
    my_result = foo.init(args, map)
    print("Testing PMIx_Initialized")
    rc = foo.initialized()
    print("Initialized: ", rc)
    vers = foo.get_version()
    print("Version: ", vers)

    # Register a fabric
    rc,fab = foo.fabric_register(None)
    print("Fabric registered: ", foo.error_string(rc))

    # setup the application
    (rc, regex) = foo.generate_regex(["test000","test001","test002"])
    print("Node regex, rc: ", regex, foo.error_string(rc))
    (rc, ppn) = foo.generate_ppn(["0,1,2", "3,4,5", "6,7"])
    print("PPN, rc: ", ppn, foo.error_string(rc))
    darray = {'type':PMIX_INFO, 'array':[{'key':PMIX_ALLOC_FABRIC_ID,
                            'value':'SIMPSCHED.net', 'val_type':PMIX_STRING},
                           {'key':PMIX_ALLOC_FABRIC_SEC_KEY, 'value':'T',
                            'val_type':PMIX_BOOL},
                           {'key':PMIX_SETUP_APP_ENVARS, 'value':'T',
                            'val_type':PMIX_BOOL}]}
    kyvals = [{'key':PMIX_NODE_MAP, 'value':regex, 'val_type':PMIX_STRING},
              {'key':PMIX_PROC_MAP, 'value':ppn, 'val_type':PMIX_STRING},
              {'key':PMIX_ALLOC_FABRIC, 'value':darray, 'val_type':PMIX_DATA_ARRAY}]

    appinfo = []
    rc, appinfo = foo.setup_application("SIMPSCHED", kyvals)
    print("SETUPAPP: ", appinfo)

    rc = foo.setup_local_support("SIMPSCHED", appinfo)
    print("SETUPLOCAL: ", foo.error_string(rc))

    # get our environment as a base
    env = os.environ.copy()

    # register an nspace for the client app
    kvals = [{'key':PMIX_NODE_MAP, 'value':regex, 'val_type':PMIX_STRING},
             {'key':PMIX_PROC_MAP, 'value':ppn, 'val_type':PMIX_STRING},
             {'key':PMIX_UNIV_SIZE, 'value':1, 'val_type':PMIX_UINT32},
             {'key':PMIX_JOB_SIZE, 'value':1, 'val_type':PMIX_UINT32}]
    print("REGISTERING NSPACE")
    rc = foo.register_nspace("testnspace", 1, kvals)
    print("RegNspace ", foo.error_string(rc))

    # register a client
    uid = os.getuid()
    gid = os.getgid()
    print("REGISTERING CLIENT")
    rc = foo.register_client({'nspace':"testnspace", 'rank':0}, uid, gid)
    print("RegClient ", foo.error_string(rc))

    # setup the fork
    rc = foo.setup_fork({'nspace':"testnspace", 'rank':0}, env)
    print("SetupFrk", foo.error_string(rc))

    # setup the client argv
    args = ["./client.py"]
    # open a subprocess with stdout and stderr
    # as distinct pipes so we can capture their
    # output as the process runs
    p = subprocess.Popen(args, env=env,
        stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    # define storage to catch the output
    stdout = []
    stderr = []
    # loop until the pipes close
    while True:
        reads = [p.stdout.fileno(), p.stderr.fileno()]
        ret = select.select(reads, [], [])

        stdout_done = True
        stderr_done = True

        for fd in ret[0]:
            # if the data
            if fd == p.stdout.fileno():
                read = p.stdout.readline()
                if read:
                    read = read.decode('utf-8').rstrip()
                    print('stdout: ' + read)
                    stdout_done = False
            elif fd == p.stderr.fileno():
                read = p.stderr.readline()
                if read:
                    read = read.decode('utf-8').rstrip()
                    print('stderr: ' + read)
                    stderr_done = False

        if stdout_done and stderr_done:
            break

    print("FINALIZING")
    foo.finalize()

if __name__ == '__main__':
    global killer
    killer = GracefulKiller()
    main()
