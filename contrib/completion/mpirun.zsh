#compdef mpirun orterun mpiexec oshrun # -*- shell-script -*-

# Completion script for Open MPI's mpirun command v1.0.1
#
# Copyright (c) 2013-2014 Los Alamos National Security, LLC. All rights
#                         reserved.
#
# If completion is not already set up, follow the instructions in section
# 20.2 of the zsh completion documentation found here:
#   http://zsh.sourceforge.net/Doc/Release/Completion-System.html
#
# Rename this to _mpirun a place it into any directory in your $fpath. If
# needed, you can add another directory to $fpath (I use ~/.zsh/completion)
# by adding the following line to ~/.zshrc before the call to compinit:
#
#      fpath=(/path/to/completion/scripts $fpath)

local mca_variable_names mca_variable_values mca_component_names

_generate_mca_variable_names_zsh() {
    local excl=$1 TMP_IFS=$IFS

    IFS="
"
    mca_variable_names=($(ompi_info -a --parsable | perl -e '
my %values, %help, @uniq;

while (<>) {
  if (/^mca:/) {
    chop;
    @tmp = split(":");
    $values{$tmp[4]} = 1;
    if ($tmp[5] eq "help") {
      $help{$tmp[4]} = $tmp[6];
      $help{$tmp[4]} =~ s/:/\\:/g;
    }
  }
}

for $exclude (split(" ", "'"$excl"'")) {
  delete $values{$exclude};
}

for $key (sort(keys %values)) {
  print $key;
  if ($help{$key} ne "") {
    print ":$help{$key}";
  }

  print "\n";
}' 2> /dev/null))
    IFS=$TMP_IFS
}

_generate_mca_variable_values_zsh() {
    mca_variable_values=($(ompi_info -a --parsable | grep ":$1:" | perl -e '
my @values;

while (<>) {
  chop;
  @tmp = split(":");

  if ($tmp[5] eq "enumerator") {
    $values[++$#values] = $tmp[7];
    $values[++$#values] = $tmp[8];
  }
}
print join(" ", @values);' 2> /dev/null))
}

# given a framework (type) name print a list of components
_generate_mca_component_names() {
    # components are printed by default (-a is not needed)
    mca_component_names=($(ompi_info --parsable | grep "mca:$1:" | perl -e '
my %values;

while (<>) {
  if (/^mca:/) {
    @tmp = split(":");
    $values{$tmp[2]} = 1;
  }
}

@uniq = keys %values;
print join(" ", sort(@uniq));'))
}

# remove items from $1 that are also in $2. lists must be sorted
_set_remove () {
    comm -23 <(echo $1 | sort | tr " " "\n") <(echo $2 | sort | tr " " "\n") 2>/dev/null
}

# Complete colon separated paths
_complete_path() {
    compset -P '*:'
    compset -S ':*'
    _default -r '\-\n\t /:' "$@"
}

# Complete comma separated paths
_complete_path_comma() {
    compset -P '*,'
    compset -S ',*'
    _default -r '\-\n\t /,' "$@"
}

# Complete files for --report-uri and --report-pid
_report_file() {
    local expl fds
    fds=('-:stdout' '+:stderr')
    # Complete files
    _files "$@"
    # Complete special files (+, -)
    _describe -t fds "file descriptors" fds
}

# find mca parameters specified on the command line (prevent duplicates)
_find_mca_parameters() {
    for ((i = 1 ; i < CURRENT ; i++)) ; do
	# mca parameters appear after an -mca, --mca, -gmca, or --gmca option
	if test ${words[$i]##*[-g]} = "mca" -a $i -lt $((CURRENT-1)) ; then
	    echo ${words[$i+1]}
	fi
    done
}

_mpirun() {
    local state curcontext="$curcontext" expl cur parameter available_components already_specified values ret=1
    typeset -A opt_args

    setopt localoptions extendedglob

    _arguments -a \
	'(-am --am)'{-am,--am}'[Aggregate MCA parameter set file list]:mca variable set file list:_complete_path' \
	'(- *)'{-app,--app}'[Provide an appfile; ignore all other command line options]:app file:_files' \
	'(-bind-to --bind-to)'{-bind-to,--bind-to}'[Policy for binding processes (default: none)]:binding policy:(none hwthread core socket numa board)' \
	'(-bind-to --bind-to --bind-to-socket -bind-to-socket --bind-to-core -bind-to-core)'{-bind-to-core,--bind-to-core}'[Bind processes to cores]' \
	'(-bind-to --bind-to --bind-to-socket -bind-to-socket --bind-to-core -bind-to-core)'{-bind-to-socket,--bind-to-socket}'[Bind processes to sockets]' \
	'(-bynode --bynode -byslot --byslot)'{-bynode,--bynode}'[Whether to map and rank processes round-robin by node]' \
	'(-bynode --bynode -byslot --byslot)'{-byslot,--byslot}'[Whether to map and rank processes round-robin by slot]' \
	'*'{-c,-n,--n,-np,--np}'[Number of processes to run]:Number of processes:_guard "[[\:digit\:]]#" "process count"' \
	'(-cf --cartofile)'{-cf,--cartofile}'[Provide a cartography file]:Carto file:_files' \
	'(-cpu-set --cpu-set)'{-cpu-set,--cpu-set}'[Comma-separated list of ranges specifying logical cpus allocated to this job (default: none)]:CPU Set:' \
	'(-cpus-per-rank --cpus-per-rank -cpus-per-proc --cpus-per-proc)'{-cpus-per-proc,--cpus-per-proc,-cpus-per-rank,--cpus-per-rank}'[Number of cpus to use for each process (default: 1)]:CPUs per proc:_guard "[[\:digit\:]]#" "number"' \
	'(-d -debug-level --debug-level)'{-d,-debug-devel,--debug-devel}'[Enable debugging of OpenRTE]' \
	'(-debug --debug -tv --tv)'{-debug,--debug,-tv,--tv}'[Invoke the user-level debugger indicated by the orte_base_user_debugger MCA parameter]' \
	'(-debug-daemons --debug-daemons -debug-daemons-file --debug-daemons-file)'{-debug-daemons,--debug-daemons}'[Enable debugging of any OpenRTE daemons used by this application]' \
	'(-debug-daemons --debug-daemons -debug-daemons-file --debug-daemons-file)'{-debug-daemons-file,--debug-daemons-file}'[Enable debugging of any OpenRTE daemons used by this application, storing output in files]' \
	'(-debugger --debugger)'{-debugger,--debugger}'[Sequence of debuggers to search for when "--debug" is used]:Debuggers:' \
	'(-default-hostfile --default-hostfile)'{-default-hostfile,--default-hostfile}'[Provide a default hostfile]:Default hostfile:_files' \
	'(-disable-recovery --disable-recovery)'{-disable-recovery,--disable-recovery}'[Disable recovery (resets all recovery options to off)]' \
	'(-display-allocation --display-allocation -display-devel-allocation --display-devel-allocation)'{-display-allocation,--display-allocation}'[Display the allocation being used by this job]' \
	'(-display-allocation --display-allocation -display-devel-allocation --display-devel-allocation)'{-display-devel-allocation,--display-devel-allocation}'[Display a detailed list (mostly intended for developers) of the allocation being used by this job]' \
	'(-display-devel-map --display-devel-map)'{-display-devel-map,--display-devel-map}'[Display a detailed process map (mostly intended for developers) just before launch]' \
	'(-display-diffable-map --display-diffable-map -display-map --display-map)'{-display-diffable-map,--display-diffable-map}'[Display a diffable process map (mostly intended for developers) just before launch]' \
	'(-display-map --display-map -display-diffable-map --display-diffable-map)'{-display-map,--display-map}'[Display the process map just before launch]' \
	'(-display-topo --display-topo)'{-display-topo,--display-topo}'[Display the topology as part of the process map (mostly intended for developers) just before launch]' \
	'(-do-not-launch --do-not-launch)'{-do-not-launch,--do-not-launch}'[Perform all necessary operations to prepare to launch the application, but do not actually launch it]' \
	'(-do-not-resolve --do-not-resolve)'{-do-not-resolve,--do-not-resolve}'[Do not attempt to resolve interfaces]' \
	'(-enable-recovery --enable-recovery)'{-enable-recovery,--enable-recovery}'[Enable recovery from process failure (default: disabled)]' \
	'*'{-gmca,--gmca}'[Pass global MCA parameters that are applicable to all contexts (arg0 is the parameter name; arg1 is the parameter value)]:mca variable name:->mca_variable_name:mca variable value:->mca_variable_value' \
	'(- *)'{-h,--help}'[Help message]' \
	'*'{-H,-host,--host}'[List of hosts to invoke processes on]:hostnames:' \
	'(-hetero-apps --hetero-apps)'{-hetero-apps,--hetero-apps}'[Indicates that multiple app_contexts are being provided that are a mix of 32/64 bit binaries]' \
	'(-hetero-nodes --hetero-nodes)'{-hetero-nodes,--hetero-nodes}'[Nodes in cluster may differ in topology, so send the topology back from each node (default: false)]' \
	'(-index-argv-by-rank --index-argv-by-rank)'{-index-argv-by-rank,--index-argv-by-rank}'[Uniquely index argv\[0\] for each process using its rank]' \
	{-launch-agent,--launch-agent}'[Command used to start processes on remote nodes (default: orted)]:launch agent:_files' \
	'(-leave-session-attached --leave-session-attached)'{-leave-session-attached,--leave-session-attached}'[Enable debugging of OpenRTE]' \
	'(-hostfile --hostfile -machinefile --machinefile)'{-hostfile,--hostfile,-machinefile,--machinefile}'[Provide a hostfile]:hostfile:_files' \
	'(-map-by --map-by)'{-map-by,--map-by}'[Mapping Policy (default: slot)]:mapping:(slot hwthread core socket numa board node)' \
	'(-max-restarts --max-restarts)'{-max-restarts,--max-restarts}'[Max number of times to restart a failed process]:max restarts:_guard "[[\:digit\:]]#" "number"' \
	'(-max-vm-size --max-vm-size)'{-max-vm-size,--max-vm-size}'[Number of processes to run]:max vm size:_guard "[[\:digit\:]]#" "number"' \
	'*'{-mca,--mca}'[Pass context-specific MCA parameters; they are considered global if --gmca is not used and only one context is specified (arg0 is the parameter name; arg1 is the parameter value)]:mca variable name:->mca_variable_name:mca variable value:->mca_variable_value' \
	'(-nolocal --nolocal)'{-nolocal,--nolocal}'[Do not run any MPI applications on the local node]' \
	'(-oversubscribe --oversubscribe -nooversubscribe --nooversubscribe)'{-nooversubscribe,--nooversubscribe}'[Nodes are not to be oversubscribed, even if the system supports such operation]' \
	'(-noprefix --noprefix)'{-noprefix,--noprefix}'[Disable automatic --prefix behavior]' \
	'(-novm --novm)'{-novm,--novm}'[Execute without creating an allocation-spanning virtual machine (only start daemons on nodes hosting application procs)]' \
	'(-N -npernode --npernode -npersocket --npersocket)'{-N,-npernode,--npernode}'[Launch n processes per node on all allocated nodes]:npernode:_guard "[[\:digit\:]]#" "number"' \
	'(-npernode --npernode -npersocket --npersocket)'{-npersocket,--npersocket}'[Launch n processes per socket on all allocated nodes]:npersocket:_guard "[[\:digit\:]]#" "number"' \
	'(-ompi-server --ompi-server)'{-ompi-server,--ompi-server}'[Specify the URI of the Open MPI server, or the name of the file (specified as file:filename) that contains that info]:server uri:' \
	'(-output-filename --output-filename)'{-output-filename,--output-filename}'[Redirect output from application processes into filename.rank]:filename:' \
	'(-output-proctable --output-proctable)'{-output-proctable,--output-proctable}'[Output the debugger proctable after launch]' \
	'(-oversubscribe --oversubscribe -nooversubscribe --nooversubscribe)'{-oversubscribe,--oversubscribe}'[Nodes are allowed to be oversubscribed, even on a managed system]' \
	'(-path --path)'{-path,--path}'[PATH to be used to look for executables to start processes]:path:_files -/' \
	'(-bynode --bynode -pernode --pernode)'{-pernode,--pernode}'[Launch one process per available node]' \
	'(-ppr --ppr)'{-ppr,--ppr}'[Comma-separated list of number of processes on a given resource type (default: none)]:ppr:' \
	'(-prefix --prefix)'{-prefix,--prefix}'[Prefix where Open MPI is installed on remote nodes]:ompi prefix:_files -/' \
	'(-preload-files --preload-files)'{-preload-files,--preload-files}'[Preload the comma separated list of files to the remote machines current working directory before starting the remote process.]:preload files:_complete_path_comma' \
	'(-q --quiet)'{-q,--quiet}'[Suppress helpful messages]' \
	'(-rank-by --rank-by)'{-rank-by,--rank-by}'[Ranking Policy (default: slot)]:ranking policy:(slot hwthread core socket numa board node)' \
	'(-report-bindings --report-bindings)'{-report-bindings,--report-bindings}'[Whether to report process bindings to stderr]' \
	'(-report-child-jobs-separately --report-child-jobs-separately)'{-report-child-jobs-separately,--report-child-jobs-separately}'[Return the exit status of the primary job only]' \
	'(-report-events --report-events)'{-report-events,--report-events}'[Report events to a tool listening at the specified URI]:URI:' \
	'(-report-pid --report-pid)'{-report-pid,--report-pid}'[Printout pid on stdout (-), stderr (+), or a file (anything else)]:report file:_report_file' \
	'(-report-uri --report-uri)'{-report-uri,--report-uri}'[Printout URI on stdout (-), stderr (+), or a file (anything else)]:report file:_report_file' \
	'(-rf --rankfile)'{-rf,--rankfile}'[Provide a rankfile file]:rank file:_files' \
	'(-s --preload-binary)'{-s,--preload-binary}'[Preload the binary on the remote machine before starting the remote process.]' \
	'(-server-wait-time --server-wait-time)'{-server-wait-time,--server-wait-time}'[Time in seconds to wait for ompi-server (default: 10 sec)]:time in sec:' \
	'(-wd --wd -wdir --wdir -set-cwd-to-session-dir --set-cwd-to-session-dir)'{-set-cwd-to-session-dir,--set-cwd-to-session-dir}'[Set the working directory of the started processes to their session directory]' \
	'(-show-progress --show-progress)'{-show-progress,--show-progress}'[Output a brief periodic report on launch progress]' \
	'(-slot-list --slot-list)'{-slot-list,--slot-list}'[List of processor IDs to bind processes to (default: NULL)]:slot list:]' \
	'(-staged --staged)'{-staged,--staged}'[Used staged execution if inadequate resources are present (cannot support MPI jobs)]' \
	'(-stdin --stdin)'{-stdin,--stdin}'[Specify procs to receive stdin \[rank, all, none\] (default: 0, indicating rank 0)]:rank list:' \
	'(-tag-output --tag-output)'{-tag-output,--tag-output}'[Tag all output with \[job,rank\]]' \
	'(-timestamp-output --timestamp-output)'{-timestamp-output,--timestamp-output}'[Timestamp all application process output]' \
	'(-use-hwthread-cpus --use-hwthread-cpus)'{-use-hwthread-cpus,--use-hwthread-cpus}'[Use hardware threads as independent cpus]' \
	'(-use-regexp --use-regexp)'{-use-regexp,--use-regexp}'[Use regular expressions for launch]' \
	'(-v --verbose)'{-v,--verbose}'[Be verbose]' \
	'(- *)'{-V,--version}'[Print version and exit]' \
	'(-wait-for-server --wait-for-server)'{-wait-for-server,--wait-for-server}'[If ompi-server is not already running, wait until it is detected (default: false)]' \
	'(-wd --wd -wdir --wdir -set-cwd-to-session-dir --set-cwd-to-session-dir)'{-wdir,--wdir,-wd,--wd}'[Set the working directory of the started processes]:working directory:_files -/' \
	'*-x[Export an environment variable, optionally specifying a value (e.g., "-x foo" exports the environment variable foo and takes its value from the current environment; "-x foo=bar" exports the environment variable name foo and sets its value to "bar" in the started processes)]:environment variable:compadd ${(k)parameters[(R)*export*]}' \
	'(-xml --xml -xml-file --xml-file)'{-xml,--xml}'[Provide all output in XML format]' \
	'(-xml --xml -xml-file --xml-file)'{-xml-file,--xml-file}'[Provide all output in XML format to the specified file]:xml output file:' \
	'(-xterm --xterm)'{-xterm,--xterm}'[Create a new xterm window and display output from the specified ranks there]:rank list:' \
	'*::args:_normal' && ret=0

    case $state in
	mca_variable_name)
	    already_specified=($(_find_mca_parameters))
	    _generate_mca_variable_names_zsh "${already_specified[*]}"
	    _describe -t mca_variable_names 'mca variable name' mca_variable_names
	    ;;
        mca_variable_value)
	    cur=${words[CURRENT]}
	    parameter=${words[CURRENT - 1]}
	    if test "${parameter#_}" = "${parameter}" ; then
		_generate_mca_component_names $parameter
		available_components=($(_set_remove "${mca_component_names[*]}" "$(echo $cur | tr ',' '\n')"))
		compset -P '*,'
		compset -S ',*'
		_describe -t available_components 'available components' available_components
	    else
		_generate_mca_variable_values_zsh $parameter
		_describe -t mca_variable_values 'mca variable value' mca_variable_values
	    fi

	    ;;
    esac

    return ret
}

_mpirun "$@"
