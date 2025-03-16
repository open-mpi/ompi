#!/bin/bash # -*- Mode: shell-script; indent-tabs-mode:nil -*-
# (c) 2008-2013 Nathan Hjelm <hjelmn@cs.unm.edu>
# mpirun completion v1.1
#
# Bash completion script for Open MPI's mpirun
#
# Installation:
#   If bash completion is not already installed, follow the instructions at:
#      http://anonscm.debian.org/gitweb/?p=bash-completion/bash-completion.git;a=blob_plain;f=README
#
# Then install mpirun completion by dropping this file into either the system bash_completion.d
# (usually found in /etc) or somewhere in your home directory (~/.bash_completion.d is a good place).
# If installing the script in a location other than the system-wide location, you will probably need
# to add the following to either ~/.bash_profile or ~/.bash_completion:
#
#        [ -r /path/to/mpirun.sh ] && . /path/to/mpirun.sh

# find available MCA variables
_get_mca_variable_names() {
    ompi_info -a --parsable | perl -e '
my %values, %help, @uniq;

$maxlen = 0;

while (<>) {
  if (/^mca:/) {
    chop;
    @tmp = split(":");
    $length = length($tmp[4]);
    $maxlen = ($length, $maxlen)[$length < $maxlen];
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
    print " " x ($maxlen - length($key)) . "- $help{$key}";
  }

  print "\n";
}' 2> /dev/null
}

# given a framework (type) name print a list of components
_get_mca_component_names() {
    # components are printed by default (-a is not needed)
    ompi_info -l 9 --parsable | grep "mca:$1:" | perl -e '
my %values;

while (<>) {
  if (/^mca:/) {
    @tmp = split(":");
    $values{$tmp[2]} = 1;
  }
}

@uniq = keys %values;
print join(" ", sort(@uniq));'
}

_get_mpirun_switches() {
    mpirun --help 2>&1 | sed 's/^\s*//g' | egrep '^-' | cut -d' ' -f1 | tr '|\n' ' '
}

# get enumerator values for a variable
_get_enum_values() {
    ompi_info -a -l 9 --parsable | grep ":$1:" | perl -e '
my @values;

while (<>) {
  chop;
  @tmp = split(":");

  if ($tmp[5] eq "enumerator") {
    $values[++$#values] = $tmp[7];
    $values[++$#values] = $tmp[8];
  }
}
print join(" ", @values);'
}

# remove items from $1 that are also in $2. lists must be sorted
_set_remove () {
    comm -23 <(echo $1 | sort | tr " " "\n") <(echo $2 | sort | tr " " "\n") 2>/dev/null
}

# find mca parameters specified on the command line (prevent duplicates)
_find_mca_parameters() {
    for ((i = 1; i < COMP_CWORD; i++)) ; do
        # the subcommand is the first item on the command line that isn't module or a switch
        if test ${COMP_WORDS[$i]##*-} == "mca" -a $i -lt $((COMP_CWORD-1)) ; then
            echo ${COMP_WORDS[$i+1]}
        fi
    done
}

# check for matches that have the search term somewhere in the name (but not the description)
_fuzzy_search() {
    for match in $2 ; do
        if [[ ${match%%[[:space:]]*} =~ .*"$1".* ]] ; then
            echo $match
        fi
    done
}

# mpirun/orterun completion
_mpirun() {
    local cur prv tb switches already_specified all_variables avail_variables enumerations save_IFS
    local disable_description=yes

    # Enable descriptions if we can detect the completion type (so we can strip the description off
    # if there is only one result. This is the case for normal completion (ASCII 9) or successive
    # completion (?: ASCII 63)
    if test "$COMP_TYPE" = "9" -o "$COMP_TYPE" = "63" ; then
        disable_description=no
    fi

    COMPREPLY=()
    if test $COMP_CWORD -gt 1 ; then
        tb=${COMP_WORDS[COMP_CWORD-2]}
    else
        tb=""
    fi

    if test $COMP_CWORD -gt 0 ; then
        prv=${COMP_WORDS[COMP_CWORD-1]}
    else
        prv=""
    fi

    cur=${COMP_WORDS[COMP_CWORD]}

    if test "${prv##*-}" = "mca" ; then
        # Complete variable name

        # Temporarily change IFS to newline
        save_IFS=$IFS
        IFS="
"

        # Remove mca parameters already on the command line
        avail_variables=($(_get_mca_variable_names "$(_find_mca_parameters)"))

	IFS=" "
        # Check if we should disable descriptions
        if test "$disable_description" = "yes" ; then
            avail_variables=(${avail_variables[*]%%[[:space:]]*})
        fi
	IFS="
"

        # Return a fuzzy-search of the mca parameter names
        COMPREPLY=($(_fuzzy_search "$cur" "${avail_variables[*]}"))

        # Remove the description if this is the last item
        if test ${#COMPREPLY[@]} -eq 1 ; then
            completion=${COMPREPLY[0]}
            COMPREPLY=("${COMPREPLY[0]%%[[:space:]]*}")
        fi
        IFS=$save_IFS
    elif test "${tb##*-}" = "mca" ; then
        # Complete variable value

        # Check if the variable is a selection variable (no _ in the name)
        if test "${prv#_}" = "${prv}" ; then
            # component selection variable, find available components (removing ones already selected)
            enumerations=($(_set_remove "$(_get_mca_component_names $prv)" "$(echo $cur | tr ',' '\n')"))

            # Prepend the current selection if there is one
            if test "${cur%,*}" = "${cur}" ; then
                COMPREPLY=($(_fuzzy_search "${cur##*,}" "${enumerations[*]}"))
            else
                COMPREPLY=($(_fuzzy_search "${cur##*,}" "${enumerations[*]}" | sed "s/^/${cur%,*},/g"))
            fi
        else
            enumerations=($(_get_enum_values "$prv"))
            COMPREPLY=($(_fuzzy_search "$cur" "${enumerations[*]}"))
        fi
    elif test "${prv}" = "--bind-to" ; then
        COMPREPLY=($(compgen -W "none hwthread core socket numa board" -- "$cur"))
    elif test "${prv}" = "--map-by" -o "${prv}" = "-map-by" ; then
        COMPREPLY=($(compgen -W "slot hwthread core socket numa board node" -- "$cur"))
    elif test "${prv##*-}" = "hostfile" ; then
        COMPREPLY=($(compgen -f -- "$cur"))
    elif test "${cur:0:1}" = "-" ; then
        switches=$(_get_mpirun_switches)
        COMPREPLY=($(compgen -W "$switches" -- "$cur"))
    else
        COMPREPLY=($(compgen -f -- "$cur"))
    fi

    return 0
}

complete -o nospace -F _mpirun mpirun
complete -o nospace -F _mpirun orterun
complete -o nospace -F _mpirun mpiexec
