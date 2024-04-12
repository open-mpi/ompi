#!/bin/bash -xeE
export PATH=/hpc/local/bin::/usr/local/bin:/bin:/usr/bin:/usr/sbin:${PATH}

rel_path=$(dirname $0)
abs_path=$(readlink -f $rel_path)

jenkins_test_build=${jenkins_test_build:="yes"}
jenkins_test_check=${jenkins_test_check:="yes"}
jenkins_test_src_rpm=${jenkins_test_src_rpm:="yes"}
jenkins_test_cov=${jenkins_test_cov:="yes"}
jenkins_test_comments=${jenkins_test_comments:="no"}
jenkins_test_vg=${jenkins_test_vg:="no"}

timeout_exe=${timout_exe:="timeout -s SIGKILL 1m"}

# prepare to run from command line w/o jenkins
if [ -z "$WORKSPACE" ]; then
    WORKSPACE=$PWD
    JOB_URL=$WORKSPACE
    BUILD_NUMBER=1
    JENKINS_RUN_TESTS=yes
    NOJENKINS=${NOJENKINS:="yes"}
fi

prefix=jenkins
rm -rf ${WORKSPACE}/${prefix}
mkdir -p ${WORKSPACE}/${prefix}
pmix_dir=${WORKSPACE}/${prefix}/install
build_dir=${WORKSPACE}/${prefix}/build
rpm_dir=${WORKSPACE}/${prefix}/rpms
cov_dir=${WORKSPACE}/${prefix}/cov
tarball_dir=${WORKSPACE}/${prefix}/tarball


make_opt="-j$(nproc)"

# extract jenkins commands from function args
function check_commands
{
    local cmd=$1
    local pat=""
    local test_list="threads src_rpm oshmem check help_txt known_issues cov all"
    for pat in $(echo $test_list); do
        echo -n "checking $pat "
        if [[ $cmd =~ jenkins\:.*no${pat}.* ]]; then
            echo disabling
            eval "jenkins_test_${pat}=no"
        elif [[ $cmd =~ jenkins\:.*${pat}.* ]]; then
            echo enabling
            eval "jenkins_test_${pat}=yes"
        else
            echo no directive for ${pat}
        fi
    done

    if [ "$jenkins_test_all" = "yes" ]; then
        echo Enabling all tests
        for pat in $(echo $test_list); do
            eval "jenkins_test_${pat}=yes"
        done
    fi
}

function test_cov
{
    local cov_root_dir=$1
    local cov_proj=$2
    local cov_make_cmd=$3
    local cov_directive=$4

    local nerrors=0;

    module load tools/cov

    local cov_build_dir=$cov_dir/$cov_proj

    rm -rf $cov_build_dir
    cov-build   --dir $cov_build_dir $cov_make_cmd

    for excl in $cov_exclude_file_list; do
        cov-manage-emit --dir $cov_build_dir --tu-pattern "file('$excl')" delete
    done

    cov-analyze --dir $cov_build_dir
    nerrors=$(cov-format-errors --dir $cov_build_dir | awk '/Processing [0-9]+ errors?/ { print $2 }')

    index_html=$(cd $cov_build_dir && find . -name index.html | cut -c 3-)

    if [ -n "$nerrors" ]; then
        if [ "$nerrors" = "0" ]; then
            echo ok - coverity found no issues for $cov_proj >> $cov_stat_tap
        else
            echo "not ok - coverity detected $nerrors failures in $cov_proj # $cov_directive" >> $cov_stat_tap
            local cov_proj_disp="$(echo $cov_proj|cut -f1 -d_)"
            echo "" >> $gh_cov_msg
            echo "* Coverity found $nerrors errors for ${cov_proj_disp}" >> $gh_cov_msg
            echo "<li><a href=${cov_proj}/output/errors/index.html>Report for ${cov_proj}</a>" >> $cov_dir/index.html
        fi
    else
        echo "not ok - coverity failed to run for $cov_proj # SKIP failed to init coverity" >> $cov_stat_tap
    fi

    module unload tools/cov

    return $nerrors
}

# check for jenkins commands in PR title
if [ -n "$ghprbPullTitle" ]; then
    check_commands "$ghprbPullTitle"
fi

# check for jenkins command in PR last comment
if [ -n "$ghprbPullLink" ]; then
    set +xeE
    pr_url=$(echo $ghprbPullLink | sed -e s,github.com,api.github.com/repos,g -e s,pull,issues,g)
    pr_url="${pr_url}/comments"
    pr_file="$WORKSPACE/github_pr_${ghprbPullId}.json"
    curl -s $pr_url > $pr_file
    echo Fetching PR comments from URL: $pr_url

    # extracting last comment
    pr_comments="$(cat $pr_file | jq -M -a '.[length-1] | .body')"

    echo Last comment: $pr_comments
    if [ -n "$pr_comments" ]; then
        check_commands "$pr_comments"
    fi
    set -xeE
fi

echo Running following tests:
set|grep jenkins_test_

function on_start()
{
    echo Starting on host: $(hostname)

    export distro_name=$(python -c 'import platform ; print platform.dist()[0]' | tr '[:upper:]' '[:lower:]')
    export distro_ver=$(python  -c 'import platform ; print platform.dist()[1]' | tr '[:upper:]' '[:lower:]')
    if [ "$distro_name" == "suse" ]; then
        patch_level=$(egrep PATCHLEVEL /etc/SuSE-release|cut -f2 -d=|sed -e "s/ //g")
        if [ -n "$patch_level" ]; then
            export distro_ver="${distro_ver}.${patch_level}"
        fi
    fi
    echo $distro_name -- $distro_ver

    # save current environment to support debugging
#    set +x
#    env| sed -ne "s/\(\w*\)=\(.*\)\$/export \1='\2'/p" > $WORKSPACE/test_env.sh
#    chmod 755 $WORKSPACE/test_env.sh
#    set -x
}

function on_exit
{
    set +x
    rc=$((rc + $?))
    echo exit code=$rc
    if [ $rc -ne 0 ]; then
        # FIX: when rpmbuild fails, it leaves folders w/o any permissions even for owner
        # jenkins fails to remove such and fails
        find $rpm_dir -type d -exec chmod +x {} \;
    fi
}

# $1 - test name
# $2 - test command
function check_result()
{
    set +e
    eval $timeout_exe $2
    ret=$?
    set -e
    if [ $ret -gt 0 ]; then
        echo "not ok $test_id $1" >> $run_tap
    else
        echo "ok $test_id $1" >> $run_tap
    fi
    test_id=$((test_id+1))
}

trap "on_exit" INT TERM ILL KILL FPE SEGV ALRM

on_start


cd $WORKSPACE
if [ "$jenkins_test_build" = "yes" ]; then
    echo "Checking for build ..."

    cd ${WORKSPACE}/${prefix}
    wget http://downloads.sourceforge.net/levent/libevent-2.0.22-stable.tar.gz
    tar zxf libevent-2.0.22-stable.tar.gz
    cd libevent-2.0.22-stable
    libevent_dir=$PWD/install
    ./autogen.pl && ./configure --prefix=$libevent_dir && make && make install

    cd $WORKSPACE
    configure_args="--with-libevent=$libevent_dir"

    # build pmix
    ./autogen.pl
    echo ./configure --prefix=$pmix_dir $configure_args | bash -xeE
    make $make_opt install
    jenkins_build_passed=1

    # make check
    if [ "$jenkins_test_check" = "yes" ]; then
        make $make_opt check || exit 12
    fi
fi

cd $WORKSPACE
if [ -n "jenkins_build_passed" -a "$jenkins_test_cov" = "yes" ]; then
    echo "Checking for coverity ..."

    vpath_dir=$WORKSPACE
    cov_proj="all"
    gh_cov_msg=$WORKSPACE/cov_gh_msg.txt
    cov_stat=$vpath_dir/cov_stat.txt
    cov_stat_tap=$vpath_dir/cov_stat.tap
    cov_build_dir=$vpath_dir/${prefix}/cov_build
    cov_url_webroot=${JOB_URL}/${BUILD_ID}/Coverity_Report

    rm -f $cov_stat $cov_stat_tap

    if [ -d "$vpath_dir" ]; then
        mkdir -p $cov_build_dir
        pushd $vpath_dir
        for dir in $cov_proj; do
            if [ "$dir" = "all" ]; then
                make_cov_opt=""
                cov_directive="SKIP"
            else
                if [ ! -d "$dir" ]; then
                    continue
                fi
                cov_directive="TODO"
                make_cov_opt="-C $dir"
            fi
            echo Working on $dir

            cov_proj="cov_$(basename $dir)"
            set +eE
            make $make_cov_opt $make_opt clean 2>&1 > /dev/null
            test_cov $cov_build_dir $cov_proj "make $make_cov_opt $make_opt all" $cov_directive
            set -eE
        done
        if [ -n "$ghprbPullId" -a -f "$gh_cov_msg" ]; then
            echo "* Coverity report at $cov_url_webroot" >> $gh_cov_msg
            if [ "$jenkins_test_comments" = "yes" ]; then
                gh pr $ghprbPullId --comment "$(cat $gh_cov_msg)"
            fi
        fi
        popd
    fi
fi

cd $WORKSPACE
if [ "$jenkins_test_src_rpm" = "yes" ]; then
    echo "Checking for rpm ..."

    # check distclean
    make $make_opt distclean
    ./autogen.pl
    echo ./configure --prefix=$pmix_dir $configure_args | bash -xeE || exit 11

    if [ -x /usr/bin/dpkg-buildpackage ]; then
        echo "Do not support PMIX on debian"
    else
        echo "Building PMIX src.rpm"
        rm -rf $tarball_dir
        mkdir -p $tarball_dir

        make_dist_args="--highok --distdir=$tarball_dir --greekonly"

        for arg in no-git-update dirtyok verok; do
            if grep $arg contrib/make_tarball 2>&1 > /dev/null; then
                make_dist_args="$make_dist_args --${arg}"
            fi
        done

        # ugly hack, make_tarball has hardcoded "-j32" and sometimes it fails on some race
        sed -i -e s,-j32,-j8,g contrib/make_tarball

        export LIBEVENT=$libevent_dir
        chmod +x ./contrib/make* ./contrib/buildrpm.sh
        echo contrib/make_tarball $make_dist_args | bash -xeE || exit 11

        # build src.rpm
        # svn_r=$(git rev-parse --short=7 HEAD| tr -d '\n') ./contrib/make_tarball --distdir=$tarball_dir
        tarball_src=$(ls -1 $tarball_dir/pmix-*.tar.bz2|sort -r|head -1)

        echo "Building PMIX bin.rpm"
        rpm_flags="--define 'mflags -j8' --define '_source_filedigest_algorithm md5' --define '_binary_filedigest_algorithm md5'"
        (cd ./contrib/ && env rpmbuild_options="$rpm_flags" rpmtopdir=$rpm_dir ./buildrpm.sh $tarball_src)
    fi
fi

#
# JENKINS_RUN_TESTS should be set in jenkins slave node to indicate that node can run tests
#
cd $WORKSPACE
if [ -n "$JENKINS_RUN_TESTS" -a "$JENKINS_RUN_TESTS" -ne "0" ]; then
    echo "Checking for tests ..."

    run_tap=$WORKSPACE/run_test.tap
    rm -rf $run_tap

    # build pmix
    ./autogen.pl
    echo ./configure --prefix=$pmix_dir $configure_args --disable-visibility | bash -xeE
    make $make_opt install

    cd $WORKSPACE/test

    echo "1..11" > $run_tap

    test_id=1
    # 1 blocking fence with data exchange among all processes from two namespaces:
    test_exec='./pmix_test -n 4 --ns-dist 3:1 --fence "[db | 0:0-2;1:3]"'
    check_result "blocking fence w/ data all" "$test_exec"
    test_exec='./pmix_test -n 4 --ns-dist 3:1 --fence "[db | 0:;1:3]"'
    check_result "blocking fence w/ data all" "$test_exec"
    test_exec='./pmix_test -n 4 --ns-dist 3:1 --fence "[db | 0:;1:]"'
    check_result "blocking fence w/ data all" "$test_exec"

    # 1 non-blocking fence without data exchange among processes from the 1st namespace
    test_exec='./pmix_test -n 4 --ns-dist 3:1 --fence "[0:]"'
    check_result "non-blocking fence w/o data" "$test_exec"

    # blocking fence without data exchange among processes from the 1st namespace
    test_exec='./pmix_test -n 4 --ns-dist 3:1 --fence "[b | 0:]"'
    check_result "blocking fence w/ data" "$test_exec"

    # non-blocking fence with data exchange among processes from the 1st namespace. Ranks 0, 1 from ns 0 are sleeping for 2 sec before doing fence test.
    test_exec='./pmix_test -n 4 --ns-dist 3:1 --fence "[d | 0:]" --noise "[0:0,1]"'
    check_result "non-blocking fence w/ data" "$test_exec"

    # blocking fence with data exchange across processes from the same namespace.
    test_exec='./pmix_test -n 4 --job-fence -c'
    check_result "blocking fence w/ data on the same nspace" "$test_exec"

    # 3 fences: 1 - non-blocking without data exchange across processes from ns 0,
    # 2 - non-blocking across processes 0 and 1 from ns 0 and process 3 from ns 1,
    # 3 - blocking with data exchange across processes from their own namespace.
#    Disabled as incorrect at the moment
#    test_exec='./pmix_test -n 4 --job-fence -c --fence "[0:][d|0:0-1;1:]" --use-same-keys --ns-dist "3:1"'
#    check_result "mix fence" $test_exec

    # test publish/lookup/unpublish functionality.
    test_exec='./pmix_test -n 2 --test-publish'
    check_result "publish" "$test_exec"

    # test spawn functionality.
    test_exec='./pmix_test -n 2 --test-spawn'
    check_result "spawn" "$test_exec"

    # test connect/disconnect between processes from the same namespace.
    test_exec='./pmix_test -n 2 --test-connect'
    check_result "connect" "$test_exec"

    # resolve peers from different namespaces.
    test_exec='./pmix_test -n 5 --test-resolve-peers --ns-dist "1:2:2"'
    check_result "resolve peers" "$test_exec"

    # run valgrind
    if [ "$jenkins_test_vg" = "yes" ]; then
        set +e
        module load tools/valgrind

        vg_opt="--tool=memcheck --leak-check=full --error-exitcode=0 --trace-children=yes  --trace-children-skip=*/sed,*/collect2,*/gcc,*/cat,*/rm,*/ls --track-origins=yes --xml=yes --xml-file=valgrind%p.xml --fair-sched=try --gen-suppressions=all"

        valgrind $vg_opt  ./pmix_test -n 4 --timeout 60 --ns-dist 3:1 --fence "[db | 0:;1:3]"

        valgrind $vg_opt  ./pmix_test -n 4 --timeout 60 --job-fence -c

        valgrind $vg_opt  ./pmix_test -n 2 --timeout 60 --test-publish

        valgrind $vg_opt  ./pmix_test -n 2 --timeout 60 --test-spawn

        valgrind $vg_opt  ./pmix_test -n 2 --timeout 60 --test-connect

        valgrind $vg_opt  ./pmix_test -n 5 --timeout 60 --test-resolve-peers --ns-dist "1:2:2"

        module unload tools/valgrind
        set -e
    fi
fi
