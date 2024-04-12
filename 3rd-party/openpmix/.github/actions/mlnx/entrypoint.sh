#!/bin/bash -xeE

rel_path=$(dirname $0)
abs_path=$(readlink -f $rel_path)

if [ "$1" = "build" ]; then
    jenkins_test_build="yes"
    jenkins_test_check="no"
    jenkins_test_src_rpm="no"
elif [ "$1" = "srcrpm" ]; then
    jenkins_test_build="no"
    jenkins_test_check="no"
    jenkins_test_src_rpm="yes"
elif [ "$1" = "test" ]; then
    jenkins_test_build="no"
    jenkins_test_check="yes"
    jenkins_test_src_rpm="no"
fi

jenkins_test_build=${jenkins_test_build:="yes"}
jenkins_test_check=${jenkins_test_check:="yes"}
jenkins_test_src_rpm=${jenkins_test_src_rpm:="yes"}
jenkins_test_vg=${jenkins_test_vg:="no"}

timeout_exe=${timout_exe:="timeout -s SIGKILL 1m"}

# prepare to run from command line w/o jenkins
if [ -z "$WORKSPACE" ]; then
    WORKSPACE=$PWD
    JOB_URL=$WORKSPACE
    BUILD_NUMBER=1
    JENKINS_RUN_TESTS=1
    NOJENKINS=${NOJENKINS:="yes"}
fi

OUTDIR=$WORKSPACE/out

prefix=jenkins
rm -rf ${WORKSPACE}/${prefix}
mkdir -p ${WORKSPACE}/${prefix}
work_dir=${WORKSPACE}/${prefix}
build_dir=${work_dir}/build
pmix_dir=${work_dir}/install
build_dir=${work_dir}/build
rpm_dir=${work_dir}/rpms
tarball_dir=${work_dir}/tarball

pmix_ver=`cat VERSION | grep "^major=" | cut -d "=" -f 2``cat VERSION | grep "^minor=" | cut -d "=" -f 2`

make_opt="-j$(nproc)"

test_ret=0

echo Running following tests:
set|grep jenkins_test_

function on_start()
{
    echo Starting on host: $(hostname)

    export distro_name=`python -c 'import platform ; print platform.dist()[0]' | tr '[:upper:]' '[:lower:]'`
    export distro_ver=`python  -c 'import platform ; print platform.dist()[1]' | tr '[:upper:]' '[:lower:]'`
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

function check_out()
{
    local ret=0
    for out in `ls $OUTDIR/out.*`; do
        if [ "$pmix_ver" -ge 31 ]; then
            status=`cat $out | awk '{print $2}'`
        else
            status=`cat $out`
        fi
        if [ "$status" != "OK" ]; then
            ret=1
        fi
    done
    echo $ret
}

# $1 - test name
# $2 - test command
function check_result()
{
    set +e
    eval $timeout_exe $2
    ret=$?
    set -e
    client_ret=$(check_out)
    echo client_ret $client_ret
    if [ $ret -gt 0 ] || [ $client_ret -gt 0 ]; then
        echo "not ok $test_id $1 ($2)" >> $run_tap
        test_ret=1
    else
        echo "ok $test_id $1 ($2)" >> $run_tap
    fi
    rm $OUTDIR/*
    test_id=$((test_id+1))
}

trap "on_exit" INT TERM ILL KILL FPE SEGV ALRM

on_start

autogen_done=0
if [ -e ".autogen_done" ]; then
    autogen_done=1
fi

if [ -x "autogen.sh" ]; then
    autogen_script=./autogen.sh
else
    autogen_script=./autogen.pl
fi
configure_args=""

cd $WORKSPACE
if [ "$jenkins_test_build" = "yes" ]; then
    echo "==========================  TEST BUILD  =========================="
    $autogen_script && touch .autogen_done
    echo ./configure --prefix=$pmix_dir $configure_args | bash -xeE
    make $make_opt install
    cd test
    export PMIX_MCA_pcompress_base_silence_warning=1
    echo "Running make check ..."
    make $make_opt check || (cat test-suite.log && exit 12)
    echo "Make check complete"
    echo "========================  TEST COMPLETE  ========================="
fi

cd $WORKSPACE
if [ "$jenkins_test_src_rpm" = "yes" ]; then
    echo "Checking for rpm ..."

    # check distclean
    make $make_opt distclean
    if [ "${autogen_done}" != "1" ]; then
        $autogen_script && touch .autogen_done
    fi

    if [ -x /usr/bin/dpkg-buildpackage ]; then
        echo "Do not support PMIX on debian"
    else
        # Install Sphinx so that we can "make dist"
        virtualenv --python=python3 venv
        . ./venv/bin/activate
        # This job runs in a CentOS 7 docker container, which does not
        # understand the "--use-feature" pip CLI argument.
        grep -v use-feature docs/requirements.txt > docs/r2.txt
        pip install -r docs/r2.txt

        echo ./configure --prefix=$pmix_dir $configure_args | bash -xeE || exit 11
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

        chmod +x ./contrib/make* ./contrib/buildrpm.sh
        echo contrib/make_tarball $make_dist_args | bash -xeE || exit 11

        # build src.rpm
        # svn_r=$(git rev-parse --short=7 HEAD| tr -d '\n') ./contrib/make_tarball --distdir=$tarball_dir
        tarball_src=$(ls -1 $tarball_dir/pmix-*.tar.bz2|sort -r|head -1)

        echo "Building PMIX bin.rpm"
        rpm_flags="--define 'mflags -j8' --define '_source_filedigest_algorithm md5' --define '_binary_filedigest_algorithm md5'"
        (cd ./contrib/ && env rpmbuild_options="$rpm_flags" rpmtopdir=$rpm_dir ./buildrpm.sh $tarball_src)
        # check distclean
        make $make_opt distclean
    fi
fi
