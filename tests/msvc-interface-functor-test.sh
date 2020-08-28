#!/bin/bash

set -e

TESTDIR=$1
shift
TESTNAME=$1

script_location=$(dirname $0)
test_dir_wsl=$($script_location/create-msvc-build-dir.sh)
mkdir $test_dir_wsl 1>/dev/null 2>/dev/null || true
cp -R * $test_dir_wsl
functor_dir=$(wslpath -w $TESTDIR)
# We need to copy the include directory because windows doesn't
# like the WSL symlinks
cp -Lr $SOUFFLE_INC $test_dir_wsl 1>&2 2>/dev/null || true
souffle_include=$(wslpath -w $test_dir_wsl/include)

get_opt_lib='getopt.lib'

# Uses the environment variable SOUFFLE_TESTS_MSVC_VARS, which ought to be
# the windows path of the vcvars batch file, with no spaces.
cat <<EOF > $test_dir_wsl/compile.bat
call $SOUFFLE_TESTS_MSVC_VARS
cl.exe $functor_dir\functors.cpp /std:c++17 /permissive- /nologo /c
lib functors.obj

cl.exe $TESTNAME.cpp /Fe: $TESTNAME.exe /std:c++17 /permissive- /nologo /I $souffle_include /EHsc /W4 /WX /D_CRT_SECURE_NO_WARNINGS /link $get_opt_lib functors.lib
EOF

workdir=$(pwd)
pushd $test_dir_wsl 2>&1 1>/dev/null
cmd.exe /C "compile.bat" 2>>$workdir/$TESTNAME.err 1>/dev/null
popd 2>&1 1>/dev/null

cp $test_dir_wsl/$TESTNAME.exe ./
rm -rf $test_dir_wsl
