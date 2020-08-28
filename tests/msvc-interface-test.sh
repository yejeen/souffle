#!/bin/bash

set -e

TESTDIR=$1
shift
TESTNAME=$1

script_location=$(dirname $0)
test_dir_wsl=$($script_location/create-msvc-build-dir.sh)
mkdir $test_dir_wsl 1>/dev/null 2>/dev/null || true
cp -R * $test_dir_wsl
test_dir=$(wslpath -w $test_dir_wsl)
souffle_include=$(wslpath -w /usr/local/include)
driver_filename=$(wslpath -w $TESTDIR/driver.cpp)

get_opt_lib='getopt.lib'

# Uses the environment variable SOUFFLE_TESTS_MSVC_VARS, which ought to be
# the windows path of the vcvars batch file, with gno spaces.
cat <<EOF > $test_dir_wsl/compile.bat
call $SOUFFLE_TESTS_MSVC_VARS

cl.exe $driver_filename $TESTNAME.cpp /Fe: $TESTNAME.exe /permissive- /nologo /D__EMBEDDED_SOUFFLE__ /I $souffle_include /EHsc /W4 /WX /D_CRT_SECURE_NO_WARNINGS /link $get_opt_lib
EOF

workdir=$(pwd)
pushd $test_dir_wsl 2>&1 1>/dev/null
cmd.exe /C "compile.bat" 1>>$workdir/$TESTNAME.err 2>>$workdir/$TESTNAME.err
popd 2>&1 1>/dev/null
cp $test_dir_wsl/$TESTNAME.exe ./
cp $test_dir_wsl/*.cpp ./
rm -rf $test_dir_wsl
