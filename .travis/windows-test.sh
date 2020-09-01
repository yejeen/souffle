#!/bin/bash

set -e
set -x

export MSYS_NO_PATHCONV=1
export MSYS2_ARG_CONV_EXCL="*"

cat > run-msvc-test.sh <<EOF
set -x
set -x

export SOUFFLE_CATEGORY="Interface"
export SOUFFLE_CONFS="-j8"
export SOUFFLE_TESTS_MSVC_VARS="C:\VS\VC\Auxiliary\Build\vcvars64.bat"
export GETOPT_LIB="$(cygpath -w $(pwd)/vcpkg/installed/x64-windows/lib/getopt.lib)"
export GETOPT_INCLUDE="$(cygpath -w $(pwd)/vcpkg/installed/x64-windows/include)"

./bootstrap
./configure
make -j$(nproc)
make check || true
echo "MSVC_VARS: \$SOUFFLE_TESTS_MSVC_VARS"
cd tests
./testsuite --keywords=insert_print -v -d -x
cd ..
cat tests/testsuite.dir/*/*
more tests/testsuite.dir/*/*.err | cat

EOF

chmod +x run-msvc-test.sh
wsl ./run-msvc-test.sh
