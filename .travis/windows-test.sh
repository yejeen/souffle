#!/bin/bash

set -e
set -x


MSYS_NO_PATHCONV=1
MSYS2_ARG_CONV_EXCL="*;./test-script.sh;./bootstrap"
SOUFFLE_CATEGORY="Interface"
SOUFFLE_CONFS="-j8"
SOUFFLE_TESTS_MSVC_VARS="C:\VS\VC\Auxiliary\Build\vcvars64.bat"
wsl bash bootstrap
wsl bash configure
wsl make -j$(nproc)
wsl make check
