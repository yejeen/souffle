#!/bin/bash

set -e
set -x


MSYS_NO_PATHCONV=1
MSYS2_ARG_CONV_EXCL="*;./test-script.sh;./bootstrap"

wsl bash bootstrap
wsl bash configure
wsl make -j$(nproc)
