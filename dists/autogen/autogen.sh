#!/bin/sh
# find absolute dists/autogen and project-root dirs
AUTOGEN_DIR=`dirname $0`
ABS_AUTOGEN_DIR=`cd ${AUTOGEN_DIR}; pwd`
ROOT_DIR="${AUTOGEN_DIR}/../.."
ABS_ROOT_DIR=`cd ${ROOT_DIR}; pwd`
# change to project-root and create configure-script
cd ${ROOT_DIR}
aclocal -I "${ABS_AUTOGEN_DIR}/m4" && autoconf && echo "configure created in ${ABS_ROOT_DIR}"
