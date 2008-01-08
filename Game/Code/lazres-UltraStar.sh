#!/bin/bash

## 
# Creates an .lrs resource-file.
# This script reads an rc resource definition file
# and uses lazres to compile it into an lrs-file.
##

RCFILE=UltraStar.rc
OUTFILE=UltraStar.lrs
# set this to your lazarus directory if autodetection fails
LAZDIR_DEFAULT=/usr/local/share/lazarus

LAZDIR=`whereis -b lazarus | cut -s -d ' ' -f2`
if [ ! -d "${LAZDIR}" ]; then
  if [ ! -d "${LAZDIR_DEFAULT}" ]; then
    echo "Autodetecting Lazarus-directory failed!"
    echo "Set LAZDIR_DEFAULT to your Lazarus directory in this script."
    exit 1
  fi
  LAZDIR="${LAZDIR_DEFAULT}"
fi

LAZRES="${LAZDIR}/tools/lazres"
LAZRES_SRC="${LAZRES}.pp"
LAZRES_PROJ="${LAZRES}.lpi"
LAZBUILD="${LAZDIR}/lazbuild"
UNIT_PATH=-Fu${LAZDIR}/lcl/units/i386-linux

# check if lazres is available
if [ ! -x "${LAZRES}" ]; then
  # lazres not available -> build it
  echo -e "Building of lazres required...\n"
  # check if lazres project-file exists
  if [ ! -f "${LAZRES_SRC}" ]; then
    echo -e "\n${LAZRES_SRC} not found!"
    exit 1
  fi
  # build lazres
  fpc -dRELEASE "${UNIT_PATH}" "${LAZRES_SRC}"
  #"${LAZBUILD}" "${LAZRES_PROJ}"
  # did it fail?
  if [ $? -ne 0 ]; then
    echo -e "\nBuilding lazres failed!"
    exit 1
  fi
  echo -e "\nBuilding lazres finished!"
fi

# create temp-dir for file renaming (do not use varname TMPDIR!)
RENAME_DIR=`mktemp -t -d usdxresXXXXXXXXXX` || exit 1
echo "Temporary directory for file-renaming created! (${RENAME_DIR})"

# read each line of RC-file and add resource-path to parameter-list
LAZRES_PARAMS=""
echo "Reading RC-file..."
{
while read -r res_name res_type res_path; do
  # check if line is commented out
  if (echo ${res_name} | grep "^//" >/dev/null) ; then
    echo "Skip resource: ${res_name}"
    continue
  fi
  # add non-empty paths to parameter-list
  if [ ! -z "${res_path}" ]; then
    # replace backslashes (\\) and carriage return (\r) (MS-DOS line-ending)
    RES_PATH=`echo "${res_path}" | tr '\r\\' '\0/'`
    RES_NEW_PATH="${RENAME_DIR}/${res_name}.${res_type}"
    eval cp "${RES_PATH}" "${RES_NEW_PATH}"
    # append to parameter-list
    LAZRES_PARAMS="${LAZRES_PARAMS} ${RES_NEW_PATH}"
  fi
done
} < "${RCFILE}"

# create resource file
rm -f ${OUTFILE}
echo "Creating resource file..."
"${LAZRES}" "${OUTFILE}" ${LAZRES_PARAMS} 
RES=0
if [ -f "${OUTFILE}" ]; then
  echo -e "\nResource file successfully created!"
  RES=1
else
  echo -e "\nCreation of resource file failed!"
fi

# remove temp-directory
echo "Removing temporary directory..."
rm -rf "${RENAME_DIR}"

if [ $RES -eq 1 ]; then
  echo -e "\nReady."
fi
