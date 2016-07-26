#!/bin/bash
# See: http://www.microsoft.com/resources/msdn/goglobal/default.mspx?OS=Windows%20Vista

declare -a forceLang=()
declare -a forceCP=()

function convertUTF8 {
  local fromcp;
  local fromcptext;
  if [ ! -f "$1" ]; then
    echo "skip $1: does not exist"
    return
  fi
  
  fromcp=`getCP "$1"`
  if [ -n "$fromcp" ]; then
    fromcptext=" from $fromcp";
	fromcp=" -f $fromcp"
  fi

  # UTF-8 BOM (0xEF 0xBB 0xBF)
  BOM=`echo -n -e "\0357\0273\0277"`
  HEADER=`head -c3 "$1"`
  if [ $HEADER != $BOM ]; then
    echo -n "Convert $1$fromcptext to UTF8"
    echo -n $BOM >"$1.tmp"
    ERROR=$(iconv $fromcp -t UTF-8 "$1" 2>&1 >> "$1.tmp" )
    
    if [ -z "$ERROR" ]; then
      mv "$1.tmp" "$1"
      echo "... done."
    else
      # file wasn't converted propertly
      rm -f "$1.tmp"
      echo ". Error, unable to convert. Check $1.log"
      echo $ERROR>"$1.log"
    fi
  else
    echo "skip $1: already UTF8"    
  fi
}

function Force() {
  forceLang+=($1.ini)
  forceCP+=($2)
}

function getCP () {
  local e
  local found
  local value=$1
  
  i=0
  for e in "${forceLang[@]}"
  do
    if [ "$e" == "$value" ] ; then
      echo ${forceCP[$i]}
      return 1
    fi
    let i=i+1
  done
  
  return 0
}

# Enable forcing a specific codepage to convert from
# add likes like the following to do so
#Force "Spanish" "CP1252" 

for f in *.ini; do convertUTF8 "$f"; done
convertUTF8 Language.new
