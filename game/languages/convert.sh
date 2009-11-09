#!/bin/bash
# See: http://www.microsoft.com/resources/msdn/goglobal/default.mspx?OS=Windows%20Vista

function convertUTF8 {
  if [ ! -f $2.ini ]; then
    echo "skip $2.ini: does not exist"
    return
  fi

  # UTF-8 BOM (0xEF 0xBB 0xBF)
  BOM=`echo -n -e "\0357\0273\0277"`
  HEADER=`head -c3 $2.ini`
  if [ $HEADER != $BOM ]; then
    echo "Convert $2.ini from $1 to UTF8"
    echo -n $BOM >$2.tmp
    iconv -f $1 -t UTF-8 $2.ini >>$2.tmp
    mv $2.tmp $2.ini
  else
    echo "skip $2.ini: already UTF8"    
  fi
}

#convertUTF8 UTF8 Catalan
convertUTF8 CP1252 French 
convertUTF8 CP1252 Italian 
convertUTF8 CP1252 Spanish 
convertUTF8 CP1250 Croatian
convertUTF8 CP1252 Euskara 
convertUTF8 CP1252 German 
#convertUTF8 UCS-2LE Japanese 
convertUTF8 CP1252 Swedish 
convertUTF8 CP1252 Dutch 
convertUTF8 CP1252 Finnish 
convertUTF8 CP1253 Greek 
convertUTF8 CP1252 Portuguese 

convertUTF8 CP1252 Danish 
convertUTF8 CP1252 Norwegian 
#convertUTF8 CP1251 Serbian 
convertUTF8 CP1250 Serbian 
convertUTF8 CP1250 Slovenian 
convertUTF8 CP1250 Polish 
convertUTF8 CP1250 Slovak 
