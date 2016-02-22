#!/bin/bash

if [ "$1" != "ok" ] || ! [ -f  $HOME/pas/h2pas/h2pas ]
then
  echo
  echo "*** Warning: You don't want to run this script! ***"
  echo
  exit 1
fi

INDENT='indent -npro -nut -l 1024 -br -brs -cdw -ce -npsl -nlps -bap -saf -sai -saw'

REGNUM='\(\(0x[0-9A-Fa-f]\+\)\|\([0-9]\+\)\)'

C_REGEX='^ *CURL\(\(OPT\)\|\(INFO\)\)_[A-Z0-9_]\+ *= *'$REGNUM' *[+] *'$REGNUM' *[,]\?'

P_REGEX='^ *CURL\(\(OPT\)\|\(INFO\)\)_[A-Z0-9_]\+ *:= *[0-9]\+[,]\?'

FPC_HAS_VARARGS="yes"
SAVE_TEMP_FILES="no";

# don't try this one at home ;-)

H2PAS_EXE=$HOME/pas/h2pas/h2pas
H2PAS="$H2PAS_EXE -c -S -k -p -L -D -l curl -n LIB_CURL -E 1 -A 0 -U Libc -u curl_h -o - "

CURL_H_ORIG="/usr/local/include/curl/curl.h"
CURL_H_PREP="./curl_h.h"
CURL_H_MACS="./curl_macs.h"
ENUMSORT_TMP="./enumsort.tmp"


function say () {
  echo -e "\n$@" 1>&2
}


say 'Preprocessing macros...'
cpp -P -nostdinc -DCURL_NO_OLDIES -dM $CURL_H_ORIG | awk '{gsub(/\t/, " "); gsub(/[ ]+/, " "); sub(/^ +/, ""); sub(/ +$/, ""); print $0}' \
| grep -v '^$' | indent -npro -nut -l 1024 -br -brs -cdw -ce -npsl -nlps -bap -saf -sai -saw | \
awk '{sub(/^\}$/, "}\n"); print}' | grep -v '^\#define __' | grep -v '^\#define i386\>' | \
grep -v '^\#define unix\>' | grep -v '^\#define linux\>' | grep -v '\#\#' > $CURL_H_MACS


say 'Converting macros...'
$H2PAS_EXE -c -S -k -p -L -i $CURL_H_MACS -o - | \
grep -iv '^{\$define ' | \
grep -iv '^const$'| sort | uniq | \
awk '{sub(/^ */, "  "); print}' > curl_macs.inc


say 'Preprocessing header...'
cpp -P -nostdinc -DCURL_NO_OLDIES $CURL_H_ORIG | \
awk '{ gsub(/=-/,"= -"); gsub(/\t/, " "); gsub(/[ ]+/, " "); sub(/^ +/, ""); sub(/ +$/, ""); if ( /./ ) print $0}' | \
$INDENT | tr -s ' ' | while read LINE
do
  echo $LINE | grep "$C_REGEX" > /dev/null
  if [ $? -ne 0 ]
  then
    echo $LINE | grep "^\(typedef \)\?enum {$" > /dev/null
    if [ $? -ne 0 ]
    then
      echo "$LINE"
    else
      echo -n "$LINE "
    fi     
  else
    echo ${LINE//,/} | { read NAME EQUALS ARG_ONE PLUS ARG_TWO
      echo -n $NAME = $(( ARG_ONE + ARG_TWO ))
    }
    echo "$LINE" | grep ',' > /dev/null && echo ',' || echo
  fi
done | $INDENT | awk '{
sub(/^const /, "");
sub(/enum { CURL_HTTP_VERSION_NONE,/, "enum curl_http_version {\n  CURL_HTTP_VERSION_NONE,"); # <- fix anonomymous enum
sub(/enum { CURL_SSLVERSION_DEFAULT,/,"enum curl_sslversion{\n  CURL_SSLVERSION_DEFAULT,"); # <- fix anonomymous enum
gsub(/\<CURLMsg\>/, "CurlMsgRec"); # <- Case-Insensitive naming collision
sub(/CURLINFO_LASTONE = 28/,"CURLINFO_LASTONE = 28,\n  CURLINFO_VERYLAST"); # <- I need an implicit "LAST"
sub(/typedef enum { CURLM_CALL_MULTI_PERFORM = -1,/,"typedef int CURLMcode;\nenum {\n  CURLM_CALL_MULTI_PERFORM = -1,"); # <- enum cant be neg
sub(/typedef enum {/,"typedef enum {\n ");
sub(/} CURLMcode;/, "};") # <- contains a negative value, so change enum into list of const
if ( /.*\<curl_easy_getinfo\>.*/ ) sub(/\.\.\./, "int arg"); # <- only curl_formadd() needs varargs
print}' | grep -vw  'CURLOPT_SSLKEYPASSWD' > $CURL_H_PREP



say 'Converting header...'




echo -n > curl_h.pas
echo '(***  Copyright (c) 2002-2005, Jeffrey Pohlmeyer, <yetanothergeek@yahoo.com>  ***)' >> curl_h.pas
echo '(* Licensed per the file COPYING, which should be included in all distributions *)' >> curl_h.pas
echo >> curl_h.pas

SKIP_NEXT="no"

$H2PAS $CURL_H_PREP | \
awk '{ sub(/pcurl_httppost=\^curl_httppost;/, "pcurl_httppost=^curl_httppost;\nppcurl_httppost=^pcurl_httppost;"); print }' | \
while read LINE
  do
  echo "$LINE" | grep '^ *CURL\(\(OPT\)\|\(INFO\)\)_[A-Z0-9_]\+ *:= *[0-9]\+[,]\?' > /dev/null
  if [ $? -eq 0 ]
  then
    HAVE_SSL_DUP="no"
    echo -n "${LINE/,/}" > $ENUMSORT_TMP
    while read SUB
    do
      echo "$SUB" | grep '^ *CURL\(\(OPT\)\|\(INFO\)\)_[A-Z0-9_]\+ *:= *[0-9]\+[,]\?' > /dev/null
      if [ $? -eq 0 ]
      then
        echo -en ",\n${SUB/,/}" >> $ENUMSORT_TMP
      else
        echo ',' >> $ENUMSORT_TMP
        sort -g -k 3 $ENUMSORT_TMP | align
        echo "$SUB"
        break
      fi
    done
  else
    if [ "$LINE" = "implementation" ]
    then
      echo -e 'const\nCURLOPT_SSLKEYPASSWD = CURLOPT_SSLCERTPASSWD;'
    fi
    echo "$LINE" | grep '; args:array of const.* cdecl;' > /dev/null
    if [ $?  -eq 0 ]
    then
      echo "$LINE" | { 
                       if [ "$FPC_HAS_VARARGS" = "yes" ]
                       then
                         awk '{
                               sub(/; args:array of const/, ""); 
                               sub(/ cdecl;/, " varargs; cdecl;"); print }' 
                       else
                         awk '{
                               sub(/; args:array of const/, "{$IFDEF FPC}; args:array of const{$ENDIF}"); 
                               sub(/ cdecl;/, "{$IFNDEF FPC} varargs; cdecl;{$ENDIF}"); print }' 
                       fi
                     }
      SKIP_NEXT="yes" # next line after function with 'array of const' is a duplicate with no varargs
    else
      if [ $SKIP_NEXT = "no" ] 
      then 
        echo "$LINE" | grep 'arg:longint' > /dev/null
        if [ $? -eq 0 ]
        then
          echo $LINE | grep -w 'curl_easy_getinfo' > /dev/null
          if [ $? -eq 0 ]
          then
            echo "$LINE" | awk '{sub(/arg:longint/,"out value"); print}'
          else
            echo $LINE | grep -w 'curl_\(\(easy\)\|\(share\)\)_setopt' > /dev/null
            if [ $? -eq 0 ]
            then
              echo "$LINE overload;"
              echo "$LINE overload;" | awk '{sub(/arg:longint/,"arg:pointer"); print}'
            else
              say "*** Warning: unrecognized ellipsis:\n  $LINE"
              echo "$LINE"
            fi
          fi
        else
          echo "$LINE"
        fi
      fi
      SKIP_NEXT="no"
    fi
  fi
done | \
awk '{ sub(/\^pchar;/,"ppchar;"); sub(/:=/,"="); sub(/LIB_CURL=\047curl\047;/,"LIB_CURL = {$IFDEF LINUX} \047libcurl.so\047 {$ELSE} \047libcurl-3.dll\047 {$ENDIF};"); print }' | \
tr -s '\n' | {
  # Half-assed attempt at pretty printing...
  SHIFT=""
  LEVEL=0
  echo '{$IFNDEF CURL_H_INCLUDE}'
  while read LINE
  do
    case "$LINE" in
    '{$'*)
      echo -e "\n$LINE"
    ;;
    'CURLM_'*'='*'('*')'*)
     echo -n "$SHIFT${LINE%%=*}="
     VALUE=${LINE##*=}
     VALUE=${VALUE/;/}
     let VALUE="$VALUE"
     echo " $VALUE;"
     ;;
    'const'|'type')
      SHIFT="  "
      echo -e "\n$LINE"
    ;;
    *'(')
      echo "$SHIFT$LINE"
      SHIFT="    "
    ;;
    ');')
      SHIFT="  "
      echo -e "$SHIFT$LINE\n"
    ;;
    *' record')
      [ $LEVEL -eq 0 ] && echo
      LEVEL=$(( LEVEL + 1 ))
      echo "$SHIFT$LINE"
      SHIFT="$SHIFT  "
    ;;
    'case '*' of')
       echo -e "$SHIFT$LINE"
       SHIFT="$SHIFT  "
    ;;
    'end;')
       LEVEL=$(( LEVEL - 1 ))
       case $LEVEL in
       0)
         SHIFT="  "
       ;;
       1)
         SHIFT="    "
       ;;
       2)
         SHIFT="      "
       ;;
       *)
         SHIFT=""
       esac
       echo "$SHIFT$LINE"
       [ $LEVEL -eq 0 ] && echo
    ;;
    'procedure '*)
       SHIFT=""
       echo "$SHIFT$LINE"
    ;;
    'function '*)
       SHIFT=""
       echo "$SHIFT${LINE/ /  }"
    ;;
    'uses '*)
       SHIFT=""
       echo -e "\n$SHIFT$LINE"
    ;;
    'interface')
       SHIFT=""
       echo -e "\n$SHIFT$LINE"
    ;;
    'implementation')
       SHIFT=""
       # Merge in the "macro" constants, moving the forward-dependencies to the bottom...
       cat ./curl_macs.inc | \
         grep -v '^ *CURLAUTH_ANYSAFE *= *not *( *CURLAUTH_BASIC *) *; *$' | \
         grep -v '^ *CURL_GLOBAL_ALL *= *CURL_GLOBAL_SSL \+or \+CURL_GLOBAL_WIN32 *;' | \
         grep -v '^ *CURL_GLOBAL_DEFAULT *= *CURL_GLOBAL_ALL *;'
       rm -f ./curl_macs.inc
       echo '  CURLAUTH_ANYSAFE =  not (CURLAUTH_BASIC);'
       echo '  CURL_GLOBAL_ALL = CURL_GLOBAL_SSL or CURL_GLOBAL_WIN32;'
       echo '  CURL_GLOBAL_DEFAULT = CURL_GLOBAL_ALL;'
       echo -e '\n{$IFNDEF CURL_H_INCLUDE}'
       echo -e "$SHIFT$LINE"
    ;;
    'end.')
       echo "$LINE"
       echo '{$ENDIF CURL_H_INCLUDE}'
     ;;
    *)
      echo "$SHIFT$LINE"
      ;;
    esac
  done
} | awk '{ 
sub(/; cdecl; external LIB_CURL name \047[a-z_]+\047;/,"; cdecl; external LIB_CURL;"); 
sub(/^\{\$MODE OBJFPC\}$/, "{$IFDEF FPC}{$MODE OBJFPC}{$ENDIF}");
sub(/^\{\$PACKRECORDS C\}$/, "{$IFDEF FPC}{$PACKRECORDS C}{$ENDIF}");
gsub(/\<pfd_set\>/, "pfdset");
gsub(/\<dword\>/, "longword");
sub(/\<length\>/,"len");
sub(/\<_string\>/,"s");
sub(/\<_para1:pcurl_slist\>/, "list:pcurl_slist");
sub(/\<_para2:pchar\>/, "s:pchar");
sub(/\<_para1:pCURLSH\>/, "sh:pCURLSH");
sub(/\<_para1:CURLversion\>/, "ver:CURLversion");
if ( /\<_para1:[A-Za-z_0-9]+code\>/ ) sub(/\<_para1:/,"code:");
sub(/\<uses Libc;/,
"{$IFDEF WIN32}\n\
uses winsock;\n\
{$ELSE}\n\
uses Libc;\n\
{$ENDIF}\n\
{$ENDIF CURL_H_INCLUDE}\n\
\n\
{$MINENUMSIZE 4}\n\
\n\
{$IFDEF WIN32}\n\
type\n\
  off_t = longint;\n\
  size_t = longword;\n\
  time_t = longint;\n\
  ptime_t = ^time_t;\n\
{$ENDIF}");
print }' >> curl_h.pas

say ""
[ "$SAVE_TEMP_FILES" = "yes" ] || rm -f -v $CURL_H_PREP $CURL_H_MACS $ENUMSORT_TMP



say 'Test-compiling result... \n'
ppc386 -vewh -gl -Crtoi curl_h.pas || exit
say ""
dcc curl_h.pas  || exit;
say ""

rm -f -v curl_h.ppu curl_h.o curl_h.dcu


# ping -c 1 192.168.0.2 > /dev/null &&  say 'Uploading...' && put-local curl_h.pas


say 'done.'

exit
