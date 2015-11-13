CurlPas is a pascal binding to the libcurl web client library.
TCurl is an object-oriented "wrapper" around CurlPas.

This project was developed and tested on SuSE Linux 9.2, and should be compatible
with most other Linux x86 systems. ( MS-Windows users: see Win32 notes, below )

CurlPas requires a recent version the libcurl library, available from: 
  http://curl.haxx.se/download.html
( Most modern GNU/Linux distributions already include libcurl. )


To build CurlPas with GNU Make, cd into the /curlpas/ directory and type "make"

The makefiles are not compatible with Borland MAKE, if you don't have GNU make
you can build CurlPas on Win32 with "Makewin.bat"

Important: The makefile depends on finding your compiler in the PATH !

To install curlpas, you may either copy the files from the /curlpas/src/ directory
into a location where the compiler can find them, or you can edit your compiler 
configuration to add the /curlpas/src/ directory to the search path.

For information on using curlpas in your own applications, consult the /curlpas/doc/
directories, the curlpas sources, and the curl and libcurl documentation included 
with your curl installation, in particular the man pages on curl, libcurl, 
curl_easy_setopt, and curl_easy_getinfo.


Win32 notes:

1. Using CurlPas on MS-Windows requires the LIBCURL DLL, available from:
     http://curl.haxx.se/download.html
   Be sure to get one flagged "DEVEL" or "LIBCURL", not the statically-linked
   "BINARY". Either the GENERIC, MINGW32 or MSVC versions should work, but the
   "CYGWIN" version is probably not compatible. 

   At the time of this writing, the pascal sources were tested with the generic
   libcurl-7.15.0, where the DLL is  named "libcurl-3.dll", but with some other 
   versions the DLL might be named just "libcurl.dll".  If your application can't 
   find the file, you can either rename the DLL, or adjust the LIB_CURL constant
   ( defined in "curl_h.pas" ) and recompile.
   
   The SSL-enabled version also requires the OpenSSL libraries, you can get them from
   various places, including:
     http://www.paehl.com/open_source/?download=libssl.zip
   OR
     http://www.shininglightpro.com/products/Win32OpenSSL.html

2. Some of the POSIX-style system calls in Windows depend on the DLL "msvcrt.dll"
   I had some problems with this file on my old Win98 machine.
   If you run into trouble with this, try updating the DLL to the latest version,
   along with its cousins, "msvcirt.dll"  and "msvcp60.dll" . 
   Also note that msvcrt.dll uses special threading functions which might not be
   compatible with Delphi's TThread object.

3. To build the Delphi component version of CurlPas, be sure to first type
   the command "makewin dcc" in the \curlpas\ directory (from a DOS window). 
   This will generate the *.res files for the package and pallette icon.

4. CurlPas has been tested on Windows 98. I would like to hear from anyone who
   has success (or problems) with other versions of Windows.


Please contact me with any comments, suggestions, bug reports, fixes, etc...
<yetanothergeek@yahoo.com>


Thanks for trying CurlPas !!!
