//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("DCPcppbuilder5.res");
USEPACKAGE("vcl50.bpi");
USEUNIT("DCPbase64.pas");
USEUNIT("DCPblockciphers.pas");
USEUNIT("DCPconst.pas");
USEUNIT("DCPcrypt2.pas");
USEUNIT("DCPreg.pas");
USEUNIT("Ciphers\DCPblowfish.pas");
USEUNIT("Ciphers\DCPcast128.pas");
USEUNIT("Ciphers\DCPcast256.pas");
USEUNIT("Ciphers\DCPdes.pas");
USEUNIT("Ciphers\DCPgost.pas");
USEUNIT("Ciphers\DCPice.pas");
USEUNIT("Ciphers\DCPidea.pas");
USEUNIT("Ciphers\DCPmars.pas");
USEUNIT("Ciphers\DCPmisty1.pas");
USEUNIT("Ciphers\DCPrc2.pas");
USEUNIT("Ciphers\DCPrc4.pas");
USEUNIT("Ciphers\DCPrc5.pas");
USEUNIT("Ciphers\DCPrc6.pas");
USEUNIT("Ciphers\DCPrijndael.pas");
USEUNIT("Ciphers\DCPserpent.pas");
USEUNIT("Ciphers\DCPtea.pas");
USEUNIT("Ciphers\DCPtwofish.pas");
USEUNIT("Hashes\DCPhaval.pas");
USEUNIT("Hashes\DCPmd4.pas");
USEUNIT("Hashes\DCPmd5.pas");
USEUNIT("Hashes\DCPripemd128.pas");
USEUNIT("Hashes\DCPripemd160.pas");
USEUNIT("Hashes\DCPsha1.pas");
USEUNIT("Hashes\DCPsha256.pas");
USEUNIT("Hashes\DCPsha512.pas");
USEUNIT("Hashes\DCPtiger.pas");
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------

//   Package source.
//---------------------------------------------------------------------------

#pragma argsused
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
        return 1;
}
//---------------------------------------------------------------------------
