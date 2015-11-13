{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals 4.00                                        }
{   File name:        cDefines.inc                                             }
{   Home page:        http://fundementals.sourceforge.net                      }
{   Copyright:        Copyright © 1998-2007, David J Butler                    }
{                     All rights reserved.                                     }
{                                                                              }
{ Conditionals defined elsewhere:                                              }
{                                                                              }
{   DEBUG             - Enable debug mode.                                     }
{                       Sets compiler options for debug information,           }
{                       range/overflow checking, assertions, etc.              }
{                       If not defined, set compiler options for release.      }
{   PUREPASCAL        - Disable assembler optimisations.                       }
{                       All code is compiled in pure Pascal for maximum        }
{                       portability.                                           }
{                                                                              }
{ Conditionals defined here:                                                   }
{                                                                              }
{   FUNDAMENTALS      - Fundamentals Library.                                  }
{     FUNDAMENTALS4   - Fundamentals Library 4.x.                              }
{                                                                              }
{   DELPHI            - Delphi compiler.                                       }
{     DELPHI1         - Delphi 1.                                              }
{     DELPHI2         - Delphi 2.                                              }
{     DELPHI3         - Delphi 3.                                              }
{     DELPHI4         - Delphi 4.                                              }
{     DELPHI5         - Delphi 5.                                              }
{     DELPHI6         - Delphi 6.                                              }
{     DELPHI7         - Delphi 7.                                              }
{     DELPHI8         - Delphi 8.                                              }
{     DELPHI2005      - Delphi 2005.                                           }
{     DELPHI2006      - Delphi 2006.                                           }
{     DELPHI2007      - Delphi 2007.                                           }
{     DELPHI2009      - Delphi 2009.                                           }
{     DELPHI3_UP      - Delphi 3 or later.                                     }
{     DELPHI4_UP      - Delphi 4 or later.                                     }
{     DELPHI5_UP      - Delphi 5 or later.                                     }
{     DELPHI6_UP      - Delphi 6 or later.                                     }
{     DELPHI7_UP      - Delphi 7 or later.                                     }
{     DELPHI8_UP      - Delphi 8 or later.                                     }
{     DELPHI2005_UP   - Delphi 2005 or later.                                  }
{     DELPHI2006_UP   - Delphi 2006 or later.                                  }
{     DELPHI2007_UP   - Delphi 2007 or later.                                  }
{     DELPHI2009_UP   - Delphi 2009 or later.                                  }
{     DELPHI3_DOWN    - Delphi 3 or earlier.                                   }
{     DELPHI4_DOWN    - Delphi 4 or earlier.                                   }
{     DELPHI5_DOWN    - Delphi 5 or earlier.                                   }
{     DELPHI6_DOWN    - Delphi 6 or earlier.                                   }
{   KYLIX             - Kylix compiler.                                        }
{   BCB               - Borland C++ Builder compiler.                          }
{   FREEPASCAL        - Free Pascal compiler.                                  }
{     FREEPASCAL1     - FreePascal 1.x                                         }
{     FREEPASCAL2     - FreePascal 2.x                                         }
{     FREEPASCAL2_UP  - FreePascal 2.x or later.                               }
{                                                                              }
{   SupportCardinal   - Compiler supports Cardinal type.                       }
{   SupportDynArray   - Compiler supports dynamic arrays.                      }
{   SupportInt64      - Compiler supports Int64 type.                          }
{   SupportWideChar   - Compiler supports WideChar type.                       }
{   SupportCurrency   - Compiler supports Currency type.                       }
{   SupportInline     - Compiler supports inline functions / procedures.       }
{                                                                              }
{   OS_UNIX           - Unix type operating system.                            }
{     OS_BSD          - BSD type operating system.                             }
{       OS_FREEBSD    - FreeBSD operating system.                              }
{       OS_NETBSD     - NetBSD operating system.                               }
{       OS_OPENBSD    - OpenBSD operating system.                              }
{     OS_LINUX        - Linux operating system.                                }
{   OS_MSWIN          - MS Windows type operating system.                      }
{     OS_WIN16        - Win16 operating system.                                }
{     OS_WIN32        - Win32 operating system.                                }
{     OS_WIN64        - Win64 operating system.                                }
{   OS_MACOS          - MacOS operating system.                                }
{   OS_OS2            - OS/2 operating system.                                 }
{   OS_DOS            - DOS operating system.                                  }
{     OS_DOS32        - DOS operating system with 32-bit extender.             }
{                                                                              }
{   ManagedCode       - Compiles managed code.                                 }
{     DOT_NET         - Compiles .NET code.                                    }
{                                                                              }
{   NativeCode        - Compiles native code.                                  }
{     CPU_INTEL386    - Intel 386 compatible CPU.                              }
{     CPU_68K         - Motorola 68000 compatible CPU.                         }
{     CPU_POWERPC     - PowerPC 32-bit CPU.                                    }
{                                                                              }
{   WindowsPlatform   - Win32/Win64/.NET code on a Windows platform.           }
{                                                                              }
{   ASM386_DELPHI     - Compiler supports 386 assembler (Delphi style)         }
{   ASM386_FREEPASCAL - Compiler supports 386 assembler (FreePascal style)     }
{   ASM386            - Compiler supports 386 assembler (Generic)              }
{                                                                              }
{******************************************************************************}



{                                                                              }
{ Fundamentals Library                                                         }
{                                                                              }
{$DEFINE FUNDAMENTALS}
{$DEFINE FUNDAMENTALS4}



{                                                                              }
{ Delphi 1                                                                     }
{                                                                              }
{$IFDEF VER80}
  {$DEFINE DELPHI}
  {$DEFINE DELPHI1}
  {$DEFINE DELPHI3_DOWN}
  {$DEFINE DELPHI4_DOWN}
  {$DEFINE DELPHI5_DOWN}
  {$DEFINE DELPHI6_DOWN}
  {$DEFINE WIN16}
{$ENDIF}



{                                                                              }
{ Delphi 2                                                                     }
{                                                                              }
{$IFDEF VER90}
  {$DEFINE DELPHI}
  {$DEFINE DELPHI2}
  {$DEFINE DELPHI3_DOWN}
  {$DEFINE DELPHI4_DOWN}
  {$DEFINE DELPHI5_DOWN}
  {$DEFINE DELPHI6_DOWN}
  {$DEFINE WIN32}
  {$DEFINE CPU386}
{$ENDIF}
{$IFDEF VER93}
  {$DEFINE BCB}
  {$DEFINE BCB1}
{$ENDIF}



{                                                                              }
{ Delphi 3                                                                     }
{                                                                              }
{$IFDEF VER100}
  {$DEFINE DELPHI}
  {$DEFINE DELPHI3}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI3_DOWN}
  {$DEFINE DELPHI4_DOWN}
  {$DEFINE DELPHI5_DOWN}
  {$DEFINE DELPHI6_DOWN}
  {$DEFINE WIN32}
  {$DEFINE CPU386}
{$ENDIF}
{$IFDEF VER110}
  {$DEFINE BCB}
  {$DEFINE BCB3}
{$ENDIF}



{                                                                              }
{ Delphi 4                                                                     }
{                                                                              }
{$IFDEF VER120}
  {$DEFINE DELPHI}
  {$DEFINE DELPHI4}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI4_UP}
  {$DEFINE DELPHI4_DOWN}
  {$DEFINE DELPHI5_DOWN}
  {$DEFINE DELPHI6_DOWN}
  {$DEFINE WIN32}
  {$DEFINE CPU386}
{$ENDIF}
{$IFDEF VER125}
  {$DEFINE BCB}
  {$DEFINE BCB4}
{$ENDIF}



{                                                                              }
{ Delphi 5                                                                     }
{                                                                              }
{$IFDEF VER130}
  {$DEFINE DELPHI}
  {$DEFINE DELPHI5}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI4_UP}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI5_DOWN}
  {$DEFINE DELPHI6_DOWN}
  {$DEFINE WIN32}
  {$DEFINE CPU386}
{$ENDIF}
{$IFDEF VER135}
  {$DEFINE BCB}
  {$DEFINE BCB5}
{$ENDIF}



{                                                                              }
{ Delphi 6                                                                     }
{                                                                              }
{$IFDEF VER140}
  {$DEFINE DELPHI}
  {$DEFINE DELPHI6}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI4_UP}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI6_DOWN}
  {$IFDEF LINUX}
    {$DEFINE KYLIX}
  {$ENDIF}
{$ENDIF}



{                                                                              }
{ Delphi 7                                                                     }
{                                                                              }
{$IFDEF VER150}
  {$DEFINE DELPHI}
  {$DEFINE DELPHI7}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI4_UP}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI7_UP}
  {$IFDEF LINUX}
    {$DEFINE KYLIX}
  {$ENDIF}
{$ENDIF}



{                                                                              }
{ Delphi 8                                                                     }
{                                                                              }
{$IFDEF VER160}
  {$DEFINE DOT_NET}
  {$DEFINE DELPHI}
  {$DEFINE DELPHI8}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI4_UP}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI7_UP}
  {$DEFINE DELPHI8_UP}
{$ENDIF}



{                                                                              }
{ Delphi 2005                                                                  }
{                                                                              }
{$IFDEF VER170}
  {$DEFINE DELPHI}
  {$DEFINE DELPHI2005}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI4_UP}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI7_UP}
  {$DEFINE DELPHI8_UP}
  {$DEFINE DELPHI2005_UP}
{$ENDIF}



{                                                                              }
{ Delphi 2006                                                                  }
{ Delphi 2007                                                                  }
{                                                                              }
{$IFDEF VER180}
  {$DEFINE DELPHI}
  {$DEFINE DELPHI2006}
  {$DEFINE DELPHI2007}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI4_UP}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI7_UP}
  {$DEFINE DELPHI8_UP}
  {$DEFINE DELPHI2005_UP}
  {$DEFINE DELPHI2006_UP}
  {$DEFINE DELPHI2007_UP}
{$ENDIF}
{$IFDEF VER185}
  {$DEFINE DELPHI}
  {$DEFINE DELPHI2007}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI4_UP}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI7_UP}
  {$DEFINE DELPHI8_UP}
  {$DEFINE DELPHI2005_UP}
  {$DEFINE DELPHI2006_UP}
  {$DEFINE DELPHI2007_UP}
{$ENDIF}



{                                                                              }
{ Delphi 2007 for .NET                                                         }
{                                                                              }
{$IFDEF VER190}
  {$DEFINE DOT_NET}
  {$DEFINE DELPHI}
  {$DEFINE DELPHI2007}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI4_UP}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI7_UP}
  {$DEFINE DELPHI8_UP}
  {$DEFINE DELPHI2005_UP}
  {$DEFINE DELPHI2006_UP}
  {$DEFINE DELPHI2007_UP}
{$ENDIF}



{                                                                              }
{ Delphi 2009                                                                  }
{                                                                              }
{$IFDEF VER200}
  {$DEFINE DELPHI}
  {$DEFINE DELPHI2009}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI4_UP}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI7_UP}
  {$DEFINE DELPHI8_UP}
  {$DEFINE DELPHI2005_UP}
  {$DEFINE DELPHI2006_UP}
  {$DEFINE DELPHI2007_UP}
  {$DEFINE DELPHI2009_UP}
{$ENDIF}



{                                                                              }
{ Future Delphi versions                                                       }
{                                                                              }
{$IFDEF VER205}
  {$DEFINE DELPHI_Future}
{$ENDIF}
{$IFDEF VER210}
  {$DEFINE DELPHI_Future}
{$ENDIF}
{$IFDEF VER215}
  {$DEFINE DELPHI_Future}
{$ENDIF}
{$IFDEF VER220}
  {$DEFINE DELPHI_Future}
{$ENDIF}
{$IFDEF VER225}
  {$DEFINE DELPHI_Future}
{$ENDIF}
{$IFDEF DELPHI_Future}
  {$DEFINE DELPHI}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI4_UP}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI7_UP}
  {$DEFINE DELPHI8_UP}
  {$DEFINE DELPHI2005_UP}
  {$DEFINE DELPHI2006_UP}
  {$DEFINE DELPHI2007_UP}
  {$DEFINE DELPHI2009_UP}
{$ENDIF}



{                                                                              }
{ Free Pascal                                                                  }
{                                                                              }
{$IFDEF FPC}
  {$DEFINE FREEPASCAL}
  {$MODE DELPHI}
  {$IFDEF CPUI386}
    {$DEFINE CPU386}
  {$ENDIF}
  {$IFDEF CPU68}
    {$DEFINE CPU68K}
  {$ENDIF}
  {$IFDEF VER1}
    {$DEFINE FREEPASCAL1}
  {$ELSE}
    {$DEFINE FREEPASCAL2_UP}
    {$IFDEF VER2}
      {$DEFINE FREEPASCAL2}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}



{                                                                              }
{ Supported language features                                                  }
{                                                                              }
{$IFDEF DELPHI3_UP}
  {$DEFINE SupportCardinal}
  {$DEFINE SupportCurrency}
{$ENDIF}
{$IFDEF DELPHI4_UP}
  {$DEFINE SupportInt64}
  {$DEFINE SupportWideChar}
  {$DEFINE SupportDynArray}
{$ENDIF}
{$IFDEF FREEPASCAL2_UP}
  {$DEFINE SupportCardinal}
  {$DEFINE SupportCurrency}
  {$DEFINE SupportInt64}
  {$DEFINE SupportWideChar}
  {$DEFINE SupportDynArray}
{$ENDIF}
{$IFDEF DELPHI2005_UP}
  {$DEFINE SupportInline}
  {$DEFINE SupportOperatorOverloading}
{$ENDIF}

{$IFDEF SupportInline}
  {$DEFINE UseInline}
{$ENDIF}



{                                                                              }
{ Operating system type                                                        }
{                                                                              }

{ Windows }
{$IFDEF WIN16}
  {$DEFINE OS_WIN16}
{$ENDIF}
{$IFDEF WIN32}
  {$DEFINE OS_WIN32}
{$ENDIF}
{$IFDEF WIN64}
  {$DEFINE OS_WIN64}
{$ENDIF}
{$IFDEF OS_WIN16}
  {$DEFINE OS_MSWIN}
{$ENDIF}
{$IFDEF OS_WIN32}
  {$DEFINE OS_MSWIN}
{$ENDIF}
{$IFDEF OS_WIN64}
  {$DEFINE OS_MSWIN}
{$ENDIF}
{$IFDEF MSWINDOWS}
  {$DEFINE OS_MSWIN}
{$ENDIF}
{$IFDEF OS_MSWIN}
  {$DEFINE MSWIN}
{$ENDIF}

{ Unix-family }
{$IFDEF LINUX}
  {$DEFINE UNIX}
  {$DEFINE OS_LINUX}
{$ENDIF}
{$IFDEF FREEBSD}
  {$DEFINE BSD}
  {$DEFINE OS_FREEBSD}
{$ENDIF}
{$IFDEF NETBSD}
  {$DEFINE BSD}
  {$DEFINE OS_NETBSD}
{$ENDIF}
{$IFDEF OPENBSD}
  {$DEFINE BSD}
  {$DEFINE OS_OPENBSD}
{$ENDIF}
{$IFDEF BSD}
  {$DEFINE UNIX}
  {$DEFINE OS_BSD}
{$ENDIF}
{$IFDEF UNIX}
  {$DEFINE OS_UNIX}
{$ENDIF}

{ Other }
{$IFDEF MACOS}
  {$DEFINE OS_MACOS}
{$ENDIF}
{$IFDEF OS2}
  {$DEFINE OS_OS2}
{$ENDIF}
{$IFDEF DOS32}
  {$DEFINE DOS}
  {$DEFINE OS_DOS32}
{$ENDIF}
{$IFDEF DOS}
  {$DEFINE OS_DOS}
{$ENDIF}



{                                                                              }
{ .NET                                                                         }
{                                                                              }
{$IFDEF CLR}
  {$DEFINE DOT_NET}
{$ENDIF}
{$IFDEF DOT_NET}
  {$DEFINE ManagedCode}
{$ENDIF}



{                                                                              }
{ Windows platform                                                             }
{                                                                              }
{$IFDEF DOT_NET}
  {$DEFINE WindowsPlatform}
{$ENDIF}
{$IFDEF OS_WIN32}
  {$DEFINE WindowsPlatform}
{$ENDIF}
{$IFDEF OS_WIN64}
  {$DEFINE WindowsPlatform}
{$ENDIF}



{                                                                              }
{ CPU type                                                                     }
{                                                                              }
{$IFNDEF ManagedCode}
  {$DEFINE NativeCode}
{$ENDIF}
{$IFDEF CPU386}
  {$DEFINE INTEL386}
  {$DEFINE CPU_INTEL386}
{$ENDIF}
{$IFDEF CPU68K}
  {$DEFINE CPU_68K}
{$ENDIF}
{$IFDEF CPUPPC}
  {$DEFINE CPU_POWERPC}
{$ENDIF}



{                                                                              }
{ Assembler style                                                              }
{                                                                              }
{$IFNDEF PUREPASCAL}
  {$IFNDEF ManagedCode}
    {$IFDEF CPU_INTEL386}
      {$DEFINE ASM386}
      {$IFDEF DELPHI}{$IFDEF OS_WIN32}
        {$DEFINE ASM386_DELPHI}
        {$IFNDEF UseInline}
          {$DEFINE ASM386_DELPHI_INLINE_OFF}
        {$ENDIF}
      {$ENDIF}{$ENDIF}
      {$IFDEF FREEPASCAL2_UP}
        {$DEFINE ASM386_FREEPASCAL}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}



{                                                                              }
{ Compiler warnings                                                            }
{                                                                              }
{$IFDEF DELPHI7}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CAST OFF}
{$ENDIF}

{$IFDEF DOT_NET}
  {$WARN UNIT_PLATFORM OFF}
{$ENDIF}

{$IFNDEF DEBUG}
  {$IFDEF DELPHI6_UP}
    {$WARN SYMBOL_PLATFORM OFF}
    {$WARN UNIT_PLATFORM OFF}
    {$WARN UNIT_DEPRECATED OFF}
  {$ENDIF}
{$ENDIF}



{                                                                              }
{ Standard compiler directives                                                 }
{                                                                              }
{$EXTENDEDSYNTAX ON}
{$IOCHECKS ON}
{$LONGSTRINGS ON}
{$BOOLEVAL OFF}
{$IFDEF DELPHI}
  {$OPTIMIZATION ON}
{$ENDIF}
{$WRITEABLECONST OFF}
{$IFDEF DEBUG}
  {$ASSERTIONS ON}
  {$DEBUGINFO ON}
  {$OVERFLOWCHECKS ON}
  {$RANGECHECKS ON}
  {$WARNINGS ON}
  {$HINTS ON}
{$ELSE}
  {$ASSERTIONS OFF}
  {$DEBUGINFO OFF}
  {$OVERFLOWCHECKS OFF}
  {$RANGECHECKS OFF}
  {$WARNINGS OFF}
  {$HINTS OFF}
{$ENDIF}
{$IFDEF CLR}
  {$UNSAFECODE OFF}
{$ENDIF}
{$IFDEF DELPHI2005_UP}
  {$INLINE ON}
{$ENDIF}

