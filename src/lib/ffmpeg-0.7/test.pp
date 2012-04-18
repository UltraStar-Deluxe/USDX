
unit test;
interface

{
  Automatically converted by H2Pas 1.0.0 from test.h
  The following command line parameters were used:
    test.h
}

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}


{$include <stdint.h>}
{$include <limits.h>}
{$include "attributes.h"}
  {/< numerator }
  {/< denominator }

  type
    AVRational = record
        num : longint;
        den : longint;
      end;
(* error 
	int tmp;
 in declarator_list *)
(* error 
    if(tmp) return ((tmp ^ a.den ^ b.den)>>63)|1;
 in declarator_list *)
(* error 
    else if(b.den && a.den) return 0;
 in declarator_list *)
(* error 
    else if(a.num && b.num) return (a.num>>31) - (b.num>>31);
 in declarator_list *)
(* error 
    else                    return INT_MIN;
 in declarator_list *)
(* error 
}

implementation


end.
