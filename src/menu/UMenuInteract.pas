unit UMenuInteract;

interface

{$I switches.inc}

type
  TInteract = record // for moving thru menu
    Typ:    integer;  // 0 - button, 1 - select, 2 - Text, 3 - Select SLide, 5 - ButtonCollection Child
    Num:    integer;  // number of this item in proper list like buttons, selects
  end;

implementation

end.
 