unit UModules;

interface
{*********************
  UModules
  Unit Contains all used Modules in its uses clausel
  and a const with an array of all Modules to load
*********************}

uses
  UCoreModule;

const
  CORE_MODULES_TO_LOAD: Array[0..0] of cCoreModule = (
    TCoreModule //Remove this later, just a dummy
  );

implementation

end.