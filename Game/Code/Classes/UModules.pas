unit UModules;

interface
{*********************
  UModules
  Unit Contains all used Modules in its uses clausel
  and a const with an array of all Modules to load
*********************}

uses
  UCoreModule,
  UPluginLoader;

const
  CORE_MODULES_TO_LOAD: Array[0..2] of cCoreModule = (
    TPluginLoader,      //First because it has to look if there are Module replacements (Feature o/t Future)
    TCoreModule,        //Remove this later, just a dummy
    TtehPlugins         //Represents the Plugins. Last because they may use CoreModules Services etc.
  );

implementation

end.