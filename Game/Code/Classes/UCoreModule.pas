unit UCoreModule;

interface
{*********************
  TCoreModule
  Dummy Class that has Methods that will be called from Core
  In the Best case every Piece of this Software is a Module
*********************}
uses UPluginDefs;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

type
  PCoreModule = ^TCoreModule;
  TCoreModule = class
    public
      Constructor Create; virtual;
      
      //Function that gives some Infos about the Module to the Core
      Procedure Info(const pInfo: PModuleInfo); virtual;

      //Is Called on Loading.
      //In this Method only Events and Services should be created
      //to offer them to other Modules or Plugins during the Init process
      //If False is Returned this will cause a Forced Exit
      Function Load: Boolean; virtual;

      //Is Called on Init Process
      //In this Method you can Hook some Events and Create + Init
      //your Classes, Variables etc.
      //If False is Returned this will cause a Forced Exit
      Function Init: Boolean; virtual;

      //Is Called during Mainloop before 'Core/MainLoop' Hook and Drawing
      //If False is Returned this will cause a Forced Exit
      Function MainLoop: Boolean; virtual;

      //Is Called if this Module has been Inited and there is a Exit.
      //Deinit is in backwards Initing Order
      //If False is Returned this will cause a Forced Exit
      Procedure DeInit; virtual;

      //Is Called if this Module will be unloaded and has been created
      //Should be used to Free Memory
      Procedure Free; virtual;
  end;
  cCoreModule = class of TCoreModule;

implementation

//-------------
// Just the Constructor
//-------------
Constructor TCoreModule.Create;
begin
  //Dummy maaaan ;)
end;

//-------------
// Function that gives some Infos about the Module to the Core
//-------------
Procedure TCoreModule.Info(const pInfo: PModuleInfo);
begin
  pInfo^.Name := 'Not Set';
  pInfo^.Version := 0;
  pInfo^.Description := 'Not Set';
end;

//-------------
//Is Called on Loading.
//In this Method only Events and Services should be created
//to offer them to other Modules or Plugins during the Init process
//If False is Returned this will cause a Forced Exit
//-------------
Function TCoreModule.Load: Boolean;
begin
  //Dummy ftw!!
  Result := True;
end;

//-------------
//Is Called on Init Process
//In this Method you can Hook some Events and Create + Init
//your Classes, Variables etc.
//If False is Returned this will cause a Forced Exit
//-------------
Function TCoreModule.Init: Boolean;
begin
  //Dummy ftw!!
  Result := True;
end;

//-------------
//Is Called during Mainloop before 'Core/MainLoop' Hook and Drawing
//If False is Returned this will cause a Forced Exit
//-------------
Function TCoreModule.MainLoop: Boolean;
begin
  //Dummy ftw!!
  Result := True;
end;

//-------------
//Is Called if this Module has been Inited and there is a Exit.
//Deinit is in backwards Initing Order
//-------------
Procedure TCoreModule.DeInit;
begin
  //Dummy ftw!!
end;

//-------------
//Is Called if this Module will be unloaded and has been created
//Should be used to Free Memory
//-------------
Procedure TCoreModule.Free;
begin
  //Dummy ftw!!
end;

end.