unit UCoreModule;

interface
{*********************
  TCoreModule
  Dummy Class that has Methods that will be called from Core
  In the Best case every Piece of this Software is a Module
*********************}

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

type
  PModuleInfo = ^TModuleInfo;
  TModuleInfo = record
    Name:         String;
    Version:      LongWord;
    Description:  String;
  end;

  TCoreModule = class
    public
      //Function that gives some Infos about the Module to the Core
      Procedure Info(const pInfo: PModuleInfo);

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
  end;
  cCoreModule = class of TCoreModule;

implementation

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

end.