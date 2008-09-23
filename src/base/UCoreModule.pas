{* UltraStar Deluxe - Karaoke Game
 *
 * UltraStar Deluxe is the legal property of its developers, whose names
 * are too numerous to list here. Please refer to the COPYRIGHT
 * file distributed with this source distribution.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; see the file COPYING. If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 *
 * $URL$
 * $Id$
 *}

unit UCoreModule;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

{*********************
  TCoreModule
  Dummy class that has methods that will be called from core
  In the best case every piece of this software is a module
*********************}
uses
 UPluginDefs;

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
      Destructor Destroy; override;
  end;
  cCoreModule = class of TCoreModule;

implementation

//-------------
// Just the Constructor
//-------------
Constructor TCoreModule.Create;
begin
  //Dummy maaaan ;)
  inherited;
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
Destructor TCoreModule.Destroy;
begin
  //Dummy ftw!!
  inherited;
end;

end.
