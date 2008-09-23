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

unit UModules;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

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