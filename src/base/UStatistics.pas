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
 *}

unit UStatistics;

interface

{$IFDEF FPC}{$MODE Delphi}{$ENDIF}
{$I switches.inc}

function StatSinceDate: TDateTime;
function StatPeriodText: UTF8String;
procedure RefreshStatPeriod;
procedure CycleStatPeriod;

implementation

uses UIni, ULanguage, SysUtils;

const
  PeriodDays: array[0..4] of integer = (0, 1, 7, 30, 365);
  PeriodText: array[0..4] of string = ('STAT_PERIOD_ALL', 'STAT_PERIOD_DAY',
      'STAT_PERIOD_WEEK', 'STAT_PERIOD_MONTH', 'STAT_PERIOD_YEAR');
var
  SinceDate: TDateTime;

function StatSinceDate: TDateTime;
begin
  Result := SinceDate;
end;

function StatPeriodText: UTF8String;
begin
  Result := Language.Translate(PeriodText[Ini.StatPeriod]);
end;

procedure RefreshStatPeriod;
begin
  if PeriodDays[Ini.StatPeriod] = 0 then
    SinceDate := 0
  else
    SinceDate := Now - PeriodDays[Ini.StatPeriod];
end;

procedure CycleStatPeriod;
begin
  Ini.StatPeriod := (Ini.StatPeriod + 1) mod Length(PeriodDays);
  RefreshStatPeriod;
  Ini.SaveStatPeriod;
end;

end.
