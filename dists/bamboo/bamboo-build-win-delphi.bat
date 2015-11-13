"C:\Program Files\Borland\BDS\4.0\Bin\brc32.exe" -r ./UltraStar.rc

"C:\Program Files\Borland\BDS\4.0\Bin\dcc32.exe" -U"lib\JEDI-SDL\SDL\Pas" -O"lib\JEDI-SDL\SDL\Pas" -I"lib\JEDI-SDL\SDL\Pas" -R"lib\JEDI-SDL\SDL\Pas" UltraStar.dpr
cp UltraStar.exe ..\..\

rem cd ..\..\Installer
rem "C:\Program Files\NSIS\makeNSIS.exe" UltraStarDeluxe.nsi

rem cd ..\Game\Code