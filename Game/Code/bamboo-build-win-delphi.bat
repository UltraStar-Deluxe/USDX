"C:\Program Files\Borland\BDS\4.0\Bin\dcc32.exe" -U"lib\JEDI-SDLv1.0\SDL\Pas" -O"lib\JEDI-SDLv1.0\SDL\Pas" -I"lib\JEDI-SDLv1.0\SDL\Pas" -R"lib\JEDI-SDLv1.0\SDL\Pas" UltraStar.dpr
cp UltraStar.exe ..\..\

cd ..\..\Installer
"C:\Program Files\NSIS\makeNSIS.exe" UltraStarDeluxe.nsi

cd ..\Game\Code