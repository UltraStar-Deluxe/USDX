svn update

clear
fpc  -S2cgi -OG1 -gl -vewnhi -l -Filib/JEDI-SDLv1.0/SDL/Pas/ -Fu/usr/lib/lazarus/components/images/lib/i386-linux/ -Fu/usr/lib/lazarus/lcl/units/i386-linux/ -Fu/usr/lib/lazarus/lcl/units/i386-linux/gtk2/ -Fu/usr/lib/lazarus/packager/units/i386-linux/ -Fu. -oUltraStar -dLCL -dLCLgtk2 UltraStar.lpr

#mv ./UltraStar /home/jay/src/ultrastardx/output/
