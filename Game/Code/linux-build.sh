clear
fpc -S2cgi -OG1 -gl -vewnhi -l -Filib/JEDI-SDLv1.0/SDL/Pas/ -Fu/usr/bin/lazarus/components/images/lib/i386-linux/ -Fu/usr/bin/lazarus/lcl/units/i386-linux/ -Fu/usr/bin/lazarus/lcl/units/i386-linux/gtk2/ -Fu/usr/bin/lazarus/packager/units/i386-linux/ -Fu. -oUltraStar -dLCL -dLCLgtk2 UltraStar.lpr

mv ./UltraStar /home/jay/src/ultrastardx/output/
