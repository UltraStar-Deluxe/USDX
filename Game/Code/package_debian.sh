# This script should be run post-compile
# and i should move files to the correct location for packaging ... ( for DEB package )

rm -fr ../../../deb-package
rm -fr ../../../packages
clear

mkdir ../../../packages

mkdir ../../../deb-package
mkdir ../../../deb-package/DEBIAN
mkdir ../../../deb-package/usr
mkdir ../../../deb-package/usr/share
mkdir ../../../deb-package/usr/share/UltraStarDeluxe
mkdir ../../../deb-package/usr/bin

cp ./UltraStar ../../../deb-package/usr/bin/UltraStarDeluxe

cp -a ../../Themes/ ../../../deb-package/usr/share/UltraStarDeluxe/
cp -a ../../Sounds/ ../../../deb-package/usr/share/UltraStarDeluxe/
cp -a ../../Skins/ ../../../deb-package/usr/share/UltraStarDeluxe/
cp -a ../../Languages/ ../../../deb-package/usr/share/UltraStarDeluxe/

cp UltraStarDeluxe.control ../../../deb-package/DEBIAN/control

cd ../../../

dpkg-deb --build ./deb-package
mv deb-package.deb ./packages/UltraStarDeluxe_1.1_i386.deb

rm -fr ../../../deb-package
