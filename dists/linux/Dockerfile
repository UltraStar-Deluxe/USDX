FROM centos:7

RUN yum upgrade -y
# RUN yum install -y --setopt=tsflags=nodocs centos-release-scl

# Base
RUN yum install -y --setopt=tsflags=nodocs \
    make automake autoconf cmake patch gcc gcc-c++ \
    which file git unzip \
    glibc-devel mesa-libGL-devel \
    lua-devel alsa-lib-devel \
    libX11-devel libXext-devel libXcursor-devel libXinerama-devel libXrandr-devel libXi-devel libxkbcommon-devel \
    libwebp-devel libjpeg-turbo-devel libtiff-devel \
    dbus-devel systemd-devel \
    libsamplerate-devel opencv-devel \
    https://sourceforge.net/projects/freepascal/files/Linux/3.0.4/fpc-3.0.4-1.x86_64.rpm

# 32-bit libs
# glibc-devel.i686 libgcc.i686 \
# lua-devel.i686 lua.i686 alsa-lib-devel.i686 \
# libX11-devel.i686 \
# libwebp-devel.i686 libjpeg-turbo-devel.i686 libtiff-devel.i686

RUN yum clean all -y

# install devtoolset-7
# ENTRYPOINT ["scl", "enable", "devtoolset-7"]
