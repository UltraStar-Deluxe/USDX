FROM centos:7

RUN yum upgrade -y && yum install -y --setopt=tsflags=nodocs centos-release-scl

RUN yum install -y --setopt=tsflags=nodocs \
    devtoolset-7 make automake autoconf which file chrpath git unzip \
    glibc-devel lua-devel alsa-lib-devel mesa-libGL-devel libX11-devel libXext-devel \
    dbus-devel systemd-devel ibus-devel fcitx-devel \
    libsamplerate-devel portmidi-devel opencv-devel \
    glibc-devel.i686 libgcc.i686 libX11-devel.i686 lua-devel.i686 lua.i686

RUN yum clean all -y

ENTRYPOINT ["scl", "enable", "devtoolset-7"]
