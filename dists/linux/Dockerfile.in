FROM %%from%%

RUN yum upgrade -y --setopt=tsflags=nodocs

RUN yum install -y --setopt=tsflags=nodocs \
    make automake autoconf cmake patch which file git unzip gcc gcc-c++\
    glibc-devel mesa-libGL-devel lua-devel alsa-lib-devel \
    libX11-devel libXext-devel libXcursor-devel libXinerama-devel libXrandr-devel libXi-devel libxkbcommon-devel \
    libwebp-devel libjpeg-turbo-devel libtiff-devel \
    dbus-devel systemd-devel \
    libsamplerate-devel opencv-devel

RUN yum install -y --setopt=tsflags=nodocs \
	"%%fpcpackage%%"

RUN yum clean all -y

CMD ["bash"]
