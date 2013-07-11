FFMPEG_VERSION=1.0

all: ffmpeg_c ffmpeg_pas
	@./ffmpeg_c >ffmpeg_c.txt
	@./ffmpeg_pas >ffmpeg_pas.txt
	@diff ffmpeg_c.txt ffmpeg_pas.txt

ffmpeg_c: ffmpeg_c.c
	@gcc -o $@ $^

ffmpeg_pas: ffmpeg_pas.pas
	@fpc -v0 -Fi../../src $^ >/dev/null 2>&1

ffmpeg_c.c:
	@FFMPEG_VERSION=$(FFMPEG_VERSION) ./find_avtypes

ffmpeg_pas.pas:
	@FFMPEG_VERSION=$(FFMPEG_VERSION) ./find_avtypes

.PHONY: clean
clean:
	@rm -f ffmpeg_c* ffmpeg_pas*
