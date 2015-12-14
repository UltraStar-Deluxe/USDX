/*
    This file is part of Acinerella.

    Acinerella is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Acinerella is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Acinerella.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef VIDEOPLAY_H
#define VIDEOPLAY_H

#include <stdint.h>
#include <stdio.h>
#include <stdbool.h>

#ifdef _WIN32
#define CALL_CONVT __cdecl
#else
#define CALL_CONVT 
#endif

typedef long long int64;

/*Defines the type of an Acinerella media stream. Currently only video and
 audio streams are supported, subtitle and data streams will be marked as
 "unknown".*/
enum _ac_stream_type {
  /*The type of the media stream is not known. This kind of stream can not be
      decoded.*/
  AC_STREAM_TYPE_VIDEO = 0,
  /*This media stream is a video stream.*/
  AC_STREAM_TYPE_AUDIO = 1,
  /*This media stream is an audio stream.*/  
  AC_STREAM_TYPE_UNKNOWN = -1
};

typedef enum _ac_stream_type ac_stream_type;

/*Defines the type of an Acinerella media decoder.*/
enum _ac_decoder_type {
  /*This decoder is used to decode a video stream.*/
  AC_DECODER_TYPE_VIDEO = 0,
  /* This decoder is used to decode an audio stram.*/
  AC_DECODER_TYPE_AUDIO = 1
};

typedef enum _ac_decoder_type ac_decoder_type;

/*Defines the format video/image data is outputted in*/
enum _ac_output_format {
  AC_OUTPUT_RGB24 = 0,
  AC_OUTPUT_BGR24 = 1,
  AC_OUTPUT_RGBA32 = 2,
  AC_OUTPUT_BGRA32 = 3
};

typedef enum _ac_output_format ac_output_format;

/*Contains information about the whole file/stream that has been opened. Default values are "" 
for strings and -1 for integer values.*/
struct _ac_file_info { 
  /*Information about the file*/
  char title[512];
  char author[512];
  char copyright[512];
  char comment[512];
  char album[512];
  int year;
  int track;
  char genre[32]; 
  
  /*Length of the file*/
  int64_t duration;  
 
  /*Bitrate*/
  int bitrate;
};

typedef struct _ac_file_info ac_file_info;
typedef ac_file_info* lp_ac_file_info;

/*TAc_instance represents an Acinerella instance. Each instance can open and
 decode one file at once. There can be only 26 Acinerella instances opened at
 once.*/
struct _ac_instance {
  /*If true, the instance currently opened a media file.*/
  bool opened;
  /*Contains the count of streams the media file has. This value is available
   after calling the ac_open function.*/
  int stream_count;  
  /*Set this value to change the image output format */
  ac_output_format output_format;
  /*Contains information about the opened stream/file*/
  ac_file_info info;  
};

typedef struct _ac_instance ac_instance;
/*Pointer on the Acinerella instance record.*/
typedef ac_instance* lp_ac_instance;

/*Contains information about an Acinerella video stream.*/
struct _ac_video_stream_info {
  /*The width of one frame.*/ 
  int frame_width;
  /*The height of one frame.*/
  int frame_height;
  /*The width of one pixel. 1.07 for 4:3 format, 1,42 for the 16:9 format*/
  float pixel_aspect;
  /*Frames per second that should be played.*/  
  double frames_per_second;
};

/*Contains information about an Acinerella audio stream.*/
struct _ac_audio_stream_info {
  /*Samples per second. Default values are 44100 or 48000.*/
  int samples_per_second;
  /*Bits per sample. Can be 8 or 16 Bit.*/
  int bit_depth;
  /*Count of channels in the audio stream.*/
  int channel_count;
};

/*Additional info about the stream - use "video_info" when the stream
  is an video stream, "audio_info" when the stream is an audio stream.*/
union _ac_additional_stream_info {
  struct _ac_audio_stream_info audio_info;
  struct _ac_video_stream_info video_info;
};

/*Contains information about an Acinerella stream.*/
struct _ac_stream_info {
  /*Contains the type of the stream.*/
  ac_stream_type stream_type;
  union _ac_additional_stream_info additional_info;
};

typedef struct _ac_stream_info ac_stream_info;
/*Pointer on TAc_stream_info*/
typedef ac_stream_info* lp_ac_stream_info;

/*Contains information about an Acinerella video/audio decoder.*/
struct _ac_decoder {
  /*Pointer on the Acinerella instance*/
  lp_ac_instance pacInstance;
  /*Contains the type of the decoder.*/
  ac_decoder_type type;
  
  /*The timecode of the currently decoded picture in seconds */
  double timecode;
  
  double video_clock;
  
  /*Contains information about the stream the decoder is attached to.*/
  ac_stream_info stream_info;
  /*The index of the stream the decoder is attached to.*/
  int stream_index;
  
  /*Pointer to the buffer which contains the data.*/
  char *pBuffer;  
  /*Size of the data in the buffer.*/  
  int buffer_size;  
};

typedef struct _ac_decoder ac_decoder;
/*Pointer on TAc_decoder.*/
typedef ac_decoder* lp_ac_decoder;

/*Contains information about an Acinerella package.*/
struct _ac_package {
  /*The stream the package belongs to.*/
  int stream_index;
};

typedef struct _ac_package ac_package;
/*Pointer on TAc_package*/
typedef ac_package* lp_ac_package;

typedef void* lp_ac_proberesult;

/*Callback function used to ask the application to read data. Should return
   the number of bytes read or an value smaller than zero if an error occured.*/
typedef int CALL_CONVT (*ac_read_callback)(void *sender, char *buf, int size);
/*Callback function used to ask the application to seek. return 0 if succeed , -1 on failure.*/
typedef int64_t CALL_CONVT (*ac_seek_callback)(void *sender, int64_t pos, int whence);
/*Callback function that is used to notify the application when the data stream
   is opened or closed. For example the file pointer should be resetted to zero
   when the "open" function is called.*/
typedef int CALL_CONVT (*ac_openclose_callback)(void *sender);

typedef void* CALL_CONVT (*ac_malloc_callback)(size_t size);
typedef void* CALL_CONVT (*ac_realloc_callback)(void *ptr, size_t size);
typedef void CALL_CONVT (*ac_free_callback)(void *ptr);

/*Initializes an Acinerella instance.*/
extern lp_ac_instance CALL_CONVT ac_init(void);
extern void CALL_CONVT ac_free(lp_ac_instance pacInstance);

/*Opens a media file.
 @param(inst specifies the Acinerella Instance the stream should be opened for)
 @param(sender specifies a pointer that is sent to all callback functions to
  allow you to do object orientated programming. May be NULL.)
 @param(open_proc specifies the callback function that is called, when the
  media file is opened. May be NULL.)
 @param(seek_proc specifies the callback function that is called, when the ffmpeg decoder
  wants to seek in the file. May be NULL)
 @param(close_proc specifies the callback function that is called when the media
  file is closed. May be NULL.)*/
extern int CALL_CONVT ac_open(
  lp_ac_instance pacInstance,
  void *sender, 
  ac_openclose_callback open_proc,
  ac_read_callback read_proc,
  ac_seek_callback seek_proc,
  ac_openclose_callback close_proc,
  lp_ac_proberesult proberesult);
/*Closes an opened media file.*/
extern void CALL_CONVT ac_close(lp_ac_instance pacInstance);
  
/*Stores information in "pInfo" about stream number "nb".*/
extern void CALL_CONVT ac_get_stream_info(lp_ac_instance pacInstance, int nb, lp_ac_stream_info info);

/*Reads a package from an opened media file.*/
extern lp_ac_package CALL_CONVT ac_read_package(lp_ac_instance pacInstance);
/*Frees a package that has been read.*/
extern void CALL_CONVT ac_free_package(lp_ac_package pPackage);

/*Creates an decoder for the specified stream number. Returns NIL if no decoder
 could be found.*/
extern lp_ac_decoder CALL_CONVT ac_create_decoder(lp_ac_instance pacInstance, int nb);
/*Frees an created decoder.*/
extern void CALL_CONVT ac_free_decoder(lp_ac_decoder pDecoder);
/*Decodes a package using the specified decoder. The decodec data is stored in the
 "buffer" property of the decoder.*/
extern int CALL_CONVT ac_decode_package(lp_ac_package pPackage, lp_ac_decoder pDecoder);
extern int CALL_CONVT ac_drop_decode_package(lp_ac_package pPackage, lp_ac_decoder pDecoder);

/*Seeks to the given target position in the file. The seek funtion is not able to seek a single audio/video stream
but seeks the whole file forward. The stream number paremter (nb) is only used for the timecode reference.
The parameter "dir" specifies the seek direction: 0 for forward, -1 for backward.
The target_pos paremeter is in milliseconds. Returns 1 if the functions succeded.*/
extern int CALL_CONVT ac_seek(lp_ac_decoder pDecoder, int dir, int64_t target_pos);

extern lp_ac_proberesult CALL_CONVT ac_probe_input_buffer(char* buf, int bufsize, char* filename, int* score_max);

#endif /*VIDEOPLAY_H*/
