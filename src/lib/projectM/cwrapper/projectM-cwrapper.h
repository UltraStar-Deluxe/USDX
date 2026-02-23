#ifndef __PROJECTM_CWRAPPER_H__
#define __PROJECTM_CWRAPPER_H__

#include "projectM.hpp"

// PROJECTM_VERSION define is not very helpful, lets create our own
#define PROJECTM_VERSION_1_0_0 1000000 // 1.00 = 1.0 or 1.0.1 (same version number for 1.0 and 1.0.1)
#define PROJECTM_VERSION_1_1_0 1001000 // 1.10 = 1.1 (fixed up in configure)
#define PROJECTM_VERSION_1_2_0 1002000 // 1.2.0 = 1.2
#define PROJECTM_VERSION_2_0_0 2000000 // 2.0.0 = 2.0

// version of projectM to wrap (see PROJECTM_VERSION)
#ifndef PROJECTM_VERSION_INT
#define PROJECTM_VERSION_INT PROJECTM_VERSION_2_0_0
#endif

#if defined(_WIN32) || defined(__CYGWIN__)
#define PROJECTM_CWRAPPER_CALL __cdecl
#else
#define PROJECTM_CWRAPPER_CALL
#endif

#define PROJECTM_CWRAPPER_API DLLEXPORT

extern "C" {

    #if (PROJECTM_VERSION_INT > 1000000)
    struct Settings {
	int meshX;
	int meshY;
	int fps;
	int textureSize;
	int windowWidth;
	int windowHeight;
	const char* presetURL;
	const char* titleFontURL;
	const char* menuFontURL;		
	int smoothPresetDuration;
	int presetDuration;
	float beatSensitivity;
	char aspectCorrection;
	float easterEgg;
	char shuffleEnabled;
    };
    #endif

    typedef void* projectM_ptr;

    PROJECTM_CWRAPPER_API projectM_ptr PROJECTM_CWRAPPER_CALL projectM_create1(char* config_file);
    #if (PROJECTM_VERSION_INT < 1000000 || PROJECTM_VERSION_INT >= 2000000)
    PROJECTM_CWRAPPER_API projectM_ptr PROJECTM_CWRAPPER_CALL projectM_create2(int gx, int gy, int fps, int texsize,
					    int width, int height, char* preset_url,
					    char* title_fonturl, char* title_menuurl);
    #endif

    PROJECTM_CWRAPPER_API void PROJECTM_CWRAPPER_CALL projectM_resetGL(projectM_ptr pm, int width, int height);
    PROJECTM_CWRAPPER_API void PROJECTM_CWRAPPER_CALL projectM_setTitle(projectM_ptr pm, char* title);
    PROJECTM_CWRAPPER_API void PROJECTM_CWRAPPER_CALL projectM_renderFrame(projectM_ptr pm);
    PROJECTM_CWRAPPER_API unsigned PROJECTM_CWRAPPER_CALL projectM_initRenderToTexture(projectM_ptr pm);
    PROJECTM_CWRAPPER_API void PROJECTM_CWRAPPER_CALL projectM_key_handler(projectM_ptr pm, projectMEvent event,
					projectMKeycode keycode, projectMModifier modifier);

    PROJECTM_CWRAPPER_API void PROJECTM_CWRAPPER_CALL projectM_free(projectM_ptr pm);

    PROJECTM_CWRAPPER_API void PROJECTM_CWRAPPER_CALL PCM_addPCMfloat(projectM_ptr pm, float *PCMdata, int samples);
    PROJECTM_CWRAPPER_API void PROJECTM_CWRAPPER_CALL PCM_addPCM16(projectM_ptr pm, short [2][512]);
    PROJECTM_CWRAPPER_API void PROJECTM_CWRAPPER_CALL PCM_addPCM16Data(projectM_ptr pm, const short* pcm_data, short samples);
    PROJECTM_CWRAPPER_API void PROJECTM_CWRAPPER_CALL PCM_addPCM8(projectM_ptr pm, unsigned char [2][1024]);
    PROJECTM_CWRAPPER_API void PROJECTM_CWRAPPER_CALL PCM_addPCM8_512(projectM_ptr pm, const unsigned char [2][512]);

    #if (PROJECTM_VERSION_INT > 1000000)
    PROJECTM_CWRAPPER_API void PROJECTM_CWRAPPER_CALL projectM_settings(projectM_ptr pm, Settings* settings);
    #endif
}

#endif
