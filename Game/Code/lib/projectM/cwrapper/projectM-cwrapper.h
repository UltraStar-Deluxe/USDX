#ifndef __PROJECTM_CWRAPPER_H__
#define __PROJECTM_CWRAPPER_H__

#include "projectM.hpp"

// PROJECTM_VERSION define is not very helpful, lets create our own
#define PROJECTM_VERSION_1_00_00 1000000 // 1.00.00 = 1.0 or 1.01 (same version number for 1.0 and 1.01)
#define PROJECTM_VERSION_1_10_00 1010000 // 1.10.00 = 1.1 (bigger than 1.2 due to strange versioning)
#define PROJECTM_VERSION_1_02_00 1002000 // 1.02.00 = 1.2

// version of projectM to wrap (see PROJECTM_VERSION)
#ifndef PROJECTM_VERSION_INT
#define PROJECTM_VERSION_INT PROJECTM_VERSION_1_02_00
#endif

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

    DLLEXPORT projectM_ptr projectM_create1(char* config_file);
    #if (PROJECTM_VERSION_INT < 1000000)
    DLLEXPORT projectM_ptr projectM_create2(int gx, int gy, int fps, int texsize, 
					    int width, int height, char* preset_url, 
					    char* title_fonturl, char* title_menuurl);
    #endif

    DLLEXPORT void projectM_resetGL(projectM_ptr pm, int width, int height);
    DLLEXPORT void projectM_setTitle(projectM_ptr pm, char* title);
    DLLEXPORT void projectM_renderFrame(projectM_ptr pm);
    DLLEXPORT unsigned projectM_initRenderToTexture(projectM_ptr pm); 
    DLLEXPORT void projectM_key_handler(projectM_ptr pm, projectMEvent event, 
					projectMKeycode keycode, projectMModifier modifier);
    
    DLLEXPORT void projectM_free(projectM_ptr pm);

    DLLEXPORT void PCM_addPCMfloat(projectM_ptr pm, float *PCMdata, int samples);
    DLLEXPORT void PCM_addPCM16(projectM_ptr pm, short [2][512]);
    DLLEXPORT void PCM_addPCM16Data(projectM_ptr pm, const short* pcm_data, short samples);
    DLLEXPORT void PCM_addPCM8(projectM_ptr pm, unsigned char [2][1024]);
    DLLEXPORT void PCM_addPCM8_512(projectM_ptr pm, const unsigned char [2][512]);

    #if (PROJECTM_VERSION_INT > 1000000)
    DLLEXPORT void projectM_settings(projectM_ptr pm, Settings* settings);
    #endif
}

#endif
