#ifndef __PROJECTM_CWRAPPER_H__
#define __PROJECTM_CWRAPPER_H__

#include "projectM.hpp"

extern "C" {
	DLLEXPORT projectM* projectM_create1(char* config_file);
	DLLEXPORT projectM* projectM_create2(int gx, int gy, int fps, int texsize, 
		int width, int height, char* preset_url, char* title_fonturl, char* title_menuurl);

	DLLEXPORT void projectM_resetGL(projectM* pm, int width, int height);
	DLLEXPORT void projectM_setTitle(projectM* pm, char* title);
	DLLEXPORT void projectM_renderFrame(projectM* pm);
	DLLEXPORT unsigned projectM_initRenderToTexture(projectM* pm); 
	DLLEXPORT void projectM_key_handler(projectM* pm, projectMEvent event, 
		projectMKeycode keycode, projectMModifier modifier);
	    
	DLLEXPORT void projectM_free(projectM* pm);

    DLLEXPORT void PCM_addPCMfloat(projectM* pm, float *PCMdata, int samples);
    DLLEXPORT void PCM_addPCM16(projectM* pm, short [2][512]);
    DLLEXPORT void PCM_addPCM16Data(projectM* pm, const short* pcm_data, short samples);
    DLLEXPORT void PCM_addPCM8(projectM* pm, unsigned char [2][1024]);
	DLLEXPORT void PCM_addPCM8_512(projectM* pm, const unsigned char [2][512]);
}

#endif