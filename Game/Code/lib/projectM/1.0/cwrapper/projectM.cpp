#include "projectM.h"

projectM* projectM_create1(char* config_file) 
{
	return new projectM(config_file);
}

projectM* projectM_create2(int gx, int gy, int fps, int texsize, 
		int width, int height, char* preset_url, char* title_fonturl, char* title_menuurl)
{
	return new projectM(gx, gy, fps, texsize, width, height, 
		preset_url, title_fonturl, title_menuurl);
}

void projectM_resetGL(projectM* pm, int width, int height)
{
	pm->projectM_resetGL(width, height);
}

void projectM_setTitle(projectM* pm, char* title)
{
	pm->projectM_setTitle(title);
}

void projectM_renderFrame(projectM* pm)
{
	pm->renderFrame();
}

unsigned projectM_initRenderToTexture(projectM* pm)
{
	return pm->initRenderToTexture();
}

void projectM_key_handler(projectM* pm, projectMEvent event, 
		projectMKeycode keycode, projectMModifier modifier)
{
	pm->key_handler(event, keycode, modifier);
}
	    
void projectM_free(projectM* pm)
{
	delete pm;
}

void PCM_addPCMfloat(projectM* pm, float *PCMdata, int samples)
{
	pm->pcm->addPCMfloat(PCMdata, samples);
}

void PCM_addPCM16(projectM* pm, short pcm_data[2][512])
{
	pm->pcm->addPCM16(pcm_data);
}

void PCM_addPCM16Data(projectM* pm, const short* pcm_data, short samples)
{
	pm->pcm->addPCM16Data(pcm_data, samples);
}

void PCM_addPCM8(projectM* pm, unsigned char pcm_data[2][1024])
{
	pm->pcm->addPCM8(pcm_data);
}

void PCM_addPCM8_512(projectM* pm, const unsigned char pcm_data[2][512])
{
	pm->pcm->addPCM8_512(pcm_data);
}
