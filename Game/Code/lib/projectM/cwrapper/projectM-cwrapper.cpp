#include "projectM-cwrapper.h"

#define PM_CLASS(pm) ((projectM*)pm)

#if (PROJECTM_VERSION_INT > 1000000)
#define	PM_PCM(pm) (PM_CLASS(pm)->pcm())
#else
#define	PM_PCM(pm) (PM_CLASS(pm)->pcm)
#endif

projectM_ptr projectM_create1(char* config_file) 
{
    return projectM_ptr(new projectM(config_file));
}

#if (PROJECTM_VERSION_INT < 1000000)
projectM_ptr projectM_create2(int gx, int gy, int fps, int texsize, 
			      int width, int height, char* preset_url, 
			      char* title_fonturl, char* title_menuurl)
{
    return projectM_ptr(new projectM(gx, gy, fps, texsize, width, height, 
    				     preset_url, title_fonturl, title_menuurl));}
#endif

void projectM_resetGL(projectM_ptr pm, int width, int height)
{
    PM_CLASS(pm)->projectM_resetGL(width, height);
}

void projectM_setTitle(projectM_ptr pm, char* title)
{
    PM_CLASS(pm)->projectM_setTitle(title);
}

void projectM_renderFrame(projectM_ptr pm)
{
    PM_CLASS(pm)->renderFrame();
}

unsigned projectM_initRenderToTexture(projectM_ptr pm)
{
    return PM_CLASS(pm)->initRenderToTexture();
}

void projectM_key_handler(projectM_ptr pm, projectMEvent event, 
		projectMKeycode keycode, projectMModifier modifier)
{
    PM_CLASS(pm)->key_handler(event, keycode, modifier);
}
	    
void projectM_free(projectM_ptr pm)
{
    delete PM_CLASS(pm);
}

void PCM_addPCMfloat(projectM_ptr pm, float *PCMdata, int samples)
{
    PM_PCM(pm)->addPCMfloat(PCMdata, samples);
}

void PCM_addPCM16(projectM_ptr pm, short pcm_data[2][512])
{
    PM_PCM(pm)->addPCM16(pcm_data);
}

void PCM_addPCM16Data(projectM_ptr pm, const short* pcm_data, short samples)
{
    PM_PCM(pm)->addPCM16Data(pcm_data, samples);
}

void PCM_addPCM8(projectM_ptr pm, unsigned char pcm_data[2][1024])
{
    PM_PCM(pm)->addPCM8(pcm_data);
}

void PCM_addPCM8_512(projectM_ptr pm, const unsigned char pcm_data[2][512])
{
    PM_PCM(pm)->addPCM8_512(pcm_data);
}

#define COPY_FIELD(c_ptr, s, fld) (c_ptr->fld = s.fld)

#if (PROJECTM_VERSION_INT > 1000000)
void projectM_settings(projectM_ptr pm, Settings* settings)
{
    const projectM::Settings& pmSettings = PM_CLASS(pm)->settings();

    COPY_FIELD(settings, pmSettings, meshX);
    COPY_FIELD(settings, pmSettings, meshY);
    COPY_FIELD(settings, pmSettings, fps);
    COPY_FIELD(settings, pmSettings, textureSize);
    COPY_FIELD(settings, pmSettings, windowWidth);
    COPY_FIELD(settings, pmSettings, windowHeight);
    settings->presetURL    = pmSettings.presetURL.c_str();
    settings->titleFontURL = pmSettings.titleFontURL.c_str();
    settings->menuFontURL  = pmSettings.menuFontURL.c_str();
    COPY_FIELD(settings, pmSettings, smoothPresetDuration);
    COPY_FIELD(settings, pmSettings, presetDuration);
    COPY_FIELD(settings, pmSettings, beatSensitivity);
    COPY_FIELD(settings, pmSettings, aspectCorrection);
    COPY_FIELD(settings, pmSettings, easterEgg);
    COPY_FIELD(settings, pmSettings, shuffleEnabled);
}
#endif
