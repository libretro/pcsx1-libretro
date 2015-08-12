/*
 * (C) notaz, 2010-2012
 *
 * This work is licensed under the terms of the GNU GPLv2 or later.
 * See the COPYING file in the top-level directory.
 */

#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <unistd.h>
#include <signal.h>
#include <time.h>
#ifndef _WIN32
#include <dlfcn.h>
#endif

#include "main.h"
#include "plugin.h"
#include "plugin_lib.h"
#include "pcnt.h"
#include "menu.h"
#include "plat.h"
#include "../libpcsxcore/misc.h"
#include "../libpcsxcore/cheat.h"
#include "../libpcsxcore/new_dynarec/new_dynarec.h"
#include "../plugins/cdrcimg/cdrcimg.h"
#include "../plugins/dfsound/spu_config.h"
#include "revision.h"

#ifndef BOOT_MSG
#define BOOT_MSG "Booting up..."
#endif

#ifdef DEBUG
// don't include debug.h - it breaks ARM build (R1 redefined)
void StartDebugger();
void StopDebugger();
#endif

int ready_to_go, g_emu_want_quit, g_emu_resetting;
unsigned long gpuDisp;
char cfgfile_basename[MAXPATHLEN];
int state_slot;
enum sched_action emu_action, emu_action_old;
char hud_msg[64];
int hud_new_msg;

static void make_path(char *buf, size_t size, const char *dir, const char *fname)
{
	if (fname)
		snprintf(buf, size, ".%s%s", dir, fname);
	else
		snprintf(buf, size, ".%s", dir);
}
#define MAKE_PATH(buf, dir, fname) \
	make_path(buf, sizeof(buf), dir, fname)

static int get_gameid_filename(char *buf, int size, const char *fmt, int i) {
	char trimlabel[33];
	int j;

	strncpy(trimlabel, CdromLabel, 32);
	trimlabel[32] = 0;
	for (j = 31; j >= 0; j--)
		if (trimlabel[j] == ' ')
			trimlabel[j] = 0;
		else
			continue;

	snprintf(buf, size, fmt, trimlabel, CdromId, i);

	return 0;
}

void set_cd_image(const char *fname)
{
	const char *ext = NULL;
	
	if (fname != NULL)
		ext = strrchr(fname, '.');

	if (ext && (
	    strcasecmp(ext, ".z") == 0 || strcasecmp(ext, ".bz") == 0 ||
	    strcasecmp(ext, ".znx") == 0 /*|| strcasecmp(ext, ".pbp") == 0*/)) {
		SetIsoFile(NULL);
		cdrcimg_set_fname(fname);
		strcpy(Config.Cdr, "builtin_cdrcimg");
	} else {
		SetIsoFile(fname);
		strcpy(Config.Cdr, "builtin_cdr");
	}
}

static void set_default_paths(void)
{
	strcpy(Config.PluginsDir, "plugins");
	strcpy(Config.Gpu, "builtin_gpu");
	strcpy(Config.Spu, "builtin_spu");
	strcpy(Config.Cdr, "builtin_cdr");
	strcpy(Config.Pad1, "builtin_pad");
	strcpy(Config.Pad2, "builtin_pad");
	strcpy(Config.Net, "Disabled");
}

void emu_set_default_config(void)
{
	// try to set sane config on which most games work
	Config.Xa = Config.Cdda = Config.Sio =
	Config.SpuIrq = Config.RCntFix = Config.VSyncWA = 0;
	Config.PsxAuto = 1;

	pl_rearmed_cbs.gpu_neon.allow_interlace = 2; // auto
	pl_rearmed_cbs.gpu_neon.enhancement_enable =
	pl_rearmed_cbs.gpu_neon.enhancement_no_main = 0;
	pl_rearmed_cbs.gpu_peops.iUseDither = 0;
	pl_rearmed_cbs.gpu_peops.dwActFixes = 1<<7;
	pl_rearmed_cbs.gpu_unai.abe_hack =
	pl_rearmed_cbs.gpu_unai.no_light =
	pl_rearmed_cbs.gpu_unai.no_blend = 0;
	memset(&pl_rearmed_cbs.gpu_peopsgl, 0, sizeof(pl_rearmed_cbs.gpu_peopsgl));
	pl_rearmed_cbs.gpu_peopsgl.iVRamSize = 64;
	pl_rearmed_cbs.gpu_peopsgl.iTexGarbageCollection = 1;

	spu_config.iUseReverb = 1;
	spu_config.iUseInterpolation = 1;
	spu_config.iXAPitch = 0;
	spu_config.iVolume = 768;
	spu_config.iTempo = 0;
	spu_config.iUseThread = 1; // no effect if only 1 core is detected
#if defined(__arm__) && !defined(__ARM_ARCH_7A__) /* XXX GPH hack */
	spu_config.iUseReverb = 0;
	spu_config.iUseInterpolation = 0;
	spu_config.iTempo = 1;
#endif
	new_dynarec_hacks = 0;
	cycle_multiplier = 200;

	in_type1 = PSE_PAD_TYPE_STANDARD;
	in_type2 = PSE_PAD_TYPE_STANDARD;
}

static char basic_lcase(char c)
{
	if ('A' <= c && c <= 'Z')
		return c - 'A' + 'a';
	return c;
}

static int cdidcmp(const char *id1, const char *id2)
{
	while (*id1 != 0 && *id2 != 0) {
		if (*id1 == '_') { id1++; continue; }
		if (*id2 == '_') { id2++; continue; }
		if (basic_lcase(*id1) != basic_lcase(*id2))
			break;
		id1++;
		id2++;
	}

	return *id1 - *id2;
}

static void parse_cwcheat(void)
{
	char line[256], buf[64], name[64], *p;
	int newcheat = 1;
	u32 a, v;
	FILE *f;

	f = fopen("cheatpops.db", "r");
	if (f == NULL)
		return;

	/* find the game */
	while (fgets(line, sizeof(line), f)) {
		if (sscanf(line, "_S %63s", buf) != 1)
			continue;
		if (cdidcmp(buf, CdromId) == 0)
			break;
	}

	if (feof(f))
		goto out;

	SysPrintf("cwcheat section found for %s\n", CdromId);
	while (fgets(line, sizeof(line), f))
	{
		p = line + strlen(line);
		for (p--; p >= line && (*p == '\r' || *p == '\n' || *p == ' '); p--)
			*p = 0;
		if (*p == 0 || *p == '#' || *p == ';')
			continue;

		if (strncmp(line, "_S", 2) == 0)
			break;
		if (strncmp(line, "_G", 2) == 0) {
			SysPrintf("  cwcheat game name: '%s'\n", line + 3);
			continue;
		}
		if (strncmp(line, "_C0", 3) == 0) {
			if (!newcheat && Cheats[NumCheats - 1].n == 0) {
				SysPrintf("cheat '%s' failed to parse\n", name);
				free(Cheats[NumCheats - 1].Descr);
				NumCheats--;
			}
			snprintf(name, sizeof(name), "%s", line + 4);
			newcheat = 1;
			continue;
		}
		if (sscanf(line, "_L %x %x", &a, &v) != 2) {
			SysPrintf("line failed to parse: '%s'\n", line);
			continue;
		}

		if (newcheat) {
			if (NumCheats >= NumCheatsAllocated) {
				NumCheatsAllocated += 16;
				Cheats = realloc(Cheats, sizeof(Cheat) *
						NumCheatsAllocated);
				if (Cheats == NULL)
					break;
			}
			Cheats[NumCheats].Descr = strdup(name);
			Cheats[NumCheats].Enabled = 0;
			Cheats[NumCheats].WasEnabled = 0;
			Cheats[NumCheats].First = NumCodes;
			Cheats[NumCheats].n = 0;
			NumCheats++;
			newcheat = 0;
		}

		if (NumCodes >= NumCodesAllocated) {
			NumCodesAllocated += 16;
			CheatCodes = realloc(CheatCodes, sizeof(CheatCode) *
				NumCodesAllocated);
			if (CheatCodes == NULL)
				break;
		}
		CheatCodes[NumCodes].Addr = a;
		CheatCodes[NumCodes].Val = v;
		NumCodes++;
		Cheats[NumCheats - 1].n++;
	}

out:
	fclose(f);
}

void emu_on_new_cd(int show_hud_msg)
{
	ClearAllCheats();
	parse_cwcheat();

	if (Config.HLE) {
		SysPrintf("note: running with HLE BIOS, expect compatibility problems\n");
		SysPrintf("----------------------------------------------------------\n");
	}

	if (show_hud_msg) {
		snprintf(hud_msg, sizeof(hud_msg), BOOT_MSG);
		hud_new_msg = 3;
	}
}

int emu_core_preinit(void)
{
	// what is the name of the config file?
	// it may be redefined by -cfg on the command line
	strcpy(cfgfile_basename, "pcsx.cfg");

#ifdef IOS
	emuLog = fopen("/User/Documents/pcsxr.log", "w");
	if (emuLog == NULL)
		emuLog = fopen("pcsxr.log", "w");
	if (emuLog == NULL)
#endif
	emuLog = stdout;

	SetIsoFile(NULL);

	memset(&Config, 0, sizeof(Config));

	set_default_paths();
	emu_set_default_config();
	strcpy(Config.Bios, "HLE");

	return 0;
}

int emu_core_init(void)
{
	SysPrintf("Starting PCSX-ReARMed " REV "\n");

	if (EmuInit() == -1) {
		SysPrintf("PSX emulator couldn't be initialized.\n");
		return -1;
	}

	LoadMcds(Config.Mcd1, Config.Mcd2);

#ifdef DEBUG
	if (Config.Debug)
		StartDebugger();
#endif

	return 0;
}

void emu_core_ask_exit(void)
{
	stop = 1;
	g_emu_want_quit = 1;
}

void SysRunGui() {
        printf("SysRunGui\n");
}

static void dummy_lace()
{
}

void SysReset() {
	// rearmed hack: EmuReset() runs some code when real BIOS is used,
	// but we usually do reset from menu while GPU is not open yet,
	// so we need to prevent updateLace() call..
	void *real_lace = GPU_updateLace;
	GPU_updateLace = dummy_lace;
	g_emu_resetting = 1;

	// reset can run code, timing must be set
	pl_timing_prepare(Config.PsxType);

	EmuReset();

	// hmh core forgets this
	CDR_stop();

	GPU_updateLace = real_lace;
	g_emu_resetting = 0;
}

void SysClose() {
	EmuShutdown();
	ReleasePlugins();

#ifdef DEBUG
	StopDebugger();
#endif

	if (emuLog != NULL && emuLog != stdout && emuLog != stderr) {
		fclose(emuLog);
		emuLog = NULL;
	}
}

void SysUpdate() {
}

int get_state_filename(char *buf, int size, int i) {
	return get_gameid_filename(buf, size,
		"." STATES_DIR "%.32s-%.9s.%3.3d", i);
}

#ifndef ANDROID

void SysPrintf(const char *fmt, ...) {
	va_list list;

	va_start(list, fmt);
	vfprintf(emuLog, fmt, list);
	va_end(list);
	fflush(emuLog);
}

#else

#include <android/log.h>

void SysPrintf(const char *fmt, ...) {
	va_list list;

	va_start(list, fmt);
	__android_log_vprint(ANDROID_LOG_INFO, "PCSX", fmt, list);
	va_end(list);
}

#endif

void SysMessage(const char *fmt, ...) {
	va_list list;
	char msg[512];
	int ret;

	va_start(list, fmt);
	ret = vsnprintf(msg, sizeof(msg), fmt, list);
	va_end(list);

	if (ret < sizeof(msg) && msg[ret - 1] == '\n')
		msg[ret - 1] = 0;

	SysPrintf("%s\n", msg);
}

#define PARSEPATH(dst, src) \
	ptr = src + strlen(src); \
	while (*ptr != '\\' && ptr != src) ptr--; \
	if (ptr != src) { \
		strcpy(dst, ptr+1); \
	}

static int _OpenPlugins(void) {
	int ret;

	GPU_clearDynarec(clearDynarec);

	ret = CDR_open();
	if (ret < 0) { SysMessage(_("Error opening CD-ROM plugin!")); return -1; }
	ret = SPU_open();
	if (ret < 0) { SysMessage(_("Error opening SPU plugin!")); return -1; }
	SPU_registerCallback(SPUirq);
	SPU_registerScheduleCb(SPUschedule);
	// pcsx-rearmed: we handle gpu elsewhere
	//ret = GPU_open(&gpuDisp, "PCSX", NULL);
	//if (ret < 0) { SysMessage(_("Error opening GPU plugin!")); return -1; }
	ret = PAD1_open(&gpuDisp);
	if (ret < 0) { SysMessage(_("Error opening Controller 1 plugin!")); return -1; }
	ret = PAD2_open(&gpuDisp);
	if (ret < 0) { SysMessage(_("Error opening Controller 2 plugin!")); return -1; }

	if (Config.UseNet && !NetOpened) {
		netInfo info;
		char path[MAXPATHLEN];
		char dotdir[MAXPATHLEN];

		MAKE_PATH(dotdir, "/.pcsx/plugins/", NULL);

		strcpy(info.EmuName, "PCSX");
		strncpy(info.CdromID, CdromId, 9);
		strncpy(info.CdromLabel, CdromLabel, 9);
		info.psxMem = psxM;
		info.GPU_showScreenPic = GPU_showScreenPic;
		info.GPU_displayText = GPU_displayText;
		info.GPU_showScreenPic = GPU_showScreenPic;
		info.PAD_setSensitive = PAD1_setSensitive;
		sprintf(path, "%s%s", Config.BiosDir, Config.Bios);
		strcpy(info.BIOSpath, path);
		strcpy(info.MCD1path, Config.Mcd1);
		strcpy(info.MCD2path, Config.Mcd2);
		sprintf(path, "%s%s", dotdir, Config.Gpu);
		strcpy(info.GPUpath, path);
		sprintf(path, "%s%s", dotdir, Config.Spu);
		strcpy(info.SPUpath, path);
		sprintf(path, "%s%s", dotdir, Config.Cdr);
		strcpy(info.CDRpath, path);
		NET_setInfo(&info);

		ret = NET_open(&gpuDisp);
		if (ret < 0) {
			if (ret == -2) {
				// -2 is returned when something in the info
				// changed and needs to be synced
				char *ptr;

				PARSEPATH(Config.Bios, info.BIOSpath);
				PARSEPATH(Config.Gpu,  info.GPUpath);
				PARSEPATH(Config.Spu,  info.SPUpath);
				PARSEPATH(Config.Cdr,  info.CDRpath);

				strcpy(Config.Mcd1, info.MCD1path);
				strcpy(Config.Mcd2, info.MCD2path);
				return -2;
			} else {
				Config.UseNet = FALSE;
			}
		} else {
			if (NET_queryPlayer() == 1) {
				if (SendPcsxInfo() == -1) Config.UseNet = FALSE;
			} else {
				if (RecvPcsxInfo() == -1) Config.UseNet = FALSE;
			}
		}
		NetOpened = TRUE;
	} else if (Config.UseNet) {
		NET_resume();
	}

	return 0;
}

int OpenPlugins() {
	int ret;

	while ((ret = _OpenPlugins()) == -2) {
		ReleasePlugins();
		LoadMcds(Config.Mcd1, Config.Mcd2);
		if (LoadPlugins() == -1) return -1;
	}
	return ret;
}

void ClosePlugins() {
	int ret = CDR_close();
	if (ret < 0) { SysMessage(_("Error closing CD-ROM plugin!")); return; }
	ret = SPU_close();
	if (ret < 0) { SysMessage(_("Error closing SPU plugin!")); return; }
	ret = PAD1_close();
	if (ret < 0) { SysMessage(_("Error closing Controller 1 Plugin!")); return; }
	ret = PAD2_close();
	if (ret < 0) { SysMessage(_("Error closing Controller 2 plugin!")); return; }
	// pcsx-rearmed: we handle gpu elsewhere
	//ret = GPU_close();
	//if (ret < 0) { SysMessage(_("Error closing GPU plugin!")); return; }

	if (Config.UseNet) {
		NET_pause();
	}
}

/* we hook statically linked plugins here */
static const char *builtin_plugins[] = {
	"builtin_gpu", "builtin_spu", "builtin_cdr", "builtin_pad",
	"builtin_cdrcimg",
};

static const int builtin_plugin_ids[] = {
	PLUGIN_GPU, PLUGIN_SPU, PLUGIN_CDR, PLUGIN_PAD,
	PLUGIN_CDRCIMG,
};

void *SysLoadLibrary(const char *lib) {
	const char *tmp = strrchr(lib, '/');
	void *ret = NULL;
	int i;

	SysPrintf("plugin: %s\n", lib);

	if (tmp != NULL) {
		tmp++;
		for (i = 0; i < ARRAY_SIZE(builtin_plugins); i++)
			if (strcmp(tmp, builtin_plugins[i]) == 0)
				return (void *)(long)(PLUGIN_DL_BASE + builtin_plugin_ids[i]);
	}

#ifndef _WIN32
	ret = dlopen(lib, RTLD_NOW);
	if (ret == NULL)
		SysMessage("dlopen: %s", dlerror());
#else
	/* no external plugin support, abi is no longer
	 * compatible with psemu/pcsx anyway */
#endif
	return ret;
}

void *SysLoadSym(void *lib, const char *sym) {
	unsigned int plugid = (unsigned int)(long)lib;

	if (PLUGIN_DL_BASE <= plugid && plugid < PLUGIN_DL_BASE + ARRAY_SIZE(builtin_plugins))
		return plugin_link(plugid - PLUGIN_DL_BASE, sym);

#ifndef _WIN32
	return dlsym(lib, sym);
#else
	return NULL;
#endif
}

const char *SysLibError() {
#ifndef _WIN32
	return dlerror();
#else
	return "not supported";
#endif
}

void SysCloseLibrary(void *lib) {
	unsigned int plugid = (unsigned int)(long)lib;

	if (PLUGIN_DL_BASE <= plugid && plugid < PLUGIN_DL_BASE + ARRAY_SIZE(builtin_plugins))
		return;

#ifndef _WIN32
	dlclose(lib);
#endif
}

