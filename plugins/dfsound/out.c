#include <stdio.h>
#include <stdlib.h>
#include "out.h"

#define MAX_OUT_DRIVERS 5

static struct out_driver out_drivers[MAX_OUT_DRIVERS];
struct out_driver *out_current;
static int driver_count;

#define REGISTER_DRIVER(d) { \
	extern void out_register_##d(struct out_driver *drv); \
	out_register_##d(&out_drivers[driver_count++]); \
}

void SetupSound(void)
{
	int i;

	if (driver_count == 0) {
		REGISTER_DRIVER(libretro);
	}

	for (i = 0; i < driver_count; i++)
		if (out_drivers[i].init() == 0)
			break;

	if (i < 0 || i >= driver_count) {
		printf("the impossible happened\n");
		abort();
	}

	out_current = &out_drivers[i];
	printf("selected sound output driver: %s\n", out_current->name);
}

