#include <stdlib.h>
#include <string.h>
#include <joystick.h>
#include <pce.h>
#include <6502.h>

unsigned char cnt = 0;
unsigned char resetcnt = 0;
unsigned char stack[128];

void vsync()
{
	volatile unsigned char cnt2 = cnt;

	while (cnt2 == cnt)
		;
}

unsigned char irqhandler()
{
	++cnt;

	return IRQ_NOT_HANDLED;
}

int main (void)
{
	unsigned char *ptrJoy = (unsigned char*)(0x1000);
	unsigned char *ptrcmd = (unsigned char*)(0x1E00);
	volatile unsigned char *ptrVDC = (volatile unsigned char*)(0x0000);

	joy_install(joy_static_stddrv);

	set_irq(irqhandler, stack, 128);

	//enable vsync interrupts
	ptrVDC[0] = 5;
	ptrVDC[2] = 0x08;
	ptrVDC[3] = 0x00;

	resetcnt = 0;

	CLI();

	while(1)
	{
		unsigned char j1 = joy_read(JOY_1);
		ptrcmd[0] = j1;		
		ptrcmd[1] = joy_read(JOY_2);

		if ((j1 & (0x08 | 0x04)) == (0x08 | 0x04))
		{
			++resetcnt;
			if (resetcnt > 120)
			{
				ptrcmd[2] = 1;
			}
		}
		else
			resetcnt = 0;
		

		vsync();
	}

	
    return EXIT_SUCCESS;
}
