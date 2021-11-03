# SHDS3_NES
WIP NES Core for Super HD System 3

This is a work in progress core for Super HD System 3 based on the open source FPGANES core: https://github.com/strigeus/fpganes

## SD card setup

Copy the SYS folder to the root of the SD card. 
If you already have a SYS folder, you can just copy the NES folder that is inside SYS to your sd card SYS folder.

## Building the core

To build the core, you'll need Xilinx ISE 14.7. 
* Open the pcesd_nes.xise
* When building the core for the first time, you'll need to regenerate the codes, to do so select the *xc6slx25-2csg324* node, and in the bottom box, expand Design Utilities and double click *Regenerate All Cores*
* Select the PCESD_NES core
* In the bottom box, double click Generate Programming File
* Once finished, you'll have a file named *PCESD_NES.bit* in the project directory. Rename it to *fpga.bit* and place in the SYS/NES folder in the memory card.


**Documentation on FPGA signals and how to build your own core will follow shortly** 