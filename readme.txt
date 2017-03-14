*********************************
* SMS video player / encoder v7 *
*********************************

Project page: https://gitlab.com/gligli/tiler

By: GliGli, with help from Maxim and others from http://www.smspower.org/forums/16306-FullscreenVideoPlaybackOnSegaMasterSystem
License: GNU GPL v3

Encoder usage (tiler.exe)
=========================

Input:
	Path to input video as a series of pictures.
	Pictures dimensions should always be 256x192, at a 12.5fps rate.
	Supported file formats include BMP, PNG, JPG.
	test%.4d.bmp means: expect frames pictures to be named test0000.bmp, test0001.bmp and so on.
	(VirtualDub has an option to output this kind of format)

	Keyframes should have an additional .kf file next to the picture. Eg: test0050.bmp -> test0050.kf
	First picture is a keyframe by default.
	Keyframes hold color palette information so they should be added when there's a major change in picture color, eg on a scene change.
	The more keyframes you add, the slower the encoding will be but it won't make the video much bigger.
	
Frame count:
	Number of pictures to load, ie the number of video frames.

Average tiles per frame: 
	Overall number of tiles per frame, ie a 50 frame video with 100 average tiles per frame will lead to max 5000 unique tiles in the video data.
	Higher means better quality at the expense of video size.
	/!\ Video won't work if there's more than about 65000 unique tiles in the whole video.
	
Max tiles per frame:
	Hard limit on the number of unique tiles per frame, higher means better quality at the expense of video size.
	Should not be more than 207 for v7 video player.

KModes parallel runs:
	Higher will make the encoding process slower but quality will be better.
	Setting this to the number of CPU cores or lower won't make the encoding much slower.

Temporal smoothing strength:
	Encoding noise reducer to produce smaller videos.
	Setting this value too high can significantly reduce video quality.
	Set to 0 to deactivate.

WAV (mono):
	Path to the video soundtrack in mono 16bit WAV format, it must be long enough for the video.
	It is highly recommended to make it as loud as possible, for example by using a limiter.
	To encode with no sound, make this field empty and compile player_nosound.asm (TODO for v7).

Output:
	Path where data.bin, the video data file, will be written.
	
Play:
	Check it to preview the video as a loop.

Show dithered:
	When checked, the output video will be displayed in the SMS palette.

Press "Run all" to run the entire encoding process or right click on it to run a specific sub process.
The process tries to use all the available CPU cores, but can still take hours to finish depending on the number of frames and the settings.

Player compilation (player_new.asm)
===================================

- Compile with a recent WLA-DX. I used this tutorial to setup the environment: http://www.smspower.org/maxim/HowToProgram/Index

Building a video ROM
====================

- Put data.bin from the encoder in ./tiled/
- Run append.bat to append video data to the player ROM.
- Final ROM file is output_final.sms.

ROM usage
=========

- Only plays properly on 50Hz hardware.
- Most emulators should be able to play it.

History
=======
	
v3:
	Initial release.
v4:
	Improved video quality.
	Greatly reduced encoding time by using an external K-Means processor (yakmo: http://www.tkl.iis.u-tokyo.ac.jp/~ynaga/yakmo/ ).
	Fixed some bugs with plain colors noiseless videos. If you still get frames wrongly reduced to 1 tile, set "KMeans restarts" to 1.
	No more need to create a .kf file for the first frame.
	New longer demo video from Sonic CD :)
v5:
	Reduced video noise by basically swapping inter and intra tiling passes.
	Added an option to further reduce video noise by taking into account tiles spatial / temporal coordinates while merging tiles.
	To reduce data size, per frame tile indices and tilemaps are now packed. Fixed 3:4 ratio for tile indices. Temporal compression for tilemaps.
	Changed the compiling process to prevent the ROM header from corrupting tiles.
v6:
	PCM sound, using pcmenc ( http://github.com/maxim-zhao/pcmenc ).
	Temporary files for yakmo are now stored in Windows temp folder.
	Improved tilemap compression (skip enough tiles to make it a net size gain).
v6.2:
	Made player code smaller to allow for longer videos.
	Fixed a player tile corruption bug.
	Encoder reindexing process speedup.
	Proper 1MB Sonic CD video plus a full version.
v7:
	Complete rewrite.
	Now using a global tile repository instead of per keyframe.
	Now using the 2 hardware palettes for max 28 unique colors per keyframe.
	Now using horizontal and vertical mirrors on tiles to improve quality vs size ratio.
	Can now encode much longer videos, although encoding time increase is worse than linear with video length (ie a 1000 frame video takes more than twice the time of a 500 frame video to encode).
	Improved emulator compatibility.