# GliGli's TileMotion video codec

This project achieved its goal: to find a novel way to **compress a video** so that the **decoder is computationally cheap** and **trivially simple to write**.

So far, the **encoder** is mostly FreePascal code with some x64 SSE3 assembler (additional libraries and tools being mostly C/C++) and has a GUI to showcase it.

The **reference decoder** is written entirely in *javascript* and uses *HTML5 Canvas* to decode, proving that minimal code and computing power is necessary.

Upon bitstream format maturity I will probably write a DirectShow plugin or something akin.

Online demo here: <https://gligli.github.io/tiler/demo/>

The project is in **Beta stage** and located at: <https://github.com/gligli/tiler> (go to the 'releases' section for binary downloads).

_Author: GliGli_
_License: GNU GPL v3_

## Details:

It's a video lossy compressor, compression ratio is close to other well known video codecs like MPEG-4/AVC/... (It needs a little more bitrate, but eg. at 7-8MBits/sec it can do 720p with a good picture).

Quality is also not too far from MPEG4/AVC/... but decoding is absurdly cheaper (depacking LZMA2, which is used for the bitstream, costs about the same as the decoding itself).

The video format is based on keyframes / palettes / frames / tilemaps and tiles.
Keyframes hold tiles and palette colors and are constructed detecting color variations ine the video (usually 20-100 frames long).
Frames works a bit like old video game consoles where a frame is a tilemap with tile indexes and attributes (H/V mirrors, palette index, ...).
Tiles are 8*8 blocks of indexes that map to a palette.

Here's the whole decoder in javascript for reference : https://github.com/gligli/tiler/blob/master/decoders/htmljs/gtm.player.js

As a drawback, the encoder needs a lot of memory and CPU, encoding a short video can take many hours depending on resolution and settings.
