# GliGli's TileMotion video codec

This project achieved its goal: to find a novel way to **compress an entire video** (as a whole) so that the **decoder is computationally cheap** and **trivially simple to write**.

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
Keyframes hold palette colors and are constructed detecting color variations ine the video (usually 20-100 frames long).
Frames works a bit like old video game consoles where a frame is a tilemap with tile indexes and attributes (H/V mirrors, palette index, ...).
Tiles are 8*8 blocks of indexes that map to a palette.

Here's the whole decoder in javascript for reference : https://github.com/gligli/tiler/blob/master/decoders/htmljs/gtm.player.js

As a drawback, the encoder needs a lot of memory and CPU, encoding a short video can take many hours depending on resolution and settings.


## Encoder steps description:

- Load:
  * Using FFMPEG, extract frames PNGs.
  * Find keyframes start points using frame to frame **Pearson correlations** on RGB pixels, from the fact that keyframes hold palette information.
  * Build a tileset for the whole video and simple 1:1 tilemaps.

- Dither:
  * Apply **K-Means** (https://github.com/gligli/yakmo) clustering on a __RGB/Intensity__ _PsychoVisual_ descriptive model on the tiles of a keyframe to prepare splitting them into tile palettes.
  * Use **Dennis Lee v3** or custom "Value At Risk" quantizer to extract tile palettes for each previously computed tile cluster.
  * Use **Thomas Knoll** pattern-based or **Yliluoma 2** dithering to make truecolor tiles into palettized tiles, selecting the best palette using the __RGB/I__ _PsychoVisual_ model.

- MakeUnique:
  * Simple, cheap to compute prepass for the global tiling to merge strictly similar tiles together.

- GlobalTiling:
  * Use **K-Modes** clustering on the tiles indexes to globally reduce tile count (about 1% left after that), using the centroids as new tiles.
  * Remap tiles from the frames tilemaps to the reduced tiles.
  * Output TileSet (GTS) as file for potential reuse on another video (TODO: only write the tiles references in the final bitstream GTM file).

- FrameTiling:
  * Using **K Nearest Neighbors** (https://github.com/gligli/annlib) with a __Eye strain__ _PsychoVisual_ descriptive model of tiles in all H/V mirroring positions, find the best tile for a given frame tilemap position.

- Reindex:
  * Prune now unused tiles from the global tileset, packing it.
  * Sort remaining global tiles by use count, so that the first tiles in the tileset are more likely (high probability of use).
  * Remap tiles from the frames tilemaps, previous algorithm taken into account.

- Smooth:
  * Use the __Eye strain__ _PsychoVisual_ model to merge tiles from frame to frame at same XY offset, reducing tilemaps complexity and helping the next step safely skip them inside a keyframe.

- Save:
  * Describe the tiles / tileset format as a binary command stream, using "zone" skip commands to pre-pack it.
  * Save key frames as self-contained LZMA2 streams into the final TileMotion (GTM) video stream.
