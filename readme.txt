*************************************
* GliGli's TileMotion video encoder *
*************************************

The goal of this project is to find a novel way to encode a video stream so that the decoder is computationally cheap and trivially simple to write.
So far, the encoder is mostly FreePascal code with some x64 SSE3 assembler (additional libraries and tools being mostly C/C++) and has a GUI to showcase it.
The current decoder is written entirely in javascript and uses HTML5 Canvas to decode.

Upon bitstream format maturity I will probably write a DirectShow plugin or something akin.

The project is in Alpha stage and located at: https://github.com/gligli/tiler

By: GliGli
License: GNU GPL v3

Encoder steps description:
==========================

- Load:
  * Using FFMPEG, extract frames PNGs.
  * Find keyframes start points using Pearson correlations from frame to frame.
  * Build a tileset for the whole video and simple 1:1 tilemaps.

- Dither:
  * Apply "K Means" clustering on a psychovisual descriptive model on the tiles of a keyframe to prepare splitting them into tile palettes.
  * Use "Dennis Lee v3" or custom "Value At Risk" quantizer to extract tile palettes for each previously computed tile cluster.
  * Use "Thomas Knoll pattern-based" or "Yiluoma 2" dithering to make truecolor tiles into palettized tiles, selecting the best palette using the PsyV model.

- MakeUnique:
  * Simple prepass for the global tiling to merge strictly similar tiles together.

- GlobalTiling:
  * Use KModes clustering on the tiles indexes to globally reduce tile count (about 1% left after that), using the centroids as new tiles.
  * Remap tiles from the frames tilemaps to the reduced tiles.
  * Output TileSet (GTS) as file for potential reuse on another video (TODO: only write the tiles references in the final bitstream GTM file).

- FrameTiling:
  * Using "K Nearest Neighbors" on the psychovisual descriptive model of tiles in all H/V mirroring positions, find the best tile for a given frame tilemap position.

- Reindex:
  * Prune now unused tiles from the global tileset, packing it.
  * Sort remaining global tiles by use count, so that the first tiles in the tileset are more likely to be used compared to the rest.
  * Remap tiles from the frames tilemaps, accounting for the sorting.

- Smooth:
  * Use the PsyV model to merge tiles from frame to frame at the same screen position, so that the "Save" step can safely skip them inside a keyframe.

- Save:
  * Describe the tiles / tileset format as a binary command stream, using "zone" skip commands to try to pack it.
  * Save key frames as self-contained LZMA2 streams into the final TileMotion (GTM) video stream.
