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