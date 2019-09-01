#pragma once

/*	quantizer.h

	See quantizer.c for more information
*/

#define PALETTE_MAX 65536

#define schar	signed char
#define sshort	signed short
#define slong	signed long
#define uchar	unsigned char
#define ushort	unsigned short
#define ulong	unsigned long

__declspec(dllexport) int __stdcall dl1quant(uchar *inbuf, int width, int height,
			int quant_to, int lookup_bpc, uchar userpal[3][PALETTE_MAX]);

__declspec(dllexport) int __stdcall dl3quant(uchar *inbuf, int width, int height,
			int quant_to, int lookup_bpc, uchar userpal[3][PALETTE_MAX]);

__declspec(dllexport) int __stdcall dl3floste(uchar *inbuf, uchar *outbuf, int width, int height,
			int quant_to, int dither, int lookup_bpc, uchar userpal[3][PALETTE_MAX]);
