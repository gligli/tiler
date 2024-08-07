/*	quantizer.c

	The code in this file was originally written by Dennis Lee in files dl1quant.c, dl1quant.h, dl3quant.c, dl3quant.h

	As per instruction from Dennis I am including the original copyright notice which entitles me (or anyone else who keeps the notice intact) to copy, modify and redistribute this code.  If you would like to see the original distribution of source files and documentation, email me and I will send you the zip file (the email address Dennis mentions below does not work anymore).

	During January 2005 I made numerous changes to the code in order to get it to interface with mtPaint and to compile on gcc-2.95, but the core palette reduction algorithm is the same as the original.

	Mark Tyler, January 2005

	I improved speed of DL3 quantizer by some 30%.
	Dmitry Groshev, March 2007
*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <stdint.h>
#include <stdbool.h>

#include "quantizer.h"

#define CScale   1     /* For RGB: all components equal */
//#define CScale   2.5   /* For YCrCb: Y component favored */

#define DITHER_MAX 20

/*
 * File: dl1quant.h
 *
 * Header file for dl1quant.c (DL1 Quantization)
 *
 * Copyright (C) 1993-1997 Dennis Lee
 */


typedef struct
{
	ulong r, g, b;
	ulong pixel_count;
	ulong pixels_in_cube;
	uchar children;
	uchar palette_index;
} CUBE1;

typedef struct
{
	uchar  level;
	ushort index;
} FCUBE;

typedef struct
{
	uchar palette_index, red, green, blue;
	ulong distance;
	ulong squares[255+255+1];
} CLOSEST_INFO;

// Taken from dl3 below

typedef struct {
	ulong r, g, b;
	ulong pixel_count;
	float err;
	slong cc;
	uchar rr, gg, bb;
} CUBE3;


typedef struct {
	CUBE3 *rgb_table3;

	uchar palette[3][PALETTE_MAX];
	CUBE1 *rgb_table1[6];
	ushort r_offset[256], g_offset[256], b_offset[256];
	CLOSEST_INFO c_info;
	int tot_colors, pal_index, did_init, lookup_size, lookup_bpc;
	ulong *squares1;
	FCUBE *heap;
	short *dl_image;

	float sqr_tbl[255 + 255 + 1], *squares3;
} DLCONTEXT;

static void	copy_pal(DLCONTEXT *ctx, uchar userpal[3][PALETTE_MAX]);
static void	dlq_init(DLCONTEXT *ctx);
static int	dlq_start(DLCONTEXT *ctx);
static void	dlq_finish(DLCONTEXT *ctx);
static int	build_table1(DLCONTEXT *ctx, uchar *image, ulong pixels);
static void	fixheap(DLCONTEXT *ctx, ulong id);
static void	reduce_table1(DLCONTEXT *ctx, int num_colors);
static void	set_palette1(DLCONTEXT *ctx, int index, int level);

static int	init_table(DLCONTEXT *ctx);

static int	quantize_image3(DLCONTEXT *ctx, uchar *inbuf, uchar *outbuf, int width, int height, int dither);

/*
 * DL1 Quantization
 * ================
 *
 * File: dl1quant.c
 * Author: Dennis Lee   E-mail: denlee@ecf.utoronto.ca
 *
 * Copyright (C) 1993-1997 Dennis Lee
 *
 * C implementation of DL1 Quantization.
 * DL1 Quantization is a 2-pass color quantizer optimized for speed.
 * The method was designed around the steps required by a 2-pass
 * quantizer and constructing a model that would require the least
 * amount of extra work.  The resulting method is extremely fast --
 * about half the speed of a memcpy.  That should make DL1 Quant the
 * fastest 2-pass color quantizer.
 *
 * This quantizer's quality is also among the best, slightly
 * better than Wan et al's marginal variance based quantizer.  For
 * more on DL1 Quant's performance and other related information,
 * see DLQUANT.TXT included in this distribution.
 *
 *
 * NOTES
 * =====
 *
 * The dithering code is based on code from the IJG's jpeg library.
 *
 * This source code may be freely copied, modified, and redistributed,
 * provided this copyright notice is attached.
 * Compiled versions of this code, modified or not, are free for
 * personal use.  Compiled versions used in distributed software
 * is also free, but a notification must be sent to the author.
 * An e-mail to denlee@ecf.utoronto.ca will do.
 *
 */

int dl1quant(uchar *inbuf, int width, int height, int quant_to, int lookup_bpc, uchar userpal[3][PALETTE_MAX])
{
	DLCONTEXT *ctx = calloc(sizeof(DLCONTEXT), 1);
	
	ctx->lookup_size = 1 << (lookup_bpc * 3);
	ctx->lookup_bpc = lookup_bpc;

	if (!ctx->did_init)
	{
		ctx->did_init = 1;
		dlq_init(ctx);
	}
	if (dlq_start(ctx) == 0)
	{
		dlq_finish(ctx);
		return 1;
	}
	if (build_table1(ctx, inbuf, (ulong)width * (ulong)height) == 0)
	{
		dlq_finish(ctx);
		return 1;
	}
	reduce_table1(ctx, quant_to);
	set_palette1(ctx, 0, 0);

	dlq_finish(ctx);
	copy_pal(ctx, userpal);

	free(ctx);

	return 0;		// Success
}

static void copy_pal(DLCONTEXT *ctx, uchar userpal[3][PALETTE_MAX])
{
	int i;

	for (i = 0; i < PALETTE_MAX; i++)
	{
		userpal[0][i] = ctx->palette[0][i];
		userpal[1][i] = ctx->palette[1][i];
		userpal[2][i] = ctx->palette[2][i];
	}
}

static void dlq_init(DLCONTEXT *ctx)
{
	int i;

	for (i = 0; i < 256; i++)
	{
		ctx->r_offset[i] = (i & 128) << 7 | (i & 64) << 5 | (i & 32) << 3 |
			(i & 16)  << 1 | (i & 8)  >> 1;
		ctx->g_offset[i] = (i & 128) << 6 | (i & 64) << 4 | (i & 32) << 2 |
			(i & 16)  << 0 | (i & 8)  >> 2;
		ctx->b_offset[i] = (i & 128) << 5 | (i & 64) << 3 | (i & 32) << 1 |
			(i & 16)  >> 1 | (i & 8)  >> 3;
	}

	for (i = -255; i <= 255; i++) ctx->c_info.squares[i+255] = i*i;
	ctx->squares1 = ctx->c_info.squares + 255;
}

static int dlq_start(DLCONTEXT *ctx)
{
	int i;

	ctx->rgb_table1[0] = (CUBE1 *) calloc(sizeof(CUBE1), 1);
	ctx->rgb_table1[1] = (CUBE1 *) calloc(sizeof(CUBE1), 8);
	ctx->rgb_table1[2] = (CUBE1 *) calloc(sizeof(CUBE1), 64);
	ctx->rgb_table1[3] = (CUBE1 *) calloc(sizeof(CUBE1), 512);
	ctx->rgb_table1[4] = (CUBE1 *) calloc(sizeof(CUBE1), 4096);
	ctx->rgb_table1[5] = (CUBE1 *) calloc(sizeof(CUBE1), 32768);

	for (i = 0; i <= 5; i++) if (ctx->rgb_table1[i] == NULL) return 0;	// Failure

	ctx->pal_index = 0;
	return 1;		// Success
}

static void dlq_finish(DLCONTEXT *ctx)
{
	if (ctx->rgb_table1[0] != NULL) free(ctx->rgb_table1[0]);
	if (ctx->rgb_table1[1] != NULL) free(ctx->rgb_table1[1]);
	if (ctx->rgb_table1[2] != NULL) free(ctx->rgb_table1[2]);
	if (ctx->rgb_table1[3] != NULL) free(ctx->rgb_table1[3]);
	if (ctx->rgb_table1[4] != NULL) free(ctx->rgb_table1[4]);
	if (ctx->rgb_table1[5] != NULL) free(ctx->rgb_table1[5]);

	if (ctx->heap != NULL) free(ctx->heap);
	if (ctx->dl_image != NULL) free(ctx->dl_image);
}

/* returns 1 on success, 0 on failure */
static int build_table1(DLCONTEXT *ctx, uchar *image, ulong pixels)
{
	ulong i, index, cur_count, head, tail;
	slong j;

	ctx->heap = (FCUBE *) malloc(sizeof(FCUBE) * 32769);
	if (ctx->heap == NULL) return 0;

	for (i = 0; i < pixels; i++)
	{
		index = ctx->r_offset[image[0]] + ctx->g_offset[image[1]] + ctx->b_offset[image[2]];

		ctx->rgb_table1[5][index].r += image[0];
		ctx->rgb_table1[5][index].g += image[1];
		ctx->rgb_table1[5][index].b += image[2];

		ctx->rgb_table1[5][index].pixel_count++;
		image += 3;
	}

	ctx->tot_colors = 0;
	for (i = 0; i < ctx->lookup_size; i++)
	{
		cur_count = ctx->rgb_table1[5][i].pixel_count;
		if (cur_count)
		{
			ctx->heap[++ctx->tot_colors].level = 5;
			ctx->heap[ctx->tot_colors].index = i;
			ctx->rgb_table1[5][i].pixels_in_cube = cur_count;

			head = i;
			for (j = 4; j >= 0; j--)
			{
				tail = head & 0x7;
				head >>= 3;
				ctx->rgb_table1[j][head].pixels_in_cube += cur_count;
				ctx->rgb_table1[j][head].children |= 1 << tail;
			}
		}
	}

	for (i = ctx->tot_colors; i > 0; i--) fixheap(ctx, i);

	return 1;
}

static void fixheap(DLCONTEXT *ctx, ulong id)
{
	uchar thres_level = ctx->heap[id].level;
	ulong thres_index = ctx->heap[id].index, index, half_totc = ctx->tot_colors >> 1,
	thres_val = ctx->rgb_table1[thres_level][thres_index].pixels_in_cube;

	while (id <= half_totc)
	{
		index = id << 1;

		if (index < ctx->tot_colors)
		if (ctx->rgb_table1[ctx->heap[index].level][ctx->heap[index].index].pixels_in_cube
			> ctx->rgb_table1[ctx->heap[index+1].level][ctx->heap[index+1].index].pixels_in_cube)
				index++;

		if (thres_val <= ctx->rgb_table1[ctx->heap[index].level][ctx->heap[index].index].pixels_in_cube)
			break;
		else
		{
			ctx->heap[id] = ctx->heap[index];
			id = index;
		}
	}
	ctx->heap[id].level = thres_level;
	ctx->heap[id].index = thres_index;
}

static void reduce_table1(DLCONTEXT *ctx, int num_colors)
{
	while (ctx->tot_colors > num_colors)
	{
		uchar tmp_level = ctx->heap[1].level, t_level = tmp_level - 1;
		ulong tmp_index = ctx->heap[1].index, t_index = tmp_index >> 3;

		if (ctx->rgb_table1[t_level][t_index].pixel_count) ctx->heap[1] = ctx->heap[ctx->tot_colors--];
		else
		{
			ctx->heap[1].level = t_level;
			ctx->heap[1].index = t_index;
		}
		ctx->rgb_table1[t_level][t_index].pixel_count += ctx->rgb_table1[tmp_level][tmp_index].pixel_count;
		ctx->rgb_table1[t_level][t_index].r += ctx->rgb_table1[tmp_level][tmp_index].r;
		ctx->rgb_table1[t_level][t_index].g += ctx->rgb_table1[tmp_level][tmp_index].g;
		ctx->rgb_table1[t_level][t_index].b += ctx->rgb_table1[tmp_level][tmp_index].b;
		ctx->rgb_table1[t_level][t_index].children &= ~(1 << (tmp_index & 0x7));
		fixheap(ctx, 1);
	}
}

static void set_palette1(DLCONTEXT *ctx, int index, int level)
{
	ulong r_sum, g_sum, b_sum, sum;
	int i;

	if (ctx->rgb_table1[level][index].children)
		for (i = 7; i >= 0; i--)
			if (ctx->rgb_table1[level][index].children & (1 << i))
				set_palette1(ctx, (index << 3) + i, level + 1);

	if (ctx->rgb_table1[level][index].pixel_count)
	{
		ctx->rgb_table1[level][index].palette_index = ctx->pal_index;
		r_sum = ctx->rgb_table1[level][index].r;
		g_sum = ctx->rgb_table1[level][index].g;
		b_sum = ctx->rgb_table1[level][index].b;
		sum = ctx->rgb_table1[level][index].pixel_count;
		ctx->palette[0][ctx->pal_index] = (r_sum + (sum >> 1)) / sum;
		ctx->palette[1][ctx->pal_index] = (g_sum + (sum >> 1)) / sum;
		ctx->palette[2][ctx->pal_index] = (b_sum + (sum >> 1)) / sum;
		ctx->pal_index++;
	}
}

/*
 * File: dl3quant.h
 *
 * Header file for dl3quant.c (DL3 Quantization)
 *
 * Copyright (C) 1993-1997 Dennis Lee
 */

static void	build_table3(DLCONTEXT *ctx, uchar *image, int size);
static ulong	calc_err(DLCONTEXT *ctx, int, int);
static int	reduce_table3(DLCONTEXT *ctx, int num_colors);
static void	set_palette3(DLCONTEXT *ctx);
static int	bestcolor3(DLCONTEXT *ctx, int r, int g, int b);


/*
 * DL3 Quantization
 * ================
 *
 * File: dl3quant.c
 * Author: Dennis Lee   E-mail: denlee@ecf.utoronto.ca
 *
 * Copyright (C) 1993-1997 Dennis Lee
 *
 * C implementation of DL3 Quantization.
 * DL3 Quantization is a 2-pass color quantizer that uses an
 * exhaustive search technique to minimize error introduced at
 * each step during palette reduction.
 *
 * I believe DL3 Quant offers the highest quality of all existing
 * color quantizers.  It is truly 'optimal' except for a few provisos.
 * These provisos and other information about DL3 Quant can be found
 * in DLQUANT.TXT, which is included in this distribution.
 *
 *
 * NOTES
 * =====
 *
 * The dithering code is based on code from the IJG's jpeg library.
 *
 * DL3 Quantization can take a long time to reduce a palette.
 * Times can range from seconds to minutes or even hours depending on
 * the image and the computer used.  This eliminates DL3 Quant for
 * typical usage unless the user has a very fast computer and/or has a
 * lot of patience.  However, the reward is a quantized image that is
 * the best currently possible.  The number of colors in the source image,
 * not the image size, determines the time required to quantize it.
 *
 * This source code may be freely copied, modified, and redistributed,
 * provided this copyright notice is attached.
 * Compiled versions of this code, modified or not, are free for
 * personal use.  Compiled versions used in distributed software
 * is also free, but a notification must be sent to the author.
 * An e-mail to denlee@ecf.utoronto.ca will do.
 *
 */

int dl3floste(uchar *inbuf, uchar *outbuf, int width, int height,
			int quant_to, int dither, int lookup_bpc, uchar userpal[3][PALETTE_MAX])
{
	DLCONTEXT *ctx = calloc(sizeof(DLCONTEXT), 1);

	ctx->lookup_size = 1 << (lookup_bpc * 3);
	ctx->lookup_bpc = lookup_bpc;

	// This procedure was written by M.Tyler to quantize with current palette

	int i, j;

	if (init_table(ctx) == 0) return 1;

	ctx->tot_colors = quant_to;
	for (i=0; i<quant_to; i++)
		for (j=0; j<3; j++)
			ctx->palette[j][i] = userpal[j][i];

	if (quantize_image3(ctx, inbuf, outbuf, width, height, dither) == 0)
	{
		free(ctx->rgb_table3);
		return 1;
	}
	free(ctx->rgb_table3);

	free(ctx);

	return 0;
}


int dl3quant(uchar *inbuf, int width, int height, int quant_to, int lookup_bpc, uchar userpal[3][PALETTE_MAX])
{
	DLCONTEXT *ctx = calloc(sizeof(DLCONTEXT), 1);

	ctx->lookup_size = 1 << (lookup_bpc * 3);
	ctx->lookup_bpc = lookup_bpc;

	if (init_table(ctx) == 0) return 1;
	build_table3(ctx, inbuf, width * height);

	if ( reduce_table3(ctx, quant_to) ) return 0;	// Return if stop button pressed
	set_palette3(ctx);

	copy_pal(ctx, userpal);

	free(ctx);

	return 0;		// Success
}

static int init_table(DLCONTEXT *ctx)
{
	int i;

	ctx->rgb_table3 = (CUBE3 *) calloc(sizeof(CUBE3), ctx->lookup_size);

	if (ctx->rgb_table3 == NULL) return 0;

	for (i = (-255); i <= 255; i++) ctx->sqr_tbl[i+255] = i*i;

	ctx->squares3 = ctx->sqr_tbl + 255;

	return 1;
}

static void setrgb(CUBE3 *rec)
{
	int v = rec->pixel_count, v2 = v >> 1;
	rec->rr = (rec->r + v2) / v;
	rec->gg = (rec->g + v2) / v;
	rec->bb = (rec->b + v2) / v;
}

static void build_table3(DLCONTEXT *ctx, uchar *image, int size)
{
	int i, index;

	for (i = 0; i < size; i++)
	{
		int r, g, b, mbpc;

		mbpc = (1 << ctx->lookup_bpc) - 1;

		r = image[0] * mbpc / 255;
		g = image[1] * mbpc / 255;
		b = image[2] * mbpc / 255;

		index = b | (g << ctx->lookup_bpc) | (r << (ctx->lookup_bpc << 1));

		ctx->rgb_table3[index].r += image[0] * CScale;
		ctx->rgb_table3[index].g += image[1];
		ctx->rgb_table3[index].b += image[2];
		ctx->rgb_table3[index].pixel_count++;
		image += 3;
	}

	ctx->tot_colors = 0;
	for (i = 0; i < ctx->lookup_size; i++)
		if (ctx->rgb_table3[i].pixel_count)
		{
			setrgb(ctx->rgb_table3 + i);
			ctx->rgb_table3[ctx->tot_colors++] = ctx->rgb_table3[i];
		}
}

static float calc_err(DLCONTEXT *ctx, int c1, int c2)
{
	float dist1, dist2;
	ulong P1, P2, P3;
	int R1, G1, B1, R2, G2, B2, R3, G3, B3;

	P1 = ctx->rgb_table3[c1].pixel_count;
	P2 = ctx->rgb_table3[c2].pixel_count;
	P3 = P1 + P2;

	R3 = (ctx->rgb_table3[c1].r + ctx->rgb_table3[c2].r + (P3 >> 1)) / P3;
	G3 = (ctx->rgb_table3[c1].g + ctx->rgb_table3[c2].g + (P3 >> 1)) / P3;
	B3 = (ctx->rgb_table3[c1].b + ctx->rgb_table3[c2].b + (P3 >> 1)) / P3;

	R1 = ctx->rgb_table3[c1].rr;
	G1 = ctx->rgb_table3[c1].gg;
	B1 = ctx->rgb_table3[c1].bb;

	R2 = ctx->rgb_table3[c2].rr;
	G2 = ctx->rgb_table3[c2].gg;
	B2 = ctx->rgb_table3[c2].bb;

	dist1 = ctx->squares3[R3 - R1] + ctx->squares3[G3 - G1] + ctx->squares3[B3 - B1];
	dist1 = sqrtf(dist1) * P1;

	dist2 = ctx->squares3[R2 - R3] + ctx->squares3[G2 - G3] + ctx->squares3[B2 - B3];
	dist2 = sqrtf(dist2) * P2;

	return (dist1 + dist2);
}

static void recount_next(DLCONTEXT *ctx, int i)
{
	int j, c2 = 0;
	float err, cur_err;

	err = HUGE_VALF;
	for (j = i + 1; j < ctx->tot_colors; j++)
	{
		cur_err = calc_err(ctx, i, j);
		if (cur_err < err)
		{
			err = cur_err;
			c2 = j;
		}
	}
	ctx->rgb_table3[i].err = err;
	ctx->rgb_table3[i].cc = c2;
}

static void recount_dist(DLCONTEXT *ctx, int c1)
{
	int i;
	float cur_err;

	recount_next(ctx, c1);
	for (i = 0; i < c1; i++)
	{
		if (ctx->rgb_table3[i].cc == c1) recount_next(ctx, i);
		else
		{
			cur_err = calc_err(ctx, i, c1);
			if (cur_err < ctx->rgb_table3[i].err)
			{
				ctx->rgb_table3[i].err = cur_err;
				ctx->rgb_table3[i].cc = c1;
			}
		}
	}
}

static int reduce_table3(DLCONTEXT *ctx, int num_colors)
{
	int i, c1=0, c2=0, grand_total, bailout = false;
	float err;

	progress_init("Quantize Pass 1", 1);
	for (i = 0; i < (ctx->tot_colors - 1); i++)
	{
		if ( i%16 == 0 ) bailout = progress_update( ((float) i) / (ctx->tot_colors-1) );
		if (bailout) goto stop;

		recount_next(ctx, i);
	}
	progress_end();

	ctx->rgb_table3[i].err = HUGE_VALF;
	ctx->rgb_table3[i].cc = ctx->tot_colors;

	grand_total = ctx->tot_colors-num_colors;
	progress_init("Quantize Pass 2", 1);
	while (ctx->tot_colors > num_colors)
	{
		if ( (ctx->tot_colors-num_colors)%16 == 0 )
			bailout = progress_update( ((float) (grand_total- ctx->tot_colors+num_colors)) /
				grand_total );
		if (bailout) goto stop;

		err = HUGE_VALF;
		for (i = 0; i < ctx->tot_colors; i++)
		{
			if (ctx->rgb_table3[i].err < err)
			{
				err = ctx->rgb_table3[i].err;
				c1 = i;
			}
		}
		c2 = ctx->rgb_table3[c1].cc;
		ctx->rgb_table3[c2].r += ctx->rgb_table3[c1].r;
		ctx->rgb_table3[c2].g += ctx->rgb_table3[c1].g;
		ctx->rgb_table3[c2].b += ctx->rgb_table3[c1].b;
		ctx->rgb_table3[c2].pixel_count += ctx->rgb_table3[c1].pixel_count;
		setrgb(ctx->rgb_table3 + c2);
		ctx->tot_colors--;

		ctx->rgb_table3[c1] = ctx->rgb_table3[ctx->tot_colors];
		ctx->rgb_table3[ctx->tot_colors-1].err = HUGE_VALF;
		ctx->rgb_table3[ctx->tot_colors-1].cc = ctx->tot_colors;

		for (i = 0; i < c1; i++)
		{
			if (ctx->rgb_table3[i].cc == ctx->tot_colors) ctx->rgb_table3[i].cc = c1;
		}

		for (i = c1 + 1; i < ctx->tot_colors; i++)
		{
			if (ctx->rgb_table3[i].cc == ctx->tot_colors) recount_next(ctx, i);
		}

		recount_dist(ctx, c1);
		if (c2 != ctx->tot_colors) recount_dist(ctx, c2);
	}
stop:
	progress_end();

	return bailout;
}

static void set_palette3(DLCONTEXT *ctx)
{
	int i;
	ulong sum;

	for (i = 0; i < ctx->tot_colors; i++)
	{
		sum = ctx->rgb_table3[i].pixel_count;
		ctx->palette[0][i] = ctx->rgb_table3[i].rr;
		ctx->palette[1][i] = ctx->rgb_table3[i].gg;
		ctx->palette[2][i] = ctx->rgb_table3[i].bb;
	}
	free(ctx->rgb_table3);
}

static int quantize_image3(DLCONTEXT *ctx, uchar *in, uchar *out, int width, int height, int dither)
{
	int i;
	sshort *lookup, *erowerr, *orowerr, *thisrowerr, *nextrowerr;
	slong j, r_pix, g_pix, b_pix, offset, dir, two_val, odd_scanline = 0, err_len = (width + 2) * 3;
	uchar *range_tbl, *range;
	schar *dith_max_tbl, *dith_max;

	if (!dither)
	{
		for (i = 0; i < (width * height); i++)
		{
			out[i] = bestcolor3(ctx, in[0], in[1], in[2]);
			in += 3;
		}
	}
	else
	{
		range_tbl = malloc(3 * 256);
		range = range_tbl + 256;
		lookup  = malloc(sizeof(short) * ctx->lookup_size);
		erowerr = malloc(sizeof(short) * err_len);
		orowerr = malloc(sizeof(short) * err_len);
		dith_max_tbl = malloc(512);
		dith_max = dith_max_tbl + 256;

		if (range_tbl == NULL || lookup == NULL ||
			erowerr == NULL || orowerr == NULL || dith_max_tbl == NULL)
		{
			if (range_tbl != NULL) free(range_tbl);
			if (lookup != NULL) free(lookup);
			if (erowerr != NULL) free(erowerr);
			if (orowerr != NULL) free(orowerr);
			if (dith_max_tbl != NULL) free(dith_max_tbl);
			return 0;
		}

		for (i = 0; i < err_len; i++) erowerr[i] = 0;

		for (i = 0; i < ctx->lookup_size; i++) lookup[i] = -1;

		for (i = 0; i < 256; i++)
		{
			range_tbl[i] = 0;
			range_tbl[i + 256] = (uchar) i;
			range_tbl[i + 512] = 255;
		}

		for (i = 0; i < 256; i++)
		{
			dith_max_tbl[i] = -DITHER_MAX;
			dith_max_tbl[i + 256] = DITHER_MAX;
		}
		for (i = -DITHER_MAX; i <= DITHER_MAX; i++)
			dith_max_tbl[i + 256] = i;

		for (i = 0 ; i < height; i++)
		{
			if (odd_scanline)
			{
				dir = -1;
				in  += (width - 1) * 3;
				out += (width - 1);
				thisrowerr = orowerr + 3;
				nextrowerr = erowerr + width * 3;
			}
			else
			{
				dir = 1;
				thisrowerr = erowerr + 3;
				nextrowerr = orowerr + width * 3;
			}
			nextrowerr[0] = nextrowerr[1] = nextrowerr[2] = 0;
			for (j = 0; j < width; j++)
			{
				r_pix = range[((thisrowerr[0] + 8) >> 4) + in[0]];
				g_pix = range[((thisrowerr[1] + 8) >> 4) + in[1]];
				b_pix = range[((thisrowerr[2] + 8) >> 4) + in[2]];

				offset = (r_pix&248) << 7 | (g_pix&248) << 2 | b_pix >> 3;
				if (lookup[offset] < 0)
					lookup[offset] = bestcolor3(ctx, r_pix, g_pix, b_pix);

				*out = lookup[offset];
				r_pix = dith_max[r_pix - ctx->palette[0][lookup[offset]]];
				g_pix = dith_max[g_pix - ctx->palette[1][lookup[offset]]];
				b_pix = dith_max[b_pix - ctx->palette[2][lookup[offset]]];

				two_val = r_pix * 2;
				nextrowerr[0-3]  = r_pix;
				r_pix += two_val;
				nextrowerr[0+3] += r_pix;
				r_pix += two_val;
				nextrowerr[0  ] += r_pix;
				r_pix += two_val;
				thisrowerr[0+3] += r_pix;
				two_val = g_pix * 2;
				nextrowerr[1-3]  = g_pix;
				g_pix += two_val;
				nextrowerr[1+3] += g_pix;
				g_pix += two_val;
				nextrowerr[1  ] += g_pix;
				g_pix += two_val;
				thisrowerr[1+3] += g_pix;
				two_val = b_pix * 2;
				nextrowerr[2-3]  = b_pix;
				b_pix += two_val;
				nextrowerr[2+3] += b_pix;
				b_pix += two_val;
				nextrowerr[2  ] += b_pix;
				b_pix += two_val;
				thisrowerr[2+3] += b_pix;

				thisrowerr += 3;
				nextrowerr -= 3;
				in  += dir * 3;
				out += dir;
			}
			if ((i % 2) == 1)
			{
				in  += (width + 1) * 3;
				out += (width + 1);
			}
			odd_scanline = !odd_scanline;
		}

		free(range_tbl);
		free(lookup);
		free(erowerr);
		free(orowerr);
		free(dith_max_tbl);
	}
	return 1;
}

static int bestcolor3(DLCONTEXT *ctx, int r, int g, int b)
{
	ulong i, bestcolor=0, curdist, mindist;
	slong rdist, gdist, bdist;

	mindist = 200000;
	for (i = 0; i < ctx->tot_colors; i++)
	{
		rdist = ctx->palette[0][i] - r;
		gdist = ctx->palette[1][i] - g;
		bdist = ctx->palette[2][i] - b;
		curdist = ctx->squares3[rdist] + ctx->squares3[gdist] + ctx->squares3[bdist];
		if (curdist < mindist)
		{
			mindist = curdist;
			bestcolor = i;
		}
	}
	return bestcolor;
}
