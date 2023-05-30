(*
 * Copyright (c) 2006 Michael Niedermayer <michaelni@gmx.at>
 * Copyright (c) 2008 Peter Ross
 *
 * This file is part of FFmpeg.
 *
 * FFmpeg is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * FFmpeg is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with FFmpeg; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 *)

(**
 * @file
 * audio channel layout utility functions
 *)

(*
 * FFVCL - Delphi FFmpeg VCL Components
 * http://www.DelphiFFmpeg.com
 *
 * Original file: libavcore/audioconvert.h
 * Ported by CodeCoolie@CNSW 2010/11/24 -> 2011-01-21
 * Original file: libavutil/audioconvert.h
 * Ported by CodeCoolie@CNSW 2011/07/02 -> 2013-01-19
 * Original file: libavutil/channel_layout.h
 * Ported by CodeCoolie@CNSW 2013/01/14 -> $Date:: 2022-08-28 #$
 *)

(*
FFmpeg Delphi/Pascal Headers and Examples License Agreement

A modified part of FFVCL - Delphi FFmpeg VCL Components.
Copyright (c) 2008-2023 DelphiFFmpeg.com
All rights reserved.
http://www.DelphiFFmpeg.com

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:
1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.

This source code is provided "as is" by DelphiFFmpeg.com without
warranty of any kind, either expressed or implied, including but not
limited to the implied warranties of merchantability and/or fitness
for a particular purpose.

Please also notice the License agreement of FFmpeg libraries.
*)

unit libavutil_channel_layout;

interface

{$I CompilerDefines.inc}

uses
  FFTypes,
  libavutil_bprint;

{$I libversion.inc}

(**
 * @addtogroup lavu_audio
 * @{
 *)

type
  TAVChannel = (
    ///< Invalid channel index
    AV_CHAN_NONE = -1,
    AV_CHAN_FRONT_LEFT,
    AV_CHAN_FRONT_RIGHT,
    AV_CHAN_FRONT_CENTER,
    AV_CHAN_LOW_FREQUENCY,
    AV_CHAN_BACK_LEFT,
    AV_CHAN_BACK_RIGHT,
    AV_CHAN_FRONT_LEFT_OF_CENTER,
    AV_CHAN_FRONT_RIGHT_OF_CENTER,
    AV_CHAN_BACK_CENTER,
    AV_CHAN_SIDE_LEFT,
    AV_CHAN_SIDE_RIGHT,
    AV_CHAN_TOP_CENTER,
    AV_CHAN_TOP_FRONT_LEFT,
    AV_CHAN_TOP_FRONT_CENTER,
    AV_CHAN_TOP_FRONT_RIGHT,
    AV_CHAN_TOP_BACK_LEFT,
    AV_CHAN_TOP_BACK_CENTER,
    AV_CHAN_TOP_BACK_RIGHT,
    (** Stereo downmix. *)
    AV_CHAN_STEREO_LEFT = 29,
    (** See above. *)
    AV_CHAN_STEREO_RIGHT,
    AV_CHAN_WIDE_LEFT,
    AV_CHAN_WIDE_RIGHT,
    AV_CHAN_SURROUND_DIRECT_LEFT,
    AV_CHAN_SURROUND_DIRECT_RIGHT,
    AV_CHAN_LOW_FREQUENCY_2,
    AV_CHAN_TOP_SIDE_LEFT,
    AV_CHAN_TOP_SIDE_RIGHT,
    AV_CHAN_BOTTOM_FRONT_CENTER,
    AV_CHAN_BOTTOM_FRONT_LEFT,
    AV_CHAN_BOTTOM_FRONT_RIGHT,

    (** Channel is empty can be safely skipped. *)
    AV_CHAN_UNUSED = $200,

    (** Channel contains data, but its position is unknown. *)
    AV_CHAN_UNKNOWN = $300,

    (**
     * Range of channels between AV_CHAN_AMBISONIC_BASE and
     * AV_CHAN_AMBISONIC_END represent Ambisonic components using the ACN system.
     *
     * Given a channel id <i> between AV_CHAN_AMBISONIC_BASE and
     * AV_CHAN_AMBISONIC_END (inclusive), the ACN index of the channel <n> is
     * <n> = <i> - AV_CHAN_AMBISONIC_BASE.
     *
     * @note these values are only used for AV_CHANNEL_ORDER_CUSTOM channel
     * orderings, the AV_CHANNEL_ORDER_AMBISONIC ordering orders the channels
     * implicitly by their position in the stream.
     *)
    AV_CHAN_AMBISONIC_BASE = $400,
    // leave space for 1024 ids, which correspond to maximum order-32 harmonics,
    // which should be enough for the foreseeable use cases
    AV_CHAN_AMBISONIC_END  = $7ff
  );

  TAVChannelOrder = (
    (**
     * Only the channel count is specified, without any further information
     * about the channel order.
     *)
    AV_CHANNEL_ORDER_UNSPEC,
    (**
     * The native channel order, i.e. the channels are in the same order in
     * which they are defined in the AVChannel enum. This supports up to 63
     * different channels.
     *)
    AV_CHANNEL_ORDER_NATIVE,
    (**
     * The channel order does not correspond to any other predefined order and
     * is stored as an explicit map. For example, this could be used to support
     * layouts with 64 or more channels, or with empty/skipped (AV_CHAN_SILENCE)
     * channels at arbitrary positions.
     *)
    AV_CHANNEL_ORDER_CUSTOM,
    (**
     * The audio is represented as the decomposition of the sound field into
     * spherical harmonics. Each channel corresponds to a single expansion
     * component. Channels are ordered according to ACN (Ambisonic Channel
     * Number).
     *
     * The channel with the index n in the stream contains the spherical
     * harmonic of degree l and order m given by
     * @code{.unparsed}
     *   l   = floor(sqrt(n)),
     *   m   = n - l * (l + 1).
     * @endcode
     *
     * Conversely given a spherical harmonic of degree l and order m, the
     * corresponding channel index n is given by
     * @code{.unparsed}
     *   n = l * (l + 1) + m.
     * @endcode
     *
     * Normalization is assumed to be SN3D (Schmidt Semi-Normalization)
     * as defined in AmbiX format $ 2.1.
     *)
    AV_CHANNEL_ORDER_AMBISONIC
  );


const
(**
 * @defgroup channel_masks Audio channel masks
 *
 * A channel layout is a 64-bits integer with a bit set for every channel.
 * The number of bits set must be equal to the number of channels.
 * The value 0 means that the channel layout is not known.
 * @note this data structure is not powerful enough to handle channels
 * combinations that have the same channel multiple times, such as
 * dual-mono.
 *
 * @{
 *)
  AV_CH_FRONT_LEFT            = (Int64(1) shl Ord(AV_CHAN_FRONT_LEFT)           );
  AV_CH_FRONT_RIGHT           = (Int64(1) shl Ord(AV_CHAN_FRONT_RIGHT)          );
  AV_CH_FRONT_CENTER          = (Int64(1) shl Ord(AV_CHAN_FRONT_CENTER)         );
  AV_CH_LOW_FREQUENCY         = (Int64(1) shl Ord(AV_CHAN_LOW_FREQUENCY)        );
  AV_CH_BACK_LEFT             = (Int64(1) shl Ord(AV_CHAN_BACK_LEFT)            );
  AV_CH_BACK_RIGHT            = (Int64(1) shl Ord(AV_CHAN_BACK_RIGHT)           );
  AV_CH_FRONT_LEFT_OF_CENTER  = (Int64(1) shl Ord(AV_CHAN_FRONT_LEFT_OF_CENTER) );
  AV_CH_FRONT_RIGHT_OF_CENTER = (Int64(1) shl Ord(AV_CHAN_FRONT_RIGHT_OF_CENTER));
  AV_CH_BACK_CENTER           = (Int64(1) shl Ord(AV_CHAN_BACK_CENTER)          );
  AV_CH_SIDE_LEFT             = (Int64(1) shl Ord(AV_CHAN_SIDE_LEFT)            );
  AV_CH_SIDE_RIGHT            = (Int64(1) shl Ord(AV_CHAN_SIDE_RIGHT)           );
  AV_CH_TOP_CENTER            = (Int64(1) shl Ord(AV_CHAN_TOP_CENTER)           );
  AV_CH_TOP_FRONT_LEFT        = (Int64(1) shl Ord(AV_CHAN_TOP_FRONT_LEFT)       );
  AV_CH_TOP_FRONT_CENTER      = (Int64(1) shl Ord(AV_CHAN_TOP_FRONT_CENTER)     );
  AV_CH_TOP_FRONT_RIGHT       = (Int64(1) shl Ord(AV_CHAN_TOP_FRONT_RIGHT)      );
  AV_CH_TOP_BACK_LEFT         = (Int64(1) shl Ord(AV_CHAN_TOP_BACK_LEFT)        );
  AV_CH_TOP_BACK_CENTER       = (Int64(1) shl Ord(AV_CHAN_TOP_BACK_CENTER)      );
  AV_CH_TOP_BACK_RIGHT        = (Int64(1) shl Ord(AV_CHAN_TOP_BACK_RIGHT)       );
  AV_CH_STEREO_LEFT           = (Int64(1) shl Ord(AV_CHAN_STEREO_LEFT)          );
  AV_CH_STEREO_RIGHT          = (Int64(1) shl Ord(AV_CHAN_STEREO_RIGHT)         );
  AV_CH_WIDE_LEFT             = (Int64(1) shl Ord(AV_CHAN_WIDE_LEFT)            );
  AV_CH_WIDE_RIGHT            = (Int64(1) shl Ord(AV_CHAN_WIDE_RIGHT)           );
  AV_CH_SURROUND_DIRECT_LEFT  = (Int64(1) shl Ord(AV_CHAN_SURROUND_DIRECT_LEFT) );
  AV_CH_SURROUND_DIRECT_RIGHT = (Int64(1) shl Ord(AV_CHAN_SURROUND_DIRECT_RIGHT));
  AV_CH_LOW_FREQUENCY_2       = (Int64(1) shl Ord(AV_CHAN_LOW_FREQUENCY_2)      );
  AV_CH_TOP_SIDE_LEFT         = (Int64(1) shl Ord(AV_CHAN_TOP_SIDE_LEFT)        );
  AV_CH_TOP_SIDE_RIGHT        = (Int64(1) shl Ord(AV_CHAN_TOP_SIDE_RIGHT)       );
  AV_CH_BOTTOM_FRONT_CENTER   = (Int64(1) shl Ord(AV_CHAN_BOTTOM_FRONT_CENTER)  );
  AV_CH_BOTTOM_FRONT_LEFT     = (Int64(1) shl Ord(AV_CHAN_BOTTOM_FRONT_LEFT)    );
  AV_CH_BOTTOM_FRONT_RIGHT    = (Int64(1) shl Ord(AV_CHAN_BOTTOM_FRONT_RIGHT)   );

{$IFDEF FF_API_OLD_CHANNEL_LAYOUT}
(** Channel mask value used for AVCodecContext.request_channel_layout
    to indicate that the user requests the channel order of the decoder output
    to be the native codec channel order.
    @deprecated channel order is now indicated in a special field in
                AVChannelLayout
  *)
  AV_CH_LAYOUT_NATIVE          = Int64($8000000000000000);
{$ENDIF}

(**
 * @}
 * @defgroup channel_mask_c Audio channel layouts
 * @{
 * *)
  AV_CH_LAYOUT_MONO            = (AV_CH_FRONT_CENTER);
  AV_CH_LAYOUT_STEREO          = (AV_CH_FRONT_LEFT or AV_CH_FRONT_RIGHT);
  AV_CH_LAYOUT_2POINT1         = (AV_CH_LAYOUT_STEREO or AV_CH_LOW_FREQUENCY);
  AV_CH_LAYOUT_2_1             = (AV_CH_LAYOUT_STEREO or AV_CH_BACK_CENTER);
  AV_CH_LAYOUT_SURROUND        = (AV_CH_LAYOUT_STEREO or AV_CH_FRONT_CENTER);
  AV_CH_LAYOUT_3POINT1         = (AV_CH_LAYOUT_SURROUND or AV_CH_LOW_FREQUENCY);
  AV_CH_LAYOUT_4POINT0         = (AV_CH_LAYOUT_SURROUND or AV_CH_BACK_CENTER);
  AV_CH_LAYOUT_4POINT1         = (AV_CH_LAYOUT_4POINT0 or AV_CH_LOW_FREQUENCY);
  AV_CH_LAYOUT_2_2             = (AV_CH_LAYOUT_STEREO or AV_CH_SIDE_LEFT or AV_CH_SIDE_RIGHT);
  AV_CH_LAYOUT_QUAD            = (AV_CH_LAYOUT_STEREO or AV_CH_BACK_LEFT or AV_CH_BACK_RIGHT);
  AV_CH_LAYOUT_5POINT0         = (AV_CH_LAYOUT_SURROUND or AV_CH_SIDE_LEFT or AV_CH_SIDE_RIGHT);
  AV_CH_LAYOUT_5POINT1         = (AV_CH_LAYOUT_5POINT0 or AV_CH_LOW_FREQUENCY);
  AV_CH_LAYOUT_5POINT0_BACK    = (AV_CH_LAYOUT_SURROUND or AV_CH_BACK_LEFT or AV_CH_BACK_RIGHT);
  AV_CH_LAYOUT_5POINT1_BACK    = (AV_CH_LAYOUT_5POINT0_BACK or AV_CH_LOW_FREQUENCY);
  AV_CH_LAYOUT_6POINT0         = (AV_CH_LAYOUT_5POINT0 or AV_CH_BACK_CENTER);
  AV_CH_LAYOUT_6POINT0_FRONT   = (AV_CH_LAYOUT_2_2 or AV_CH_FRONT_LEFT_OF_CENTER or AV_CH_FRONT_RIGHT_OF_CENTER);
  AV_CH_LAYOUT_HEXAGONAL       = (AV_CH_LAYOUT_5POINT0_BACK or AV_CH_BACK_CENTER);
  AV_CH_LAYOUT_6POINT1         = (AV_CH_LAYOUT_5POINT1 or AV_CH_BACK_CENTER);
  AV_CH_LAYOUT_6POINT1_BACK    = (AV_CH_LAYOUT_5POINT1_BACK or AV_CH_BACK_CENTER);
  AV_CH_LAYOUT_6POINT1_FRONT   = (AV_CH_LAYOUT_6POINT0_FRONT or AV_CH_LOW_FREQUENCY);
  AV_CH_LAYOUT_7POINT0         = (AV_CH_LAYOUT_5POINT0 or AV_CH_BACK_LEFT or AV_CH_BACK_RIGHT);
  AV_CH_LAYOUT_7POINT0_FRONT   = (AV_CH_LAYOUT_5POINT0 or AV_CH_FRONT_LEFT_OF_CENTER or AV_CH_FRONT_RIGHT_OF_CENTER);
  AV_CH_LAYOUT_7POINT1         = (AV_CH_LAYOUT_5POINT1 or AV_CH_BACK_LEFT or AV_CH_BACK_RIGHT);
  AV_CH_LAYOUT_7POINT1_WIDE    = (AV_CH_LAYOUT_5POINT1 or AV_CH_FRONT_LEFT_OF_CENTER or AV_CH_FRONT_RIGHT_OF_CENTER);
  AV_CH_LAYOUT_7POINT1_WIDE_BACK=(AV_CH_LAYOUT_5POINT1_BACK or AV_CH_FRONT_LEFT_OF_CENTER or AV_CH_FRONT_RIGHT_OF_CENTER);
  AV_CH_LAYOUT_OCTAGONAL       = (AV_CH_LAYOUT_5POINT0 or AV_CH_BACK_LEFT or AV_CH_BACK_CENTER or AV_CH_BACK_RIGHT);
  AV_CH_LAYOUT_HEXADECAGONAL   = (AV_CH_LAYOUT_OCTAGONAL or AV_CH_WIDE_LEFT or AV_CH_WIDE_RIGHT or AV_CH_TOP_BACK_LEFT or
    AV_CH_TOP_BACK_RIGHT or AV_CH_TOP_BACK_CENTER or AV_CH_TOP_FRONT_CENTER or AV_CH_TOP_FRONT_LEFT or AV_CH_TOP_FRONT_RIGHT);
  AV_CH_LAYOUT_STEREO_DOWNMIX  = (AV_CH_STEREO_LEFT or AV_CH_STEREO_RIGHT);
  AV_CH_LAYOUT_22POINT2        = (AV_CH_LAYOUT_5POINT1_BACK or AV_CH_FRONT_LEFT_OF_CENTER or AV_CH_FRONT_RIGHT_OF_CENTER or
    AV_CH_BACK_CENTER or AV_CH_LOW_FREQUENCY_2 or AV_CH_SIDE_LEFT or AV_CH_SIDE_RIGHT or AV_CH_TOP_FRONT_LEFT or
    AV_CH_TOP_FRONT_RIGHT or AV_CH_TOP_FRONT_CENTER or AV_CH_TOP_CENTER or AV_CH_TOP_BACK_LEFT or AV_CH_TOP_BACK_RIGHT or
    AV_CH_TOP_SIDE_LEFT or AV_CH_TOP_SIDE_RIGHT or AV_CH_TOP_BACK_CENTER or AV_CH_BOTTOM_FRONT_CENTER or
    AV_CH_BOTTOM_FRONT_LEFT or AV_CH_BOTTOM_FRONT_RIGHT);

type
  TAVMatrixEncoding = (
    AV_MATRIX_ENCODING_NONE,
    AV_MATRIX_ENCODING_DOLBY,
    AV_MATRIX_ENCODING_DPLII,
    AV_MATRIX_ENCODING_DPLIIX,
    AV_MATRIX_ENCODING_DPLIIZ,
    AV_MATRIX_ENCODING_DOLBYEX,
    AV_MATRIX_ENCODING_DOLBYHEADPHONE,
    AV_MATRIX_ENCODING_NB
  );

(**
 * @}
 *)

(**
 * An AVChannelCustom defines a single channel within a custom order layout
 *
 * Unlike most structures in FFmpeg, sizeof(AVChannelCustom) is a part of the
 * public ABI.
 *
 * No new fields may be added to it without a major version bump.
 *)
  PAVChannelCustom = ^TAVChannelCustom;
  TAVChannelCustom = record
    id: TAVChannel;
    name: array[0..15] of AnsiChar;
    opaque: Pointer;
  end;

(**
 * An AVChannelLayout holds information about the channel layout of audio data.
 *
 * A channel layout here is defined as a set of channels ordered in a specific
 * way (unless the channel order is AV_CHANNEL_ORDER_UNSPEC, in which case an
 * AVChannelLayout carries only the channel count).
 *
 * Unlike most structures in Libav, sizeof(AVChannelLayout) is a part of the
 * public ABI and may be used by the caller. E.g. it may be allocated on stack
 * or embedded in caller-defined structs.
 *
 * AVChannelLayout can be initialized as follows:
 * - default initialization with {0}, followed by setting all used fields
 *   correctly;
 * - by assigning one of the predefined AV_CHANNEL_LAYOUT_* initializers;
 * - with a constructor function, such as av_channel_layout_default(),
 *   av_channel_layout_from_mask() or av_channel_layout_from_string().
 *
 * The channel layout must be unitialized with av_channel_layout_uninit()
 *
 * Copying an AVChannelLayout via assigning is forbidden,
 * av_channel_layout_copy() must be used instead (and its return value should
 * be checked)
 *
 * No new fields may be added to it without a major version bump, except for
 * new elements of the union fitting in sizeof(uint64_t).
 *)
  PAVChannelLayout = ^TAVChannelLayout;
  TAVChannelLayout = record
    (**
     * Channel order used in this layout.
     * This is a mandatory field.
     *)
    order: TAVChannelOrder;

    (**
     * Number of channels in this layout. Mandatory field.
     *)
    nb_channels: Integer;

    (**
     * Details about which channels are present in this layout.
     * For AV_CHANNEL_ORDER_UNSPEC, this field is undefined and must not be
     * used.
     *)
    u: record
      case Integer of
        (**
         * This member must be used for AV_CHANNEL_ORDER_NATIVE, and may be used
         * for AV_CHANNEL_ORDER_AMBISONIC to signal non-diegetic channels.
         * It is a bitmask, where the position of each set bit means that the
         * AVChannel with the corresponding value is present.
         *
         * I.e. when (mask & (1 << AV_CHAN_FOO)) is non-zero, then AV_CHAN_FOO
         * is present in the layout. Otherwise it is not present.
         *
         * @note when a channel layout using a bitmask is constructed or
         * modified manually (i.e.  not using any of the av_channel_layout_*
         * functions), the code doing it must ensure that the number of set bits
         * is equal to nb_channels.
         *)
        0: (mask: Int64);
        (**
         * This member must be used when the channel order is
         * AV_CHANNEL_ORDER_CUSTOM. It is a nb_channels-sized array, with each
         * element signalling the presence of the AVChannel with the
         * corresponding value in map[i].id.
         *
         * I.e. when map[i].id is equal to AV_CHAN_FOO, then AV_CH_FOO is the
         * i-th channel in the audio data.
         *
         * When map[i].id is in the range between AV_CHAN_AMBISONIC_BASE and
         * AV_CHAN_AMBISONIC_END (inclusive), the channel contains an ambisonic
         * component with ACN index (as defined above)
         * n = map[i].id - AV_CHAN_AMBISONIC_BASE.
         *
         * map[i].name may be filled with a 0-terminated string, in which case
         * it will be used for the purpose of identifying the channel with the
         * convenience functions below. Otherise it must be zeroed.
         *)
        1: (map: PAVChannelCustom);
      end;

    (**
     * For some private data of the user.
     *)
    opaque: Pointer;
  end;

(*
#define AV_CHANNEL_LAYOUT_MASK(nb, m) \
    { .order = AV_CHANNEL_ORDER_NATIVE, .nb_channels = (nb), .u = { .mask = (m) }}

#define AV_CHANNEL_LAYOUT_MONO              AV_CHANNEL_LAYOUT_MASK(1,  AV_CH_LAYOUT_MONO)
#define AV_CHANNEL_LAYOUT_STEREO            AV_CHANNEL_LAYOUT_MASK(2,  AV_CH_LAYOUT_STEREO)
#define AV_CHANNEL_LAYOUT_2POINT1           AV_CHANNEL_LAYOUT_MASK(3,  AV_CH_LAYOUT_2POINT1)
#define AV_CHANNEL_LAYOUT_2_1               AV_CHANNEL_LAYOUT_MASK(3,  AV_CH_LAYOUT_2_1)
#define AV_CHANNEL_LAYOUT_SURROUND          AV_CHANNEL_LAYOUT_MASK(3,  AV_CH_LAYOUT_SURROUND)
#define AV_CHANNEL_LAYOUT_3POINT1           AV_CHANNEL_LAYOUT_MASK(4,  AV_CH_LAYOUT_3POINT1)
#define AV_CHANNEL_LAYOUT_4POINT0           AV_CHANNEL_LAYOUT_MASK(4,  AV_CH_LAYOUT_4POINT0)
#define AV_CHANNEL_LAYOUT_4POINT1           AV_CHANNEL_LAYOUT_MASK(5,  AV_CH_LAYOUT_4POINT1)
#define AV_CHANNEL_LAYOUT_2_2               AV_CHANNEL_LAYOUT_MASK(4,  AV_CH_LAYOUT_2_2)
#define AV_CHANNEL_LAYOUT_QUAD              AV_CHANNEL_LAYOUT_MASK(4,  AV_CH_LAYOUT_QUAD)
#define AV_CHANNEL_LAYOUT_5POINT0           AV_CHANNEL_LAYOUT_MASK(5,  AV_CH_LAYOUT_5POINT0)
#define AV_CHANNEL_LAYOUT_5POINT1           AV_CHANNEL_LAYOUT_MASK(6,  AV_CH_LAYOUT_5POINT1)
#define AV_CHANNEL_LAYOUT_5POINT0_BACK      AV_CHANNEL_LAYOUT_MASK(5,  AV_CH_LAYOUT_5POINT0_BACK)
#define AV_CHANNEL_LAYOUT_5POINT1_BACK      AV_CHANNEL_LAYOUT_MASK(6,  AV_CH_LAYOUT_5POINT1_BACK)
#define AV_CHANNEL_LAYOUT_6POINT0           AV_CHANNEL_LAYOUT_MASK(6,  AV_CH_LAYOUT_6POINT0)
#define AV_CHANNEL_LAYOUT_6POINT0_FRONT     AV_CHANNEL_LAYOUT_MASK(6,  AV_CH_LAYOUT_6POINT0_FRONT)
#define AV_CHANNEL_LAYOUT_HEXAGONAL         AV_CHANNEL_LAYOUT_MASK(6,  AV_CH_LAYOUT_HEXAGONAL)
#define AV_CHANNEL_LAYOUT_6POINT1           AV_CHANNEL_LAYOUT_MASK(7,  AV_CH_LAYOUT_6POINT1)
#define AV_CHANNEL_LAYOUT_6POINT1_BACK      AV_CHANNEL_LAYOUT_MASK(7,  AV_CH_LAYOUT_6POINT1_BACK)
#define AV_CHANNEL_LAYOUT_6POINT1_FRONT     AV_CHANNEL_LAYOUT_MASK(7,  AV_CH_LAYOUT_6POINT1_FRONT)
#define AV_CHANNEL_LAYOUT_7POINT0           AV_CHANNEL_LAYOUT_MASK(7,  AV_CH_LAYOUT_7POINT0)
#define AV_CHANNEL_LAYOUT_7POINT0_FRONT     AV_CHANNEL_LAYOUT_MASK(7,  AV_CH_LAYOUT_7POINT0_FRONT)
#define AV_CHANNEL_LAYOUT_7POINT1           AV_CHANNEL_LAYOUT_MASK(8,  AV_CH_LAYOUT_7POINT1)
#define AV_CHANNEL_LAYOUT_7POINT1_WIDE      AV_CHANNEL_LAYOUT_MASK(8,  AV_CH_LAYOUT_7POINT1_WIDE)
#define AV_CHANNEL_LAYOUT_7POINT1_WIDE_BACK AV_CHANNEL_LAYOUT_MASK(8,  AV_CH_LAYOUT_7POINT1_WIDE_BACK)
#define AV_CHANNEL_LAYOUT_OCTAGONAL         AV_CHANNEL_LAYOUT_MASK(8,  AV_CH_LAYOUT_OCTAGONAL)
#define AV_CHANNEL_LAYOUT_HEXADECAGONAL     AV_CHANNEL_LAYOUT_MASK(16, AV_CH_LAYOUT_HEXADECAGONAL)
#define AV_CHANNEL_LAYOUT_STEREO_DOWNMIX    AV_CHANNEL_LAYOUT_MASK(2,  AV_CH_LAYOUT_STEREO_DOWNMIX)
#define AV_CHANNEL_LAYOUT_22POINT2          AV_CHANNEL_LAYOUT_MASK(24, AV_CH_LAYOUT_22POINT2)
#define AV_CHANNEL_LAYOUT_AMBISONIC_FIRST_ORDER \
    { .order = AV_CHANNEL_ORDER_AMBISONIC, .nb_channels = 4, .u = { .mask = 0 }}
*)

{$IFDEF FF_API_OLD_CHANNEL_LAYOUT}
(**
 * Return a channel layout id that matches name, or 0 if no match is found.
 *
 * name can be one or several of the following notations,
 * separated by '+' or '|':
 * - the name of an usual channel layout (mono, stereo, 4.0, quad, 5.0,
 *   5.0(side), 5.1, 5.1(side), 7.1, 7.1(wide), downmix);
 * - the name of a single channel (FL, FR, FC, LFE, BL, BR, FLC, FRC, BC,
 *   SL, SR, TC, TFL, TFC, TFR, TBL, TBC, TBR, DL, DR);
 * - a number of channels, in decimal, followed by 'c', yielding
 *   the default channel layout for that number of channels (@see
 *   av_get_default_channel_layout);
 * - a channel layout mask, in hexadecimal starting with "0x" (see the
 *   AV_CH_* macros).
 *
 * Example: "stereo+FC" = "2c+FC" = "2c+1c" = "0x7"
 *
 * @deprecated use av_channel_layout_from_string()
 *)
function av_get_channel_layout(const name: PAnsiChar): Int64; cdecl; external AVUTIL_LIBNAME name _PU + 'av_get_channel_layout';

(**
 * Return a channel layout and the number of channels based on the specified name.
 *
 * This function is similar to (@see av_get_channel_layout), but can also parse
 * unknown channel layout specifications.
 *
 * @param[in]  name             channel layout specification string
 * @param[out] channel_layout   parsed channel layout (0 if unknown)
 * @param[out] nb_channels      number of channels
 *
 * @return 0 on success, AVERROR(EINVAL) if the parsing fails.
 * @deprecated use av_channel_layout_from_string()
 *)
function av_get_extended_channel_layout(const name: PAnsiChar; channel_layout: PInt64; nb_channels: PInteger): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_get_extended_channel_layout';

(**
 * Return a description of a channel layout.
 * If nb_channels is <= 0, it is guessed from the channel_layout.
 *
 * @param buf put here the string containing the channel layout
 * @param buf_size size in bytes of the buffer
 * @deprecated use av_channel_layout_describe()
 *)
procedure av_get_channel_layout_string(buf: PAnsiChar; buf_size, nb_channels: Integer; channel_layout: Int64); cdecl; external AVUTIL_LIBNAME name _PU + 'av_get_channel_layout_string';

(**
 * Append a description of a channel layout to a bprint buffer.
 * @deprecated use av_channel_layout_describe()
 *)
procedure av_bprint_channel_layout(bp: PAVBPrint; nb_channels: Integer; channel_layout: Int64); cdecl; external AVUTIL_LIBNAME name _PU + 'av_bprint_channel_layout';

(**
 * Return the number of channels in the channel layout.
 * @deprecated use AVChannelLayout.nb_channels
 *)
function av_get_channel_layout_nb_channels(channel_layout: Int64): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_get_channel_layout_nb_channels';

(**
 * Return default channel layout for a given number of channels.
 *
 * @deprecated use av_channel_layout_default()
 *)
function av_get_default_channel_layout(nb_channels: Integer): Int64; cdecl; external AVUTIL_LIBNAME name _PU + 'av_get_default_channel_layout';

(**
 * Get the index of a channel in channel_layout.
 *
 * @param channel a channel layout describing exactly one channel which must be
 *                present in channel_layout.
 *
 * @return index of channel in channel_layout on success, a negative AVERROR
 *         on error.
 *
 * @deprecated use av_channel_layout_index_from_channel()
 *)
function av_get_channel_layout_channel_index(channel_layout, channel: Int64): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_get_channel_layout_channel_index';

(**
 * Get the channel with the given index in channel_layout.
 * @deprecated use av_channel_layout_channel_from_index()
 *)
function av_channel_layout_extract_channel(channel_layout: Int64; index: Integer): Int64; cdecl; external AVUTIL_LIBNAME name _PU + 'av_channel_layout_extract_channel';

(**
 * Get the name of a given channel.
 *
 * @return channel name on success, NULL on error.
 *
 * @deprecated use av_channel_name()
 *)
function av_get_channel_name(channel: Int64): PAnsiChar; cdecl; external AVUTIL_LIBNAME name _PU + 'av_get_channel_name';

(**
 * Get the description of a given channel.
 *
 * @param channel  a channel layout with a single channel
 * @return  channel description on success, NULL on error
 * @deprecated use av_channel_description()
 *)
function av_get_channel_description(channel: Int64): PAnsiChar; cdecl; external AVUTIL_LIBNAME name _PU + 'av_get_channel_description';

(**
 * Get the value and name of a standard channel layout.
 *
 * @param[in]  index   index in an internal list, starting at 0
 * @param[out] layout  channel layout mask
 * @param[out] name    name of the layout
 * @return  0  if the layout exists,
 *          <0 if index is beyond the limits
 * @deprecated use av_channel_layout_standard()
 *)
function av_get_standard_channel_layout(index: Cardinal; layout: PInt64;
                                                  const name: PPAnsiChar): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_get_standard_channel_layout';
{$ENDIF}

(**
 * Get a human readable string in an abbreviated form describing a given channel.
 * This is the inverse function of @ref av_channel_from_string().
 *
 * @param buf pre-allocated buffer where to put the generated string
 * @param buf_size size in bytes of the buffer.
 * @return amount of bytes needed to hold the output string, or a negative AVERROR
 *         on failure. If the returned value is bigger than buf_size, then the
 *         string was truncated.
 *)
function av_channel_name(buf: PAnsiChar; buf_size: Size_t; channel: TAVChannel): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_channel_name';

(**
 * bprint variant of av_channel_name().
 *
 * @note the string will be appended to the bprint buffer.
 *)
procedure av_channel_name_bprint(bp: PAVBPrint; channel_id: TAVChannel); cdecl; external AVUTIL_LIBNAME name _PU + 'av_channel_name_bprint';

(**
 * Get a human readable string describing a given channel.
 *
 * @param buf pre-allocated buffer where to put the generated string
 * @param buf_size size in bytes of the buffer.
 * @return amount of bytes needed to hold the output string, or a negative AVERROR
 *         on failure. If the returned value is bigger than buf_size, then the
 *         string was truncated.
 *)
function av_channel_description(buf: PAnsiChar; buf_size: Size_t; channel: TAVChannel): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_channel_description';

(**
 * bprint variant of av_channel_description().
 *
 * @note the string will be appended to the bprint buffer.
 *)
procedure av_channel_description_bprint(bp: PAVBPrint; channel_id: TAVChannel); cdecl; external AVUTIL_LIBNAME name _PU + 'av_channel_description_bprint';

(**
 * This is the inverse function of @ref av_channel_name().
 *
 * @return the channel with the given name
 *         AV_CHAN_NONE when name does not identify a known channel
 *)
  //TODO: function av_channel_from_string(const name: PAnsiChar): TAVChannel; cdecl; external AVUTIL_LIBNAME name _PU + 'av_channel_from_string';

(**
 * Initialize a native channel layout from a bitmask indicating which channels
 * are present.
 *
 * @param channel_layout the layout structure to be initialized
 * @param mask bitmask describing the channel layout
 *
 * @return 0 on success
 *         AVERROR(EINVAL) for invalid mask values
 *)
function av_channel_layout_from_mask(channel_layout: PAVChannelLayout; mask: Int64): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_channel_layout_from_mask';

(**
 * Initialize a channel layout from a given string description.
 * The input string can be represented by:
 *  - the formal channel layout name (returned by av_channel_layout_describe())
 *  - single or multiple channel names (returned by av_channel_name(), eg. "FL",
 *    or concatenated with "+", each optionally containing a custom name after
 *    a "@", eg. "FL@Left+FR@Right+LFE")
 *  - a decimal or hexadecimal value of a native channel layout (eg. "4" or "0x4")
 *  - the number of channels with default layout (eg. "4c")
 *  - the number of unordered channels (eg. "4C" or "4 channels")
 *  - the ambisonic order followed by optional non-diegetic channels (eg.
 *    "ambisonic 2+stereo")
 *
 * @param channel_layout input channel layout
 * @param str string describing the channel layout
 * @return 0 channel layout was detected, AVERROR_INVALIDATATA otherwise
 *)
function av_channel_layout_from_string(channel_layout: PAVChannelLayout;
                                  const str: PAnsiChar): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_channel_layout_from_string';

(**
 * Get the default channel layout for a given number of channels.
 *
 * @param channel_layout the layout structure to be initialized
 * @param nb_channels number of channels
 *)
procedure av_channel_layout_default(ch_layout: PAVChannelLayout; nb_channels: Integer); cdecl; external AVUTIL_LIBNAME name _PU + 'av_channel_layout_default';

(**
 * Iterate over all standard channel layouts.
 *
 * @param opaque a pointer where libavutil will store the iteration state. Must
 *               point to NULL to start the iteration.
 *
 * @return the standard channel layout or NULL when the iteration is
 *         finished
 *)
function av_channel_layout_standard(opaque: PPointer): PAVChannelLayout; cdecl; external AVUTIL_LIBNAME name _PU + 'av_channel_layout_standard';

(**
 * Free any allocated data in the channel layout and reset the channel
 * count to 0.
 *
 * @param channel_layout the layout structure to be uninitialized
 *)
procedure av_channel_layout_uninit(channel_layout: PAVChannelLayout); cdecl; external AVUTIL_LIBNAME name _PU + 'av_channel_layout_uninit';

(**
 * Make a copy of a channel layout. This differs from just assigning src to dst
 * in that it allocates and copies the map for AV_CHANNEL_ORDER_CUSTOM.
 *
 * @note the destination channel_layout will be always uninitialized before copy.
 *
 * @param dst destination channel layout
 * @param src source channel layout
 * @return 0 on success, a negative AVERROR on error.
 *)
function av_channel_layout_copy(dst: PAVChannelLayout; const src: PAVChannelLayout): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_channel_layout_copy';

(**
 * Get a human-readable string describing the channel layout properties.
 * The string will be in the same format that is accepted by
 * @ref av_channel_layout_from_string(), allowing to rebuild the same
 * channel layout, except for opaque pointers.
 *
 * @param channel_layout channel layout to be described
 * @param buf pre-allocated buffer where to put the generated string
 * @param buf_size size in bytes of the buffer.
 * @return amount of bytes needed to hold the output string, or a negative AVERROR
 *         on failure. If the returned value is bigger than buf_size, then the
 *         string was truncated.
 *)
function av_channel_layout_describe(const channel_layout: PAVChannelLayout;
                               buf: PAnsiChar; buf_size: Size_t): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_channel_layout_describe';

(**
 * bprint variant of av_channel_layout_describe().
 *
 * @note the string will be appended to the bprint buffer.
 * @return 0 on success, or a negative AVERROR value on failure.
 *)
function av_channel_layout_describe_bprint(const channel_layout: PAVChannelLayout;
                                      bp: PAVBPrint): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_channel_layout_describe_bprint';

(**
 * Get the channel with the given index in a channel layout.
 *
 * @param channel_layout input channel layout
 * @return channel with the index idx in channel_layout on success or
 *         AV_CHAN_NONE on failure (if idx is not valid or the channel order is
 *         unspecified)
 *)
function av_channel_layout_channel_from_index(const channel_layout: PAVChannelLayout; idx: Cardinal): TAVChannel; cdecl; external AVUTIL_LIBNAME name _PU + 'av_channel_layout_channel_from_index';

(**
 * Get the index of a given channel in a channel layout. In case multiple
 * channels are found, only the first match will be returned.
 *
 * @param channel_layout input channel layout
 * @return index of channel in channel_layout on success or a negative number if
 *         channel is not present in channel_layout.
 *)
function av_channel_layout_index_from_channel(const channel_layout: PAVChannelLayout;
                                         channel: TAVChannel): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_channel_layout_index_from_channel';

(**
 * Get the index in a channel layout of a channel described by the given string.
 * In case multiple channels are found, only the first match will be returned.
 *
 * This function accepts channel names in the same format as
 * @ref av_channel_from_string().
 *
 * @param channel_layout input channel layout
 * @return a channel index described by the given string, or a negative AVERROR
 *         value.
 *)
function av_channel_layout_index_from_string(const channel_layout: PAVChannelLayout;
                                        const name: PAnsiChar): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_channel_layout_index_from_string';

(**
 * Get a channel described by the given string.
 *
 * This function accepts channel names in the same format as
 * @ref av_channel_from_string().
 *
 * @param channel_layout input channel layout
 * @return a channel described by the given string in channel_layout on success
 *         or AV_CHAN_NONE on failure (if the string is not valid or the channel
 *         order is unspecified)
 *)
function av_channel_layout_channel_from_string(const channel_layout: PAVChannelLayout;
                                      const name: PAnsiChar): TAVChannel; cdecl; external AVUTIL_LIBNAME name _PU + 'av_channel_layout_channel_from_string';

(**
 * Find out what channels from a given set are present in a channel layout,
 * without regard for their positions.
 *
 * @param channel_layout input channel layout
 * @param mask a combination of AV_CH_* representing a set of channels
 * @return a bitfield representing all the channels from mask that are present
 *         in channel_layout
 *)
function av_channel_layout_subset(const channel_layout: PAVChannelLayout;
                                  mask: Int64): Int64; cdecl; external AVUTIL_LIBNAME name _PU + 'av_channel_layout_subset';

(**
 * Check whether a channel layout is valid, i.e. can possibly describe audio
 * data.
 *
 * @param channel_layout input channel layout
 * @return 1 if channel_layout is valid, 0 otherwise.
 *)
function av_channel_layout_check(const channel_layout: PAVChannelLayout): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_channel_layout_check';

(**
 * Check whether two channel layouts are semantically the same, i.e. the same
 * channels are present on the same positions in both.
 *
 * If one of the channel layouts is AV_CHANNEL_ORDER_UNSPEC, while the other is
 * not, they are considered to be unequal. If both are AV_CHANNEL_ORDER_UNSPEC,
 * they are considered equal iff the channel counts are the same in both.
 *
 * @param chl input channel layout
 * @param chl1 input channel layout
 * @return 0 if chl and chl1 are equal, 1 if they are not equal. A negative
 *         AVERROR code if one or both are invalid.
 *)
function av_channel_layout_compare(const chl: PAVChannelLayout; const chl1: PAVChannelLayout): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_channel_layout_compare';

(**
 * @}
 * @}
 *)

implementation

end.
