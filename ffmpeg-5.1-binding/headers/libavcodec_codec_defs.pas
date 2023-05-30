(*
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

(*
 * FFVCL - Delphi FFmpeg VCL Components
 * http://www.DelphiFFmpeg.com
 *
 * Original file: libavcodec/defs.h
 * Ported by CodeCoolie@CNSW 2022/02/09 -> $Date:: 2022-08-28 #$
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

unit libavcodec_codec_defs;

interface

{$I CompilerDefines.inc}

uses
  FFTypes;

{$I libversion.inc}

(**
 * @file
 * @ingroup libavc
 * Misc types and constants that do not belong anywhere else.
 *)

(**
 * @ingroup lavc_decoding
 * Required number of additionally allocated bytes at the end of the input bitstream for decoding.
 * This is mainly needed because some optimized bitstream readers read
 * 32 or 64 bit at once and could read over the end.<br>
 * Note: If the first 23 bits of the additional bytes are not 0, then damaged
 * MPEG bitstreams could cause overread and segfault.
 *)
const
  AV_INPUT_BUFFER_PADDING_SIZE = 64;

(**
 * @ingroup lavc_decoding
 *)
type
  TAVDiscard = (
    (* We leave some space between them for extensions (drop some
     * keyframes for intra-only or drop just some bidir frames). *)
    AVDISCARD_NONE    =-16, ///< discard nothing
    AVDISCARD_DEFAULT =  0, ///< discard useless packets like 0 size packets in avi
    AVDISCARD_NONREF  =  8, ///< discard all non reference
    AVDISCARD_BIDIR   = 16, ///< discard all bidirectional frames
    AVDISCARD_NONINTRA= 24, ///< discard all non intra frames
    AVDISCARD_NONKEY  = 32, ///< discard all frames except keyframes
    AVDISCARD_ALL     = 48  ///< discard all
  );

  TAVAudioServiceType = (
    AV_AUDIO_SERVICE_TYPE_MAIN              = 0,
    AV_AUDIO_SERVICE_TYPE_EFFECTS           = 1,
    AV_AUDIO_SERVICE_TYPE_VISUALLY_IMPAIRED = 2,
    AV_AUDIO_SERVICE_TYPE_HEARING_IMPAIRED  = 3,
    AV_AUDIO_SERVICE_TYPE_DIALOGUE          = 4,
    AV_AUDIO_SERVICE_TYPE_COMMENTARY        = 5,
    AV_AUDIO_SERVICE_TYPE_EMERGENCY         = 6,
    AV_AUDIO_SERVICE_TYPE_VOICE_OVER        = 7,
    AV_AUDIO_SERVICE_TYPE_KARAOKE           = 8,
    AV_AUDIO_SERVICE_TYPE_NB                     ///< Not part of ABI
  );

(**
 * Pan Scan area.
 * This specifies the area which should be displayed.
 * Note there may be multiple such areas for one frame.
 *)
  TAVPanScan = record
    (**
     * id
     * - encoding: Set by user.
     * - decoding: Set by libavcodec.
     *)
    id: Integer;

    (**
     * width and height in 1/16 pel
     * - encoding: Set by user.
     * - decoding: Set by libavcodec.
     *)
    width: Integer;
    height: Integer;

    (**
     * position of the top left corner in 1/16 pel for up to 3 fields/frames
     * - encoding: Set by user.
     * - decoding: Set by libavcodec.
     *)
    //int16_t position[3][2];
    position: array[0..2] of array[0..1] of SmallInt; // int16_t
  end;

(**
 * This structure describes the bitrate properties of an encoded bitstream. It
 * roughly corresponds to a subset the VBV parameters for MPEG-2 or HRD
 * parameters for H.264/HEVC.
 *)
  PAVCPBProperties = ^TAVCPBProperties;
  TAVCPBProperties = record
    (**
     * Maximum bitrate of the stream, in bits per second.
     * Zero if unknown or unspecified.
     *)
    max_bitrate: Int64;
    (**
     * Minimum bitrate of the stream, in bits per second.
     * Zero if unknown or unspecified.
     *)
    min_bitrate: Int64;
    (**
     * Average bitrate of the stream, in bits per second.
     * Zero if unknown or unspecified.
     *)
    avg_bitrate: Int64;

    (**
     * The size of the buffer to which the ratecontrol is applied, in bits.
     * Zero if unknown or unspecified.
     *)
    buffer_size: Int64;

    (**
     * The delay between the time the packet this structure is associated with
     * is received and the time when it should be decoded, in periods of a 27MHz
     * clock.
     *
     * UINT64_MAX when unknown or unspecified.
     *)
    vbv_delay: Int64;
  end;

(**
 * Allocate a CPB properties structure and initialize its fields to default
 * values.
 *
 * @param size if non-NULL, the size of the allocated struct will be written
 *             here. This is useful for embedding it in side data.
 *
 * @return the newly allocated struct or NULL on failure
 *)
function av_cpb_properties_alloc(size: Size_t): PAVCPBProperties; cdecl; external AVCODEC_LIBNAME name _PU + 'av_cpb_properties_alloc';

type
(**
 * This structure supplies correlation between a packet timestamp and a wall clock
 * production time. The definition follows the Producer Reference Time ('prft')
 * as defined in ISO/IEC 14496-12
 *)
  TAVProducerReferenceTime = record
    (**
     * A UTC timestamp, in microseconds, since Unix epoch (e.g, av_gettime()).
     *)
    wallclock: Int64;
    flags: Integer;
  end;

(**
 * Encode extradata length to a buffer. Used by xiph codecs.
 *
 * @param s buffer to write to; must be at least (v/255+1) bytes long
 * @param v size of extradata in bytes
 * @return number of bytes written to the buffer.
 *)
function av_xiphlacing(s: Byte; v: Cardinal): Cardinal; cdecl; external AVCODEC_LIBNAME name _PU + 'av_xiphlacing';

implementation

end.
