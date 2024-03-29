(*
 * Copyright (C) 2011-2013 Michael Niedermayer (michaelni@gmx.at)
 *
 * This file is part of libswresample
 *
 * libswresample is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * libswresample is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with libswresample; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 *)

(**
 * @file
 * @ingroup lswr
 * libswresample public header
 *)

(*
 * FFVCL - Delphi FFmpeg VCL Components
 * http://www.DelphiFFmpeg.com
 *
 * Original file: libswresample/swresample.h
 * Ported by CodeCoolie@CNSW 2011/11/29 -> $Date:: 2022-08-28 #$
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

unit libswresample;

interface

{$I CompilerDefines.inc}

uses
  FFTypes,
  libavutil_channel_layout,
  libavutil_frame,
  libavutil_log,
  libavutil_samplefmt;

{$I libversion.inc}

(**
 * @defgroup lswr libswresample
 * @{
 *
 * Audio resampling, sample format conversion and mixing library.
 *
 * Interaction with lswr is done through SwrContext, which is
 * allocated with swr_alloc() or swr_alloc_set_opts2(). It is opaque, so all parameters
 * must be set with the @ref avoptions API.
 *
 * The first thing you will need to do in order to use lswr is to allocate
 * SwrContext. This can be done with swr_alloc() or swr_alloc_set_opts2(). If you
 * are using the former, you must set options through the @ref avoptions API.
 * The latter function provides the same feature, but it allows you to set some
 * common options in the same statement.
 *
 * For example the following code will setup conversion from planar float sample
 * format to interleaved signed 16-bit integer, downsampling from 48kHz to
 * 44.1kHz and downmixing from 5.1 channels to stereo (using the default mixing
 * matrix). This is using the swr_alloc() function.
 * @code
 * SwrContext *swr = swr_alloc();
 * av_opt_set_channel_layout(swr, "in_channel_layout",  AV_CH_LAYOUT_5POINT1, 0);
 * av_opt_set_channel_layout(swr, "out_channel_layout", AV_CH_LAYOUT_STEREO,  0);
 * av_opt_set_int(swr, "in_sample_rate",     48000,                0);
 * av_opt_set_int(swr, "out_sample_rate",    44100,                0);
 * av_opt_set_sample_fmt(swr, "in_sample_fmt",  AV_SAMPLE_FMT_FLTP, 0);
 * av_opt_set_sample_fmt(swr, "out_sample_fmt", AV_SAMPLE_FMT_S16,  0);
 * @endcode
 *
 * The same job can be done using swr_alloc_set_opts2() as well:
 * @code
 * SwrContext *swr = NULL;
 * int ret = swr_alloc_set_opts2(&swr,         // we're allocating a new context
 *                       &(AVChannelLayout)AV_CHANNEL_LAYOUT_STEREO, // out_ch_layout
 *                       AV_SAMPLE_FMT_S16,    // out_sample_fmt
 *                       44100,                // out_sample_rate
 *                       &(AVChannelLayout)AV_CHANNEL_LAYOUT_5POINT1, // in_ch_layout
 *                       AV_SAMPLE_FMT_FLTP,   // in_sample_fmt
 *                       48000,                // in_sample_rate
 *                       0,                    // log_offset
 *                       NULL);                // log_ctx
 * @endcode
 *
 * Once all values have been set, it must be initialized with swr_init(). If
 * you need to change the conversion parameters, you can change the parameters
 * using @ref AVOptions, as described above in the first example; or by using
 * swr_alloc_set_opts2(), but with the first argument the allocated context.
 * You must then call swr_init() again.
 *
 * The conversion itself is done by repeatedly calling swr_convert().
 * Note that the samples may get buffered in swr if you provide insufficient
 * output space or if sample rate conversion is done, which requires "future"
 * samples. Samples that do not require future input can be retrieved at any
 * time by using swr_convert() (in_count can be set to 0).
 * At the end of conversion the resampling buffer can be flushed by calling
 * swr_convert() with NULL in and 0 in_count.
 *
 * The samples used in the conversion process can be managed with the libavutil
 * @ref lavu_sampmanip "samples manipulation" API, including av_samples_alloc()
 * function used in the following example.
 *
 * The delay between input and output, can at any time be found by using
 * swr_get_delay().
 *
 * The following code demonstrates the conversion loop assuming the parameters
 * from above and caller-defined functions get_input() and handle_output():
 * @code
 * uint8_t **input;
 * int in_samples;
 *
 * while (get_input(&input, &in_samples)) {
 *     uint8_t *output;
 *     int out_samples = av_rescale_rnd(swr_get_delay(swr, 48000) +
 *                                      in_samples, 44100, 48000, AV_ROUND_UP);
 *     av_samples_alloc(&output, NULL, 2, out_samples,
 *                      AV_SAMPLE_FMT_S16, 0);
 *     out_samples = swr_convert(swr, &output, out_samples,
 *                                      input, in_samples);
 *     handle_output(output, out_samples);
 *     av_freep(&output);
 * }
 * @endcode
 *
 * When the conversion is finished, the conversion
 * context and everything associated with it must be freed with swr_free().
 * A swr_close() function is also available, but it exists mainly for
 * compatibility with libavresample, and is not required to be called.
 *
 * There will be no memory leak if the data is not completely flushed before
 * swr_free().
 *)

const
(**
 * @name Option constants
 * These constants are used for the @ref avoptions interface for lswr.
 * @{
 *
 *)

  SWR_FLAG_RESAMPLE = 1; ///< Force resampling even if equal sample rate
//TODO use int resample ?
//long term TODO can we enable this dynamically?

(** Dithering algorithms *)
type
  TSwrDitherType = (
    SWR_DITHER_NONE = 0,
    SWR_DITHER_RECTANGULAR,
    SWR_DITHER_TRIANGULAR,
    SWR_DITHER_TRIANGULAR_HIGHPASS,

    SWR_DITHER_NS = 64,         ///< not part of API/ABI
    SWR_DITHER_NS_LIPSHITZ,
    SWR_DITHER_NS_F_WEIGHTED,
    SWR_DITHER_NS_MODIFIED_E_WEIGHTED,
    SWR_DITHER_NS_IMPROVED_E_WEIGHTED,
    SWR_DITHER_NS_SHIBATA,
    SWR_DITHER_NS_LOW_SHIBATA,
    SWR_DITHER_NS_HIGH_SHIBATA,
    SWR_DITHER_NB               ///< not part of API/ABI
  );

(** Resampling Engines *)
  TSwrEngine = (
    SWR_ENGINE_SWR,             (**< SW Resampler *)
    SWR_ENGINE_SOXR,            (**< SoX Resampler *)
    SWR_ENGINE_NB               ///< not part of API/ABI
  );

(** Resampling Filter Types *)
  TSwrFilterType = (
    SWR_FILTER_TYPE_CUBIC,              (**< Cubic *)
    SWR_FILTER_TYPE_BLACKMAN_NUTTALL,   (**< Blackman Nuttall windowed sinc *)
    SWR_FILTER_TYPE_KAISER              (**< Kaiser windowed sinc *)
  );

(**
 * @}
 *)

(**
 * The libswresample context. Unlike libavcodec and libavformat, this structure
 * is opaque. This means that if you would like to set options, you must use
 * the @ref avoptions API and cannot directly set values to members of the
 * structure.
 *)
  PPSwrContext = ^PSwrContext;
  PSwrContext = ^TSwrContext;
  TSwrContext = record
    // need {$ALIGN 8}
    // defined in libswresample/swresample_internal.h
  end;

(**
 * Get the AVClass for SwrContext. It can be used in combination with
 * AV_OPT_SEARCH_FAKE_OBJ for examining options.
 *
 * @see av_opt_find().
 * @return the AVClass of SwrContext
 *)
function swr_get_class: PAVClass; cdecl; external SWRESAMPLE_LIBNAME name _PU + 'swr_get_class';

(**
 * @name SwrContext constructor functions
 * @{
 *)

(**
 * Allocate SwrContext.
 *
 * If you use this function you will need to set the parameters (manually or
 * with swr_alloc_set_opts2()) before calling swr_init().
 *
 * @see swr_alloc_set_opts2(), swr_init(), swr_free()
 * @return NULL on error, allocated context otherwise
 *)
function swr_alloc: PSwrContext; cdecl; external SWRESAMPLE_LIBNAME name _PU + 'swr_alloc';

(**
 * Initialize context after user parameters have been set.
 * @note The context must be configured using the AVOption API.
 *
 * @see av_opt_set_int()
 * @see av_opt_set_dict()
 *
 * @param[in,out]   s Swr context to initialize
 * @return AVERROR error code in case of failure.
 *)
function swr_init(s: PSwrContext): Integer; cdecl; external SWRESAMPLE_LIBNAME name _PU + 'swr_init';

(**
 * Check whether an swr context has been initialized or not.
 *
 * @param[in]       s Swr context to check
 * @see swr_init()
 * @return positive if it has been initialized, 0 if not initialized
 *)
function swr_is_initialized(s: PSwrContext): Integer; cdecl; external SWRESAMPLE_LIBNAME name _PU + 'swr_is_initialized';

{$IFDEF FF_API_OLD_CHANNEL_LAYOUT}
(**
 * Allocate SwrContext if needed and set/reset common parameters.
 *
 * This function does not require s to be allocated with swr_alloc(). On the
 * other hand, swr_alloc() can use swr_alloc_set_opts() to set the parameters
 * on the allocated context.
 *
 * @param s               existing Swr context if available, or NULL if not
 * @param out_ch_layout   output channel layout (AV_CH_LAYOUT_* )
 * @param out_sample_fmt  output sample format (AV_SAMPLE_FMT_* ).
 * @param out_sample_rate output sample rate (frequency in Hz)
 * @param in_ch_layout    input channel layout (AV_CH_LAYOUT_* )
 * @param in_sample_fmt   input sample format (AV_SAMPLE_FMT_* ).
 * @param in_sample_rate  input sample rate (frequency in Hz)
 * @param log_offset      logging level offset
 * @param log_ctx         parent logging context, can be NULL
 *
 * @see swr_init(), swr_free()
 * @return NULL on error, allocated context otherwise
 * @deprecated use @ref swr_alloc_set_opts2()
 *)
function swr_alloc_set_opts(s: PSwrContext;
                                out_ch_layout: Int64; out_sample_fmt: TAVSampleFormat; out_sample_rate: Integer;
                                in_ch_layout: Int64; in_sample_fmt: TAVSampleFormat; in_sample_rate: Integer;
                                log_offset: Integer; log_ctx: Pointer): PSwrContext; cdecl; external SWRESAMPLE_LIBNAME name _PU + 'swr_alloc_set_opts';
{$ENDIF}

(**
 * Allocate SwrContext if needed and set/reset common parameters.
 *
 * This function does not require *ps to be allocated with swr_alloc(). On the
 * other hand, swr_alloc() can use swr_alloc_set_opts2() to set the parameters
 * on the allocated context.
 *
 * @param ps              Pointer to an existing Swr context if available, or to NULL if not.
 *                        On success, *ps will be set to the allocated context.
 * @param out_ch_layout   output channel layout (e.g. AV_CHANNEL_LAYOUT_* )
 * @param out_sample_fmt  output sample format (AV_SAMPLE_FMT_* ).
 * @param out_sample_rate output sample rate (frequency in Hz)
 * @param in_ch_layout    input channel layout (e.g. AV_CHANNEL_LAYOUT_* )
 * @param in_sample_fmt   input sample format (AV_SAMPLE_FMT_* ).
 * @param in_sample_rate  input sample rate (frequency in Hz)
 * @param log_offset      logging level offset
 * @param log_ctx         parent logging context, can be NULL
 *
 * @see swr_init(), swr_free()
 * @return 0 on success, a negative AVERROR code on error.
 *         On error, the Swr context is freed and *ps set to NULL.
 *)
function swr_alloc_set_opts2(ps: PPSwrContext;
                        out_ch_layout: PAVChannelLayout; out_sample_fmt: TAVSampleFormat; out_sample_rate: Integer;
                        in_ch_layout: PAVChannelLayout; in_sample_fmt: TAVSampleFormat; in_sample_rate: Integer;
                        log_offset: Integer; log_ctx: Pointer): Integer; cdecl; external SWRESAMPLE_LIBNAME name _PU + 'swr_alloc_set_opts2';

(**
 * @}
 *
 * @name SwrContext destructor functions
 * @{
 *)

(**
 * Free the given SwrContext and set the pointer to NULL.
 *
 * @param[in] s a pointer to a pointer to Swr context
 *)
procedure swr_free(s: PPSwrContext); cdecl; external SWRESAMPLE_LIBNAME name _PU + 'swr_free';

(**
 * Closes the context so that swr_is_initialized() returns 0.
 *
 * The context can be brought back to life by running swr_init(),
 * swr_init() can also be used without swr_close().
 * This function is mainly provided for simplifying the usecase
 * where one tries to support libavresample and libswresample.
 *
 * @param[in,out] s Swr context to be closed
 *)
procedure swr_close(s: PSwrContext); cdecl; external SWRESAMPLE_LIBNAME name _PU + 'swr_close';

(**
 * @}
 *
 * @name Core conversion functions
 * @{
 *)

(**
 * Convert audio.
 *
 * in and in_count can be set to 0 to flush the last few samples out at the
 * end.
 *
 * If more input is provided than output space, then the input will be buffered.
 * You can avoid this buffering by using swr_get_out_samples() to retrieve an
 * upper bound on the required number of output samples for the given number of
 * input samples. Conversion will run directly without copying whenever possible.
 *
 * @param s         allocated Swr context, with parameters set
 * @param out       output buffers, only the first one need be set in case of packed audio
 * @param out_count amount of space available for output in samples per channel
 * @param in        input buffers, only the first one need to be set in case of packed audio
 * @param in_count  number of input samples available in one channel
 *
 * @return number of samples output per channel, negative value on error
 *)
function swr_convert(s: PSwrContext; out_: PPByte; out_count: Integer;
                              const in_: PPByte; in_count: Integer): Integer; cdecl; external SWRESAMPLE_LIBNAME name _PU + 'swr_convert';

(**
 * Convert the next timestamp from input to output
 * timestamps are in 1/(in_sample_rate * out_sample_rate) units.
 *
 * @note There are 2 slightly differently behaving modes.
 *       @li When automatic timestamp compensation is not used, (min_compensation >= FLT_MAX)
 *              in this case timestamps will be passed through with delays compensated
 *       @li When automatic timestamp compensation is used, (min_compensation < FLT_MAX)
 *              in this case the output timestamps will match output sample numbers.
 *              See ffmpeg-resampler(1) for the two modes of compensation.
 *
 * @param s[in]     initialized Swr context
 * @param pts[in]   timestamp for the next input sample, INT64_MIN if unknown
 * @see swr_set_compensation(), swr_drop_output(), and swr_inject_silence() are
 *      function used internally for timestamp compensation.
 * @return the output timestamp for the next output sample
 *)
function swr_next_pts(s: PSwrContext; pts: Int64): Int64; cdecl; external SWRESAMPLE_LIBNAME name _PU + 'swr_next_pts';

(**
 * @}
 *
 * @name Low-level option setting functions
 * These functons provide a means to set low-level options that is not possible
 * with the AVOption API.
 * @{
 *)

(**
 * Activate resampling compensation ("soft" compensation). This function is
 * internally called when needed in swr_next_pts().
 *
 * @param[in,out] s             allocated Swr context. If it is not initialized,
 *                              or SWR_FLAG_RESAMPLE is not set, swr_init() is
 *                              called with the flag set.
 * @param[in]     sample_delta  delta in PTS per sample
 * @param[in]     compensation_distance number of samples to compensate for
 * @return    >= 0 on success, AVERROR error codes if:
 *            @li @c s is NULL,
 *            @li @c compensation_distance is less than 0,
 *            @li @c compensation_distance is 0 but sample_delta is not,
 *            @li compensation unsupported by resampler, or
 *            @li swr_init() fails when called.
 *)
function swr_set_compensation(s: PSwrContext; sample_delta, compensation_distance: Integer): Integer; cdecl; external SWRESAMPLE_LIBNAME name _PU + 'swr_set_compensation';

(**
 * Set a customized input channel mapping.
 *
 * @param[in,out] s           allocated Swr context, not yet initialized
 * @param[in]     channel_map customized input channel mapping (array of channel
 *                            indexes, -1 for a muted channel)
 * @return >= 0 on success, or AVERROR error code in case of failure.
 *)
function swr_set_channel_mapping(s: PSwrContext; const channel_map: PInteger): Integer; cdecl; external SWRESAMPLE_LIBNAME name _PU + 'swr_set_channel_mapping';

{$IFDEF FF_API_OLD_CHANNEL_LAYOUT}
(**
 * Generate a channel mixing matrix.
 *
 * This function is the one used internally by libswresample for building the
 * default mixing matrix. It is made public just as a utility function for
 * building custom matrices.
 *
 * @param in_layout           input channel layout
 * @param out_layout          output channel layout
 * @param center_mix_level    mix level for the center channel
 * @param surround_mix_level  mix level for the surround channel(s)
 * @param lfe_mix_level       mix level for the low-frequency effects channel
 * @param rematrix_maxval     if 1.0, coefficients will be normalized to prevent
 *                            overflow. if INT_MAX, coefficients will not be
 *                            normalized.
 * @param[out] matrix         mixing coefficients; matrix[i + stride * o] is
 *                            the weight of input channel i in output channel o.
 * @param stride              distance between adjacent input channels in the
 *                            matrix array
 * @param matrix_encoding     matrixed stereo downmix mode (e.g. dplii)
 * @param log_ctx             parent logging context, can be NULL
 * @return                    0 on success, negative AVERROR code on failure
 * @deprecated                use @ref swr_build_matrix2()
 *)
function swr_build_matrix(in_layout, out_layout: Int64;
                     center_mix_level, surround_mix_level,
                     lfe_mix_level, rematrix_maxval,
                     rematrix_volume: Double; matrix: PDouble;
                     stride: Integer; matrix_encoding: TAVMatrixEncoding;
                     log_ctx: Pointer): Integer; cdecl; external SWRESAMPLE_LIBNAME name _PU + 'swr_build_matrix';
{$ENDIF}

(**
 * Generate a channel mixing matrix.
 *
 * This function is the one used internally by libswresample for building the
 * default mixing matrix. It is made public just as a utility function for
 * building custom matrices.
 *
 * @param in_layout           input channel layout
 * @param out_layout          output channel layout
 * @param center_mix_level    mix level for the center channel
 * @param surround_mix_level  mix level for the surround channel(s)
 * @param lfe_mix_level       mix level for the low-frequency effects channel
 * @param rematrix_maxval     if 1.0, coefficients will be normalized to prevent
 *                            overflow. if INT_MAX, coefficients will not be
 *                            normalized.
 * @param[out] matrix         mixing coefficients; matrix[i + stride * o] is
 *                            the weight of input channel i in output channel o.
 * @param stride              distance between adjacent input channels in the
 *                            matrix array
 * @param matrix_encoding     matrixed stereo downmix mode (e.g. dplii)
 * @param log_ctx             parent logging context, can be NULL
 * @return                    0 on success, negative AVERROR code on failure
 *)
function swr_build_matrix2(const in_layout, out_layout: PAVChannelLayout;
                      center_mix_level, surround_mix_level,
                      lfe_mix_level, maxval,
                      rematrix_volume: Double; matrix: PDouble;
                      stride: Integer; matrix_encoding: TAVMatrixEncoding;
                      log_context: Pointer): Integer; cdecl; external SWRESAMPLE_LIBNAME name _PU + 'swr_build_matrix2';

(**
 * Set a customized remix matrix.
 *
 * @param s       allocated Swr context, not yet initialized
 * @param matrix  remix coefficients; matrix[i + stride * o] is
 *                the weight of input channel i in output channel o
 * @param stride  offset between lines of the matrix
 * @return  >= 0 on success, or AVERROR error code in case of failure.
 *)
function swr_set_matrix(s: PSwrContext; const matrix: PDouble; stride: Integer): Integer; cdecl; external SWRESAMPLE_LIBNAME name _PU + 'swr_set_matrix';

(**
 * @}
 *
 * @name Sample handling functions
 * @{
 *)

(**
 * Drops the specified number of output samples.
 *
 * This function, along with swr_inject_silence(), is called by swr_next_pts()
 * if needed for "hard" compensation.
 *
 * @param s     allocated Swr context
 * @param count number of samples to be dropped
 *
 * @return >= 0 on success, or a negative AVERROR code on failure
 *)
function swr_drop_output(s: PSwrContext; count: Integer): Integer; cdecl; external SWRESAMPLE_LIBNAME name _PU + 'swr_drop_output';

(**
 * Injects the specified number of silence samples.
 *
 * This function, along with swr_drop_output(), is called by swr_next_pts()
 * if needed for "hard" compensation.
 *
 * @param s     allocated Swr context
 * @param count number of samples to be dropped
 *
 * @return >= 0 on success, or a negative AVERROR code on failure
 *)
function swr_inject_silence(s: PSwrContext; count: Integer): Integer; cdecl; external SWRESAMPLE_LIBNAME name _PU + 'swr_inject_silence';

(**
 * Gets the delay the next input sample will experience relative to the next output sample.
 *
 * Swresample can buffer data if more input has been provided than available
 * output space, also converting between sample rates needs a delay.
 * This function returns the sum of all such delays.
 * The exact delay is not necessarily an integer value in either input or
 * output sample rate. Especially when downsampling by a large value, the
 * output sample rate may be a poor choice to represent the delay, similarly
 * for upsampling and the input sample rate.
 *
 * @param s     swr context
 * @param base  timebase in which the returned delay will be:
 *              @li if it's set to 1 the returned delay is in seconds
 *              @li if it's set to 1000 the returned delay is in milliseconds
 *              @li if it's set to the input sample rate then the returned
 *                  delay is in input samples
 *              @li if it's set to the output sample rate then the returned
 *                  delay is in output samples
 *              @li if it's the least common multiple of in_sample_rate and
 *                  out_sample_rate then an exact rounding-free delay will be
 *                  returned
 * @returns     the delay in 1 / @c base units.
 *)
function swr_get_delay(s: PSwrContext; base: Int64): Int64; cdecl; external SWRESAMPLE_LIBNAME name _PU + 'swr_get_delay';

(**
 * Find an upper bound on the number of samples that the next swr_convert
 * call will output, if called with in_samples of input samples. This
 * depends on the internal state, and anything changing the internal state
 * (like further swr_convert() calls) will may change the number of samples
 * swr_get_out_samples() returns for the same number of input samples.
 *
 * @param in_samples    number of input samples.
 * @note any call to swr_inject_silence(), swr_convert(), swr_next_pts()
 *       or swr_set_compensation() invalidates this limit
 * @note it is recommended to pass the correct available buffer size
 *       to all functions like swr_convert() even if swr_get_out_samples()
 *       indicates that less would be used.
 * @returns an upper bound on the number of samples that the next swr_convert
 *          will output or a negative value to indicate an error
 *)
function swr_get_out_samples(s: PSwrContext; in_samples: Integer): Integer; cdecl; external SWRESAMPLE_LIBNAME name _PU + 'swr_get_out_samples';

(**
 * @}
 *
 * @name Configuration accessors
 * @{
 *)

(**
 * Return the @ref LIBSWRESAMPLE_VERSION_INT constant.
 *
 * This is useful to check if the build-time libswresample has the same version
 * as the run-time one.
 *
 * @returns     the unsigned int-typed version
 *)
function swresample_version: Cardinal; cdecl; external SWRESAMPLE_LIBNAME name _PU + 'swresample_version';

(**
 * Return the swr build-time configuration.
 *
 * @returns     the build-time @c ./configure flags
 *)
function swresample_configuration: PAnsiChar; cdecl; external SWRESAMPLE_LIBNAME name _PU + 'swresample_configuration';

(**
 * Return the swr license.
 *
 * @returns     the license of libswresample, determined at build-time
 *)
function swresample_license: PAnsiChar; cdecl; external SWRESAMPLE_LIBNAME name _PU + 'swresample_license';

(**
 * @}
 *
 * @name AVFrame based API
 * @{
 *)

(**
 * Convert the samples in the input AVFrame and write them to the output AVFrame.
 *
 * Input and output AVFrames must have channel_layout, sample_rate and format set.
 *
 * If the output AVFrame does not have the data pointers allocated the nb_samples
 * field will be set using av_frame_get_buffer()
 * is called to allocate the frame.
 *
 * The output AVFrame can be NULL or have fewer allocated samples than required.
 * In this case, any remaining samples not written to the output will be added
 * to an internal FIFO buffer, to be returned at the next call to this function
 * or to swr_convert().
 *
 * If converting sample rate, there may be data remaining in the internal
 * resampling delay buffer. swr_get_delay() tells the number of
 * remaining samples. To get this data as output, call this function or
 * swr_convert() with NULL input.
 *
 * If the SwrContext configuration does not match the output and
 * input AVFrame settings the conversion does not take place and depending on
 * which AVFrame is not matching AVERROR_OUTPUT_CHANGED, AVERROR_INPUT_CHANGED
 * or the result of a bitwise-OR of them is returned.
 *
 * @see swr_delay()
 * @see swr_convert()
 * @see swr_get_delay()
 *
 * @param swr             audio resample context
 * @param output          output AVFrame
 * @param input           input AVFrame
 * @return                0 on success, AVERROR on failure or nonmatching
 *                        configuration.
 *)
function swr_convert_frame(swr: PSwrContext;
                      output: PAVFrame; const intput: PAVFrame): Integer; cdecl; external SWRESAMPLE_LIBNAME name _PU + 'swr_convert_frame';

(**
 * Configure or reconfigure the SwrContext using the information
 * provided by the AVFrames.
 *
 * The original resampling context is reset even on failure.
 * The function calls swr_close() internally if the context is open.
 *
 * @see swr_close();
 *
 * @param swr             audio resample context
 * @param output          output AVFrame
 * @param input           input AVFrame
 * @return                0 on success, AVERROR on failure.
 *)
function swr_config_frame(swr: PSwrContext; const output, input: PAVFrame): Integer; cdecl; external SWRESAMPLE_LIBNAME name _PU + 'swr_config_frame';

(**
 * @}
 * @}
 *)

implementation

end.
