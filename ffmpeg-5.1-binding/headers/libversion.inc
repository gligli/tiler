const
  // libavcodec
  LIBAVCODEC_VERSION_MAJOR = 59;
  LIBAVCODEC_VERSION_MINOR = 37;
  LIBAVCODEC_VERSION_MICRO = 100;
  LIBAVCODEC_VERSION       = '59.37.100';
  LIBAVCODEC_VERSION_INT   = (LIBAVCODEC_VERSION_MAJOR shl 16) or
                             (LIBAVCODEC_VERSION_MINOR shl 8) or
                              LIBAVCODEC_VERSION_MICRO;
  LIBAVCODEC_BUILD         = LIBAVCODEC_VERSION_INT;
  LIBAVCODEC_IDENT         = 'Lavc' + LIBAVCODEC_VERSION;

(**
 * FF_API_* defines may be placed below to indicate public API that will be
 * dropped at a future version bump. The defines themselves are not part of
 * the public API and may change, break or disappear at any time.
 *
 * @note, when bumping the major version it is recommended to manually
 * disable each FF_API_* in its own commit instead of disabling them all
 * at once through the bump. This improves the git bisect-ability of the change.
 *)

{$IF LIBAVCODEC_VERSION_MAJOR < 60}
  {$DEFINE FF_API_OPENH264_SLICE_MODE}
  {$DEFINE FF_API_OPENH264_CABAC}
  {$DEFINE FF_API_UNUSED_CODEC_CAPS}
  {$DEFINE FF_API_THREAD_SAFE_CALLBACKS}
  {$DEFINE FF_API_DEBUG_MV}
  {$DEFINE FF_API_GET_FRAME_CLASS}
  {$DEFINE FF_API_AUTO_THREADS}
  {$DEFINE FF_API_INIT_PACKET}
  {$DEFINE FF_API_AVCTX_TIMEBASE}
  {$DEFINE FF_API_FLAG_TRUNCATED}
  {$DEFINE FF_API_SUB_TEXT_FORMAT}
  {$DEFINE FF_API_IDCT_NONE}
  {$DEFINE FF_API_SVTAV1_OPTS}
{$IFEND}

  // libavdevice
  LIBAVDEVICE_VERSION_MAJOR = 59;
  LIBAVDEVICE_VERSION_MINOR = 7;
  LIBAVDEVICE_VERSION_MICRO = 100;
  LIBAVDEVICE_VERSION       = '59.7.100';
  LIBAVDEVICE_VERSION_INT   = (LIBAVDEVICE_VERSION_MAJOR shl 16) or
                              (LIBAVDEVICE_VERSION_MINOR shl 8) or
                               LIBAVDEVICE_VERSION_MICRO;
  LIBAVDEVICE_BUILD         = LIBAVDEVICE_VERSION_INT;
  LIBAVDEVICE_IDENT         = 'Lavc' + LIBAVDEVICE_VERSION;

(**
 * FF_API_* defines may be placed below to indicate public API that will be
 * dropped at a future version bump. The defines themselves are not part of
 * the public API and may change, break or disappear at any time.
 *)
{$IF LIBAVDEVICE_VERSION_MAJOR < 60}
  {$DEFINE FF_API_DEVICE_CAPABILITIES}
{$IFEND}

  // libavfilter
  LIBAVFILTER_VERSION_MAJOR  = 8;
  LIBAVFILTER_VERSION_MINOR  = 44;
  LIBAVFILTER_VERSION_MICRO  = 100;
  LIBAVFILTER_VERSION        = '8.44.100';
  LIBAVFILTER_VERSION_INT    = (LIBAVFILTER_VERSION_MAJOR shl 16) or
                               (LIBAVFILTER_VERSION_MINOR shl 8) or
                                LIBAVFILTER_VERSION_MICRO;
  LIBAVFILTER_BUILD          = LIBAVFILTER_VERSION_INT;
  LIBAVFILTER_IDENT          = 'Lavfi' + LIBAVFILTER_VERSION;

(**
 * FF_API_* defines may be placed below to indicate public API that will be
 * dropped at a future version bump. The defines themselves are not part of
 * the public API and may change, break or disappear at any time.
 *)

{$IF LIBAVFILTER_VERSION_MAJOR < 9}
  {$DEFINE FF_API_SWS_PARAM_OPTION}
  {$DEFINE FF_API_BUFFERSINK_ALLOC}
  {$DEFINE FF_API_PAD_COUNT}
{$IFEND}

  // libavformat
  LIBAVFORMAT_VERSION_MAJOR = 59;
  LIBAVFORMAT_VERSION_MINOR = 27;
  LIBAVFORMAT_VERSION_MICRO = 100;
  LIBAVFORMAT_VERSION       = '59.27.100';
  LIBAVFORMAT_VERSION_INT   = (LIBAVFORMAT_VERSION_MAJOR shl 16) or
                              (LIBAVFORMAT_VERSION_MINOR shl 8) or
                               LIBAVFORMAT_VERSION_MICRO;
  LIBAVFORMAT_BUILD         = LIBAVFORMAT_VERSION_INT;
  LIBAVFORMAT_IDENT         = 'Lavf' + LIBAVFORMAT_VERSION;

(**
 * FF_API_* defines may be placed below to indicate public API that will be
 * dropped at a future version bump. The defines themselves are not part of
 * the public API and may change, break or disappear at any time.
 *
 * @note, when bumping the major version it is recommended to manually
 * disable each FF_API_* in its own commit instead of disabling them all
 * at once through the bump. This improves the git bisect-ability of the change.
 *
 *)

{$IF LIBAVFORMAT_VERSION_MAJOR < 60}
  {$DEFINE FF_API_LAVF_PRIV_OPT}
  {$DEFINE FF_API_COMPUTE_PKT_FIELDS2}
  {$DEFINE FF_API_AVIOCONTEXT_WRITTEN}
  {$DEFINE FF_HLS_TS_OPTIONS}
{$IFEND}
{$IF LIBAVFORMAT_VERSION_MAJOR > 59}
  {$DEFINE FF_API_AVSTREAM_CLASS}
{$IFEND}

{$DEFINE FF_API_R_FRAME_RATE}

  // libavutil
  LIBAVUTIL_VERSION_MAJOR = 57;
  LIBAVUTIL_VERSION_MINOR = 28;
  LIBAVUTIL_VERSION_MICRO = 100;
  LIBAVUTIL_VERSION       = '57.28.100';
  LIBAVUTIL_VERSION_INT   = (LIBAVUTIL_VERSION_MAJOR shl 16) or
                            (LIBAVUTIL_VERSION_MINOR shl 8) or
                             LIBAVUTIL_VERSION_MICRO;
  LIBAVUTIL_BUILD         = LIBAVUTIL_VERSION_INT;
  LIBAVUTIL_IDENT         = 'Lavu' + LIBAVUTIL_VERSION;

(**
 * @defgroup lavu_depr_guards Deprecation Guards
 * FF_API_* defines may be placed below to indicate public API that will be
 * dropped at a future version bump. The defines themselves are not part of
 * the public API and may change, break or disappear at any time.
 *
 * @note, when bumping the major version it is recommended to manually
 * disable each FF_API_* in its own commit instead of disabling them all
 * at once through the bump. This improves the git bisect-ability of the change.
 *
 * @{
 *)

{$IF LIBAVUTIL_VERSION_MAJOR < 58}
  {$DEFINE FF_API_D2STR}
  {$DEFINE FF_API_DECLARE_ALIGNED}
  {$DEFINE FF_API_COLORSPACE_NAME}
  {$DEFINE FF_API_AV_MALLOCZ_ARRAY}
  {$DEFINE FF_API_FIFO_PEEK2}
  {$DEFINE FF_API_FIFO_OLD_API}
  {$DEFINE FF_API_XVMC}
  {$DEFINE FF_API_OLD_CHANNEL_LAYOUT}
  {$DEFINE FF_API_AV_FOPEN_UTF8}
{$IFEND}

(**
 * @}
 *)

  // libswresample
  LIBSWRESAMPLE_VERSION_MAJOR = 4;
  LIBSWRESAMPLE_VERSION_MINOR = 7;
  LIBSWRESAMPLE_VERSION_MICRO = 100;
  LIBSWRESAMPLE_VERSION       = '4.7.100';
  LIBSWRESAMPLE_VERSION_INT   = (LIBSWRESAMPLE_VERSION_MAJOR shl 16) or
                                (LIBSWRESAMPLE_VERSION_MINOR shl 8) or
                                 LIBSWRESAMPLE_VERSION_MICRO;
  LIBSWRESAMPLE_BUILD         = LIBSWRESAMPLE_VERSION_INT;
  LIBSWRESAMPLE_IDENT         = 'SwS' + LIBSWRESAMPLE_VERSION;

  // libswscale
  LIBSWSCALE_VERSION_MAJOR = 6;
  LIBSWSCALE_VERSION_MINOR = 7;
  LIBSWSCALE_VERSION_MICRO = 100;
  LIBSWSCALE_VERSION       = '6.7.100';
  LIBSWSCALE_VERSION_INT   = (LIBSWSCALE_VERSION_MAJOR shl 16) or
                             (LIBSWSCALE_VERSION_MINOR shl 8) or
                              LIBSWSCALE_VERSION_MICRO;
  LIBSWSCALE_BUILD         = LIBSWSCALE_VERSION_INT;
  LIBSWSCALE_IDENT         = 'SwS' + LIBSWSCALE_VERSION;

(**
 * FF_API_* defines may be placed below to indicate public API that will be
 * dropped at a future version bump. The defines themselves are not part of
 * the public API and may change, break or disappear at any time.
 *)

{$IFDEF MSWINDOWS}
  CLibPrefix = '';
  CLibConnector = '-';
  CLibExtension = '.dll';
  _PU = '';
{$ENDIF}
{$IFDEF POSIX}
  CLibPrefix = 'lib';
  CLibConnector = '.';
  CLibExtension = '.dylib';
  // On Mac OSX, cdecl names have a preceeding underscore
  // if x86 native backend.
  {$IF Defined(UNDERSCOREIMPORTNAME)}
    _PU = '_';
  {$ELSE}
    _PU = '';
  {$IFEND}
{$ENDIF}

  SLIBAVCODEC_VERSION_MAJOR    = '59';
  SLIBAVDEVICE_VERSION_MAJOR   = '59';
  SLIBAVFILTER_VERSION_MAJOR   = '8';
  SLIBAVFORMAT_VERSION_MAJOR   = '59';
  SLIBAVUTIL_VERSION_MAJOR     = '57';
  SLIBSWRESAMPLE_VERSION_MAJOR = '4';
  SLIBSWSCALE_VERSION_MAJOR    = '6';

  AVCODEC_LIBNAME    = CLibPrefix + 'avcodec'    + CLibConnector + SLIBAVCODEC_VERSION_MAJOR    + CLibExtension;
  AVDEVICE_LIBNAME   = CLibPrefix + 'avdevice'   + CLibConnector + SLIBAVDEVICE_VERSION_MAJOR   + CLibExtension;
  AVFILTER_LIBNAME   = CLibPrefix + 'avfilter'   + CLibConnector + SLIBAVFILTER_VERSION_MAJOR   + CLibExtension;
  AVFORMAT_LIBNAME   = CLibPrefix + 'avformat'   + CLibConnector + SLIBAVFORMAT_VERSION_MAJOR   + CLibExtension;
  AVUTIL_LIBNAME     = CLibPrefix + 'avutil'     + CLibConnector + SLIBAVUTIL_VERSION_MAJOR     + CLibExtension;
  SWRESAMPLE_LIBNAME = CLibPrefix + 'swresample' + CLibConnector + SLIBSWRESAMPLE_VERSION_MAJOR + CLibExtension;
  SWSCALE_LIBNAME    = CLibPrefix + 'swscale'    + CLibConnector + SLIBSWSCALE_VERSION_MAJOR    + CLibExtension;
