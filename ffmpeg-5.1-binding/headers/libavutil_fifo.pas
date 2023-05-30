(*
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
 * a very simple circular buffer FIFO implementation
 *)

(*
 * FFVCL - Delphi FFmpeg VCL Components
 * http://www.DelphiFFmpeg.com
 *
 * Original file: libavutil/fifo.h
 * Ported by CodeCoolie@CNSW 2008/03/25 -> $Date:: 2022-08-28 #$
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

unit libavutil_fifo;

interface

{$I CompilerDefines.inc}

uses
  FFTypes;

{$I libversion.inc}

(**
 * Automatically resize the FIFO on writes, so that the data fits. This
 * automatic resizing happens up to a limit that can be modified with
 * av_fifo_auto_grow_limit().
 *)
const
  AV_FIFO_FLAG_AUTO_GROW     = (1 shl 0);

type
  PPAVFifo = ^PAVFifo;
  PAVFifo = ^TAVFifo;
  TAVFifo = record
    // need {$ALIGN 8}
    // defined in libavutil/fifo.c
  end;

(**
 * Callback for writing or reading from a FIFO, passed to (and invoked from) the
 * av_fifo_*_cb() functions. It may be invoked multiple times from a single
 * av_fifo_*_cb() call and may process less data than the maximum size indicated
 * by nb_elems.
 *
 * @param opaque the opaque pointer provided to the av_fifo_*_cb() function
 * @param buf the buffer for reading or writing the data, depending on which
 *            av_fifo_*_cb function is called
 * @param nb_elems On entry contains the maximum number of elements that can be
 *                 read from / written into buf. On success, the callback should
 *                 update it to contain the number of elements actually written.
 *
 * @return 0 on success, a negative error code on failure (will be returned from
 *         the invoking av_fifo_*_cb() function)
 *)
  TAVFifoCB = function(opaque: Pointer; buf: Pointer; nb_elems: PSize_t): Integer; cdecl;

(**
 * Allocate and initialize an AVFifo with a given element size.
 *
 * @param elems     initial number of elements that can be stored in the FIFO
 * @param elem_size Size in bytes of a single element. Further operations on
 *                  the returned FIFO will implicitly use this element size.
 * @param flags a combination of AV_FIFO_FLAG_*
 *
 * @return newly-allocated AVFifo on success, a negative error code on failure
 *)
function av_fifo_alloc2(elems, elem_size: Size_t;
                       flags: Cardinal): PAVFifo; cdecl; external AVUTIL_LIBNAME name _PU + 'av_fifo_alloc2';

(**
 * @return Element size for FIFO operations. This element size is set at
 *         FIFO allocation and remains constant during its lifetime
 *)
function av_fifo_elem_size(const f: PAVFifo): Size_t; cdecl; external AVUTIL_LIBNAME name _PU + 'av_fifo_elem_size';

(**
 * Set the maximum size (in elements) to which the FIFO can be resized
 * automatically. Has no effect unless AV_FIFO_FLAG_AUTO_GROW is used.
 *)
procedure av_fifo_auto_grow_limit(f: PAVFifo; max_elems: Size_t); cdecl; external AVUTIL_LIBNAME name _PU + 'av_fifo_auto_grow_limit';

(**
 * @return number of elements available for reading from the given FIFO.
 *)
function av_fifo_can_read(const f: PAVFifo): Size_t; cdecl; external AVUTIL_LIBNAME name _PU + 'av_fifo_can_read';

(**
 * @return number of elements that can be written into the given FIFO.
 *)
function av_fifo_can_write(const f: PAVFifo): Size_t; cdecl; external AVUTIL_LIBNAME name _PU + 'av_fifo_can_write';

(**
 * Enlarge an AVFifo.
 *
 * On success, the FIFO will be large enough to hold exactly
 * inc + av_fifo_can_read() + av_fifo_can_write()
 * elements. In case of failure, the old FIFO is kept unchanged.
 *
 * @param f AVFifo to resize
 * @param inc number of elements to allocate for, in addition to the current
 *            allocated size
 * @return a non-negative number on success, a negative error code on failure
 *)
function av_fifo_grow2(f: PAVFifo; inc: Size_t): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_fifo_grow2';

(**
 * Write data into a FIFO.
 *
 * In case nb_elems > av_fifo_can_write(f), nothing is written and an error
 * is returned.
 *
 * @param f the FIFO buffer
 * @param buf Data to be written. nb_elems * av_fifo_elem_size(f) bytes will be
 *            read from buf on success.
 * @param nb_elems number of elements to write into FIFO
 *
 * @return a non-negative number on success, a negative error code on failure
 *)
function av_fifo_write(f: PAVFifo; const buf: Pointer; nb_elems: Size_t): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_fifo_write';

(**
 * Write data from a user-provided callback into a FIFO.
 *
 * @param f the FIFO buffer
 * @param read_cb Callback supplying the data to the FIFO. May be called
 *                multiple times.
 * @param opaque opaque user data to be provided to read_cb
 * @param nb_elems Should point to the maximum number of elements that can be
 *                 written. Will be updated to contain the number of elements
 *                 actually written.
 *
 * @return non-negative number on success, a negative error code on failure
 *)
function av_fifo_write_from_cb(f: PAVFifo; read_cb: TAVFifoCB;
                          opaque: Pointer; nb_elems: PSize_t): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_fifo_write_from_cb';

(**
 * Read data from a FIFO.
 *
 * In case nb_elems > av_fifo_can_read(f), nothing is read and an error
 * is returned.
 *
 * @param f the FIFO buffer
 * @param buf Buffer to store the data. nb_elems * av_fifo_elem_size(f) bytes
 *            will be written into buf on success.
 * @param nb_elems number of elements to read from FIFO
 *
 * @return a non-negative number on success, a negative error code on failure
 *)
function av_fifo_read(f: PAVFifo; buf: Pointer; nb_elems: Size_t): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_fifo_read';

(**
 * Feed data from a FIFO into a user-provided callback.
 *
 * @param f the FIFO buffer
 * @param write_cb Callback the data will be supplied to. May be called
 *                 multiple times.
 * @param opaque opaque user data to be provided to write_cb
 * @param nb_elems Should point to the maximum number of elements that can be
 *                 read. Will be updated to contain the total number of elements
 *                 actually sent to the callback.
 *
 * @return non-negative number on success, a negative error code on failure
 *)
function av_fifo_read_to_cb(f: PAVFifo; write_cb: TAVFifoCB;
                       opaque: Pointer; nb_elems: PSize_t): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_fifo_read_to_cb';

(**
 * Read data from a FIFO without modifying FIFO state.
 *
 * Returns an error if an attempt is made to peek to nonexistent elements
 * (i.e. if offset + nb_elems is larger than av_fifo_can_read(f)).
 *
 * @param f the FIFO buffer
 * @param buf Buffer to store the data. nb_elems * av_fifo_elem_size(f) bytes
 *            will be written into buf.
 * @param nb_elems number of elements to read from FIFO
 * @param offset number of initial elements to skip.
 *
 * @return a non-negative number on success, a negative error code on failure
 *)
function av_fifo_peek(f: PAVFifo; buf: Pointer; nb_elems, offset: Size_t): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_fifo_peek';

(**
 * Feed data from a FIFO into a user-provided callback.
 *
 * @param f the FIFO buffer
 * @param write_cb Callback the data will be supplied to. May be called
 *                 multiple times.
 * @param opaque opaque user data to be provided to write_cb
 * @param nb_elems Should point to the maximum number of elements that can be
 *                 read. Will be updated to contain the total number of elements
 *                 actually sent to the callback.
 * @param offset number of initial elements to skip; offset + *nb_elems must not
 *               be larger than av_fifo_can_read(f).
 *
 * @return a non-negative number on success, a negative error code on failure
 *)
function av_fifo_peek_to_cb(f: PAVFifo; write_cb: TAVFifoCB; opaque: Pointer;
                       nb_elems: PSize_t; offset: Size_t): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_fifo_peek_to_cb';

(**
 * Discard the specified amount of data from an AVFifo.
 * @param size number of elements to discard, MUST NOT be larger than
 *             av_fifo_can_read(f)
 *)
procedure av_fifo_drain2(f: PAVFifo; size: Size_t); cdecl; external AVUTIL_LIBNAME name _PU + 'av_fifo_drain2';

(*
 * Empty the AVFifo.
 * @param f AVFifo to reset
 *)
procedure av_fifo_reset2(f: PAVFifo); cdecl; external AVUTIL_LIBNAME name _PU + 'av_fifo_reset2';

(**
 * Free an AVFifo and reset pointer to NULL.
 * @param f Pointer to an AVFifo to free. *f == NULL is allowed.
 *)
procedure av_fifo_freep2(f: PPAVFifo); cdecl; external AVUTIL_LIBNAME name _PU + 'av_fifo_freep2';


{$IFDEF FF_API_FIFO_OLD_API}
type
  PPAVFifoBuffer = ^PAVFifoBuffer;
  PAVFifoBuffer = ^TAVFifoBuffer;
  TAVFifoBuffer = record
    buffer: PByte;
    rptr, wptr, eend: PByte;
    rndx, wndx: Cardinal;
  end;

  TfifoCall = procedure(v1, v2: Pointer; i: Integer); cdecl;
  TwriteCall = function(p1, p2: Pointer; i: Integer): Integer; cdecl;

(**
 * Initialize an AVFifoBuffer.
 * @param size of FIFO
 * @return AVFifoBuffer or NULL in case of memory allocation failure
 * @deprecated use av_fifo_alloc2()
 *)
function av_fifo_alloc(size: Cardinal): PAVFifoBuffer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_fifo_alloc';

(**
 * Initialize an AVFifoBuffer.
 * @param nmemb number of elements
 * @param size  size of the single element
 * @return AVFifoBuffer or NULL in case of memory allocation failure
 * @deprecated use av_fifo_alloc2()
 *)
function av_fifo_alloc_array(nmemb, size: Size_t): PAVFifoBuffer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_fifo_alloc_array';

(**
 * Free an AVFifoBuffer.
 * @param f AVFifoBuffer to free
 * @deprecated use the AVFifo API with av_fifo_freep2()
 *)
procedure av_fifo_free(f: PAVFifoBuffer); cdecl; external AVUTIL_LIBNAME name _PU + 'av_fifo_free';

(**
 * Free an AVFifoBuffer and reset pointer to NULL.
 * @param f AVFifoBuffer to free
 * @deprecated use the AVFifo API with av_fifo_freep2()
 *)
procedure av_fifo_freep(f: PPAVFifoBuffer); cdecl; external AVUTIL_LIBNAME name _PU + 'av_fifo_freep';

(**
 * Reset the AVFifoBuffer to the state right after av_fifo_alloc, in particular it is emptied.
 * @param f AVFifoBuffer to reset
 * @deprecated use av_fifo_reset2() with the new AVFifo-API
 *)
procedure av_fifo_reset(f: PAVFifoBuffer); cdecl; external AVUTIL_LIBNAME name _PU + 'av_fifo_reset';

(**
 * Return the amount of data in bytes in the AVFifoBuffer, that is the
 * amount of data you can read from it.
 * @param f AVFifoBuffer to read from
 * @return size
 * @deprecated use av_fifo_can_read() with the new AVFifo-API
 *)
function av_fifo_size(const f: PAVFifoBuffer): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_fifo_size';

(**
 * Return the amount of space in bytes in the AVFifoBuffer, that is the
 * amount of data you can write into it.
 * @param f AVFifoBuffer to write into
 * @return size
 * @deprecated use av_fifo_can_write() with the new AVFifo-API
 *)
function av_fifo_space(const f: PAVFifoBuffer): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_fifo_space';

(**
 * Feed data at specific position from an AVFifoBuffer to a user-supplied callback.
 * Similar as av_fifo_gereric_read but without discarding data.
 * @param f AVFifoBuffer to read from
 * @param offset offset from current read position
 * @param buf_size number of bytes to read
 * @param func generic read function
 * @param dest data destination
 *
 * @return a non-negative number on success, a negative error code on failure
 *
 * @deprecated use the new AVFifo-API with av_fifo_peek() when func == NULL,
 *             av_fifo_peek_to_cb() otherwise
 *)
type
  TfifopeekCall = procedure(a, b: Pointer; c: Integer); cdecl;
function av_fifo_generic_peek_at(f: PAVFifoBuffer; dest: Pointer; offset, buf_size: Integer; func: TfifopeekCall): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_fifo_generic_peek_at';

(**
 * Feed data from an AVFifoBuffer to a user-supplied callback.
 * Similar as av_fifo_gereric_read but without discarding data.
 * @param f AVFifoBuffer to read from
 * @param buf_size number of bytes to read
 * @param func generic read function
 * @param dest data destination
 *
 * @return a non-negative number on success, a negative error code on failure
 *
 * @deprecated use the new AVFifo-API with av_fifo_peek() when func == NULL,
 *             av_fifo_peek_to_cb() otherwise
 *)
function av_fifo_generic_peek(f: PAVFifoBuffer; dest: Pointer; buf_size: Integer; func: TfifoCall): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_fifo_generic_peek';

(**
 * Feed data from an AVFifoBuffer to a user-supplied callback.
 * @param f AVFifoBuffer to read from
 * @param buf_size number of bytes to read
 * @param func generic read function
 * @param dest data destination
 *
 * @return a non-negative number on success, a negative error code on failure
 *
 * @deprecated use the new AVFifo-API with av_fifo_read() when func == NULL,
 *             av_fifo_read_to_cb() otherwise
 *)
function av_fifo_generic_read(f: PAVFifoBuffer; dest: Pointer; buf_size: Integer; func: TfifoCall): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_fifo_generic_read';

(**
 * Feed data from a user-supplied callback to an AVFifoBuffer.
 * @param f AVFifoBuffer to write to
 * @param src data source; non-const since it may be used as a
 * modifiable context by the function defined in func
 * @param size number of bytes to write
 * @param func generic write function; the first parameter is src,
 * the second is dest_buf, the third is dest_buf_size.
 * func must return the number of bytes written to dest_buf, or <= 0 to
 * indicate no more data available to write.
 * If func is NULL, src is interpreted as a simple byte array for source data.
 * @return the number of bytes written to the FIFO or a negative error code on failure
 *
 * @deprecated use the new AVFifo-API with av_fifo_write() when func == NULL,
 *             av_fifo_write_from_cb() otherwise
 *)
function av_fifo_generic_write(f: PAVFifoBuffer; src: Pointer; size: Integer; func: TwriteCall): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_fifo_generic_write';

(**
 * Resize an AVFifoBuffer.
 * In case of reallocation failure, the old FIFO is kept unchanged.
 *
 * @param f AVFifoBuffer to resize
 * @param size new AVFifoBuffer size in bytes
 * @return <0 for failure, >=0 otherwise
 *
 * @deprecated use the new AVFifo-API with av_fifo_grow2() to increase FIFO size,
 *             decreasing FIFO size is not supported
 *)
function av_fifo_realloc2(f: PAVFifoBuffer; size: Cardinal): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_fifo_realloc2';

(**
 * Enlarge an AVFifoBuffer.
 * In case of reallocation failure, the old FIFO is kept unchanged.
 * The new fifo size may be larger than the requested size.
 *
 * @param f AVFifoBuffer to resize
 * @param additional_space the amount of space in bytes to allocate in addition to av_fifo_size()
 * @return <0 for failure, >=0 otherwise
 *
 * @deprecated use the new AVFifo-API with av_fifo_grow2(); note that unlike
 * this function it adds to the allocated size, rather than to the used size
 *)
function av_fifo_grow(f: PAVFifoBuffer; additional_space: Cardinal): Integer; cdecl; external AVUTIL_LIBNAME name _PU + 'av_fifo_grow';

(**
 * Read and discard the specified amount of data from an AVFifoBuffer.
 * @param f AVFifoBuffer to read from
 * @param size amount of data to read in bytes
 *
 * @deprecated use the new AVFifo-API with av_fifo_drain2()
 *)
procedure av_fifo_drain(f: PAVFifoBuffer; size: Integer); cdecl; external AVUTIL_LIBNAME name _PU + 'av_fifo_drain';

{$IFDEF FF_API_FIFO_PEEK2}
(**
 * Return a pointer to the data stored in a FIFO buffer at a certain offset.
 * The FIFO buffer is not modified.
 *
 * @param f    AVFifoBuffer to peek at, f must be non-NULL
 * @param offs an offset in bytes, its absolute value must be less
 *             than the used buffer size or the returned pointer will
 *             point outside to the buffer data.
 *             The used buffer size can be checked with av_fifo_size().
 * @deprecated use the new AVFifo-API with av_fifo_peek() or av_fifo_peek_to_cb()
 *)
function av_fifo_peek2(const f: PAVFifoBuffer; offs: Integer): PByte; {$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF}
{$ENDIF}

implementation

{$IFDEF FF_API_FIFO_OLD_API}
{$IFDEF FF_API_FIFO_PEEK2}
function av_fifo_peek2(const f: PAVFifoBuffer; offs: Integer): PByte;
var
  ptr: PByte;
begin
  ptr := f^.rptr;
  Inc(ptr, offs);
{$IFNDEF FPC}
  {$IF CompilerVersion <= 20.0} // Delphi 2009 and and olders
    {$DEFINE NO_NATIVE_UINT}
  {$IFEND}
{$ENDIF}
{$IFDEF NO_NATIVE_UINT}
  if Cardinal(ptr) >= Cardinal(f^.eend) then
    Dec(ptr, Cardinal(f^.eend) - Cardinal(f^.buffer))
  else if Cardinal(ptr) < Cardinal(f^.buffer) then
    Inc(ptr, Cardinal(f^.eend) - Cardinal(f^.buffer));
{$ELSE}
  if NativeUInt(ptr) >= NativeUInt(f^.eend) then
    Dec(ptr, NativeUInt(f^.eend) - NativeUInt(f^.buffer))
  else if NativeUInt(ptr) < NativeUInt(f^.buffer) then
    Inc(ptr, NativeUInt(f^.eend) - NativeUInt(f^.buffer));
{$ENDIF}
  Result := ptr;
end;
{$ENDIF}
{$ENDIF}

end.
