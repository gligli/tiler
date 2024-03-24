unit utils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, math, fgl, extern;

const
  // tweakable constants

  cSmoothingQWeighting = True;

  cYakmoMaxIterations = 300;
  cDCTFeaturesMul = 1;

  {$if false}
    cRedMul = 2126;
    cGreenMul = 7152;
    cBlueMul = 722;
  {$else}
    cRedMul = 299;
    cGreenMul = 587;
    cBlueMul = 114;
  {$endif}

  cRGBw = 13; // in 1 / 32th

  // don't change these

  cLumaDiv = cRedMul + cGreenMul + cBlueMul;

  cBitsPerComp = 8;
  cVecInvWidth = 16;
  cTileWidthBits = 3;
  cTileWidth = 1 shl cTileWidthBits;
  cColorCpns = 3;
  cTileDCTSize = cColorCpns * sqr(cTileWidth * cDCTFeaturesMul);
  cUnrolledDCTSize = sqr(sqr(cTileWidth * cDCTFeaturesMul));
  cPhi = (1 + sqrt(5)) / 2;
  cInvPhi = 1 / cPhi;

  cDitheringListLen = 256;
  cDitheringMap : array[0..8*8 - 1] of Byte = (
     0, 48, 12, 60,  3, 51, 15, 63,
    32, 16, 44, 28, 35, 19, 47, 31,
     8, 56,  4, 52, 11, 59,  7, 55,
    40, 24, 36, 20, 43, 27, 39, 23,
     2, 50, 14, 62,  1, 49, 13, 61,
    34, 18, 46, 30, 33, 17, 45, 29,
    10, 58,  6, 54,  9, 57,  5, 53,
    42, 26, 38, 22, 41, 25, 37, 21
  );
  cDitheringLen = length(cDitheringMap);

  cQ = 16;
  cDCTQuantization: array[0..cColorCpns-1{YUV}, 0..7, 0..7] of TFloat = (
    (
      // Luma
      (cQ / 16, cQ /  11, cQ /  10, cQ /  16, cQ /  24, cQ /  40, cQ /  51, cQ /  61),
      (cQ / 12, cQ /  12, cQ /  14, cQ /  19, cQ /  26, cQ /  58, cQ /  60, cQ /  55),
      (cQ / 14, cQ /  13, cQ /  16, cQ /  24, cQ /  40, cQ /  57, cQ /  69, cQ /  56),
      (cQ / 14, cQ /  17, cQ /  22, cQ /  29, cQ /  51, cQ /  87, cQ /  80, cQ /  62),
      (cQ / 18, cQ /  22, cQ /  37, cQ /  56, cQ /  68, cQ / 109, cQ / 103, cQ /  77),
      (cQ / 24, cQ /  35, cQ /  55, cQ /  64, cQ /  81, cQ / 104, cQ / 113, cQ /  92),
      (cQ / 49, cQ /  64, cQ /  78, cQ /  87, cQ / 103, cQ / 121, cQ / 120, cQ / 101),
      (cQ / 72, cQ /  92, cQ /  95, cQ /  98, cQ / 112, cQ / 100, cQ / 103, cQ /  99)
    ),
    (
      // U, weighted by luma importance
      (cQ / 17, cQ /  18, cQ /  24, cQ /  47, cQ /  99, cQ /  99, cQ /  99, cQ /  99),
      (cQ / 18, cQ /  21, cQ /  26, cQ /  66, cQ /  99, cQ /  99, cQ /  99, cQ / 112),
      (cQ / 24, cQ /  26, cQ /  56, cQ /  99, cQ /  99, cQ /  99, cQ / 112, cQ / 128),
      (cQ / 47, cQ /  66, cQ /  99, cQ /  99, cQ /  99, cQ / 112, cQ / 128, cQ / 144),
      (cQ / 99, cQ /  99, cQ /  99, cQ /  99, cQ / 112, cQ / 128, cQ / 144, cQ / 160),
      (cQ / 99, cQ /  99, cQ /  99, cQ / 112, cQ / 128, cQ / 144, cQ / 160, cQ / 176),
      (cQ / 99, cQ /  99, cQ / 112, cQ / 128, cQ / 144, cQ / 160, cQ / 176, cQ / 192),
      (cQ / 99, cQ / 112, cQ / 128, cQ / 144, cQ / 160, cQ / 176, cQ / 192, cQ / 208)
    ),
    (
      // V, weighted by luma importance
      (cQ / 17, cQ /  18, cQ /  24, cQ /  47, cQ /  99, cQ /  99, cQ /  99, cQ /  99),
      (cQ / 18, cQ /  21, cQ /  26, cQ /  66, cQ /  99, cQ /  99, cQ /  99, cQ / 112),
      (cQ / 24, cQ /  26, cQ /  56, cQ /  99, cQ /  99, cQ /  99, cQ / 112, cQ / 128),
      (cQ / 47, cQ /  66, cQ /  99, cQ /  99, cQ /  99, cQ / 112, cQ / 128, cQ / 144),
      (cQ / 99, cQ /  99, cQ /  99, cQ /  99, cQ / 112, cQ / 128, cQ / 144, cQ / 160),
      (cQ / 99, cQ /  99, cQ /  99, cQ / 112, cQ / 128, cQ / 144, cQ / 160, cQ / 176),
      (cQ / 99, cQ /  99, cQ / 112, cQ / 128, cQ / 144, cQ / 160, cQ / 176, cQ / 192),
      (cQ / 99, cQ / 112, cQ / 128, cQ / 144, cQ / 160, cQ / 176, cQ / 192, cQ / 208)
    )
  );


  cDCTUVRatio: array[0..7,0..7] of TFloat = (
    (0.5, sqrt(0.5), sqrt(0.5), sqrt(0.5), sqrt(0.5), sqrt(0.5), sqrt(0.5), sqrt(0.5)),
    (sqrt(0.5), 1, 1, 1, 1, 1, 1, 1),
    (sqrt(0.5), 1, 1, 1, 1, 1, 1, 1),
    (sqrt(0.5), 1, 1, 1, 1, 1, 1, 1),
    (sqrt(0.5), 1, 1, 1, 1, 1, 1, 1),
    (sqrt(0.5), 1, 1, 1, 1, 1, 1, 1),
    (sqrt(0.5), 1, 1, 1, 1, 1, 1, 1),
    (sqrt(0.5), 1, 1, 1, 1, 1, 1, 1)
  );

type
  TSpinlock = LongInt;
  PSpinLock = ^TSpinlock;

  { TCountIndex }

  TCountIndex = record
    Index, Count: Integer;
    R, G, B: Byte;
    Hue, Sat, Val: Byte;
  end;

  PCountIndex = ^TCountIndex;
  TCountIndexList = specialize TFPGList<PCountIndex>;

procedure SpinEnter(Lock: PSpinLock); assembler;
procedure SpinLeave(Lock: PSpinLock); assembler;
procedure Exchange(var a, b: Integer);
function iDiv0(x, y: Integer): Integer;overload;inline;
function iDiv0(x, y: Int64): Int64;overload;inline;
function Div0(x, y: TFloat): TFloat;inline;
procedure DivMod(Dividend: Int64; Divisor: LongInt; var Result, Remainder: LongInt);
function NanDef(x, def: TFloat): TFloat; inline;
function SwapRB(c: Integer): Integer; inline;
function ToRGB(r, g, b: Byte): Integer; inline;
procedure FromRGB(col: Integer; out r, g, b: Integer); inline; overload;
procedure FromRGB(col: Integer; out r, g, b: Byte); inline; overload;
function ToLuma(r, g, b: Byte): Integer; inline;
function ToBW(col: Integer): Integer;
function CompareIntegers(const Item1, Item2: Integer): Integer;
function lerp(x, y, alpha: TFloat): TFloat; inline;
function ilerp(x, y, alpha, maxAlpha: Integer): Integer; inline;
function revlerp(x, r, alpha: TFloat): TFloat; inline;
function Posterize(v, bpc: Byte): Byte; inline;
function CompareEuclideanDCTPtr(pa, pb: PFloat): TFloat; overload;
function CompareEuclideanDCTPtr_asm(pa_rcx, pb_rdx: PFloat): TFloat; register; assembler;
function CompareEuclideanDCT(const a, b: TFloatDynArray): TFloat; inline; overload;
function CompareEuclidean(const a, b: TFloatDynArray): TFloat; inline;
function CompareEuclidean(a, b: PDouble; size: Integer): Double; inline;
function CompareCMULHS(const Item1,Item2:PCountIndex):Integer;
function ComparePaletteUseCount(Item1,Item2,UserParameter:Pointer):Integer;
function ComputeBlendingError_Asm(PPlain_rcx, PPrev_rdx, POff_r8: PFloat; bp_xmm3, bo_stack: TFloat): TFloat; register; assembler;
function EqualQualityTileCount(tileCount: TFloat): Integer;


implementation

procedure SpinEnter(Lock: PSpinLock); assembler;
label spin_lock;
asm
spin_lock:
     mov     eax, 1          // Set the EAX register to 1.

     xchg    eax, [Lock]     // Atomically swap the EAX register with the lock variable.
                             // This will always store 1 to the lock, leaving the previous value in the EAX register.

     test    eax, eax        // Test EAX with itself. Among other things, this will set the processor's Zero Flag if EAX is 0.
                             // If EAX is 0, then the lock was unlocked and we just locked it.
                             // Otherwise, EAX is 1 and we didn't acquire the lock.

     jnz     spin_lock       // Jump back to the MOV instruction if the Zero Flag is not set;
                             // the lock was previously locked, and so we need to spin until it becomes unlocked.
end;

procedure SpinLeave(Lock: PSpinLock); assembler;
asm
    xor     eax, eax        // Set the EAX register to 0.

    xchg    eax, [Lock]     // Atomically swap the EAX register with the lock variable.
end;

procedure Exchange(var a, b: Integer);
var
  tmp: Integer;
begin
  tmp := b;
  b := a;
  a := tmp;
end;

function iDiv0(x, y: Integer): Integer;overload;inline;
begin
  Result := 0;
  if y <> 0 then
    Result := x div y;
end;

function iDiv0(x, y: Int64): Int64;overload;inline;
begin
  Result := 0;
  if y <> 0 then
    Result := x div y;
end;

function Div0(x, y: TFloat): TFloat;inline;
begin
  Result := 0;
  if y <> 0 then
    Result := x / y;
end;

procedure DivMod(Dividend: Int64; Divisor: LongInt; var Result, Remainder: LongInt);
begin
  Result := Dividend div Divisor;
  Remainder := Dividend mod Divisor;
end;

function NanDef(x, def: TFloat): TFloat; inline;
begin
  Result := x;
  if IsNan(Result) then
    Result := def;
end;

function SwapRB(c: Integer): Integer; inline;
begin
  Result := ((c and $ff) shl 16) or ((c shr 16) and $ff) or (c and $ff00);
end;

function ToRGB(r, g, b: Byte): Integer; inline;
begin
  Result := (b shl 16) or (g shl 8) or r;
end;

procedure FromRGB(col: Integer; out r, g, b: Integer); inline; overload;
begin
  r := col and $ff;
  g := (col shr 8) and $ff;
  b := (col shr 16) and $ff;
end;

procedure FromRGB(col: Integer; out r, g, b: Byte); inline; overload;
begin
  r := col and $ff;
  g := (col shr 8) and $ff;
  b := (col shr 16) and $ff;
end;

function ToLuma(r, g, b: Byte): Integer; inline;
begin
  Result := r * cRedMul + g * cGreenMul + b * cBlueMul;
end;

function ToBW(col: Integer): Integer;
var
  r, g, b: Byte;
begin
  FromRGB(col, r, g, b);
  Result := ToLuma(r, g, b);
  Result := Result div cLumaDiv;
  Result := ToRGB(Result, Result, Result);
end;

function CompareIntegers(const Item1, Item2: Integer): Integer;
begin
  Result := CompareValue(Item1, Item2);
end;

function lerp(x, y, alpha: TFloat): TFloat; inline;
begin
  Result := x + (y - x) * alpha;
end;

function ilerp(x, y, alpha, maxAlpha: Integer): Integer; inline;
begin
  Result := x + ((y - x) * alpha) div maxAlpha;
end;

function revlerp(x, r, alpha: TFloat): TFloat; inline;
begin
  Result := x + (r - x) / alpha;
end;

function Posterize(v, bpc: Byte): Byte; inline;
var
  cvt: Integer;
begin
  cvt := (1 shl bpc) - 1;

  Result := min(255, Round(Round((v * cvt) / 255.0) * 255.0 / cvt));
end;

function CompareEuclideanDCTPtr(pa, pb: PFloat): TFloat; overload;
var
  i: Integer;
begin
  Result := 0;
  for i := cTileDCTSize div 8 - 1 downto 0 do
  begin
    Result += sqr(pa^ - pb^); Inc(pa); Inc(pb);
    Result += sqr(pa^ - pb^); Inc(pa); Inc(pb);
    Result += sqr(pa^ - pb^); Inc(pa); Inc(pb);
    Result += sqr(pa^ - pb^); Inc(pa); Inc(pb);
    Result += sqr(pa^ - pb^); Inc(pa); Inc(pb);
    Result += sqr(pa^ - pb^); Inc(pa); Inc(pb);
    Result += sqr(pa^ - pb^); Inc(pa); Inc(pb);
    Result += sqr(pa^ - pb^); Inc(pa); Inc(pb);
  end;
end;

function CompareEuclideanDCTPtr_asm(pa_rcx, pb_rdx: PFloat): TFloat; register; assembler;
label loop;
asm
  push rax
  push rcx
  push rdx

  sub rsp, 16 * 12
  movdqu oword ptr [rsp],       xmm1
  movdqu oword ptr [rsp + $10], xmm2
  movdqu oword ptr [rsp + $20], xmm3
  movdqu oword ptr [rsp + $30], xmm4
  movdqu oword ptr [rsp + $40], xmm5
  movdqu oword ptr [rsp + $50], xmm6
  movdqu oword ptr [rsp + $60], xmm7
  movdqu oword ptr [rsp + $70], xmm8
  movdqu oword ptr [rsp + $80], xmm9
  movdqu oword ptr [rsp + $90], xmm10
  movdqu oword ptr [rsp + $a0], xmm11
  movdqu oword ptr [rsp + $b0], xmm12

  // unrolled for 48 = (cTileDCTSize / 4)

  pxor xmm0, xmm0
  mov al, (cTileDCTSize / 48)
  loop:

    // step 1

    movups xmm2,  oword ptr [rcx]
    movups xmm4,  oword ptr [rcx + $10]
    movups xmm6,  oword ptr [rcx + $20]
    movups xmm8,  oword ptr [rcx + $30]
    movups xmm10, oword ptr [rcx + $40]
    movups xmm12, oword ptr [rcx + $50]

    movups xmm1,  oword ptr [rdx]
    movups xmm3,  oword ptr [rdx + $10]
    movups xmm5,  oword ptr [rdx + $20]
    movups xmm7,  oword ptr [rdx + $30]
    movups xmm9,  oword ptr [rdx + $40]
    movups xmm11, oword ptr [rdx + $50]

    subps xmm1,  xmm2
    subps xmm3,  xmm4
    subps xmm5,  xmm6
    subps xmm7,  xmm8
    subps xmm9,  xmm10
    subps xmm11, xmm12

    mulps xmm1,  xmm1
    mulps xmm3,  xmm3
    mulps xmm5,  xmm5
    mulps xmm7,  xmm7
    mulps xmm9,  xmm9
    mulps xmm11, xmm11

    addps xmm1, xmm7
    addps xmm3, xmm9
    addps xmm5, xmm11

    addps xmm1, xmm3
    addps xmm0, xmm5
    addps xmm0, xmm1

    // step 2

    movups xmm2,  oword ptr [rcx + $60]
    movups xmm4,  oword ptr [rcx + $70]
    movups xmm6,  oword ptr [rcx + $80]
    movups xmm8,  oword ptr [rcx + $90]
    movups xmm10, oword ptr [rcx + $a0]
    movups xmm12, oword ptr [rcx + $b0]

    movups xmm1,  oword ptr [rdx + $60]
    movups xmm3,  oword ptr [rdx + $70]
    movups xmm5,  oword ptr [rdx + $80]
    movups xmm7,  oword ptr [rdx + $90]
    movups xmm9,  oword ptr [rdx + $a0]
    movups xmm11, oword ptr [rdx + $b0]

    subps xmm1,  xmm2
    subps xmm3,  xmm4
    subps xmm5,  xmm6
    subps xmm7,  xmm8
    subps xmm9,  xmm10
    subps xmm11, xmm12

    mulps xmm1,  xmm1
    mulps xmm3,  xmm3
    mulps xmm5,  xmm5
    mulps xmm7,  xmm7
    mulps xmm9,  xmm9
    mulps xmm11, xmm11

    addps xmm1, xmm7
    addps xmm3, xmm9
    addps xmm5, xmm11

    addps xmm1, xmm3
    addps xmm0, xmm5
    addps xmm0, xmm1

    // loop

    lea rcx, [rcx + $c0]
    lea rdx, [rdx + $c0]
    lea r8, [r8 + $c0]

    dec al
    jnz loop

  // end

  movdqu xmm1,  oword ptr [rsp]
  movdqu xmm2,  oword ptr [rsp + $10]
  movdqu xmm3,  oword ptr [rsp + $20]
  movdqu xmm4,  oword ptr [rsp + $30]
  movdqu xmm5,  oword ptr [rsp + $40]
  movdqu xmm6,  oword ptr [rsp + $50]
  movdqu xmm7,  oword ptr [rsp + $60]
  movdqu xmm8,  oword ptr [rsp + $70]
  movdqu xmm9,  oword ptr [rsp + $80]
  movdqu xmm10, oword ptr [rsp + $90]
  movdqu xmm11, oword ptr [rsp + $a0]
  movdqu xmm12, oword ptr [rsp + $b0]
  add rsp, 16 * 12

  haddps xmm0, xmm0
  haddps xmm0, xmm0

  pop rdx
  pop rcx
  pop rax
end;

function CompareEuclideanDCT(const a, b: TFloatDynArray): TFloat; inline; overload;
begin
  Result := CompareEuclideanDCTPtr_asm(@a[0], @b[0]);
end;

function CompareEuclidean(const a, b: TFloatDynArray): TFloat; inline;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to High(a) do
    Result += sqr(a[i] - b[i]);
end;

function CompareEuclidean(a, b: PDouble; size: Integer): Double; inline;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to size - 1 do
    Result += sqr(a[i] - b[i]);
end;

function CompareCMULHS(const Item1,Item2:PCountIndex):Integer;
begin
  Result := CompareValue(Item1^.Val, Item2^.Val);
  if Result = 0 then
    Result := CompareValue(Item1^.Sat, Item2^.Sat);
  if Result = 0 then
    Result := CompareValue(Item1^.Hue, Item2^.Hue);
end;

function ComparePaletteUseCount(Item1,Item2,UserParameter:Pointer):Integer;
begin
  Result := CompareValue(PInteger(Item2)^, PInteger(Item1)^);
end;

function ComputeBlendingError(PPlain, PPrev, POff: PFloat; bp, bo: TFloat): TFloat; inline;
var
  i: Integer;
begin
  Result := 0.0;
  for i := 0 to cTileDCTSize div 8 - 1 do // unroll by 8
  begin
    Result += Sqr(PPlain^ - (PPrev^ * bp + POff^ * bo)); Inc(PPlain); Inc(PPrev); Inc(POff);
    Result += Sqr(PPlain^ - (PPrev^ * bp + POff^ * bo)); Inc(PPlain); Inc(PPrev); Inc(POff);
    Result += Sqr(PPlain^ - (PPrev^ * bp + POff^ * bo)); Inc(PPlain); Inc(PPrev); Inc(POff);
    Result += Sqr(PPlain^ - (PPrev^ * bp + POff^ * bo)); Inc(PPlain); Inc(PPrev); Inc(POff);
    Result += Sqr(PPlain^ - (PPrev^ * bp + POff^ * bo)); Inc(PPlain); Inc(PPrev); Inc(POff);
    Result += Sqr(PPlain^ - (PPrev^ * bp + POff^ * bo)); Inc(PPlain); Inc(PPrev); Inc(POff);
    Result += Sqr(PPlain^ - (PPrev^ * bp + POff^ * bo)); Inc(PPlain); Inc(PPrev); Inc(POff);
    Result += Sqr(PPlain^ - (PPrev^ * bp + POff^ * bo)); Inc(PPlain); Inc(PPrev); Inc(POff);
  end;
end;


function ComputeBlendingError_Asm(PPlain_rcx, PPrev_rdx, POff_r8: PFloat; bp_xmm3, bo_stack: TFloat): TFloat; register; assembler;
const
  cDCTSizeOffset = cTileDCTSize * SizeOf(TFloat);
label loop;
asm
  push rcx
  push rdx
  push r8

  sub rsp, 16 * 14
  movdqu oword ptr [rsp],       xmm1
  movdqu oword ptr [rsp + $10], xmm2
  movdqu oword ptr [rsp + $20], xmm3
  movdqu oword ptr [rsp + $30], xmm4
  movdqu oword ptr [rsp + $40], xmm5
  movdqu oword ptr [rsp + $50], xmm6
  movdqu oword ptr [rsp + $60], xmm7
  movdqu oword ptr [rsp + $70], xmm8
  movdqu oword ptr [rsp + $80], xmm9
  movdqu oword ptr [rsp + $90], xmm10
  movdqu oword ptr [rsp + $a0], xmm11
  movdqu oword ptr [rsp + $b0], xmm12
  movdqu oword ptr [rsp + $c0], xmm13
  movdqu oword ptr [rsp + $d0], xmm14

  pshufd xmm1, xmm3, 0
  pshufd xmm2, dword ptr [bo_stack], 0
  xorps xmm0, xmm0

  lea rax, byte ptr [rcx + cDCTSizeOffset]
  loop:
    movups xmm3,  oword ptr [rcx]
    movups xmm6,  oword ptr [rcx + $10]
    movups xmm9,  oword ptr [rcx + $20]
    movups xmm12, oword ptr [rcx + $30]

    movups xmm4,  oword ptr [rdx]
    movups xmm7,  oword ptr [rdx + $10]
    movups xmm10, oword ptr [rdx + $20]
    movups xmm13, oword ptr [rdx + $30]

    movups xmm5,  oword ptr [r8]
    movups xmm8,  oword ptr [r8 + $10]
    movups xmm11, oword ptr [r8 + $20]
    movups xmm14, oword ptr [r8 + $30]

    mulps xmm4, xmm1
    mulps xmm5, xmm2
    addps xmm4, xmm5
    subps xmm3, xmm4
    mulps xmm3, xmm3
    addps xmm0, xmm3

    mulps xmm7, xmm1
    mulps xmm8, xmm2
    addps xmm7, xmm8
    subps xmm6, xmm7
    mulps xmm6, xmm6
    addps xmm0, xmm6

    mulps xmm10, xmm1
    mulps xmm11, xmm2
    addps xmm10, xmm11
    subps xmm9, xmm10
    mulps xmm9, xmm9
    addps xmm0, xmm9

    mulps xmm13, xmm1
    mulps xmm14, xmm2
    addps xmm13, xmm14
    subps xmm12, xmm13
    mulps xmm12, xmm12
    addps xmm0, xmm12

    lea rcx, [rcx + $40]
    lea rdx, [rdx + $40]
    lea r8, [r8 + $40]

    cmp rcx, rax
    jne loop

  haddps xmm0, xmm0
  haddps xmm0, xmm0

  movdqu xmm1,  oword ptr [rsp]
  movdqu xmm2,  oword ptr [rsp + $10]
  movdqu xmm3,  oword ptr [rsp + $20]
  movdqu xmm4,  oword ptr [rsp + $30]
  movdqu xmm5,  oword ptr [rsp + $40]
  movdqu xmm6,  oword ptr [rsp + $50]
  movdqu xmm7,  oword ptr [rsp + $60]
  movdqu xmm8,  oword ptr [rsp + $70]
  movdqu xmm9,  oword ptr [rsp + $80]
  movdqu xmm10, oword ptr [rsp + $90]
  movdqu xmm11, oword ptr [rsp + $a0]
  movdqu xmm12, oword ptr [rsp + $b0]
  movdqu xmm13, oword ptr [rsp + $c0]
  movdqu xmm14, oword ptr [rsp + $d0]
  add rsp, 16 * 14

  pop r8
  pop rdx
  pop rcx
end;

function EqualQualityTileCount(tileCount: TFloat): Integer;
begin
  Result := round(sqrt(tileCount) * log2(1 + tileCount));
end;


end.

