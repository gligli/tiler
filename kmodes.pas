// Inspired by https://github.com/nicodv/kmodes

unit kmodes;

//{$define HAS_NO_POPCNT}
//{$define GENERIC_DISSIM}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, Math, LazLogger, MTProcs, windows;

type
  PInteger = ^Integer;
  TCompareFunction=function(Item1,Item2,UserParameter:Pointer):Integer;
  TIntegerDynArray2 = array of TIntegerDynArray;
  TIntegerDynArray3 = array of TIntegerDynArray2;
  TByteDynArray2 = array of TByteDynArray;
  TUInt64DynArray = array of UInt64;
  TSpinlock = LongInt;

  TKmodesRun = packed record
    Labels: TIntegerDynArray;
    Centroids: TByteDynArray2;
    Cost: Integer;
    NIter: Integer;
    TotalMoves: Integer;
    StartingPoint: Integer;
  end;

  { TKModes }

  TKModes = class
  private
    membship, labels: TIntegerDynArray;
    X: TByteDynArray2;
    centroids: TByteDynArray2;
    cl_attr_freq: TIntegerDynArray3;
    MaxIter, NumClusters, NumThreads, NumAttrs, NumPoint: Integer;
    Log: Boolean;

    procedure DoCost(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
    procedure DoMembship(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);

    function CountClusterMembers(cluster: Integer): Integer;
    function GetMaxClusterMembers(out member_count: Integer): Integer;
    function LabelsCost: Integer;
    function KModesIter(var Seed: Cardinal): Integer;
    function InitFarthestFirst(init_point: Integer): TByteDynArray2;  // negative init_point means randomly chosen
    procedure MovePointCat(const point: TByteDynArray; ipoint, to_clust, from_clust: Integer);
  public
    constructor Create(aNumThreads: Integer = 0; aMaxIter: Integer = 0; aLog: Boolean = False);
    function ComputeKModes(const ADataset: TByteDynArray2; ANumClusters, ANumInit, ANumModalities: Integer; var FinalLabels: TIntegerDynArray; out FinalCentroids: TByteDynArray2): Integer;
  end;


function RandInt(Range: Cardinal; var Seed: Cardinal): Cardinal;
procedure QuickSort(var AData;AFirstItem,ALastItem,AItemSize:Integer;ACompareFunction:TCompareFunction;AUserParameter:Pointer=nil);
function GetMinMatchingDissim_Asm(item_rcx: PByte; list_rdx: PPByte; count_r8: UInt64): UInt64;
function GetMinMatchingDissim(const a: TByteDynArray2; const b: TByteDynArray; out bestDissim: Integer): Integer;


// negative n_init means use -n_init as starting point
// FinalLabels cluster indexes start at 1

implementation

{$PUSH}
{$RANGECHECKS OFF}
function RandInt(Range: Cardinal; var Seed: Cardinal): Cardinal;
begin
  Seed := Integer(Seed * $08088405) + 1;
  Result := (Seed * Range) shr 32;
end;
{$POP}

procedure QuickSort(var AData;AFirstItem,ALastItem,AItemSize:Integer;ACompareFunction:TCompareFunction;AUserParameter:Pointer=nil);
var I, J, P: Integer;
    PData,P1,P2: PByte;
    Tmp: array[0..4095] of Byte;
begin
  if ALastItem <= AFirstItem then
    Exit;

  Assert(AItemSize < SizeOf(Tmp),'AItemSize too big!');
  PData:=PByte(@AData);
  repeat
    I := AFirstItem;
    J := ALastItem;
    P := (AFirstItem + ALastItem) shr 1;
    repeat
      P1:=PData;Inc(P1,I*AItemSize);
      P2:=PData;Inc(P2,P*AItemSize);
      while ACompareFunction(P1, P2, AUserParameter) < 0 do
      begin
        Inc(I);
        Inc(P1,AItemSize);
      end;
      P1:=PData;Inc(P1,J*AItemSize);
      //P2:=PData;Inc(P2,P*AItemSize); already done
      while ACompareFunction(P1, P2, AUserParameter) > 0 do
      begin
        Dec(J);
        Dec(P1,AItemSize);
      end;
      if I <= J then
      begin
        P1:=PData;Inc(P1,I*AItemSize);
        P2:=PData;Inc(P2,J*AItemSize);
        Move(P2^, Tmp[0], AItemSize);
        Move(P1^, P2^, AItemSize);
        Move(Tmp[0], P1^, AItemSize);

        if P = I then
          P := J
        else if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if AFirstItem < J then QuickSort(AData,AFirstItem,J,AItemSize,ACompareFunction,AUserParameter);
    AFirstItem := I;
  until I >= ALastItem;
end;

function CompareLines(Item1,Item2,UserParameter:Pointer): Integer;
begin
  Result := CompareByte(PByteArray(Item1)^[0], PByteArray(Item2)^[0], SizeInt(UserParameter));
end;

function CompareDis(Item1,Item2,UserParameter:Pointer): Integer;
begin
  Result := CompareValue(PByte(UserParameter)[PInteger(Item1)^], PByte(UserParameter)[PInteger(Item2)^]);
end;

function GetLocPoints(AIndex, n_chunks, NumPoint: Integer; out n_loc_points: Integer): Integer;
var
  nlp: Integer;
begin
  nlp := NumPoint div n_chunks;
  Result := AIndex * nlp;
  if AIndex = n_chunks - 1 then
    nlp := NumPoint - Result;

  n_loc_points := nlp;
end;

function GetMaxValueIndex(const arr: TIntegerDynArray): Integer; overload;
var
  best, ik: Integer;
begin
  Result := -1;
  best := Low(Integer);
  for ik := 0 to High(arr) do
    if arr[ik] > best then
    begin
      best := arr[ik];
      Result := ik;
    end;
end;

function GetMinValueIndex(const arr: TIntegerDynArray): Integer; overload;
var
  best, ik: Integer;
begin
  Result := -1;
  best := High(Integer);
  for ik := 0 to High(arr) do
    if arr[ik] < best then
    begin
      best := arr[ik];
      Result := ik;
    end;
end;

function GetMinValueIndex(const arr: TByteDynArray): Integer; overload;
var
  ik: Integer;
  best: Byte;
begin
  Result := -1;
  best := High(Byte);
  for ik := 0 to High(arr) do
    if arr[ik] < best then
    begin
      best := arr[ik];
      Result := ik;
    end;
end;

function GetUniqueRows(const X: TByteDynArray2): TByteDynArray2;
var
  i, j, Cnt: Integer;
begin
  Result := Copy(X);

  if Length(Result) <= 1 then
    Exit;

  QuickSort(Result[0], 0, High(Result), SizeOf(TByteDynArray), @CompareLines, Pointer(Length(Result[0])));

  Cnt := Length(Result);
  for i := High(Result) - 1 downto 0 do
    if CompareByte(Result[i, 0], Result[i + 1, 0], Length(Result[0])) = 0 then
    begin
      for j := i + 1 to Cnt - 2 do
        Result[j] := Result[j + 1];
      Dec(Cnt);
    end;
  SetLength(Result, Cnt);
end;

function CountUniqueRows(const X: TByteDynArray2): Integer;
var
  i, Cnt: Integer;
  NumAttrs: Integer;
  unique: TByteDynArray2;
begin
  Result := 0;

  if Length(X) < 1 then
    Exit;

  NumAttrs := Length(X[0]);

  unique := Copy(X);
  QuickSort(unique[0], 0, High(unique), SizeOf(TByteDynArray), @CompareLines, Pointer(NumAttrs));

  Cnt := Length(unique);
  for i := High(unique) - 1 downto 0 do
    if CompareByte(unique[i, 0], unique[i + 1, 0], NumAttrs) = 0 then
      Dec(Cnt);
  Result := Cnt;
end;

{$if defined(GENERIC_DISSIM) or not defined(CPUX86_64)}
function MatchingDissim(const a: TByteDynArray; const b: TByteDynArray): UInt64; inline;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to High(a) do
    if a[i] <> b[i] then
      Inc(Result);
end;
{$else}

function MatchingDissim(const a: TByteDynArray; const b: TByteDynArray): UInt64; inline;
var
  i: Integer;
begin
  Result := 0;

  i := 0;
  Result += Ord(a[i+0] <> b[i+0]); Result += Ord(a[i+1] <> b[i+1]); Result += Ord(a[i+2] <> b[i+2]); Result += Ord(a[i+3] <> b[i+3]);
  Result += Ord(a[i+4] <> b[i+4]); Result += Ord(a[i+5] <> b[i+5]); Result += Ord(a[i+6] <> b[i+6]); Result += Ord(a[i+7] <> b[i+7]);
  i := 8;
  Result += Ord(a[i+0] <> b[i+0]); Result += Ord(a[i+1] <> b[i+1]); Result += Ord(a[i+2] <> b[i+2]); Result += Ord(a[i+3] <> b[i+3]);
  Result += Ord(a[i+4] <> b[i+4]); Result += Ord(a[i+5] <> b[i+5]); Result += Ord(a[i+6] <> b[i+6]); Result += Ord(a[i+7] <> b[i+7]);
  i := 16;
  Result += Ord(a[i+0] <> b[i+0]); Result += Ord(a[i+1] <> b[i+1]); Result += Ord(a[i+2] <> b[i+2]); Result += Ord(a[i+3] <> b[i+3]);
  Result += Ord(a[i+4] <> b[i+4]); Result += Ord(a[i+5] <> b[i+5]); Result += Ord(a[i+6] <> b[i+6]); Result += Ord(a[i+7] <> b[i+7]);
  i := 24;
  Result += Ord(a[i+0] <> b[i+0]); Result += Ord(a[i+1] <> b[i+1]); Result += Ord(a[i+2] <> b[i+2]); Result += Ord(a[i+3] <> b[i+3]);
  Result += Ord(a[i+4] <> b[i+4]); Result += Ord(a[i+5] <> b[i+5]); Result += Ord(a[i+6] <> b[i+6]); Result += Ord(a[i+7] <> b[i+7]);
  i := 32;
  Result += Ord(a[i+0] <> b[i+0]); Result += Ord(a[i+1] <> b[i+1]); Result += Ord(a[i+2] <> b[i+2]); Result += Ord(a[i+3] <> b[i+3]);
  Result += Ord(a[i+4] <> b[i+4]); Result += Ord(a[i+5] <> b[i+5]); Result += Ord(a[i+6] <> b[i+6]); Result += Ord(a[i+7] <> b[i+7]);
  i := 40;
  Result += Ord(a[i+0] <> b[i+0]); Result += Ord(a[i+1] <> b[i+1]); Result += Ord(a[i+2] <> b[i+2]); Result += Ord(a[i+3] <> b[i+3]);
  Result += Ord(a[i+4] <> b[i+4]); Result += Ord(a[i+5] <> b[i+5]); Result += Ord(a[i+6] <> b[i+6]); Result += Ord(a[i+7] <> b[i+7]);
  i := 48;
  Result += Ord(a[i+0] <> b[i+0]); Result += Ord(a[i+1] <> b[i+1]); Result += Ord(a[i+2] <> b[i+2]); Result += Ord(a[i+3] <> b[i+3]);
  Result += Ord(a[i+4] <> b[i+4]); Result += Ord(a[i+5] <> b[i+5]); Result += Ord(a[i+6] <> b[i+6]); Result += Ord(a[i+7] <> b[i+7]);
  i := 56;
  Result += Ord(a[i+0] <> b[i+0]); Result += Ord(a[i+1] <> b[i+1]); Result += Ord(a[i+2] <> b[i+2]); Result += Ord(a[i+3] <> b[i+3]);
  Result += Ord(a[i+4] <> b[i+4]); Result += Ord(a[i+5] <> b[i+5]); Result += Ord(a[i+6] <> b[i+6]); Result += Ord(a[i+7] <> b[i+7]);

end;

function CountPopulation(const x:UInt64):UInt64;
const
  m1:UInt64 = $5555555555555555; //binary: 0101...
  m2:UInt64 = $3333333333333333; //binary: 00110011..
  m4:UInt64 = $0f0f0f0f0f0f0f0f; //binary:  4 zeros,  4 ones ...
  h1:UInt64 = $0101010101010101; //the sum of 256 to the power of 0,1,2,3...
begin
  Result := x;

  // Wikipedia popcount_3 port ( http://en.wikipedia.org/wiki/Popcnt )

  Result := Result - ((Result shr 1) and m1);          //put count of each 2 bits into those 2 bits
  Result := (Result and m2) + ((Result shr 2) and m2); //put count of each 4 bits into those 4 bits
  Result := (Result + (Result shr 4)) and m4;          //put count of each 8 bits into those 8 bits
  Result := (Result * h1) shr 56; //returns left 8 bits of Result + (Result<<8) + (Result<<16) + (Result<<24) + ...
end;

const
  cMatchingIdxWidth = 48;

function GetMinMatchingDissim_Asm(item_rcx: PByte; list_rdx: PPByte; count_r8: UInt64): UInt64; register; assembler; nostackframe;
label loop, worst;
asm
  // SIMD for 64 items

  push rbx
  push rcx
  push rsi
  push rdi
  push r8
  push rdx

  sub rsp, 16 * 8
  movdqu oword ptr [rsp],       xmm0
  movdqu oword ptr [rsp + $10], xmm1
  movdqu oword ptr [rsp + $20], xmm2
  movdqu oword ptr [rsp + $30], xmm3
  movdqu oword ptr [rsp + $40], xmm4
  movdqu oword ptr [rsp + $50], xmm5
  movdqu oword ptr [rsp + $60], xmm6
  movdqu oword ptr [rsp + $70], xmm7

  movdqu xmm4, oword ptr [item_rcx]
  movdqu xmm5, oword ptr [item_rcx + $10]
  movdqu xmm6, oword ptr [item_rcx + $20]
  movdqu xmm7, oword ptr [item_rcx + $30]

  lea rbx, [list_rdx + 8 * count_r8 - 8]

  lea r8, [list_rdx - 8]

  xor rax, rax
  dec rax

  loop:
    mov rcx, [list_rdx]

    movdqu xmm0, oword ptr [rcx]
    movdqu xmm1, oword ptr [rcx + $10]
    movdqu xmm2, oword ptr [rcx + $20]
    movdqu xmm3, oword ptr [rcx + $30]

    pcmpeqb xmm0, xmm4
    pcmpeqb xmm1, xmm5
    pcmpeqb xmm2, xmm6
    pcmpeqb xmm3, xmm7

    pmovmskb edi, xmm0
    mov rsi, rdi
    pmovmskb edi, xmm1
    shl rsi, 16
    or rsi, rdi
    pmovmskb edi, xmm2
    shl rsi, 16
    or rsi, rdi
    pmovmskb edi, xmm3
    shl rsi, 16
    or rsi, rdi

    not rsi

{$ifdef HAS_NO_POPCNT}
    mov rcx, rsi
    call CountPopulation
{$else}
    popcnt rsi, rsi
{$endif}

    cmp rsi, rax
    ja worst

      mov rax, rsi
      mov r8, list_rdx

    worst:

    add rcx, 64
    add list_rdx, 8
    cmp list_rdx, rbx
    jle loop

  movdqu xmm0, oword ptr [rsp]
  movdqu xmm1, oword ptr [rsp + $10]
  movdqu xmm2, oword ptr [rsp + $20]
  movdqu xmm3, oword ptr [rsp + $30]
  movdqu xmm4, oword ptr [rsp + $40]
  movdqu xmm5, oword ptr [rsp + $50]
  movdqu xmm6, oword ptr [rsp + $60]
  movdqu xmm7, oword ptr [rsp + $70]
  add rsp, 16 * 8

  pop rdx

  shl rax, cMatchingIdxWidth
  sub r8, list_rdx
  sar r8, 3
  add rax, r8

  pop r8
  pop rdi
  pop rsi
  pop rcx
  pop rbx

end;

{$endif}

function GetMinMatchingDissim(const a: TByteDynArray2; const b: TByteDynArray; out bestDissim: Integer): Integer;
var
  db: UInt64;
begin
  db := GetMinMatchingDissim_Asm(@b[0], @a[0], Length(a));
  Result := Integer(db and ((1 shl cMatchingIdxWidth) - 1));
  bestDissim := Integer(db shr cMatchingIdxWidth);
end;


function TKModes.CountClusterMembers(cluster: Integer): Integer;
var
  i: Integer;
  pm: PInteger;
begin
  Result := 0;
  pm := @membship[0];

  for i := 0 to High(membship) do
  begin
    Inc(Result, Ord(pm^ = cluster));
    Inc(pm);
  end;
end;

function TKModes.GetMaxClusterMembers(out member_count: Integer): Integer;
var
  quantum, mc_loc, mc, cluster, i: Integer;
  pm0, pm1, pm2, pm3: PInteger;
begin
  Result := 0;
  mc := 0;
  quantum := Length(membship) shr 2;

  for cluster := 0 to NumClusters - 1 do
  begin
    mc_loc := 0;
    pm0 := @membship[quantum * 0];
    pm1 := @membship[quantum * 1];
    pm2 := @membship[quantum * 2];
    pm3 := @membship[quantum * 3];

    for i := 0 to quantum - 1 do
    begin
      Inc(mc_loc, Ord(pm0^ = cluster)); Inc(pm0);
      Inc(mc_loc, Ord(pm1^ = cluster)); Inc(pm1);
      Inc(mc_loc, Ord(pm2^ = cluster)); Inc(pm2);
      Inc(mc_loc, Ord(pm3^ = cluster)); Inc(pm3);
    end;

    for i := (quantum shl 2) to Length(membship) - 1 do
    begin
      Inc(mc_loc, Ord(pm3^ = cluster)); Inc(pm3);
    end;

    if mc_loc >= mc then
    begin
      mc := mc_loc;
      Result := cluster;
    end;
  end;

  member_count := mc;
end;


function TKModes.InitFarthestFirst(init_point: Integer): TByteDynArray2;  // negative init_point means randomly chosen
var
  icentroid, ifarthest: Integer;
  used: TBooleanDynArray;
  mindistance: TByteDynArray;

  procedure UpdateMinDistance(icenter: Integer);
  var
    i: Integer;
    dis: Byte;
  begin
    for i := 0 to NumPoint - 1 do
      if not used[i] then
      begin
        dis := MatchingDissim(X[icenter], X[i]);
        if dis < mindistance[i] then
          mindistance[i] := dis;
      end;
  end;

  function FarthestAway: Integer;
  var
    i: Integer;
    max: Byte;
  begin
    max := 0;
    Result := -1;
    for i := 0 to NumPoint - 1 do
      if not used[i] and (max <= mindistance[i]) then
      begin
        max := mindistance[i];
        Result := i;
      end;
  end;

begin
  SetLength(Result, NumClusters, NumAttrs);
  SetLength(used, NumPoint);
  SetLength(mindistance, NumPoint);

  for icentroid := 0 to NumClusters - 1 do
    FillByte(Result[icentroid, 0], NumAttrs, High(Byte));
  FillChar(used[0], NumPoint, False);
  FillByte(mindistance[0], NumPoint, High(Byte));

  ifarthest := init_point;
  Move(X[ifarthest, 0], Result[icentroid, 0], NumAttrs);
  used[ifarthest] := True;
  UpdateMinDistance(ifarthest);

  for icentroid := 0 to NumClusters - 1 do
  begin
    ifarthest := FarthestAway;

    Move(X[ifarthest, 0], Result[icentroid, 0], NumAttrs);
    used[ifarthest] := True;
    UpdateMinDistance(ifarthest);
  end;
end;

procedure TKModes.DoCost(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
var
  clust, dis, dis_acc: Integer;
  n_loc_points, ipoint, iloc: Integer;
begin
  ipoint := GetLocPoints(AIndex, NumThreads, NumPoint, n_loc_points);

  dis_acc := 0;

  for iloc := 0 to n_loc_points - 1 do
  begin
    clust := GetMinMatchingDissim(centroids, X[ipoint], dis);
    labels[ipoint] := clust;

    Inc(dis_acc, dis);
    Inc(ipoint);
  end;

  InterLockedExchangeAdd(PInteger(AData)^, dis_acc);
end;

function TKModes.LabelsCost: Integer;
begin
  Result := 0;
  SetLength(labels, NumPoint);
  FillDWord(labels[0], NumPoint, 0);

  ProcThreadPool.DoParallel(@DoCost, 0, NumThreads - 1, @Result, NumThreads);
end;

procedure TKModes.MovePointCat(const point: TByteDynArray; ipoint, to_clust, from_clust: Integer);
var
  iattr, curattr, current_attribute_value_freq, current_centroid_value, current_centroid_freq, old_centroid_value: Integer;
  to_attr_counts, from_attr_counts: TIntegerDynArray;
begin
  membship[ipoint] := to_clust;

  for iattr := 0 to High(point) do
  begin
    curattr := point[iattr];

    to_attr_counts := cl_attr_freq[to_clust, iattr];
    from_attr_counts := cl_attr_freq[from_clust, iattr];

    Inc(to_attr_counts[curattr]);

    current_attribute_value_freq := to_attr_counts[curattr];
    current_centroid_value := centroids[to_clust, iattr];
    current_centroid_freq := to_attr_counts[current_centroid_value];
    if current_centroid_freq < current_attribute_value_freq then
      centroids[to_clust, iattr] := curattr;

    Dec(from_attr_counts[curattr]);

    old_centroid_value := centroids[from_clust, iattr];
    if old_centroid_value = curattr then
      centroids[from_clust, iattr] := GetMaxValueIndex(from_attr_counts);
  end;
end;

constructor TKModes.Create(aNumThreads: Integer; aMaxIter: Integer; aLog: Boolean);
begin
  inherited Create;

  Self.MaxIter := aMaxIter;
  Self.NumThreads := aNumThreads;
  Self.Log := aLog;
end;

function TKModes.KModesIter(var Seed: Cardinal): Integer;
var
  ipoint, clust, old_clust, from_clust, rindx, cnt, dis, n_clusters: Integer;
  choices: TIntegerDynArray;
begin
  n_clusters := Length(centroids);
  Result := 0;

  SetLength(choices, NumPoint);

  for ipoint := 0 to NumPoint - 1 do
  begin
    clust := GetMinMatchingDissim(centroids, X[ipoint], dis);

    if membship[ipoint] <> clust then
    begin
      Inc(Result);
      old_clust := membship[ipoint];

      MovePointCat(X[ipoint], ipoint, clust, old_clust);

      if CountClusterMembers(old_clust) = 0 then
      begin
        from_clust := GetMaxClusterMembers(n_clusters);
        cnt := CountClusterMembers(from_clust);
        rindx := choices[RandInt(cnt, Seed)];

        MovePointCat(X[rindx], rindx, old_clust, from_clust);
      end;
    end;
  end;
end;

procedure TKModes.DoMembship(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
var
  iattr, clust, n_loc_points, ipoint, iloc, dummy: Integer;
begin
  ipoint := GetLocPoints(AIndex, NumThreads, NumPoint, n_loc_points);

  //DebugLn([ipoint,#9,n_loc_points, #9,n_threads]);

  for iloc := 0 to n_loc_points - 1 do
  begin
    clust := GetMinMatchingDissim(centroids, X[ipoint], dummy);

    membship[ipoint] := clust;

    for iattr := 0 to NumAttrs - 1 do
      InterLockedIncrement(cl_attr_freq[clust, iattr, X[ipoint, iattr]]);

    Inc(ipoint);
  end;
end;

function TKModes.ComputeKModes(const ADataset: TByteDynArray2; ANumClusters, ANumInit, ANumModalities: Integer;
  var FinalLabels: TIntegerDynArray; out FinalCentroids: TByteDynArray2): Integer;
var
  init: TByteDynArray2;
  all: array of TKmodesRun;
var
  best, i, j, init_no, iattr, ik, summemb, itr, moves, totalmoves: Integer;
  converged: Boolean;
  cost, ncost: Integer;
  InvGoldenRatio, GRAcc: Single;
  Seed: Cardinal;
begin
  Seed := $42381337;

  X := ADataset;

  NumPoint := Length(X);
  NumClusters := ANumClusters;

  NumAttrs := 0;
  if NumPoint > 0 then
    NumAttrs := Length(X[0]);

  if NumThreads <= 0 then
    NumThreads := ProcThreadPool.MaxThreadCount;

  if MaxIter <= 0 then
    MaxIter := MaxInt;

  init := nil;
  if CountUniqueRows(X) <= NumClusters then
  begin
    MaxIter := 0;
    ANumInit := 1;
    init := GetUniqueRows(X);
    NumClusters := Length(init);
  end;

  if ANumInit <= 0 then
  begin
    SetLength(all, 1);
    all[0].StartingPoint := -ANumInit;
  end
  else
  begin
    SetLength(all, ANumInit);
    InvGoldenRatio := power(NumPoint, 1 / ANumInit);
    GRAcc := 1;
    for i := 0 to ANumInit - 1 do
    begin
      all[i].StartingPoint := Round(GRAcc) - 1;
      if (i > 0) and (all[i].StartingPoint <= all[i - 1].StartingPoint) then
        all[i].StartingPoint := Min(NumPoint - 1, all[i - 1].StartingPoint + 1);
      GRAcc := GRAcc * InvGoldenRatio;
    end;
  end;

  for init_no := 0 to High(all) do
  begin
    SetLength(membship, NumPoint);
    SetLength(cl_attr_freq, NumClusters, NumAttrs, ANumModalities);

    if init = nil then
      centroids := InitFarthestFirst(all[init_no].StartingPoint)
    else
      centroids := init;

    FillDWord(membship[0], NumPoint, $ffffffff);
    for j := 0 to NumClusters - 1 do
      for i := 0 to NumAttrs - 1 do
        FillDWord(cl_attr_freq[j, i, 0], ANumModalities, 0);

    ProcThreadPool.DoParallel(@DoMembship, 0, NumThreads - 1, nil, NumThreads);

    for ik := 0 to NumClusters - 1  do
    begin
      summemb := CountClusterMembers(ik);
      if summemb = 0 then
      begin
        for iattr := 0 to NumAttrs - 1 do
          centroids[ik, iattr] := X[RandInt(NumPoint, Seed), iattr]
      end
      else
      begin
         for iattr := 0 to NumAttrs - 1 do
           centroids[ik, iattr] := GetMaxValueIndex(cl_attr_freq[ik, iattr]);
      end;
    end;

    itr := 0;
    converged := False;
    cost := MaxInt;
    totalmoves := 0;

    while (itr <= MaxIter) and not converged do
    begin
      Inc(itr);

      moves := KModesIter(Seed);
      ncost := LabelsCost;

      converged := (moves = 0) and (ncost >= cost);
      cost := ncost;

      totalmoves += moves;

      if Log then
        DebugLn(['Itr: ', itr, #9'Moves: ', moves, #9'Cost: ', cost]);
    end;

    all[init_no].Labels := Copy(labels);
    all[init_no].Centroids := Copy(centroids);
    all[init_no].Cost := cost;
    all[init_no].NIter := itr;
    all[init_no].TotalMoves := totalmoves;
  end;

  j := -1;
  best := MaxInt;
  for i := 0 to High(all) do
    if all[i].Cost < best then
    begin
      best := all[i].Cost;
      j := i;
    end;

  FinalLabels := all[j].Labels;
  FinalCentroids := all[j].Centroids;
  Result := Length(centroids);
end;

end.

