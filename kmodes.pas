// Adapted from https://github.com/nicodv/kmodes

unit kmodes;

//{$define GENERIC_DISSIM}

{$mode delphi}

interface

uses
  Classes, SysUtils, Types, Math, LazLogger, MTProcs, windows;

const
  cKModesFeatureCount = 64;
  cPhi = (1 + sqrt(5)) / 2;
  cInvPhi = 1 / cPhi;

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
    Cost: UInt64;
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
    MaxIter, NumClusters, NumThreads, NumAttrs, NumPoints: Integer;
    Log: Boolean;

    function CountClusterMembers(cluster: Integer): Integer;
    function GetMaxClusterMembers(out member_count: Integer): Integer;
    function LabelsCost: UInt64;
    function KModesIter(var Seed: Cardinal): Integer;
    function InitFarthestFirst(InitPoint: Integer): TByteDynArray2;  // negative init_point means randomly chosen
    procedure MovePointCat(const point: TByteDynArray; ipoint, to_clust, from_clust: Integer);
  public
    constructor Create(aNumThreads: Integer = 0; aMaxIter: Integer = 0; aLog: Boolean = False);
    function ComputeKModes(const ADataset: TByteDynArray2; ANumClusters, ANumInit, ANumModalities: Integer; out FinalLabels: TIntegerDynArray; out FinalCentroids: TByteDynArray2): Integer;
    // negative n_init means use -n_init as starting point
    // FinalLabels cluster indexes start at 1
  end;


function RandInt(Range: Cardinal; var Seed: Cardinal): Cardinal;
procedure QuickSort(var AData;AFirstItem,ALastItem,AItemSize:Integer;ACompareFunction:TCompareFunction;AUserParameter:Pointer=nil);
function GetMinMatchingDissim(const a: TByteDynArray2; const b: TByteDynArray; count: Integer; out bestDissim: UInt64): Integer;

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

const
  cDissimSubMatchingSize = 34;

{$if defined(GENERIC_DISSIM) or not defined(CPUX86_64)}

function MatchingDissim(const a: TByteDynArray; const b: TByteDynArray): UInt64; inline;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to High(a) do
  begin
    if a[i] <> b[i] then
      Result += 1 shl cDissimSubMatchingSize;
    Result += abs(a[i] - b[i]);
  end;
end;

function GetMinMatchingDissim(const a: TByteDynArray2; const b: TByteDynArray; count: Integer; out bestDissim: UInt64): Integer;
var
  i: Integer;
  dis, best: UInt64;
begin
  Result := -1;
  best := High(UInt64);
  for i := 0 to count - 1 do
  begin
    dis := MatchingDissim(a[i], b);
    if dis <= best then
    begin
      best := dis;
      Result := i;
    end;
  end;

  bestDissim := best;
end;

function GetMatchingDissimSum(const a: TByteDynArray2; const b: TByteDynArray; count: Integer): UInt64;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to count - 1 do
    Result += MatchingDissim(a[i], b);
end;

{$else}

function GetMinMatchingDissim_Asm(item_rcx: PByte; list_rdx: PPByte; count_r8: UInt64; pbest_r9: PUInt64): Int64; register; assembler; nostackframe;
label loop, worst;
asm
  push rbx
  push rcx
  push rsi
  push rdi
  push r8
  push r10
  push rdx

  sub rsp, 16 * 10
  movdqu oword ptr [rsp],       xmm0
  movdqu oword ptr [rsp + $10], xmm1
  movdqu oword ptr [rsp + $20], xmm2
  movdqu oword ptr [rsp + $30], xmm3
  movdqu oword ptr [rsp + $40], xmm4
  movdqu oword ptr [rsp + $50], xmm5
  movdqu oword ptr [rsp + $60], xmm6
  movdqu oword ptr [rsp + $70], xmm7
  movdqu oword ptr [rsp + $80], xmm8
  movdqu oword ptr [rsp + $90], xmm9

  movdqu xmm6, oword ptr [item_rcx]
  movdqu xmm7, oword ptr [item_rcx + $10]
  movdqu xmm8, oword ptr [item_rcx + $20]
  movdqu xmm9, oword ptr [item_rcx + $30]

  lea rbx, [list_rdx + 8 * count_r8]

  lea rax, [list_rdx - 8]

  xor r8, r8
  dec r8

  loop:
    mov rcx, qword ptr [list_rdx]

    movdqu xmm0, oword ptr [rcx]
    movdqu xmm1, oword ptr [rcx + $10]
    movdqu xmm2, oword ptr [rcx + $20]
    movdqu xmm3, oword ptr [rcx + $30]

    movdqa xmm4, xmm0
    psubb xmm4, xmm6
    pabsb xmm4, xmm4

    movdqa xmm5, xmm1
    psadbw xmm5, xmm7
    paddw xmm4, xmm5

    movdqa xmm5, xmm2
    psadbw xmm5, xmm8
    paddw xmm4, xmm5

    movdqa xmm5, xmm3
    psadbw xmm5, xmm9
    paddw xmm4, xmm5

    pcmpeqb xmm0, xmm6
    pcmpeqb xmm1, xmm7
    pcmpeqb xmm2, xmm8
    pcmpeqb xmm3, xmm9

    pmovmskb edi, xmm0
    mov rsi, rdi
    pmovmskb edi, xmm1
    rol rsi, 16
    or rsi, rdi
    pmovmskb edi, xmm2
    rol rsi, 16
    or rsi, rdi
    pmovmskb edi, xmm3
    rol rsi, 16
    or rsi, rdi
    not rsi
    popcnt rsi, rsi

    shl rsi, cDissimSubMatchingSize
    pextrw r10d, xmm4, 0
    add rsi, r10
    pextrw r10d, xmm4, 4
    add rsi, r10

    cmp rsi, r8
    ja worst

      mov r8, rsi
      mov rax, list_rdx

    worst:

    add list_rdx, 8
    cmp list_rdx, rbx
    jne loop

  movdqu xmm0, oword ptr [rsp]
  movdqu xmm1, oword ptr [rsp + $10]
  movdqu xmm2, oword ptr [rsp + $20]
  movdqu xmm3, oword ptr [rsp + $30]
  movdqu xmm4, oword ptr [rsp + $40]
  movdqu xmm5, oword ptr [rsp + $50]
  movdqu xmm6, oword ptr [rsp + $60]
  movdqu xmm7, oword ptr [rsp + $70]
  movdqu xmm8, oword ptr [rsp + $80]
  movdqu xmm9, oword ptr [rsp + $90]
  add rsp, 16 * 10

  mov qword ptr [pbest_r9], r8

  pop rdx

  sub rax, list_rdx
  sar rax, 3

  pop r10
  pop r8
  pop rdi
  pop rsi
  pop rcx
  pop rbx
end;

function GetMatchingDissimSum_Asm(item_rcx: PByte; list_rdx: PPByte; count_r8: UInt64): UInt64; register; assembler; nostackframe;
label loop;
asm
  push rbx
  push rcx
  push rsi
  push rdi
  push r10
  push rdx

  sub rsp, 16 * 10
  movdqu oword ptr [rsp],       xmm0
  movdqu oword ptr [rsp + $10], xmm1
  movdqu oword ptr [rsp + $20], xmm2
  movdqu oword ptr [rsp + $30], xmm3
  movdqu oword ptr [rsp + $40], xmm4
  movdqu oword ptr [rsp + $50], xmm5
  movdqu oword ptr [rsp + $60], xmm6
  movdqu oword ptr [rsp + $70], xmm7
  movdqu oword ptr [rsp + $80], xmm8
  movdqu oword ptr [rsp + $90], xmm9

  movdqu xmm6, oword ptr [item_rcx]
  movdqu xmm7, oword ptr [item_rcx + $10]
  movdqu xmm8, oword ptr [item_rcx + $20]
  movdqu xmm9, oword ptr [item_rcx + $30]

  lea rbx, [list_rdx + 8 * count_r8]

  xor rax, rax
  
  loop:
    mov rcx, qword ptr [list_rdx]
    add list_rdx, 8

    movdqu xmm0, oword ptr [rcx]
    movdqu xmm1, oword ptr [rcx + $10]
    movdqu xmm2, oword ptr [rcx + $20]
    movdqu xmm3, oword ptr [rcx + $30]

    movdqa xmm4, xmm0
    psubb xmm4, xmm6
    pabsb xmm4, xmm4

    movdqa xmm5, xmm1
    psadbw xmm5, xmm7
    paddw xmm4, xmm5

    movdqa xmm5, xmm2
    psadbw xmm5, xmm8
    paddw xmm4, xmm5

    movdqa xmm5, xmm3
    psadbw xmm5, xmm9
    paddw xmm4, xmm5

    pcmpeqb xmm0, xmm6
    pcmpeqb xmm1, xmm7
    pcmpeqb xmm2, xmm8
    pcmpeqb xmm3, xmm9

    pmovmskb edi, xmm0
    mov rsi, rdi
    pmovmskb edi, xmm1
    rol rsi, 16
    or rsi, rdi
    pmovmskb edi, xmm2
    rol rsi, 16
    or rsi, rdi
    pmovmskb edi, xmm3
    rol rsi, 16
    or rsi, rdi
    not rsi
    popcnt rsi, rsi

    shl rsi, cDissimSubMatchingSize
    pextrw r10d, xmm4, 0
    add rsi, r10
    pextrw r10d, xmm4, 4
    add rsi, r10

    add rax, rsi

    cmp list_rdx, rbx
    jne loop

  movdqu xmm0, oword ptr [rsp]
  movdqu xmm1, oword ptr [rsp + $10]
  movdqu xmm2, oword ptr [rsp + $20]
  movdqu xmm3, oword ptr [rsp + $30]
  movdqu xmm4, oword ptr [rsp + $40]
  movdqu xmm5, oword ptr [rsp + $50]
  movdqu xmm6, oword ptr [rsp + $60]
  movdqu xmm7, oword ptr [rsp + $70]
  movdqu xmm8, oword ptr [rsp + $80]
  movdqu xmm9, oword ptr [rsp + $90]
  add rsp, 16 * 10

  pop rdx
  pop r10
  pop rdi
  pop rsi
  pop rcx
  pop rbx

end;

function GetMinMatchingDissim(const a: TByteDynArray2; const b: TByteDynArray; count: Integer; out bestDissim: UInt64): Integer;
var
  bd: UInt64;
begin
  Result := GetMinMatchingDissim_Asm(@b[0], @a[0], count, @bd);
  bestDissim := bd;
end;

function GetMatchingDissimSum(const a: TByteDynArray2; const b: TByteDynArray; count: Integer): UInt64;
begin
  Result := GetMatchingDissimSum_Asm(@b[0], @a[0], count);
end;

{$endif}

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
      Inc(mc_loc, Ord(pm0^ = cluster)); Inc(pm0, quantum);
      Inc(mc_loc, Ord(pm1^ = cluster)); Inc(pm1, quantum);
      Inc(mc_loc, Ord(pm2^ = cluster)); Inc(pm2, quantum);
      Inc(mc_loc, Ord(pm3^ = cluster)); Inc(pm3, quantum);
    end;

    for i := (quantum shl 2) to Length(membship) - 1 do
    begin
      Inc(mc_loc, Ord(pm0^ = cluster)); Inc(pm0);
    end;

    if mc_loc >= mc then
    begin
      mc := mc_loc;
      Result := cluster;
    end;
  end;

  member_count := mc;
end;


function TKModes.InitFarthestFirst(InitPoint: Integer): TByteDynArray2;  // negative init_point means randomly chosen
var
  iCentroid, iFarthest: Integer;
  Used: TBooleanDynArray;
  MinDistance: TUInt64DynArray;

  ApproxCentroids: TByteDynArray2;
  ApproxCentroidCount: Integer;

  procedure BuildApproximateCentroids(NewCentroid: TByteDynArray; CentroidCount: Integer);
  var
    IdealCount: Integer;
    i: Integer;
    dis: UInt64;
    idx: Integer;
  begin
    ApproxCentroids[ApproxCentroidCount] := NewCentroid;
    Inc(ApproxCentroidCount);

    IdealCount := CentroidCount;
    IdealCount := ceil(power(CentroidCount, cInvPhi));

    if ApproxCentroidCount > IdealCount then
    begin
      Assert(ApproxCentroidCount - IdealCount = 1);

      idx := GetMinMatchingDissim(ApproxCentroids, NewCentroid, ApproxCentroidCount, dis);

      for i := ApproxCentroidCount - 1 downto idx + 1 do
        ApproxCentroids[i - 1] := ApproxCentroids[i];

      Dec(ApproxCentroidCount);
    end;
  end;

  procedure UpdateMinDistance(CentroidCount: Integer);
  var
    i: Integer;
  begin
    for i := 0 to NumPoints - 1 do
      if not Used[i] then
        MinDistance[i] := GetMatchingDissimSum(ApproxCentroids, X[i], ApproxCentroidCount);
  end;

  function FarthestAway: Integer;
  var
    i: Integer;
    max: UInt64;
  begin
    max := 0;
    Result := -1;
    for i := 0 to NumPoints - 1 do
      if not Used[i] and (max <= MinDistance[i]) then
      begin
        max := MinDistance[i];
        Result := i;
      end;
  end;

begin
  SetLength(Result, NumClusters, NumAttrs);
  SetLength(ApproxCentroids, NumClusters);
  SetLength(Used, NumPoints);
  SetLength(MinDistance, NumPoints);

  ApproxCentroidCount := 0;

  for iCentroid := 0 to NumClusters - 1 do
    FillByte(Result[iCentroid, 0], NumAttrs, High(Byte));
  FillChar(Used[0], NumPoints, False);
  FillQWord(MinDistance[0], NumPoints, High(UInt64));

  iCentroid := 0;

  iFarthest := InitPoint;
  Move(X[iFarthest, 0], Result[iCentroid, 0], NumAttrs);
  Used[iFarthest] := True;
  BuildApproximateCentroids(Result[iCentroid], 1);
  UpdateMinDistance(1);

  for iCentroid := 1 to NumClusters - 1 do
  begin
    iFarthest := FarthestAway;
    Move(X[iFarthest, 0], Result[iCentroid, 0], NumAttrs);
    Used[iFarthest] := True;
    BuildApproximateCentroids(Result[iCentroid], iCentroid + 1);
    UpdateMinDistance(iCentroid + 1);
  end;
end;

function TKModes.LabelsCost: UInt64;
var
  ipoint, clust: Integer;
  dis: UInt64;
begin
  Result := 0;
  SetLength(labels, NumPoints);
  FillDWord(labels[0], NumPoints, 0);

  for ipoint := 0 to NumPoints - 1 do
  begin
    clust := GetMinMatchingDissim(centroids, X[ipoint], Length(centroids), dis);
    labels[ipoint] := clust;
    Result += dis;
  end;
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
  ipoint, clust, old_clust, from_clust, rindx, cnt, dummy, i: Integer;
  dis: UInt64;
  choices: TIntegerDynArray;
begin
  Result := 0;

  SetLength(choices, NumPoints);

  for ipoint := 0 to NumPoints - 1 do
  begin
    clust := GetMinMatchingDissim(centroids, X[ipoint], Length(centroids),dis);

    if membship[ipoint] <> clust then
    begin
      Inc(Result);
      old_clust := membship[ipoint];

      MovePointCat(X[ipoint], ipoint, clust, old_clust);

      if CountClusterMembers(old_clust) = 0 then
      begin
        from_clust := GetMaxClusterMembers(dummy);
        writeln('zero', #9, from_clust, #9, dummy);

        cnt := 0;
        for i := 0 to High(membship) do
          if membship[i] = from_clust then
          begin
            choices[cnt] := i;
            Inc(cnt);
          end;

        rindx := choices[RandInt(cnt, Seed)];

        MovePointCat(X[rindx], rindx, old_clust, from_clust);
      end;
    end;
  end;
end;

function TKModes.ComputeKModes(const ADataset: TByteDynArray2; ANumClusters, ANumInit, ANumModalities: Integer;
  out FinalLabels: TIntegerDynArray; out FinalCentroids: TByteDynArray2): Integer;
var
  init: TByteDynArray2;
  all: array of TKmodesRun;
var
  i, j, init_no, iattr, ipoint, ik, summemb, itr, moves, totalmoves, clust: Integer;
  best, dis: UInt64;
  converged: Boolean;
  cost, ncost: UInt64;
  InvGoldenRatio, GRAcc: Single;
  Seed: Cardinal;
begin
  Seed := $42381337;

  X := ADataset;

  NumPoints := Length(X);
  NumClusters := ANumClusters;

  NumAttrs := 0;
  if NumPoints > 0 then
    NumAttrs := Length(X[0]);

  if NumThreads <= 0 then
    NumThreads := ProcThreadPool.MaxThreadCount;

  if MaxIter <= 0 then
    MaxIter := MaxInt;

  //init := nil;
  //if CountUniqueRows(X) <= NumClusters then
  //begin
  //  MaxIter := 0;
  //  ANumInit := 1;
  //  init := GetUniqueRows(X);
  //  NumClusters := Length(init);
  //end;

  if ANumInit <= 0 then
  begin
    SetLength(all, 1);
    all[0].StartingPoint := -ANumInit;
  end
  else
  begin
    SetLength(all, ANumInit);
    InvGoldenRatio := power(NumPoints, 1 / ANumInit);
    GRAcc := 1;
    for i := 0 to ANumInit - 1 do
    begin
      all[i].StartingPoint := Round(GRAcc) - 1;
      if (i > 0) and (all[i].StartingPoint <= all[i - 1].StartingPoint) then
        all[i].StartingPoint := Min(NumPoints - 1, all[i - 1].StartingPoint + 1);
      GRAcc := GRAcc * InvGoldenRatio;
    end;
  end;

  for init_no := 0 to High(all) do
  begin
    SetLength(membship, NumPoints);
    SetLength(cl_attr_freq, NumClusters, NumAttrs, ANumModalities);

    if init = nil then
      centroids := InitFarthestFirst(all[init_no].StartingPoint)
    else
      centroids := init;

    FillDWord(membship[0], NumPoints, $ffffffff);
    for j := 0 to NumClusters - 1 do
      for i := 0 to NumAttrs - 1 do
        FillDWord(cl_attr_freq[j, i, 0], ANumModalities, 0);

    for ipoint := 0 to NumPoints - 1 do
    begin
      clust := GetMinMatchingDissim(centroids, X[ipoint], Length(centroids), dis);

      membship[ipoint] := clust;

      for iattr := 0 to NumAttrs - 1 do
        Inc(cl_attr_freq[clust, iattr, X[ipoint, iattr]]);
    end;

    for ik := 0 to NumClusters - 1  do
    begin
      summemb := CountClusterMembers(ik);
      if summemb = 0 then
      begin
        for iattr := 0 to NumAttrs - 1 do
          centroids[ik, iattr] := X[RandInt(NumPoints, Seed), iattr]
      end
      else
      begin
         for iattr := 0 to NumAttrs - 1 do
           centroids[ik, iattr] := GetMaxValueIndex(cl_attr_freq[ik, iattr]);
      end;
    end;

    itr := 0;
    converged := False;
    cost := High(UInt64);
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
  best := High(UInt64);
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

