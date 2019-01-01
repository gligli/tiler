// Ported from https://github.com/nicodv/kmodes

unit kmodes;

//{$define HAS_NO_POPCNT}
//{$define GENERIC_DISSIM}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, Math, LazLogger, MTProcs, windows;

type
  TCompareFunction=function(Item1,Item2,UserParameter:Pointer):Integer;
  TIntegerDynArray2 = array of TIntegerDynArray;
  TIntegerDynArray3 = array of TIntegerDynArray2;
  TByteDynArray2 = array of TByteDynArray;
  TUInt64DynArray = array of UInt64;
  TSpinlock = LongInt;

function RandInt(Range: Cardinal; var Seed: Cardinal): Cardinal;
procedure QuickSort(var AData;AFirstItem,ALastItem,AItemSize:Integer;ACompareFunction:TCompareFunction;AUserParameter:Pointer=nil);
function MatchingDissim(const a: TByteDynArray; const b: TByteDynArray): Byte;
function GetMinMatchingDissim(const a: TByteDynArray2; const b: TByteDynArray; out bestDissim: Integer): Integer;
function ComputeKModes(const X: TByteDynArray2; n_clusters, max_iter, n_init, n_modalities, n_threads: Integer; var FinalLabels: TIntegerDynArray; out FinalCentroids: TByteDynArray2): Integer;
// negative n_init means use -n_init as starting point
// FinalLabels cluster indexes start at 1

implementation

function RandInt(Range: Cardinal; var Seed: Cardinal): Cardinal;
begin
  Seed := Integer(Seed * $08088405) + 1;
  Result := (Seed * Range) shr 32;
end;

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

function GetLocPoints(AIndex, n_chunks, npoints: Integer; out n_loc_points: Integer): Integer;
var
  nlp: Integer;
begin
  nlp := npoints div n_chunks;
  Result := AIndex * nlp;
  if AIndex = n_chunks - 1 then
    nlp := npoints - Result;

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
  nattrs: Integer;
  unique: TByteDynArray2;
begin
  Result := 0;

  if Length(X) < 1 then
    Exit;

  nattrs := Length(X[0]);

  unique := Copy(X);
  QuickSort(unique[0], 0, High(unique), SizeOf(TByteDynArray), @CompareLines, Pointer(nattrs));

  Cnt := Length(unique);
  for i := High(unique) - 1 downto 0 do
    if CompareByte(unique[i, 0], unique[i + 1, 0], nattrs) = 0 then
      Dec(Cnt);
  Result := Cnt;
end;

{$if defined(GENERIC_DISSIM) or not defined(CPUX86_64)}
function MatchingDissim(const a: TByteDynArray; const b: TByteDynArray): Byte; inline;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to High(a) do
    if a[i] <> b[i] then
      Inc(Result);
end;
{$else}
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

function MatchingDissim(const a: TByteDynArray; const b: TByteDynArray): Byte; assembler; nostackframe;
asm
  // SIMD for 64 items

  movdqu xmm0, oword ptr [rcx]
  movdqu xmm2, oword ptr [rcx + $10]
  movdqu xmm4, oword ptr [rcx + $20]
  movdqu xmm6, oword ptr [rcx + $30]

  movdqu xmm1, oword ptr [rdx]
  movdqu xmm3, oword ptr [rdx + $10]
  movdqu xmm5, oword ptr [rdx + $20]
  movdqu xmm7, oword ptr [rdx + $30]

  pcmpeqb xmm0, xmm1
  pcmpeqb xmm2, xmm3
  pcmpeqb xmm4, xmm5
  pcmpeqb xmm6, xmm7

  pmovmskb r8d, xmm0
  mov rax, r8
  pmovmskb r8d, xmm2
  rol rax, 16
  or rax, r8
  pmovmskb r8d, xmm4
  rol rax, 16
  or rax, r8
  pmovmskb r8d, xmm6
  rol rax, 16
  or rax, r8

  not rax

  {$ifdef HAS_NO_POPCNT}
  mov rcx, rax
  call CountPopulation
  {$else}
  popcnt rax, rax
  {$endif}

end ['r8', 'xmm0', 'xmm1', 'xmm2', 'xmm3', 'xmm4', 'xmm5', 'xmm6', 'xmm7'];
{$endif}

function GetMinMatchingDissim(const a: TByteDynArray2; const b: TByteDynArray; out bestDissim: Integer): Integer;
const
  cIdxWidth = 48;
var
  i: Integer;
  dis: TInt64DynArray;
  db: Int64;
begin
  SetLength(dis, Length(a));

  for i := 0 to High(a) do
    dis[i] := Int64(MatchingDissim(a[i], b)) shl cIdxWidth or i;

  db := dis[0];
  for i := 1 to High(dis) do
    if dis[i] < db then
      db := dis[i];

  Result := Integer(db and ((1 shl cIdxWidth) - 1));
  bestDissim := db shr cIdxWidth;
end;

function CountClusterMembers(cluster: Integer; const membship: TIntegerDynArray): Integer;
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

function GetMaxClusterMembers(n_clusters: Integer; const membship: TIntegerDynArray; out member_count: Integer): Integer;
var
  quantum, mc_loc, mc, cluster, i: Integer;
  pm0, pm1, pm2, pm3: PInteger;
begin
  Result := 0;
  mc := 0;
  quantum := Length(membship) shr 2;

  for cluster := 0 to n_clusters - 1 do
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


procedure InitFarthestFirst(const X: TByteDynArray2; n_clusters, init_point: Integer; var centroids: TByteDynArray2);  // negative init_point means randomly chosen
var
  nattrs, npoints, icentroid, ifarthest: Integer;
  used: TBooleanDynArray;
  mindistance: TByteDynArray;

  procedure UpdateMinDistance(icenter: Integer);
  var
    i: Integer;
    dis: Byte;
  begin
    for i := 0 to npoints - 1 do
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
    for i := 0 to npoints - 1 do
      if not used[i] and (max <= mindistance[i]) then
      begin
        max := mindistance[i];
        Result := i;
      end;
  end;

begin
  nattrs := Length(X[0]);
  npoints := Length(X);
  SetLength(centroids, n_clusters, nattrs);
  SetLength(used, npoints);
  SetLength(mindistance, npoints);

  for icentroid := 0 to n_clusters - 1 do
    FillByte(centroids[icentroid, 0], nattrs, High(Byte));
  FillChar(used[0], npoints, False);
  FillByte(mindistance[0], npoints, High(Byte));

  ifarthest := init_point;
  Move(X[ifarthest, 0], centroids[icentroid, 0], nattrs);
  used[ifarthest] := True;
  UpdateMinDistance(ifarthest);

  for icentroid := 0 to n_clusters - 1 do
  begin
    ifarthest := FarthestAway;

    Move(X[ifarthest, 0], centroids[icentroid, 0], nattrs);
    used[ifarthest] := True;
    UpdateMinDistance(ifarthest);
  end;
end;

function LabelsCost(const X, centroids: TByteDynArray2; var labels: TIntegerDynArray; n_threads: Integer): Integer;

var
  npoints: Integer;

  procedure DoCost(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    clust, dis, dis_acc: Integer;
    n_loc_points, ipoint, iloc: Integer;
  begin
    ipoint := GetLocPoints(AIndex, n_threads, npoints, n_loc_points);

    dis_acc := 0;

    for iloc := 0 to n_loc_points - 1 do
    begin
      clust := GetMinMatchingDissim(centroids, X[ipoint], dis);
      labels[ipoint] := clust;

      Inc(dis_acc, dis);
      Inc(ipoint);
    end;

    InterLockedExchangeAdd(Result, dis_acc);
  end;

begin
  npoints := Length(X);
  Result := 0;
  SetLength(labels, npoints);
  FillDWord(labels[0], npoints, 0);

  ProcThreadPool.DoParallelLocalProc(@DoCost, 0, n_threads - 1, nil, n_threads);
end;

procedure MovePointCat(const point: TByteDynArray; ipoint, to_clust, from_clust: Integer; var cl_attr_freq: TIntegerDynArray3; var centroids: TByteDynArray2; var membship: TIntegerDynArray);
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

function KModesIter(const X: TByteDynArray2; var cl_attr_freq: TIntegerDynArray3; var centroids: TByteDynArray2; var membship: TIntegerDynArray; var Seed: Cardinal): Integer;
var
  npoints, ipoint, dummy, clust, old_clust, from_clust, rindx, cnt, dis, n_clusters: Integer;
  choices: TIntegerDynArray;
begin
  n_clusters := Length(centroids);
  npoints := Length(X);
  Result := 0;

  SetLength(choices, npoints);

  for ipoint := 0 to npoints - 1 do
  begin
    clust := GetMinMatchingDissim(centroids, X[ipoint], dis);

    if membship[ipoint] <> clust then
    begin
      Inc(Result);
      old_clust := membship[ipoint];

      MovePointCat(X[ipoint], ipoint, clust, old_clust, cl_attr_freq, centroids, membship);

      if CountClusterMembers(old_clust, membship) = 0 then
      begin
        from_clust := GetMaxClusterMembers(n_clusters, membship, dummy);
        cnt := CountClusterMembers(from_clust, membship);
        rindx := choices[RandInt(cnt, Seed)];

        MovePointCat(X[rindx], rindx, old_clust, from_clust, cl_attr_freq, centroids, membship);
      end;
    end;
  end;
end;

type
  TKmodesRun = packed record
    Labels: TIntegerDynArray;
    Centroids: TByteDynArray2;
    Cost: Integer;
    NIter: Integer;
    TotalMoves: Integer;
    StartingPoint: Integer;
  end;

function ComputeKModes(const X: TByteDynArray2; n_clusters, max_iter, n_init, n_modalities,
  n_threads: Integer; var FinalLabels: TIntegerDynArray; out FinalCentroids: TByteDynArray2): Integer;
var
  init: TByteDynArray2;
  npoints, nattrs: Integer;
  all: array of TKmodesRun;
  cl_attr_freq: TIntegerDynArray3;
  centroids: TByteDynArray2;
  membship: TIntegerDynArray;

  procedure DoMembship(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    iattr, clust, dis, dis_acc: Integer;
    n_loc_points, ipoint, iloc: Integer;
  begin
    ipoint := GetLocPoints(AIndex, n_threads, npoints, n_loc_points);
    dis_acc := 0;

    //DebugLn([ipoint,#9,n_loc_points, #9,n_threads]);

    for iloc := 0 to n_loc_points - 1 do
    begin
      clust := GetMinMatchingDissim(centroids, X[ipoint], dis);

      membship[ipoint] := clust;

      for iattr := 0 to nattrs - 1 do
        InterLockedIncrement(cl_attr_freq[clust, iattr, X[ipoint, iattr]]);

      Inc(dis_acc, dis);
      Inc(ipoint);
    end;

    InterLockedExchangeAdd(Result, dis_acc);
  end;

var
  best, i, j, init_no, iattr, ik, summemb, itr, moves, totalmoves: Integer;
  converged: Boolean;
  cost, ncost: Integer;
  labels: TIntegerDynArray;
  InvGoldenRatio, GRAcc: Double;
  Seed: Cardinal;
begin
  Seed := $42381337;

  npoints := Length(X);

  nattrs := 0;
  if npoints > 0 then
    nattrs := Length(X[0]);

  if n_threads <= 0 then
    n_threads := ProcThreadPool.MaxThreadCount;

  init := nil;
  if CountUniqueRows(X) <= n_clusters then
  begin
    max_iter := 0;
    n_init := 1;
    init := GetUniqueRows(X);
    n_clusters := Length(init);
  end;

  if n_init <= 0 then
  begin
    SetLength(all, 1);
    all[0].StartingPoint := -n_init;
  end
  else
  begin
    SetLength(all, n_init);
    InvGoldenRatio := power(npoints, 1 / n_init);
    GRAcc := 1;
    for i := 0 to n_init - 1 do
    begin
      all[i].StartingPoint := Round(GRAcc) - 1;
      if (i > 0) and (all[i].StartingPoint <= all[i - 1].StartingPoint) then
        all[i].StartingPoint := Min(npoints - 1, all[i - 1].StartingPoint + 1);
      GRAcc := GRAcc * InvGoldenRatio;
    end;
  end;

  for init_no := 0 to High(all) do
  begin
    SetLength(membship, npoints);
    SetLength(cl_attr_freq, n_clusters, nattrs, n_modalities);

    if init = nil then
      InitFarthestFirst(X, n_clusters, all[init_no].StartingPoint, centroids)
    else
      centroids := init;

    FillDWord(membship[0], npoints, $ffffffff);
    for j := 0 to n_clusters - 1 do
      for i := 0 to nattrs - 1 do
        FillDWord(cl_attr_freq[j, i, 0], n_modalities, 0);

    ProcThreadPool.DoParallelLocalProc(@DoMembship, 0, n_threads - 1, nil, n_threads);

    for ik := 0 to n_clusters - 1  do
    begin
      summemb := CountClusterMembers(ik, membship);
      if summemb = 0 then
      begin
        for iattr := 0 to nattrs - 1 do
          centroids[ik, iattr] := X[RandInt(npoints, Seed), iattr]
      end
      else
      begin
         for iattr := 0 to nattrs - 1 do
           centroids[ik, iattr] := GetMaxValueIndex(cl_attr_freq[ik, iattr]);
      end;
    end;

    itr := 0;
    converged := False;
    cost := MaxInt;
    totalmoves := 0;

    while (itr <= max_iter) and not converged do
    begin
      Inc(itr);

      moves := KModesIter(X, cl_attr_freq, centroids, membship, Seed);
      ncost := LabelsCost(X, centroids, labels, n_threads);

      converged := (moves = 0) or (ncost >= cost);
      cost := ncost;

      totalmoves += moves;
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

