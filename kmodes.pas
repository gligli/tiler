// Ported from https://github.com/nicodv/kmodes

unit kmodes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, Math, LazLogger, MTProcs;

type
  TCompareFunction=function(Item1,Item2,UserParameter:Pointer):Integer;
  TIntegerDynArray2 = array of TIntegerDynArray;
  TIntegerDynArray3 = array of TIntegerDynArray2;
  TByteDynArray2 = array of TByteDynArray;

procedure QuickSort(var AData;AFirstItem,ALastItem,AItemSize:Integer;ACompareFunction:TCompareFunction;AUserParameter:Pointer=nil);
procedure ComputeKModes(const X: TByteDynArray2; n_clusters, max_iter, n_init, n_modalities, n_threads: Integer; var FinalLabels: TIntegerDynArray);

implementation

procedure QuickSort(var AData;AFirstItem,ALastItem,AItemSize:Integer;ACompareFunction:TCompareFunction;AUserParameter:Pointer=nil);
var I, J, K, P: Integer;
    PData,P1,P2:PByte;
    Tmp:integer;
begin
  Assert((AItemSize and 3)=0,'AItemSize doit être multiple de 4 pour le moment');
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
      //P2:=PData;Inc(P2,P*AItemSize); déjà fait avant
      while ACompareFunction(P1, P2, AUserParameter) > 0 do
      begin
        Dec(J);
        Dec(P1,AItemSize);
      end;
      if I <= J then
      begin
        // Swap 4 octets par 4 octets
        P1:=PData;Inc(P1,I*AItemSize);
        P2:=PData;Inc(P2,J*AItemSize);
        for k:=1 to AItemSize shr 2 do
        begin
          Tmp:=PInteger(P2)^;
          PInteger(P2)^:=PInteger(P1)^;
          PInteger(P1)^:=Tmp;
          Inc(P1,4);
          Inc(P2,4);
        end;

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
  Result := CompareByte(PByteArray(Item1)^[0], PByteArray(Item2)^[0], NativeInt(UserParameter));
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

function GetMinValueIndex(const arr: TIntegerDynArray): Integer;
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

function GetMinValueIndex(const arr: TByteDynArray): Integer;
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
begin
  Result := 0;

  if Length(X) < 1 then
    Exit;

  nattrs := Length(X[0]);

  QuickSort(X[0], 0, High(X), SizeOf(TByteDynArray), @CompareLines, Pointer(nattrs));

  Cnt := Length(X);
  for i := High(X) - 1 downto 0 do
    if CompareByte(X[i, 0], X[i + 1, 0], nattrs) = 0 then
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
function MatchingDissim(const a: TByteDynArray; const b: TByteDynArray): Byte;
begin
  asm
    xor rax, rax

    // Unrolled for 64 items
    mov r8b, byte ptr [rcx]; inc rcx; mov r9b, byte ptr [rdx]; inc rdx; cmp r8b, r9b; setnz ah; add al, ah
    mov r8b, byte ptr [rcx]; inc rcx; mov r9b, byte ptr [rdx]; inc rdx; cmp r8b, r9b; setnz ah; add al, ah
    mov r8b, byte ptr [rcx]; inc rcx; mov r9b, byte ptr [rdx]; inc rdx; cmp r8b, r9b; setnz ah; add al, ah
    mov r8b, byte ptr [rcx]; inc rcx; mov r9b, byte ptr [rdx]; inc rdx; cmp r8b, r9b; setnz ah; add al, ah
    mov r8b, byte ptr [rcx]; inc rcx; mov r9b, byte ptr [rdx]; inc rdx; cmp r8b, r9b; setnz ah; add al, ah
    mov r8b, byte ptr [rcx]; inc rcx; mov r9b, byte ptr [rdx]; inc rdx; cmp r8b, r9b; setnz ah; add al, ah
    mov r8b, byte ptr [rcx]; inc rcx; mov r9b, byte ptr [rdx]; inc rdx; cmp r8b, r9b; setnz ah; add al, ah
    mov r8b, byte ptr [rcx]; inc rcx; mov r9b, byte ptr [rdx]; inc rdx; cmp r8b, r9b; setnz ah; add al, ah

    mov r8b, byte ptr [rcx]; inc rcx; mov r9b, byte ptr [rdx]; inc rdx; cmp r8b, r9b; setnz ah; add al, ah
    mov r8b, byte ptr [rcx]; inc rcx; mov r9b, byte ptr [rdx]; inc rdx; cmp r8b, r9b; setnz ah; add al, ah
    mov r8b, byte ptr [rcx]; inc rcx; mov r9b, byte ptr [rdx]; inc rdx; cmp r8b, r9b; setnz ah; add al, ah
    mov r8b, byte ptr [rcx]; inc rcx; mov r9b, byte ptr [rdx]; inc rdx; cmp r8b, r9b; setnz ah; add al, ah
    mov r8b, byte ptr [rcx]; inc rcx; mov r9b, byte ptr [rdx]; inc rdx; cmp r8b, r9b; setnz ah; add al, ah
    mov r8b, byte ptr [rcx]; inc rcx; mov r9b, byte ptr [rdx]; inc rdx; cmp r8b, r9b; setnz ah; add al, ah
    mov r8b, byte ptr [rcx]; inc rcx; mov r9b, byte ptr [rdx]; inc rdx; cmp r8b, r9b; setnz ah; add al, ah
    mov r8b, byte ptr [rcx]; inc rcx; mov r9b, byte ptr [rdx]; inc rdx; cmp r8b, r9b; setnz ah; add al, ah

    mov r8b, byte ptr [rcx]; inc rcx; mov r9b, byte ptr [rdx]; inc rdx; cmp r8b, r9b; setnz ah; add al, ah
    mov r8b, byte ptr [rcx]; inc rcx; mov r9b, byte ptr [rdx]; inc rdx; cmp r8b, r9b; setnz ah; add al, ah
    mov r8b, byte ptr [rcx]; inc rcx; mov r9b, byte ptr [rdx]; inc rdx; cmp r8b, r9b; setnz ah; add al, ah
    mov r8b, byte ptr [rcx]; inc rcx; mov r9b, byte ptr [rdx]; inc rdx; cmp r8b, r9b; setnz ah; add al, ah
    mov r8b, byte ptr [rcx]; inc rcx; mov r9b, byte ptr [rdx]; inc rdx; cmp r8b, r9b; setnz ah; add al, ah
    mov r8b, byte ptr [rcx]; inc rcx; mov r9b, byte ptr [rdx]; inc rdx; cmp r8b, r9b; setnz ah; add al, ah
    mov r8b, byte ptr [rcx]; inc rcx; mov r9b, byte ptr [rdx]; inc rdx; cmp r8b, r9b; setnz ah; add al, ah
    mov r8b, byte ptr [rcx]; inc rcx; mov r9b, byte ptr [rdx]; inc rdx; cmp r8b, r9b; setnz ah; add al, ah

    mov r8b, byte ptr [rcx]; inc rcx; mov r9b, byte ptr [rdx]; inc rdx; cmp r8b, r9b; setnz ah; add al, ah
    mov r8b, byte ptr [rcx]; inc rcx; mov r9b, byte ptr [rdx]; inc rdx; cmp r8b, r9b; setnz ah; add al, ah
    mov r8b, byte ptr [rcx]; inc rcx; mov r9b, byte ptr [rdx]; inc rdx; cmp r8b, r9b; setnz ah; add al, ah
    mov r8b, byte ptr [rcx]; inc rcx; mov r9b, byte ptr [rdx]; inc rdx; cmp r8b, r9b; setnz ah; add al, ah
    mov r8b, byte ptr [rcx]; inc rcx; mov r9b, byte ptr [rdx]; inc rdx; cmp r8b, r9b; setnz ah; add al, ah
    mov r8b, byte ptr [rcx]; inc rcx; mov r9b, byte ptr [rdx]; inc rdx; cmp r8b, r9b; setnz ah; add al, ah
    mov r8b, byte ptr [rcx]; inc rcx; mov r9b, byte ptr [rdx]; inc rdx; cmp r8b, r9b; setnz ah; add al, ah
    mov r8b, byte ptr [rcx]; inc rcx; mov r9b, byte ptr [rdx]; inc rdx; cmp r8b, r9b; setnz ah; add al, ah

    mov r8b, byte ptr [rcx]; inc rcx; mov r9b, byte ptr [rdx]; inc rdx; cmp r8b, r9b; setnz ah; add al, ah
    mov r8b, byte ptr [rcx]; inc rcx; mov r9b, byte ptr [rdx]; inc rdx; cmp r8b, r9b; setnz ah; add al, ah
    mov r8b, byte ptr [rcx]; inc rcx; mov r9b, byte ptr [rdx]; inc rdx; cmp r8b, r9b; setnz ah; add al, ah
    mov r8b, byte ptr [rcx]; inc rcx; mov r9b, byte ptr [rdx]; inc rdx; cmp r8b, r9b; setnz ah; add al, ah
    mov r8b, byte ptr [rcx]; inc rcx; mov r9b, byte ptr [rdx]; inc rdx; cmp r8b, r9b; setnz ah; add al, ah
    mov r8b, byte ptr [rcx]; inc rcx; mov r9b, byte ptr [rdx]; inc rdx; cmp r8b, r9b; setnz ah; add al, ah
    mov r8b, byte ptr [rcx]; inc rcx; mov r9b, byte ptr [rdx]; inc rdx; cmp r8b, r9b; setnz ah; add al, ah
    mov r8b, byte ptr [rcx]; inc rcx; mov r9b, byte ptr [rdx]; inc rdx; cmp r8b, r9b; setnz ah; add al, ah

    mov r8b, byte ptr [rcx]; inc rcx; mov r9b, byte ptr [rdx]; inc rdx; cmp r8b, r9b; setnz ah; add al, ah
    mov r8b, byte ptr [rcx]; inc rcx; mov r9b, byte ptr [rdx]; inc rdx; cmp r8b, r9b; setnz ah; add al, ah
    mov r8b, byte ptr [rcx]; inc rcx; mov r9b, byte ptr [rdx]; inc rdx; cmp r8b, r9b; setnz ah; add al, ah
    mov r8b, byte ptr [rcx]; inc rcx; mov r9b, byte ptr [rdx]; inc rdx; cmp r8b, r9b; setnz ah; add al, ah
    mov r8b, byte ptr [rcx]; inc rcx; mov r9b, byte ptr [rdx]; inc rdx; cmp r8b, r9b; setnz ah; add al, ah
    mov r8b, byte ptr [rcx]; inc rcx; mov r9b, byte ptr [rdx]; inc rdx; cmp r8b, r9b; setnz ah; add al, ah
    mov r8b, byte ptr [rcx]; inc rcx; mov r9b, byte ptr [rdx]; inc rdx; cmp r8b, r9b; setnz ah; add al, ah
    mov r8b, byte ptr [rcx]; inc rcx; mov r9b, byte ptr [rdx]; inc rdx; cmp r8b, r9b; setnz ah; add al, ah

    mov r8b, byte ptr [rcx]; inc rcx; mov r9b, byte ptr [rdx]; inc rdx; cmp r8b, r9b; setnz ah; add al, ah
    mov r8b, byte ptr [rcx]; inc rcx; mov r9b, byte ptr [rdx]; inc rdx; cmp r8b, r9b; setnz ah; add al, ah
    mov r8b, byte ptr [rcx]; inc rcx; mov r9b, byte ptr [rdx]; inc rdx; cmp r8b, r9b; setnz ah; add al, ah
    mov r8b, byte ptr [rcx]; inc rcx; mov r9b, byte ptr [rdx]; inc rdx; cmp r8b, r9b; setnz ah; add al, ah
    mov r8b, byte ptr [rcx]; inc rcx; mov r9b, byte ptr [rdx]; inc rdx; cmp r8b, r9b; setnz ah; add al, ah
    mov r8b, byte ptr [rcx]; inc rcx; mov r9b, byte ptr [rdx]; inc rdx; cmp r8b, r9b; setnz ah; add al, ah
    mov r8b, byte ptr [rcx]; inc rcx; mov r9b, byte ptr [rdx]; inc rdx; cmp r8b, r9b; setnz ah; add al, ah
    mov r8b, byte ptr [rcx]; inc rcx; mov r9b, byte ptr [rdx]; inc rdx; cmp r8b, r9b; setnz ah; add al, ah

    mov r8b, byte ptr [rcx]; inc rcx; mov r9b, byte ptr [rdx]; inc rdx; cmp r8b, r9b; setnz ah; add al, ah
    mov r8b, byte ptr [rcx]; inc rcx; mov r9b, byte ptr [rdx]; inc rdx; cmp r8b, r9b; setnz ah; add al, ah
    mov r8b, byte ptr [rcx]; inc rcx; mov r9b, byte ptr [rdx]; inc rdx; cmp r8b, r9b; setnz ah; add al, ah
    mov r8b, byte ptr [rcx]; inc rcx; mov r9b, byte ptr [rdx]; inc rdx; cmp r8b, r9b; setnz ah; add al, ah
    mov r8b, byte ptr [rcx]; inc rcx; mov r9b, byte ptr [rdx]; inc rdx; cmp r8b, r9b; setnz ah; add al, ah
    mov r8b, byte ptr [rcx]; inc rcx; mov r9b, byte ptr [rdx]; inc rdx; cmp r8b, r9b; setnz ah; add al, ah
    mov r8b, byte ptr [rcx]; inc rcx; mov r9b, byte ptr [rdx]; inc rdx; cmp r8b, r9b; setnz ah; add al, ah
    mov r8b, byte ptr [rcx]; inc rcx; mov r9b, byte ptr [rdx]; inc rdx; cmp r8b, r9b; setnz ah; add al, ah

    xor ah, ah
  end ['r8b', 'r9b'];
end;
{$ifend}

procedure MatchingDissim(const a: TByteDynArray2; const b: TByteDynArray; var Dissim: TByteDynArray);
var
  i: Integer;
begin
  SetLength(Dissim, Length(a));

  for i := 0 to High(a) do
    Dissim[i] := MatchingDissim(a[i], b);
end;

function CountClusterMembers(cluster: Integer; var membship: TIntegerDynArray): Integer;
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

procedure InitFarthestFirst(const X: TByteDynArray2; n_clusters: Integer; var centroids: TByteDynArray2);
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

  for icentroid := 0 to n_clusters - 1 do
  begin
    if icentroid = 0 then
      ifarthest := Random(npoints)
    else
      ifarthest := FarthestAway;

    Move(X[ifarthest, 0], centroids[icentroid, 0], nattrs);
    used[ifarthest] := True;
    UpdateMinDistance(ifarthest);
  end;
end;

function LabelsCost(const X, centroids: TByteDynArray2; var labels: TIntegerDynArray): Integer;
var
  npoints, ipoint, clust: Integer;
  dis: TByteDynArray;
begin
  npoints := Length(X);
  Result := 0;
  SetLength(labels, npoints);
  FillDWord(labels[0], npoints, 0);

  for ipoint := 0 to npoints - 1 do
  begin
    MatchingDissim(centroids, X[ipoint], dis);
    clust := GetMinValueIndex(dis);
    labels[ipoint] := clust;
    Result += dis[clust];
  end;
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

function KModesIter(const X: TByteDynArray2; var cl_attr_freq: TIntegerDynArray3; var centroids: TByteDynArray2; var membship: TIntegerDynArray): Integer;
var
  npoints, ipoint, ipoint2, ik, clust, old_clust, from_clust, rindx, cnt: Integer;
  clsize, choices: TIntegerDynArray;
  dis: TByteDynArray;
begin
  npoints := Length(X);
  Result := 0;

  for ipoint := 0 to npoints - 1 do
  begin
    MatchingDissim(centroids, X[ipoint], dis);
    clust := GetMinValueIndex(dis);
    if membship[ipoint] = clust then
      Continue;

    Inc(Result);
    old_clust := membship[ipoint];

    MovePointCat(X[ipoint], ipoint, clust, old_clust, cl_attr_freq, centroids, membship);

    if CountClusterMembers(old_clust, membship) = 0 then
    begin
      SetLength(clsize, Length(membship));
      for ik := 0 to High(membship) do
        clsize[ik] := CountClusterMembers(ik, membship);
      from_clust := GetMaxValueIndex(clsize);

      SetLength(choices, npoints);
      cnt := 0;
      for ipoint2 := 0 to npoints - 1 do
        if membship[ipoint2] = from_clust then
        begin
          choices[cnt] := ipoint2;
          Inc(cnt);
        end;
      rindx := choices[Random(cnt)];

      MovePointCat(X[rindx], rindx, old_clust, from_clust, cl_attr_freq, centroids, membship);
    end;
  end;
end;

procedure ComputeKModes(const X: TByteDynArray2; n_clusters, max_iter, n_init, n_modalities,
  n_threads: Integer; var FinalLabels: TIntegerDynArray);
var
  best, i, j, npoints, nattrs: Integer;
  init: TByteDynArray2;
  all: array of record
    Labels: TIntegerDynArray;
    Cost: Integer;
    NIter: Integer;
    TotalMoves: Integer;
  end;

  procedure DoKModes(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    i, j, init_no, ipoint, iattr, clust, ik, summemb, itr, moves, totalmoves: Integer;
    converged: Boolean;
    cost, ncost: Integer;
    labels, membship: TIntegerDynArray;
    dis: TByteDynArray;
    centroids: TByteDynArray2;
    cl_attr_freq: TIntegerDynArray3;
  begin
    init_no := AIndex;

    SetLength(membship, npoints);
    SetLength(cl_attr_freq, n_clusters, nattrs, n_modalities);

    if init = nil then
      InitFarthestFirst(X, n_clusters, centroids)
    else
      centroids := init;

    FillDWord(membship[0], npoints, $ffffffff);
    for j := 0 to n_clusters - 1 do
      for i := 0 to nattrs - 1 do
        FillDWord(cl_attr_freq[j, i, 0], n_modalities, 0);

    for ipoint := 0 to npoints - 1 do
    begin
      MatchingDissim(centroids, X[ipoint], dis);
      clust := GetMinValueIndex(dis);

      membship[ipoint] := clust;

      for iattr := 0 to nattrs - 1 do
        Inc(cl_attr_freq[clust, iattr, X[ipoint, iattr]]);
    end;

    for ik := 0 to n_clusters - 1  do
    begin
      summemb := CountClusterMembers(ik, membship);
      for iattr := 0 to nattrs - 1 do
        if summemb = 0 then
          centroids[ik, iattr] := X[Random(npoints), iattr]
        else
          centroids[ik, iattr] := GetMaxValueIndex(cl_attr_freq[ik, iattr]);
    end;

    itr := 0;
    converged := False;
    cost := MaxInt;
    totalmoves := 0;

    while (itr <= max_iter) and not converged do
    begin
      Inc(itr);

      moves := KModesIter(X, cl_attr_freq, centroids, membship);
      ncost := LabelsCost(X, centroids, labels);

      converged := (moves = 0) or (ncost >= cost);
      cost := ncost;

      totalmoves += moves;
    end;

    all[init_no].Labels := Copy(labels);
    all[init_no].Cost := cost;
    all[init_no].NIter := itr;
    all[init_no].TotalMoves := totalmoves;
  end;

begin
  npoints := Length(X);
  nattrs := 0;
  if npoints > 0 then
    nattrs := Length(X[0]);

  if CountUniqueRows(X) <= n_clusters then
  begin
    max_iter := 0;
    n_init := 1;
    init := GetUniqueRows(X);
    n_clusters := Length(init);
  end;

  SetLength(all, n_init);
  ProcThreadPool.DoParallelLocalProc(@DoKModes, 0, n_init - 1, nil, n_threads);

  j := -1;
  best := MaxInt;
  for i := 0 to n_init - 1 do
    if all[i].Cost < best then
    begin
      best := all[i].Cost;
      j := i;
    end;

  FinalLabels := all[j].Labels;
end;

end.

