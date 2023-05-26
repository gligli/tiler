unit extern;

{$mode objfpc}{$H+}

interface

uses
  LazLogger, Windows, Classes, SysUtils, Types, Process, strutils, math;

type
  TFloat = Single;

  TIntegerDynArray2 = array of TIntegerDynArray;
  TByteDynArray2 = array of TByteDynArray;
  TFloatDynArray = array of TFloat;
  TFloatDynArray2 = array of TFloatDynArray;
  TFloatDynArray3 = array of TFloatDynArray2;
  TDoubleDynArray2 = array of TDoubleDynArray;
  TBooleanDynArray2 = array of TBooleanDynArray;
  PFloat = ^TFloat;
  PPFloat = ^PFloat;
  PFloatDynArray = ^TFloatDynArray;
  PFloatDynArray2 = ^TFloatDynArray2;

  TANNsplitRule = (
  		ANN_KD_STD = 0,      // the optimized kd-splitting rule
  		ANN_KD_MIDPT = 1,    // midpoint split
  		ANN_KD_FAIR	= 2,     // fair split
  		ANN_KD_SL_MIDPT = 3, // sliding midpoint splitting method
  		ANN_KD_SL_FAIR = 4,  // sliding fair split method
  		ANN_KD_SUGGEST = 5 // the authors' suggestion for best
  );

  TANNFloat = Double;
  PANNFloat = ^TANNFloat;
  PPANNFloat = ^PANNFloat;
  TANNFloatDynArray = array of TANNFloat;
  TANNFloatDynArray2 = array of TANNFloatDynArray;

  TANNkdtree = record
  end;

  PANNkdtree = ^TANNkdtree;

  TDLUserPal = array[0..2, 0..65535] of Byte;
  PDLUserPal = ^TDLUserPal;

  TYakmo = record
  end;

  PYakmo = ^TYakmo;

  TYakmoSingle = record
  end;

  PYakmoSingle = ^TYakmoSingle;

  TBIRCH = record
  end;

  PBIRCH = ^TBIRCH;

  TBICO = record
  end;

  PBICO = ^TBICO;

  flann_index_t = Pointer;

  flann_algorithm_t = (
  	FLANN_INDEX_LINEAR = 0,
  	FLANN_INDEX_KDTREE = 1,
  	FLANN_INDEX_KMEANS = 2,
  	FLANN_INDEX_COMPOSITE = 3,
  	FLANN_INDEX_KDTREE_SINGLE = 4,
  	FLANN_INDEX_HIERARCHICAL = 5,
  	FLANN_INDEX_LSH = 6,
  	FLANN_INDEX_KDTREE_CUDA = 7, // available if compiled with CUDA
  	FLANN_INDEX_SAVED = 254,
  	FLANN_INDEX_AUTOTUNED = 255
  );

  flann_centers_init_t = (
  	FLANN_CENTERS_RANDOM = 0,
  	FLANN_CENTERS_GONZALES = 1,
  	FLANN_CENTERS_KMEANSPP = 2
  );

  flann_log_level_t = (
  	FLANN_LOG_NONE = 0,
  	FLANN_LOG_FATAL = 1,
  	FLANN_LOG_ERROR = 2,
  	FLANN_LOG_WARN = 3,
  	FLANN_LOG_INFO = 4,
  	FLANN_LOG_DEBUG = 5
  );

  TFLANNParameters = record
    algorithm: flann_algorithm_t; (* the algorithm to use *)

    (* search time parameters *)
    checks: Integer;                (* how many leafs (features) to check in one search *)
    eps: Single;     (* eps parameter for eps-knn search *)
    sorted: Integer;     (* indicates if results returned by radius search should be sorted or not *)
    max_neighbors: Integer;  (* limits the maximum number of neighbors should be returned by radius search *)
    cores: Integer;      (* number of paralel cores to use for searching *)

    (*  kdtree index parameters *)
    trees: Integer;                 (* number of randomized trees to use (for kdtree) *)
    leaf_max_size: Integer;

    (* kmeans index parameters *)
    branching: Integer;             (* branching factor (for kmeans tree) *)
    iterations: Integer;            (* max iterations to perform in one kmeans cluetering (kmeans tree) *)
    centers_init: flann_centers_init_t;  (* algorithm used for picking the initial cluster centers for kmeans tree *)
    cb_index: Single;            (* cluster boundary index. Used when searching the kmeans tree *)

    (* autotuned index parameters *)
    target_precision: Single;    (* precision desired (used for autotuning, -1 otherwise) *)
    build_weight: Single;        (* build tree time weighting factor *)
    memory_weight: Single;       (* index memory weigthing factor *)
    sample_fraction: Single;     (* what fraction of the dataset to use for autotuning *)

    (* LSH parameters *)
    table_number_: Cardinal; (** The number of hash tables to use *)
    key_size_: Cardinal;     (** The length of the key in the hash tables *)
    multi_probe_level_: Cardinal; (** Number of levels to use in multi-probe LSH, 0 for standard LSH *)

    (* other parameters *)
    log_level: flann_log_level_t;    (* determines the verbosity of each flann function *)
    random_seed: LongInt;            (* random seed to use *)
  end;

  PFLANNParameters = ^TFLANNParameters;

  TCompareFunction=function(Item1,Item2,UserParameter:Pointer):Integer;

procedure QuickSort(var AData;AFirstItem,ALastItem:Int64;AItemSize:Integer;ACompareFunction:TCompareFunction;AUserParameter:Pointer=nil);

procedure LZCompress(ASourceStream: TStream; PrintProgress: Boolean; var ADestStream: TStream);

procedure DoExternalSKLearn(Dataset: TByteDynArray2; ClusterCount, Precision: Integer; Compiled, PrintProgress: Boolean; var Clusters: TIntegerDynArray);
procedure DoExternalKMeans(Dataset: TFloatDynArray2;  ClusterCount, ThreadCount: Integer; PrintProgress: Boolean; var Clusters: TIntegerDynArray);

procedure GenerateSVMLightData(Dataset: TFloatDynArray2; Output: TStringList; Header: Boolean);
function GenerateSVMLightFile(Dataset: TFloatDynArray2; Header: Boolean): String;
function GetSVMLightLine(index: Integer; lines: TStringList): TFloatDynArray;
function GetSVMLightClusterCount(lines: TStringList): Integer;

function ann_kdtree_create(pa: PPANNFloat; n, dd, bs: Integer; split: TANNsplitRule): PANNkdtree; external 'ANN.dll';
procedure ann_kdtree_destroy(akd: PANNkdtree); external 'ANN.dll';
function ann_kdtree_search(akd: PANNkdtree; q: PANNFloat; eps: TANNFloat; err: PANNFloat): Integer; external 'ANN.dll';
function ann_kdtree_pri_search(akd: PANNkdtree; q: PANNFloat; eps: TANNFloat; err: PANNFloat): Integer; external 'ANN.dll';
procedure ann_kdtree_search_multi(akd: PANNkdtree; idxs: PInteger; errs: PANNFloat; cnt: Integer; q: PANNFloat; eps: TANNFloat); external 'ANN.dll';
procedure ann_kdtree_pri_search_multi(akd: PANNkdtree; idxs: PInteger; errs: PANNFloat; cnt: Integer; q: PANNFloat; eps: TANNFloat); external 'ANN.dll';

procedure DEFAULT_FLANN_PARAMETERS; cdecl; external 'flann.dll';
function flann_build_index(dataset: PSingle; rows, cols: Integer; speedup: PSingle; flann_params: PFLANNParameters): flann_index_t; cdecl; external 'flann.dll';
function flann_free_index(index_id: flann_index_t; flann_params: PFLANNParameters): Integer; cdecl; external 'flann.dll';
function flann_find_nearest_neighbors_index(index_id: flann_index_t; testset: PSingle; trows: Integer; indices: PInteger; dists: PSingle; nn: Integer; flann_params: PFLANNParameters): Integer; cdecl; external 'flann.dll';
function flann_build_index_double(dataset: PDouble; rows, cols: Integer; speedup: PDouble; flann_params: PFLANNParameters): flann_index_t; cdecl; external 'flann.dll';
function flann_free_index_double(index_id: flann_index_t; flann_params: PFLANNParameters): Integer; cdecl; external 'flann.dll';
function flann_find_nearest_neighbors_index_double(index_id: flann_index_t; testset: PDouble; trows: Integer; indices: PInteger; dists: PDouble; nn: Integer; flann_params: PFLANNParameters): Integer; cdecl; external 'flann.dll';


function dl1quant(inbuf: PByte; width, height, quant_to, lookup_bpc: Integer; userpal: PDLUserPal): Integer; stdcall; external 'dlquant_dll.dll';
function dl3quant(inbuf: PByte; width, height, quant_to, lookup_bpc: Integer; userpal: PDLUserPal): Integer; stdcall; external 'dlquant_dll.dll';

function yakmo_create(k: Cardinal; restartCount: Cardinal; maxIter: Integer; initType: Integer; initSeed: Integer; doNormalize: Integer; isVerbose: Integer): PYakmo; stdcall; external 'yakmo.dll';
procedure yakmo_destroy(ay: PYakmo); stdcall; external 'yakmo.dll';
procedure yakmo_load_train_data(ay: PYakmo; rowCount: Cardinal; colCount: Cardinal; dataset: PPDouble); stdcall; external 'yakmo.dll';
procedure yakmo_train_on_data(ay: PYakmo; pointToCluster: PInteger); stdcall; external 'yakmo.dll';
procedure yakmo_get_centroids(ay: PYakmo; centroids: PPDouble); stdcall; external 'yakmo.dll';

function yakmo_single_create(k: Cardinal; restartCount: Cardinal; maxIter: Integer; initType: Integer; initSeed: Integer; doNormalize: Integer; isVerbose: Integer): PYakmoSingle; stdcall; external 'yakmo_single.dll' name 'yakmo_create';
procedure yakmo_single_destroy(ay: PYakmoSingle); stdcall; external 'yakmo_single.dll' name 'yakmo_destroy';
procedure yakmo_single_load_train_data(ay: PYakmoSingle; rowCount: Cardinal; colCount: Cardinal; dataset: PPANNFloat); stdcall; external 'yakmo_single.dll' name 'yakmo_load_train_data';
procedure yakmo_single_train_on_data(ay: PYakmoSingle; pointToCluster: PInteger); stdcall; external 'yakmo_single.dll' name 'yakmo_train_on_data';
procedure yakmo_single_get_centroids(ay: PYakmoSingle; centroids: PPANNFloat); stdcall; external 'yakmo_single.dll' name 'yakmo_get_centroids';

function birch_create(dist_threshold: TFloat; mem_limit: UInt64): PBIRCH; stdcall; external 'BIRCH.dll';
procedure birch_destroy(birch: PBIRCH); stdcall; external 'BIRCH.dll';
procedure birch_insert_line(birch: PBIRCH; line: PDouble); stdcall; external 'BIRCH.dll';
procedure birch_get_results(birch: PBIRCH; pointToCluster: PInteger); stdcall; external 'BIRCH.dll';

function bico_create(dimension, npoints, k, nrandproj, coresetsize: Int64; randomSeed: Integer): PBICO; stdcall; external 'BICO.dll';
procedure bico_destroy(bico: PBICO); stdcall; external 'BICO.dll';
procedure bico_set_num_threads(num_threads: Integer); stdcall; external 'BICO.dll';
procedure bico_insert_line(bico: PBICO; line: PDouble; weight: Double); stdcall; external 'BICO.dll';
function bico_get_results(bico: PBICO; centroids: PDouble; weights: PDouble): Int64; stdcall; external 'BICO.dll';

function NumberOfProcessors: Integer;
function HalfNumberOfProcessors: Integer;
function QuarterNumberOfProcessors: Integer;
function InvariantFormatSettings: TFormatSettings;
function internalRuncommand(p:TProcess;var outputstring:string;
                            var stderrstring:string; var exitstatus:integer; PrintOut: Boolean):integer;

const
  CRandomSeed = $42381337;

  CDefaultFLANNParameters: TFLANNParameters = (
      algorithm: FLANN_INDEX_KDTREE;
      checks: 32; eps: 0.0;
      sorted: 1; max_neighbors: -1; cores: 1;
      trees: 1; leaf_max_size: 32;
      branching: 32; iterations: 11; centers_init: FLANN_CENTERS_RANDOM; cb_index: 0.2;
      target_precision: 0.9; build_weight: 0.01; memory_weight: 0; sample_fraction: 0.1;
      table_number_: 0; key_size_: 0; multi_probe_level_: 0;
      log_level: FLANN_LOG_NONE; random_seed: CRandomSeed
  );

  //struct FLANNParameters DEFAULT_FLANN_PARAMETERS = {
  //    FLANN_INDEX_KDTREE,
  //    32, 0.0f,
  //    0, -1, 0,
  //    4, 4,
  //    32, 11, FLANN_CENTERS_RANDOM, 0.2f,
  //    0.9f, 0.01f, 0, 0.1f,
  //    FLANN_LOG_NONE, 0
  //};

implementation

var
  GTempAutoInc : Integer = 0;
  GInvariantFormatSettings: TFormatSettings;
  GNumberOfProcessors: Integer = 0;

const
  READ_BYTES = 65536; // not too small to avoid fragmentation when reading large files.

// helperfunction that does the bulk of the work.
// We need to also collect stderr output in order to avoid
// lock out if the stderr pipe is full.
function internalRuncommand(p:TProcess;var outputstring:string;
                            var stderrstring:string; var exitstatus:integer; PrintOut: Boolean):integer;
var
    numbytes,bytesread,available : integer;
    outputlength, stderrlength : integer;
    stderrnumbytes,stderrbytesread, PrintLastPos, prp : integer;
begin
  result:=-1;
  try
    try
    p.Options :=  [poUsePipes];
    bytesread:=0;
    outputlength:=0;
    stderrbytesread:=0;
    stderrlength:=0;
    PrintLastPos:=1;
    p.Execute;
    while p.Running do
      begin
        // Only call ReadFromStream if Data from corresponding stream
        // is already available, otherwise, on  linux, the read call
        // is blocking, and thus it is not possible to be sure to handle
        // big data amounts bboth on output and stderr pipes. PM.
        available:=P.Output.NumBytesAvailable;
        if  available > 0 then
          begin
            if (BytesRead + available > outputlength) then
              begin
                outputlength:=BytesRead + READ_BYTES;
                Setlength(outputstring,outputlength);
              end;
            NumBytes := p.Output.Read(outputstring[1+bytesread], available);

            // output to screen
            prp := Pos(#10, Copy(outputstring, PrintLastPos, bytesread - PrintLastPos + NumBytes));
            if PrintOut and (prp <> 0) then
            begin
              Write(Copy(outputstring, PrintLastPos, prp));
              PrintLastPos += prp;
            end;

            if NumBytes > 0 then
              Inc(BytesRead, NumBytes);
          end
        // The check for assigned(P.stderr) is mainly here so that
        // if we use poStderrToOutput in p.Options, we do not access invalid memory.
        else if assigned(P.stderr) and (P.StdErr.NumBytesAvailable > 0) then
          begin
            available:=P.StdErr.NumBytesAvailable;
            if (StderrBytesRead + available > stderrlength) then
              begin
                stderrlength:=StderrBytesRead + READ_BYTES;
                Setlength(stderrstring,stderrlength);
              end;
            StderrNumBytes := p.StdErr.Read(stderrstring[1+StderrBytesRead], available);

            if StderrNumBytes > 0 then
              Inc(StderrBytesRead, StderrNumBytes);
          end
        else
          Sleep(10);
      end;

    if PrintOut then
      Write(Copy(stderrstring, PrintLastPos, StderrBytesRead - PrintLastPos));

    // Get left output after end of execution
    available:=P.Output.NumBytesAvailable;
    while available > 0 do
      begin
        if (BytesRead + available > outputlength) then
          begin
            outputlength:=BytesRead + READ_BYTES;
            Setlength(outputstring,outputlength);
          end;
        NumBytes := p.Output.Read(outputstring[1+bytesread], available);
        if NumBytes > 0 then
          Inc(BytesRead, NumBytes);
        available:=P.Output.NumBytesAvailable;
      end;
    setlength(outputstring,BytesRead);
    while assigned(P.stderr) and (P.Stderr.NumBytesAvailable > 0) do
      begin
        available:=P.Stderr.NumBytesAvailable;
        if (StderrBytesRead + available > stderrlength) then
          begin
            stderrlength:=StderrBytesRead + READ_BYTES;
            Setlength(stderrstring,stderrlength);
          end;
        StderrNumBytes := p.StdErr.Read(stderrstring[1+StderrBytesRead], available);
        if StderrNumBytes > 0 then
          Inc(StderrBytesRead, StderrNumBytes);
      end;
    setlength(stderrstring,StderrBytesRead);
    exitstatus:=p.exitstatus;
    result:=0; // we came to here, document that.
    except
      on e : Exception do
         begin
           result:=1;
           setlength(outputstring,BytesRead);
         end;
     end;
  finally
    p.free;
  end;
end;

procedure QuickSort(var AData;AFirstItem,ALastItem:Int64;AItemSize:Integer;ACompareFunction:TCompareFunction;AUserParameter:Pointer=nil);
var I, J, P: Int64;
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

procedure LZCompress(ASourceStream: TStream; PrintProgress: Boolean; var ADestStream: TStream);
var
  Process: TProcess;
  RetCode: Integer;
  Output, ErrOut, SrcFN, DstFN: String;
  SrcStream, DstStream: TFileStream;
begin
  Process := TProcess.Create(nil);
  Process.CurrentDirectory := ExtractFilePath(ParamStr(0));
  Process.Executable := 'lzma.exe';

  SrcFN := GetTempFileName('', 'lz-' + IntToStr(GetCurrentThreadId) + '.dat');
  DstFN := ChangeFileExt(SrcFN, ExtractFileExt(SrcFN) + '.lzma');

  SrcStream := TFileStream.Create(SrcFN, fmCreate or fmShareDenyWrite);
  try
    ASourceStream.Seek(0, soBeginning);
    SrcStream.CopyFrom(ASourceStream, ASourceStream.Size);
  finally
    SrcStream.Free;
  end;

  Process.Parameters.Add('e "' + SrcFN + '" "' + DstFN + '" -lc8 -eos');
  Process.ShowWindow := swoHIDE;
  Process.Priority := ppIdle;

  RetCode := 0;
  internalRuncommand(Process, Output, ErrOut, RetCode, PrintProgress); // destroys Process

  DstStream := TFileStream.Create(DstFN, fmOpenRead or fmShareDenyWrite);
  try
    ADestStream.CopyFrom(DstStream, DstStream.Size);
  finally
    DstStream.Free;
  end;

  DeleteFile(PChar(SrcFN));
  DeleteFile(PChar(DstFN));
end;

procedure DoExternalSKLearn(Dataset: TByteDynArray2; ClusterCount, Precision: Integer; Compiled, PrintProgress: Boolean;
  var Clusters: TIntegerDynArray);
var
  i, j, st: Integer;
  InFN, Line, Output, ErrOut: String;
  SL, Shuffler: TStringList;
  Process: TProcess;
  OutputStream: TMemoryStream;
  pythonExe: array[0..MAX_PATH-1] of Char;
begin
  SL := TStringList.Create;
  Shuffler := TStringList.Create;
  OutputStream := TMemoryStream.Create;
  try
    for i := 0 to High(Dataset) do
    begin
      Line := '';
      for j := 0 to High(Dataset[0]) do
        Line := Line + IntToStr(Dataset[i, j]) + ' ';
      SL.Add(Line);
    end;

    InFN := GetTempFileName('', 'dataset-'+IntToStr(InterLockedIncrement(GTempAutoInc))+'.txt');
    SL.SaveToFile(InFN);
    SL.Clear;

    Process := TProcess.Create(nil);
    Process.CurrentDirectory := ExtractFilePath(ParamStr(0));

    if Compiled then
    begin
      Process.Executable := 'cluster.exe';
    end
    else
    begin
      if SearchPath(nil, 'python.exe', nil, MAX_PATH, pythonExe, nil) = 0 then
        pythonExe := 'python.exe';
      Process.Executable := pythonExe;
    end;

    for i := 0 to GetEnvironmentVariableCount - 1 do
      Process.Environment.Add(GetEnvironmentString(i));
    Process.Environment.Add('MKL_NUM_THREADS=1');
    Process.Environment.Add('NUMEXPR_NUM_THREADS=1');
    Process.Environment.Add('OMP_NUM_THREADS=1');

    if not Compiled then
      Process.Parameters.Add('cluster.py');
    Process.Parameters.Add('-i "' + InFN + '" -n ' + IntToStr(ClusterCount) + ' -t ' + FloatToStr(intpower(10.0, -Precision + 1)));
    if PrintProgress then
      Process.Parameters.Add('-d');
    Process.ShowWindow := swoHIDE;
    Process.Priority := ppIdle;

    st := 0;
    internalRuncommand(Process, Output, ErrOut, st, PrintProgress); // destroys Process
    Write(Output);
    Write(ErrOut);

    SL.LoadFromFile(InFN + '.membership');

    DeleteFile(PChar(InFN));
    DeleteFile(PChar(InFN + '.membership'));
    DeleteFile(PChar(InFN + '.cluster_centres'));

    SetLength(Clusters, SL.Count);
    for i := 0 to SL.Count - 1 do
    begin
      Line := SL[i];
      Clusters[i] := StrToIntDef(Line, -1);
    end;
  finally
    OutputStream.Free;
    Shuffler.Free;
    SL.Free;
  end;
end;

procedure DoExternalKMeans(Dataset: TFloatDynArray2; ClusterCount, ThreadCount: Integer; PrintProgress: Boolean;
  var Clusters: TIntegerDynArray);
var
  i, j, Clu, Inp, st: Integer;
  Line, Output, ErrOut, InFN: String;
  OutSL, Shuffler: TStringList;
  FS: TFileStream;
  Process: TProcess;
  OutputStream: TMemoryStream;
  pdi: PFloat;
  pfo: PSingle;
  fbuf: TSingleDynArray;
begin
  OutSL := TStringList.Create;
  Shuffler := TStringList.Create;
  OutputStream := TMemoryStream.Create;
  try
    InFN := GetTempFileName('', 'dataset-'+IntToStr(InterLockedIncrement(GTempAutoInc))+'.bin');
    FS := TFileStream.Create(InFN, fmCreate or fmShareDenyWrite);
    try
      FS.WriteDWord(Length(Dataset));
      FS.WriteDWord(Length(Dataset[0]));
      SetLength(fbuf, Length(Dataset[0]));
      for i := 0 to High(Dataset) do
      begin
        pdi := @Dataset[i, 0];
        pfo := @fbuf[0];
        for j := 0 to High(Dataset[0]) do
        begin
          pfo^ := pdi^;
          Inc(pdi);
          Inc(pfo);
        end;
        FS.Write(fbuf[0], Length(Dataset[0]) * SizeOf(pfo^));
      end;
    finally
      FS.Free;
    end;

    Process := TProcess.Create(nil);
    Process.CurrentDirectory := ExtractFilePath(ParamStr(0));
    Process.Executable := 'omp_main.exe';
    Process.Parameters.Add(' -b -i "' + InFN + '" -n ' + IntToStr(ClusterCount) + ' -t 0 ' + ifthen(ThreadCount > 0, ' -p ' + IntToStr(ThreadCount) + ' '));
    Process.ShowWindow := swoHIDE;
    Process.Priority := ppIdle;

    st := 0;
    internalRuncommand(Process, Output, ErrOut, st, PrintProgress); // destroys Process

    OutSL.LoadFromFile(InFN + '.membership');

    DeleteFile(PChar(InFN));
    DeleteFile(PChar(InFN + '.membership'));
    DeleteFile(PChar(InFN + '.cluster_centres'));

    for i := 0 to OutSL.Count - 1 do
    begin
      Line := OutSL[i];
      if TryStrToInt(Copy(Line, 1, Pos(' ', Line) - 1), Inp) and
          TryStrToInt(RightStr(Line, Pos(' ', ReverseString(Line)) - 1), Clu) then
        Clusters[Inp] := Clu;
    end;
  finally
    OutputStream.Free;
    Shuffler.Free;
    OutSL.Free;
  end;
end;

procedure GenerateSVMLightData(Dataset: TFloatDynArray2; Output: TStringList; Header: Boolean);
var
  i, j, cnt: Integer;
  Line: String;
begin
  Output.Clear;
  Output.LineBreak := sLineBreak;

  if Header then
  begin
    Output.Add('1 # m');
    Output.Add(IntToStr(Length(Dataset)) + ' # k');
    Output.Add(IntToStr(Length(Dataset[0])) + ' # number of features');
  end;

  for i := 0 to High(Dataset) do
  begin
    Line := Format('%d ', [i], GInvariantFormatSettings);

    cnt := Length(Dataset[i]);
    j := 0;

    while cnt > 16 do
    begin
      Line := Format('%s %d:%.12f %d:%.12f %d:%.12f %d:%.12f %d:%.12f %d:%.12f %d:%.12f %d:%.12f %d:%.12f %d:%.12f %d:%.12f %d:%.12f %d:%.12f %d:%.12f %d:%.12f %d:%.12f',
        [
          Line,
          j + 1,  Dataset[i, j + 0],  j + 2,  Dataset[i, j + 1],  j + 3,  Dataset[i, j + 2],  j + 4,  Dataset[i, j + 3],
          j + 5,  Dataset[i, j + 4],  j + 6,  Dataset[i, j + 5],  j + 7,  Dataset[i, j + 6],  j + 8,  Dataset[i, j + 7],
          j + 9,  Dataset[i, j + 8],  j + 10, Dataset[i, j + 9],  j + 11, Dataset[i, j + 10], j + 12, Dataset[i, j + 11],
          j + 13, Dataset[i, j + 12], j + 14, Dataset[i, j + 13], j + 15, Dataset[i, j + 14], j + 16, Dataset[i, j + 15]
        ],
        GInvariantFormatSettings);
      Dec(cnt, 16);
      Inc(j, 16);
    end;

    while cnt > 0 do
    begin
      Line := Format('%s %d:%.12f', [Line, j + 1,  Dataset[i, j]], GInvariantFormatSettings);
      Dec(cnt);
      Inc(j);
    end;

    Output.Add(Line);
  end;
end;

function GenerateSVMLightFile(Dataset: TFloatDynArray2; Header: Boolean): String;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    GenerateSVMLightData(Dataset, SL, Header);

    Result := GetTempFileName('', 'dataset-'+IntToStr(InterLockedIncrement(GTempAutoInc))+'.txt');

    SL.SaveToFile(Result);
  finally
    SL.Free;
  end;
end;

function GetLineInt(line: String): Integer;
begin
  Result := StrToInt(copy(line, 1, Pos(' ', line) - 1));
end;

function GetSVMLightLine(index: Integer; lines: TStringList): TFloatDynArray;
var
  i, p, np, clusterCount, restartCount: Integer;
  line, val, sc: String;

begin
  // TODO: so far, only compatible with YAKMO centroids

  restartCount := GetLineInt(lines[0]);
  clusterCount := GetLineInt(lines[1]);
  SetLength(Result, GetLineInt(lines[2]) + 1);

  Assert(InRange(index, 0, clusterCount - 1), 'wrong index!');

  line := lines[3 + clusterCount * (restartCount - 1) + index];
  for i := 0 to High(Result) do
  begin
    sc := ' ' + IntToStr(i) + ':';

    p := Pos(sc, line);
    if p = 0 then
    begin
      Result[i] := 0.0; //svmlight zero elimination
    end
    else
    begin
      p += Length(sc);

      np := PosEx(' ', line, p);
      if np = 0 then
        np := Length(line) + 1;
      val := Copy(line, p, np - p);

      //writeln(i, #9 ,index,#9,p,#9,np,#9, val);

      if Pos('nan', val) = 0 then
        Result[i] := StrToFloat(val, GInvariantFormatSettings)
      else
        Result[i] := abs(NaN); // Quiet NaN
    end;
  end;
end;

function GetSVMLightClusterCount(lines: TStringList): Integer;
begin
  Result := GetLineInt(lines[1]);
end;

function NumberOfProcessors: Integer;
begin
  Result := GNumberOfProcessors;
end;

function HalfNumberOfProcessors: Integer;
begin
  Result := max(1, GNumberOfProcessors div 2);
end;

function QuarterNumberOfProcessors: Integer;
begin
  Result := max(1, GNumberOfProcessors div 4);
end;

function InvariantFormatSettings: TFormatSettings;
begin
  Result := GInvariantFormatSettings;
end;

var
  SystemInfo: SYSTEM_INFO;
initialization
  GetLocaleFormatSettings(LOCALE_INVARIANT, GInvariantFormatSettings);
  GetSystemInfo(SystemInfo);
  GNumberOfProcessors := SystemInfo.dwNumberOfProcessors;
end.

