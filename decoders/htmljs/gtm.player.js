"use strict";

const GTMHeader = {
  'FourCC' : 0, // ASCII "GTMv"
  'RIFFSize' : 1,
  'WholeHeaderSize' : 2, // including TGTMKeyFrameInfo and all
  'EncoderVersion' : 3,
  'FramePixelWidth' : 4,
  'FramePixelHeight' : 5,
  'KFCount' : 6,
  'FrameCount' : 7,
  'AverageBytesPerSec' : 8,
  'KFMaxBytesPerSec' : 9
}; 

// Commands Description:
// =====================
//
// SkipBlock:        data -> none; commandBits -> skip count - 1 (10 bits)
// ShortTileIdx:     data -> tile index (16 bits); commandBits -> palette index (8 bits); V mirror (1 bit); H mirror (1 bit)
// LongTileIdx:      data -> tile index (32 bits); commandBits -> palette index (8 bits); V mirror (1 bit); H mirror (1 bit)
// LoadPalette:      data -> palette index (8 bits); palette format (8 bits) (0: RGBA32); RGBA bytes (32bits); commandBits -> none
//
// (insert new commands here...)
//
// FrameEnd:         data -> none; commandBits bit 0 -> keyframe end
// TileSet:          data -> start tile (32 bits); end tile (32 bits); { indexes per tile (64 bytes) } * count; commandBits -> indexes count per palette
// SetDimensions:    data -> height in tiles (16 bits); width in tiles (16 bits); frame length in nanoseconds (32 bits) (2^32-1: still frame); tile count (32 bits); commandBits -> none
// ExtendedCommand:  data -> custom commands, proprietary extensions, ...; commandBits -> extended command index (10 bits)
//
// ReservedArea:     reserving the MSB for future use (do not use for new commands)

const GTMCommand = {
  'SkipBlock' : 0,
  'ShortTileIdx' : 1,
  'LongTileIdx' : 2,
  'LoadPalette' : 3,

  'FrameEnd' : 28,
  'TileSet' : 29,
  'SetDimensions' : 30,
  'ExtendedCommand' : 31,

  'ReservedAreaBegin' : 32,
  'ReservedAreaEnd' : 63
}; 

const CTileWidth = 8;
const CTMAttrBits = 1 + 1 + 8; // HMir + VMir + PalIdx
const CShortIdxBits = 16 - CTMAttrBits;

var gtmCanvasId = '';
var gtmInStream = null;
var gtmOutStream = null;
var gtmHeader = null;
var gtmLzmaDecoder = new LZMA.Decoder();
var gtmLzmaBytesPerSecond = 1024 * 1024;
var gtmFrameData = null;
var gtmTMImageData = null;
var gtmPaletteR = new Array(256);
var gtmPaletteG = new Array(256);
var gtmPaletteB = new Array(256);
var gtmPaletteA = new Array(256);
var gtmReady = false;
var gtmPlaying = true;
var gtmDataPos = 0;
var gtmWidth = 0;
var gtmHeight = 0;
var gtmFrameLength = 0;
var gtmTiles = null;
var gtmTileCount = 0;
var gtmPalSize = 0;
var gtmTMPos = 0;
var gtmLoopCount = 0;

function gtmPlayFromFile(file, canvasId) {
  gtmCanvasId = canvasId;
  gtmReady = false;
  
  var oReader = new FileReader();
  
  oReader.onload = function (oEvent) {
    gtmInStream = new LZMA.iStream(oReader.result);
    startFromReader();
  };
  
  oReader.readAsArrayBuffer(file);
}

function gtmPlayFromURL(url, canvasId) {
  gtmCanvasId = canvasId;
  gtmReady = false;

  var oReq = new XMLHttpRequest();
  oReq.open("GET", url, true);
  oReq.responseType = "arraybuffer";
  
  oReq.onload = function (oEvent) {
    gtmInStream = new LZMA.iStream(oReq.response);
    startFromReader();
  };
  
  oReq.send(null);
}

function gtmSetPlaying(playing) {
  gtmPlaying = playing;
}

function startFromReader() {
  parseHeader();
  
  gtmOutStream = new LZMA.oStream();
  LZMA.decodeMaxSize(gtmLzmaDecoder, gtmInStream, gtmOutStream, Infinity);
  gtmFrameData = gtmOutStream.toUint8Array();
  
  if (!gtmReady) {
    gtmDataPos = 0;
    gtmReady = true;
    setTimeout(decodeFrame, 10);
  }
}

function getHeaderDWord () {
  let v = gtmInStream.readByte();
  v |= gtmInStream.readByte() << 8;
  v |= gtmInStream.readByte() << 16;
  v |= gtmInStream.readByte() << 24;
  return v;
}

function parseHeader() {
  let fcc = getHeaderDWord();
  
  if (fcc == 0x764D5447) { // "GTMv"; file header
    let hdrsize = getHeaderDWord();
    let whlsize = getHeaderDWord();
    
    gtmHeader = new Array(whlsize >>> 2);
    gtmHeader[GTMHeader.FourCC] = fcc;
    gtmHeader[GTMHeader.RIFFSize] = hdrsize;
    gtmHeader[GTMHeader.WholeHeaderSize] = whlsize;
    for (let p = GTMHeader.WholeHeaderSize + 1; p < whlsize >>> 2; p++) {
      gtmHeader[p] = getHeaderDWord();
    }
    
    gtmWidth = (gtmHeader[GTMHeader.FramePixelWidth] / CTileWidth) >>> 0;
    gtmHeight = (gtmHeader[GTMHeader.FramePixelHeight] / CTileWidth) >>> 0;
    gtmLzmaBytesPerSecond = gtmHeader[GTMHeader.KFMaxBytesPerSec];
    console.log('Header:', gtmHeader[GTMHeader.FramePixelWidth], 'x', gtmHeader[GTMHeader.FramePixelHeight], ',', gtmLzmaBytesPerSecond * 8 / 1024, 'MBps');
    
    redimFrame();
  } else {
    gtmInStream.offset -= 4;
  }
}

function redimFrame() {
  var frame = document.getElementById(gtmCanvasId);
  
  if (frame.width != gtmWidth * CTileWidth || frame.height != gtmHeight * CTileWidth)
  {
    frame.width = gtmWidth * CTileWidth;
    frame.height = gtmHeight * CTileWidth;
    
    var canvas = frame.getContext('2d');
    canvas.fillStyle = 'black';
    canvas.fillRect(0, 0, gtmWidth * CTileWidth, gtmHeight * CTileWidth);

    gtmTMImageData = canvas.getImageData(0, 0, frame.width, frame.height);
  }
}

function unpackData() {
  if (gtmInStream.offset >= gtmInStream.size) {
    return;
  }
  
  let maxSize = Math.round(gtmLzmaBytesPerSecond / (1000 / gtmFrameLength));
  
  let res = LZMA.decodeMaxSize(gtmLzmaDecoder, gtmInStream, gtmOutStream, maxSize);

  if (res != null) {
    gtmOutStream = res;
    gtmFrameData = gtmOutStream.toUint8Array();
  }
}

function renderEnd() {
  if (gtmWidth * gtmHeight == 0) {
    return;
  }
  
  var frame = document.getElementById(gtmCanvasId);
  var canvas = frame.getContext('2d');
  canvas.putImageData(gtmTMImageData, 0, 0);
}

function drawTilemapItem(idx, attrs) {
  let palIdx = attrs >>> 2;
  let tile = gtmTiles[idx];
  let palR = gtmPaletteR[palIdx];
  let palG = gtmPaletteG[palIdx];
  let palB = gtmPaletteB[palIdx];
  let palA = gtmPaletteA[palIdx];
  let x = (gtmTMPos % gtmWidth) * CTileWidth;
  let y = Math.trunc(gtmTMPos / gtmWidth) * CTileWidth;
  let p = (y * gtmWidth * CTileWidth + x) * 4;
  var data = gtmTMImageData.data
  
  if (attrs & 1)
  {
    if (attrs & 2)
    {
      // HV mirrored
      for (let ty = CTileWidth - 1; ty >= 0; ty--) {
        for (let tx = CTileWidth - 1; tx >= 0; tx--) {
          let v = tile[tx + CTileWidth * ty];
          data[p++] = palR[v]; 
          data[p++] = palG[v]; 
          data[p++] = palB[v]; 
          data[p++] = palA[v]; 
        }
        p += (gtmWidth - 1) * CTileWidth * 4;
      }
    } else {
      // H mirrored
      for (let ty = 0; ty < CTileWidth; ty++) {
        for (let tx = CTileWidth - 1; tx >= 0; tx--) {
          let v = tile[tx + CTileWidth * ty];
          data[p++] = palR[v]; 
          data[p++] = palG[v]; 
          data[p++] = palB[v]; 
          data[p++] = palA[v]; 
        }
        p += (gtmWidth - 1) * CTileWidth * 4;
      }
    }
  } else {
    if (attrs & 2)
    {
      // V mirrored
      for (let ty = CTileWidth - 1; ty >= 0; ty--) {
        for (let tx = 0; tx < CTileWidth; tx++) {
          let v = tile[tx + CTileWidth * ty];
          data[p++] = palR[v]; 
          data[p++] = palG[v]; 
          data[p++] = palB[v]; 
          data[p++] = palA[v]; 
        }
        p += (gtmWidth - 1) * CTileWidth * 4;
      }
    } else {
      // standard
      for (let ty = 0; ty < CTileWidth; ty++) {
        for (let tx = 0; tx < CTileWidth; tx++) {
          let v = tile[tx + CTileWidth * ty];
          data[p++] = palR[v]; 
          data[p++] = palG[v]; 
          data[p++] = palB[v]; 
          data[p++] = palA[v]; 
        }
        p += (gtmWidth - 1) * CTileWidth * 4;
      }
    }
  }
  gtmTMPos++;
}

function readByte() {
  return gtmFrameData[gtmDataPos++];
}

function readWord() {
  let v = readByte();
  v |= readByte() << 8;
  return v;
}

function readDWord() {
  let v = readWord();
  v |= readWord() << 16;
  return v;
}

function readCommand() {
  let v = readWord();
  return [v & ((1 << CShortIdxBits) - 1), v >>> CShortIdxBits];
}

function decodeFrame() {
  gtmReady |= gtmDataPos < gtmFrameData.length;
  
  if (gtmReady && gtmPlaying)
  {
    renderEnd();
    
    let doContinue = true;
    do {
      let cmd = readCommand();
      
      switch (cmd[0]) {
        case GTMCommand.SetDimensions:
          gtmWidth = readWord();
          gtmHeight = readWord();
          gtmFrameLength = Math.round(readDWord() / (1000 * 1000));
          gtmTileCount = readDWord();
          
          if (gtmLoopCount <= 0) {
            setInterval(decodeFrame, gtmFrameLength);
            gtmTiles = new Array(gtmTileCount);
            redimFrame();
          }
          break;
          
        case GTMCommand.TileSet:
          let tstart = readDWord();
          let tend = readDWord();
          gtmPalSize = cmd[1];
          
          for (let p = tstart; p <= tend; p++) {
            gtmTiles[p] = new Array(CTileWidth * CTileWidth);
            for (let i = 0; i < CTileWidth * CTileWidth; i++) {
              gtmTiles[p][i] = readByte();
            }
          }
          break;
        
        case GTMCommand.FrameEnd:
          if (gtmTMPos != gtmWidth * gtmHeight) {
            console.error('Incomplete tilemap ' + gtmTMPos + ' <> ' + gtmWidth * gtmHeight + '\n');
          }
          gtmTMPos = 0;
          doContinue = false;
          break;
          
        case GTMCommand.SkipBlock:
          gtmTMPos += cmd[1] + 1;
          break;
          
        case GTMCommand.ShortTileIdx:
          drawTilemapItem(readWord(), cmd[1]);
          break;
          
        case GTMCommand.LongTileIdx:
          drawTilemapItem(readDWord(), cmd[1]);
          break;
          
        case GTMCommand.LoadPalette:
          let palIdx = readByte();
          readByte(); // palette format
          gtmPaletteR[palIdx] = new Array(gtmPalSize);
          gtmPaletteG[palIdx] = new Array(gtmPalSize);
          gtmPaletteB[palIdx] = new Array(gtmPalSize);
          gtmPaletteA[palIdx] = new Array(gtmPalSize);
          for (let i = 0; i < gtmPalSize; i++) {
            gtmPaletteR[palIdx][i] = readByte();
            gtmPaletteG[palIdx][i] = readByte();
            gtmPaletteB[palIdx][i] = readByte();
            gtmPaletteA[palIdx][i] = readByte();
          }
          break;
          
        default:
          console.error('Undecoded command @' + gtmDataPos + ': ' + cmd + '\n');
          break;
      }
      
      gtmReady = (gtmDataPos < gtmFrameData.length);
    } while (doContinue && gtmReady);
    
    if (!doContinue && !gtmReady && gtmInStream.offset >= gtmInStream.size) {
      gtmDataPos = 0;
      gtmLoopCount++;
      gtmReady = true;
    }
  }
  
  unpackData();
}
