"use strict";

const GTMCommand = { // commandBits -> palette index (8 bits); V mirror (1 bit); H mirror (1 bit)
    'SkipBlock' : 0, // commandBits -> skip count - 1 (10 bits)
    'ShortTileIdx' : 1, // data -> tile index (16 bits)
    'LongTileIdx' : 2, // data -> tile index (32 bits)
    'LoadPalette' : 3, // data -> palette index (8 bits); palette format (8 bits) (00: RGBA32); RGBA bytes (32bits)
    // new commands here
    'FrameEnd' : 28, // commandBits bit 0 -> keyframe end
    'TileSet' : 29, // data -> start tile (32 bits); end tile (32 bits); { indexes per tile (64 bytes) } * count; commandBits -> indexes count per palette
    'SetDimensions' : 30, // data -> height in tiles (16 bits); width in tiles (16 bits); frame length in nanoseconds (32 bits); tile count (32 bits);
    'ExtendedCommand' : 31, // data -> custom commands, proprietary extensions, ...; commandBits -> extended command index (10 bits)

    'ReservedAreaBegin' : 32, // reserving the MSB for future use
    'ReservedAreaEnd' : 63
}; 

const CTileWidth = 8;
const CTMAttrBits = 1 + 1 + 8; // HMir + VMir + PalIdx
const CShortIdxBits = 16 - CTMAttrBits;

var gtmCanvasId = '';
var gtmReader = null;
var gtmInStream = null;
var gtmOutStream = null;
var gtmLzmaDecoder = new LZMA.Decoder();
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
  gtmReader = new FileReader();
  gtmReader.addEventListener('load', (e) => {
    gtmInStream = new LZMA.iStream(gtmReader.result);
    startFromReader();
  });
  gtmReader.readAsArrayBuffer(file);
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
  gtmOutStream = new LZMA.oStream();
  LZMA.decodeMaxSize(gtmLzmaDecoder, gtmInStream, gtmOutStream, Infinity);
  gtmFrameData = gtmOutStream.toUint8Array();
  
  if (!gtmReady) {
    gtmDataPos = 0;
    gtmReady = true;
    setTimeout(decodeFrame, 10);
  }
}

function redimFrame() {
  var frame = document.getElementById(gtmCanvasId);
  frame.width = gtmWidth * CTileWidth;
  frame.height = gtmHeight * CTileWidth;
  var canvas = frame.getContext('2d');
  canvas.fillStyle = 'black';
  canvas.fillRect(0, 0, gtmWidth * CTileWidth, gtmHeight * CTileWidth);

  gtmTMImageData = canvas.getImageData(0, 0, frame.width, frame.height);
  gtmTiles = new Array(gtmTileCount);

  setInterval(decodeFrame, gtmFrameLength);
}

function unpackData() {
  if (gtmInStream.offset >= gtmInStream.size) {
    return;
  }
  
  let res = LZMA.decodeMaxSize(gtmLzmaDecoder, gtmInStream, gtmOutStream, Math.round((2048 * 1024) / (1000 / gtmFrameLength)));

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
