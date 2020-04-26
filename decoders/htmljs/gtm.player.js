"use strict";

const GTMCommand = { // commandBits: palette # bit count + 2 (H/V mirrors)
  'LongTileIdx' : 0, // data -> tile index (24 bits); palette index (8 bits)
  'LoadPalette' : 1, // data -> palette index (8 bits); palette format (8 bits) (00: RGBA32); RGBA bytes (32bits)
  // new commands here
  'FrameEnd' : 28, // commandBits bit 0 -> keyframe end
  'Tileset' : 29, // data -> start tile (32 bits); end tile (32 bits); indexes per tile (64 bytes); commandBits -> indexes bit count
  'SetDimensions' : 30, // data -> height in tiles (16 bits); width in tiles (16 bits); frame length in nanoseconds (32 bits); tile count (32 bits);
  'ExtendedCommand' : 31, // data -> custom commands, proprietary extensions, ...; commandBits -> extended command index
  
  'ShortTileIdxStart' : 32, // short tile index #0 ...; commandBits: palette index (3 bits); V mirror (1 bit); H mirror (1 bit)
  'ShortTileIdxEnd' : 1919, // ... short tile index #1887
  
  'SkipBlockStart' : 1920, // skipping 128 tiles ...
  'SkipBlockEnd' : 2047, // ... skipping 1 tile
};     

const CTileWidth = 8;
const CTMAttrBits = 1 + 1 + 3; // HMir + VMir + PalIdx
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
    gtmOutStream = new LZMA.oStream();
    gtmOutStream = LZMA.decodeMaxSize(gtmLzmaDecoder, gtmInStream, gtmOutStream, Infinity);
    gtmFrameData = gtmOutStream.toUint8Array();

    if (!gtmReady) {
      gtmDataPos = 0;
      gtmReady = true;
      drop.parentNode.removeChild(drop);
      setTimeout(decodeFrame, 10);
    }
  });

  gtmReader.readAsArrayBuffer(file);
}

function gtmSetPlaying(playing) {
  gtmPlaying = playing;
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
  
  let res = LZMA.decodeMaxSize(gtmLzmaDecoder, gtmInStream, gtmOutStream, Math.round((4096 * 1024) / (1000 / gtmFrameLength)));

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
  let tile = gtmTiles[idx];
  let palR = gtmPaletteR[attrs >>> 2];
  let palG = gtmPaletteG[attrs >>> 2];
  let palB = gtmPaletteB[attrs >>> 2];
  let palA = gtmPaletteA[attrs >>> 2];
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
          
        case GTMCommand.Tileset:
          let start = readDWord();
          let end = readDWord();
          gtmPalSize = 1 << cmd[1];
          
          for (let p = start; p <= end; p++) {
            gtmTiles[p] = new Array(CTileWidth * CTileWidth);
            for (let i = 0; i < CTileWidth * CTileWidth; i++) {
              gtmTiles[p][i] = readByte();
            }
          }
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
          
        case GTMCommand.LongTileIdx:
	  let v = readDWord();
          drawTilemapItem(v & 0x00ffffff, (cmd[1] & 3) | ((v >> 22) & 0x3fc))
          break;
          
        case GTMCommand.FrameEnd:
          if (gtmTMPos != gtmWidth * gtmHeight) {
            console.error('Incomplete tilemap ' + gtmTMPos + ' <> ' + gtmWidth * gtmHeight + '\n');
          }
          gtmTMPos = 0;
          doContinue = false;
          break;
          
        default:
          if (cmd[0] >= GTMCommand.ShortTileIdxStart && cmd[0] <= GTMCommand.ShortTileIdxEnd) {
            drawTilemapItem(cmd[0] - GTMCommand.ShortTileIdxStart, cmd[1])
          } else if (cmd[0] >= GTMCommand.SkipBlockStart && cmd[0] <= GTMCommand.SkipBlockEnd) {
            gtmTMPos += GTMCommand.SkipBlockEnd - cmd[0] + 1;
          } else {
            console.error('Undecoded command @' + gtmDataPos + ': ' + cmd + '\n');
          }
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
