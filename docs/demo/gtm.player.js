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
// SkipBlock:		  data -> none; commandBits -> skip count - 1 (10 bits)
// ShortTileIdx:	  data -> tile index (16 bits); commandBits -> palette index (8 bits); V mirror (1 bit); H mirror (1 bit)
// LongTileIdx:		  data -> tile index (32 bits); commandBits -> palette index (8 bits); V mirror (1 bit); H mirror (1 bit)
// LoadPalette:		  data -> palette index (8 bits); palette format (8 bits) (0: RGBA32); RGBA bytes (32bits); commandBits -> none
// ShortBlendTileIdx: data -> tile index (16 bits); BlendY offset(4 bits); BlendX offset(4 bits); Previous frame factor (4 bits); Current frame factor (4 bits); commandBits -> palette index (8 bits); V mirror (1 bit); H mirror (1 bit)
// LongLendTileIdx:	  data -> tile index (32 bits); BlendY offset(4 bits); BlendX offset(4 bits); Previous frame factor (4 bits); Current frame factor (4 bits); commandBits -> palette index (8 bits); V mirror (1 bit); H mirror (1 bit)
//
// (insert new commands here...)
//
// FrameEnd:		  data -> none; commandBits bit 0 -> keyframe end
// TileSet:			  data -> start tile (32 bits); end tile (32 bits); { indexes per tile (64 bytes) } * count; commandBits -> indexes count per palette
// SetDimensions:	  data -> height in tiles (16 bits); width in tiles (16 bits); frame length in nanoseconds (32 bits) (2^32-1: still frame); tile count (32 bits); commandBits -> none
// ExtendedCommand:	  data -> custom commands, proprietary extensions, ...; commandBits -> extended command index (10 bits)
//
// ReservedArea:	  reserving the MSB for future use (do not use for new commands)

const GTMCommand = {
	'SkipBlock' : 0,
	'ShortTileIdx' : 1,
	'LongTileIdx' : 2,
	'LoadPalette' : 3,
	'ShortBlendTileIdx' : 4,
	'LongBlendTileIdx' : 5,
	
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

let gtmCanvasId = '';
let gtmInStream = null;
let gtmOutStream = null;
let gtmHeader = null;
let gtmLzmaDecoder = new LZMA.Decoder();
let gtmLzmaBytesPerSecond = 1024 * 1024;
let gtmTiles = null;
let gtmFrameData = null;
let gtmTMImageData = null;
let gtmPaletteR = new Array(512);
let gtmPaletteG = new Array(512);
let gtmPaletteB = new Array(512);
let gtmPaletteA = new Array(512);
let gtmTMIndexes = new Array(2);
let gtmTMAttrs = new Array(2);
let gtmFrameInterval = null;

let gtmAwaitingCanvasId = ''
let gtmAwaitingFile = null
let gtmAwaitingURL = null

let gtmReady = false;
let gtmPlaying = true;
let gtmDataPos = 0;
let gtmWidth = 0;
let gtmHeight = 0;
let gtmFrameLength = 0;
let gtmTileCount = 0;
let gtmPalSize = 0;
let gtmTMPos = 0;
let gtmTMDblBuff = 0;
let gtmKFCurDblBuff = 0;
let gtmKFPrevDblBuff = 0;
let gtmLoopCount = 0;

function gtmPlayFromFile(file, canvasId) {
	if (gtmReady) {
		gtmAwaitingCanvasId = canvasId
		gtmAwaitingFile = file
		gtmAwaitingURL = null
	} else {
		resetDecoding();

		gtmCanvasId = canvasId;
		
		var oReader = new FileReader();
		
		oReader.onload = function (oEvent) {
			gtmInStream = new LZMA.iStream(oReader.result);
			startFromReader();
		};
		
		oReader.readAsArrayBuffer(file);
	}
}

function gtmPlayFromURL(url, canvasId) {
	if (gtmReady) {
		gtmAwaitingCanvasId = canvasId
		gtmAwaitingFile = null
		gtmAwaitingURL = url
	} else {
		resetDecoding();

		gtmCanvasId = canvasId;
		
		var oReq = new XMLHttpRequest();
		oReq.open("GET", url, true);
		oReq.responseType = "arraybuffer";
		
		oReq.onload = function (oEvent) {
			gtmInStream = new LZMA.iStream(oReq.response);
			startFromReader();
		};
		
		oReq.send(null);
	}
}

function gtmSetPlaying(playing) {
	gtmPlaying = playing;
}

function resetDecoding() {
	if (gtmFrameInterval != null) {
		clearInterval(gtmFrameInterval);
		gtmFrameInterval = null;
	}
	
	gtmCanvasId = '';
	gtmInStream = null;
	gtmOutStream = null;
	gtmHeader = null;
	gtmLzmaDecoder = new LZMA.Decoder();
	gtmLzmaBytesPerSecond = 1024 * 1024;
	gtmTiles = null;
	gtmFrameData = null;

	gtmReady = false;
	gtmDataPos = 0;
	gtmFrameLength = 0;
	gtmTileCount = 0;
	gtmPalSize = 0;
	gtmTMPos = 0;
	gtmTMDblBuff = 0;
	gtmKFCurDblBuff = 0;
	gtmKFPrevDblBuff = 0;
	gtmLoopCount = 0;
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
		console.log('Header:', gtmHeader[GTMHeader.FramePixelWidth], 'x', gtmHeader[GTMHeader.FramePixelHeight], ',', Math.round(gtmHeader[GTMHeader.AverageBytesPerSec] * 8 / 1024), 'KBps');
		
		redimFrame();
	} else {
		gtmInStream.offset -= 4;
	}
}

function redimFrame() {
	var frame = document.getElementById(gtmCanvasId);
	
	if (frame.width != gtmWidth * CTileWidth || frame.height != gtmHeight * CTileWidth) {
		frame.width = gtmWidth * CTileWidth;
		frame.height = gtmHeight * CTileWidth;
		
		var canvas = frame.getContext('2d');
		canvas.fillStyle = 'black';
		canvas.fillRect(0, 0, gtmWidth * CTileWidth, gtmHeight * CTileWidth);

		gtmTMImageData = canvas.getImageData(0, 0, frame.width, frame.height);
		gtmTMIndexes[0] = new Uint32Array(gtmWidth * gtmHeight);
		gtmTMIndexes[1] = new Uint32Array(gtmWidth * gtmHeight);
		gtmTMAttrs[0] = new Uint16Array(gtmWidth * gtmHeight);
		gtmTMAttrs[1] = new Uint16Array(gtmWidth * gtmHeight);
	}
}

function unpackData() {
	if (gtmInStream.offset >= gtmInStream.size) {
		return;
	}
	
	let maxSize = Math.max(Math.round(gtmLzmaBytesPerSecond * gtmFrameLength / 1000),  65536); // 65536 -> low-res threshold
	
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
	let palIdx = (attrs >> 2) + 256 * gtmKFCurDblBuff;
	let tOff = (attrs & 3) * CTileWidth * CTileWidth;
	let tile = gtmTiles[idx];
	let palR = gtmPaletteR[palIdx];
	let palG = gtmPaletteG[palIdx];
	let palB = gtmPaletteB[palIdx];
	let palA = gtmPaletteA[palIdx];
	let x = (gtmTMPos % gtmWidth) * CTileWidth;
	let y = Math.trunc(gtmTMPos / gtmWidth) * CTileWidth;
	let p = (y * gtmWidth * CTileWidth + x) * 4;
	var data = gtmTMImageData.data;
	
	for (let ty = 0; ty < CTileWidth; ty++) {
		for (let tx = 0; tx < CTileWidth; tx++) {
			let v = tile[tOff++];
			data[p++] = palR[v]; 
			data[p++] = palG[v]; 
			data[p++] = palB[v]; 
			data[p++] = palA[v]; 
		}
		p += (gtmWidth - 1) * CTileWidth * 4;
	}
	
	gtmTMIndexes[gtmTMDblBuff][gtmTMPos] = idx;
	gtmTMAttrs[gtmTMDblBuff][gtmTMPos] = attrs;
	gtmTMPos++;
}

function drawBlendedTilemapItem(idx, attrs, blend) {
	let blendOff = (((blend >> 12) & 7) - ((blend >> 12) & 8)) * gtmWidth + (((blend >> 8) & 7) - ((blend >> 8) & 8));
	let blendPrev = (blend >> 4) & 15;
	let blendCur = blend & 15;
	
	let palIdx = (attrs >> 2) + 256 * gtmKFCurDblBuff;
	let tOff = (attrs & 3) * CTileWidth * CTileWidth;
	let tile = gtmTiles[idx];
	let palR = gtmPaletteR[palIdx];
	let palG = gtmPaletteG[palIdx];
	let palB = gtmPaletteB[palIdx];
	let palA = gtmPaletteA[palIdx];
	
	let prevAttrs = gtmTMAttrs[1 - gtmTMDblBuff][gtmTMPos + blendOff];
	let prevIdx = gtmTMIndexes[1 - gtmTMDblBuff][gtmTMPos + blendOff];
	
	let prevPalIdx = (prevAttrs >> 2)  + 256 * gtmKFPrevDblBuff;
	let prevTOff = (prevAttrs & 3) * CTileWidth * CTileWidth;
	let prevTile = gtmTiles[prevIdx];
	let prevPalR = gtmPaletteR[prevPalIdx];
	let prevPalG = gtmPaletteG[prevPalIdx];
	let prevPalB = gtmPaletteB[prevPalIdx];
	
	let x = (gtmTMPos % gtmWidth) * CTileWidth;
	let y = Math.trunc(gtmTMPos / gtmWidth) * CTileWidth;
	let p = (y * gtmWidth * CTileWidth + x) * 4;
	
	var data = gtmTMImageData.data;
	
	for (let ty = 0; ty < CTileWidth; ty++) {
		for (let tx = 0; tx < CTileWidth; tx++) {
			let pv = prevTile[prevTOff++];
			let cv = tile[tOff++];
			data[p++] = Math.min(255, ((palR[cv] * blendCur + prevPalR[pv] * blendPrev) / 15) >> 0);
			data[p++] = Math.min(255, ((palG[cv] * blendCur + prevPalG[pv] * blendPrev) / 15) >> 0);
			data[p++] = Math.min(255, ((palB[cv] * blendCur + prevPalB[pv] * blendPrev) / 15) >> 0);
			data[p++] = palA[cv];
		}
		p += (gtmWidth - 1) * CTileWidth * 4;
	}
	
	gtmTMIndexes[gtmTMDblBuff][gtmTMPos] = idx;
	gtmTMAttrs[gtmTMDblBuff][gtmTMPos] = attrs;
	gtmTMPos++;
}

function skipBlock(skipCount) {
	for(let s = 0; s < skipCount; s++)
	{
		gtmTMIndexes[gtmTMDblBuff][gtmTMPos] = gtmTMIndexes[1 - gtmTMDblBuff][gtmTMPos];
		gtmTMAttrs[gtmTMDblBuff][gtmTMPos] = gtmTMAttrs[1 - gtmTMDblBuff][gtmTMPos];
		gtmTMPos++;
	}
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
	return [v & ((1 << CShortIdxBits) - 1), v >> CShortIdxBits];
}

function decodeFrame() {
	gtmReady |= gtmDataPos < gtmFrameData.length;
	
	if (gtmReady && gtmPlaying) {
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
				console.log('TileCount:', gtmTileCount);
				
				if (gtmLoopCount <= 0) {
					gtmFrameInterval = setInterval(decodeFrame, gtmFrameLength);
					gtmTiles = new Array(gtmTileCount);
					redimFrame();
				}
				break;
				
			case GTMCommand.TileSet:
				let tstart = readDWord();
				let tend = readDWord();
				gtmPalSize = cmd[1];
				
				for (let p = tstart; p <= tend; p++) {
					gtmTiles[p] = new Uint8Array(CTileWidth * CTileWidth * 4);
					let tile = gtmTiles[p];
					let offh = CTileWidth * CTileWidth * 1;
					let offv = CTileWidth * CTileWidth * 2;
					let offhv = CTileWidth * CTileWidth * 3;
					
					for (let ty = 0; ty < CTileWidth; ty++) {
						for (let tx = 0; tx < CTileWidth; tx++) {
							let b = readByte();
							tile[ty * CTileWidth + tx] = b;
							tile[offh + ty * CTileWidth + (CTileWidth - 1 - tx)] = b;
							tile[offv + (CTileWidth - 1 - ty) * CTileWidth + tx] = b;
							tile[offhv + (CTileWidth - 1 - ty) * CTileWidth + (CTileWidth - 1 - tx)] = b;
						}
					}
				}
				break;
				
			case GTMCommand.FrameEnd:
				if (gtmTMPos != gtmWidth * gtmHeight) {
					console.error('Incomplete tilemap ' + gtmTMPos + ' <> ' + gtmWidth * gtmHeight + '\n');
				}
				gtmTMPos = 0;

				gtmTMDblBuff = 1 - gtmTMDblBuff;
				gtmKFPrevDblBuff = gtmKFCurDblBuff;
				if (cmd[1] & 1) // keyframe?
				{
					gtmKFCurDblBuff = 1 - gtmKFCurDblBuff;
				}
				doContinue = false;
				break;
				
			case GTMCommand.SkipBlock:
				skipBlock(cmd[1] + 1);
				break;
				
			case GTMCommand.ShortTileIdx:
				drawTilemapItem(readWord(), cmd[1]);
				break;
				
			case GTMCommand.LongTileIdx:
				drawTilemapItem(readDWord(), cmd[1]);
				break;
				
			case GTMCommand.LoadPalette:
				let palIdx = readByte() + 256 * gtmKFCurDblBuff;
				readByte(); // palette format
				
				gtmPaletteR[palIdx] = new Uint8Array(gtmPalSize);
				gtmPaletteG[palIdx] = new Uint8Array(gtmPalSize);
				gtmPaletteB[palIdx] = new Uint8Array(gtmPalSize);
				gtmPaletteA[palIdx] = new Uint8Array(gtmPalSize);
				
				for (let i = 0; i < gtmPalSize; i++) {
					gtmPaletteR[palIdx][i] = readByte();
					gtmPaletteG[palIdx][i] = readByte();
					gtmPaletteB[palIdx][i] = readByte();
					gtmPaletteA[palIdx][i] = readByte();
				}
				break;
				
			case GTMCommand.ShortBlendTileIdx:
				let idxW = readWord()
				let blendW = readWord()
				drawBlendedTilemapItem(idxW, cmd[1], blendW);
				break;
				
			case GTMCommand.LongBlendTileIdx:
				let idxDW = readDWord()
				let blendDW = readWord()
				drawBlendedTilemapItem(idxDW, cmd[1], blendDW);
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
	
	if(gtmAwaitingFile != null || gtmAwaitingURL != null) {
		console.log('Processing awaiting video...');

		gtmReady = false;
		if (gtmAwaitingFile != null)
		{
			gtmPlayFromFile(gtmAwaitingFile, gtmAwaitingCanvasId)
		} else if (gtmAwaitingURL != null) {
			gtmPlayFromURL(gtmAwaitingURL, gtmAwaitingCanvasId)
		}

		gtmAwaitingCanvasId = ''
		gtmAwaitingFile = null
		gtmAwaitingURL = null
	}
}
