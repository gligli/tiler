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
// PredictedTileShortOffsets:        data -> none; commandBits -> y offset (6 bits); x offset (6 bits)
// PredictedTileLongOffsets:         data -> x offset (8 bits); y offset (8 bits); commandBits -> none
// ShortTileIdxShortPalIdx:          data -> tile index (16 bits); commandBits -> palette index (10 bits); V mirror (1 bit); H mirror (1 bit)
// LongTileIdxShortPalIdx:           data -> tile index (32 bits); commandBits -> palette index (10 bits); V mirror (1 bit); H mirror (1 bit)
// LongTileIdxLongPalIdx:            data -> palette index (16 bits); tile index (32 bits); commandBits -> none (10 bits); V mirror (1 bit); H mirror (1 bit)
// IntraTile:                        data -> palette index (16 bits); indexes per pixel (64 bytes); commandBits -> none (10 bits); V mirror (1 bit); H mirror (1 bit)
// SkipBlock:                        data -> none; commandBits -> skip count - 1 (12 bits)
//
// (insert new commands here...)
//
// FrameEnd:                         data -> none; commandBits -> none (11 bits); keyframe end (1 bit)
// LoadPalette:                      data -> palette index (16 bits); { RGBA bytes (32bits) } * indexes count; commandBits -> palette format (0: RGBA32) (12 bits)
// TileSet:                          data -> start tile (32 bits); end tile (32 bits); { indexes per pixel (64 bytes) } * count; commandBits -> indexes count per palette
// SetDimensions:                    data -> width in tiles (16 bits); height in tiles (16 bits); frame length in nanoseconds (32 bits) (2^32-1: still frame); tile count (32 bits); commandBits -> none
// ExtendedCommand:                  data -> following bytes count (32 bits); custom commands, proprietary extensions, ...; commandBits -> extended command index (12 bits)

const GTMCommand = {
    'PredictedTileShortOffsets' : 0,
    'PredictedTileLongOffsets' : 1,
    'ShortTileIdxShortPalIdx' : 2,
    'LongTileIdxShortPalIdx' : 3,
    'LongTileIdxLongPalIdx' : 4,
    'IntraTile' : 5,
    'SkipBlock' : 6,
	
    'FrameEnd' : 11,
    'LoadPalette' : 12,
    'TileSet' : 13,
    'SetDimensions' : 14,
    'ExtendedCommand' : 15,
}; 

const CTileWidth = 8;
const CTMAttrBits = 1 + 1 + 10; // HMir + VMir + PalIdx
const CShortIdxBits = 16 - CTMAttrBits;
const CTileSize = CTileWidth * CTileWidth;
const CMaxPaletteCount = 65536;

let gtmWLZMA = null;

let gtmCanvasId = '';
let gtmInBuffer = null;
let gtmOutStream = null;
let gtmHeader = null;
let gtmTiles = null;
let gtmPaletteR = new Array(CMaxPaletteCount);
let gtmPaletteG = new Array(CMaxPaletteCount);
let gtmPaletteB = new Array(CMaxPaletteCount);
let gtmPaletteA = new Array(CMaxPaletteCount);
let gtmTMImageData = new Array(2);
let gtmFrameInterval = null;

let gtmAwaitingCanvasId = ''
let gtmAwaitingFile = null
let gtmAwaitingURL = null

let gtmReady = false;
let gtmPlaying = true;
let gtmUnpackingFinished = false;
let gtmDataBufIdx = 0;
let gtmDataBufPos = 0;
let gtmDataBufGlobalPos = 0;
let gtmWidth = 0;
let gtmHeight = 0;
let gtmFrameLength = 0;
let gtmTileCount = 0;
let gtmCurIntraTile = 0;
let gtmPalSize = 0;
let gtmTMPos = 0;
let gtmTMDblBuff = 0;
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
			startFromReader(oReader.result);
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
			startFromReader(oReq.response);
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
	gtmWLZMA = new WLZMA.Manager(1, URL.createObjectURL(new Blob(["("+worker_function.toString()+")(\""+ document.URL +"\")"], {type: 'text/javascript'})));
	
	gtmCanvasId = '';
	gtmInBuffer = null;
	gtmOutStream = null;
	gtmHeader = null;
	gtmTiles = null;
	
	gtmReady = false;
	gtmUnpackingFinished = false;
	gtmDataBufIdx = 0;
	gtmDataBufPos = 0;
	gtmDataBufGlobalPos = 0;
	gtmFrameLength = 0;
	gtmTileCount = 0;
	gtmCurIntraTile = 0;
	gtmPalSize = 0;
	gtmTMPos = 0;
	gtmTMDblBuff = 0;
	gtmLoopCount = 0;
}

function startFromReader(buffer) {
	gtmInBuffer = parseHeader(buffer);
	gtmOutStream = new LZMA.oStream();

	// invoke asynchronous decoding of the first keyframe (EOS terminated)
	gtmWLZMA.decode(gtmInBuffer)
	.then(function (outStream) {
		unpackNextKeyframe();
		
		if (!gtmReady) {
			gtmDataBufIdx = 0;
			gtmDataBufPos = 0;
			gtmDataBufGlobalPos = 0;
			gtmReady = true;
			setTimeout(decodeFrame, 10);
		}

	})
	.catch(function(err) {
		throw new Error(err.message);
	})	
}

function getHeaderDWord(stream) {
	let v = stream.readByte();
	v |= stream.readByte() << 8;
	v |= stream.readByte() << 16;
	v |= stream.readByte() << 24;
	return v;
}

function parseHeader(buffer) {
	let stream = new LZMA.iStream(buffer)
	
	let fcc = getHeaderDWord(stream);
	
	if (fcc == 0x764D5447) { // "GTMv"; file header
		let hdrsize = getHeaderDWord(stream);
		let whlsize = getHeaderDWord(stream);
		
		gtmHeader = new Array(whlsize >>> 2);
		gtmHeader[GTMHeader.FourCC] = fcc;
		gtmHeader[GTMHeader.RIFFSize] = hdrsize;
		gtmHeader[GTMHeader.WholeHeaderSize] = whlsize;
		for (let p = GTMHeader.WholeHeaderSize + 1; p < whlsize >>> 2; p++) {
			gtmHeader[p] = getHeaderDWord(stream);
		}
		
		gtmWidth = (gtmHeader[GTMHeader.FramePixelWidth] / CTileWidth) >>> 0;
		gtmHeight = (gtmHeader[GTMHeader.FramePixelHeight] / CTileWidth) >>> 0;
		console.log('Header:', gtmHeader[GTMHeader.FramePixelWidth], 'x', gtmHeader[GTMHeader.FramePixelHeight], ',',
			Math.round(gtmHeader[GTMHeader.AverageBytesPerSec] * 8 / 1024), 'KBps (average)', ',',
			Math.round(gtmHeader[GTMHeader.KFMaxBytesPerSec] * 8 / 1024), 'KBps (max)');
		
		stream.offset = whlsize; // position on start of LZMA bitstream
		
		redimFrame();
	} else {
		stream.offset -= 4;
	}
	
	buffer = buffer.slice(stream.offset, buffer.length); // remove header from buffer
	return buffer;
}

function unpackNextKeyframe() {
	if (gtmWLZMA.nextStreams.length <= 0) {
		return;
	}
	
	if (gtmWLZMA.nextStreams.length >= 1 && gtmWLZMA.nextStreams[gtmWLZMA.nextStreams.length - 1] == null) {
		gtmWLZMA.nextStreams.pop();
		console.log('Finished Unpacking LZMA')
		gtmUnpackingFinished = true;
		return;
	}
	
	let outStream = gtmWLZMA.nextStreams.shift();
	
	for (let i = 0; i < outStream.buffers.length; ++i)
	{
		gtmOutStream.buffers.push(outStream.buffers[i]);
	}
	gtmOutStream.size += outStream.size;
}

function redimFrame() {
	var frame = document.getElementById(gtmCanvasId);
	
	if (frame.width != gtmWidth * CTileWidth || frame.height != gtmHeight * CTileWidth) {
		frame.width = gtmWidth * CTileWidth;
		frame.height = gtmHeight * CTileWidth;
		
		var canvas = frame.getContext('2d');
		canvas.fillStyle = 'black';
		canvas.fillRect(0, 0, gtmWidth * CTileWidth, gtmHeight * CTileWidth);

		gtmTMImageData[0] = canvas.getImageData(0, 0, frame.width, frame.height);
		gtmTMImageData[1] = canvas.getImageData(0, 0, frame.width, frame.height);
	}
}

function renderEnd() {
	if (gtmWidth * gtmHeight == 0) {
		return;
	}
	
	var frame = document.getElementById(gtmCanvasId);
	var canvas = frame.getContext('2d');
	canvas.putImageData(gtmTMImageData[gtmTMDblBuff], 0, 0);
}

function drawTilemapItem(idx, attrs) {
	let palIdx = attrs >> 2;
	let tOff = ((attrs & 3) * gtmTileCount + idx) * CTileSize;
	let palR = gtmPaletteR[palIdx];
	let palG = gtmPaletteG[palIdx];
	let palB = gtmPaletteB[palIdx];
	let palA = gtmPaletteA[palIdx];
	let x = (gtmTMPos % gtmWidth) * CTileWidth;
	let y = Math.trunc(gtmTMPos / gtmWidth) * CTileWidth;
	let p = (y * gtmWidth * CTileWidth + x) * 4;
	var data = gtmTMImageData[gtmTMDblBuff].data;
	
	for (let ty = 0; ty < CTileWidth; ty++) {
		for (let tx = 0; tx < CTileWidth; tx++) {
			let v = gtmTiles[tOff++];
			data[p++] = palR[v]; 
			data[p++] = palG[v]; 
			data[p++] = palB[v]; 
			data[p++] = palA[v]; 
		}
		p += (gtmWidth - 1) * CTileWidth * 4;
	}
	
	gtmTMPos++;
}

function drawPredictedTilemapItem(offsetX, offsetY) {
	var data = gtmTMImageData[gtmTMDblBuff].data;
	var prevData = gtmTMImageData[1 - gtmTMDblBuff].data;

	let x = (gtmTMPos % gtmWidth) * CTileWidth;
	let y = Math.trunc(gtmTMPos / gtmWidth) * CTileWidth;

	let p = (y * gtmWidth * CTileWidth + x) * 4;
	let o = p + (offsetY * gtmWidth * CTileWidth + offsetX) * 4;
	
	for (let ty = 0; ty < CTileWidth; ty++) {
		for (let tx = 0; tx < CTileWidth; tx++) {
			data[p++] = prevData[o++];
			data[p++] = prevData[o++];
			data[p++] = prevData[o++];
			data[p++] = prevData[o++];
		}
		p += (gtmWidth - 1) * CTileWidth * 4;
		o += (gtmWidth - 1) * CTileWidth * 4;
	}

	gtmTMPos++;
}

function skipBlock(skipCount) {
	for(let s = 0; s < skipCount; s++)
	{
		drawPredictedTilemapItem(0, 0);
	}
}

function readByte() {
	let v = gtmOutStream.buffers[gtmDataBufIdx][gtmDataBufPos++];
	
	if (gtmDataBufPos >= gtmOutStream.buffers[gtmDataBufIdx].length)
	{
		// move to next buffer
		++gtmDataBufIdx;
		gtmDataBufPos = 0;
	}
	
	++gtmDataBufGlobalPos;
	
	return v;
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
	gtmReady |= gtmDataBufGlobalPos < gtmOutStream.size;
	
	if (gtmReady && gtmPlaying) {
		renderEnd();
		
		let doContinue = true;
		do {
			let cmd = readCommand();
			
			// console.log('command @' + gtmDataBufGlobalPos + ': ' + cmd + '\n');
			
			switch (cmd[0]) {
			case GTMCommand.SetDimensions:
				gtmWidth = readWord();
				gtmHeight = readWord();
				gtmFrameLength = Math.round(readDWord() / (1000 * 1000));
				gtmTileCount = readDWord();
				console.log('TileCount:', gtmTileCount);
				
				gtmCurIntraTile = gtmTileCount;
				gtmTileCount += gtmWidth * gtmHeight * 2 // for intra tiles (at most 2 entire tilemaps)
				
				if (gtmLoopCount <= 0) {
					gtmFrameInterval = setInterval(decodeFrame, gtmFrameLength);
					gtmTiles = new Uint8Array(gtmTileCount * CTileSize * 4);
					redimFrame();
				}
				break;
				
			case GTMCommand.TileSet:
				let tstart = readDWord();
				let tend = readDWord();
				gtmPalSize = cmd[1];
				
				let off = tstart * CTileSize + gtmTileCount * CTileSize * 0;
				let offh = tstart * CTileSize + gtmTileCount * CTileSize * 1;
				let offv = tstart * CTileSize + gtmTileCount * CTileSize * 2;
				let offhv = tstart * CTileSize + gtmTileCount * CTileSize * 3;

				for (let p = tstart; p <= tend; p++) {
					for (let ty = 0; ty < CTileWidth; ty++) {
						for (let tx = 0; tx < CTileWidth; tx++) {
							let b = readByte();
							gtmTiles[off + ty * CTileWidth + tx] = b;
							gtmTiles[offh + ty * CTileWidth + (CTileWidth - 1 - tx)] = b;
							gtmTiles[offv + (CTileWidth - 1 - ty) * CTileWidth + tx] = b;
							gtmTiles[offhv + (CTileWidth - 1 - ty) * CTileWidth + (CTileWidth - 1 - tx)] = b;
						}
					}
					
					off += CTileSize;
					offh += CTileSize;
					offv += CTileSize;
					offhv += CTileSize;
				}
				break;
				
			case GTMCommand.FrameEnd:
				if (gtmTMPos != gtmWidth * gtmHeight) {
					console.error('Incomplete tilemap ' + gtmTMPos + ' <> ' + gtmWidth * gtmHeight + '\n');
				}
				gtmTMPos = 0;

				gtmTMDblBuff = 1 - gtmTMDblBuff;
				doContinue = false;
				break;
				
			case GTMCommand.SkipBlock:
				skipBlock(cmd[1] + 1);
				break;
				
			case GTMCommand.ShortTileIdxShortPalIdx:
				drawTilemapItem(readWord(), cmd[1]);
				break;
				
			case GTMCommand.LongTileIdxShortPalIdx:
				drawTilemapItem(readDWord(), cmd[1]);
				break;
				
			case GTMCommand.LongTileIdxLongPalIdx:
				let palIdxW = readWord();
				drawTilemapItem(readDWord(), cmd[1] | (palIdxW << 2));
				break;
				
			case GTMCommand.LoadPalette:
				let palIdx = readWord();
				
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
				
			case GTMCommand.PredictedTileShortOffsets:
				drawPredictedTilemapItem((cmd[1] & 31) - (cmd[1] & 32), ((cmd[1] >> 6) & 31) - ((cmd[1] >> 6) & 32));
				break;

			case GTMCommand.PredictedTileLongOffsets:
				let offsetX = readByte();
				let offsetY = readByte();
				drawPredictedTilemapItem((offsetX & 127) - (offsetX & 128), (offsetY & 127) - (offsetY & 128));
				break;
				
			case GTMCommand.IntraTile:
				let palIdxWI = readWord();

				let ioff = gtmCurIntraTile * CTileSize + gtmTileCount * CTileSize * 0;
				let ioffh = gtmCurIntraTile * CTileSize + gtmTileCount * CTileSize * 1;
				let ioffv = gtmCurIntraTile * CTileSize + gtmTileCount * CTileSize * 2;
				let ioffhv = gtmCurIntraTile * CTileSize + gtmTileCount * CTileSize * 3;

				for (let ty = 0; ty < CTileWidth; ty++) {
					for (let tx = 0; tx < CTileWidth; tx++) {
						let b = readByte();
						gtmTiles[ioff + ty * CTileWidth + tx] = b;
						gtmTiles[ioffh + ty * CTileWidth + (CTileWidth - 1 - tx)] = b;
						gtmTiles[ioffv + (CTileWidth - 1 - ty) * CTileWidth + tx] = b;
						gtmTiles[ioffhv + (CTileWidth - 1 - ty) * CTileWidth + (CTileWidth - 1 - tx)] = b;
					}
				}
				
				drawTilemapItem(gtmCurIntraTile, cmd[1] | (palIdxWI << 2));
				
				if((++gtmCurIntraTile) >= gtmTileCount) {
					gtmCurIntraTile = gtmTileCount - gtmWidth * gtmHeight * 2;
				}
				break;
				
			case GTMCommand.ExtendedCommand:
				let size = readDWord();
				let settings = '';
				for (let i = 0; i < size; i++) {
					settings += String.fromCharCode(readByte());
				}
				if (cmd[1] == 0) {
					console.log(settings);
				}
				break;
			
			default:
				console.error('Undecoded command @' + gtmDataBufGlobalPos + ': ' + cmd + '\n');
				break;
			}

			gtmReady = gtmDataBufGlobalPos < gtmOutStream.size;
		} while (doContinue && gtmReady);
		
		if (!doContinue && !gtmReady && gtmUnpackingFinished) {
			gtmDataBufIdx = 0;
			gtmDataBufPos = 0;
			gtmDataBufGlobalPos = 0;
			gtmLoopCount++;
			gtmReady = true;
		}
	}
	
	unpackNextKeyframe();
	
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
