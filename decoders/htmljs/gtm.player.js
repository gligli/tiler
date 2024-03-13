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
// SkipBlock:               data -> none; commandBits -> skip count - 1 (10 bits)
// ShortTileIdx:            data -> tile index (16 bits); commandBits -> palette index (8 bits); V mirror (1 bit); H mirror (1 bit)
// LongTileIdx:             data -> tile index (32 bits); commandBits -> palette index (8 bits); V mirror (1 bit); H mirror (1 bit)
// LoadPalette:             data -> palette index (8 bits); palette format (8 bits) (0: RGBA32); RGBA bytes (32bits); commandBits -> none
// ShortBlendTileIdx:       data -> tile index (16 bits); BlendY offset(4 bits); BlendX offset(4 bits); Previous frame factor (4 bits); Current frame factor (4 bits);
//                            commandBits -> palette index (8 bits); V mirror (1 bit); H mirror (1 bit)
// LongLendTileIdx:         data -> tile index (32 bits); BlendY offset(4 bits); BlendX offset(4 bits); Previous frame factor (4 bits); Current frame factor (4 bits);
//                            commandBits -> palette index (8 bits); V mirror (1 bit); H mirror (1 bit)
// ShortAddlBlendTileIdx:   data -> tile index (16 bits) * 2; blending ratio (8 bits); padding (8 bits); commandBits -> palette index (8 bits); V mirror (1 bit); H mirror (1 bit)
// LongAddlBlendTileIdx:    data -> tile index (32 bits) * 2; blending ratio (8 bits); padding (8 bits); commandBits -> palette index (8 bits); V mirror (1 bit); H mirror (1 bit)
// ShortAdditionalTileIdx:  data -> tile index (16 bits) * 2; commandBits -> palette index (8 bits); V mirror (1 bit); H mirror (1 bit)
// LongAdditionalTileIdx:   data -> tile index (32 bits) * 2; commandBits -> palette index (8 bits); V mirror (1 bit); H mirror (1 bit)
//
// (insert new commands here...)
//
// FrameEnd:                data -> none; commandBits bit 0 -> keyframe end
// TileSet:                 data -> start tile (32 bits); end tile (32 bits); { indexes per tile (64 bytes) } * count; commandBits -> indexes count per palette
// SetDimensions:           data -> height in tiles (16 bits); width in tiles (16 bits); frame length in nanoseconds (32 bits) (2^32-1: still frame); tile count (32 bits); commandBits -> none
// ExtendedCommand:         data -> following bytes count (32 bits); custom commands, proprietary extensions, ...; commandBits -> extended command index (10 bits)
//
// ReservedArea:            reserving the MSB for future use (do not use for new commands)    

const GTMCommand = {
	'SkipBlock' : 0,
	'ShortTileIdx' : 1,
	'LongTileIdx' : 2,
	'LoadPalette' : 3,
	'ShortBlendTileIdx' : 4,
	'LongBlendTileIdx' : 5,
	'ShortAddlBlendTileIdx' : 6,
    'LongAddlBlendTileIdx' : 7,
	'PrevFrameBlend' : 8,
    'ShortAdditionalTileIdx' : 9,
    'LongAdditionalTileIdx' : 10,
	
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
const CTileSize = CTileWidth * CTileWidth;

let gtmWLZMA = null;

let gtmCanvasId = '';
let gtmInBuffer = null;
let gtmOutStream = null;
let gtmHeader = null;
let gtmLzmaBytesPerSecond = 1024 * 1024;
let gtmTiles = null;
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
let gtmUnpackingFinished = false;
let gtmDataBufIdx = 0;
let gtmDataBufPos = 0;
let gtmDataBufGlobalPos = 0;
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
let gtmMaxFTBlend = 16;
let gtmMaxPFBlend = 255;

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
	gtmLzmaBytesPerSecond = 1024 * 1024;
	gtmTiles = null;
	
	gtmReady = false;
	gtmUnpackingFinished = false;
	gtmDataBufIdx = 0;
	gtmDataBufPos = 0;
	gtmDataBufGlobalPos = 0;
	gtmFrameLength = 0;
	gtmTileCount = 0;
	gtmPalSize = 0;
	gtmTMPos = 0;
	gtmTMDblBuff = 0;
	gtmKFCurDblBuff = 0;
	gtmKFPrevDblBuff = 0;
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
		gtmLzmaBytesPerSecond = gtmHeader[GTMHeader.KFMaxBytesPerSec];
		console.log('Header:', gtmHeader[GTMHeader.FramePixelWidth], 'x', gtmHeader[GTMHeader.FramePixelHeight], ',',
			Math.round(gtmHeader[GTMHeader.AverageBytesPerSec] * 8 / 1024), 'KBps (average)', ',',
			Math.round(gtmHeader[GTMHeader.KFMaxBytesPerSec] * 8 / 1024), 'KBps (max)');
		
		stream.offset = whlsize; // position on start of LZMA bitstream
		
		// KLUDGE: before version 2, blending had wrong extents
		if (gtmHeader[GTMHeader.EncoderVersion] < 2)
			gtmMaxFTBlend = 15
		else
			gtmMaxFTBlend = 16;
		
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

		gtmTMImageData = canvas.getImageData(0, 0, frame.width, frame.height);
		gtmTMIndexes[0] = new Uint32Array(gtmWidth * gtmHeight);
		gtmTMIndexes[1] = new Uint32Array(gtmWidth * gtmHeight);
		gtmTMAttrs[0] = new Uint16Array(gtmWidth * gtmHeight);
		gtmTMAttrs[1] = new Uint16Array(gtmWidth * gtmHeight);
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
	let tOff = ((attrs & 3) * gtmTileCount + idx) * CTileSize;
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
			let v = gtmTiles[tOff++];
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
	let tOff = ((attrs & 3) * gtmTileCount + idx) * CTileSize;
	let palR = gtmPaletteR[palIdx];
	let palG = gtmPaletteG[palIdx];
	let palB = gtmPaletteB[palIdx];
	let palA = gtmPaletteA[palIdx];
	
	let prevAttrs = gtmTMAttrs[1 - gtmTMDblBuff][gtmTMPos + blendOff];
	let prevIdx = gtmTMIndexes[1 - gtmTMDblBuff][gtmTMPos + blendOff];
	
	let prevPalIdx = (prevAttrs >> 2)  + 256 * gtmKFPrevDblBuff;
	let prevTOff = ((prevAttrs & 3) * gtmTileCount + prevIdx) * CTileSize;
	let prevPalR = gtmPaletteR[prevPalIdx];
	let prevPalG = gtmPaletteG[prevPalIdx];
	let prevPalB = gtmPaletteB[prevPalIdx];
	
	let x = (gtmTMPos % gtmWidth) * CTileWidth;
	let y = Math.trunc(gtmTMPos / gtmWidth) * CTileWidth;
	let p = (y * gtmWidth * CTileWidth + x) * 4;
	
	var data = gtmTMImageData.data;
	
	for (let ty = 0; ty < CTileWidth; ty++) {
		for (let tx = 0; tx < CTileWidth; tx++) {
			let pv = gtmTiles[prevTOff++];
			let cv = gtmTiles[tOff++];
			data[p++] = Math.min(255, ((palR[cv] * blendCur + prevPalR[pv] * blendPrev) / gtmMaxFTBlend) >> 0);
			data[p++] = Math.min(255, ((palG[cv] * blendCur + prevPalG[pv] * blendPrev) / gtmMaxFTBlend) >> 0);
			data[p++] = Math.min(255, ((palB[cv] * blendCur + prevPalB[pv] * blendPrev) / gtmMaxFTBlend) >> 0);
			data[p++] = palA[cv];
		}
		p += (gtmWidth - 1) * CTileWidth * 4;
	}
	
	gtmTMIndexes[gtmTMDblBuff][gtmTMPos] = idx;
	gtmTMAttrs[gtmTMDblBuff][gtmTMPos] = attrs;
	gtmTMPos++;
}

function drawPrevFrameBlended(offsets, blend) {
	let offset = (((offsets >> 5) & 15) - ((offsets >> 5) & 16)) * gtmWidth + ((offsets & 15) - (offsets & 16));
	let blendOff = (blend >> 8) & 255;
	let blendPrev = blend & 255;
	
	let prevAttrs = gtmTMAttrs[1 - gtmTMDblBuff][gtmTMPos];
	let prevIdx = gtmTMIndexes[1 - gtmTMDblBuff][gtmTMPos];
	
	let prevPalIdx = (prevAttrs >> 2)  + 256 * gtmKFPrevDblBuff;
	let prevTOff = ((prevAttrs & 3) * gtmTileCount + prevIdx) * CTileSize;
	let prevPalR = gtmPaletteR[prevPalIdx];
	let prevPalG = gtmPaletteG[prevPalIdx];
	let prevPalB = gtmPaletteB[prevPalIdx];
	let prevPalA = gtmPaletteA[prevPalIdx];
	
	let offAttrs = gtmTMAttrs[1 - gtmTMDblBuff][gtmTMPos + offset];
	let offIdx = gtmTMIndexes[1 - gtmTMDblBuff][gtmTMPos + offset];

	let offPalIdx = (offAttrs >> 2)  + 256 * gtmKFPrevDblBuff;
	let offTOff = ((offAttrs & 3) * gtmTileCount + offIdx) * CTileSize;
	let offPalR = gtmPaletteR[offPalIdx];
	let offPalG = gtmPaletteG[offPalIdx];
	let offPalB = gtmPaletteB[offPalIdx];
	
	let x = (gtmTMPos % gtmWidth) * CTileWidth;
	let y = Math.trunc(gtmTMPos / gtmWidth) * CTileWidth;
	let p = (y * gtmWidth * CTileWidth + x) * 4;
	
	var data = gtmTMImageData.data;
	
	for (let ty = 0; ty < CTileWidth; ty++) {
		for (let tx = 0; tx < CTileWidth; tx++) {
			let ov = gtmTiles[offTOff++];
			let pv = gtmTiles[prevTOff++];
			data[p++] = Math.min(255, ((offPalR[ov] * blendOff + prevPalR[pv] * blendPrev) / gtmMaxPFBlend) >> 0);
			data[p++] = Math.min(255, ((offPalG[ov] * blendOff + prevPalG[pv] * blendPrev) / gtmMaxPFBlend) >> 0);
			data[p++] = Math.min(255, ((offPalB[ov] * blendOff + prevPalB[pv] * blendPrev) / gtmMaxPFBlend) >> 0);
			data[p++] = prevPalA[pv];
		}
		p += (gtmWidth - 1) * CTileWidth * 4;
	}
	
	gtmTMIndexes[gtmTMDblBuff][gtmTMPos] = prevIdx;
	gtmTMAttrs[gtmTMDblBuff][gtmTMPos] = prevAttrs;
	gtmTMPos++;
}

function drawAdditionalTilemapItem(idx, idx2, attrs, blend) {
	let palIdx = (attrs >> 2) + 256 * gtmKFCurDblBuff;
	let tOff = ((attrs & 3) * gtmTileCount + idx) * CTileSize;
	let tOff2 = ((attrs & 3) * gtmTileCount + idx2) * CTileSize;
	let palR = gtmPaletteR[palIdx];
	let palG = gtmPaletteG[palIdx];
	let palB = gtmPaletteB[palIdx];
	let palA = gtmPaletteA[palIdx];
	let x = (gtmTMPos % gtmWidth) * CTileWidth;
	let y = Math.trunc(gtmTMPos / gtmWidth) * CTileWidth;
	let p = (y * gtmWidth * CTileWidth + x) * 4;
	var data = gtmTMImageData.data;
	
	if (blend == 128)
	{
		for (let ty = 0; ty < CTileWidth; ty++) {
			for (let tx = 0; tx < CTileWidth; tx++) {
				let v = gtmTiles[tOff++];
				let v2 = gtmTiles[tOff2++];

				data[p++] = (palR[v] + palR[v2]) >> 1;
				data[p++] = (palG[v] + palG[v2]) >> 1;
				data[p++] = (palB[v] + palB[v2]) >> 1;
				data[p++] = (palA[v] + palA[v2]) >> 1;
			}
			p += (gtmWidth - 1) * CTileWidth * 4;
		}
	}
	else
	{
		let rblend = 256 - blend;
		
		for (let ty = 0; ty < CTileWidth; ty++) {
			for (let tx = 0; tx < CTileWidth; tx++) {
				let v = gtmTiles[tOff++];
				let v2 = gtmTiles[tOff2++];

				data[p++] = (palR[v] * rblend + palR[v2] * blend) >> 8;
				data[p++] = (palG[v] * rblend + palG[v2] * blend) >> 8;
				data[p++] = (palB[v] * rblend + palB[v2] * blend) >> 8;
				data[p++] = (palA[v] * rblend + palA[v2] * blend) >> 8;
			}
			p += (gtmWidth - 1) * CTileWidth * 4;
		}
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
			
			switch (cmd[0]) {
			case GTMCommand.SetDimensions:
				gtmWidth = readWord();
				gtmHeight = readWord();
				gtmFrameLength = Math.round(readDWord() / (1000 * 1000));
				gtmTileCount = readDWord();
				console.log('TileCount:', gtmTileCount);
				
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
				let idxW = readWord();
				let blendW = readWord();
				drawBlendedTilemapItem(idxW, cmd[1], blendW);
				break;
				
			case GTMCommand.LongBlendTileIdx:
				let idxDW = readDWord();
				let blendDW = readWord();
				drawBlendedTilemapItem(idxDW, cmd[1], blendDW);
				break;
				
			case GTMCommand.ShortAddlBlendTileIdx:
				let idx1bW = readWord();
				let idx2bW = readWord();
				let blendbW = readByte();
				readByte(); // padding
				drawAdditionalTilemapItem(idx1bW, idx2bW, cmd[1], blendbW);
				break;
				
			case GTMCommand.LongAddlBlendTileIdx:
				let idx1bDW = readDWord();
				let idx2bDW = readDWord();
				let blendbDW = readByte();
				readByte(); // padding
				drawAdditionalTilemapItem(idx1bDW, idx2bDW, cmd[1], blendbDW);
				break;
				
			case GTMCommand.ShortAdditionalTileIdx:
				let idx1W = readWord();
				let idx2W = readWord();
				drawAdditionalTilemapItem(idx1W, idx2W, cmd[1], 128);
				break;

			case GTMCommand.LongAdditionalTileIdx:
				let idx1DW = readDWord();
				let idx2DW = readDWord();
				drawAdditionalTilemapItem(idx1DW, idx2DW, cmd[1], 128);
				break;

			case GTMCommand.PrevFrameBlend:
				let blend = readWord();
				drawPrevFrameBlended(cmd[1], blend);
				break;
				
			case GTMCommand.ExtendedCommand:
				let size = readDWord();
				for (let i = 0; i < size; i++) {
					readByte();
				}
			
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
