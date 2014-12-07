#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "bmpio.h"

BMP_FileHeader * new_BMP_FileHeader() {
    return (BMP_FileHeader *) calloc(1, sizeof(BMP_FileHeader));
}

BMP_InfoHeader * new_BMP_InfoHeader() {
    return (BMP_InfoHeader *) calloc(1, sizeof(BMP_InfoHeader));
}

int ReadFileHeader(FILE *source,BMP_FileHeader *target) {
	int back=0;
	back+=fread(&target->FileType,2,1,source);
	back+=fread(&target->FileSize,4,1,source);
	back+=fread(&target->BMP_Reserved_1,2,1,source);
	back+=fread(&target->BMP_Reserved_2,2,1,source);
	back+=fread(&target->FileOffBits,4,1,source);
	return back==5;
}

int ReadInfoHeader(FILE *source,BMP_InfoHeader *target) {
	int back=0;
	back+=fread(&target->InfoSize,4,1,source);
	back+=fread(&target->FileWidth,4,1,source);
	back+=fread(&target->FileHeight,4,1,source);
	back+=fread(&target->FilePlanes,2,1,source);
	back+=fread(&target->FileBitCount,2,1,source);
	back+=fread(&target->FileCompression,4,1,source);
	back+=fread(&target->ImageSize,4,1,source);
	back+=fread(&target->XPixelPerMeter,4,1,source);
	back+=fread(&target->YPixelPerMeter,4,1,source);
	back+=fread(&target->ColorUsed,4,1,source);
	back+=fread(&target->ImportantColor,4,1,source);
	return back==11;
}

void PrintFileHeader(BMP_FileHeader *target) {
	printf("====BMP File Header====\n");
	printf("FileType:%c%c\n",target->FileType%0x100,target->FileType/0x100);
	printf("FileSize:%u\n",target->FileSize);
	printf("Reserved:%x,%x\n", target->BMP_Reserved_1,target->BMP_Reserved_2);
	printf("Bits Off:%u\n\n",target->FileOffBits);
}

void PrintInfoHeader(BMP_InfoHeader *target) {
	printf("====BMP Info Header====\n");
	printf("InfoSize:%u\n",target->InfoSize);
	printf("FileWidth:%u\n",target->FileWidth);
	printf("FileHeight:%u\n",target->FileHeight);
	printf("FilePlanes:%u\n",target->FilePlanes);
	printf("FileBitCount:%u\n",target->FileBitCount);
	printf("Compression:%u\n",target->FileCompression);
	printf("ImageSize:%u\n",target->ImageSize);
	printf("XPixelPerMeter:%u\n",target->XPixelPerMeter);
	printf("YPixelPerMeter:%u\n",target->YPixelPerMeter);
	printf("UsedColor:%u\n",target->ColorUsed);
	printf("ImportantColor:%u\n\n",target->ImportantColor);
}

int WriteFileHeader(FILE *source,BMP_FileHeader *target) {
	int back = 0;
	back += fwrite(&target->FileType,2,1,source);
	back += fwrite(&target->FileSize,4,1,source);
	back += fwrite(&target->BMP_Reserved_1,2,1,source);
	back += fwrite(&target->BMP_Reserved_2,2,1,source);
	back += fwrite(&target->FileOffBits,4,1,source);
	return back == 5;
}

int WriteInofHeader(FILE *source,BMP_InfoHeader *target) {
	int back=0;
	back += fwrite(&target->InfoSize,4,1,source);
	back += fwrite(&target->FileWidth,4,1,source);
	back += fwrite(&target->FileHeight,4,1,source);
	back += fwrite(&target->FilePlanes,2,1,source);
	back += fwrite(&target->FileBitCount,2,1,source);
	back += fwrite(&target->FileCompression,4,1,source);
	back += fwrite(&target->ImageSize,4,1,source);
	back += fwrite(&target->XPixelPerMeter,4,1,source);
	back += fwrite(&target->YPixelPerMeter,4,1,source);
	back += fwrite(&target->ColorUsed,4,1,source);
	back += fwrite(&target->ImportantColor,4,1,source);
	return back == 11;
}

void dump(byte * pixels, FILE *target) {
	void *NewHole;
	//临时数据
	byte ColorPad,ColorPad_ZERO=0;
	//行列跳转
	ui32 delta;
	ui32 RowSize;
	//文件头数据
	ui32 xLimit = 800, yLimit = 800, XPixelPerMeter = 4096, YPixelPerMeter = 4096;
	ui32 FileSize       = 800 * 800 + 1078;
	ui32 RFileSize       = 800 * 800;
    ui32 InfoHeaderSize = 40;
	ui32 InfoCompression=0,NewOffBits=1078;
	ui16 NewBitCounts=8,FileHeadBM=0x4d42,Reserved=0,NewFilePlanes=1;

	//计算
	RowSize  = (xLimit*8 +31)/32*4;
	delta    = RowSize-xLimit;
	NewHole  = malloc(delta);
	memset(NewHole, 0, delta);
	//写入新文件头-16项,54字节
	fwrite(&FileHeadBM,2,1,target);
	fwrite(&FileSize,4,1,target);
	fwrite(&Reserved,2,1,target);
	fwrite(&Reserved,2,1,target);
	fwrite(&NewOffBits,4,1,target);
	fwrite(&InfoHeaderSize,4,1,target);
	fwrite(&xLimit,4,1,target);
	fwrite(&yLimit,4,1,target);
	fwrite(&NewFilePlanes,2,1,target);
	fwrite(&NewBitCounts,2,1,target);
	fwrite(&InfoCompression,4,1,target);
	fwrite(&RFileSize,4,1,target);
	fwrite(&XPixelPerMeter,4,1,target);
	fwrite(&YPixelPerMeter,4,1,target);
	fwrite(&InfoCompression,4,1,target);//写最后64位0
	fwrite(&InfoCompression,4,1,target);//写最后64位0
	// 写入256级灰度调色板
	for(int i=0; i<256; ++i)
	{
		ColorPad=(byte)i;
		fwrite(&ColorPad,1,1,target);
		fwrite(&ColorPad,1,1,target);
		fwrite(&ColorPad,1,1,target);
		fwrite(&ColorPad_ZERO,1,1,target);
	}
	//生成文件内容
    fwrite(pixels, 800*800, 1, target);
}
