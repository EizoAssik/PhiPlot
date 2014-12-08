#include "common.h"

typedef struct struct_BMPFILEHEADER {
	ui16 FileType;        //文件类型
	ui32 FileSize;        //文件大小-字节
	ui16 BMP_Reserved_1;
	ui16 BMP_Reserved_2;  //两个保留字段均必须为0
	ui32 FileOffBits;	   //文件头偏移量
} BMP_FileHeader;

typedef struct struct_BMPINFOHEADER {
	ui32 InfoSize;		  //信息头大小
	ui32 FileWidth;
	ui32 FileHeight;	  //正数 左下->右上;负数 
	ui16 FilePlanes;	  //颜色平面数 总是为1
	ui16 FileBitCount;
	ui32 FileCompression;
	ui32 ImageSize;	  //字节 
	ui32 XPixelPerMeter;
	ui32 YPixelPerMeter;
	ui32 ColorUsed;
	ui32 ImportantColor; //0表示都重要
} BMP_InfoHeader;

typedef struct struct_RGBColor {
	byte RGB_B;
	byte RGB_G;
	byte RGB_R;
	byte Alpha;
} BMPColor;

void dump(byte * pixels, FILE * target, ui32 x, ui32 y);

BMP_FileHeader * new_BMP_FileHeader();
BMP_InfoHeader * new_BMP_InfoHeader();
void WriteFileHeader(FILE *source,BMP_FileHeader *target);
void WriteInofHeader(FILE *source,BMP_InfoHeader *target);
