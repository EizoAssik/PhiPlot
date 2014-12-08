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

// 1-2：  图像文件头。0x4d42=’BM’，表示是Windows支持的BMP格式。(注意：查ascii表B 0x42,M0x4d,bfType 为两个字节，B为low字节，M为high字节所以bfType=0x4D42，而不是0x424D，但注意)
// 3-6：  整个文件大小。4690 0000，为00009046h=36934。
// 7-8：  保留，必须设置为0。
// 9-10： 保留，必须设置为0。
// 11-14：从文件开始到位图数据之间的偏移量(14+40+4*（2^biBitCount）)。4600 0000，为00000046h=70，上面的文件头就是35字=70字节。位图信息头
// 15-18：位图图信息头长度。
// 19-22：位图宽度，以像素为单位。8000 0000，为00000080h=128。
// 23-26：位图高度，以像素为单位。9000 0000，为00000090h=144。
// 27-28：位图的位面数，该值总是1。0100，为0001h=1。
// 29-30：每个像素的位数。有1（单色），4（16色），8（256色），16（64K色，高彩色），24（16M色，真彩色），32（4096M色，增强型真彩色）。1000为0010h=16。
// 31-34：压缩说明：有0（不压缩），1（RLE 8，8位RLE压缩），2（RLE 4，4位RLE压缩，3（Bitfields，位域存放）。RLE简单地说是采用像素数+像素值的方式进行压缩。T408采用的是位域存放方式，用两个字节表示一个像素，位域分配为r5b6g5。图中0300 0000为00000003h=3。
// 35-38：用字节数表示的位图数据的大小，该数必须是4的倍数，数值上等于（≥位图宽度的最小的4的倍数）×位图高度×每个像素位数。0090 0000为00009000h=80×90×2h=36864。
// 39-42：用象素/米表示的水平分辨率。A00F 0000为0000 0FA0h=4000。
// 43-46：用象素/米表示的垂直分辨率。A00F 0000为0000 0FA0h=4000。
// 47-50：位图使用的颜色索引数。设为0的话，则说明使用所有调色板项。
// 51-54：对图象显示有重要影响的颜色索引的数目。如果是0，表示都重要。
