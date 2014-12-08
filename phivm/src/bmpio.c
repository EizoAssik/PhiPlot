#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "bmpio.h"

BMP_FileHeader * new_BMP_FileHeader() {
    BMP_FileHeader * header = (BMP_FileHeader*) calloc(1, sizeof(BMP_FileHeader));
    header->FileType = 0x4D42;
    header->FileOffBits = 54;
    return header;
}

BMP_InfoHeader * new_BMP_InfoHeader() {
    BMP_InfoHeader * header = (BMP_InfoHeader*) calloc(1, sizeof(BMP_InfoHeader));
    header->InfoSize = 40;
    header->FilePlanes = 1;
    header->FileBitCount = 24;
    return header;
}

void WriteFileHeader(FILE *source, BMP_FileHeader * target) {
    fwrite(&target->FileType,2,1,source);
    fwrite(&target->FileSize,4,1,source);
    fwrite(&target->BMP_Reserved_1,2,1,source);
    fwrite(&target->BMP_Reserved_2,2,1,source);
    fwrite(&target->FileOffBits,4,1,source);
}

void WriteInfoHeader(FILE *source, BMP_InfoHeader * target) {
    fwrite(&target->InfoSize,4,1,source);
    fwrite(&target->FileWidth,4,1,source);
    fwrite(&target->FileHeight,4,1,source);
    fwrite(&target->FilePlanes,2,1,source);
    fwrite(&target->FileBitCount,2,1,source);
    fwrite(&target->FileCompression,4,1,source);
    fwrite(&target->ImageSize,4,1,source);
    fwrite(&target->XPixelPerMeter,4,1,source);
    fwrite(&target->YPixelPerMeter,4,1,source);
    fwrite(&target->ColorUsed,4,1,source);
    fwrite(&target->ImportantColor,4,1,source);
}

void dump(byte * pixels, FILE *target, ui32 width, ui32 height) {
    size_t image_size = 24 * width * height;
    BMP_FileHeader * file_header = new_BMP_FileHeader();
    BMP_InfoHeader * info_header = new_BMP_InfoHeader();
    file_header->FileSize   = 54 + image_size;
    info_header->ImageSize  = image_size;
    info_header->FileWidth  = width;
    info_header->FileHeight = height;
    if(sizeof(BMP_FileHeader) == 14) {
        fwrite(file_header, 14, 1, target);
    } else {
        WriteFileHeader(target, file_header);
    }
    if(sizeof(BMP_InfoHeader) == 40) {
        fwrite(info_header, 40, 1, target);
    } else {
        WriteInfoHeader(target, info_header);
    }
    fwrite(pixels, image_size, 1, target);
}
