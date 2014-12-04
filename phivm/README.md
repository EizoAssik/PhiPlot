PhiVM
=====

PhiVM被实现为一个堆栈机器，栈空间使用64位存储单元划分。
PhiVM的浮点数直接使用了C语言的`double`，因此其具体实现是平台相关的。
PhiVM的指令为64位定长无符号整数。使用该长度是为了确保能够在主流平台下完成对虚拟RAM的寻址。
PhiVM的RAM被实现为一个二维网格，具体实现如同BMP24。
PhiVM本身**不支持IO**，执行停机指令后RAM中的数据将被转储成为位图文件。


暂定的指令如下：
PhiVM的指令集如下：

0x00 NOP
0x01 POP
0x02 PUSH
0x03 SWAP
0x04 DUP
0x05 ADD
0x06 SUB
0x07 MUL
0x08 DIV
0x09 CALL
0x0A RET
0x0B NEG // top <- -top
0x0C AND
0x0D OR
0x0E JMP
0x0F JP // Jump when postive
0x10 JZ
0x11 <
0x12 ==
0x13 >
0x14 NOT
0x15 DRAW
0x16 HALT
0x17 STORE
0x18 LOAD
