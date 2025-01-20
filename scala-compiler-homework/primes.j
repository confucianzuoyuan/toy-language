
.class public primes
.super java/lang/Object

.method public static write(I)V
  .limit locals 1
  .limit stack 2
  getstatic java/lang/System/out Ljava/io/PrintStream;
  iload 0
  invokevirtual java/io/PrintStream/println(I)V
  return
.end method

.method public static writes(Ljava/lang/String;)V
  .limit stack 2
  .limit locals 1
  getstatic java/lang/System/out Ljava/io/PrintStream;
  aload 0
  invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V
  return
.end method

.method public static read()I
  .limit locals 10
  .limit stack 10
  ldc 0
  istore 1 ; this will hold our final integer
  Label1:
  getstatic java/lang/System/in Ljava/io/InputStream;
  invokevirtual java/io/InputStream/read()I
  istore 2
  iload 2
  ldc 10 ; test for the newline delimiter for Unix
  isub
  ifeq Label2
  iload 2
  ldc 13 ; test for the carriage-return in Windows
  isub
  ifeq Label2
  iload 2
  ldc 32 ; the space delimiter
  isub
  ifeq Label2
  iload 2
  ldc 48 ; we have our digit in ASCII, have to subtract it from 48
  isub
  ldc 10
  iload 1
  imul
  iadd
  istore 1
  goto Label1
  Label2:
  ; when we come here we have our integer computed
  ; in local variable 1
  iload 1
  ireturn
.end method

.method public static main([Ljava/lang/String;)V
  .limit locals 200
  .limit stack 200
  ldc 100
  istore 0
  ldc 2
  istore 1
L.wbegin.3:
  iload 1
  iload 0
  if_icmpge L.wend.3
  ldc 2
  istore 2
  ldc 0
  istore 3
L.wbegin.4:
  iload 2
  iload 1
  ldc 2
  idiv
  ldc 1
  iadd
  if_icmpge L.wend.4
  iload 3
  ldc 0
  if_icmpne L.wend.4
  iload 1
  iload 2
  idiv
  iload 2
  imul
  iload 1
  if_icmpne L.ifelse.5
  ldc 1
  istore 3
  goto L.ifend.5
L.ifelse.5:
L.ifend.5:
  iload 2
  ldc 1
  iadd
  istore 2
  goto L.wbegin.4
L.wend.4:
  iload 3
  ldc 0
  if_icmpne L.ifelse.7
  iload 1
  invokestatic primes/write(I)V
  ldc "\n"
  invokestatic primes/writes(Ljava/lang/String;)V
  goto L.ifend.7
L.ifelse.7:
L.ifend.7:
  iload 1
  ldc 1
  iadd
  istore 1
  goto L.wbegin.3
L.wend.3:

  return
.end method
