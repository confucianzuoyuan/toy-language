玩具语言

- 使用`Sulzmann Lu Algorithm`实现正则表达式引擎。
- 使用Scala3实现了Parser Combinator
- 转换成AST之后，直接解释执行
- 编译成JVM bytecode后使用jasmin assembler将编译成的字节码转换成`.class`文件。可以使用`java cp . test/test`执行。
