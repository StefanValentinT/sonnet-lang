define i32 @fib(i32 %arg_tmp.0) {
entry:
  %tmp.3 = alloca i32
  %tmp.11 = alloca i32
  %tmp.1 = alloca i32
  %tmp.10 = alloca i32
  %tmp.9 = alloca i32
  %tmp.7 = alloca i32
  %tmp.6 = alloca i32
  %tmp.8 = alloca i32
  %tmp.0 = alloca i32
  store i32 %arg_tmp.0, i32* %tmp.0
  %r0 = load i32, i32* %tmp.0
  %r2 = icmp slt i32 %r0, 2
  %r3 = zext i1 %r2 to i32
  store i32 %r3, i32* %tmp.6
  %r4 = load i32, i32* %tmp.6
  %r5 = icmp eq i32 %r4, 0
  br i1 %r5, label %cond_else4, label %cont6
cont6:
  %r6 = load i32, i32* %tmp.0
  store i32 %r6, i32* %tmp.3
  br label %cond_end5
cond_else4:
  %r7 = load i32, i32* %tmp.0
  %r8 = sub i32 %r7, 1
  store i32 %r8, i32* %tmp.7
  %r9 = load i32, i32* %tmp.7
  %r10 = call i32 @fib(i32 %r9)
  store i32 %r10, i32* %tmp.8
  %r11 = load i32, i32* %tmp.0
  %r12 = sub i32 %r11, 2
  store i32 %r12, i32* %tmp.9
  %r13 = load i32, i32* %tmp.9
  %r14 = call i32 @fib(i32 %r13)
  store i32 %r14, i32* %tmp.10
  %r15 = load i32, i32* %tmp.8
  %r16 = load i32, i32* %tmp.10
  %r17 = add i32 %r15, %r16
  store i32 %r17, i32* %tmp.11
  %r18 = load i32, i32* %tmp.11
  store i32 %r18, i32* %tmp.3
  br label %cond_end5
cond_end5:
  %r19 = load i32, i32* %tmp.3
  store i32 %r19, i32* %tmp.1
  %r20 = load i32, i32* %tmp.1
  ret i32 %r20
}

define i32 @main() {
entry:
  %tmp.12 = alloca i32
  %tmp.2 = alloca i32
  %r0 = call i32 @fib(i32 10)
  store i32 %r0, i32* %tmp.12
  %r1 = load i32, i32* %tmp.12
  store i32 %r1, i32* %tmp.2
  %r2 = load i32, i32* %tmp.2
  ret i32 %r2
}


declare i32 @puts(i8*)

define i32 @print({ i32*, i32 } %s) {
entry:
  ; extract pointer and length from slice
  %ptr_i32 = extractvalue { i32*, i32 } %s, 0
  %len_i32 = extractvalue { i32*, i32 } %s, 1
  %len = sext i32 %len_i32 to i64

  ; allocate temporary buffer and index
  %buf = alloca [1024 x i8]
  %i = alloca i64
  store i64 0, i64* %i
  br label %loop

loop:
  %idx = load i64, i64* %i
  %cmp = icmp ult i64 %idx, %len
  br i1 %cmp, label %body, label %end

body:
  ; load i32 character
  %char32_ptr = getelementptr inbounds i32, i32* %ptr_i32, i64 %idx
  %c32 = load i32, i32* %char32_ptr
  %c8 = trunc i32 %c32 to i8

  ; store in buffer
  %buf_ptr = getelementptr [1024 x i8], [1024 x i8]* %buf, i64 0, i64 %idx
  store i8 %c8, i8* %buf_ptr

  ; increment
  %next = add i64 %idx, 1
  store i64 %next, i64* %i
  br label %loop

end:
  ; null-terminate
  %buf_end = getelementptr [1024 x i8], [1024 x i8]* %buf, i64 0, i64 %len
  store i8 0, i8* %buf_end

  ; call puts
  %buf_ptr0 = getelementptr [1024 x i8], [1024 x i8]* %buf, i64 0, i64 0
  call i32 @puts(i8* %buf_ptr0)
  ret i32 0
}
