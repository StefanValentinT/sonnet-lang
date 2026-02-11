define i32 @main() {
entry:
  %tmp.0 = alloca [12 x i32]
  %tmp.3 = alloca i32
  %tmp.4 = alloca i32
  %tmp.1 = alloca { i32*, i32 }
  %tmp.2 = alloca i32*
  %r0 = getelementptr inbounds [12 x i32], [12 x i32]* %tmp.0, i64 0, i64 0
  store i32 72, i32* %r0
  %r1 = getelementptr inbounds [12 x i32], [12 x i32]* %tmp.0, i64 0, i64 1
  store i32 101, i32* %r1
  %r2 = getelementptr inbounds [12 x i32], [12 x i32]* %tmp.0, i64 0, i64 2
  store i32 108, i32* %r2
  %r3 = getelementptr inbounds [12 x i32], [12 x i32]* %tmp.0, i64 0, i64 3
  store i32 108, i32* %r3
  %r4 = getelementptr inbounds [12 x i32], [12 x i32]* %tmp.0, i64 0, i64 4
  store i32 111, i32* %r4
  %r5 = getelementptr inbounds [12 x i32], [12 x i32]* %tmp.0, i64 0, i64 5
  store i32 32, i32* %r5
  %r6 = getelementptr inbounds [12 x i32], [12 x i32]* %tmp.0, i64 0, i64 6
  store i32 87, i32* %r6
  %r7 = getelementptr inbounds [12 x i32], [12 x i32]* %tmp.0, i64 0, i64 7
  store i32 111, i32* %r7
  %r8 = getelementptr inbounds [12 x i32], [12 x i32]* %tmp.0, i64 0, i64 8
  store i32 114, i32* %r8
  %r9 = getelementptr inbounds [12 x i32], [12 x i32]* %tmp.0, i64 0, i64 9
  store i32 108, i32* %r9
  %r10 = getelementptr inbounds [12 x i32], [12 x i32]* %tmp.0, i64 0, i64 10
  store i32 100, i32* %r10
  %r11 = getelementptr inbounds [12 x i32], [12 x i32]* %tmp.0, i64 0, i64 11
  store i32 33, i32* %r11
  %r12 = getelementptr [12 x i32], [12 x i32]* %tmp.0, i32 0
  store i32* %r12, i32** %tmp.2
  store i32 12, i32* %tmp.3
  %r13 = load i32*, i32** %tmp.2
  %r14 = getelementptr inbounds { i32*, i32 }, { i32*, i32 }* %tmp.1, i32 0, i32 0
  store i32* %r13, i32** %r14
  %r15 = load i32, i32* %tmp.3
  %r16 = getelementptr inbounds { i32*, i32 }, { i32*, i32 }* %tmp.1, i32 0, i32 1
  store i32 %r15, i32* %r16
  %r17 = load { i32*, i32 }, { i32*, i32 }* %tmp.1
  %r18 = call i32 @print({ i32*, i32 } %r17)
  store i32 %r18, i32* %tmp.4
  ret i32 0
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
