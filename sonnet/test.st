fib = fun (x i32) i32 -> 
    if x < 2 then 1 else fib(n-1) + fib(n-2)

CHUNK_SIZE i32 = 16

deftype Color enum{
    Red, Green, Blue
}

deftype Pos struct{
    x i32, y i32
}

test_pointers = fun () i32 ->
{
    let val = 10;
    let ptr Ptr i32 = val.*; 
    print(ptr.!);
    ptr <- 20;
    val
}

an_structs = fun () i32 ->
{
    let pos = struct{x 10, y 11};
    print((struct{x 5}).x);
}


main = fun () void ->
{
    let pos = struct{x 0, y 10};
    print(pos.x);
    let c = Color.Red;
    print(c);

    ptr <- 10;
    let ptr2 Ptr i32 = pos.x.*;
}