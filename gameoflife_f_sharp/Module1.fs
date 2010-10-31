// Learn more about F# at http://fsharp.net

module Module1
open NUnit.Framework

let rec countneight previous alist =
    match alist with
    | ' '::' '::T ->   [previous] @  (countneight 0 ([' '] @ T))
    | ' '::'x'::T ->   [previous+1] @  (countneight 0 (['x'] @ T))
    | 'x'::'x'::T ->   [previous+1] @  (countneight 1 (['x'] @ T))
    | 'x'::' '::T ->   [previous] @  (countneight 1 ([' '] @ T))
    | _::[] -> [previous]

let rec countneighall previous alist =
    match alist with
    | ' '::' '::T ->   [previous] @  (countneighall 0 ([' '] @ T))
    | ' '::'x'::T ->   [previous+1] @  (countneighall 0 (['x'] @ T))
    | 'x'::'x'::T ->   [previous+2] @  (countneighall 1 (['x'] @ T))
    | 'x'::' '::T ->   [previous+1] @  (countneighall 1 ([' '] @ T))
    | 'x'::[] -> [previous+1]
    | ' '::[] -> [previous]


let rec sumsOfTriplets thesum=
    match thesum with
    | (a,b,c)::T -> [a+b+c] @ sumsOfTriplets T
    | _ -> []

let rec bigsum thesum=
    match thesum with
    | H::T -> sumsOfTriplets H :: bigsum T
    | _ -> []
    
   
let rec neicount previous alist =
    match alist with
    | H1 :: H2 :: T -> [List.zip3 (countneighall 0  previous) (countneight 0 H1) (countneighall 0 H2)] @ neicount H1 (H2::T)
    | H :: [] -> [List.zip3 (countneighall 0 previous) (countneight 0 H) (countneighall 0 [' ';' ';' '])]
    | _ -> []


let rec reczip lll aaa =
    match lll,aaa with
    |H1::T1,H2::T2 -> [(List.zip H1 H2)] @ (reczip T1 T2)
    | _ -> [] 


let matrixnei alist =
    let pre = [1.. (List.length (List.head alist))] |> Seq.fold (fun acc x -> acc @ [' '])[]
    let nei = bigsum (neicount pre alist)
    reczip alist nei


let rec singleLineEvolution alist =
    match alist with
    | (' ',3)::T  -> 'x' :: singleLineEvolution T
    | ('x',b)::T when b=2 || b=3 -> 'x':: singleLineEvolution T
    | (_,_)::T -> ' ':: singleLineEvolution T
    | _ -> []


let rec sevolve mat =
    match mat with
    | H::T -> (singleLineEvolution H) :: sevolve T
    | _ -> []


let mevolve mat =
    sevolve (matrixnei mat)



[<TestFixture>]
 type ``lkjfdlf lkdjdfdfdfaldfa `` ()=
 
  [<Test>] member test.
   ``count neighborhs`` ()=
   Assert.AreEqual([0;0;0], countneight 0 [' ';' ';' '])

  [<Test>] member test.
   ``count neighborhs 1`` ()=
   Assert.AreEqual([0;0;0;0], countneight 0 [' ';' ';' ';' '])

  [<Test>] member test.
   ``count neighborhs 2`` ()=
   Assert.AreEqual([0;1;0;0], countneight 0 ['x';' ';' ';' '])

  [<Test>] member test.
   ``count neighborhs 3`` ()=
   Assert.AreEqual([1;1;1;0], countneight 0 ['x';'x';' ';' '])

  [<Test>] member test.
   ``count neigh including itself`` ()=
   Assert.AreEqual([2;2;1], countneighall 0 ['x';'x';' ']  )

  [<Test>] member test.
   ``map for getting the neigh count `` ()=
   Assert.AreEqual([[(0,1,2);(0,2,3);(0,1,2)];[(2,1,2);(3,2,3);(2,1,2)];[(2,1,0);(3,2,0);(2,1,0)]], neicount [' ';' ';' '] [['x';'x';'x'];['x';'x';'x'];['x';'x';'x']])

  [<Test>] member test.
   ``map for getting the neigh count dfd`` ()=
   Assert.AreEqual([[(0,0,1);(0,2,2);(0,0,1)];[(1,0,1);(2,2,2);(1,0,1)];[(1,0,0);(2,2,0);(1,0,0)]], neicount [' ';' ';' '] [['x';' ';'x'];['x';' ';'x'];['x';' ';'x']])

  [<Test>] member test.
   ``map for getting the neigh count dfddfd`` ()=
   Assert.AreEqual([[(0,0,1);(0,1,2);(0,0,1)];[(0,0,1);(1,2,2);(1,0,1)];[(1,0,0);(2,2,0);(1,0,0)]], neicount [' ';' ';' '] [[' ';' ';'x'];['x';' ';'x'];['x';' ';'x']])



  [<Test>] member test.
   ``con count`` ()=
   Assert.AreEqual([5;8;6], sumsOfTriplets [(2,1,2);(3,2,3);(3,1,2)] )

  [<Test>] member test.
   ``con coufasdfnt`` ()=
   Assert.AreEqual([[5;8;6];[5;8;6]], bigsum [[(2,1,2);(3,2,3);(3,1,2)];[(2,1,2);(3,2,3);(3,1,2)]])

  [<Test>] member test.
   ``map dfsasaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaunt `` ()=
   Assert.AreEqual([[3;5;3];[5;8;5];[3;5;3]], bigsum (neicount [' ';' ';' '] [['x';'x';'x'];['x';'x';'x'];['x';'x';'x']]))

  [<Test>] member test.
   ``map dfaaaaaaaaaaaaaaaaaaaaunt `` ()=
   Assert.AreEqual([[3;5;3];[5;8;5];[3;5;3]], bigsum (neicount [' ';' ';' '] [['x';'x';'x'];['x';'x';'x'];['x';'x';'x']]))



  [<Test>] member test.
   ``fsdfds `` ()=
   Assert.AreEqual([[('x',3);('x',5);('x',3)];[('x',5);('x',8);('x',5)];[('x',3);('x',5);('x',3)]], matrixnei [['x';'x';'x'];['x';'x';'x'];['x';'x';'x']])


  [<Test>] member test.
   ``dslkfkdlflksadfas fd`` ()=
   Assert.AreEqual([[' ';'x';' '];[' ';'x';' '];[' ';'x';' ']], mevolve [[' ';' ';' '];['x';'x';'x'];[' ';' ';' ']])

   [<Test>] member test.
    ``single line evolution `` ()=
    Assert.AreEqual(['x';'x';' '], singleLineEvolution [('x',3);(' ',3);(' ',1)])

