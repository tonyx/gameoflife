// Learn more about F# at http://fsharp.net

module Module1
open NUnit.Framework
open System
open System.Drawing
open System.Windows.Forms
open System.ComponentModel
open System.Threading

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
    | H :: [] -> [List.zip3 (countneighall 0 previous) (countneight 0 H) (countneighall 0 ([1.. (List.length H)] |> List.fold (fun acc x -> acc @ [' '])[]))]
    | _ -> []


let rec reczip first second =
    match first,second with
    |H1::T1,H2::T2 -> [(List.zip H1 H2)] @ (reczip T1 T2)
    | _ -> [] 


let matrixToNeighboursCountForCellNotation alist =
    let pre = [1.. (List.length (List.head alist))] |> Seq.fold (fun acc x -> acc @ [' '])[]
    let nei = bigsum (neicount pre alist)
    reczip alist nei


let rec singleLineEvolution alist =
    match alist with
    | (' ',3)::T  -> 'x' :: singleLineEvolution T
    | ('x',b)::T when b=2 || b=3 -> 'x':: singleLineEvolution T
    | (_,_)::T -> ' ':: singleLineEvolution T
    | _ -> []


let rec evolveFromMatrixWithCountNotation mat =
    match mat with
    | H::T -> (singleLineEvolution H) :: evolveFromMatrixWithCountNotation T
    | _ -> []


let nextGenerationMatrix mat =
    evolveFromMatrixWithCountNotation (matrixToNeighboursCountForCellNotation mat)


Application.EnableVisualStyles()
Application.SetCompatibleTextRenderingDefault(false)


let brush = new SolidBrush(Color.Black)

let size = new SizeF((float32)5.0,(float32)5.0)

let rec singleaccum countx county matrix =
    match matrix with
    | 'x'::T -> new RectangleF(new PointF((float32)countx,(float32)county), size) :: singleaccum (countx + 5) county T
    | ' '::T -> singleaccum (countx + 5) county T
    | _ -> []

let rec matrixtoRectangles indexy matrix =
    match matrix with
    | H::T -> (singleaccum 0 indexy H) @ matrixtoRectangles (indexy + 5) T
    | _ -> []


let bru = new SolidBrush(Color.Black)

let paint (g:Graphics) matrix =
      matrixtoRectangles 60 matrix |> List.iter(fun x -> g.FillRectangle(bru,x))

let form = new Form(Text="Game of life")

let setupMenu () =
    let menu = new MenuStrip()
    let fileMenuItem = new ToolStripMenuItem("&File")
    let settMenuItem = new ToolStripMenuItem("&Settings")
    let exitMenuItem = new ToolStripMenuItem("&Exit")
    menu.Items.Add(fileMenuItem) |> ignore
    menu.Items.Add(settMenuItem) |> ignore
    fileMenuItem.DropDownItems.Add(exitMenuItem) |> ignore
    exitMenuItem.Click.Add(fun _ -> form.Close ())
    menu

form.MainMenuStrip <- setupMenu()
form.Controls.Add(form.MainMenuStrip)

let rand = new Random()

let rec randomrow index acc =
    if (index<300) then
        if (rand.NextDouble() > 0.5) then 
            randomrow (index + 1) (' ' :: acc) 
        else 
            randomrow (index + 1) ('x' :: acc)
    else acc

let rec randommatrix index acc =
    if index < 100 then
        [randomrow 0 []]   @ randommatrix (index + 1) acc 
    else 
        acc



let mutable mat = randommatrix 0 []


form.Paint.Add(fun e -> paint (e.Graphics)  mat)



type myDelegate = delegate of unit -> unit



form.Width <- 1600
form.Height <-700
form.Show()
//


let aloop x =
    while true do
        mat <- nextGenerationMatrix mat
        Thread.Sleep(10)
        form.Refresh()

let nst = new myDelegate(fun x  -> aloop x)

let p = form.Invoke(nst)




//[<STAThread>]
// do 
//   Application.Run(form)


  
[<TestFixture>]
 type ``lkjfdlf lkdjdfdfdfaldfa `` ()=

  [<Test>] member test.
   ``matrix to rectangles`` ()=
   Assert.AreEqual(8, List.length (matrixtoRectangles 0 [['x';'x';'x'];['x';' ';'x'];['x';'x';'x']]) )


  [<Test>] member test.
   ``for a 3 sized line of empty elements, the count of neighbours is a three sized sequence with all zero`` ()=
   Assert.AreEqual([0;0;0], countneight 0 [' ';' ';' '])

  [<Test>] member test.
   ``for a 4 sized line of empty elements, the count of neighbours is a 4 sized sequence with all zero`` ()=
   Assert.AreEqual([0;0;0;0], countneight 0 [' ';' ';' ';' '])

  [<Test>] member test.
   ``for a 4 sized line with only first non empty the count of neighbours is a 4 sized sequence with a 1 and resting 0`` ()=
   Assert.AreEqual([0;1;0;0], countneight 0 ['x';' ';' ';' '])

  [<Test>] member test.
   ``first two element non empty, 1,1,1,0 (the first has as neighbour the second, the second, the first, etc... `` ()=
   Assert.AreEqual([1;1;1;0], countneight 0 ['x';'x';' ';' '])

  [<Test>] member test.
   ``count neigh including itself`` ()=
   Assert.AreEqual([2;2;1], countneighall 0 ['x';'x';' ']  )

  [<Test>] member test.
   ``count neigh not including itself`` ()=
   Assert.AreEqual([1;1;1], countneight 0 ['x';'x';' ']  )

  [<Test>] member test.
   ``neicount should get triplets of the neighbours in the preceding line, the current line and the following line`` ()=
   Assert.AreEqual([[(0,1,2);(0,2,3);(0,1,2)];[(2,1,2);(3,2,3);(2,1,2)];[(2,1,0);(3,2,0);(2,1,0)]], neicount [' ';' ';' '] [['x';'x';'x'];
                                                                                                                            ['x';'x';'x'];
                                                                                                                            ['x';'x';'x']])

  [<Test>] member test.
   ``neicount should get triplets of the neighbours`` ()=
   Assert.AreEqual([[(0,0,1);(0,2,2);(0,0,1)];[(1,0,1);(2,2,2);(1,0,1)];[(1,0,0);(2,2,0);(1,0,0)]], neicount [' ';' ';' '] [['x';' ';'x'];['x';' ';'x'];['x';' ';'x']])

  [<Test>] member test.
   ``neicount should get triplets of the neighbour...`` ()=
   Assert.AreEqual([[(0,0,1);(0,1,2);(0,0,1)];[(0,0,1);(1,2,2);(1,0,1)];[(1,0,0);(2,2,0);(1,0,0)]], neicount [' ';' ';' '] [[' ';' ';'x'];['x';' ';'x'];['x';' ';'x']])



  [<Test>] member test.
   ``can sum triplets of neighbors count con counts in a line`` ()=
   Assert.AreEqual([5;8;6], sumsOfTriplets [(2,1,2);(3,2,3);(3,1,2)] )

  [<Test>] member test.
   ``can sum triplets of neighbors count in more lines`` ()=
   Assert.AreEqual([[5;8;6];[5;8;6]], bigsum [[(2,1,2);(3,2,3);(3,1,2)];[(2,1,2);(3,2,3);(3,1,2)]])

  [<Test>] member test.
   ``map dfsasaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaunt `` ()=
   Assert.AreEqual([[3;5;3];[5;8;5];[3;5;3]], bigsum (neicount [' ';' ';' '] [['x';'x';'x'];['x';'x';'x'];['x';'x';'x']]))

  [<Test>] member test.
   ``map dfaaaaaaaaaaaaaaaaaaaaunt `` ()=
   Assert.AreEqual([[3;5;3];[5;8;5];[3;5;3]], bigsum (neicount [' ';' ';' '] [['x';'x';'x'];['x';'x';'x'];['x';'x';'x']]))



  [<Test>] member test.
   ``fsdfds `` ()=
   Assert.AreEqual([[('x',3);('x',5);('x',3)];[('x',5);('x',8);('x',5)];[('x',3);('x',5);('x',3)]], matrixToNeighboursCountForCellNotation [['x';'x';'x'];['x';'x';'x'];['x';'x';'x']])


  [<Test>] member test.
   ``dslkfkdlflksadfas fd`` ()=
   Assert.AreEqual([[' ';'x';' '];
                    [' ';'x';' '];
                    [' ';'x';' ']], nextGenerationMatrix [[' ';' ';' '];
                                            ['x';'x';'x'];
                                            [' ';' ';' ']])

  [<Test>] member test.
   ``dsdfadfasdfslkfkdlflksadfas fd`` ()=
   Assert.AreEqual([[' ';' ';' '];
                    ['x';'x';'x'];
                    [' ';' ';' ']], nextGenerationMatrix [[' ';'x';' '];
                                            [' ';'x';' '];
                                            [' ';'x';' ']])



   [<Test>] member test.
    ``single line evolution `` ()=
    Assert.AreEqual(['x';'x';' '], singleLineEvolution [('x',3);(' ',3);(' ',1)])

