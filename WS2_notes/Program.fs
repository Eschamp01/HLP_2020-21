// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System

// Define a function to construct a message to print
let from whom =
    sprintf "from %s" whom


let int = 6


[<EntryPoint>]
let main argv =
    let message = from "F#" // Call the function
    printfn "Hello world %s" message

    let tuple1 = 6, "hello"

    type Person = {
        Age: int;
        FirstName: string;
        FamilyName: string;
        Email: string
    }

    let edwardSchamp = {
        Age=20;
        FirstName="Edward";
        FamilyName="Schamp";
        Email="edwardjls@gmail.com"
    }

    let name = edwardSchamp.FirstName

    let {Email="edwardjls@gmail.com"} = edwardSchamp

    let oliverSchamp = {edwardSchamp with FirstName = "Oliver"; Email="oliverpj.schamp@gmail.com"}
    let toTuple p = p.Age, p.FirstName, p.FamilyName, p.Email
    toTuple oliverSchamp

    let ageUp p = {p with Age = p.Age + 1}

    toTuple (ageUp oliverSchamp)
    toTuple oliverSchamp

    let printFamily = function
        |{FamilyName = "Schamp"} as p -> printf "%s is in the Schamp family" p.FirstName
    
    printFamily 

    //tuples used as a return value from a function
    let sumAndMean lst = 
        let length = float <| List.length lst
        let sum = float <| List.reduce(+) lst
        sum, (sum/length) //usually not the best data structure to return values, since it's unclear which is the sum / mean
    
    let lst1 = [1;4;6;8]
    sumAndMean lst1

    let hypoteneuse (x,y) = sqrt (x**2.0+y**2.0)
    let hypoteneuse2 (x,y) = (x**2.0+y**2.0) |> sqrt
    let triangle = 3.0,4.0
    hypoteneuse triangle
    hypoteneuse2 (3.0,4.0)

    //options are of type None or Some x where x is of type 'T

    let opt1 = Some 3 // val (opt1 : int option) = Some 3
    let opt2 = Some "helo" // val (opt2 : string option) = Some "hello"
    let opt3 = Some true // val (opt3 : bool option) = Some true
    let opt4 = Some (3,"hello",true, 'd') // val opt4 : (int * string * bool * char) option = Some (3, "hello", true, 'd')
    let opt5 = Some [true, "hello"] // val opt5 : (bool * string) list option = Some [true, "hello"]

    let lst1 = [-1;-3;0;5;-8]
    let lst2 = [3;5;7;8;99;20]

    let firstNeg lst = List.tryFind((>) 0) lst
    let firstNeg2 lst = List.find((>) 0) lst

    firstNeg lst1 // returns int option = Some -1
    firstNeg lst2 // returns int option = Some None

    firstNeg2 lst1 // returns val it : int = -1
    firstNeg2 lst2 // returns an error!

    let customFunction lst = 
        List.tryFind((>) 0) lst 
        |> Option.orElse (List.tryFind((<) 0) lst)
        |> Option.defaultValue 0
    
    

    let ceiling (f:float) = Math.Ceiling f

    let divideAndGetCeiling numer denom = 
        if denom = 0.0
        then Error <| sprintf("can't divide by 0")
        else Ok <| numer/denom
        |> Result.map ceiling

    divideAndGetCeiling 3.0 0.0

    let maxOf n m =
        match n > m with
        | true -> n
        | false -> m
    
    maxOf 4.1 4.2

    let lst = [1;2;3;4]

    match lst with
    | hd::t1 -> printfn "this is the head: %d \n This is the tail: %A" hd tl
    | [] -> printfn "This is an empty list"

    let rec map1 f lst = 
        match lst with
        | hd::tl -> (f hd) :: (map1 f tl)
        | [] -> []

    let rec filter f lst = 
        match lst with
        | hd::tl when f hd -> hd::(filter f tl)
        | _::tl -> filter f tl
        | [] -> []

    let list1 = [-1;1;-1;1]

    filter ((>)0) list1


    let m = Map ['3','4';'5','6']
    Map
    m |> 
    Map.toList 
    |> List.map fst

    Map.toList

    let x = [1,2;2,2;2,3]

    let compute arg = 
        match arg with
        | x, [a ; b] ->
            x * (a / b)
        | x, [ a ] ->
            x * a
        | x, [] ->
            x
        | x, _ ->
            failwithf "Invalid argument {lst} to compute"

    compute (3,[1;2])
    compute (3,[])
    compute (3,[3;4;5;6;7])

    let Fact n = 
        [1..n]
        |> List.fold (*) 2

List.fold 
(*) 2

    Fact 6



    let Reverse lst = 
        List.fold (fun lst el -> el::lst) [] lst //buildins a list using cons operator (::) builds it from tail to head

    Reverse [1;2;3;4]

    let printPipe x = printfn "%A" x; x
    
    let testPipe x =
        x
        |> List.pairwise
        |> printPipe // A
        |> List.indexed
        |> List.filter (fun (i,_) -> i % 2 <> 0)
        |> printPipe // B
        |> List.map (fun (a,(b,c)) -> a+b+c)
        |> List.sum

    //the Map type (dictionary) and how to use it

    let m = Map[4,"dog";5,"c"]
    let v = m.[4]

    let getKeysVals m = 
        m
        |> Map.toList 
        |> List.unzip
    
    let (keys,vals) = getKeysVals m

    m.[4]

    let a : Map<string,int> = Map.empty
    let a' = Map.add "hello" 4 a
    let a'' = Map.add "goodbye" 10 a'
    let b = Map.ofList ["one",1;"two",2;"three",3]

    a''.["hello"]
    a''.["goodbye"]
    a''.[""]

    let months = Map.ofList ["April",30;"June",30]//;"September",30;"November",30;"February",28]

    let daysInMonth m = 
        match Map.tryFind m months with
        | None -> 31
        | Some numOfDays -> numOfDays

    daysInMonth "April"

    let inverseMap m = 
        m
        |> Map.toList
        |> List.map (fun (k,v) -> (v,k))
        |> Map.ofList

    inverseMap months

    //obtaining a list of all keys in a Map m
    m |> Map.toList |> List.map fst

    let data = "The quick brown fox jumps over the lazy dog"

    let histogram (data:string) =
        data
        |> Seq.toList
        |> List.groupBy id
        |> Map.ofList
        |> Map.map (fun k v -> List.length v)

    histogram data |> Map.iter (fun (c,n) -> printfn "number of '%c' characters = %d" c n)

    let list1 = [10;1;3;4]

    //pipeline function form
    let gap lst = 
        lst
        |> List.sort
        |> List.pairwise
        |> List.map (fun (a,b) -> b-a)
        |> List.max

    gap list1
    
    //equivalent function brackets form
    let gap2 lst = List.max (List.map (fun (a,b) -> b-a) (List.pairwise (List.sort lst)))

    //equivalent function composition operator form (first calculates master function using all >>, then applies to lst)
    let gap3 lst = (List.sort >> List.pairwise >> List.map (fun (a,b) -> b-a) >> List.max) lst

    gap list1
    gap2 list1
    gap3 list1

    let mode lst =
        let counts = 
            lst
            |> List.countBy id
            |> List.sortByDescending snd
        match counts with
        | (m, cnt) :: _ ->
            let modes = List.takeWhile (fun (x,cnt') -> cnt' = cnt) counts
            List.map fst modes, cnt
        | [] -> failwithf "empty list"


    let list2 = [1;2;1;2;3;4;3;2]

    mode list2

    let sequences a b =
        List.allPairs [a..b] [a..b]
        |> List.filter (fun (a,b) -> b>=a)
        |> List.map (fun (a,b) -> [a..b])

    sequences 3 5

    let insertElement el index (lst:'a list) =
        lst.[0..index-1] @ [el] @ [index..lst.Length-1]
    
    insertElement 3 0 list1
    0 // return an integer exit code