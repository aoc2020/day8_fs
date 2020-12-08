// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.IO
open System.Text.RegularExpressions

let read_file (fileName:String) = seq {
    use sr = new StreamReader(fileName)
    while not sr.EndOfStream do
        sr.ReadLine ()
}

let instRE = new Regex("^([a-z]+) ([+-][0-9]+)")

let parse inst =
    let m = instRE.Match inst
    let opCode = m.Groups.[1].Value 
    let value = m.Groups.[2].Value |> int
    (opCode,value)


type Instruction (op:String, value:int) as self =
    member this.opCode = op
    member this.param = value
    
    member this.print =
        printfn "[%s] [%d]" op value
        
let to_instr (i:String): Instruction =
    let (op,value) = i |> parse
    new Instruction(op,value)

type Program = Map<int,Instruction>
let readProgram (instr: seq<Instruction>): Program = instr |> Seq.mapi (fun i x -> i,x) |> Map.ofSeq

type VM (program: Program, acc:int, ip:int, visited: Set<int>) as self =
    
    member this.exec_inst () =
        let op = program.[ip].opCode
        let param = program.[ip].param
        let newVisited = visited.Add ip
        match op with
        | "jmp" -> new VM (program,acc,ip+param,newVisited)
        | "acc" -> new VM (program,acc+param,ip+1,newVisited)
        | "nop" -> new VM (program,acc,ip+1,newVisited)
    member this.second_visit () = visited.Contains ip
    member this.getAccumulator () = acc
    member this.terminated () = program.ContainsKey ip |> not
    member this.terminatedCorrectly () =
        this.terminated  () && program.ContainsKey (ip-1)
       
let rec run_until_stop (vm:VM):VM =
    if vm.terminated() || vm.second_visit () then vm
    else run_until_stop (vm.exec_inst ())
    
// Define a function to construct a message to print

let change_program (program:Program) (ip:int) : Program =
    let inst = program.[ip]
    let value = inst.param
    let new_op = match inst.opCode with
                      | "jmp" -> "nop"
                      | "nop" -> "jmp"
                      | _ -> inst.opCode 
    let new_inst = new Instruction(new_op,value)
    program.Add (ip,new_inst) 
    
let rec try_all_mods (program:Program) (ip:int) : VM =
    let new_program = change_program program ip
    let new_vm = new VM (new_program,0,0,Set.empty)
    let stopped_vm = run_until_stop new_vm
    if stopped_vm.terminatedCorrectly ()
    then stopped_vm
    else try_all_mods program (ip+1)
    
[<EntryPoint>]
let main argv =
    let input = read_file("/Users/xeno/projects/aoc2020/day8_fs/input.txt")
    let program : Program = input |> Seq.map to_instr |> readProgram 
    let vm = new VM(program, 0 ,0, Set.empty)
    let looped_vm = run_until_stop vm
    printfn "%d" (looped_vm.getAccumulator())    
    let term_vm = try_all_mods program 0
    printf "%d" (term_vm.getAccumulator())

    0 // return an integer exit code