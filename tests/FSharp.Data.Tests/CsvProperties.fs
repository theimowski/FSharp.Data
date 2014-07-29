#if INTERACTIVE
#r "../../bin/FSharp.Data.dll"
#r "../../packages/NUnit.2.6.3/lib/nunit.framework.dll"
#r "../../packages/FsCheck.0.9.2.0/lib/net40-Client/FsCheck.dll"
#load "../Common/FsUnit.fs"
#else
module FSharp.Data.Tests.CsvProperties
#endif

open NUnit.Framework
open System
open FSharp.Data
open FsUnit
open FsCheck

let CR = char 0x0D
let LF = char 0x0A
let separator = ","
let quoteMark = "\""
let dQuoteMark = quoteMark + quoteMark
    
let legalChars =
    [seq {0x20..0x21}
     seq {0x23..0x2B}
     seq {0x2D..0x7E}]
    |> Seq.concat
    |> Seq.map char
    |> Seq.append [CR;LF]
    |> Seq.toList

let csvStringGen : Gen<string> =   
    let quote s = sprintf "%s%s%s" quoteMark s quoteMark

    let textGen =
        Gen.arrayOf (Gen.elements legalChars)
        |> Gen.map (fun chars -> String(chars))
    
    let headerTextGen =
        Gen.nonEmptyListOf (Gen.elements legalChars)
        |> Gen.map (fun chars -> String(List.toArray chars))
        |> Gen.map (fun s -> s.Trim())

    let canonicalize (field:string) =
        if (field.Contains(quoteMark))
        then field.Replace(quoteMark, dQuoteMark) |> quote
        elif (field.Contains(separator)) then field |> quote
        elif (field |> Seq.exists ((=) CR)) then field |> quote
        elif (field |> Seq.exists ((=) LF)) then field |> quote
        else field
    
    let rowGen textGen s =
        Gen.listOfLength (1+s) (textGen |> Gen.map canonicalize)
        |> Gen.map (String.concat separator)

    let headerGen = rowGen headerTextGen
    let dataRowGen = rowGen textGen

    let emptyLine (line : string) = line.Trim(',') |> String.IsNullOrWhiteSpace

    let rowsGen s =
        (headerGen s, Gen.listOfLength (s) (dataRowGen s))
        ||> Gen.map2 (fun h t -> h :: t)
        |> Gen.map (List.filter (not << emptyLine))
        |> Gen.map (String.concat Environment.NewLine)
        |> Gen.map (fun g -> g + Environment.NewLine)

    Gen.sized rowsGen

Gen.sample 3 1 csvStringGen |> List.head

[<Test>]
let ``Stringifing parsed string returns the same string`` () =
    let stringifyParsed (s : string) =
        let csvFile = CsvFile.Parse s
        csvFile.SaveToString() = s
    let csvStringArb = Arb.fromGen (csvStringGen)
    Check.QuickThrowOnFailure (Prop.forAll csvStringArb stringifyParsed)

``Stringifing parsed string returns the same string`` ()

[<Literal>]
let x =
    """ f ,z
k ,
"""

let csv = CsvFile.Parse x
csv.Headers |> Option.iter (Array.iter (printfn "%s"))
csv.Rows |> Seq.map (fun r -> r.Columns.[0])
csv.SaveToString()