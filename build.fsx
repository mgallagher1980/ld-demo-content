#I "/tools/packages/NICE.Freya/tools"
#r "/tools/packages/NICE.Freya/tools/FSharp.Markdown.dll"
#r "/tools/packages/NICE.Freya/tools/freya.exe"
#r "/tools/packages/NICE.Freya/tools/ExtCore.dll"
#r "/tools/packages/NICE.Freya/tools/SharpYaml.dll"
#r "/tools/packages/NICE.Freya/tools/FSharp.RDF.dll"

open Freya
open Freya.Builder
open Freya.YamlParser
open FSharp.Markdown
open Freya.Markdown
open FSharp.RDF


let mkKey (x : string) = x.Replace(" ", "").ToLowerInvariant()

///Load all resources from uri and make a map of rdfs:label -> resource uri
let vocabLookup uri =
  let rdfslbl = Uri.from "http://www.w3.org/2000/01/rdf-schema#label"
  let gcd = Graph.loadFrom uri
  let onlySome = List.choose id
  Resource.fromPredicate rdfslbl gcd
  |> List.map (fun r ->
       match r with
       | FunctionalDataProperty rdfslbl xsd.string x ->
         Some(mkKey x, Resource.id r)
       | r -> None)
  |> onlySome
  |> Map.ofList


///Map of annotation vocab name to vocabulary
let lookupVocab =
  ([ "setting",
     vocabLookup "http://schema/ns/qualitystandard/setting.ttl"

     "agegroup",
     vocabLookup "http://schema/ns/qualitystandard/agegroup.ttl"

     "lifestylecondition",
     vocabLookup
       "http://schema/ns/qualitystandard/lifestylecondition.ttl"

     "conditiondisease",
     vocabLookup "http://schema/ns/qualitystandard/conditiondisease.ttl"

     "servicearea",
     vocabLookup "http://schema/ns/qualitystandard/servicearea.ttl" ]
   |> Map.ofList)

///Map of annotation vocabulary name to restricted property
let lookupProperty =
  ([ "setting", Uri.from "http://ld.nice.org.uk/ns/qualitystandard#setting"

     "agegroup",
     Uri.from "http://ld.nice.org.uk/ns/qualitystandard#targetPopulation"

     "conditiondisease",
     Uri.from "http://ld.nice.org.uk/ns/qualitystandard#targetPopulation"

     "servicearea",
     Uri.from "http://ld.nice.org.uk/ns/qualitystandard#serviceArea"

     "lifestylecondition",
     Uri.from "http://ld.nice.org.uk/ns/qualitystandard#targetPopulation" ]
   |> Map.ofList)

type YNode = Freya.YamlParser.Node

open FSharp.RDF.Assertion
open rdf

let owlAllValuesFrom property  = function
  | [] -> []
  | ranges -> [ for r in ranges -> objectProperty property r ]

let qsAnnotations ctx =
  let message f x = f x (Tracing.fileLocation ctx.Path)
  let info = message Tracing.info
  let warn = message Tracing.warn
  let onlySome = List.filter Option.isSome >> List.map Option.get

  //Pairs of trace message / annotation uri
  let lookUpScalar vocabKey =
    function
    | Node.Scalar(Scalar.String term) ->
      printfn "%A %A" vocabKey term
      match Map.tryFind (mkKey vocabKey) lookupVocab with
      | Some vocab ->
        match Map.tryFind (mkKey term) vocab with
        | Some uri -> (info (sprintf "Annotating for term %s" term), Some uri)
        | None -> (warn (sprintf "Cannot find '%s' in '%s'" term vocabKey), None)
      | None -> (warn (sprintf "Cannot find vocabulary '%s'" vocabKey), None)
    | _ -> (warn (sprintf "Malformed yaml"), None)

  let extracted =
    match ctx.Content with
    | Map xs ->
      xs
      |> List.map (function
           | k, YNode.List xv ->
             (Map.tryFind (mkKey k) lookupProperty,
              List.map (lookUpScalar (mkKey k)) xv)
           | k, _ -> (None, []))
      |> List.map (function
           | Some k, xs ->
             (List.map fst xs,
              owlAllValuesFrom k ((List.map snd xs |> onlySome)) )
           | _, xs -> (List.map fst xs, []))
    | _ -> []

  { Trace = List.concat (List.map fst extracted)
    Extracted =  List.map snd extracted
                 |> List.map (owl.individual ctx.TargetId [] )}


let qsDC (ctx:ExtractionContext<FSharp.Markdown.MarkdownDocument>) =

  let message f x = f x (Tracing.fileLocation ctx.Path)
  let info = message Tracing.info
  let warn = message Tracing.warn

  let h1 = ctx.Content.Paragraphs
           |> List.choose MarkdownParagraph.hAny
           |> List.map MarkdownSpan.text
           |> List.map (fun x -> rdf.dataProperty !!"http://purl.org/dc/terms/title" (x^^xsd.string) )
           |> List.tryHead

  let p1 = ctx.Content.Paragraphs
           |> MarkdownParagraph.following
                   (MarkdownParagraph.h3 >>= (MarkdownSpan.re ".*(Q|q)uality.*(S|s)tatement.*"))
           |> List.map MarkdownParagraph.text
           |> List.map (fun x -> rdf.dataProperty !!"http://purl.org/dc/terms/abstract" (x^^xsd.string) )
           |> List.tryHead


  {Extracted = [rdf.resource ctx.TargetId (( Option.toList h1 ) @ (Option.toList p1 ))]
   Trace = [
        if Option.isNone h1 then yield (warn "No title found")
        if Option.isNone p1 then yield (warn "No abstract found")
  ]}


markdownExtractor "QsDC" qsDC

yamlExtractor "QsAnnotations" qsAnnotations


target "QualityStandards" (dir "qualitystandards")
target "QualityStandardDir" (dir "qs$(QualityStandardId)")
target "QualityStatementDir" (dir "st$(QualityStatementId)")
target "QualityStatement" (file "Statement.md"
                                ["Content";"QsDC";"QsAnnotations";"HtmlFragment"]
                                (Some "/templates/QualityStatement.md")
                                "owl:NamedIndividual")
target "QualityStandard" (file "Standard.md"
                               ["Content"]
                               (Some "/templates/QualityStandard.md")
                               "owl:NamedIndividual")

"QualityStandards"
===> ["QualityStandardDir"
      ===> ["QualityStandard"
            "QualityStatementDir"
                  ===> ["QualityStatement"]]]
