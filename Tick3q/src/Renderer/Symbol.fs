module Symbol
open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React
open Helpers
open CommonTypes

type Msg = Unit // no messages needed

type Model = Unit // No model needed

//-------------------------Helper functions for Tick 3-------------------------------------//

// Helper functions that are useful should normally be put into Helpers module
// For Tick3 only put Helper functions here for ease of assessment and feedback
// The obvious helpers to simplify solution are those that create and manipulate SVG elements.
// SVG boilerplate should be greatly reduced in a well-written system.

let posOf x y = {X=x;Y=y} // helper

// add your own functions as needed

//-----------------------------------------------------------------------------------------//


/// write this for Tick3 using your modified ComponentType
/// you may add to type definition in CommonTypes
let makeBusDecoderComponent (pos:XYPos) (w: int) (a: int) (n: int) = 
    {
        Type = BusDecoder (w,a,n);
        X = int pos.X;
        Y = int pos.Y;
        H = 17+10*n;
        W = 60;
    }

//-----------------------Elmish functions with no content in Tick3----------------------//

/// For this program init() generates the required result
let init () =
    (), Cmd.none

/// update function does nothing!
let update (msg : Msg) (model : Model): Model*Cmd<'a>  =
    match msg with
    | () -> model, Cmd.none // do nothing if we receive the (only) message

//----------------------------View Function for Symbol----------------------------//

/// Tick3 answer
let busDecoderView (comp: Component) = 
    let fX = float comp.X
    let fY = float comp.Y
    let fH = float comp.H
    //let fW = float comp.W
    let decoderParams = match comp.Type with
        | BusDecoder (w,a,n) -> [w;a;n]
        | _ -> failwithf "component BusDecoder type error"
    let w = decoderParams.[0]
    let a = decoderParams.[1]
    let n = decoderParams.[2]
    let height = string (10.0+fH)
    let symbolDimensions = "10,10 70,10 70,"+height+" 10,"+height
    let inputPos = 10.0+fH/2.0
    let outputList = [a..a+n-1]
    let listToReactElem i x = 
        text [ // a demo text svg element
            let yVal = float (24+10*i)
            X 61.; 
            Y yVal; 
            Style [
                TextAnchor "middle" // left/right/middle: horizontal algnment vs (X,Y)
                DominantBaseline "hanging" // auto/middle/hanging: vertical alignment vs (X,Y)
                FontSize "10px"
                FontWeight "Bold"
                Fill "Green" // demo font color
            ]
        ] [str <| sprintf "%d" x]
    let printOutNums = List.mapi listToReactElem outputList
    let reactElementList = 
        text [ // a demo text svg element
                    X 40.; 
                    Y 12.; 
                    Style [
                        TextAnchor "middle" // left/right/middle: horizontal algnment vs (X,Y)
                        DominantBaseline "hanging" // auto/middle/hanging: vertical alignment vs (X,Y)
                        FontSize "10px"
                        FontWeight "Bold"
                        Fill "Blue" // demo font color
                    ]
                ] [str <| sprintf "Bus Decode"]::

        text [ // a demo text svg element
                X 17.; 
                Y inputPos; 
                Style [
                    TextAnchor "middle" // left/right/middle: horizontal algnment vs (X,Y)
                    DominantBaseline "hanging" // auto/middle/hanging: vertical alignment vs (X,Y)
                    FontSize "10px"
                    FontWeight "Bold"
                    Fill "Blue" // demo font color
                ]
            ] [str <| sprintf "In"]::
            
        polygon [ // a demo svg polygon quad
            SVGAttr.Points symbolDimensions
            SVGAttr.StrokeWidth "2px"
            SVGAttr.Stroke "Black"
            SVGAttr.FillOpacity 0.1
            SVGAttr.Fill "grey"] []::

        printOutNums

    g   [ Style [ 
                // the transform here does rotation, scaling, and translation
                // the rotation and scaling happens with TransformOrigin as fixed point first
                TransformOrigin "0px 50px" // so that rotation is around centre of line
                Transform (sprintf "translate(%fpx,%fpx)" fX fY)
                ]
            
            ]  // g optional attributes in first list
            reactElementList

/// View function - in this case view is independent of model
let view (model : Model) (dispatch : Msg -> unit) =   

    [   // change for Tick3 answer
        makeBusDecoderComponent {X=40.; Y=10.} 7 10 10 // for Tick 3 two components
        makeBusDecoderComponent {X=160.; Y=10.} 8 8 2
    ] 
    |> List.map busDecoderView // change for Tick3 answer
    |> (fun svgEls -> 
        svg [
            Style [
                Border "3px solid green"
                Height 200.
                Width 300.   
            ]
        ]   svgEls )


type ValidateError =
   | WIsInvalid // ignoring a,n
   | AIsInvalid // for given w, ignoring n
   | NIsInvalid // for given a,w

/// Tick3 answer
let busDecoderValidate (comp:Component) : Result<Component, ValidateError*string> =

    let decoderParams comp = match comp.Type with
        | BusDecoder (w,a,n) -> (w,a,n)
        | _ -> failwithf "component BusDecoder type error"

    let w,a,n = decoderParams comp

    if w <= 0 then
        Error (WIsInvalid, "W must be greater than zero")
    elif (a < 0) || (float(a) > (2.0 ** float w) - 1.0) then
        Error (AIsInvalid, "a must be >=0 and < 2^w-1")
    elif float(a + n) > (2.0 ** float w) || n <= 0 then
        Error (NIsInvalid, "n is invalid. a + n must be <= 2^w, and n must be > 0")
    else
        Ok comp



    


