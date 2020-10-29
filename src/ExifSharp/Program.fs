namespace ExifSharp

open System
open System.Diagnostics
open System.Threading
open System.Threading.Tasks
open System.IO
open FSharp.Data.JsonProvider
open Newtonsoft.Json
open Newtonsoft.Json.Linq
open ICSharpCode.SharpZipLib.Zip
open System.Runtime.InteropServices

#nowarn "9"

type ImageOrientation =
    | Normal = 1
    | MirrorX = 2
    | Rot180 = 3
    | MirrorY = 4
    | Transpose = 5
    | Rot270 = 6
    | Transverse = 7
    | Rot90 = 8

[<StructuredFormatDisplay("{AsString}")>]
type CameraInfo =
    {
        width           : int
        height          : int
        cropFactor      : float
        make            : string
        model           : string
        focal           : option<float>
        aperture        : option<float>
        exposureTime    : option<float>
        lensMake        : option<string>
        lensModel       : option<string>
        exif            : JObject
    }

    member x.sensorWidth = 
        let len = sqrt (float x.width * float x.width + float x.height * float x.height)
        let d = 43.26661531 / (x.cropFactor * len)
        float x.width * d
    
    member x.sensorHeight = 
        let len = sqrt (float x.width * float x.width + float x.height * float x.height)
        let d = 43.26661531 / (x.cropFactor * len)
        float x.height * d
    
    member private x.AsString =
        x.ToString()

    override x.ToString() =
        let props =
            [
                "size", sprintf "%dx%d" x.width x.height
                "crop", sprintf "%.3f" x.cropFactor

                let model = 
                    if x.model.ToLower().StartsWith (x.make.ToLower()) then x.model.Substring(x.make.Length).Trim()
                    else x.model

                "name", sprintf "%s %s" x.make model

                let lensMake = 
                    match x.lensMake with
                    | Some l -> l
                    | None -> x.make
                match x.lensModel with
                | Some m ->
                    let m = 
                        if m.ToLower().StartsWith (lensMake.ToLower()) then m.Substring lensMake.Length
                        else m
                    "lens", sprintf "%s %s" lensMake m
                | None ->
                    if lensMake <> x.make then
                        "lens", lensMake
                //match x.lensModel with
                //| Some l -> "lensModel", sprintf "%A" l
                //| None -> ()
                
                
                match x.exposureTime  with
                | Some f -> "exposureTime", sprintf "%.3gs" f
                | None -> ()

                match x.focal  with
                | Some f -> "focal", sprintf "%.3g" f
                | None -> ()

                match x.aperture with
                | Some a -> "aperture", sprintf "%.3g" a
                | None -> ()
            ]
        props 
        |> Seq.map (fun (n,v) -> sprintf "%s: %s" n v)
        |> String.concat ", "
        |> sprintf "CameraInfo { %s }"


module private ExifHelpers =
    open System.Text.RegularExpressions

    let withUnitRx = Regex @"([0-9]+(?:\.[0-9]+)?)(?:[ \t]*([a-zA-Z_0-9]+))?"

    let rationalRx = Regex @"([0-9]+)[ \t]*\/[ \t]*([0-9]+)"


    let parseFocalLength(str : string) =
        let m = withUnitRx.Match str
        if m.Success then
            let unit = 
                if m.Groups.[2].Success then m.Groups.[2].Value.ToLower()
                else "mm"

            match unit with
            | "mm" -> m.Groups.[1].Value |> float |> Some
            | _ -> None

        else
            None


    let parseRational (str : string) =
        match System.Double.TryParse(str, System.Globalization.NumberStyles.Float, System.Globalization.CultureInfo.InvariantCulture) with
        | (true, v) -> Some v
        | _ ->
            let m = rationalRx.Match str
            if m.Success then
                let a = m.Groups.[1].Value |> int
                let b = m.Groups.[2].Value |> int

            
                float a / float b |> Some

            else
                None

module private JsonHelper =

    let private value<'a> (t : JToken) =
        if typeof<'a> = typeof<JToken> then 
            t |> unbox<'a> |> Some
        elif typeof<'a> = typeof<JObject> then 
            match t with
            | :? JObject as o -> o |> unbox<'a> |> Some
            | _ -> None
        elif typeof<'a> = typeof<JArray> then
            match t with
            | :? JArray as o -> o |> unbox<'a> |> Some
            | _ -> None
        elif typeof<'a> = typeof<string> then
            try Some (t.ToObject<string>().Trim() |> unbox<'a>)
            with _ -> None
        else
            try Some (t.ToObject<'a>())
            with _ -> None

    let tryGet<'a> (name : string) (t : JToken) =
        match t with
        | :? JObject as o ->
            match o.TryGetValue name with
            | (true, t) ->
                value<'a> t
            | _ ->
                None
        | :? JArray as a ->
            match System.Int32.TryParse name with
            | (true, idx) when idx >= 0 && idx < a.Count ->
                a.[idx] |> value<'a>
            | _ ->
                None
        | _ ->
            None

    type Getter private() =
        static member Get<'a>(t : JToken, name : string) = tryGet<'a> name t
        static member Get<'a>(t : Option<JToken>, name : string) =
            match t with
            | Some t -> tryGet<'a> name t
            | None -> None

    let inline private getAux (d : ^d) (a : ^a) (name : ^b) : ^c =
        ((^a or ^b or ^d) : (static member Get : ^a * ^b -> ^c) (a, name))

    let inline (?) a b =
        getAux Unchecked.defaultof<Getter> a b

    type JToken with
        member x.TryGet<'a> name =
            tryGet<'a> name x
            
        member x.TryGet<'a> names =
            names |> List.tryPick (fun name -> tryGet<'a> name x)

    type OptionBuilder() =
        member x.Bind(m : option<'a>, f : 'a -> option<'b>) =
            match m with
            | Some a -> f a
            | None -> None

        member x.Return v = Some v
        
        member x.ReturnFrom (v : option<'a>) = v

        member x.Delay (f : unit -> option<'a>) = f

        member x.Zero() = Some ()

        member x.Run(f : unit -> option<'a>) = f()

        member x.Combine(l : option<unit>, r : unit -> option<'a>) =
            match l with
            | Some () -> r()
            | None -> None

        member x.For(seq : seq<'a>, action : 'a -> option<unit>) =
            use e = seq.GetEnumerator()
            let rec run () =
                if e.MoveNext() then
                    match action e.Current with
                    | Some () -> run()
                    | None -> None
                else
                    Some ()
            run()

    let option = OptionBuilder()

    let whiteSpace = System.Text.RegularExpressions.Regex @"[ \t\r\n]+" 

    let pascalCase (str : string) =
        whiteSpace.Split str 
        |> Array.map (fun w ->
            if w.Length > 0 then
                let h = w.Substring(0, 1)
                let t = w.Substring 1
                h.ToUpper() + t.ToLower()
            else
                w
        )
        |> String.concat " "


    let parse (res : string) =
        let arr = JArray.Parse res
        if arr.Count > 0 then
            option {
                let o = arr.[0] :?> JObject

                let! make = o.TryGet<string> "Make"
                let! model = o.TryGet<string> "Model"

                let! w = o.TryGet<int> ["ImageWidth"; "ExifImageWidth"; "CanonImageWidth"; "OriginalImageWidth"]
                let! h = o.TryGet<int> ["ImageHeight"; "ExifImageHeight"; "CanonImageHeight"; "OriginalImageHeight"]

                let focal = o.TryGet<string> "FocalLength" |> Option.bind ExifHelpers.parseFocalLength
                let aperture = o.TryGet<string> "Aperture" |> Option.bind ExifHelpers.parseRational
                let exposureTime = o.TryGet "ExposureTime" |> Option.bind ExifHelpers.parseRational
                let lensId = o.TryGet<string> "LensID"
                let lensMake = o.TryGet<string> "LensMake"

                let make =
                    let make =
                        let oo = "olympus imaging corp."
                        let idx = make.ToLower().IndexOf oo
                        if idx >= 0 then make.Substring(0, idx) + "Olympus" + make.Substring(idx + oo.Length)
                        else make

                    make.Trim()
                    |> pascalCase

                //let lensModel = lensId
                //    match o.TryGet<string> "LensModel" with
                //    | Some model ->
                //        let lModel = model.Trim().ToLower()
                //        let lMake = make.ToLower()
                //        if lModel.StartsWith lMake then
                //            model.Substring(lMake.Length).Trim() |> Some
                //        else
                //            Some model
                //    | None ->
                //        None

                let focal, cropFactor =
                    match o.TryGet<float> "ScaleFactor35efl" with
                    | Some crop ->
                        focal, crop
                    | None -> 
                        match o.TryGet<float> "FocalLength35efl" with
                        | Some f35 -> 
                            match focal with
                            | Some f -> focal, f35 / f
                            | None -> Some f35, 1.0
                        | None ->
                            let c = if make.ToLower().Contains "canon" then 1.6 else 1.5
                            focal, c

                let orientation =
                    match o.TryGet<int> "Orientation" with
                    | Some str -> unbox<ImageOrientation> str
                    | None -> ImageOrientation.Normal

                let w, h =
                    match orientation with
                    | ImageOrientation.Transpose
                    | ImageOrientation.Transverse
                    | ImageOrientation.Rot90
                    | ImageOrientation.Rot270 ->
                        h, w
                    | _ ->
                        w, h


                return {
                    width           = w
                    height          = h
                    cropFactor      = cropFactor
                    make            = make
                    model           = model
                    focal           = focal
                    aperture        = aperture
                    exposureTime    = exposureTime
                    lensMake        = lensMake
                    lensModel       = lensId
                    exif            = o
                }

                //printfn "%s" make
                //printfn "%s" model
                //focal |> Option.iter (printfn "%.3fmm")
                //exposure |> Option.iter (printfn "%.3f")
                //return! None
            }
        else
            None

open JsonHelper

module private WindowsChildProcessTracker =
    open System.Runtime.InteropServices
    open Microsoft.FSharp.NativeInterop

    type JOBOBJECTLIMIT =
        | JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE = 0x2000u

    [<StructLayout(LayoutKind.Sequential)>]
    type JOBOBJECT_BASIC_LIMIT_INFORMATION =
        struct 
            val mutable public   PerProcessUserTimeLimit    : Int64         
            val mutable public   PerJobUserTimeLimit        : Int64         
            val mutable public   LimitFlags                 : JOBOBJECTLIMIT
            val mutable public   MinimumWorkingSetSize      : UIntPtr       
            val mutable public   MaximumWorkingSetSize      : UIntPtr       
            val mutable public   ActiveProcessLimit         : UInt32        
            val mutable public   Affinity                   : Int64         
            val mutable public   PriorityClass              : UInt32        
            val mutable public   SchedulingClass            : UInt32        
        end
        
    [<StructLayout(LayoutKind.Sequential)>]
    type IO_COUNTERS =
        struct
            val mutable public ReadOperationCount   : uint64
            val mutable public WriteOperationCount  : uint64
            val mutable public OtherOperationCount  : uint64
            val mutable public ReadTransferCount    : uint64
            val mutable public WriteTransferCount   : uint64
            val mutable public OtherTransferCount   : uint64
        end
        
    [<StructLayout(LayoutKind.Sequential)>]
    type JOBOBJECT_EXTENDED_LIMIT_INFORMATION =
        struct
            val mutable public BasicLimitInformation    : JOBOBJECT_BASIC_LIMIT_INFORMATION
            val mutable public IoInfo                   : IO_COUNTERS
            val mutable public ProcessMemoryLimit       : UIntPtr
            val mutable public JobMemoryLimit           : UIntPtr
            val mutable public PeakProcessMemoryUsed    : UIntPtr
            val mutable public PeakJobMemoryUsed        : UIntPtr
        end


    type JobObjectInfoType =
        | AssociateCompletionPortInformation = 7
        | BasicLimitInformation = 2
        | BasicUIRestrictions = 4
        | EndOfJobTimeInformation = 6
        | ExtendedLimitInformation = 9
        | SecurityLimitInformation = 5
        | GroupInformation = 11

    [<DllImport("kernel32.dll", CharSet = CharSet.Unicode)>]
    extern IntPtr CreateJobObject(IntPtr lpJobAttributes, string name)

    [<DllImport("kernel32.dll")>]
    extern bool SetInformationJobObject(IntPtr job, JobObjectInfoType infoType,
        IntPtr lpJobObjectInfo, uint32 cbJobObjectInfoLength)

    [<DllImport("kernel32.dll", SetLastError = true)>]
    extern bool AssignProcessToJobObject(IntPtr job, IntPtr proc)


    type Job(name : string) =
        let handle = 
            let handle = CreateJobObject(0n, name)

            let mutable info = JOBOBJECT_BASIC_LIMIT_INFORMATION()
            info.LimitFlags <- JOBOBJECTLIMIT.JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE

            let mutable extendedInfo = JOBOBJECT_EXTENDED_LIMIT_INFORMATION()
            extendedInfo.BasicLimitInformation <- info

            use ptr = fixed [| extendedInfo |]
            let len = sizeof<JOBOBJECT_EXTENDED_LIMIT_INFORMATION>
            if not (SetInformationJobObject(handle, JobObjectInfoType.ExtendedLimitInformation, NativePtr.toNativeInt ptr, uint32 len)) then
                failwith "bad"

            handle

        member x.Add(proc : Process) =
            if proc.Handle <> 0n then
                if not (AssignProcessToJobObject(handle, proc.Handle)) then
                    failwith "could not assign process"

type private ExifToolProcess(path : string) =

    static let job = 
        if RuntimeInformation.IsOSPlatform OSPlatform.Windows then
            try Some (WindowsChildProcessTracker.Job "ExifSharpExifTool")
            with _ -> None
        else    
            None

    static let tags =
        List.map (sprintf "-%s") [
            "Make"
            "Model"
            "ImageWidth"; "ExifImageWidth"; "CanonImageWidth"; "OriginalImageWidth"
            "ImageHeight"; "ExifImageHeight"; "CanonImageHeight"; "OriginalImageHeight"
            "FocalLength#"
            "Aperture#"
            "ExposureTime#"
            "LensMake"
            "LensID"
            "ScaleFactor35efl#"
            "FocalLength35efl#"
            "Orientation#"
        ]

    let info =
        ProcessStartInfo(
            path,
            "-stay_open 1 -@ -",
            RedirectStandardOutput = true,
            RedirectStandardInput = true,
            RedirectStandardError = true,
            CreateNoWindow = true,
            UseShellExecute = false
        )

    let temp = Path.ChangeExtension(Path.GetTempFileName(), ".exif")

    let proc = Process.Start info
    do match job with | Some job -> job.Add proc | None -> ()
    let stdin = proc.StandardInput

    //let mutable currentFile = ""
    let json = System.Text.StringBuilder()

    let finished = 
        let finished = Event<Result<string, string>>()
        proc.OutputDataReceived.Add (fun e ->
            if isNull e.Data then
                //if currentFile <> "" then
                //    try File.Delete currentFile
                //    with _ -> ()
                //    currentFile <- ""

                finished.Trigger(Error(proc.StandardError.ReadToEnd()))

            elif e.Data = "{ready}" then
                //if currentFile <> "" then
                //    try File.Delete currentFile
                //    with _ -> ()
                //    currentFile <- ""

                finished.Trigger(Ok(json.ToString()))
                json.Clear() |> ignore
            else
                json.AppendLine e.Data |> ignore
        )
        proc.BeginOutputReadLine()
        finished.Publish

    member x.Run (exif : byte[]) =
        lock x (fun () ->
            let result = ref None
            use s = 
                finished.Subscribe (fun e ->
                    lock result (fun () ->
                        result := Some e
                        Monitor.PulseAll result
                    )
                )

            //let temp = Path.ChangeExtension(Path.GetTempFileName(), ".exif")
            File.WriteAllBytes(temp, exif)
            //currentFile <- temp

            stdin.WriteLine("-j")
            for t in tags do
                stdin.WriteLine t
            stdin.WriteLine "-All"
            stdin.WriteLine(temp)
            stdin.WriteLine("-execute")

            
            lock result (fun () ->
                while Option.isNone !result do
                    Monitor.Wait result |> ignore

                match result.Value.Value with
                | Ok res -> res
                | Error err -> failwithf "error: %A" err
            )
        )
    
    member x.Run (file : string) =
        lock x (fun () ->
            let result = ref None
            use s = 
                finished.Subscribe (fun e ->
                    lock result (fun () ->
                        result := Some e
                        Monitor.PulseAll result
                    )
                )

            stdin.WriteLine("-j")
            for t in tags do
                stdin.WriteLine t
            stdin.WriteLine "-All"
            stdin.WriteLine(file)
            stdin.WriteLine("-execute")

            
            lock result (fun () ->
                while Option.isNone !result do
                    Monitor.Wait result |> ignore
                    
                match result.Value.Value with
                | Ok res -> res
                | Error err -> failwithf "error: %A" err
            )
        )
    
    member x.Shutdown() =
        lock x (fun () ->
            stdin.WriteLine "-stay_open"
            stdin.WriteLine "False"
        )
        proc.WaitForExit()

type ExifTool private (path : string) =
    let bag = System.Collections.Concurrent.ConcurrentBag<ExifToolProcess>()
    do for i in 1 .. 1 do bag.Add(ExifToolProcess path)

    static let defaultPath =
        lazy (
            let dir = 
                Path.Combine(
                    Environment.GetFolderPath Environment.SpecialFolder.LocalApplicationData,
                    "exifsharp"
                )
            let path = 
                Path.Combine(dir, "exiftool.exe")

            if File.Exists path then
                path
            else
                if not (Directory.Exists dir) then Directory.CreateDirectory dir |> ignore
                use c = new System.Net.WebClient()

                let l = obj()
                let mutable data : byte[] = null
                Console.Write "downloading ExifTool"
                let sw = System.Diagnostics.Stopwatch()

                c.DownloadProgressChanged.Add (fun e ->
                    let p = float e.BytesReceived / float e.TotalBytesToReceive
                    Console.Write("\rdownloading ExifTool {0:0.0}%   ", 100.0 * p)
                )
                c.DownloadDataCompleted.Add (fun e ->
                    
                    lock l (fun () ->
                        data <- e.Result
                        Monitor.PulseAll l
                    )
                )
                sw.Start()
                c.DownloadDataAsync(Uri "https://exiftool.org/exiftool-12.09.zip")

                lock l (fun () ->
                    while isNull data do
                        Monitor.Wait l |> ignore
                )


                use arch = new ZipFile (new MemoryStream(data))
                let entry = 
                    Seq.init (int arch.Count) (fun i -> arch.EntryByIndex i)
                    |> Seq.find (fun e -> e.Name.ToLower().EndsWith ".exe")

                use r = arch.GetInputStream entry
                use w = File.OpenWrite path
                let buffer = Array.zeroCreate (1 <<< 20)
                let mutable cont = true
                while cont do
                    let b = r.Read(buffer, 0, buffer.Length)
                    if b > 0 then
                        w.Write(buffer, 0, b)
                    else
                        cont <- false

                sw.Stop()
                Console.WriteLine(sprintf "\rdownloading ExifTool took %.1fs" sw.Elapsed.TotalSeconds)
                path
        )

    static let instance =
        lazy (new ExifTool(defaultPath.Value))

    member private x.RunInternal(data : byte[]) =
        match bag.TryTake() with
        | (true, p) ->
            let json = p.Run data
            bag.Add p
            parse json
        | _ ->
            let p = ExifToolProcess path
            let json = p.Run data
            bag.Add p
            parse json
            
    member private x.RunInternal(file : string) =
        match bag.TryTake() with
        | (true, p) ->
            let json = p.Run file
            bag.Add p
            parse json
        | _ ->
            let p = ExifToolProcess path
            let json = p.Run file
            bag.Add p
            parse json
            
    static member TryGetCameraInfo (data : byte[]) =
        instance.Value.RunInternal data

    static member TryGetCameraInfo (file : string) =
        instance.Value.RunInternal file
