open ExifSharp
open System.IO

[<EntryPoint>]
let main argv =
    
    ExifTool.TryGetCameraInfo @"C:\Users\Schorsch\Desktop\fz2\exif.exif" |> ignore

    // https://exiftool.org/exiftool-12.08.zip
    let files = Directory.GetFiles @"C:\Users\Schorsch\Desktop\Brusher\Datasets\JBs-HAUS_OLYMPUS_2015-09-09"

    for i in 1 .. 1 do
        let data = File.ReadAllBytes @"C:\Users\Schorsch\Desktop\fz2\exif.exif"
        let sw = System.Diagnostics.Stopwatch.StartNew()

        let iter = 10
        let mutable ci = Unchecked.defaultof<_>
        for i in 1 .. iter do
            ci <- ExifTool.TryGetCameraInfo data

        sw.Stop()

        printfn "took: %.4fs" (sw.Elapsed.TotalSeconds / float iter)


    let infos =
        [|
            @"C:\Users\Schorsch\Desktop\fz2\exif.exif"
            @"C:\Users\Schorsch\Desktop\Brusher\Datasets\hw a\P1010350.JPG"
            @"C:\Users\Schorsch\Desktop\Brusher\Datasets\vvv\2019-02-15 14.51.03.jpg"
            @"C:\Users\Schorsch\Desktop\Brusher\Datasets\gletscher\DJI_0003.JPG"
            @"C:\Users\Schorsch\Desktop\Brusher\Datasets\JBs-HAUS_OLYMPUS_2015-09-09\P9094478.JPG"
            @"C:\Users\Schorsch\Desktop\Brusher\Datasets\atti\DSC02462.ARW"
        |]
        |> Array.choose (fun f -> match ExifTool.TryGetCameraInfo f with | Some i -> Some (f, i) | _ -> None)

    for f, p in infos do
        printfn "%s" f
        printfn "  %A" p
   
    0