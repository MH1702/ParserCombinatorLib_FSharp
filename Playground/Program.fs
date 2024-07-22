open System.IO

let main() = 
    use reader = new StreamReader (".\Test1.json")
    let str = reader.ReadToEnd ()

    printf "%A" (Json.Parse str)

main()