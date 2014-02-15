module Base64
open Data

let base64 (x:bytes) : string = System.Convert.ToBase64String x
let ibase64 (x:string) : byte[] = System.Convert.FromBase64String x
