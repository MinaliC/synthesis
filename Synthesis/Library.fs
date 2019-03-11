module Synthesis

let abelar a = (a>12 && a<3097 && a%12=0) 
    //failwith "Not implemented"

let area b h = 
    match b>=0.0 && h>=0.0 with 
        |true -> (b*h*0.5)
        |_ -> failwith "ShouldFail"     
    //failwith "Not implemented"

let zollo a  =
    match a<=0 with
        |true -> a * -1
        |false -> a*2
        |_ -> failwith "Not implemented"

let min a b =
    match a<b with 
        |true -> a
        |false -> b

let max a b =
    match a>b with  
        |true->a
        |false ->b
   

let ofTime h m s =(h*3600+m*60+s)
   // failwith "Not implemented"

let toTime a =
    match a>=0 with
        |false ->0,0,0
        |true ->
         let h=a/3600
         let h1=a-(h*3600)
         let m=h1/60
         let s=h1-(m*60)
         h,m,s
    //failwith "Not implemented"

let digits n =
    let rec counter a c=
        match a=0 with
            |true ->c
            |false ->counter(a/10) (c+1)
    match n>0 || n<0 with
    |true ->counter n 0
    |false ->1

   // failwith "Not implemented"

let minmax (num1,num2,num3,num4) =
    let minVal=min num1 num2 |> min num3 |> min num4 
    let maxVal=max num1 num2 |> max num3 |> max num4
    minVal,maxVal
    //failwith "Not implemented"

let isLeap a =
    match a<1582 with   
    |true->failwith "Not implemented"
    |false->
        match (a%4=0 && not(a%100=0)) with
        |true->true
        |false->
            match (a%400=0) with
            |true->true
            |false->false
    

let month m =
    match m with
    |1->("January", 31)
    |2->("February", 28)
    |3->("March", 31)
    |4->("April", 30)
    |5->("May", 31)
    |6->("June", 30)
    |7->("July", 31)
    |8->("August", 31)
    |9->("September", 30)
    |10->("October", 31)
    |11->("November", 30)
    |12->("December", 31)
    |_->failwith "Not implemented"

let toBinary a =
    let rec bin v r=
        match v=0 with 
            |true ->r
            |false->
               match v%2 with
               |0->bin (v/2) "0"+r
               |_->bin (v/2) "1"+r
    match a<0 with 
       |true-> failwith "Not implemented"
       |_ ->
        match a=0 with
            |true->"0"
            |false->bin a ""

let bizFuzz num =
    let rec div v (acc1,acc2,acc3)=
        match num<v with   
            |true->(acc1,acc2,acc3)
            |false->
                match  v%3=0 && v%5=0 with
                |true->div (v+1) (acc1+1 ,acc2+1, acc3+1)
                |false->
                    match (v%3=0) with
                        |true -> div (v+1) (acc1+1 ,acc2, acc3)
                        |false ->
                            match(v%5=0)with
                                |true->div (v+1) (acc1 ,acc2+1, acc3)
                                |false -> div (v+1) (acc1,acc2,acc3)
    match num<1 with
        |true->0,0,0
        |false->div 1 (0,0,0)

let monthDay _ _ =
    failwith "Not Implemented"

(*let monthDay d y =
        match d>0 && d<=366 && y>1582 with
            |false->failwith "Not Implemented"
            |true->
                match isLeap y=true with
            |true-> match d<=366 && y >1582 with
                        |true->match 12*d/208 with
                                |1->"January"
                                |2->"February"
                                |3->"March"
                                |4->"April"
                                |5->"May"
                                |6->"June"
                                |7->"July"
                                |8->"August"
                                |9->"September"
                                |10->"October"
                                |11->"November"
                                |12->"December"
                                |_->failwith "Not Implemented"
                        |false-> failwith "Not Implemented"
            |false-> match d<=365 && y >1582 with
                        |true-> match 12*d/208 with
                                |1->"January"
                                |2->"February"
                                |3->"March"
                                |4->"April"
                                |5->"May"
                                |6->"June"
                                |7->"July"
                                |8->"August"
                                |9->"September"
                                |10->"October"
                                |11->"November"
                                |12->"December"
                                |_->failwith "Not Implemented"
                        |false->failwith "Not Implemented"
   *)             

let sqrt n=
    let rec calculate guess i=
     match i with
         |10->guess
         |_->
            let g=(guess+n/guess)/2.0
            calculate g (i+1)
    match n<=0.0 with
        |true ->failwith "Impossible"
        |_->
            calculate(n/2.0) 0

let coord _=
    failwith "Not Implemented"
// my code for circle, the test fails 
(*let coord  c =
     let dist (x1,y1) (x2,y2) = 
        let ans=sqrt((x1-x2)*(x1-x2)+(y1-y2)*(y1-y2))
        ans
        let obj()=ans
        let within  a b =
           match x1>x2 && x1<(x2+a) && y1>(y2-b) && y1<y2 with 
            |true-> true
            |_->false
        within
     dist *)

         
            


   

        
         
    
     
                    