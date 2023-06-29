module RPS
    open System

    let random = Random ()
    let rand = random.NextDouble ()

    type Choice = 
    | Rock
    | Paper 
    | Scissors 

    let getString = function
    | Rock -> "Rock"
    | Paper -> "Paper"
    | Scissors -> "Scissors"

    let beats (a : Choice, b : Choice) =
        match a, b with 
        | Rock, Scissors -> true // Rock beats scissors
        | Paper, Rock -> true // paper beats rock
        | Scissors, Paper -> true // Scissors beats paper
        | _, _ -> false // every other combination does not beat

    // Computer move
    let genMove r p s =
        let tot = r + p + s 
        let n = rand 
        if n <= s / float tot then Rock
        elif n <= (s + r) / float tot then Paper
        else Scissors 

    // Player move
    let rec getMove () =
        printf "[R]ock, [P]aper, or [S]cissors?: "
        let choice = Console.ReadLine ()
        match choice with
        | "r" | "R" -> Rock
        | "p" | "P" -> Paper
        | "s" | "S" -> Scissors
        | _ ->
            printf "Invalid choice.\n\n"
            getMove ()

    let rec game (r : float, p: float, s: float) =
        let comp = genMove r p s 
        let player = getMove ()
        Console.WriteLine ("Player: {0} vs Computer: {1}", getString player, getString comp)
        Console.WriteLine (
            if beats(player, comp) then "Player wins!\n"
            elif beats(comp, player) then "Computer Wins!\n"
            else "Draw!\n"
        )

        let nextR = if player = Rock then r + 1.0 else r
        let nextP = if player = Paper then p + 1.0 else p
        let nextS = if player = Scissors then s + 1.0 else s 
        game (nextR, nextP, nextS)

    //module Program =
    //  [<EntryPoint>]
    // let main argv = 
        //    game (1.0, 1.0, 1.0)
        //   0

    game(1.0, 1.0, 1.0)