module ScrabbleBot

open ScrabbleUtil
open System.IO

    val startGame :
        boardProg ->                   (* Scrabble board *)
        string ->                      (* Alphabet *)
        string list ->                 (* Dictionary *)
        uint32 ->                      (* Number of players *)
        uint32 ->                      (* Your player number *)
        uint32 ->                      (* starting player number *)
        (uint32 * uint32) list ->      (* starting hand (tile id, number of tiles) *)
        Map<uint32, tile> ->           (* Tile lookup table *)
        uint32 option ->               (* Timeout in miliseconds *)
        Stream ->                      (* Communication channel to the server *)
        (unit -> unit)                 (* Delay to allow everyone to start at the same time after startup *)
