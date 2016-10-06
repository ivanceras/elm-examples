port module Main exposing (..)
import Html exposing (text,pre,div,button,textarea)
import Html.Attributes exposing (class, style,contenteditable,id)
import Html.Events exposing (onClick,on)
import String
import Diagram
import Html.App as App
import Json.Decode exposing (string)
import Regex
import Diagram


{-- code which detects lines and connections
also algorithmn to connect these lines
--}

type alias Model =
 {grid: Diagram.Model
 }

type Msg = Input String | Convert


init: (Model, Cmd Msg)
init  =
    ({grid = Diagram.init arg
     }
    ,Cmd.batch[
        setAsciiText arg
    ]
    )

textContentDecoder =
  Json.Decode.at ["target", "textContent"] string



view model =
    div [style 
            [("display", "flex")
            ]
        ]
        [
        div[] 
           [button [onClick Convert
                   ] 
                   [text "Convert >>"]
           ]
        ,div 
            [style [("width", "100%")
                   ,("height", "auto")
                   ,("overflow", "auto")
                   ]
            ]
            [
            let _ = Debug.log "start svg" ""
                svg =Diagram.getSvg model.grid
                _ = Debug.log "end svg" ""
            in svg
            ]
        ]

        
update msg model =
    case msg of
        Convert ->
            (model, getAsciiText () )
        Input asciiText ->
            ({model | grid = Diagram.init asciiText}
            ,Cmd.none
            )

main =
    App.program
    {init = init
    ,view = view
    ,update = update
    ,subscriptions = subscriptions
    }

subscriptions: Model -> Sub Msg
subscriptions model =
    Sub.batch [receiveAsciiText Input]

port getAsciiText: () -> Cmd msg

port setAsciiText: String -> Cmd msg

port receiveAsciiText: (String -> msg) -> Sub msg




arg = 
    """

+------+   +-----+   +-----+   +-----+
|      |   |     |   |     |   |     |
| Foo  +-->| Bar +---+ Baz |<--+ Moo |
|      |   |     |   |     |   |     |
+------+   +-----+   +--+--+   +-----+
              ^         |
              |         V
.-------------+-----------------------.
| Hello here and there and everywhere |
'-------------------------------------'


                        ____________
   .--------------.     \\           \\
  / a == b         \\     \\           \\     __________
 (    &&            )     ) process   )    \\         \\
  \\ 'string' ne '' /     /           /     / process /
   '--------------'     /___________/     /_________/


    __________________
    \\                 \\
     \\                 \\
      . another process .
     /                 /
    /_________________/

  User code  ^               ^ OS code
              \\             /
               \\        .--'
                \\      /
  User code  <--- Mode ----> OS code
                /      \\
            .--'        \\___
           /                \\
          v                  v 
       User code            OS code

             .---.  .---. .---.  .---.    .---.  .---.
    OS API   '---'  '---' '---'  '---'    '---'  '---'
               |      |     |      |        |      |
               v      v     |      v        |      v
             .------------. | .-----------. |  .-----.
             | Filesystem | | | Scheduler | |  | MMU |
             '------------' | '-----------' |  '-----'
                    |       |      |        |
                    v       |      |        v
                 .----.     |      |    .---------.
                 | IO |<----'      |    | Network |
                 '----'            |    '---------'
                    |              |         |
                    v              v         v
             .---------------------------------------.
             |                  HAL                  |
             '---------------------------------------'
             

   ____[]
  | ___ |
  ||   ||  device
  ||___||  loads
  | ooo |----------------------------------------------------------.
  | ooo |    |                          |                          |
  | ooo |    |                          |                          |
  '_____'    |                          |                          |
             |                          |                          |
             v                          v                          v
   .-------------------.  .---------------------------.  .-------------------.
   | Loadable module C |  |     Loadable module A     |  | Loadable module B |
   '-------------------'  |---------------------------|  |   (instrumented)  |
             |            |         .-----.           |  '-------------------'
             '------------+-------->| A.o |           |             |
                 calls    |         '-----'           |             |
                          |    .------------------.   |             |
                          |   / A.instrumented.o /<---+-------------'
                          |  '------------------'     |    calls
                          '---------------------------'   


                                        .--> Base::Class::Derived_A
                                       /
                                      .----> Base::Class::Derived_B    
      Something -------.             /         \\
                        \\           /           .---> Base::Class::Derived
      Something::else    \\         /             \\
            \\             \\       /               '--> Base::Class::Derived
             \\             \\     /
              \\             \\   .-----------> Base::Class::Derived_C 
               \\             \\ /
                '------ Base::Class
                       /  \\ \\ \\
                      '    \\ \\ \\  
                      |     \\ \\ \\
                      .      \\ \\ '--- The::Latest
                     /|       \\ \\      \\
 With::Some::fantasy  '        \\ \\      '---- The::Latest::Greatest
                     /|         \\ \\
         More::Stuff  '          \\ '- I::Am::Running::Out::Of::Ideas
                     /|           \\
         More::Stuff  '            \\
                     /              '--- Last::One
       More::Stuff  V 

  Safety
    ^
    |                       *Rust
    |           *Java
    | *Python
    |                        *C++
    +-----------------------------> Control

    ^
    :
    :
    :
    :
    +==============================>
    
    ..............................



  TODO:
        
      ^ ^ ^
       \\|/
        . 
       /|\\
      v V v 

      ^ ^ ^
       \\|/
      <-.->
       /|\\
      v V v 

        |   \\/   
       -+-  /\\      
        |   
        
        |      |    |      |
        +--  --+    +--  --+   +--  --+
                    |      |   |      |

                     |    |  |     |
             .- -.   .-  -.  ._   _.
             |   |

        .-   -.  .-.       
        '-   -'  | |  | |  
                      '-'

      \\      |    /  |
       .     '   '   .
       |    /    |    \\ 

        \\    / 
         .  .
        /    \\

       .    .
      /|    |\\

      
      \\|   |/
       '   '

       \\
       /

       /
       \\


       /      \\
      '--    --'
     /          \\

       /   \\
    --'     '--
     /       \\

                       \\         /
       --.--  --.--   --.--   --.--
        /        \\     


        |   |
        .   .
       /|   |\\ 

        |
        .
       / \\

       \\|/
        .
       /|\\

       
       \\|/
      --.--
       /|\\

       \\|/
      --+--
       /|\\
        
        |/  \\|
        .    .
        |    |


       -.  -.
       /     \\

        .-  .-
       /     \\

      
       /   /     \\    \\
      '-  '_     _'   -'
       

       .-.
      (   )
       '-'

       ..
      (  )
       ''


       .------.
      (        )
       '------'

        ________  
       /       /
      /       /
     /_______/


        ________  
        \\       \\
         \\       \\
          \\_______\\

       ________ 
      |________|


       ________ 
      |        |
      |________|

      .-.
      '-'

        ________  
        \\_______\\

       /\\
      /  \\
     /____\\

       /\\
      /  \\
     /    \\
    '------'

       ___
      /   \\
      \\___/

      ______
     /      \\
    /        \\
    \\        /
     \\______/
        

        +---------+
        |         |                        +--------------+
        |   NFS   |--+                     |              |
        |         |  |                 +-->|   CacheFS    |
        +---------+  |   +----------+  |   |  /dev/hda5   |
                     |   |          |  |   +--------------+
        +---------+  +-->|          |  |
        |         |      |          |--+
        |   AFS   |----->| FS-Cache |
        |         |      |          |--+
        +---------+  +-->|          |  |
                     |   |          |  |   +--------------+
        +---------+  |   +----------+  |   |              |
        |         |  |                 +-->|  CacheFiles  |
        |  ISOFS  |--+                     |  /var/cache  |
        |         |                        +--------------+
        +---------+
    """

