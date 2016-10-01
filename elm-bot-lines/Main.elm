import Html exposing (text,pre,div)
import Html.Attributes exposing (class, style,contenteditable)
import String
import Grid
import Html.App as App


{-- code which detects lines and connections
also algorithmn to connect these lines
--}

type alias Model =
 {grid: Grid.Model
 }

type Msg = DoSomething


view model =
    let _ = Debug.log "model" model
        _ = Debug.log "get 0 0 " <| Grid.getElement 0 2 model.grid
    in
    div [style [("display", "flex")]]
        [pre 
            [style 
                [("font-size", "13px")
                ,("word-wrap","nowrap")
                ,("overflow", "auto")
                ]
              ,contenteditable True
             ]
            [text arg]
        ,div [style [("padding-left", "10px")]]
            [Grid.getSvg model.grid
            ]
        ]
        
update msg model =
    (model, Cmd.none)

main =
    App.program
    {init = init
    ,view = view
    ,update = update
    ,subscriptions = subscriptions
    }

subscriptions: Model -> Sub Msg
subscriptions model =
    Sub.batch []




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
  | ooo |------------------------------------------------------------.
  | ooo |    |                          |                            |
  | ooo |    |                          |                            |
  '_____'    |                          |                            |
             |                          |                            |
             v                          v                            v
   .-------------------.  .---------------------------.    .-------------------.
   | Loadable module C |  |     Loadable module A     |    | Loadable module B |
   '-------------------'  |---------------------------|    |   (instrumented)  |
             |            |         .-----.           |    '-------------------'
             '------------+-------->| A.o |           |              |
                 calls    |         '-----'           |              |
                          |    .------------------.   |              |
                          |   / A.instrumented.o /<---+--------------'
                          |  '------------------'     |    calls
                          '---------------------------'   


                                        .--> Base::Class::Derived_A
                                       /
                                      .----> Base::Class::Derived_B    
      Something -------.             /         \\
                        \\           /           \\---> Base::Class::Derived::More
      Something::else    \\         /             \\
            \\             \\       /               '--> Base::Class::Derived::Deeper
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
         More::Stuff 

  Safety
    î
    |                       *Rust
    |           *Java
    | *Python
    |                        *C++
    +-----------------------------> Control

Junctions

   

    |   \\/   
   -+-  /\\      
    |   
    

                 |    |  |     |
         .- -.   .-  -.  ._   _.
         |   |

    .-   -.  .-.       
    '-   -'  | |  | |  
                  '-'

  \\      |    /  |
   .     '   '   .
   |    /    |    \\ 

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



    |   |
    .   .
   /|   |\\ 

   -.  -.
   /     \\

    .-  .-
   /     \\

  
   /   /     \\    \\
  '-  '_     _'   -'
   

Shapes

circle
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
    

    """

init: (Model, Cmd Msg)
init  =
    ({grid = Grid.init arg
     }
    ,Cmd.none
    )


