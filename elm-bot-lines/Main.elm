import Html exposing (text,pre,div)
import Html.Attributes exposing (class)
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
    div []
        [pre []
            [text arg]
        ,Grid.getSvg model.grid
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
|{io}  |   |{d}  |   |{s}  |   |cBLU |
| Foo  +-->+ Bar +---+ Baz +<--+ Moo |
|      |   |     |   |     |   |     |
+------+   +--+--+   +--+--+   +-----+
              ^         |
              |         V
.-------------+---------+-------------.
| Hello here and there and everywhere |
'-------------------------------------'


                        ____________
   .--------------.     \\           \\
  / a == b         \\     \\           \\   __________
 (    &&           )     ) process   )   \\         \\
  \\ 'string' ne '' /     /           /   ) process )
   '--------------'     /___________/    /_________/

  User code  ^            ^ OS code
              \\          /
               \\        /
                \\      /
  User code  <--- Mode ----> OS code
                /      \\
               /        \\
              /          \\
             v            v 
       User code         OS code

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
             

    """

arg2 =
    """
.-------------------------------------.
| Hello here and there and everywhere |
'-------------------------------------'
    """

arg3 =
    """
                        ____________
   .--------------.     \\\\           \\
  / a == b         \\     \\           \\   __________
 (    &&            )     ) process   )  \\         \\
  \\ 'string' ne '' /     /           /    ) process )
   '--------------'     /___________/    /_________/
    """

arg4 = 
    """
          User code ^            ^ OS code
                      \\          /
                       \\        /
                        \\      /
           User code <----Mode----->OS code
                        /      \\
                       /        \\
                      /          \\
          User code  v            v OS code
          
        
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
             


                 
                 .---------.  .---------.
                 | State 1 |  | State 2 |
                 '---------'  '---------'
                    ^   \\         ^  \\
                   /     \\       /    \\
                  /       \\     /      \\
                 /         \\   /        \\
                /           \\ /          \\
               /             v            v
            ******        ******        ******
            * T1 *        * T2 *        * T3 *
            ******        ******        ******
               ^             ^             /
                \\             \\           /
                 \\             \\         /
                  \\             \\       / stimuli
                   \\             \\     /
                    \\             \\   v
                     \\         .---------.
                      '--------| State 3 |
                               '---------'
                               

                                        .--Base::Class::Derived_A
                                       /
                                      .----Base::Class::Derived_B    
      Something--------.             /         \\
                        \\           /           '---Base::Class::Derived::More
      Something::else    \\         /             \\
            \\             \\       /               '-Base::Class::Derived::Deeper
             \\             \\     /
              \\             \\   .-----------Base::Class::Derived_C 
               \\             \\ /
                '-------Base::Class
                       /   \\ \\ \\
                      '     \\ \\ \\
                      |      \\ \\ '---The::Latest
                     /|       \\ \\      \\
 With::Some::fantasy' '        \\ \\      '----The::Latest::Greatest
                     /|         \\ \\
         More::Stuff' '          \\ '-I::Am::Running::Out::Of::Ideas
                     /|           \\
         More::Stuff' '            \\
                     /              '---Last::One
         More::Stuff'


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
             '--------------------->| A.o |           |              |
                 calls    |         '-----'           |              |
                          |    .------------------.   |              |
                          |    | A.instrumented.o |<-----------------'
                          |    '------------------'   |    calls
                          '---------------------------'   
    """


init: (Model, Cmd Msg)
init  =
    ({grid = Grid.init arg
     }
    ,Cmd.none
    )


