# snake-fury
Welcome to snake-fury: the Haskell challenge for beginners. This challenge aims to provide a learning path for people willing to learn Haskell further than basic recursion exercises by implementing the snake game in Haskell. The pedagogical approach followed by snake-fury is based in two legs:

- snake-fury isn't a tutorial, but a challenge.
- snake-fury is focused on learn by refactoring, not by example.

The first legs means that you'll be asked to implement some functions/algorithms but it is expected that the challenger will be unable to implement them without some research on hackage documentation, blogs, youtube videos, etc... Guidelines will be given to help the challenger, nevetheless an important skill when learning Haskell is to be able to search, read and understand documentation which is (oftenly, but not always) more complex and less accesible than other programming languages. 

The second legs is even more interesting. Haskell is notoriously known by its difficulty and by the popularization of the holy triad: Functor - Applicative - Monad. The are plenty of tutorials showing examples, hundred of thousand of lines triying to make them accesible and newcomer friendly... But with all due respect, It seems like they all fail to explain: "Why monads? Why not other less mathematical abstraction? Why not classic OOP patterns?". The approach given by snake-fury, is to make the same application twice... it sounds crazy, but the idea goes like this: You'll implement a "pure" version of the snake game. Not monads, no functors, no bullsh\*t. Then you will refactor the core application logic using the state monad, 




- You'll be asked to implement some functionality.


This challenge is divided in three parts:

- Part One 


## 

By the end of this exercise you'll have a minimum snake game running in the terminal. In the next exercise will refactor it to add some functionality and to make it more efficient. 

The general arquitecture of the software is the following:
- we have two thread. 
    - The sencondary thread is continuously reading from users keyboard and pushing the key strokes into an asynchronous queue
    - The main thread reads at steady time from the queue, and based on what the user has pressed, it runs the game logic and prints the board in the console
- We keep in memory two states.
    - the game state has all the info about the game logic
    - the render state has info for rendering. 
- The two states components communicate via messages. Every change in the game state sends a message to the render state.

The following diagram helps to visualize

```
        translate key strokes into snake
        movements. 
            ex: UpArrow -> Move North, 
                LeftArrow -> Move West, 
                etc...

                   +--------- Secondary Thread ---------+           This Thread runs continuously
                   |                    +------------+  |           So if the user pressed keys faster
(user keyboard) ----> writeUserInput -> | EventQueue |  |           Than the game logic updates, We
                   |                    +------------+  |           Still capture the key stroke.
                   +---------------------------|--------+
                                               |--->--|  
             +------------------ Main Thread ---------|--------+    This thread runs at constant time,  
             |                                        |        |    as the snake game does. Pulls event
 (draw to <---------|    |-- RenderMessage <--|    readEvent   |    from the queue and updates GameState
  console)   |      |    |                    |       |        |    
             |     +-------------+          +-----------+      |    Then, the changes in the games state
             |  |->| RenderState |->|    |->| GameState |->|   |    spawn messages for the rendering state
             |  |  +-------------+  |    |  +-----------+  |   |    
             |  |     update on     |    |    update on    |   |    The rendering state updates based on
             |  |   RenderMessage   |    |      Event      |   |    such messages
             |  |----<-------<------|    |----<-------<----|   |
             +-------------------------------------------------+ 

        Notice the the user might not press any key. In such a situation
        The readEvent function should return a Tick event. 

```

# Instructions.

In this exercise, you'll be writing two modules. 
- `Board.hs`. Here you will define types for the board and how to draw it on Console
- `Snake.hs`. Here you will defined the types for the snake and the logic of the game.

