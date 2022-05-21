# snake-fury
By the end of this exercise you'll have a minimum snake game running in the terminal. In the next exercise well refactor it to add some functionality and to make it more efficient. 

The general arquitecture of the software is the following:
- we have two thread. 
    - The sencondary thread is continuously reading from users key board and pushing the key strokes into an asynchronous queue
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
(user keyboard) -> |  writeUserInput -> | EventQueue |  |           Than the game logic updates, We
                   |                    +------------+  |           Still capture the key stroke.
                   +---------------------------|--------+
                                               |--->--|  
             +------------------ Main Thread ---------|--------+    This thread runs at constant time,  
             |                                        |        |    as the snake game does. Pulls event
 (draw to <- |  <---|    |-- RenderMessage <--|    readEvent   |    from the queue and updates GameState
  console)   |      |    |                    |       |        |    
             |    +-------------+           +-----------+      |    Then, the changes in the games state
             |  |->| RenderState |->|    |->| GameState |->|   |    spawn messages for the rendering state
             |  |  +-------------+  |    |  +-----------+  |   |    
             |  |     update on     |    |    update on    |   |    The rendering state updates based on
             |  |   RenderMessage   |    |      Event      |   |    such messages
             |  |----<-------<-----.|    |----<-------<----|   |
             +-------------------------------------------------+ 

        Notice the the user might not press any key. In such a situation
        The readEvent function should return a Tick event. 

```

# Instructions.

In this exercise, you'll be writing two modules. 
- `Board.hs`. Here you will define types for the board and how to draw it on Console
- `Snake.hs`. Here you will defined the types for the snake and the logic of the game.

