---
title: Part Two - Gamepad control
has-toc: yes
date: 2016-04-7
description: Adding simple character control
---

*tl;dr* In this entry I'll add the ability to control a character using a USB 
gamepad.

![a gamepad](/img/gamepad.JPG){.img-responsive}

I'll write mostly about getting SDL2 gamepad events into the network and then
using those events to move some graphics.

Intro
================================================================================
I think in the last couple articles I set expectations a little high. I wrote 
(in so many words) that these were tutorials on how to use FRP to write a game.
Maybe I was flippant, it's more likely I was just excited (still am). This 
series is more about showing off some libraries that I've written and exploring 
the space around writing games in Haskell using said libraries. Keep in mind 
both my FRP implementation 'varying' and graphics lib 'gelatin' are both in 
heavy flux and neither are production ready. By the end of the series I hope to 
have profiled my libraries, figured out some of what works and what doesn't and 
have some working code, moving closer to 1.0 releases on hackage. I'm also going 
to try to keep these posts short, which may make it possible to write them 
quicker.

> -- |
> --   Module:     Main
> --   Copyright:  (c) 2016 Schell Scivally
> --   License:    MIT
> --
> --   The entrypoint to part-two of the odin series. 
> module Main where
 
Imports and Infrastructure Updates
================================================================================
I'm still using 'sdl2', but 'gelatin-sdl2' no longer re-exports it because I ran
into a lot of export conflicts.

> import Gelatin.SDL2
> import SDL hiding (Event, time)
> import Control.Varying
> import Control.Monad

I moved a ton of the infrastructure into an App directory. App.Control.Monad 
contains all the types needed to construct the network's monad and run some 
effects. App.Control.FRP contains the various App level behaviors (which I call
streams for simplicity) - so it has all the mouse button 'Event' streams, key
event streams, etc. that are used to build up the network. App.Framework 
contains the main loop and surrounding functions. 
 
> import App.Control.Monad
> import App.Control.FRP
> import App.Framework

Adding New Streams
================================================================================
The first step to getting joystick events flowing through the network is to open
the joystick. 'sdl2' sends a kind of joystick event whenever a joystick is
plugged in or unplugged, so I added some "add" and "remove" events to 
'App.Control.Monad' 

~~~.haskell
data AppEvent = AppEventNone
              -- ...
              | AppEventJoystickAdded !Int32
              | AppEventJoystickRemoved !Int32
~~~

along with a bit of registration code in 'App.Framework.handleEvents' 

~~~.haskell
handleEvent (JoyDeviceEvent (JoyDeviceEventData iid)) = do
  vjoys <- availableJoysticks
  let fjoys = V.filter ((== iid) . fromIntegral . joystickDeviceId) vjoys
  if V.length fjoys >= 1
    then do j <- openJoystick $ V.head fjoys
            jid <- getJoystickID j
            return $ AppEventJoystickAdded jid
    -- Probably need to closeJoystick ;)
    else return $ AppEventJoystickRemoved iid
~~~

The call to 'openJoystick' tells 'sdl2' to start listening for events on that
joystick. Once we're listening 'sdl2' will push joystick events into the queue,
which will need to be handled in a similar fasion.

~~~.haskell
data AppEvent = AppEventNone
              -- ...
              | AppEventJoystickAxis !Int32 !Word8 !Int16
              | AppEventJoystickBall !Int32 !Word8 !(V2 Int16)
              | AppEventJoystickHat !Int32 !Word8 !Word8
              | AppEventJoystickButton !Int32 !Word8 !Word8

-- ...
handleEvent (JoyAxisEvent (JoyAxisEventData jid axis val)) =
  return $ AppEventJoystickAxis jid axis val
handleEvent (JoyBallEvent (JoyBallEventData jid ball rel)) =
  return $ AppEventJoystickBall jid ball rel
handleEvent (JoyHatEvent (JoyHatEventData jid hat val)) =
  return $ AppEventJoystickHat jid hat val
handleEvent (JoyButtonEvent (JoyButtonEventData jid btn st)) =
  return $ AppEventJoystickButton jid btn st
~~~

That gets the events from 'sdl2' into our network. Now we can write some FRP
streams. (This is in App.Control.FRP)

~~~.haskell
--------------------------------------------------------------------------------
-- Joystick stuff
--------------------------------------------------------------------------------
joystickAddedEvent :: Monad m => VarT m AppEvent (Event Int32)
joystickAddedEvent = var f ~> onJust
  where f (AppEventJoystickAdded iid) = Just iid
        f _ = Nothing

joystickRemovedEvent :: Monad m => VarT m AppEvent (Event Int32)
joystickRemovedEvent = var f ~> onJust
  where f (AppEventJoystickRemoved iid) = Just iid
        f _ = Nothing

joystickAxisEvent :: Monad m => Int32 -> Word8 -> VarT m AppEvent (Event Int16)
joystickAxisEvent jid axis = var f ~> onJust
  where f (AppEventJoystickAxis kid axis1 val) = if (jid,axis) == (kid,axis1)
                                                   then Just val
                                                   else Nothing
        f _ = Nothing

joystickBallEvent :: Monad m => Int32 -> Word8 -> VarT m AppEvent (Event (V2 Int16))
joystickBallEvent jid ball = var f ~> onJust
  where f (AppEventJoystickBall kid ball1 rel) = if (jid,ball) == (kid,ball1)
                                                   then Just rel
                                                   else Nothing
        f _ = Nothing

joystickHatEvent :: Monad m => Int32 -> Word8 -> VarT m AppEvent (Event Word8)
joystickHatEvent jid hat = var f ~> onJust
  where f (AppEventJoystickHat kid hat1 val) = if (jid,hat) == (kid,hat1)
                                                  then Just val
                                                  else Nothing
        f _ = Nothing

joystickButtonEvent :: Monad m => Int32 -> Word8 -> VarT m AppEvent (Event Word8)
joystickButtonEvent jid btn = var f ~> onJust
  where f (AppEventJoystickButton kid btn1 st) = if (jid,btn) == (kid,btn1)
                                                   then Just st
                                                   else Nothing
        f _ = Nothing
~~~

Writing the Network
================================================================================
So - now that the infrastructure is all in place we can write the actual
network code. We'll describe two toons - a red one and a blue one - the red one
will move along with the left analog stick and the blue will move with the right
analog stick. When the user presses the "A" button we'll reset.

> speed :: Monad m => VarT m a (V2 Float)
> speed = pure 1
>
> leftXVel :: VarT Effect AppEvent Float
> leftXVel = joystickAxisPressureEvent 0 0 ~> startingWith 0 
>
> leftYVel :: VarT Effect AppEvent Float
> leftYVel = joystickAxisPressureEvent 0 1 ~> startingWith 0 
>
> leftVel :: VarT Effect AppEvent (V2 Float)
> leftVel = V2 <$> leftXVel <*> leftYVel
>
> leftPos :: VarT Effect AppEvent (V2 Float)
> leftPos = (leftVel * speed) ~> accumulate (+) 0 
>
> rightXVel :: VarT Effect AppEvent Float
> rightXVel = joystickAxisPressureEvent 0 2 ~> startingWith 0 
>
> rightYVel :: VarT Effect AppEvent Float
> rightYVel = joystickAxisPressureEvent 0 3 ~> startingWith 0 
>
> rightVel :: VarT Effect AppEvent (V2 Float)
> rightVel = V2 <$> rightXVel <*> rightYVel
>
> rightPos :: VarT Effect AppEvent (V2 Float)
> rightPos = (rightVel * speed) ~> accumulate (+) 0 

> network :: SplineT AppEvent Pic Effect ()
> network = do
>   halfSize <- ((/2) . (fmap fromIntegral)) <$> getWindowSize
>   let lefty p = move (halfSize - (V2 10 0) + p) $ 
>                   draw $ colored $ rectangle 0 10 $ const red
>       righty p = move (halfSize + (V2 10 0) + p) $ 
>                    draw $ colored $ rectangle 0 10 $ const blue 
>       both l r = lefty l >> righty r
>
>   (both <$> leftPos <*> rightPos) `_untilEvent_` joystickButtonEvent 0 1 1 
>   infoStr "A Button pressed!"
>   step blank
>   network

Main Loop
================================================================================
The 'runApp' function takes a continuous stream of (in this case) 2d pictures 
to represent our app, so we have to take our network sequence and squash it 
down into a stream of 'Pic'

> appSignal :: VarT Effect AppEvent Pic
> appSignal = outputStream network blank
 
And then we run our app using a renderer that can render those pictures each
frame

> main :: IO ()
> main = runApp picAppRender appSignal "Odin"

Now we play!

<iframe 
  src="https://player.vimeo.com/video/162132050" 
  width="500" 
  height="332" 
  frameborder="0"
  webkitallowfullscreen 
  mozallowfullscreen 
  allowfullscreen>
</iframe>
