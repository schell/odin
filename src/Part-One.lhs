---
title: Part One - Infrastructure
has-toc: yes
date: 2016-02-03
description: Creating the infrastructure for Odin
---

Intro
================================================================================
I'd like to learn how to write a push based FRP implementation and so I decided
I would take a stab at flipping my pull based FRP into a pushy one. My FRP is
called [varying][1]. It's very simple and inspired by [netwire][4]. It uses 
automatons to generate a stream of varying values - hence the name. For info on 
the core concepts of varying check out the [hackage docs][varying core]. 
[varying][1] is a rather squishy FRP (and I use the term FRP liberally) but it's 
fun and simple. In this article we'll be building a quick demo 'game' to 
demonstrate how to set up the infrastructure needed for a bigger application. 

Get the Code
--------------------------------------------------------------------------------
This is a Literate Haskell file which can be downloaded from the 
[github repo][odin]. To build, run `stack build` from the project
directory. [Go here](http://docs.haskellstack.org/en/stable/README.html) 
for help with `stack`.

Main
================================================================================

> -- |
> --   Module:     Main
> --   Copyright:  (c) 2015 Schell Scivally
> --   License:    MIT
> --
> --   The entrypoint to part-one of the odin series. 
> module Main where

Our first import is [varying][1], which allows us to describe values 
that change over time and user input.

> import Control.Varying

Next up is the graphics library [gelatin-picture][2], which we use to 
describe two dimensional pictures. Since we'll be rendering for desktop with 
glfw we will use [gelatin-glfw][3]. It's currently the only backend for gelatin.  
[gelatin-glfw][3] re-exports [GLFW-b][glfw-b] and [gelatin-picture][2] so we 
don't have to clutter our workspace with those imports.

> import Gelatin.GLFW

Next we'll need some infrastructure in the form of `TVar` and `WriterT`. We'll
use `TVar`s to synchronize updates across threads and `WriterT` to allow our
network entities to reach out to the world and each other.

> import Control.Concurrent
> import Control.Concurrent.Async
> import Control.Concurrent.STM.TVar
> import Control.Monad.STM
> import Control.Monad.Trans.Writer.Strict
> import Control.Monad
> import Control.Monad.IO.Class

Lastly we'll need some miscellaneous bits and pieces.

> import Data.Bits ((.|.))
> import Data.Time.Clock
> import qualified Data.Set as S
> import System.Exit

Types
================================================================================
We need to be able to describe our game and since this is Haskell we'll use
lots of types.

`UserInput` will represent everything we want to push into our FRP network. If
our game or display logic needs to know about it, it should be covered by 
`UserInput`.

> data UserInput = InputUnknown String
>                | InputTime Float
>                | InputCursor Float Float
>                | InputWindowSize Int Int
>                deriving (Show)

`UserInput` is a monoid of sorts - each new event replaces an old one unless
the new one is `InputUnknown`. An empty event is `InputUnknown`. We need this
to use `stepMany` during [the network step][#the-network-step], because 
`stepMany` requires a Monoid instance.

> instance Monoid UserInput where
>     mappend a (InputUnknown _) = a
>     mappend _ b = b
>     mempty = InputUnknown ""

We use `OutputEvent` with `WriterT` to allow entities within
our network to have a very managed effect on the network as a whole.

> data OutputEvent = OutputEventUnknown String
>                  | OutputNeedsUpdate
>                  deriving (Ord, Eq)

> type Effect = Writer [OutputEvent]

We'll be rendering `Picture`s from [gelatin-picture][2] using `Font`s provided
by [FontyFruity][fonty]. [FontyFruity][fonty] is also re-exported by gelatin's 
glfw backend. We'll talk more about rendering in the [rendering][#rendering] 
section.

> type Pic = Picture Font ()

The `Network` is a varying value. This means that it represents a value that
changes over some domain. When you see the type of a varying value as 
`VarT m a b` it means that an output value `b` varies over input `a` within an 
effect `m`. [varying][1] is an arrowized FRP implementation with a twist, so if 
you've ever used [netwire][4] you'll be a bit familiar. Some differences 
between [varying][1] and [netwire][4] are

* [varying][1]'s inhibition is explicit using the type `VarT m a (Event b)`. 
* [varying][1]'s time is not encoded in its type.

Here our `Network` is defined as a varying value that depends on `UserInput`s
and produces a `Pic` inside the `Effect` monad. The `Effect` monad allows
our network streams to call out to the rest of the world, writing attempted
side-effects to the network as a whole through our `WriterT` monad stack.

> type Network = VarT Effect UserInput Pic

Rendering
================================================================================
In order to render a frame we'll need a function that uses our cache of 
renderers and the window reference in `Rez` to paint a `Pic` to the screen. The 
pic is created by our network each frame and each time we paint a frame update 
we'll get back a cache of updated renderers. 

> renderFrame :: Rez -> Cache IO Transform -> Pic 
>             -> IO (Cache IO Transform)
> renderFrame rez cache pic = do

Just like any other opengl app we need to set the viewport and clear our buffers 
before updating the screen.

>   (fbw,fbh) <- getFramebufferSize $ rezWindow rez
>   glViewport 0 0 (fromIntegral fbw) (fromIntegral fbh)
>   glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT

Now we render our data using `renderPrims` and `pictureToR2Primitives`.  Defined 
in [renderable][5] and [gelatin-picture][2] respectively, they do most of the 
heavy lifting for us. `pictureToR2Primitives` turns our `Pic` into renderable 
primitives - colored and textured lines and triangles. `renderPrims` renders the 
list of primitives and returns a new cache, allocating new renderings for us 
on-the-fly and cleaning up resources previously allocated by now stale renderers.

>   newCache <- renderPrims rez cache $ pictureToR2Primitives pic

Now swap the buffers on our OpenGL window and return the new cache.

>   swapBuffers $ rezWindow rez
>   shouldClose <- windowShouldClose $ rezWindow rez
>   if shouldClose then exitSuccess else threadDelay 100
>   return newCache 
 
A Bit More Infrastructure
================================================================================
Finally we can declare our main infrastructure type, `AppData`. An `AppData`
contains the `Network` in its current state, a cache of renderers that will
be managed by [renderable][5], and a list of user input events.

> data AppData = AppData { appNetwork :: Network
>                        , appCache   :: Cache IO Transform
>                        , appEvents  :: [UserInput]
>                        , appUTC     :: UTCTime
>                        }

The Network
================================================================================
We need a network to test our infrastructure, so for now we'll use the simplest 
network I can think of that demonstrates change over time and user input - a 
circle that follows the mouse, changing shape and color over time. 

Cursor Move Events
--------------------------------------------------------------------------------
In order to do this we'll first need a stream of cursor move events. We'll be 
using `V2 Float` from [linear][linear] to represent our points.
    
> cursorMoved :: (Applicative m, Monad m) => VarT m UserInput (Event (V2 Float))

This type signature shows that our stream will 'consume' user input and 'emit'
position events. 

For the implementation we use the `var` constructor to turn a pure function into 
a stream 

> cursorMoved = var f -- :: VarT m InputEvent (Maybe (V2 Float))

and then combine that with the event generator `onJust` which takes a `Maybe a` 
as input and produces a stream of `Event a`. We also use the plug right `~>` 
combinator to plug the output of `var f` into the input of `onJust`.

>     ~> onJust

And now we write our function that maps input values to `Maybe (V2 Float)`.

>     where f (InputCursor x y) = Just $ V2 x y
>           f _ = Nothing

Cursor Position
--------------------------------------------------------------------------------
Next we need a stream that produces the current cursor position each frame. 
Until the cursor moves we need a default cursor position. `V2 -1 -1` seems 
pretty good.

> cursorPosition :: (Applicative m, Monad m) => VarT m UserInput (V2 Float)
> cursorPosition = cursorMoved ~> foldStream (\_ v -> v) (-1)

`foldStream` works just like `foldl`, but it only operates on streams of Events,
folding event values into an accumulator. In this case the accumulator is simply
the latest cursor position.

Time Deltas
--------------------------------------------------------------------------------
To demonstrate that our pull network can be run on demand we'll describe a 
circle that follows the cursor *and* changes shape and color over time. 
Eventually the circle will stop changing shape and color so unless there's an 
input event we should see the network *go to sleep*. 

So far we have a stream for the cursor position but now we'll need a 
stream of time deltas. The reason we need deltas is that we'll use some 
tweening streams from [varying][1] that require deltas as input. This breaks 
some of the rules of hard FRP - we're not supposed to deal in deltas, but I think 
you'll find that once it's done we don't have to use deltas for anything more 
than a plug. 

> timeUpdated :: (Applicative m, Monad m) => VarT m UserInput (Event Float)
> timeUpdated = var f ~> onJust
>     where f (InputTime t) = Just t
>           f _ = Nothing

We need to fill in the gaps in `timeUpdated` when it doesn't produce an event.

> deltas :: (Applicative m, Monad m) => VarT m UserInput Float
> deltas = 0 `orE` timeUpdated

The implementation of `deltas` says "produce `0` unless `timeUpdated` produces
an event - if so, produce the value of that event.

Requesting Updates
--------------------------------------------------------------------------------
In order to have smooth animation over time we need to know that the network 
requires frequent updates. In our main loop we'll block until user input 
happens or until the network requests that itself be updated.  

Using the monadic constructor `varM` we write a stream that can reach out to the 
Writer monad, make the request and pass whatever input it received through as 
output. It's a bit hacky as far as FRP goes (we're only using its side-effect), 
but this is what's required to use our system in a pushy fashion.

> requestUpdate :: VarT Effect a a
> requestUpdate = varM $ \input -> do
>     tell [OutputNeedsUpdate] 
>     return input

You can see that both `var` and `varM` use regular functions to create a stream.
The input to the stream is the only parameter to the function and the output
of the function becomes the output of the stream. Check out the other 
constructors in the [varying docs][varying constructors].

Time
--------------------------------------------------------------------------------
We'll combine our `deltas` stream and `requestUpdate` in order to create a 
`time` stream. Whenever a part of our network depends on `time` the underlying 
infrastructure should receive a request that the network be updated. If no part 
of the network depends on time our main loop will only render on user input.

> time :: VarT Effect UserInput Float
> time = deltas ~> requestUpdate

Since we now have time flowing through our network we can use the tweening
capabilities that ship with [varying][1] in **Control.Varying.Tween**. Tweens 
run one step higher in abstraction in order to play with varying values that are 
only defined over a select domain. These are called splines. 

Tweening With Splines
--------------------------------------------------------------------------------
In [varying][1] a spline is a temporary changing value that has an end
result. Splines are essentially chains of event streams - when an event stream
stops producing, the current spline terminates and the next event stream takes 
over. The input, output and result value of a spline are encoded in the spline's 
type signature. A 'Spline a b m c' describes a spline that takes 'a's as input,
produces 'b's as output - runs in the 'm' monad and results in 'c'. Our tweening
spline will take time (`Float`) as input, return `Float` as output and result
in the last tweened output value, `Float`. We'll want to change the duration of
the spline so we'll write a function that takes the duration and returns our
spline.

> easeInOutSpline :: (Applicative m, Monad m) 
>                 => Float -> SplineT Float Float m Float
> easeInOutSpline t = do
>     halfway <- tween easeInExpo 1 0 $ t/2

Above we tween from `1` to `0` over `t/2` seconds using an easing function.
The spline produces the interpolated values until `t/2` seconds, and then 
results in the last interpolated value, which is either `0` or very close to 
`0`. 

Now we complete the tween.

>     tween linear halfway 1 $ t/2

As you can see, with splines we can use a monadic notation. Since a spline is 
only defined over a certain domain it can be considered to terminate, giving an 
end result and allowing you to chain another spline. This chaining or sequencing 
is what makes splines monadic. 

Actually Using Splines
--------------------------------------------------------------------------------
So we've got a spline that represents a tweened number over time, but splines
are only continuous over a domain and we'd like to use a completely continuous
signal to tween our `Pic`. `Spline` runs at one level of abstraction above `VarT`
and as such, we can run the `Spline` to turn it back into a `VarT`. All we need
is to use `execSpline` and give it an initial value. The resulting `VarT` will
produce the values of the `Spline` until the `Spline` concludes. Once the 
`Spline` terminates the `VarT` will use the last value forever. If the spline 
*never* produces, the initial value will be produced forever. 

> easeInOutExpo :: (Applicative m, Monad m) => Float -> VarT m Float Float 
> easeInOutExpo = outputStream 1 . easeInOutSpline

We'd like to demonstrate that the network "turns off" when time is no longer
a dependency, which means we'll have to set up a network that depends on time
for a while and then moves on. Since this sounds like sequencing event streams
we'll use splines again.

> multSequence :: Float -> SplineT UserInput Float Effect Float

Note how the type signature now contains `Effect` since we'll need to use time,
which requires the ability to write out update requests.

> multSequence t = do
>     (val,_) <- (time ~> easeInOutExpo t) `untilEvent` (time ~> after t)
>     return val

Above we plug time into two streams - the first is our tweening stream and the
second is an event stream we use as a timer. `time ~> after t` will produce 
`Event ()` forever after `t` seconds. We combine the two streams using the 
combinator `untilEvent`. This combinator produces output values using the first
stream until an event occurs in the second stream, then returns the last values
of each in a tuple. `after` will produce `Event ()`, and `untilEvent` unwraps
the event value for you, tupling it up - making the right value of our spline's 
result `()`, which we can ignore.

All together, `multSequence` is a spline that depends on time for `t` seconds
and then returns `1` forever. This should help us demonstrate that the network
no longer depends on time and can render sporatically whenever a user event
comes down the pipe.

Lastly we'll turn that spline into a continuous stream.

> multOverTime :: Float -> VarT Effect UserInput Float
> multOverTime = outputStream 0 . multSequence 

The Big Picture
--------------------------------------------------------------------------------
Now we can combine our network. We can start by writing a pure function that 
takes a position, scale, red, green and blue parameters and returns a `Pic`. 
This `Pic` is what we'll render each frame.

Here we use functions from [gelatin-picture][2] to translate, scale and fill
a circle of radius 100.  

> picture :: V2 Float -> Float -> Float -> Float -> Float -> Pic
> picture cursor s r g b = 
>     move cursor $ scale (V2 s s) $ withFill (solid $ V4 r g b 1) $ circle 100 

We put it all together with [varying][1]s Applicative instance to construct our 
`Pic` stream.

> network :: VarT Effect UserInput Pic
> network = picture <$> cursorPosition 
>                   <*> multOverTime 3
>                   <*> multOverTime 1 <*> multOverTime 2 <*> multOverTime 3

Our Game Loop
================================================================================
In our main loop we need to make a window and write functions for pushing input
and time into our network. We'll sample our network in one of these functions 
and render it.

> main :: IO ()
> main = do

Start up glfw, receiving a `Rez`. A `Rez` is a type of resource that
[gelatin-glfw][3] uses to render. It's a composite type containing a glfw window
and some shaders.

>     rez <- startupGLFWBackend 800 600 "Push Pull" Nothing Nothing 
>     let window = rezWindow rez
>     setWindowPos window 400 400

Next we'll need a tvar to contain our app data - that way we can access and 
modify it from any thread. This is going to be a big part of turning our pull
based FRP network into a push-esque system, where we only render what and when
we need. 

>     t0   <- getCurrentTime
>     tvar <- atomically $ newTVar AppData{ appNetwork = network 
>                                         , appCache   = mempty
>                                         , appEvents  = []
>                                         , appUTC     = t0
>                                         }

[varying][1] is a pull based FRP implementation. This (roughly) means that as the 
programmer you describe a network using various streams, combining them together 
until you have one final stream that you can "pull" or sample from each frame. 
Your final game data type will essentially just fall out of the network every
frame. The downside to a pull network is that you have to sample it often, 
typically every frame - regardless of whether or not anything has changed. This
is what we are trying to avoid. 

In an attempt to remedy that situation we'll only run the network when we receive 
an event from glfw - or if our network requests that we run it. Let's define the 
function we'll use to "push" a user event into our app. 

>     let push input = atomically $ modifyTVar' tvar $ \app -> 
>                          app{ appEvents = appEvents app ++ [input] }

Separately we need a function to input time. We'll run this any time glfw
receives an event of any kind. Since we'll do this any time glfw receives an
event this is also a good place to render the frame and update our cache.

>         step = do  
>             t <- getCurrentTime
>             putStrLn $ "Stepping " ++ show t

Above we print the current time just so we know the app is stepping in the 
console.

The Network Step
----------------
Now we'll run the network. A little magic happens here. `stepMany` takes a list
of input to iterate your network over, returning the last value. The magic part
and the reason for `UserInput`s `Monoid` instance is that stepMany runs over
each item of the list until it gets to `[]`. Then it runs one more time using
the `mempty` event. Ultimately, if you fed your network an empty list of events, 
`stepMany` would create one and step your network over it. 

>             AppData net cache events lastUTC <- readTVarIO tvar
>             let dt = max oneFrame $ realToFrac $ diffUTCTime t lastUTC 
>                 evs = InputTime dt : events
>                 ((pic, nextNet), outs) = runWriter $ stepMany evs net 

Now we can render our `Pic`.

>             newCache <- renderFrame rez cache pic

And write our new app state.

>             atomically $ writeTVar tvar $ AppData nextNet newCache [] t

And apply our network's requests. We fold our output using a `Set` since we only 
want unique requests. We don't want time going super fast just because more 
network nodes request it. Time...woah.

>             let requests = S.toList $ foldr S.insert S.empty outs
>             mapM_ applyOutput requests 

Here's where the update request magic happens. We spawn a new thread to wait a 
duration, whatever we see fit. In this case it's `oneFrame`, or one thirtieth of 
a second. Then we `push` a `InputTime` event into our app and call glfw's 
`postEmptyEvent`. `postEmptyEvent` will wake up the main thread from 
`waitEvents`, which you'll see later.

>         oneFrame = 1/30 
>         applyOutput OutputNeedsUpdate = void $ async $ do 
>             threadDelay $ round (oneFrame * 1000)
>             postEmptyEvent
>         applyOutput _ = return ()
                        
What about our user input events? I'm glad you asked. For this we can wire up
glfw's nifty callbacks. They'll simply push some events in our event queue.

>     setCursorPosCallback window $ Just $ \_ x y ->
>         push $ InputCursor (realToFrac x) (realToFrac y)

>     setWindowSizeCallback window $ Just $ \_ w h -> do
>         print ("window size",w,h)
>         push $ InputWindowSize w h

Here is a nifty hack. In this callback we won't push an event, we'll just call
`step` to update our app while the window is refreshing. This gives the app the
ability to run even when the window is actively being resized.

>     setWindowRefreshCallback window $ Just $ \_ -> do
>         putStrLn "widow refresh"
>         step

And now we can loop forever! We process all the stored events and render our 
app. Then we call `waitEvents` which will put the main thread to sleep until
some events have been stored - rinse and repeat. 

Output requests for updates will be scheduled in a separate thread and will post 
an empty event, waking up the main thread from `waitEvents` - causing our loop
to recurse.

>     let loop = step >> waitEvents >> loop
>     loop

Conclusion
--------------------------------------------------------------------------------
FRP is a pretty cool thing. It's got some great ideas and its (mostly) a nice
way to organize your code. It encourages very granular functions and by 
providing a small feedback mechanism you can remedy some of the bitter taste of
constant rendering. 

Hopefully this tutorial has been helpful. Please comment at HN or Reddit,
constructive or not! You can say things like "I hate me an FRP" or "I learn me
some streams, hot dang." Thanks for reading :)

Oh - also - don't forget that this is a literate haskell file and can be built 
and run in standard cabal fasion.

[1]: http://hackage.haskell.org/package/varying
[2]: http://github.com/gelatin/gelatin-picture
[3]: http://github.com/gelatin/gelatin-glfw
[4]: http://hackage.haskell.org/package/netwire
[5]: http://hackage.haskell.org/package/renderable

[odin]: https://github.com/schell/odin
[fonty]: http://hackage.haskell.org/package/FontyFruity
[linear]: http://hackage.haskell.org/package/linear
[glfw-b]: http://hackage.haskell.org/package/GLFW-b
[varying core]: http://hackage.haskell.org/package/varying/docs/Control-Varying-Core.html
[varying constructors]: http://hackage.haskell.org/package/varying/docs/Control-Varying-Core.html#g:1
