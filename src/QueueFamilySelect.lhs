\documentclass{article}
%include polycode.fmt

\usepackage{wasysym}
\usepackage[activate={true,nocompatibility},
            final,
            tracking=true,
            kerning=true,
            spacing=true,
            factor=1100,
            stretch=10,
            shrink=10]{microtype}
\microtypecontext{spacing=nonfrench}

\title{Selecting Vulkan Queue Families in Haskell}
\author{idgd}
\date{}

\begin{document}

\maketitle

\section{Problem Statement}

When working with Vulkan, there's lots of small problems to solve in order to render a triangle.
One such example of this is deciding which queue families to use.
Queue families, for the purposes of this article, can be thought of as places to submit work to the GPU.
Each one of these families has a set of capabilities: graphics (rendering images), presentation (showing images on screen), and/or compute (general processing).
The Vulkan specification only guarantees that graphics and presentation will be present; the presence of a compute queue family is optional. Vulkan's API to distinguish those queue families presents us with an array of queue family property objects.
You can query these objects for their capabilities, mostly using some bit manipulation.
Exactly how isn't important, just the end result: the indices into the original array for graphics, present, and compute queue families in a tuple.

With these, we need to create a logical (aka virtual) device.
The function to create that device takes a vector of records, each with an index for which queue family we want this logical device to use.
Our goal is to pass the smallest vector of records into that function as possible, while still covering (at minimum) graphics and presentation capabilities.

Due to the nature of the problem, we don't have one single solution; some solutions are also suboptimal.

\begin{spec}
([1,2],[2,3],[3,4]) ->
    [1,2,3] -- suboptimal
    [1,3,4] -- suboptimal
    [2,3] -- optimal
([1],[2,3],[]) ->
    [1,2] -- optimal
    [1,3] -- optimal
\end{spec}

Because of this, we're going to pick an arbitrary answer from the best options: minimizing the sum of each vector.
One way to think of the solutions in general is a set of lists; we'll call these solution space (set) and solution (lists), respectively.
Each solution is a list where each element from the three source lists is paired with one element in each other list without duplicates.
Over many possible pairs, we have solution space.

\section{Solution}

Let's start with exploring our module and imports.
We export three functions, which we'll explore next in turn.
We use a state monad later for in-place vector sorting, along with the vector and insertion sort libraries.
We picked insertion sort because our inputs are usually very small; a length of only one or two is not unusual.
We import word to match the types from Vulkan.
Finally, we hide some of the prelude to avoid any shadowing to vector functions.

\begin{code}
module QueueFamilySelect
    ( selectQueueFamilies
    , pairgp
    , pairc
    ) where

import Control.Monad.ST
import Data.Vector
import Data.Vector.Algorithms.Insertion
import Data.Word
import Prelude hiding ((++), minimum, concatMap, null, filter, elem)
\end{code}

Given our input tuple, we produce the optimal solution with two function calls.
We'll explore those two next.
The way this function is used, we pair \emph{graphics} and \emph{present}.
We use the result of that to pair with \emph{compute}; at that point, we've found the optimal solution for all three items.

\begin{code}
selectQueueFamilies :: (Vector Word32, Vector Word32, Vector Word32)
    -> Vector Word32
selectQueueFamilies (graphics, present, compute) =
    pairc (pairgp graphics present) compute
\end{code}

We'll look at \emph{pairgp} first.
We take a two vectors, \emph{a} and \emph{b}.
This pair is constrained in that it is guaranteed never to be empty, so we can safely define this function partially.
These vectors are passed into \emph{concatMap}, along with a lambda.
\emph{concatMap} will map the given function over the vector, which will generate another vector, and then concatenate the results.
The outer lambda maps another lambda over \emph{b}; the inner lambda takes each element of \emph{a} and appends it to a vector with each element of \emph{b}.
We end up with every possible pairing of each element in both vectors.

We take the minimum from this list, and pass it into a custom function which will sort it and remove any duplicates.
This prevents us from presenting any incorrect answers like \emph{([0], [0]) $\to$ [0,0]}.
Once we have this, we can move on to pairing this result and the (possibly empty) compute vector.

\begin{code}
pairgp :: Vector Word32 -> Vector Word32 -> Vector Word32
pairgp a b = vUniqSort $ minimum $
    concatMap (\i -> fmap (\j -> fromList [i, j]) b) a
\end{code}

Here, only \emph{b}, as the vector for compute queue family indices, could be null.
We check for that, and shortcut to just \emph{a} if that's the case.
Otherwise, we check if any elements from \emph{a} exist in \emph{b}.
If not, that means we need to gather something from \emph{b}, so we pick the minimum.
Finally, if we don't need to, we return \emph{a} by itself, since it sufficiently covers our input.

\begin{code}
pairc :: Vector Word32 -> Vector Word32 -> Vector Word32
pairc a b = if null b
    then a
    else if null $ filter (\i -> elem i b) a    
    then cons (minimum b) a
    else a
\end{code}

Let's finally take a look at our combination nub (remove duplicates) and sort.
We use the \emph{vector-algorithms} package to sort our list.
Since this package does in-place sorting, we use the strict state thread monad to `thaw' (make mutable), sort, and `freeze' (make immutable) our newly mutable vector.
Finally, we return the output of \emph{uniq}, which removes consecutive duplicates.
Since our vector is sorted, we can guarantee that \emph{uniq} in this case is equivalent to a nub.

\begin{code}
vUniqSort :: Vector Word32 -> Vector Word32
vUniqSort a = runST $ do
    t <- thaw a
    sort t
    f <- freeze t
    return $ uniq f
\end{code}

\section{Testing}

%include test/Spec.lhs

Now that we've done all that, I should note that most real-world inputs to this whole function will be \emph{([0,1],[0],[0])}.
If we were going for practicality, and not overengineered correctness, we could've done something much simpler.
But this was much more fun. \smiley


\section{How was this document generated?}

We use a custom setup in our package.yaml file, which stack uses to execute the below Haskell (in Setup.hs).
It acts equivalently to a shell script; we could use the pandoc package directly, but lhs2TeX provides no such in-language support; so we standardized on shell.

\begin{verbatim}
import Distribution.Simple
import System.Process

main = do
  createProcess (shell
    "lhs2TeX src/Lib.lhs > Lib.tex && texi2pdf -c -q Lib.tex")
  createProcess (shell
    "pandoc --from=LaTeX+lhs --to=plain src/Lib.lhs -o Lib.txt")
  createProcess (shell
    "pandoc --from=LaTeX+lhs --to=plain test/Spec.lhs -o Spec.txt")
  defaultMain
\end{verbatim}

I will admit I also used some manual editing to the HTML files.
This text in the top level of our package.yaml file is what guarantees Setup.hs is run.

\begin{verbatim}
build-type: Custom
custom-setup:
  dependencies: base >= 4.7 && < 5, Cabal, process
\end{verbatim}

\end{document}