To test this, we need to define our inputs and outputs.
Our inputs are:

\begin{itemize}
\item Three word32 vectors
\item The first two of the three lists are guaranteed to have one item in them
\item The last one is not guaranteed to have any
\end{itemize}

Our outputs are:

\begin{itemize}
\item One word32 vector
\item The first two input lists have at least one element represented
\item At most one element of the third input list is represented
\item No duplicates
\item At most three items
\end{itemize}

We obviously have our library; otherwise we couldn't test it.
Also here are one function from the list type, along with vector and word.
We also hide some functions from prelude in order to simplify vector calls.
Finally, we import QuickCheck in order to automate our testing.

\begin{code}
import Data.List (nub)
import Data.Vector
import Data.Word
import Prelude hiding (elem, any, null, length, minimum)
import QueueFamilySelect
import Test.QuickCheck
import Test.QuickCheck.Instances.Vector()
\end{code}

Our first test will be on the innermost function in our library, \emph{pairgp}.
We first constrain the input so we throw away any null inputs; we're guaranteed that graphics and presentation queue families will have an entry, so we reflect that here.
Next, we use our \emph{chk} function to validate that each input list is represented.
We'll explore that next.

\begin{code}
pairgpElements :: (Vector Word32, Vector Word32) -> Property
pairgpElements (a,b) =
    if (null a || null b)
    then property Discard
    else property (Prelude.all id [chka, chkb])
    where
        d :: Vector Word32
        d = pairgp a b
        chka :: Bool
        chka = chk d a
        chkb :: Bool
        chkb = chk d b
\end{code}

\emph{chk} is a little shared utility function that checks to see if any element from \emph{x} exists in \emph{y}.
Above, and further on, we use it to validate that the output of our functions still has elements from our input.

\begin{code}
chk :: Vector Word32 -> Vector Word32 -> Bool
chk x y = any id $ fmap (\i -> elem i y) x
\end{code}

We'll do the same again with \emph{pairc}; the second input can be null here, so we change our constraints to match.
We'll chain our if statements a bit further here as well, in order to check all possible null inputs.
A null \emph{b} should guarantee \emph{a} as the output, so we check that; otherwise, this function should operate exactly the same as \emph{pairgp}.

\begin{code}
paircElements :: (Vector Word32, Vector Word32) -> Property
paircElements (a,b) =
    if null a
    then property Discard
    else if null b
    then pairc a b === a
    else property (Prelude.all id [chka, chkb])
    where
        d :: Vector Word32
        d = pairc a b
        chka :: Bool
        chka = chk d a
        chkb :: Bool
        chkb = chk d b
\end{code}

This same check is done here, but we permit the third input to be null, as it could be in real usage.

\begin{code}
selectQueueFamiliesElements ::
    (Vector Word32, Vector Word32, Vector Word32)
    -> Property
selectQueueFamiliesElements (a,b,c) =
    if (null a || null b)
    then property Discard
    else property $ (Prelude.all id [chka, chkb])
        && if null c
           then True
           else chkc
    where
        d :: Vector Word32
        d = selectQueueFamilies (a,b,c)
        chka :: Bool
        chka = chk d a
        chkb :: Bool
        chkb = chk d b
        chkc :: Bool
        chkc = chk d c
\end{code}

We also want to know if the length of the output is less than three.
Because our end goal is to select one from each, and deduplicate, we should always be less than three at the end.

\begin{code}
selectQueueFamiliesLength ::
    (Vector Word32, Vector Word32, Vector Word32)
    -> Property
selectQueueFamiliesLength (a,b,c) =
    if (null a || null b)
    then property Discard
    else property $ length (selectQueueFamilies (a,b,c)) <= 3
\end{code}

Similarly, there shouldn't be any duplicates in the output, and it should be sorted.
A nub of our output should be equivalent to the output.

\begin{code}
selectQueueFamiliesDupes ::
    (Vector Word32, Vector Word32, Vector Word32)
    -> Property
selectQueueFamiliesDupes (a,b,c) =
    if (null a || null b)
    then property Discard
    else (fromList . nub . toList $ selectQueueFamilies (a,b,c))
        === (selectQueueFamilies (a,b,c))
\end{code}

Finally, let's run the complete set of tests.
We've tested both inner functions, and the three properties of our output that we know are invariant.

\begin{code}
main :: IO ()
main = do
    quickCheck pairgpElements
    quickCheck paircElements
    quickCheck selectQueueFamiliesElements
    quickCheck selectQueueFamiliesLength
    quickCheck selectQueueFamiliesDupes
    return ()
\end{code}