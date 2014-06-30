|This is the main module for the eros client. This is not the meat &
potatoes of the program, but this is the bread & butter.

> module Main where


So, here are our imports. I would like to only have IO inputs, however
the import of 'Data.Aeson' is needed in order to keep the
'Erosc.Processor' module pure.

Essentially, I was faced with either making this module be tainted by
purity, or have the other module be pure.

My qualified imports are a bit obnoxious. Even though they aren't
needed, I find that using qualified names makes the code a bit less
confusing. That is, @LzByte.hGetContents@ is a lot more informative
than just @hGetContents@.

> import           Data.Aeson             as Aeson
> import qualified Data.ByteString.Lazy   as LzByte
> import           Erosc.Processor        as Erosc
> import qualified System.IO              as StdIO


|Eventually, I'll need this data type when I use command line
arguments. For the moment, it's just a dummy type. So, enjoy it.

> data EroscOpts = EroscOpts

|So, this is the main function. Whoop de doo.  It sends the contents
of 'StdIO.stdin' to runBtStr.

Eventually, I'll add in command line argument parsing, and then we'll
have a use for the 'EroscOptions' type.

> main :: IO ()
> main = runBtStr =<< LzByte.hGetContents StdIO.stdin

|This function takes the input (presumably 'StdIO.stdin'), and tries
to decode it.

If it successfully decodes the input into the 'Erosc.Input' type,
it sends the newfound decoded input to 'runInput', which sends it to
the processor.

If it can't, the program will throw a tantrum and shit itself.

> runBtStr :: LzByte.ByteString -> IO ()
> runBtStr inputBt = do
>   let eitherJson = (Aeson.eitherDecode inputBt) :: Either String Erosc.Input
>   case eitherJson of
>     Left msg      -> fail msg
>     Right ecInput -> runInput ecInput


There is an astonishing quantity of bureaucracy in this program. You
have to go through like 5 functions before you get to a function that
actually does something useful.

|Okay, so, we're getting closer to the part of the program that
actually does stuff (or further away, depending on the location from
whence you come).

'runInput' takes the 'Erosc.Input' object (I do hate that word), and
sends it to some function in 'Erosc' that actually mutates the
data. Well, it doesn't mutate it, obviously, because this is Haskell,
but you get the idea.

@result@ needs to be '(<-)'d because 'Erosc.processInput' needs to
read the phraselist JSON file. This is a result of poor library design
on my part. I will fix this in a future release.

> runInput :: Erosc.Input -> IO ()
> runInput ipt = do
>   result <- Erosc.processInput ipt
>   let jsonText = Erosc.encode result
>   LzByte.hPutStr StdIO.stdout jsonText

