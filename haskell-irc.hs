import Network
import System.IO
import Control.Monad

handleLine :: Handle -> String -> IO ()
handleLine h line =
  putStrLn line

getLines h = do
  isReady <- hReady h
  when isReady $ do
    line <- hGetLine h
    handleLine h line
    getLines h

handleConnection h = do
  getLines h
  send <- getLine
  hPutStrLn h send
  handleConnection h

ircConnect host port nick = withSocketsDo $ do
  h <- connectTo host (PortNumber 6667)
  hSetBuffering h LineBuffering
  hPutStrLn h $ "NICK " ++ nick
  hPutStrLn h $ "USER " ++ nick ++ " " ++ nick ++ " " ++ host ++ " :" ++ nick
  handleConnection h

main = 
  ircConnect "irc.mibbit.net" 6667 "testnumse" `catch` handler

handler :: IOError -> IO ()
handler e = putStrLn "Whoops, something happened"
