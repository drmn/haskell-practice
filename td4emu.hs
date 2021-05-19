#!/usr/bin/env stack
-- stack --resolver lts-17.12 script
{-# LANGUAGE BinaryLiterals    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

{-
   TD4 (とりあえず動作するだけの 4bitCPU) エミュレータ

   例:

   1. LED ちかちか
   $ cat > rom1 << EOF
   10110011
   10110110
   10111100
   10111000
   10111000
   10111100
   10110110
   10110011
   10110001
   11110000
   EOF
   $ ./td4emu.hs rom1

   2. ラーメンタイマー
   $ cat > rom2 << EOF
   10110111    OUT 0111
   00000001    ADD A,0001
   11100001    JNC 0001
   00000001    ADD A,0001
   11100011    JNC 0011
   10110110    OUT 0110
   00000001    ADD A,0001
   11100110    JNC 0110
   00000001    ADD A,0001
   11101000    JNC 1000
   10110000    OUT 0000
   10110100    OUT 0100
   00000001    ADD A,0001
   11101010    JNC 1010
   10111000    OUT 1000
   11111111    JMP 1111
   EOF
   $ ./td4emu.hs rom2

   TD4 仕様およびサンプルプログラムは以下書籍より:
   渡波 郁 (2003) 『CPU の創りかた』 マイナビ出版
-}

import           Control.Concurrent
import           Control.Monad.Reader
import           Data.Bits
import qualified Data.ByteString.Char8 as BC
import           Data.Char             (digitToInt, intToDigit)
import qualified Data.Vector.Unboxed   as VU
import           Data.Word
import           Options.Applicative
import           System.Environment

newtype Word4 = Word4 { unWord4 :: Int }
    deriving (Eq, Ord)

instance Show Word4 where
    show = show . unWord4

instance Bounded Word4 where
    maxBound = Word4 15
    minBound = Word4 0

instance Num Word4 where
    Word4 x + Word4 y
        | x + y > unWord4 maxBound = Word4 $ x + y - 16
        | otherwise                = Word4 $ x + y
    Word4 x - Word4 y
        | x - y < unWord4 minBound = Word4 $ x - y + 16
        | otherwise                = Word4 $ x - y
    Word4 x * Word4 y = Word4 $ x * y `rem` 16
    fromInteger n = Word4 $ fromInteger n `rem` 16
    negate x = Word4 $ 16 - unWord4 x
    abs = id
    signum = const (Word4 1)

instance Enum Word4 where
    toEnum = Word4
    fromEnum = unWord4

instance Real Word4 where
    toRational = toRational . unWord4

instance Integral Word4 where
    Word4 x `quotRem` Word4 y = let (x', y') = x `quotRem` y
                                in  (Word4 x', Word4 y')
    toInteger = toInteger . unWord4

data Env = Env
    { envRom   :: !Rom
    , envCpu   :: !(MVar Cpu)
    , envClock :: !(MVar ())
    }

type Rom = VU.Vector Word8

data Cpu = Cpu
    { cpuPc   :: !Word4
    , cpuRegA :: !Word4
    , cpuRegB :: !Word4
    , cpuCf   :: !Word4
    }

process :: (MonadReader Env m, MonadIO m) => m ()
process = do
    asks envClock >>= liftIO . takeMVar

    rom <- asks envRom
    cpuRef <- asks envCpu
    cpu <- liftIO $ takeMVar cpuRef
    let pc = cpuPc cpu
        op = rom VU.! (unWord4 pc)
        opCode = op `shiftR` 4
        imData = op .&. 0b1111

    liftIO $ printPc pc
    liftIO $ printCpuState cpu

    -- Default register updates
    let cpu' = cpu { cpuCf = 0, cpuPc = pc + 1 }

    nextCpu <- case opCode of
        0b0011 -> -- MOV A,Im
            pure cpu' { cpuRegA = fromIntegral imData }
        0b0111 -> -- MOV B,Im
            pure cpu' { cpuRegB = fromIntegral imData }
        0b0001 -> -- MOV A,B
            pure cpu' { cpuRegA = cpuRegB cpu }
        0b1000 -> -- MOV B,A
            pure cpu' { cpuRegB = cpuRegA cpu }
        0b0000 -> do -- ADD A,Im
            let (a, c) = cpuRegA cpu `add'` fromIntegral imData
            pure cpu' { cpuRegA = a , cpuCf = c }
        0b0101 -> do -- ADD B,Im
            let (b, c) = cpuRegB cpu `add'` fromIntegral imData
            pure cpu' { cpuRegB = b , cpuCf = c }
        0b0010 -> do -- IN A
            i <- liftIO $ input
            pure cpu' { cpuRegA = i }
        0b0110 -> do -- IN B
            i <- liftIO $ input
            pure cpu' { cpuRegB = i }
        0b1011 -> do -- OUT Im
            liftIO $ output (fromIntegral imData)
            pure cpu'
        0b1001 -> do -- OUT B
            liftIO $ output (cpuRegB cpu)
            pure cpu'
        0b1111 -> -- JMP Im
            pure cpu' { cpuPc = fromIntegral imData }
        0b1110 -> -- JNC Im
            if cpuCf cpu == 0
               then pure cpu' { cpuPc = fromIntegral imData }
               else pure cpu'
        _  -> -- Unknown opCode
            error $ BC.unpack ("Unknown opcode: " <> showBinN 8 op)
    liftIO $ putMVar cpuRef nextCpu

  where
    add' :: Word4 -> Word4 -> (Word4, Word4)
    add' x y = let z = x + y
                   c = if z < x then 1 else 0
               in  (z, c)

-- Not implemented
input :: IO Word4
input = pure 0

output :: Word4 -> IO ()
output = printLed

readBin :: Num a => BC.ByteString -> a
readBin = fromInteger . toInteger . VU.foldl1' (\a b -> a * 2 + b) . VU.map digitToInt
        . VU.unfoldr BC.uncons . BC.takeWhile (`elem` ("01" :: [Char]))

showBin :: Integral a => a -> BC.ByteString
showBin = BC.reverse . BC.unfoldr f . fromIntegral
  where
    f x = if x == 0
          then Nothing
          else let (q, r) = x `quotRem` 2
               in  Just (intToDigit r, q)

showBinN :: Integral a => Int -> a -> BC.ByteString
showBinN n = pad n '0' . showBin

pad :: Int -> Char -> BC.ByteString -> BC.ByteString
pad n p s
    | r < 0 = BC.drop (-r) s
    | otherwise = BC.replicate r p <> s
  where
    r = n - BC.length s

printFrame :: IO ()
printFrame = do
    setPosition (1,  1) >> BC.putStr "ROM"
    setPosition (1, 24) >> BC.putStr "CPU"
    setPosition (7, 24) >> BC.putStr "LED: 3 2 1 0"
    setDefaultPosition

printRom :: Rom -> IO ()
printRom rom = do
    (`VU.imapM_` rom) $ \i op -> do
        setPosition (i + offX, offY)
        BC.putStr (showBinN 4 i <> ":  " <> showBinN 8 op)
    setDefaultPosition
  where
    (offX, offY) = (2, 2)

printPc :: Word4 -> IO ()
printPc pc = do
    forM_ [0 .. 15] $ \i -> do
        setPosition (i + offX, offY)
        BC.putStr " "

    setPosition (unWord4 pc + offX, offY)
    BC.putStr ">"
    setDefaultPosition
  where
    (offX, offY) = (2, 8)

printCpuState :: Cpu -> IO ()
printCpuState cpu = do
    setPosition (    offX, offY) >> BC.putStr ("PC: " <> showBinN 4 (cpuPc cpu))
    setPosition (1 + offX, offY) >> BC.putStr ("A:  " <> showBinN 4 (cpuRegA cpu))
    setPosition (2 + offX, offY) >> BC.putStr ("B:  " <> showBinN 4 (cpuRegB cpu))
    setPosition (3 + offX, offY) >> BC.putStr ("CF: " <> showBinN 4 (cpuCf cpu))
    setDefaultPosition
  where
    (offX, offY) = (2, 25)

printLed :: Word4 -> IO ()
printLed x = do
    setPosition (offX, offY)
    BC.putStr $ BC.intersperse ' ' $ BC.map f $ showBinN 4 x
    setDefaultPosition
  where
    (offX, offY) = (8, 29)
    f '0' = '.'
    f '1' = '#'

setPosition :: (Int, Int) -> IO ()
setPosition (x, y) = BC.putStr $ "\ESC[" <> bshow x <> ";" <> bshow y <> "H"
  where
    bshow = BC.pack . show

setDefaultPosition :: IO ()
setDefaultPosition = setPosition (18, 1)

data Arg = Arg
    { argClock   :: Int
    , argRomFile :: FilePath
    }

argParser :: Parser Arg
argParser = Arg
    <$> option auto
        (  long "clock"
        <> short 'c'
        <> help "Clock rate (Hz)"
        <> showDefault
        <> value 1
        <> metavar "INT"
        )
    <*> strArgument
        (  metavar "ROMFILE"
        <> help "ROM file (see script comments)"
        )

argParserInfo :: ParserInfo Arg
argParserInfo = info (argParser <**> helper)
    (progDesc "TD4 emulator")

main :: IO ()
main = do
    arg <- execParser argParserInfo

    rom <- VU.fromListN 16 . map readBin
           . filter ((`elem` ("01" :: [Char])) . BC.head)
           . BC.lines <$> BC.readFile (argRomFile arg)

    BC.putStr "\ESC[2J" -- Clear entire screen
    printFrame
    printRom rom

    cpuRef <- newMVar cpu0
    clockRef <- newEmptyMVar
    let env = Env {
          envRom = rom
        , envCpu = cpuRef
        , envClock = clockRef
        }

    -- Clock thread
    forkIO $ forever $ do
        putMVar clockRef ()
        threadDelay $ 10^6 `quot` (argClock arg)

    runReaderT (forever process) env

    pure ()

  where
    cpu0 = Cpu
        { cpuPc = 0
        , cpuRegA = 0
        , cpuRegB = 0
        , cpuCf = 0
        }

