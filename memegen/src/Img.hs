module Img
    ( createMeme
    ) where

import qualified Graphics.GD as G
import qualified Data.ByteString as B

textColor :: G.Color
textColor = G.rgb 255 255 255

textSize :: Double
textSize = 32.0

createMeme :: B.ByteString -> String -> String -> IO B.ByteString
createMeme imgBs upperText lowerText = do
  img <- G.loadJpegByteString imgBs
  (imgW, imgH) <- G.imageSize img

  _ <- G.useFontConfig True

  -- Draw upper text
  (_, (lrx, lry), _, (ulx, uly))
      <- G.measureString "sans" textSize 0.0 (0, 0) upperText textColor
  let (textW, textH) = (1 + lrx - ulx, 1 + lry - uly)
  let upperPos = (imgW `div` 2 - textW `div` 2, textH + 10)
  _ <- G.drawString "sans" textSize 0.0 upperPos upperText textColor img

  -- Draw lower text
  (_, (lrx, lry), _, (ulx, uly))
      <- G.measureString "sans" textSize 0.0 (0, 0) lowerText textColor
  let (textW, textH) = (1 + lrx - ulx, 1 + lry - uly)
  let lowerPos = (imgW `div` 2 - textW `div` 2, imgH - 20)
  _ <- G.drawString "sans" textSize 0.0 lowerPos lowerText textColor img

  G.saveJpegByteString 100 img
