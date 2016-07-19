module Memegen.Img
    ( createMeme
    ) where

import qualified Graphics.GD as GD
import qualified Data.ByteString as B

textColor :: GD.Color
textColor = GD.rgb 255 255 255

textSize :: Double
textSize = 32.0

createMeme :: B.ByteString -> String -> String -> IO B.ByteString
createMeme imgBs upperText lowerText = do
  img <- GD.loadJpegByteString imgBs
  (imgW, imgH) <- GD.imageSize img

  _ <- GD.useFontConfig True

  -- Draw upper text
  (_, (lrx, lry), _, (ulx, uly))
      <- GD.measureString "sans" textSize 0.0 (0, 0) upperText textColor
  let (textW, textH) = (1 + lrx - ulx, 1 + lry - uly)
  let upperPos = (imgW `div` 2 - textW `div` 2, textH + 10)
  _ <- GD.drawString "sans" textSize 0.0 upperPos upperText textColor img

  -- Draw lower text
  (_, (lrx, lry), _, (ulx, uly))
      <- GD.measureString "sans" textSize 0.0 (0, 0) lowerText textColor
  let (textW, textH) = (1 + lrx - ulx, 1 + lry - uly)
  let lowerPos = (imgW `div` 2 - textW `div` 2, imgH - 20)
  _ <- GD.drawString "sans" textSize 0.0 lowerPos lowerText textColor img

  GD.saveJpegByteString 100 img
