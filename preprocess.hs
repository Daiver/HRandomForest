import Control.Monad
--import Data.Bitmap.Base
--import Codec.Image.STB
import Codec.Picture
--import Data.Bitmap.Pure.Pixels

preprocess = eraseLines

eraseLines image = brightnessRGB8 20 image

brightnessRGB8 :: Int -> Image PixelRGBA8 -> Image PixelRGBA8
brightnessRGB8 add = pixelMap brightFunction
     where up v = fromIntegral (fromIntegral v + add)
           brightFunction (PixelRGBA8 r g b a) 
                | a < 1 = PixelRGBA8 250 0 0 255
                -- | g < 129 = PixelRGBA8 0 0 0 0
                | otherwise = PixelRGBA8 r g b a


imageSize image = (imageWidth image, imageHeight image)
borderedPixelAt img pix x y 
    | x >= 0 && y >=0 && x < w && y < h = pixelAt img x y 
    | otherwise = pix
    where  (w, h) = imageSize img

imageMap :: (Pixel b) => (Image a -> Int -> Int -> b) -> Image a -> Image b
imageMap f img = generateImage (f img) w h 
    where (w, h) = imageSize img

filterLines :: Image PixelRGBA8 -> Image PixelRGBA8
filterLines img = imageMap f img
    where
        f img x y = pixelAt img x y
        (w, h) = imageSize img



main = do
    imgE <- readImage "./images/100.png"
    let (Right (ImageRGBA8 img)) = imgE
    print $ imageWidth img
    print $ imageHeight img
    let img2 = brightnessRGB8 10 img
    --let img2 = filterLines img
    writePng "1.png" img2
    print "Start"
