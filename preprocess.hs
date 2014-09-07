import Control.Monad
import Codec.Picture

preprocess = eraseLines

eraseLines image = brightnessRGB8 20 image

brightnessRGB8 :: Int -> Image PixelRGBA8 -> Image PixelRGBA8
brightnessRGB8 add = pixelMap brightFunction
     where 
        brightFunction (PixelRGBA8 r g b a) 
            | a < 1 = PixelRGBA8 250 0 0 255
            -- | g < 129 = PixelRGBA8 0 0 0 0
            | otherwise = PixelRGBA8 r g b a


imageSize image = (imageWidth image, imageHeight image)

borderedPixelAt :: (Pixel a) => (Image a) -> a -> Int -> Int -> a
borderedPixelAt img pix x y 
    | x >= 0 && y >=0 && x < w && y < h = pixelAt img x y 
    | otherwise = pix
    where  (w, h) = imageSize img

imageMap :: (Pixel b) => (Image a -> Int -> Int -> b) -> Image a -> Image b
imageMap f img = generateImage (f img) w h 
    where (w, h) = imageSize img

kernelInds n m = [(i - (n `div` 2), j - (m `div` 2)) | i <- [0..n - 1], j <- [0..m - 1]]

convolveF :: ([PixelRGBA8] -> PixelRGBA8) -> (Int, Int) -> Image PixelRGBA8 -> Image PixelRGBA8
convolveF f (n, m) img = imageMap (\_ i j -> f $ pixels i j) img
    where
        indexes = kernelInds n m
        pixels i j  = map (\(i1, j1) -> borderedPixelAt img (PixelRGBA8 0 0 0 0) (i + i1) (j + j1)) indexes

filterLines :: Image PixelRGBA8 -> Image PixelRGBA8
filterLines img = convolveF f (3, 3) img 
    where 
        f l 
            | (> 0) . length . filter (\(PixelRGBA8 r g b a) -> b > 10 && a /= 0) $ l = pix
            | otherwise = PixelRGBA8 0 0 0 0
            where   
                pix = l !! 5
                

main = do
    imgE <- readImage "./images/100.png"
    let (Right (ImageRGBA8 img)) = imgE
    print $ imageSize img
    --let img2 = brightnessRGB8 10 img
    let img2 = filterLines img
    writePng "1.png" img2
    print "Start"
