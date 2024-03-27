# PPM-Image-Processor

A PPM Image editor that does the following functions on any PPM file:
  Grayscale: Converts the image into grayscale and returns the resulting image as a list of lists.
  Threshold: Thresholding increases image separation --- dark values become darker and light values become lighter.
  FlipHorizontal: Flips an image so that what’s on the left is now on the right, and what’s on the right is now on the left.
  EdgeDetect: Edge Detection replaces each pixel in the original image with a black pixel, (0, 0, 0), if the original pixel contains an "edge" in the original image.  If the original pixel does not contain an edge, the pixel is replaced with a white pixel (255, 255, 255). Note: An edge occurs when the color of pixel is "significantly different" when compared to the color of two of its neighboring pixels.
  RotateRight90: Rotates the image to the right 90 degrees.
