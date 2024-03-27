//
// F# image processing functions.
//
// More details?
//
// Name: Manav Gupta
// School: University of Illinois at Chicago
// Date: 3/23/22023
//

namespace ImageLibrary

module Operations =
  //
  // all functions must be indented
  //

  //
  // Grayscale:
  //
  // Converts the image into grayscale and returns the 
  // resulting image as a list of lists. Pixels in grayscale
  // have the same value for each of the Red, Green and Blue
  // values in the RGB value.  Conversion to grayscale is done
  // by using a WEIGHTED AVERAGE calculation.  A normal average
  // (adding the three values and dividing by 3) is not the best,
  // since the human eye does not perceive the brightness of 
  // red, green and blue the same.  The human eye perceives 
  // green as brighter than red and it perceived red as brighter
  // than blue.  Research has shown that the following weighted
  // values should be used when calculating grayscale.
  //  - the green value should account for 58.7% of the grayscale.
  //  - the red value should account for   29.9% of the grayscale.
  //  - the blue value should account for  11.4% of the grayscale.
  //
  // So if the RGB values were (25, 75, 250), the grayscale amount 
  // would be 80, (25 * 0.299 + 75 * 0.587 + 250 * 0.114 => 80)
  // and then all three RGB values would become 80 or (80, 80, 80).
  // We will use truncation to cast from the floating point result 
  // to the integer grayscale value.
  //
  // Returns: updated image.
  //
  let rec Grayscale (width:int) 
                    (height:int) 
                    (depth:int) 
                    (image:(int*int*int) list list) = 
    // use List.map allows us to apply a function every element of the list and add it to another list.
    // in this example, we use it to map the head (lists of tupples in image list)
    // and then change each element of the tuples in the list to fit grayscale
    // then append the newHead (list of tuples) and recursively change each list of tuples this way
    // until every list of tuples is a grayscaled version of the previous
    match image with
    | [] -> []
    | head::tail ->
      let newHead = List.map (fun (a,b,c) ->
        let avg = (float a*0.299)+(float b*0.587)+(float c*0.114)
        (int avg,int avg,int avg)) head
      newHead::Grayscale width height depth tail
    // same code in higher order
    // List.map(fun c -> c |> List.map(fun (a,b,c) -> 
    //   let avg = (float a*0.299)+(float b*0.587)+(float c*0.114)
    //   (int avg,int avg,int avg))) image


  //
  // Threshold
  //
  // Thresholding increases image separation --- dark values 
  // become darker and light values become lighter. Given a 
  // threshold value in the range 0 < threshold < color depth,
  // each RGB value is compared to see if it's > threshold.
  // If so, that RGB value is replaced by the color depth;
  // if not, that RGB value is replaced with 0. 
  //
  // Example: if threshold is 100 and depth is 255, then given 
  // a pixel (80, 120, 160), the new pixel is (0, 255, 255).
  //
  // Returns: updated image.
  //
  let rec Threshold (width:int) 
                    (height:int)
                    (depth:int)
                    (image:(int*int*int) list list)
                    (threshold:int) = 
    // similar to above, instead of changing each pixel to grayscale...
    // checks whether each value in tuple is above or below threshold
    // makes it 0 if below, and depth if above
    match image with
    | [] -> []
    | head::tail ->
      let newHead = List.map (fun (a,b,c) ->
        let valA = if threshold < a then depth else 0
        let valB = if threshold < b then depth else 0
        let valC = if threshold < c then depth else 0
        (valA,valB,valC)) head
      newHead::Threshold width height depth tail threshold
    // same code in higher order
    // List.map(fun c -> List.map(fun (a,b,c) -> 
    //   let valA = if threshold < a then depth else 0
    //   let valB = if threshold < b then depth else 0
    //   let valC = if threshold < c then depth else 0
    //   (valA,valB,valC)) c) image

  //
  // FlipHorizontal:
  //
  // Flips an image so that what’s on the left is now on 
  // the right, and what’s on the right is now on the left. 
  // That is, the pixel that is on the far left end of the
  // row ends up on the far right of the row, and the pixel
  // on the far right ends up on the far left. This is 
  // repeated as you move inwards toward the row's center.
  //
  // Returns: updated image.
  //
  let rec FlipHorizontal (width:int)
                         (height:int)
                         (depth:int)
                         (image:(int*int*int) list list) = 
    // recursively reverses each list of tuples in the image list
    // making it horizontally flipped
    match image with
    | [] -> []
    | head::tail -> List.rev head::FlipHorizontal width height depth tail
    // same function in higher order
    // List.map(fun c -> List.rev c) image


  //
  // Edge Detection:
  //
  // Edge detection is an algorithm used in computer vision to help
  // distinguish different objects in a picture or to distinguish an
  // object in the foreground of the picture from the background.
  //
  // Edge Detection replaces each pixel in the original image with
  // a black pixel, (0, 0, 0), if the original pixel contains an 
  // "edge" in the original image.  If the original pixel does not
  // contain an edge, the pixel is replaced with a white pixel 
  // (255, 255, 255).
  //
  // An edge occurs when the color of pixel is "significantly different"
  // when compared to the color of two of its neighboring pixels. 
  // We only compare each pixel in the image with the 
  // pixel immediately to the right of it and with the pixel
  // immediately below it. If either pixel has a color difference
  // greater than a given threshold, then it is "significantly
  // different" and an edge occurs. Note that the right-most column
  // of pixels and the bottom-most column of pixels can not perform
  // this calculation so the final image contain one less column
  // and one less row than the original image.
  //
  // To calculate the "color difference" between two pixels, we
  // treat the each pixel as a point on a 3-dimensional grid and
  // we calculate the distance between the two points using the
  // 3-dimensional extension to the Pythagorean Theorem.
  // Distance between (x1, y1, z1) and (x2, y2, z2) is
  //  sqrt ( (x1-x2)^2 + (y1-y2)^2 + (z1-z2)^2 )
  //
  // The threshold amount will need to be given, which is an 
  // integer 0 < threshold < 255.  If the color distance between
  // the original pixel either of the two neighboring pixels 
  // is greater than the threshold amount, an edge occurs and 
  // a black pixel is put in the resulting image at the location
  // of the original pixel. 
  //
  // Returns: updated image.
  //
  let rec EdgeDetect (width:int)
               (height:int)
               (depth:int)
               (image:(int*int*int) list list)
               (threshold:int) = 
    // CheckDistance
    //
    // takes in 2 tuples (pixel) as parameters and returns true if the calculated
    // distance is greater than threshold, or false if it isn't
    let CheckDistance (x1,y1,z1) (x2,y2,z2) = 
      // use pown and sqrt to calculate distance
      let dist = sqrt(pown (float (x1-x2)) 2 + pown (float (y1-y2)) 2 + pown (float (z1-z2)) 2)
      dist > float threshold

    // CheckEdge
    // 
    // function that checks if the right or bottom tuple (pixel) is different enough
    // to the tuple (pixel) in question
    // returns true if it is different to either right or bottom, and false otherwise
    let CheckEdge row col =
      // get right and bottom pixels, (0,0,0) is used to check right and bottom edge
      // and bottom right corner
      let right = if col < width-1 then image.[row].[col+1] else (0,0,0)
      let bottom = if row < height-1 then image.[row+1].[col] else (0,0,0)
      CheckDistance image.[row].[col] right || CheckDistance image.[row].[col] bottom

    // List.init initializes a new list of given height/width (row/column)
    // it is used to create the new list of list of tuples with either (0,0,0)
    // or (255,255,255)
    List.init (height-1) (fun row -> List.init (width-1) (fun col ->
        if CheckEdge row col then (0,0,0) else (255,255,255)))


  //
  // RotateRight90:
  //
  // Rotates the image to the right 90 degrees.
  //
  // Returns: updated image.
  //
  let rec RotateRight90 (width:int)
                        (height:int)
                        (depth:int)
                        (image:(int*int*int) list list) = 
    // list.transpose transposes the given list to rotate 90 degrees to the left
    // list.rev reverses the image (vertical flip)
    // together they rotate 90 degrees right
    List.transpose (List.rev image)
     