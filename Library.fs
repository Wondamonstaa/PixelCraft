//Name: Kiryl Baravikou
//Course: CS 341 Fall 2023
//Project 2: Image processing in F# 
//Project Description: 
//The following project involves developing a program that can process and perform image operations saved in the PPM format to manipulate their shape and color.
//Professor: Jon Solworth
//Details about the library:
//Grayscale: the following function converts the image into grayscale and returns the resulting image as a list of lists.
//Threshold: the following increases image separation --- dark values become darker and light values become lighter.
//FlipHorizontal: this function flips an image so that what’s on the left is now on the right, and what’s on the right is now on the left.
//EdgeDetect: Edge detection is an algorithm used in computer vision to help distinguish different objects in a picture or to distinguish an object in the foreground of the picture from the background. Edge Detection replaces each pixel in the original image with a black pixel, (0, 0, 0), if the original pixel contains an "edge" in the original image. If the original pixel does not contain an edge, the pixel is replaced with a white pixel (255, 255, 255).
//RotateRight90: this function rotates the image to the right 90 degrees. The image returned by this function will have different dimensions than the original image passed in.

namespace ImageLibrary

module Operations =

//Grayscale: the following function converts the image into grayscale and returns the resulting image as a list of lists.
  let rec Grayscale (width:int) 
                    (height:int) 
                    (depth:int) 
                    (image:(int*int*int) list list) = 

    //Helper function used to convert an RGB color into a grayscale representation
    let grayscaleRGB(r: int, g: int, b: int) =
        //Standard values for relative luminence according to https://www.w3.org/TR/WCAG20/#relativeluminancedef 
        let Object = int(float r * 0.299 + float g * 0.587 + float b * 0.114)
        //Return the resulting object represented as a tuple of three
        (Object, Object, Object)

    let result res =
        //Initialize the standard pattern matching => compare res with the test cases
        match res with
        //The base case
        | [] -> [] 
        //Otherwise, using wildcard as an input for the function on the right
        //I apply the grayscaleRGB function to every single element of the image
        | _  -> List.map (fun element -> List.map grayscaleRGB element) res

    //Return the result
    result image


//Threshold: the following increases image separation --- dark values become darker and light values become lighter
  let rec Threshold (width:int) 
                    (height:int)
                    (depth:int)
                    (image:(int*int*int) list list)
                    (threshold:int) = 
                    
        //The following function checks the provided threshold conditions
        let valueCheck(rgb: int) = 
            //If less than the provided threshold => the element becomes 0. Otherwise, 255
            if rgb <= threshold then 0 else depth

        //The following helper function allow to apply the valueCheck to each element
        let contrastMod =
            //The input image tuple will be matched to each of the cases below
            image
            //Base case
            //|[] -> []
            //Otherwise, applying the monadic map function to each element using inner lambda function I check each value of the provided tuple and apply the valueCheck to compare their current value to the threshold input
            |> List.map (fun e -> 
                e
                //Now, I apply the lambda to each element of the image tuple
                |> List.map (fun (r, g, b) ->
                    //Finally, apply valueCheck to RGB
                    (valueCheck r, valueCheck g, valueCheck b)
                )
            )
        //Return the result
        contrastMod
               

//FlipHorizontal: this function flips an image so that what’s on the left is now on the right, and what’s on the right is now on the left.
  let rec FlipHorizontal (width:int)
                         (height:int)
                         (depth:int)
                         (image:(int*int*int) list list) = 

        //Sanity check function
        let validation (image:(int*int*int) list list) = 
            if List.isEmpty image then [] else image
            
        //The following inner function is used to mirror the image's pixels
        let flipper = 
            //The input image tuple will be matched to each of the cases below
            validation(image)
            //For each element of the tuple, I apply the reverse function which is supposed to take every pixel from one end, and flip it with the opposite pixel on the other end, which results in a mirror-view of the initial input
            |> List.map (fun e ->
                e
                |>List.rev
            )
        //Return the result
        flipper


  //EdgeDetect: Edge detection is an algorithm used in computer vision to help distinguish different objects in a picture or to distinguish an object in the foreground of the picture from the background. 
  let rec EdgeDetect (width:int)
                 (height:int)
                 (depth:int)
                 (image:(int * int * int) list list)
                 (threshold:int) =

    // Sanity check function
    let validateImage (image:(int*int*int) list list) = 
        if List.isEmpty image then [] else image

    // Helper function used to calculate the Pythagorean distance between two elements
    let pythagoreanDistance (current : int * int * int) (right : int * int * int) (below : int * int * int) =
        //The following function calculates the pythagorean distance according to the provided formula
        let evaluate (x1, y1, z1) (x2, y2, z2) = pown(x1 - x2)2 + pown(y1 - y2)2 + pown(z1 - z2)2
        //Finally, square root is taken out of the result of the evaluation which is the final step in the pythagorean formula application
        sqrt(float (evaluate current right)) > float threshold || sqrt(float (evaluate current below)) > float threshold

    //Helper function to check if the index I am going to choose is valid
    let getItem list index =
        //If the index is in the range of the length of the list, then return Some object. Otherwise, None
        if index >= 0 && index < List.length list then Some (List.item index list) else None

    //The following function is used to convert each pixel to black or white of the passed image list and returns a new object with converted pixels
    let rec convert bound obj =
        //The current bound is used as a flag to determine if we are going out of range or not
        match bound = height - 1 with
        //If true, then the reverse function reverses the object for further processing
        | true -> List.rev obj
        | false ->
            //Helper function to flip the pixels to black and white
            let rec flipper idx objects =
                //Index must be within the acceptable range of the provided matrix of pixels
                match idx = width - 1 with
                //If true, then the reverse function reverses the objects list for further processing
                | true -> List.rev objects
                | false ->
                    //The following functions allow to get the item from the list at the specified index and store them in the corresponding vars
                    let currentOpt = getItem (List.item bound image) idx
                    let rightOpt = getItem (List.item bound image) (idx + 1)
                    let belowOpt = getItem (List.item (bound + 1) image) idx

                    //Helper function used to implement the pattern matching
                    let current, right, below =
                        match currentOpt, rightOpt, belowOpt with
                        //If all values are Some, then they will be assigned to the c, r, b, where c = current, r = right, and b = below
                        | Some c, Some r, Some b -> c, r, b
                        //Otherwise, all zeroes
                        | _ -> (0, 0, 0), (0, 0, 0), (0, 0, 0)

                    //Helper function to assign new colors to each of the pixels
                    let newColor =
                        match pythagoreanDistance current right below with
                        //If the result of the pythagoreanDistance is true, then zeroes will replace the values according to the threshold value provided => pixels become white
                        | true -> (0, 0, 0)
                        //Otherwise, the pixels become black
                        | false -> (255, 255, 255)

                    //Recursive call to the flipper to take the next pixel, and place the updated one into the list of objects 
                    flipper (idx + 1) (newColor :: objects)
                    
            //Initialization of the next pixel to call
            let nextPixel = flipper 0 []

            //Finally, the convert function is used to add the next pixel to the list of objects, and proceed further iterations
            convert (bound + 1) (nextPixel :: obj)
            
    //@return
    convert 0 []


//This function rotates the image to the right 90 degrees. The image returned by this function will have different dimensions than the original image passed in.
  let rec RotateRight90 (width:int)
                        (height:int)
                        (depth:int)
                        (image:(int*int*int) list list) = 

        //Sanity check function
        let validation (image:(int*int*int) list list) = 
            match image with
                //If List.isEmpty -> true then return empty
                |[] -> []
                //Otherwise, keep the original image
                |_ -> image

        //The following inner function is used to rotate the image's pixels by 90 degrees
        let flipper = 
            //Validate that the input is correct
            validation(image)
            
            //First, I transpose the image == matrix using the linear algebra definition A^t, where A^t represents the transpose of the matrix: rows become columns, and columns become rows. Please, note that transpose() takes a list of lists and returns a new list of lists by transposing the rows and columns, which is not direct image manipulation according to the official documentation of F#
            //https://learn.microsoft.com/en-us/dotnet/api/system.numerics.matrix4x4.transpose?view=net-7.0
            |>List.transpose
            //Via the pipe, I take the resulted transpose, and using monadic map() I apply the reverse to each elememnt of the matrix accessed via the lambda + map function
            |>List.map (fun e -> List.rev (List.map (fun (r,g,b) -> (r,g,b)) e))

        //Return the result
        flipper