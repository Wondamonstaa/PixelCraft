# PixelCraft: F# Exploration
## Kiryl Baravikou
### Date: November 2023
### Author: Kiryl Baravikou

---
PixelCraft: Project Description
---


PixelCraft, led by Kiryl Baravikou, is an exciting project in the domain of image processing using the functional programming language F#. The project's primary objective is to provide a comprehensive set of image operations capable of manipulating both the shape and color of images saved in the PPM format.

Key Features:

1. Grayscale Transformation: PixelCraft offers a Grayscale function that converts color images into grayscale. This process involves determining the relative luminance of each pixel and producing a visually pleasing grayscale representation.

2. Threshold Enhancement: The Threshold function increases image separation by making dark values darker and light values lighter. Users can fine-tune the threshold parameter to achieve the desired level of contrast.

3. Horizontal Flip: The FlipHorizontal function mirrors images horizontally, creating a visually reversed representation. What's on the left becomes the right, and vice versa.

4. Edge Detection: Employing an advanced algorithm, the EdgeDetect function identifies edges in images. Pixels containing edges are replaced with black, while others become white, offering a clear distinction between objects in the foreground and background.

5. 90-Degree Rotation: PixelCraft allows users to rotate images 90 degrees to the right. While doing so, the dimensions of the resulting image differ from the original.
---
Conclusion
---
PixelCraft's library, embedded within the ImageLibrary namespace, provides these functions as powerful tools for image manipulation. With a focus on simplicity and efficiency, PixelCraft is an excellent choice for those seeking to perform image processing tasks seamlessly in F#.
