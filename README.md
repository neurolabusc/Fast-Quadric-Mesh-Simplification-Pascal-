# Fast-Quadric-Mesh-Simplification for Pascal/Lazarus/Delphi

##### About

Mesh triangle reduction using quadrics. This is a Pascal port of Sven Forstmann's C++ mesh simplification code.  It is fast, memory efficient, free and high quality. It uses a threshold to determine which triangles to delete, leading to fast performance since it avoids sorting, though note that this might lead to lesser quality.

- [GitHub C++ source](https://github.com/sp4cerat/Fast-Quadric-Mesh-Simplification)
- [Comments on algorithm](http://www.gamedev.net/topic/656486-high-speed-quadric-mesh-simplification-without-problems-resolved/)
- [Comparison with other methods](http://voxels.blogspot.com/2014/05/quadric-mesh-simplification-with-source.html)


##### Graphical User Interface Executable

This code is embedded into the [Surf Ice](https://www.nitrc.org/plugins/mwiki/index.php/surfice:MainPage) surface rendering tool (versions 5 May 2016 and later). Surf Ice is available pre-compiled for Linux, OSX and Windows, and the source code is available on GitHub. The software reads meshes in many popular formats (3ds, ac3d, dxf, GIfTI, gts lwo, ms3d, mz3, ctm, dae/Collada, FreeSurfer, nv, obj, off, ply, stl, vtk) and can export decimated meshes to several formats (mz3, GIfTI, obj, ply). To decimate an image, load the mesh with the File/Open menu item and then choose Advanced/SimplifyMesh menu item to simplify the object. If you are happy with the results you can choose Advanced/SaveMesh. The screen shot below shows the original (left column) and 90% decimated (right column) mesh of the human brain. The top row shows a wire-frame view that helps visualize the mesh complexity, while the bottom row shows the how similar these two meshes look with per-pixel shading.

![img](https://raw.githubusercontent.com/neurolabusc/Fast-Quadric-Mesh-Simplification-Pascal-/master/screenshot.jpg?raw=true)


#####  A simple command line demo
While this algorithm is embedded into Surf Ice, that is a complex program. This GitHub page includes a basic command line program that allows the user to load a obj format mesh, decimate the mesh and save the result to disk. This should help Delphi, Lazarus or FreePascal developers who want to exploit this algorithm. You will need to have the freepascal compiler installed.

 - To compile the program with FreePascal "fpc -O3 -XX -Xs simplify.pas" (though to enforce a 64-bit version you could run ppcx64 -O3 -XX -Xs simplify.pas").
 - To compile the program with Delphi 7 "dcc32 -CC -B  simplify.pas".
 - Once compiled, you can run the program with a command like this "./simplify bunny.obj out.obj 0.2", which should produce a new mesh with 20% of the faces found in the original.


##### Versions

 - 5-May-2016 : Initial release
 - 7-May-2016 : Delphi compatibility, faster FPC (using inlining)

##### License

 - This software uses the [MIT license](https://opensource.org/licenses/MIT)

##### Links

 - [Comparison of some popular algorithms](http://www.alecjacobson.com/weblog/?tag=mesh-decimation)
 - [Introduction to terms and concepts](https://software.intel.com/en-us/articles/3d-modeling-and-parallel-mesh-simplification)
 - [Original paper from Garland & Heckbert (1997) that introduces quadric error metrics (includes nice visualization of this measure)](http://www.cs.cmu.edu/~./garland/Papers/quadric2.pdf)


