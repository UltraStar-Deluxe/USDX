Delphi interface unit for OpenGL version 1.2 compilable with Delphi 3-6 and Kylix.

This unit is open source under the Mozilla Public License and 
the original author is Dipl. Ing. Mike Lischke (public@lischke-online.de).

You can obtain this unit also from the JEDI (Joint Endeavor of Delphi Innovators) 
API page at www.delphi-jedi.org.

Note for GLScene users: Eric Grange has provided a general vector types unit which 
resolves conflicts for types which are defined in OpenGL12.pas as well as Geometry.pas.
This unit is located in the sub folder "GLScene AddOn".
Please add this unit to the uses clause of OpenGL12.pas and remove the few types which 
are already declared in VectorTypes.pas.

For tests and as starting point three demos are included into the package. Two of them (GLDiag and GLTest)
need the (also provided) simple OpenGL control GLControl (see "GLControl\Package").

- Basic is a very simple test program which only uses an empty form.
- GLTest (in GLControl) uses GLControl to show four rendering contexts simultanously.
- GLDiag is a diagnosis tool similar to DXDiag which shows some properties of the current
   OpenGL driver implementation.

Have fun and 

Ciao, Mike
www.lischke-online.de
www.delphi-unicode.net