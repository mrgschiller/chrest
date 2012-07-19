This folder contains some scripts using lisp to develop and run CHREST 
models.  These scripts have all been tested using the ABCL lisp 
implementation.

Get ABCL
========

ABCL lisp can be downloaded from http://common-lisp.net/project/armed-bear/
After downloading and unpacking the compressed folder, you need to copy 
the file 'abcl.jar' to the same folder as your scripts.

Running CHREST models using ABCL
================================

You need three files together with your script(s).  The files are:

1. abcl.jar (obtained as above)
2. chrest.jar (from the CHREST download)
3. chrest.lisp (included in the examples/lisp folder of CHREST)

To run the script demo-1.lisp, from the command line:

$ java -cp ~/Projects/chrest/chrest.jar:abcl.jar org.armedbear.lisp.Main --noinform --load demo-1.lisp

A shell script/batch file is also provided, so you can use on Linux/Mac:

$ sh run-abcl-script.sh demo-1.lisp

Integrated Environment
======================

Alternatively, you may use the J lisp editor as an integrated environment 
for developing Lisp models with CHREST.  A packaged version is provided 
at http://chrest.info/software 

