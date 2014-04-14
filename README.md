slipk2
======

Create/transform a slip distribution to be compatible with a k2 slip distribution on the fault plane 

 This program generates a k2 dislocation distribution based
 on a summation of asperities, with a fractal distribution
 of the radius (Zeng et al, bssa, 1994).
 An input file is needed. The order of the parameter inside
 this file is not important. See the exemple slip.inp
 (Messina like source ...)
                  Usage :
                      slipk2 input=filename
 The ouput is created in a tmp directory (you need to create it).
 The p.tmp file is the probability fonction on the fault and
 the d.tmp is the dislocation. The normalization may be done
 on a stress drop basis or on a total moment basis.
 If the gaussian number in the input file is null, The code
 will expect an existing p.tmp file.

 To see the probability function or the slip distribution (a
 simple 2d binary array) use with ximage
         ximage n1={ny} < d.tmp
 and with gnuplot
   gnuplot> plot 'd.tmp' binary array=({nx},{ny}) scan=yx flipy with image
