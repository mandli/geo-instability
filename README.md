GeoClaw Instability Experiments
===============================

This repository contains experiments attempting to ascertain an instability that seems to appear in certain cases in the [GeoClaw code](http://github.com/clawpack/geoclaw).  This instability disappears when higher-order corrections are turned off but it is not clear that this is actually the cause of the instability or simply staving off the instability by adding more diffusion.  Currently all Riemann f-wave based solvers exhibit the problem.

Dependencies
------------


Experiments
-----------

 - Base GeoClaw experiment in `base`
 - Non-f-wave based Riemman solvers in `no_fwave`

 