# adventOfCode2022

[Advent of Code 2022](https://adventofcode.com/2022), in Fortran, using fpm, stdlib, and various other dependencies.

Run day01 with e.g.
```        
    fpm run day01
```

Note day09 uses the `stdlib_hashmaps` module. Compiling this requires special
treatment
```
    export FPM_FFLAGS="-O3 -fno-range-check"
    fpm run day09
```

On day 11 I used a queue datastructure, modified from [flibs](https://flibs.sourceforge.net/). Compiling this
required preprocessng as well.
```
    export FPM_FFLAGS="-O3 -fno-range-check -cpp"
    fpm run day11
```

Beware with current setup, fpm does not recompile appropriately if the
included code [app/queues_flibs.inc](app/queues_flibs.inc) changes. I'm not sure
how to get that to work - but a work-around is to delete the build directory.

Day 12 was implemented using priority queues (or heaps), with 2 different implementations - [the first attempt](app/day12.f90) using the [kdtree2 library](https://github.com/jmhodges/kdtree2), and [the second attempt](app/day12_alternate.f90) using [mheap_template](https://github.com/gareth-nx/mheap_template) which also requires preprocessing. 

Day 13 (part 1) segfaults with gfortran but works with ifort.....
