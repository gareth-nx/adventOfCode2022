# adventOfCode2022

In Fortran, using fpm.

Run day01 with e.g.
```        
    fpm run day01
```

Note day09 uses the `stdlib_hashmaps` module. Compiling this requires special treatment
```
    export FPM_FFLAGS="-O3 -fno-range-check"
    fpm run day09
```

On day 11 I used a queue datastructure, modified from flibs. Compiling this required preprocessng as well.
```
    export FPM_FFLAGS="-O3 -fno-range-check -cpp"
    fpm run day11
```
With my current setup, fpm does not recompile appropriately if the [app/queues_flibs.inc](app/queues_flibs.inc) changes.

