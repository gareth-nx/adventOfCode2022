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

