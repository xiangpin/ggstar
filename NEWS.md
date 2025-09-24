# ggstar 1.0.6

+ add a version dependency on the new `ggiraph`. (2025-09-19, Fri)

# ggstar 1.0.5

+ add scale_starshape_interactive. (2025-09-03, Wed)
+ add `geom_star_interactive` which can work with `ggiraph`. (2025-08-05, Tue, @aymennasri, #7)
  - <https://github.com/xiangpin/ggstar/issues/7>
+ add new shape (starshape = 32). (2024-04-07, Sun, @brunomioto, #5)
  - <https://github.com/xiangpin/ggstar/issues/5>
+ add `semicircle` shape (starshape = 31). (2023-07-26, Wed, @lch14forever, #4)
  - <https://github.com/xiangpin/ggstar/issues/4>
+ add `scale_angle_manual`. (2023-07-26, Wed)

# ggstar 1.0.4

+ import `cli` and update some man to be compatible with ggplot2v3.4.0 (2022-10-11, Tue)

# ggstar 1.0.3

+ adjust the default order of `starshape`, now is 
  1, 13, 15, 11, 12, 14, 29, 2, 27. (2021-12-03, Fri) 
+ supporting `subset` in `aes`. (2021-06-28, Mod)
  - This is useful to display the data that meets some conditions.
    - `ggtree(tr) + geom_star(mapping=aes(subset=isTip))`

# ggstar 1.0.2

+ new version to `CRAN`. (2021-04-07, Wed)
+ update the man and help. (2021-04-07, Wed)

# ggstar 1.0.1

+ changed default starshapes. (2021-02-04, Thu)
+ modified the dev of vignettes. (2021-02-04, Thu)
+ don't use svg dev. (20200210, Wed)

# ggstar 0.0.9.1

+ next new development version before next release version. (20200710, Fri)
+ `color` to `fill` in vignettes. (20201023, Fri)

# ggstar 0.0.9

+ 0.0.7 and 0.0.8 has been skipped, because they didn't pass the check of `CRAN`,
  because they don't declared dependencies packages (`rmarkdown`, `markdown`, `prettydoc`)
+ 0.0.9 new version release to CRAN. (20200710, Fri)

# ggstar 0.0.6.4

+ add new shapes (28, 29, 30). (20200708, Wed)

# ggstar 0.0.6.3

+ fix a bug, when `starshape` is NA in legend. (20200612)

# ggstar 0.0.6.2

+ add Thin triangle and other style anise star (20200605)

# ggstar 0.0.6.1

+ add new shapes (heart, circle, Triangle star)

# ggstar 0.0.6

+ update the method to calculate the coordinates of 1,2,3,4,9,10 shapes
+ default: fill = NA and color = "black". They all can be mapped using aes.

# ggstar 0.0.5

+ update the method to calculate the coordinates of 2 and 4 starshape.

# ggstar 0.0.4

* first release to `CRAN`.
