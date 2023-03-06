#functional programming
# https://k-hench.github.io/x_functional/functional_work.html

pacman::p_load(remotes, tidyverse, rlang, stupid1, stupid2, grImport2)

add_1 <- function(x){x + 1}
add_1(x = 1)

sum_of_two <- function(x, y = 4){ x + y }
sum_of_two(x = 1)
sum_of_two(x = 1, y = 2)
sum_of_two(6)
sum_of_two(y = 6, 7)

add_2a <- function(){ x + 2 }
add_2b <- function(){ x <- 3; x + 2 }
add_2c <- function(x = 4){ x + 2 }

x <- 6
add_2a()
add_2b()
add_2c()

paster_0 <- function(){paste0(c('X: ','Y: '), c(stupid_x, stupid_y))}
paster_1 <- function(){stupid_y <- "Z";  paste0(c('X: ','Y: '), c(stupid_x, stupid_y))}

library(stupid1)
paster_0()
stupid_x
stupid_y

library(stupid2)
paster_0()
env_names(search_envs()[['package:stupid1']])
env_names(search_envs()[['package:stupid2']])

stupid1::stupid_x
stupid2::stupid_x
stupid_x <- 'D'
stupid_y <- '!'
paster_0()
paster_1()

# ... = anything else
paster_2 <- function(x, ...){ message(x) }
paster_2(x = 'test')
paster_2(x = 'test', y = 'something', z = 'else')

paster_3 <- function(x, ...){ 
  message(paste('x: ', x))
  message(paste('other parameters:', ...))
}
paster_3(x = 'test')
paster_3(x = 'test', y = 'something', z = 'else')

library(magrittr)

#sequential
a <- 1
b <- add_1(a)
c <- add_1(b)
d <- add_1(c)
d

#nested
add_1(add_1(add_1(1)))

#pipeline
1 %>%
  add_1() %>%
  add_1() %>%
  add_1()

## Demo fishes
library(tidyverse)
library(hypoimg)
library(cowplot)
hypo_read_svg <- function(file_path){
  grImport2::readPicture(file_path) %>%
    grImport2::pictureGrob(.) %>%
    grid::gList(.) %>%
    grid::gTree(children = . )
}

hypo_recolor_svg <- function(svg, layer = 1, color = 'darkgray'){
  svg[[4]][[1]][[4]][[1]][[4]][[layer]]$gp$fill <- color
  svg
}

svg_file <- "extdata/logo2.c.svg"
svg <- hypo_read_svg(svg_file)
svg %>% hypo_recolor_svg(layer = 1, color = "#FF8029") %>% ggdraw()

# Recursion
## apply a function several times

1:4 %>% 
  purrr::map(add_1)

1:4 %>% 
  purrr::map_dbl(add_1) 

# two parameters
map(.x = 1:3, .f = sum_of_two, y = (1:3) * 10)
map2(.x = 1:3, .y = (1:3) * 10, .f = sum_of_two)
#with a .define y, map iterates over them (map2)

clr <- c( "#FFFFFF", "#BDB596", "#A16D5A", "#590D0E")
map2(.x = 1:4, .y = clr, .f = hypo_recolor_svg, svg = svg) %>%  plot_grid(plotlist = .)

# map variants
mix_of_two <- function(x, y = 4){ 
  tibble( x = x, y = y,
          s = x + y, 
          p = x * y)
}
mix_of_two(1, 2)
map2_dfr(.x = 1:3, .y = (1:3)*10,
         .f =  mix_of_two)

#silent function call
create_file <- function(input){ 
  name <- str_c("tmp/", input, ".txt")
  write_lines(x = input, file = name)
}
dir.create('tmp')

letters[1:2] %>%
  map(create_file)

letters[3:4] %>%
  walk(create_file) #quiet one

dir(path = "tmp", pattern = "txt")

# reduce
reduce(1:3, sum_of_two, .init = 6) #iterating on the same object
#result of first iterations as additional input
#init is optional, results of the first iteration

# (6 + 1 + 2 + 3)
reduce2(.x = 1:4,
        .y = clr,
        .f = hypo_recolor_svg,
        .init = svg) %>% 
  ggdraw()


## Dummy data
make_fake_data <- function(spec, treatment, rep, n = 50){
  tibble(driver = rnorm(n), 
         response = rnorm(n)) %>% 
    write_tsv(file = str_c("data/experimentX_",spec, "_", treatment, "_", rep,".tsv"))
}

dir.create('data')

tibble(spec = rep(c('anglefish', 'grouper', 'chromis', 'seahorse'), each = 12),
       treatment = rep(rep(c("cold", "cool", "warm", "hot"), 4), each = 3),
       rep = rep(1:3, 16)) %>%
  pwalk(make_fake_data)
