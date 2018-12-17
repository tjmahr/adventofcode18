library(adventofcode18)
x <- readLines("./inst/input12.txt")

pots <- stringr::str_remove(x[1], "initial state: ")
rules <- x[3:length(x)]

p1 <- pots %>%
  run_plant_rules(rules, 20) %>%
  which_pots_have_plants()
sum(p1)

# p2 <- run_plant_rules(pots, rules, 50)
p2_a <- run_plant_rules(pots, rules, 500)
p2_b <- run_plant_rules(pots, rules, 5000)
p2_b1 <- run_plant_rules(p2_b, rules, 1)
p2_b2 <- run_plant_rules(p2_b1, rules, 1)

p2_a %>% which_pots_have_plants() %>% sum()

c(p2_b %>% which_pots_have_plants() %>% sum(),
p2_b1 %>% which_pots_have_plants() %>% sum(),
p2_b2 %>% which_pots_have_plants() %>% sum()) %>% diff

# p2 <- f12b(x)

stopifnot(sum(p1) == aoc18_solutions$day12a)
# stopifnot(p2 == aoc18_solutions$day12b)
