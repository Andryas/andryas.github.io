---
title: Async programming in Shiny plus Spinners
author: Andryas Wavrzenczak
date: '2022-03-05'
slug: async-programming-in-shiny-plus-spinners
categories:
  - shiny
tags:
  - async
  - shiny
  - tsp
comments: true
showTags: true
showPagination: true
showSocial: true
showDate: true
summary: "Last week I had a task job to async a part of the code because it was freezing the entire application. For those that
don’t know, the Shiny works in a sync way, which means it executes code in sequence, so if you click on one button and
it has a long operation, the entire application will wait until the operation ends."
---

Greetings earthlings!

If you are reading this, you already know the power and beauty of Shiny, so I won’t spend your time explaining this.

<!--more-->

# The problem

Last week I had a task job to async a part of the code because it was freezing the entire application. For those that
don’t know, the Shiny works in a sync way, which means it executes code in sequence, so if you click on one button and
it has a long operation, the entire application will wait until the operation ends.  

To exemplify, I create a simple app with two extensive operations. The data you will find here consists of the distance
between pairs of each city in the State of Parana in Brazil, some geographics coordinations and sf things to create
maps. And we will be using the idea of Travel Salesman to illustrate. We will calculate all the possible routes between
the cities you select, considering that the first one is the origin and seeing which ones have the shortest route.  

So, what is the idea? You can select a pool of cities, between a minimum of three and a max of seven, that is because
less than three, no makes sense, and great than seven will take too much time. 

After you choose your cities, a distance matrix between pair of cities will appear and two spinners. The left of the
distance matrix is all the possible routes in a DT, the first one is the shortest route, and the second spinner below
the distance matrix is a moving plot. The scenario with seven cities will generate 720 plots, 6!, because, remember, the
first city is our origin.  

So, we have two async operations, the first one that generates all possible routes to be taken, and the second one that
depends on the previous one to generate all the plots. You’ll see that each element appears in Shiny dependable, so no
more freezing problems. 

# The code

![](https://raw.githubusercontent.com/Andryas/shiny-async/master/example.gif)

Before the comment, if you want to go straight to the code...

```r
shiny::runGitHub("Andryas/shiny-async")
```

The code can be find [here](https://github.com/andryas/shiny-async) well commented, but I need to highlight a few
things. The code below shows part of the server.R, note that I put a NULL at the end of the async blocks inside observer
function and created two reactiveVal. The NULL is necessary to create this responsivity because the shiny would follow
the sync way without it. And the reactiveVal to allocate the future_promise created.

```r
# ...
# wait for information about the cities selected then apply tsp.
observe({
    req(tb_cities_distance())

    tb <- tb_cities_distance()
    result_val(NULL)
    waiter_show(
        id = "cities_results_spinner",
        html = waiter::spin_3(),
        color = waiter::transparent(.5)
    )
    waiter_show(
        id = "cities_plot_spinner",
        html = waiter::spin_3(),
        color = waiter::transparent(.5)
    )

    future_promise({
        tsp_naivy(tb)
    }) %...>%
        result_val()

    # Return something other than the promise so shiny remains responsive
    NULL
})


# after calculate all the possible routes generate all the possible plots
observe({
    req(result_val())

    d <- result_val() |>
        arrange(cost)
    
    plots <- list()
    
    future_promise({
        library(sf)

        for (i in 1:nrow(d)) {
            map <- d[i, ]

            map2 <- tibble::as_tibble(tb_map) |> 
                left_join(
                    tibble(name_muni = simplify(strsplit(map$routes, ";"))) |>
                        mutate(direction = 1:n(), fill = "1"),
                    by = "name_muni"
                ) |>
                mutate(fill = replace_na(fill, "0"))

            p <- map2 |>
                    ggplot() +
                    geom_sf(aes(geometry = geom, fill = fill)) +
                    geom_path(
                        data = map2 |> filter(!is.na(direction)) |> arrange(direction),
                        aes(x = longitude, y = latitude)
                    ) +
                    geom_point(
                        data = map2 |> filter(!is.na(direction) & direction == 1),
                        aes(x = longitude, y = latitude), size = 3
                    ) +
                    theme_minimal() +
                    theme(legend.position = "none") +
                    scale_fill_manual(values = c("#834d29", "#251ac5")) +
                    labs(x = "", y = "", title = paste0("Path traveled: ", formatC(map$cost / 1000, format = "f", big.mark = ",", digits = 2), " km"))
            plots <- append(plots, list(p))
        }

        plots
    }) %...>%
        result_plot()

    # Return something other than the promise so shiny remains responsive
    NULL
})

output$cities_layout_plot <- renderUI({
    req(result_plot())

    waiter_hide("cities_plot_spinner")

    fluidRow(
        sliderInput(
            inputId = "cities_routes_plot", 
            label = "",
            min = 1, 
            max = length(result_plot()), 
            value = 1, 
            step = 1,
            animate = animationOptions(1000)
        )
    )
})

output$cities_plot <- renderPlot({
    req(result_plot(), input$cities_routes_plot)
    result_plot()[[input$cities_routes_plot]]
})

output$cities_distance <- renderDT({
    req(tb_cities_distance())

    waiter_show(
        id = "cities_distance",
        html = waiter::spin_3(),
        color = waiter::transparent(.5)
    )

    tb_cities_distance() |>
        datatable(
            rownames = FALSE,
            options = list(
                dom = 't'
            )
        )
})
# ...
```


Thank you for reading and hasta la vista muchachos.  

***

To know more about async things in Shiny Async programming in R and

[Long Runni...
ng Tasks With Shiny: Challenges and Solutions](https://www.r-bloggers.com/2018/07/long-running-tasks-with-shiny-challenges-and-solutions/)

[Async programming in R and Shiny](https://medium.com/@joe.cheng/async-programming-in-r-and-shiny-ebe8c5010790)