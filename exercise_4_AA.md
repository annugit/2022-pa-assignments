Exercise_4
================

``` r
node_names <- tibble(
  id   = c(1,2,3,4,5,6,7,8,9,10),
  name = c("1","2","3","4","5","6","A","B","C","D")
)
node_names
```

    ## # A tibble: 10 x 2
    ##       id name 
    ##    <dbl> <chr>
    ##  1     1 1    
    ##  2     2 2    
    ##  3     3 3    
    ##  4     4 4    
    ##  5     5 5    
    ##  6     6 6    
    ##  7     7 A    
    ##  8     8 B    
    ##  9     9 C    
    ## 10    10 D

``` r
edge_list <- tibble(
  from = c(1,2,3,3,3,3,3,4,5,5,6,6,7,7,8,8,9),
  to   = c(2,7,4,5,8,9,10,9,6,10,8,10,8,9,9,10,10)
)
edge_list
```

    ## # A tibble: 17 x 2
    ##     from    to
    ##    <dbl> <dbl>
    ##  1     1     2
    ##  2     2     7
    ##  3     3     4
    ##  4     3     5
    ##  5     3     8
    ##  6     3     9
    ##  7     3    10
    ##  8     4     9
    ##  9     5     6
    ## 10     5    10
    ## 11     6     8
    ## 12     6    10
    ## 13     7     8
    ## 14     7     9
    ## 15     8     9
    ## 16     8    10
    ## 17     9    10

``` r
fakebook_graph <- tbl_graph(nodes = node_names, edges = edge_list, directed = FALSE)
fakebook_graph
```

    ## # A tbl_graph: 10 nodes and 17 edges
    ## #
    ## # An undirected simple graph with 1 component
    ## #
    ## # Node Data: 10 x 2 (active)
    ##      id name 
    ##   <dbl> <chr>
    ## 1     1 1    
    ## 2     2 2    
    ## 3     3 3    
    ## 4     4 4    
    ## 5     5 5    
    ## 6     6 6    
    ## # ... with 4 more rows
    ## #
    ## # Edge Data: 17 x 2
    ##    from    to
    ##   <int> <int>
    ## 1     1     2
    ## 2     2     7
    ## 3     3     4
    ## # ... with 14 more rows

``` r
fakebook_graph %>% 
    ggraph(layout = 'kk') + 
    geom_edge_link() + 
    geom_node_point(size = 8, colour = 'gray') +
    geom_node_text(aes(label = name), colour = 'steelblue', vjust = 0.4) + 
    ggtitle('fakebook bus network') + 
    theme_graph()
```

    ## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family not
    ## found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
    ## family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
    ## family not found in Windows font database

![](exercise_4_AA_files/figure-gfm/plot-1.png)<!-- -->

``` r
fakebook_graph <- fakebook_graph %>% 
  activate(nodes) %>% 
  mutate(d_centrality = centrality_degree()) %>%  # adding measure of degree centrality
  mutate(b_centrality = centrality_betweenness()) # adding betweenness centrality
```

    ## Warning in betweenness(graph = graph, v = V(graph), directed = directed, :
    ## 'nobigint' is deprecated since igraph 1.3 and will be removed in igraph 1.4

``` r
fakebook_graph %>% 
  ggraph(layout = 'kk') + 
  geom_edge_link() + 
  geom_node_point(aes(size = d_centrality, colour = b_centrality)) + 
  scale_color_continuous(guide = 'legend') +
  geom_node_text(aes(label = name), colour = 'red', vjust = 1.6) + 
  ggtitle('Fakebook bus network') + 
  theme_graph()
```

    ## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family not
    ## found in Windows font database

    ## Warning in grid.Call(C_stringMetric, as.graphicsAnnot(x$label)): font family not
    ## found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
    ## family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
    ## family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
    ## family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
    ## family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
    ## family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
    ## family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
    ## family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
    ## family not found in Windows font database

![](exercise_4_AA_files/figure-gfm/degree%20centrality-1.png)<!-- -->

## Q.2. For each seat choice (A-D), calculate various measures of centrality.

Degree centrality:

A - 2, B, C B - D, 3, C, A, 6 C - D, 3, 4, B, A D - 6, 5, 3, C, B

Betweenness centrality:

From the above graphs, we can see that if A is removed from the grpah,
there will be no connection between 1,2 and the rest of the nodes
(people). Thus, A is the most crucial in terms of betweenness
centrality.

Closeness centrality:

B and C have the shortest number of paths to any node. The longest that
B and C have to traverse is 3 nodes to get to the node farthest from
them, and this case occurs once for both B and C. On the other hand, all
other nodes take either more hops to reach the node farthest to them, or
the the number of paths of length 3 is more than 1.

\##Q.3. Discuss possible consequences of your choice of a seat. When
would this choice be beneficial? When would it be not so beneficial?

I choose B or C as the optimum seat because B and C have the maximum
number of direct connections (maximum degree centrality). Moreover, they
also have maximum closeness centrality, and thus, can reach out to
everyone with the least amount of linking through people. Even though A
is crucial in terms of being the connection between 1,2 and the rest of
the nodes, it will have to traverse long to connect to everyone. Since B
and C, both are connected to A directly, they can leverage A to reach to
2 and further to 1.
