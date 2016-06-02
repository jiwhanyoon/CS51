# Gha Young Lee, Kevin Ji Whan Yoon

## Results & Discussion:

### Results:


To see the crawler time and query time, we did the following:

1. turn off debugging by setting debug = false in 'moogle.ml'
    so that we can see the crawler time easily.

2. put a Printf inside do_query so we can see the query time 
    in the terminal each time we do a search.

To test query performance, for each html/index.html, simple-html/index.html, 
and wiki/Teenage_Mutant_Ninja_Turtles, with 40 pages, different queries 
were inputted into the localhost:8080 search box. The following query 
were searched and avg'ed - "the", which has 19, 2 and 40 results.

```

### Query Performance:

The following are the results for going through different folders (html, simple-html, wiki) in the order of time it took for ListSet and then DictSet (ListSet time, DictSet Time).

html (19 results): (0.00032, 0.00038)

simeple-html (2 results): (0.00005, 0.00005)

wiki (40 results): (0.00119, 0.00181)


```
To test crawler performance, run wiki/Teenage_Mutant_Ninja_Turtles with different 
number of pages, for each ListSet and DictSet. 3 performances were averaged. 

```

### Crawler Performance:

The following are the results in the format of (ListSet, DictSet) for 10, 40, 100, and 150 files. 

10: (1.254298, 0841794)

40: (14.893887, 3.625625)

100: (90.813905, 13.903602)

150: (220.579979, 47.076580)

```


### Discussion:

Note that the crawler performance is a lot better for DictSet compared to ListSet, but
the query performance is about the same. This is because for each query the pages crawled
through is the same, but for the crawler performance test that # was different. 

We can attribute two main reasons for the crawler faster time. One is the fact that the set is already sorted for DictSet. When the set is sorted, the program does not need to keep track of information related to the order of the set and is able to go through the entire set much quickly. There is no extra time needed to sort through the given data as well as sort through the results. 

Also, DictSet is much faster because when something in the file does not exist, it knows to stop. Instead of going through the entire set one more time to make sure nothing is missing, it is able to move on, thus reducing the time spent checking if the file exists. 
