((Auction-related things))
https://dl.acm.org/citation.cfm?id=1823847
www.cs.princeton.edu/courses/archive/fall18/cos561/papers/BitTorrent03.pdf
http://ccr.sigcomm.org/online/files/p243-levin.pdf




* Jen's ideas:
  * Investigate economic incentives for content providers.
    * Her big example: Akamai leased their content delivery platform for free for a while, 
      right until everyone depended on them enough that they could coerce them into giving really good
      deals.

My ideas:
* This paper [http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.33.2606] generates
  random graphs whose distribution of node degree is similar to that observed in the actual
  internet. Then they investigate the connectivity properties of such graphs and find that they
  seem to have "Achillies heals" (i.e. they are vulnerable to concerted attacks on few nodes).
  However, in this [https://www.youtube.com/watch?v=OAIBm8LE--E] video for a coursera course
  (taught by a Princeton big EE prof) claims that this is misleading: the highly connected 
  nodes in the real internet are near the edges of the graph, and attacking them
  will only isolate the chunks strictly underneath them.
  First, I'd like to see a paper discussing this claim more formally.

  * => Now, maybe we could see if economic incentives (or even just the Gao-Rexford
    conditions) could inform the generation of a random graph that more
    closely matches the connectivity properties of the real internet.
    
  * => What if we generated graphs using a bunch of different methods and saw which
    ones matched the connectivity properties of the internet most closely?
  
  * I guess these things have probably been done but yeah.

* In the models of [GHJRW08] or [LSZ08], do routers have an incentive to lie about being controlled by the same entity? In other words, can an AS split into multiple ASes in order to get better utility? 

[GHJRW08](http://ccr.sigcomm.org/online/files/p267-goldberg.pdf)

[LSZ08](http://www.cs.huji.ac.il/~schapiram/routing_games-full.pdf) 



