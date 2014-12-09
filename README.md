[![Build Status](https://travis-ci.org/ladderframework/LadderFramework.png?branch=master)](https://travis-ci.org/ladderframework/LadderFramework)

&#8803; LadderFramework
=======================
This isn't a framwork for you to use. It's by no means production ready. There are many great framworks and toolkits that you should look at before even waisting time on this one. 

Stop reading! 

*Go away!*

Still here? 

I'm a fool for writing this code. There are thousands of bright people that have created great frameworks. True frameworks. So why did I spend time putting together this code? I could say that other frameworks are horrible. They are not. However I think framework developers have to make a lot of choices. By writing this code I have had to make these choises as well. It has provided me with better understanding of other frameworks as well.  

> Good artists copy, great artists steal. - Pablo Picasso

So what have I done. I've looked a different frameworks that I really like. So I've picket the best parts and put them together. Lets have a look:
* Routing draws inspiration from Unfiltered. Super simple and powerfull part of Scala. It uses pattern matching. It is great but it could have been done better.
* Forms are great in Play. They should have been more typesafe so that has beed changed.
* View and rendering is great in Lift. What I like is the CSS selector part and transformer part. I just did them a little different. However there are no snippets here. The html is 100% clean.
* Request handling is built on Akka HTTP experimental and Akka actors. (BTW: Akka is great).
* Futures are heavily used. SIP-14 FTW. 

The name LadderFramework is a natural evolution (?!?) from stairs (Scala) and lift (Liftweb.net). Maybe it should have been rope but hanging by the neck is no fun.  

As you are still reading I would like to point out that this code is _abandonware_ and not software. 

Stein Kåre 
